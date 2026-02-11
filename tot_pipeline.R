# tot_pipeline.R
# End-to-end ToT loop (LLM-first), with fast->medium promotion and spec dedup

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tibble)
})

# ---- Eval-level mapping ----
oos_months_for_level <- function(eval_level) {
  switch(eval_level,
         fast = 24,
         medium = 60,
         deep = Inf,
         24)
}

# ---- Evaluate node table (preserves rationale) ----
evaluate_nodes <- function(nodes_tbl, R_xts, registry,
                           eval_level = "fast",
                           alpha_cvar = 0.99) {
  oos_months_limit <- oos_months_for_level(eval_level)

  nodes_tbl %>%
    mutate(
      eval_level = eval_level
    ) %>%
    pmap_dfr(function(node_id, parent_id, depth, method_family, W, params, constraints,
                      eval_level, feasible, sharpe, cvar99, maxdd, turnover_full, diagnostics, rationale) {

      reg <- registry[[method_family]]
      wf <- reg$weight_fn_factory(params, constraints)

      res <- evaluate_strategy_walkforward(
        R_daily_xts = R_xts,
        W = W,
        weight_fn = wf,
        constraints = constraints,
        alpha_cvar = alpha_cvar,
        oos_months_limit = oos_months_limit
      )

      tibble(
        node_id = node_id,
        parent_id = parent_id,
        depth = depth,
        method_family = method_family,
        W = W,
        params = list(params),
        constraints = list(constraints),
        eval_level = eval_level,
        feasible = isTRUE(res$feasible),
        sharpe = res$objectives$sharpe,
        cvar99 = res$objectives$cvar99,
        maxdd  = res$objectives$maxdd,
        turnover_full = res$objectives$turnover_full,
        diagnostics = list(res$diagnostics),
        rationale = rationale %||% NA_character_
      )
    })
}

# ---- Initial seeds (non-LLM) ----
seed_nodes <- function(method_families, registry,
                       n_per_method = 2,
                       W_grid = seq(90, 300, 15),
                       constraints = list(max_w = 0.7)) {

  map_dfr(method_families, function(mf) {
    reg <- registry[[mf]]
    tibble(
      node_id = replicate(n_per_method, new_id("seed")),
      parent_id = NA_character_,
      depth = 0,
      method_family = mf,
      W = sample(W_grid, n_per_method, replace = TRUE),
      params = map(seq_len(n_per_method), ~ reg$sampler()),
      constraints = list(constraints),
      eval_level = NA_character_,
      feasible = NA,
      sharpe = NA_real_, cvar99 = NA_real_, maxdd = NA_real_, turnover_full = NA_real_,
      diagnostics = vector("list", n_per_method),
      rationale = paste0("initial seed for method_family=", mf)
    )
  })
}

# ---- Round runner: LLM proposals + fallback -> fast -> prune -> medium -> prune ----
run_llm_round <- function(R_xts,
                          frontier_tbl,
                          llm,
                          registry,
                          method_families,
                          seeds_per_method = 1,
                          edits_per_parent = 2,
                          K_per_method = 2,
                          eps = c(0,0,0,0),
                          alpha_cvar = 0.99,
                          constraints = list(max_w = 0.7)) {

  parsed <- tryCatch(
    llm_propose_nodes(llm, frontier_tbl, method_families,
                      seeds_per_method = seeds_per_method, edits_per_parent = edits_per_parent),
    error = function(e) NULL
  )

  props_llm <- if (!is.null(parsed)) {
    make_node_rows_from_llm(parsed, frontier_tbl, method_families, constraints = constraints)
  } else {
    tibble()
  }

  message("\n=== ROUND: proposals ===")
  message("props_llm=", nrow(props_llm), " fallback=", length(method_families))
  
  props <- bind_rows(props_llm, fallback_seeds(method_families, constraints = constraints)) %>%
    dedup_by_spec()

  message("\n=== ROUND: props after dedup ===")
  print(props %>% count(method_family), n = 50)
  print(props %>% select(node_id, parent_id, method_family, W, rationale), n = 50)
  
  fast_eval <- evaluate_nodes(props, R_xts, registry, eval_level = "fast", alpha_cvar = alpha_cvar) %>%
    dedup_by_spec()

  message("\n=== FAST_EVAL summary ===")
  print(fast_eval %>% group_by(method_family) %>% 
          summarise(n=n(), best_sharpe=max(sharpe, na.rm=TRUE), best_to=min(turnover_full, na.rm=TRUE)))
  
  
  fast_keep <- prune_by_method_quota(fast_eval, K_per_method = K_per_method, eps = eps) %>%
    dedup_by_spec()

  message("\n=== FAST_KEEP (pruned) ===")
  print(fast_keep %>% select(node_id, method_family, W, sharpe, cvar99, maxdd, turnover_full, rationale), n=50)
  
  
  medium_eval <- evaluate_nodes(fast_keep, R_xts, registry, eval_level = "medium", alpha_cvar = alpha_cvar) %>%
    dedup_by_spec()

  medium_keep <- prune_by_method_quota(medium_eval, K_per_method = K_per_method, eps = eps) %>%
    dedup_by_spec()

  message("\n=== MEDIUM_KEEP (pruned) ===")
  print(medium_keep %>% select(node_id, method_family, W, sharpe, cvar99, maxdd, turnover_full, rationale), n=50)
  
  list(
    proposals = props,
    fast_eval = fast_eval,
    fast_keep = fast_keep,
    medium_keep = medium_keep
  )
}

# ---- Multi-round loop ----
run_tot <- function(R_xts,
                    llm = NULL,
                    use_llm = TRUE,
                    method_families = default_method_families() ,
                    n_rounds = 3,
                    init_seeds_per_method = 2 ,
                    seeds_per_method = 1,
                    edits_per_parent = 2,
                    K_per_method = 2,
                    eps = c(0,0,0,0),
                    alpha_cvar = 0.99 ,
                    constraints = list(max_w = 0.7) ,
                    registry = NULL ,
                    do_initial_fast_medium = TRUE) {

  stopifnot(inherits(R_xts, "xts"))

  registry <- registry %||% make_method_registry()
  if (isTRUE(use_llm)) llm <- llm %||% init_llm()

  reset_ids()

  # ---- Initial frontier ----
  frontier0 <- seed_nodes(method_families, registry,
                          n_per_method = init_seeds_per_method,
                          constraints = constraints) %>%
    dedup_by_spec()

  if (isTRUE(do_initial_fast_medium)) {
    f_fast <- evaluate_nodes(frontier0, R_xts, registry, eval_level = "fast", alpha_cvar = alpha_cvar) %>%
      dedup_by_spec()
    f_keep <- prune_by_method_quota(f_fast, K_per_method = K_per_method, eps = eps) %>%
      dedup_by_spec()
    frontier <- evaluate_nodes(f_keep, R_xts, registry, eval_level = "medium", alpha_cvar = alpha_cvar) %>%
      dedup_by_spec()
    frontier <- prune_by_method_quota(frontier, K_per_method = K_per_method, eps = eps) %>%
      dedup_by_spec()
  } else {
    frontier <- frontier0
  }

  rounds <- vector("list", if (isTRUE(use_llm)) n_rounds else 0)

  if (isTRUE(use_llm)) for (k in seq_len(n_rounds)) {
    rk <- run_llm_round(
      R_xts = R_xts,
      frontier_tbl = frontier,
      llm = llm,
      registry = registry,
      method_families = method_families,
      seeds_per_method = seeds_per_method,
      edits_per_parent = edits_per_parent,
      K_per_method = K_per_method,
      eps = eps,
      alpha_cvar = alpha_cvar,
      constraints = constraints
    )
    rounds[[k]] <- rk
    frontier <- rk$medium_keep %>% dedup_by_spec()
  }

  final_deep <- evaluate_nodes(frontier, R_xts, registry, eval_level = "deep", alpha_cvar = alpha_cvar) %>%
    dedup_by_spec() %>%
    prune_by_method_quota(K_per_method = K_per_method, eps = eps) %>%
    dedup_by_spec()

  ranked <- rank_final(final_deep)

  list(
    rounds = rounds,
    final_medium = frontier,
    final_deep = final_deep,
    ranked = ranked
  )
}

rank_final <- function(df, lam_dd = 1.0, lam_cvar = 10, lam_to = 0.25) {
  df %>%
    mutate(score = sharpe - lam_dd * (-maxdd) - lam_cvar * (-cvar99) - lam_to * turnover_full) %>%
    arrange(desc(score))
}
