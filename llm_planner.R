# llm_planner.R
# LLM proposals + param sanitization + conversion to node rows

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
  library(jsonlite)
})

library(ellmer)
library(yfR)

Sys.setenv(OPENAI_API_KEY = 'API-KEY')



# ellmer is optional; only required if you call init_llm() / llm_propose_nodes()
init_llm <- function() {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Package 'ellmer' is required for LLM mode. Install it first.")
  }
  ellmer::chat_openai()
}

# ---- Param sanitizers per method family ----
sanitize_params <- function(method_family, params) {
  params <- params %||% list()
  if (!is.list(params)) params <- list()

  if (method_family %in% c("min_var_pa","etl99_pa")) return(list())

  VOL_LB_GRID <- c(60, 90, 120, 180, 220, 252)

  if (method_family == "risk_parity") {
    vol_lb <- params$vol_lb %||% 120
    vol_lb <- max(min(vol_lb, 252), 60)
    vol_lb <- snap_to_grid(vol_lb, VOL_LB_GRID)
    return(list(vol_lb = as.integer(vol_lb)))
  }

  if (method_family == "quad_utility_pa") {
    ra <- params$risk_aversion %||% 0.25
    ra <- max(0.02, min(2.0, as.numeric(ra)))
    return(list(risk_aversion = ra))
  }

  if (method_family == "trend_overlay") {
    signal_lb <- params$signal_lb %||% 120
    overlay_strength <- params$overlay_strength %||% 0.2
    smooth <- params$smooth %||% 0.2

    signal_lb <- max(60, min(252, as.integer(signal_lb)))
    overlay_strength <- max(0.05, min(0.40, as.numeric(overlay_strength)))
    smooth <- max(0, min(0.5, as.numeric(smooth)))

    return(list(
      signal_lb = signal_lb,
      overlay_strength = overlay_strength,
      smooth = smooth
    ))
  }

  list()
}

# ---- JSON extraction ----
extract_json <- function(txt) {
  out <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.null(out)) return(out)
  m <- regmatches(txt, regexpr("\\{[\\s\\S]*\\}", txt))
  if (length(m) == 0 || nchar(m) < 2) return(NULL)
  tryCatch(jsonlite::fromJSON(m, simplifyVector = FALSE), error = function(e) NULL)
}

build_llm_prompt <- function(frontier_tbl, method_families, seeds_per_method = 1, edits_per_parent = 2) {
  parents <- frontier_tbl %>%
    mutate(weakness = pmap_chr(list(sharpe, cvar99, maxdd, turnover_full),
                              ~ weakness_profile(..1, ..2, ..3, ..4))) %>%
    select(node_id, method_family, W, sharpe, cvar99, maxdd, turnover_full, weakness)

  message("\n=== PARENTS (with weakness) ===")
  print(parents, n = 50)
  
  grammar <- list(
    W = list(min = 90, max = 300, step = 15),
    method_families = list(
      min_var_pa = list(params = list()),
      etl99_pa = list(params = list()),
      risk_parity = list(params = list(vol_lb = list(min = 60, max = 252))),
      quad_utility_pa = list(params = list(risk_aversion = list(min = 0.02, max = 2.0))),
      trend_overlay = list(params = list(
        signal_lb = list(min = 60, max = 252),
        overlay_strength = list(min = 0.05, max = 0.40),
        smooth = list(min = 0.0, max = 0.5)
      ))
    )
  )

  paste0(
    "You are proposing candidate portfolio construction nodes for a Tree-of-Thoughts search.\n",
    "Return ONLY valid JSON (no markdown). Follow the grammar exactly.\n\n",
    "Goal: multi-objective improvement (Sharpe up; CVaR99 less negative; MaxDD less negative; Turnover down).\n",
    "We rebalance monthly, long-only, max position weight 0.7. Rolling window W in [90,300] step 15.\n\n",
    "METHOD FAMILIES (diversity axis): ", paste(method_families, collapse = ", "), "\n",
    "Exploration quota: propose at least ", seeds_per_method, " SEED per method family.\n",
    "Also propose ", edits_per_parent, " EDITS for each parent listed.\n\n",
    "GRAMMAR:\n",
    jsonlite::toJSON(grammar, auto_unbox = TRUE, pretty = TRUE), "\n\n",
    "PARENTS (current survivors):\n",
    jsonlite::toJSON(parents, auto_unbox = TRUE, pretty = TRUE), "\n\n",
    "OUTPUT JSON SCHEMA:\n",
    "{\n",
    '  "seeds": [\n',
    '    {"method_family": "...", "W": 180, "params": {...}, "rationale": "..."},\n',
    "    ...\n",
    "  ],\n",
    '  "edits": [\n',
    '    {"parent_id": "n00001", "method_family": "...", "W": 195, "params": {...}, "rationale": "..."},\n',
    "    ...\n",
    "  ]\n",
    "}\n\n",
    "Rules:\n",
    "- W must be multiple of 15 between 90 and 300.\n",
    "- params must only contain allowed keys for that method_family.\n",
    "- risk_parity vol_lb should be one of 60/90/120/180/220/252.\n",
    "- Each rationale must mention which weakness it targets (risk/sharpe/turnover/balanced).\n"
  )
}

llm_propose_nodes <- function(llm, frontier_tbl, method_families,
                              seeds_per_method = 1, edits_per_parent = 2) {
  prompt <- build_llm_prompt(frontier_tbl, method_families, seeds_per_method, edits_per_parent)

  # message("\n=== LLM PROMPT (head) ===")
  # cat(substr(prompt, 1, 2000), "\n")
  
  txt <- llm$chat(prompt, echo = 'none')
  parsed <- extract_json(txt)
  if (is.null(parsed)) stop("LLM response could not be parsed as JSON.")
  
  message("\n=== LLM PARSED KEYS ===")
  # print(names(parsed))
  message("n_seeds=", length(parsed$seeds %||% list()), "  n_edits=", length(parsed$edits %||% list()))
  
  parsed
}

# ---- Convert LLM proposals into node tibble ----
make_node_rows_from_llm <- function(parsed, frontier_tbl, method_families,
                                    constraints = list(max_w = 0.7)) {
  valid_mf <- function(mf) is.character(mf) && length(mf) == 1 && mf %in% method_families

  parent_depth_map <- frontier_tbl %>% select(node_id, depth)

  # Seeds
  seeds <- parsed$seeds %||% list()
  seed_tbl <- map_dfr(seeds, function(s) {
    mf <- s$method_family
    if (!valid_mf(mf)) return(tibble())

    W <- snap_W(s$W %||% 180)
    params <- sanitize_params(mf, s$params %||% list())
    rationale <- as.character(s$rationale %||% paste0("seed for ", mf))

    tibble(
      node_id = new_id("s"),
      parent_id = NA_character_,
      depth = 0,
      method_family = mf,
      W = W,
      params = list(params),
      constraints = list(constraints),
      eval_level = NA_character_,
      feasible = NA,
      sharpe = NA_real_, cvar99 = NA_real_, maxdd = NA_real_, turnover_full = NA_real_,
      diagnostics = list(list()),
      rationale = rationale
    )
  })

  # Edits
  edits <- parsed$edits %||% list()
  edit_tbl <- map_dfr(edits, function(e) {
    pid <- e$parent_id
    mf <- e$method_family
    if (!valid_mf(mf)) return(tibble())

    W <- snap_W(e$W %||% 180)
    params <- sanitize_params(mf, e$params %||% list())
    rationale <- as.character(e$rationale %||% paste0("edit targeting ", pid))

    pdepth <- parent_depth_map$depth[match(pid, parent_depth_map$node_id)] %||% 0

    tibble(
      node_id = new_id("e"),
      parent_id = pid,
      depth = pdepth + 1,
      method_family = mf,
      W = W,
      params = list(params),
      constraints = list(constraints),
      eval_level = NA_character_,
      feasible = NA,
      sharpe = NA_real_, cvar99 = NA_real_, maxdd = NA_real_, turnover_full = NA_real_,
      diagnostics = list(list()),
      rationale = rationale
    )
  })

  message("\n=== LLM SEEDS (sanitized) ===")
  print(seed_tbl %>% select(node_id, method_family, W, rationale), n = 50)
  
  message("\n=== LLM EDITS (sanitized) ===")
  print(edit_tbl %>% select(node_id, parent_id, method_family, W, rationale), n = 50)
  
  bind_rows(seed_tbl, edit_tbl)
}

# ---- Deterministic fallback seeds (guarantee coverage) ----
fallback_seeds <- function(method_families, constraints = list(max_w = 0.7)) {
  map_dfr(method_families, function(mf) {
    params <- sanitize_params(mf, list())
    if (mf == "quad_utility_pa") params <- list(risk_aversion = 0.25)
    if (mf == "trend_overlay") params <- list(signal_lb = 120, overlay_strength = 0.2, smooth = 0.2)

    tibble(
      node_id = new_id("fs"),
      parent_id = NA_character_,
      depth = 0,
      method_family = mf,
      W = sample(seq(90, 300, 15), 1),
      params = list(params),
      constraints = list(constraints),
      eval_level = NA_character_,
      feasible = NA,
      sharpe = NA_real_, cvar99 = NA_real_, maxdd = NA_real_, turnover_full = NA_real_,
      diagnostics = list(list()),
      rationale = paste0("fallback seed for exploration quota in method_family=", mf)
    )
  })
}
