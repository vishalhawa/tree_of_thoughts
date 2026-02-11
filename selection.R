# selection.R
# Pareto pruning, exploration quota, weakness tagging

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
})

obj_to_losses <- function(sharpe, cvar99, maxdd, turnover_full) {
  c(
    sharpe_loss = -sharpe,
    cvar_loss   = -cvar99,
    dd_loss     = -maxdd,
    turnover    = turnover_full
  )
}

dominates <- function(a, b, eps = c(0,0,0,0)) {
  a <- as.numeric(a); b <- as.numeric(b)
  if (any(is.na(a)) || any(is.na(b))) return(FALSE)
  le_all <- all(a <= (b + eps))
  lt_one <- any(a <  (b - eps))
  isTRUE(le_all) && isTRUE(lt_one)
}

pareto_nondominated_idx <- function(loss_mat, eps = c(0,0,0,0)) {
  loss_mat <- as.matrix(loss_mat)
  n <- nrow(loss_mat)
  valid <- apply(loss_mat, 1, function(x) all(!is.na(x)))
  nd <- rep(FALSE, n); nd[valid] <- TRUE

  for (i in which(valid)) {
    if (!nd[i]) next
    for (j in which(valid)) {
      if (i == j) next
      if (nd[i] && dominates(loss_mat[j,], loss_mat[i,], eps = eps)) {
        nd[i] <- FALSE
        break
      }
    }
  }
  which(nd)
}

rank_key <- function(df) {
  df %>% arrange(desc(sharpe), (-cvar99), (-maxdd), turnover_full)
}

# Keep up to K_per_method; if Pareto < K then fill with best dominated (exploration quota)
prune_by_method_quota <- function(nodes_tbl, K_per_method = 2, eps = c(0,0,0,0)) {
  nodes_tbl %>%
    filter(feasible) %>%
    filter(is.finite(sharpe), is.finite(cvar99), is.finite(maxdd), is.finite(turnover_full)) %>%
    group_by(method_family) %>%
    group_modify(~{
      df <- .x
      if (nrow(df) <= K_per_method) return(df)

      loss_mat <- t(apply(df, 1, function(row) {
        obj_to_losses(
          sharpe = as.numeric(row[["sharpe"]]),
          cvar99 = as.numeric(row[["cvar99"]]),
          maxdd  = as.numeric(row[["maxdd"]]),
          turnover_full = as.numeric(row[["turnover_full"]])
        )
      }))

      nd_idx <- pareto_nondominated_idx(loss_mat, eps = eps)
      df_nd <- df[nd_idx, , drop = FALSE] %>% rank_key()

      keep <- df_nd %>% slice_head(n = min(K_per_method, nrow(df_nd)))

      if (nrow(keep) < K_per_method) {
        fill <- df %>%
          anti_join(keep, by = "node_id") %>%
          rank_key() %>%
          slice_head(n = K_per_method - nrow(keep))
        keep <- bind_rows(keep, fill)
      }

      message("\n[prune] method_family=", df$method_family[1], " n_in=", nrow(df))
      
      message("[prune] n_nondom=", nrow(df_nd), " keeping=", nrow(keep),
              if (nrow(keep) < K_per_method) paste0(" (fill ", K_per_method - nrow(keep), ")") else "")
      
      message("[prune] kept node_ids: ", paste(keep$node_id, collapse=", "))
      
      keep
    }) %>%
    ungroup()
}

weakness_profile <- function(sharpe, cvar99, maxdd, turnover_full) {
  if (!is.finite(sharpe) || !is.finite(cvar99) || !is.finite(maxdd) || !is.finite(turnover_full)) {
    return("balanced")
  }
  risk_bad <- (maxdd < -0.15) || (cvar99 < -0.03)
  sharpe_bad <- sharpe < 0.5
  turnover_bad <- turnover_full > 0.25

  if (risk_bad) return("risk")
  if (turnover_bad) return("turnover")
  if (sharpe_bad) return("sharpe")
  "balanced"
}

# Biased W edit: risk bad -> increase W; Sharpe bad -> decrease W; else random
edit_W_biased <- function(W, sharpe, cvar99, maxdd, turnover_full, W_min = 90, W_max = 300) {
  risk_bad <- is.finite(maxdd) && maxdd < -0.20 || is.finite(cvar99) && cvar99 < -0.03
  sharpe_bad <- is.finite(sharpe) && sharpe < 0

  if (risk_bad) {
    delta <- sample(c(15, 30, 45), 1)
  } else if (sharpe_bad) {
    delta <- sample(c(-15, -30, -45), 1)
  } else {
    delta <- sample(c(-45,-30,-15,15,30,45), 1)
  }
  pmin(W_max, pmax(W_min, W + delta))
}
