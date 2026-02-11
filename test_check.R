library(dplyr)
library(purrr)
library(tibble)

`%||%` <- function(a,b) if (!is.null(a)) a else b

loss_mat <- function(df) {
  df %>% transmute(
    sharpe_loss = -sharpe,
    cvar_loss   = -cvar99,
    dd_loss     = -maxdd,
    to_loss     = turnover_full
  ) %>% as.matrix()
}

find_dominated_prunes <- function(round_obj, stage = c("fast","medium"), eps = c(0,0,0,0), n = 2) {
  stage <- match.arg(stage)
  eval_tbl <- if (stage == "fast") round_obj$fast_eval else (round_obj$medium_eval %||% round_obj$medium_keep)
  keep_tbl <- if (stage == "fast") round_obj$fast_keep else round_obj$medium_keep
  
  dropped <- eval_tbl %>% filter(!(node_id %in% keep_tbl$node_id))
  
  out <- dropped %>%
    group_by(method_family) %>%
    group_modify(function(df_drop, key) {
      mf <- key$method_family
      
      df_all <- eval_tbl %>% filter(method_family == mf)
      L_all  <- loss_mat(df_all)
      
      map_dfr(seq_len(nrow(df_drop)), function(i) {
        idx_i <- match(df_drop$node_id[i], df_all$node_id)
        dom_j <- which(map_lgl(seq_len(nrow(df_all)),
                               ~ dominates(L_all[.x, ], L_all[idx_i, ], eps = eps)))
        dom_j <- setdiff(dom_j, idx_i)
        if (length(dom_j) == 0) return(tibble())
        j <- dom_j[1]
        
        tibble(
          stage = stage,
          method_family_ = mf,   # <- not "method_family" to satisfy group_modify()
          dropped_id = df_all$node_id[idx_i],
          dominates_id = df_all$node_id[j],
          dropped_sharpe = df_all$sharpe[idx_i],
          dom_sharpe     = df_all$sharpe[j],
          dropped_cvar99 = df_all$cvar99[idx_i],
          dom_cvar99     = df_all$cvar99[j],
          dropped_maxdd  = df_all$maxdd[idx_i],
          dom_maxdd      = df_all$maxdd[j],
          dropped_to     = df_all$turnover_full[idx_i],
          dom_to         = df_all$turnover_full[j]
        )
      })
    }) %>%
    ungroup() %>%
    # rename(method_family = method_family_) %>%
    slice_head(n = n)
  
  out
}

# Example
find_dominated_prunes(out$rounds[[1]], stage = "fast", n = 6)
find_dominated_prunes(out$rounds[[2]], stage = "fast", n = 6)
find_dominated_prunes(out$rounds[[3]], stage = "fast", n = 6)


