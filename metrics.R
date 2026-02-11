# metrics.R
# Objective metrics + weight validity helpers

suppressPackageStartupMessages({
  library(xts)
})

calc_sharpe <- function(r_daily, ann_factor = 252) {
  r <- as.numeric(r_daily)
  r <- r[is.finite(r)]
  if (length(r) < 20) return(NA_real_)
  sd_r <- sd(r)
  if (!is.finite(sd_r) || sd_r == 0) return(NA_real_)
  mean(r) / sd_r * sqrt(ann_factor)
}

calc_cvar_hist <- function(r_daily, alpha = 0.99) {
  # Historical CVaR at alpha: mean of worst (1-alpha) tail
  r <- as.numeric(r_daily)
  r <- r[is.finite(r)]
  if (length(r) < 50) return(NA_real_)
  q <- unname(quantile(r, probs = 1 - alpha, type = 7, na.rm = TRUE))
  tail_r <- r[r <= q]
  if (length(tail_r) == 0) return(NA_real_)
  mean(tail_r)
}

calc_maxdd <- function(r_daily) {
  r <- as.numeric(r_daily)
  r <- r[is.finite(r)]
  if (length(r) < 20) return(NA_real_)
  equity <- cumprod(1 + r)
  peak <- cummax(equity)
  dd <- equity / peak - 1
  min(dd)
}

# Full turnover: sum(abs(w_t - w_{t-1})) per rebalance, averaged
calc_turnover_full <- function(weights_mat) {
  if (is.null(weights_mat) || nrow(weights_mat) < 2) return(NA_real_)
  diffs <- abs(weights_mat[-1, , drop = FALSE] - weights_mat[-nrow(weights_mat), , drop = FALSE])
  to_vec <- rowSums(diffs)
  mean(to_vec, na.rm = TRUE)
}

normalize_weights <- function(w, tol = 1e-12) {
  w <- as.numeric(w)
  w[!is.finite(w)] <- 0
  s <- sum(w)
  # If the weight function failed (all zeros), return NA weights so evaluator marks infeasible
  if (!is.finite(s) || s <= tol) return(rep(NA_real_, length(w)))
  w / s
}

check_constraints <- function(w, max_w = 0.7, tol = 1e-8) {
  w <- as.numeric(w)
  if (any(!is.finite(w))) return(FALSE)
  # full investment must sum ~ 1
  if (abs(sum(w) - 1) > tol) return(FALSE)
  # long-only
  if (any(w < -tol)) return(FALSE)
  if (max(w) > max_w + tol) return(FALSE)
  TRUE
}