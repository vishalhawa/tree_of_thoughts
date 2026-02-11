# evaluator.R
# Walk-forward evaluator with monthly rebalances and configurable OOS limit

suppressPackageStartupMessages({
  library(xts)
})

# source dependencies: utils.R + metrics.R

weight_fn_equal <- function(R_window, spec = list()) {
  n <- ncol(R_window)
  rep(1 / n, n)
}

evaluate_strategy_walkforward <- function(R_daily_xts,
                                         W = 180,
                                         weight_fn = weight_fn_equal,
                                         constraints = list(max_w = 0.7),
                                         alpha_cvar = 0.99,
                                         oos_months_limit = Inf) {
  stopifnot(inherits(R_daily_xts, "xts"))
  if (nrow(R_daily_xts) < (W + 40)) stop("Not enough rows for W + evaluation period.")

  R_daily_xts <- R_daily_xts[complete.cases(R_daily_xts), ]
  assets <- colnames(R_daily_xts)

  ep <- endpoints(R_daily_xts, on = "months")
  valid_i <- which(ep >= W)
  valid_i <- valid_i[valid_i < length(ep)]
  if (length(valid_i) < 2) stop("Not enough monthly endpoints for rolling evaluation with given W.")

  if (is.finite(oos_months_limit) && oos_months_limit > 0) {
    valid_i <- tail(valid_i, oos_months_limit)
  }

  weights_list <- list()
  oos_returns <- c()
  oos_dates <- c()

  for (i in valid_i) {
    train_end <- ep[i]
    hold_start <- train_end + 1
    hold_end <- ep[i + 1]
    if (hold_start > hold_end) next

    train_start <- train_end - W + 1
    R_window <- R_daily_xts[train_start:train_end, , drop = FALSE]

    w <- tryCatch(weight_fn(R_window, spec = list(W = W, assets = assets)),
                  error = function(e) rep(NA_real_, length(assets)))

    w <- normalize_weights(w)

    if (!check_constraints(w, max_w = constraints$max_w %||% 0.7)) {
      return(list(
        feasible = FALSE,
        objectives = list(sharpe = NA_real_, cvar99 = NA_real_, maxdd = NA_real_, turnover_full = NA_real_),
        diagnostics = list(reason = "constraint_violation_or_invalid_weights")
      ))
    }

    R_hold <- R_daily_xts[hold_start:hold_end, , drop = FALSE]
    rp <- as.numeric(R_hold %*% matrix(w, ncol = 1))

    oos_returns <- c(oos_returns, rp)
    oos_dates <- c(oos_dates, index(R_hold))
    weights_list[[length(weights_list) + 1]] <- w
  }

  if (length(oos_returns) < 50) {
    return(list(
      feasible = FALSE,
      objectives = list(sharpe = NA_real_, cvar99 = NA_real_, maxdd = NA_real_, turnover_full = NA_real_),
      diagnostics = list(reason = "insufficient_oos_returns")
    ))
  }

  r_oos <- xts(oos_returns, order.by = as.Date(oos_dates))
  weights_mat <- do.call(rbind, weights_list)
  colnames(weights_mat) <- assets

  list(
    feasible = TRUE,
    objectives = list(
      sharpe = calc_sharpe(r_oos),
      cvar99 = calc_cvar_hist(r_oos, alpha = alpha_cvar),
      maxdd  = calc_maxdd(r_oos),
      turnover_full = calc_turnover_full(weights_mat)
    ),
    diagnostics = list(
      n_oos_days = NROW(r_oos),
      n_rebalances = nrow(weights_mat),
      max_weight_observed = max(weights_mat, na.rm = TRUE)
    )
  )
}
