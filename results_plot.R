## PLotting results of the model
library(dplyr)
library(purrr)
library(tidyr)
library(xts)
library(ggplot2)

# uses utils.R (%||%), metrics.R (normalize_weights, check_constraints)
# uses methods.R registry + evaluator.R logic

make_ew_buyhold_value <- function(R_xts, notional = 100) {
  stopifnot(inherits(R_xts, "xts"))
  R_xts <- R_xts[complete.cases(R_xts), ]
  R <- coredata(R_xts)
  n <- ncol(R)
  
  # start with equal dollars, then let weights drift (no rebal)
  holdings <- rep(notional / n, n)
  vals <- numeric(nrow(R))
  
  for (t in seq_len(nrow(R))) {
    holdings <- holdings * (1 + R[t, ])
    vals[t] <- sum(holdings)
  }
  xts(vals, order.by = as.Date(index(R_xts)))
}


walkforward_series <- function(R_daily_xts,
                               W,
                               weight_fn,
                               constraints = list(max_w = 0.7),
                               alpha_cvar = 0.99,
                               oos_months_limit = Inf) {
  stopifnot(inherits(R_daily_xts, "xts"))
  R_daily_xts <- R_daily_xts[complete.cases(R_daily_xts), ]
  assets <- colnames(R_daily_xts)
  
  ep <- endpoints(R_daily_xts, on = "months")
  valid_i <- which(ep >= W)
  valid_i <- valid_i[valid_i < length(ep)]
  if (is.finite(oos_months_limit) && oos_months_limit > 0) valid_i <- tail(valid_i, oos_months_limit)
  
  weights_list <- list()
  weight_dates <- c()
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
    
    if (!check_constraints(w, max_w = constraints$max_w %||% 0.7)) next
    
    R_hold <- R_daily_xts[hold_start:hold_end, , drop = FALSE]
    rp <- as.numeric(R_hold %*% matrix(w, ncol = 1))
    
    oos_returns <- c(oos_returns, rp)
    oos_dates <- c(oos_dates, index(R_hold))
    
    weights_list[[length(weights_list) + 1]] <- w
    weight_dates <- c(weight_dates, index(R_hold)[1])  # first day of the month hold
  }
  
  r_oos <- xts(oos_returns, order.by = as.Date(oos_dates))
  w_mat <- do.call(rbind, weights_list); colnames(w_mat) <- assets
  w_xts <- xts(w_mat, order.by = as.Date(weight_dates))
  
  list(r_oos = r_oos, weights_xts = w_xts, rebalance_dates = as.Date(weight_dates))
}

# --- Build $ paths for all final_deep portfolios ---
final_tbl <- out$final_deep
registry  <- make_method_registry()

bt_list <- final_tbl %>%
  mutate(
    label = paste0(node_id, " | ", method_family, " | W=", W),
    bt = pmap(list(W, method_family, params, constraints), function(W, method_family, params, constraints) {
      wf <- registry[[method_family]]$weight_fn_factory(params, constraints)
      walkforward_series(R_xts, W = W, weight_fn = wf, constraints = constraints, oos_months_limit = Inf)
    })
  )

paths_df <- bt_list %>%
  transmute(label, value_xts = map(bt, ~ 100 * cumprod(1 + .x$r_oos))) %>%
  mutate(df = map2(label, value_xts, ~ tibble(date = as.Date(index(.y)), label = .x, value = as.numeric(.y)))) %>%
  select(df) %>%
  unnest(df)

rng <- range(paths_df$date)
R_sub <- R_xts[paste0(rng[1], "/", rng[2])]

ew_val <- make_ew_buyhold_value(R_sub, notional = 100)

ew_df <- tibble(
  date = as.Date(index(ew_val)),
  label = "EW_buy_hold (no rebalance)",
  value = as.numeric(ew_val)
)

paths_df2 <- bind_rows(paths_df, ew_df)

# rebalance points (monthly)
reb_df <- bt_list %>%
  transmute(label,
            value_xts = map(bt, ~ 100 * cumprod(1 + .x$r_oos)),
            rebd = map(bt, ~ .x$rebalance_dates)) %>%
  mutate(df = pmap(list(label, value_xts, rebd), function(label, value_xts, rebd) {
    vv <- value_xts[rebd]
    tibble(date = as.Date(index(vv)), label = label, value = as.numeric(vv))
  })) %>%
  select(df) %>%
  unnest(df)

# --- Plot all portfolios on one chart ---
ggplot(paths_df2, aes(date, value, color = label)) +
  geom_line(linewidth = 0.6, alpha = 0.85) +
  geom_point(data = reb_df, aes(date, value, color = label), size = 0.6, alpha = 0.35) +
  labs(title = "Final Deep Portfolios vs Equal-Weight Buy&Hold ($100)",
       x = NULL, y = "Portfolio value ($)") +
  theme_minimal() +
  theme(legend.position = "right")

# -- plotr weights vencotr 
w_df <- bt_list %>%
  transmute(label, w = map(bt, "weights_xts")) %>%
  mutate(df = map2(label, w, ~{
    Wts <- .y
    if (is.null(Wts) || NROW(Wts) == 0) return(tibble())
    as_tibble(coredata(Wts)) %>%
      mutate(date = as.Date(index(Wts)), label = .x) %>%
      relocate(date, label)
  })) %>%
  select(df) %>% unnest(df) %>%
  pivot_longer(cols = -c(date, label), names_to = "asset", values_to = "weight")

ggplot(w_df, aes(date, weight, fill = asset)) +
  geom_area(alpha=0.75) +
  facet_wrap(~label, ncol = 2) +
  labs(title = "Monthly weights (rebalance dates) — final_deep portfolios",
       x = NULL, y = "Weight") +
  theme_minimal() +
  theme(legend.position = "bottom")

## EW portfolio
# Assumes you already sourced: utils.R, metrics.R, and have objects: out, R_xts
 

eval_ew_buyhold <- function(R_xts, out, notional = 100,
                            W_ref = out$ranked$W[1],
                            alpha_cvar = 0.99,
                            lam_dd = 1.0, lam_cvar = 10, lam_to = 0.25) {
  
  R <- R_xts[complete.cases(R_xts), ]
  
  # ---- match evaluator.R OOS start logic using W_ref ----
  ep <- endpoints(R, on = "months")
  valid_i <- which(ep >= W_ref)
  valid_i <- valid_i[valid_i < length(ep)]
  if (length(valid_i) < 2) stop("Not enough monthly endpoints for given W_ref.")
  oos_start_row <- ep[min(valid_i)] + 1
  R_oos <- R[oos_start_row:nrow(R), , drop = FALSE]
  
  # ---- equal-dollar buy & hold (no rebal) value path ----
  n <- ncol(R_oos)
  holdings <- rep(notional / n, n)
  vals <- numeric(nrow(R_oos))
  X <- coredata(R_oos)
  
  for (t in seq_len(nrow(R_oos))) {
    holdings <- holdings * (1 + X[t, ])
    vals[t] <- sum(holdings)
  }
  val_xts <- xts(vals, order.by = as.Date(index(R_oos)))
  r_p <- val_xts / lag(val_xts) - 1
  r_p <- r_p[!is.na(r_p)]
  
  sharpe <- calc_sharpe(r_p)
  cvar99 <- calc_cvar_hist(r_p, alpha = alpha_cvar)
  maxdd  <- calc_maxdd(r_p)
  turnover_full <- 0  # no rebalancing => no turnover by definition here
  
  score <- sharpe - lam_dd * (-maxdd) - lam_cvar * (-cvar99) - lam_to * turnover_full
  
  tibble(
    method_family = "equal_weight_buy_hold",
    W_ref = W_ref,
    sharpe = sharpe,
    cvar99 = cvar99,
    maxdd = maxdd,
    turnover_full = turnover_full,
    score = score
  )
}

ew_bh_obj <- eval_ew_buyhold(R_xts, out)
ew_bh_obj
