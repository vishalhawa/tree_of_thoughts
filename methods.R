# methods.R
# Portfolio construction method families + registry

suppressPackageStartupMessages({
  library(xts)
})

# ---- PortfolioAnalytics / ROI methods ----
check_pa_pkgs <- function() {
  req <- c("PortfolioAnalytics", "PerformanceAnalytics", "ROI",
           "ROI.plugin.quadprog", "ROI.plugin.glpk")
  missing <- req[!vapply(req, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing packages: ", paste(missing, collapse = ", "),
         "\nInstall them, e.g.: install.packages(c('", paste(missing, collapse="','"), "'))")
  }
  TRUE
}

make_portfolio_spec <- function(assets, max_w = 0.7) {
  PA <- getNamespace("PortfolioAnalytics")
  p <- PA$portfolio.spec(assets = assets)
  p <- PA$add.constraint(portfolio = p, type = "full_investment")
  p <- PA$add.constraint(portfolio = p, type = "long_only")
  p <- PA$add.constraint(portfolio = p, type = "box", min = 0, max = max_w)
  p
}

make_weight_fn_minvar_pa <- function(constraints) {
  check_pa_pkgs()
  function(R_window, spec = list()) {
    PA <- getNamespace("PortfolioAnalytics")
    assets <- colnames(R_window)
    
    p <- make_portfolio_spec(assets, max_w = constraints$max_w %||% 0.7)
    p <- PA$add.objective(portfolio = p, type = "risk", name = "var")
    
    opt <- tryCatch(
      PA$optimize.portfolio(R = R_window, portfolio = p, optimize_method = "ROI", trace = FALSE),
      error = function(e) NULL
    )
    w_raw <- tryCatch(if (!is.null(opt)) PA$extractWeights(opt) else NULL, error = function(e) NULL)
    
    if (is.null(w_raw)) return(rep(1/length(assets), length(assets)))
    w <- as.numeric(w_raw[assets])
    project_weights(w, max_w = constraints$max_w %||% 0.7)
  }
}

make_weight_fn_qu_pa <- function(params, constraints) {
  check_pa_pkgs()
  function(R_window, spec = list()) {
    PA <- getNamespace("PortfolioAnalytics")
    assets <- colnames(R_window)
    ra <- params$risk_aversion %||% 0.25
    
    p <- make_portfolio_spec(assets, max_w = constraints$max_w %||% 0.7)
    p <- PA$add.objective(portfolio = p, type = "return", name = "mean")
    p <- PA$add.objective(portfolio = p, type = "risk", name = "var", risk_aversion = ra)
    
    opt <- tryCatch(
      PA$optimize.portfolio(R = R_window, portfolio = p, optimize_method = "ROI", trace = FALSE),
      error = function(e) NULL
    )
    w_raw <- tryCatch(if (!is.null(opt)) PA$extractWeights(opt) else NULL, error = function(e) NULL)
    
    if (is.null(w_raw)) return(rep(1/length(assets), length(assets)))
    w <- as.numeric(w_raw[assets])
    project_weights(w, max_w = constraints$max_w %||% 0.7)
  }
}

make_weight_fn_etl99_pa <- function(constraints) {
  check_pa_pkgs()
  function(R_window, spec = list()) {
    PA <- getNamespace("PortfolioAnalytics")
    assets <- colnames(R_window)
    
    p <- make_portfolio_spec(assets, max_w = constraints$max_w %||% 0.7)
    p <- PA$add.objective(portfolio = p, type = "risk", name = "ETL", arguments = list(p = 0.99))
    
    opt <- tryCatch(
      PA$optimize.portfolio(R = R_window, portfolio = p, optimize_method = "ROI", trace = FALSE),
      error = function(e) NULL
    )
    w_raw <- tryCatch(if (!is.null(opt)) PA$extractWeights(opt) else NULL, error = function(e) NULL)
    
    if (is.null(w_raw)) return(rep(1/length(assets), length(assets)))
    w <- as.numeric(w_raw[assets])
    project_weights(w, max_w = constraints$max_w %||% 0.7)
  }
}

# ---- Projection helper (long-only, max_w cap, sum=1) ----
project_weights <- function(w, max_w = 0.7, tol = 1e-12) {
  w <- as.numeric(w); w[!is.finite(w)] <- 0
  w <- pmax(w, 0)
  if (sum(w) <= tol) w <- rep(1/length(w), length(w)) else w <- w/sum(w)
  for (iter in 1:20) {
    if (max(w) <= max_w + tol) break
    over <- w > max_w + tol
    w[over] <- max_w
    s <- sum(w)
    if (s > 1 + tol) { w <- w/s; break }
    leftover <- 1 - s
    under <- !over
    cap_room <- pmax(max_w - w, 0)
    room_sum <- sum(cap_room[under])
    if (room_sum <= tol) { w <- w/sum(w); break }
    add <- leftover * (cap_room / sum(cap_room))
    w <- pmin(max_w, w + add)
    w <- w / sum(w)
  }
  w
}

# ---- Proxy methods (cheap, interpretable) ----
w_risk_parity_proxy <- function(R_window, params, constraints) {
  X <- as.matrix(R_window)
  vol_lb <- params$vol_lb %||% 120
  vol_lb <- as.integer(vol_lb)
  vol_lb <- max(60, min(252, vol_lb))
  vol_lb <- min(vol_lb, nrow(X))

  Xs <- X[(nrow(X) - vol_lb + 1):nrow(X), , drop = FALSE]
  vol <- apply(Xs, 2, sd, na.rm = TRUE)
  score <- 1 / pmax(vol, 1e-9)
  project_weights(score, max_w = constraints$max_w %||% 0.7)
}

w_trend_overlay_proxy <- function(R_window, params, constraints) {
  signal_lb <- params$signal_lb %||% 120
  overlay_strength <- params$overlay_strength %||% 0.2
  smooth <- params$smooth %||% 0.2

  X <- as.matrix(R_window)
  lb <- min(as.integer(signal_lb), nrow(X))
  Xs <- X[(nrow(X) - lb + 1):nrow(X), , drop = FALSE]
  mom <- apply(Xs, 2, function(r) prod(1 + r, na.rm = TRUE) - 1)
  mom <- pmax(as.numeric(mom), 0)
  w_mom <- if (sum(mom) <= 1e-12) rep(1/ncol(X), ncol(X)) else mom / sum(mom)

  # baseline: risk parity with default vol_lb=120
  w_base <- w_risk_parity_proxy(R_window, params = list(vol_lb = 120), constraints = constraints)

  w_mix <- (1 - overlay_strength) * w_base + overlay_strength * w_mom
  w_eq <- rep(1/length(w_mix), length(w_mix))
  w_mix <- (1 - smooth) * w_mix + smooth * w_eq

  project_weights(w_mix, max_w = constraints$max_w %||% 0.7)
}

default_method_families <- function() {
  c("min_var_pa","quad_utility_pa","etl99_pa","risk_parity","trend_overlay")
}

make_method_registry <- function(vol_lb_grid = c(60, 90, 120, 180, 220, 252)) {
  list(
    min_var_pa = list(
      sampler = function() list(),
      editor  = function(params) params,
      weight_fn_factory = function(params, constraints) make_weight_fn_minvar_pa(constraints)
    ),
    quad_utility_pa = list(
      sampler = function() list(risk_aversion = sample(c(0.05, 0.10, 0.25, 0.50, 1.0), 1)),
      editor  = function(params) {
        mult <- sample(c(1/1.5, 1.5), 1)
        params$risk_aversion <- max(0.02, min(2.0, (params$risk_aversion %||% 0.25) * mult))
        params
      },
      weight_fn_factory = function(params, constraints) make_weight_fn_qu_pa(params, constraints)
    ),
    etl99_pa = list(
      sampler = function() list(),
      editor  = function(params) params,
      weight_fn_factory = function(params, constraints) make_weight_fn_etl99_pa(constraints)
    ),
    risk_parity = list(
      sampler = function() list(vol_lb = sample(vol_lb_grid, 1)),
      editor  = function(params) {
        params$vol_lb <- sample(vol_lb_grid, 1)
        params
      },
      weight_fn_factory = function(params, constraints) {
        function(R_window, spec = list()) w_risk_parity_proxy(R_window, params, constraints)
      }
    ),
    trend_overlay = list(
      sampler = function() list(
        signal_lb = sample(c(60, 120, 180, 252), 1),
        overlay_strength = sample(seq(0.10, 0.40, 0.10), 1),
        smooth = sample(c(0, 0.2, 0.4), 1)
      ),
      editor  = function(params) {
        choice <- sample(c("signal_lb","overlay_strength","smooth"), 1)
        if (choice == "signal_lb") params$signal_lb <- max(60, min(252, (params$signal_lb %||% 120) + sample(c(-20, 20), 1)))
        if (choice == "overlay_strength") params$overlay_strength <- max(0.05, min(0.40, (params$overlay_strength %||% 0.2) + sample(c(-0.05, 0.05), 1)))
        if (choice == "smooth") params$smooth <- max(0, min(0.5, (params$smooth %||% 0.2) + sample(c(-0.1, 0.1), 1)))
        params
      },
      weight_fn_factory = function(params, constraints) {
        function(R_window, spec = list()) w_trend_overlay_proxy(R_window, params, constraints)
      }
    )
  )
}
