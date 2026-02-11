# utils.R
# Small utilities shared across modules

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tibble)
  library(digest)
})
library(yfR)

`%||%` <- function(a, b) if (!is.null(a)) a else b

getAllTickers2 <- function(symbols,period='daily',thresh=0.5,days = 400){
  start = Sys.Date()-days ;  
  quotes_hist = yf_get(symbols, first_date = start, last_date= Sys.Date(), thresh_bad_data = thresh, bench_ticker = "SPY", do_cache = F,do_parallel = F, be_quiet = T) %>% 
    as.data.table() %>% 
    transmute(index= ref_date,open=price_open,high= price_high, low=price_low, ClosePrice=price_close  , Volume=volume  ,symbol=ticker,returns=round(ret_closing_prices,4)) %>% 
    drop_na() %>% 
    dcast(
      index ~ symbol,
      value.var = "returns"
    ) 
}

# ---- IDs (stable, collision-free within session) ----
.tot_id_env <- new.env(parent = emptyenv())
.tot_id_env$i <- 0L

reset_ids <- function() { .tot_id_env$i <- 0L; invisible(TRUE) }

new_id <- function(prefix = "n") {
  .tot_id_env$i <- .tot_id_env$i + 1L
  sprintf("%s%05d", prefix, .tot_id_env$i)
}

# ---- listcol unwrap (handles params/constraints list-cols robustly) ----
unwrap_listcol <- function(x) {
  if (is.null(x)) return(list())
  if (!is.list(x)) return(list(x))
  if (length(x) == 0) return(list())
  if (length(x) == 1 && is.list(x[[1]])) return(x[[1]])
  x
}

# ---- spec signature + dedup (ignore id/rationale; dedup on method/W/params/constraints) ----
spec_sig <- function(method_family, W, params, constraints) {
  p <- unwrap_listcol(params)
  c <- unwrap_listcol(constraints)
  digest::digest(
    list(
      method_family = method_family,
      W = as.integer(W),
      params = p,
      max_w = c$max_w %||% 0.7
    ),
    algo = "xxhash64"
  )
}

dedup_by_spec <- function(df) {
  df %>%
    mutate(.spec_sig = pmap_chr(list(method_family, W, params, constraints),
                                ~ spec_sig(..1, ..2, ..3, ..4))) %>%
    distinct(.spec_sig, .keep_all = TRUE) %>%
    select(-.spec_sig)
}

# ---- snapping helpers ----
snap_W <- function(W, W_min = 90, W_max = 300, step = 15) {
  W <- as.numeric(W)
  W <- pmin(W_max, pmax(W_min, W))
  round(W / step) * step
}

snap_to_grid <- function(x, grid) {
  x <- as.numeric(x)
  grid[which.min(abs(grid - x))]
}
