# run.R
# Driver script for the clean ToT project
#
# Interactive:
#   setwd("tot_clean_project")
#   source("R/load_all.R")
#   llm <- init_llm()
#   out <- run_tot(R_xts, llm = llm)
#
# CLI:
#   Rscript run.R --rds path/to/R_xts.rds

# load_all.R
# Source all project modules in order (from project root)
library(tidyverse)
library(data.table)

source("utils.R")
source("metrics.R")
source("evaluator.R")
source("methods.R")
source("selection.R")
source("llm_planner.R")
source( "tot_pipeline.R")


 
state <- list(
  universe = c("SPY", "TLT", "GLD",'BTC-USD'),
  objective = "maximize_score", # Not used in current implementation; placeholder for future multi-objective extensions
  constraints = list(long_only = TRUE, max_weight = 0.7)
)

returns = getAllTickers2(state$universe,days = 1600) %>% 
  drop_na()

R_xts = xts(returns[, !"index"], order.by = returns$index)

  llm <- init_llm()
  # llm$chat('testing..')
  
  # The final run with LLM in the loop; this may take a while depending on n_rounds and LLM response times
  out <- run_tot(R_xts, llm = llm)
  
## Check rresults
  out$final_deep %>%
    mutate(
      n_oos_days   = purrr::map_int(diagnostics, ~ .x$n_oos_days %||% NA_integer_),
      n_rebalances = purrr::map_int(diagnostics, ~ .x$n_rebalances %||% NA_integer_)
    ) %>%
    select(method_family, W, sharpe,cvar99 , maxdd, turnover_full, n_oos_days, n_rebalances)
  
  out$final_deep %>%
    select(method_family, W, sharpe, cvar99, maxdd, turnover_full, rationale) %>%
    arrange(desc(sharpe))
  
  
  audit_tbl <- out$final_deep %>%
    mutate(
      vol_lb = purrr::map_int(params, ~ .x$vol_lb %||% NA_integer_),
      signal_lb = purrr::map_int(params, ~ .x$signal_lb %||% NA_integer_),
      overlay_strength = purrr::map_dbl(params, ~ .x$overlay_strength %||% NA_real_),
      smooth = purrr::map_dbl(params, ~ .x$smooth %||% NA_real_)
    ) %>%
    select(method_family, W, vol_lb, signal_lb, overlay_strength, smooth,
           sharpe, cvar99, maxdd, turnover_full)
  
  audit_tbl
  
  out$final_deep %>%
    mutate(vol_lb = purrr::map_int(params, ~ .x$vol_lb %||% NA_integer_)) %>%
    select(method_family, W, vol_lb, sharpe, cvar99, maxdd, turnover_full) %>%
    arrange(desc(sharpe))
  
  out$final_deep %>%
    mutate(
      n_oos_days   = purrr::map_int(diagnostics, ~ .x$n_oos_days %||% NA_integer_),
      n_rebalances = purrr::map_int(diagnostics, ~ .x$n_rebalances %||% NA_integer_)
    ) %>%
    select(method_family, W, sharpe, n_oos_days, n_rebalances, turnover_full)
  
  
  best <- out$ranked %>% slice(1)
  diag <- best$diagnostics[[1]]
  
  names(diag)
  str(diag, max.level = 1)
  
 