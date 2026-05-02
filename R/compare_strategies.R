compare_strategies <- function(sim, strategy_list) {

  strategy_names <- names(strategy_list)
  n_strat <- length(strategy_names)

  results <- vector("list", n_strat)

  start_time <- Sys.time()

  for (i in seq_along(strategy_names)) {

    name <- strategy_names[i]
    dt <- strategy_list[[name]]

    cat("\n----------------------------------------\n")
    cat("Running strategy", i, "of", n_strat, ":", name, "\n")

    iter_start <- Sys.time()

    res <- sim %>%
      assign_positions() %>%
      assign_player_types() %>%
      add_type_modifiers() %>%
      apply_decision_table(dt) %>%   # PASS 1 → creates remaining_*
      add_adjusted_metrics() %>%     # now can compute adj_*_pc
      compute_investment() %>%       # creates price_*
      add_adjusted_metrics() %>%     # now adds price-aware metrics
      apply_decision_table(dt) %>%   # PASS 2 (price-aware decisions)
      compute_investment() %>%       # recompute after new decisions

      classify_outcomes() %>%
      calculate_ev()

    summary <- summarise_ev(res)
    summary$strategy <- name

    results[[i]] <- summary

    iter_time <- Sys.time() - iter_start
    total_time <- Sys.time() - start_time

    cat("Completed:", name, "\n")
    cat("Iteration time:", round(iter_time, 2), "minutes\n")
    cat("Total elapsed:", round(total_time, 2), "minutes\n")
  }

  cat("\nAll strategies complete.\n")

  dplyr::bind_rows(results) %>%
    dplyr::select(strategy, dplyr::everything())
}
