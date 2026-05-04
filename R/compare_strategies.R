compare_strategies <- function(sim, strategy_list) {

  strategy_names <- names(strategy_list)
  n_strat <- length(strategy_names)

  results <- vector("list", n_strat)

  start_time <- Sys.time()

  for (i in seq_along(strategy_names)) {

    name <- strategy_names[i]

    # 🔥 KEY CHANGE: separate focus vs rivals
    dt_focus <- strategy_list[[name]]
    dt_rival <- rival_strategies[["baseline"]]   # fixed rival environment

    cat("\n----------------------------------------\n")
    cat("Running strategy", i, "of", n_strat, ":", name, "\n")

    iter_start <- Sys.time()

    res <- sim %>%
      assign_positions() %>%
      assign_player_types() %>%
      add_type_modifiers() %>%

      # 🔥 PASS 1
      apply_decision_table(dt_focus, dt_rival) %>%

      add_adjusted_metrics() %>%
      compute_investment() %>%
      add_adjusted_metrics() %>%

      # 🔥 PASS 2
      apply_decision_table(dt_focus, dt_rival) %>%
      compute_investment() %>%

      classify_outcomes() %>%
      calculate_ev()

    # -------------------------------
    # CORE SUMMARY
    # -------------------------------
    summary <- summarise_ev(res)

    # -------------------------------
    # WIN TYPE COUNTS
    # -------------------------------
    win_counts <- res %>%
      dplyr::filter(is_focus) %>%
      dplyr::count(win_type) %>%
      tidyr::pivot_wider(
        names_from = win_type,
        values_from = n,
        values_fill = 0
      )

    # -------------------------------
    # DIAGNOSTICS
    # -------------------------------
    diagnostics <- res %>%
      dplyr::filter(is_focus) %>%
      dplyr::summarise(

        showdown_rate = mean(win_type == "showdown_win"),
        fold_rate     = mean(grepl("fold_win", win_type)),

        fold_preflop_rate = mean(win_type == "fold_win_preflop"),
        fold_flop_rate    = mean(win_type == "fold_win_flop"),
        fold_turn_rate    = mean(win_type == "fold_win_turn"),
        fold_river_rate   = mean(win_type == "fold_win_river"),

        avg_invested = mean(invested, na.rm = TRUE),
        avg_loss     = mean(ev[ev < 0], na.rm = TRUE),
        avg_win      = mean(ev[ev > 0], na.rm = TRUE),

        loss_rate = mean(win_type == "loss")
      )

    # -------------------------------
    # COMBINE
    # -------------------------------
    summary <- summary %>%
      dplyr::bind_cols(win_counts) %>%
      dplyr::bind_cols(diagnostics)

    summary$strategy <- name

    results[[i]] <- summary

    iter_time <- Sys.time() - iter_start
    total_time <- Sys.time() - start_time

    cat("Completed:", name, "\n")
    cat("Iteration start time:", as.character(Sys.time()), "\n")
  }

  cat("\nAll strategies complete at.", as.character(Sys.time()), "\n")

  dplyr::bind_rows(results) %>%
    dplyr::select(strategy, dplyr::everything())
}
