compare_strategies <- function(
    sim,
    strategy_list,
    rival_dt = dt_rival_simple,
    return_full_results = FALSE

) {
  strategy_names <- names(strategy_list)
  n_strat <- length(strategy_names)
  results <- vector("list", n_strat)
  full_results <- vector("list", n_strat)
  start_time <- Sys.time()

  for (i in seq_along(strategy_names)) {

    strategy_name <- strategy_names[i]
    dt_focus <- strategy_list[[strategy_name]]
    cat("\n----------------------------------\n")
    cat("Running strategy", i, "of", n_strat, "\n")
    cat("Strategy:", strategy_name, "\n")
    cat("----------------------------------\n")
    iter_start <- Sys.time()

    #### run simulation ####

    res <- sim %>%
      assign_positions() %>%
      assign_player_types() %>%
      add_hand_context() %>%
      initialize_hand_state() %>%
      apply_decision_table(
        dt_focus = dt_focus,
        dt_rival = rival_dt

      )

    #### focus player only ####

    focus_res <- res %>%
      dplyr::filter(is_focus)

    #### core metrics ####

    summary_tbl <- focus_res %>%
      dplyr::summarise(
        strategy = strategy_name,
        n_hands = dplyr::n(),
        mean_stack = mean(stack, na.rm = TRUE),
        mean_invested = mean(total_invested, na.rm = TRUE),
        mean_profit = mean(stack - 100, na.rm = TRUE),
        total_profit =sum(stack - 100, na.rm = TRUE),
        bb_per_100 = mean((stack - 100) / 1, na.rm = TRUE) * 100,
        win_rate = mean(winner, na.rm = TRUE),
        showdown_rate =
          mean(
            remaining_river > 1,
            na.rm = TRUE
          ),
        preflop_fold_rate =
          mean(
            decision_preflop == "fold",
            na.rm = TRUE
          ),
        flop_fold_rate =
          mean(
            decision_flop == "fold",
            na.rm = TRUE
          ),
        turn_fold_rate =
          mean(
            decision_turn == "fold",
            na.rm = TRUE
          ),
        river_fold_rate =
          mean(
            decision_river == "fold",
            na.rm = TRUE
          ),
        aggression_rate =
          mean(
            decision_preflop %in% c("bet", "raise") |
              decision_flop %in% c("bet", "raise") |
              decision_turn %in% c("bet", "raise") |
              decision_river %in% c("bet", "raise"),
            na.rm = TRUE

          ),

        mean_final_pot =
          mean(pot_size, na.rm = TRUE),

        max_final_pot =
          max(pot_size, na.rm = TRUE)

      )

    #### positional summary ####

    position_tbl <- res %>%

      dplyr::group_by(position) %>%

      dplyr::summarise(
        mean_stack = mean(stack, na.rm = TRUE),
        mean_profit = mean(stack - 100, na.rm = TRUE),
        win_rate = mean(winner, na.rm = TRUE),
        .groups = "drop"

      )

    #### diagnostics ####

    diagnostics_tbl <- res %>%
      dplyr::group_by(sim_id) %>%
      dplyr::summarise(
        total_stack = sum(stack, na.rm = TRUE),
        total_invested = sum(total_invested, na.rm = TRUE),
        final_pot = max(pot_size, na.rm = TRUE),
        n_winners =  sum(winner, na.rm = TRUE),
        .groups = "drop"

      ) %>%

      dplyr::mutate(
        stack_error = abs(total_stack - 100 * max(res$n_players)),
        pot_error = abs(final_pot - total_invested)

      )

    #### failed hands ####

    failed_hands <- diagnostics_tbl %>%
      dplyr::filter(
        stack_error > 1e-6 |
          pot_error > 1e-6 |
          n_winners != 1
      )

    #### combine ####

    summary_tbl$n_failed_hands <- nrow(failed_hands)

    summary_tbl$runtime_seconds <-
      as.numeric(
        difftime(
          Sys.time(),
          iter_start,
          units = "secs"
        )
      )

    results[[i]] <- list(
      summary = summary_tbl,
      positions = position_tbl,
      diagnostics = diagnostics_tbl
    )

    if (return_full_results) {

      full_results[[i]] <- res
    }

    cat("Completed:", strategy_name, "\n")
    cat("Runtime:", round(summary_tbl$runtime_seconds, 2), "seconds\n"
    )
  }

  #### bind summaries ####

  final_summary <- dplyr::bind_rows(
    lapply(results, function(x) x$summary)
  )

  #### return ####

  if (return_full_results) {
    return(list(
      summary = final_summary,
      details = results,
      raw_results = full_results
    ))
  }

  return(list(
    summary = final_summary,
    details = results
  ))
}
