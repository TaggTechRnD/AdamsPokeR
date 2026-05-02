compare_strategies <- function(sim, strategy_list) {

  results <- lapply(names(strategy_list), function(name) {

    dt <- strategy_list[[name]]

    res <- sim %>%
      assign_positions() %>%
      assign_player_types() %>%
      add_type_modifiers() %>%
      add_adjusted_metrics() %>%      # creates adj_*

      apply_decision_table(dt) %>%    # first decision pass
      compute_investment() %>%        # creates prices
      add_adjusted_metrics() %>%      # now creates adj_*_price

      apply_decision_table(dt) %>%    # second decision pass
      compute_investment() %>%        # recompute investment after revised decisions

      classify_outcomes() %>%
      calculate_ev()

    summary <- summarise_ev(res)

    summary$strategy <- name

    summary
  })

  dplyr::bind_rows(results) %>%
    dplyr::select(strategy, dplyr::everything())
}
