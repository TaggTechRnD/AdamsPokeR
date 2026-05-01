compare_strategies <- function(sim, strategy_list) {

  results <- lapply(names(strategy_list), function(name) {

    dt <- strategy_list[[name]]

    res <- sim %>%
      apply_decision_table(dt) %>%
      compute_investment() %>%
      classify_outcomes() %>%
      calculate_ev()

    summary <- summarise_ev(res)

    summary$strategy <- name

    summary
  })

  dplyr::bind_rows(results) %>%
    dplyr::select(strategy, dplyr::everything())
}
