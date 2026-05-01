test_that("EV is calculated", {

  sim <- simulate_many_hands(10, 3, seed = 42)

  dt <- tibble::tibble(
    stage = c("preflop","flop","turn","river"),
    fold  = rep("preflop_value < 15", 4),
    call  = rep("TRUE", 4),
    raise = rep("FALSE", 4)
  )

  res <- sim %>%
    apply_decision_table(dt) %>%
    compute_investment() %>%
    classify_outcomes() %>%
    calculate_ev()

  expect_true("ev" %in% names(res))
})
