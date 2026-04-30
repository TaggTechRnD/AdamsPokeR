test_that("outcomes are classified", {

  sim <- simulate_many_hands(5, 3, seed = 42)

  dt <- tibble::tibble(
    stage = c("preflop","flop","turn","river"),
    fold  = rep("preflop_value < 15", 4),
    call  = rep("TRUE", 4),
    raise = rep("FALSE", 4)
  )

  res <- sim %>%
    apply_decision_table(dt) %>%
    classify_outcomes()

  expect_true("outcome" %in% names(res))
  expect_true(any(!is.na(res$outcome)))
})
