test_that("decision table applies correctly", {

  sim <- simulate_many_hands(5, 3, seed = 42)

  dt <- tibble::tibble(
    stage = c("preflop","flop","turn","river"),
    fold  = rep("preflop_value < 10", 4),
    call  = rep("preflop_value >= 10", 4),
    raise = rep("preflop_value > 20", 4)
  )

  res <- apply_decision_table(sim, dt)

  expect_true("decision_preflop" %in% names(res))
})

test_that("players drop out correctly", {

  sim <- simulate_many_hands(10, 4, seed = 42)

  dt <- tibble::tibble(
    stage = c("preflop","flop","turn","river"),
    fold  = rep("preflop_value < 12", 4),
    call  = rep("preflop_value >= 12", 4),
    raise = rep("preflop_value > 20", 4)
  )

  res <- apply_decision_table(sim, dt)

  expect_true("remaining_preflop" %in% names(res))
  expect_true(any(res$decision_preflop == "fold"))
})
