test_that("pipeline produces sane single-hand outputs", {

  sim <- simulate_many_hands(1, n_players = 6)

  trace <- trace_single_hand(sim)

  # core invariants

  expect_equal(nrow(trace$x1), 6)
  expect_true(all(trace$x2$position >= 1))
  expect_true(all(!is.na(trace$x5$decision_preflop)))

  # CRITICAL: no ghost players
  expect_true(all(
    trace$x5$decision_flop[trace$x5$active == FALSE] == "fold"
  ))

  # CRITICAL: investment consistency
  expect_true(all(trace$x7$invested >= 0))

  # CRITICAL: exactly one winner
  expect_equal(sum(trace$x9$winner), 1)
})
