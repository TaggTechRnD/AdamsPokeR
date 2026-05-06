test_that("all strategies run end-to-end", {
  sim <- simulate_many_hands(10, 4)
  expect_no_error(compare_strategies(sim, example_strategies))
})
