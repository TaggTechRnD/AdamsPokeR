test_that("simulate_holdem_hand returns valid structure", {
  result <- simulate_holdem_hand(3, seed = 42)

  expect_equal(nrow(result), 3)
  expect_true(all(c("player", "rank_class", "winner") %in% names(result)))
  expect_true(sum(result$winner) >= 1)
})
