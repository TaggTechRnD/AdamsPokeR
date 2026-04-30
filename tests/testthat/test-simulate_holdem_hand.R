test_that("simulate_holdem_hand returns valid state-tracking structure", {
  result <- simulate_holdem_hand(3, seed = 42)

  expect_equal(nrow(result), 3)

  expect_true(all(c(
    "player",
    "hole_cards",
    "board_flop",
    "board_turn",
    "board_river",
    "preflop_value",
    "flop_rank",
    "turn_rank",
    "river_rank",
    "final_eval",
    "winner"
  ) %in% names(result)))

  expect_equal(sum(result$winner), 1)
})
