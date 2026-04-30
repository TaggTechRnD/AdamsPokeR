test_that("simulate_many_hands works", {
  res <- simulate_many_hands(n_sim = 5, n_players = 3, seed = 42)

  expect_true("sim_id" %in% names(res))
  expect_equal(nrow(res), 15)
})
