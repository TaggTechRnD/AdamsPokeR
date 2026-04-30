test_that("simulate_vs_random returns valid probabilities", {
  hand <- tibble::tibble(
    rank = c(14, 14, 13, 12, 11),
    suit = c("hearts", "clubs", "diamonds", "spades", "hearts")
  )

  result <- simulate_vs_random(hand, n_sim = 100, seed = 42)

  expect_true(is.list(result))
  expect_true(all(c("win", "loss", "tie") %in% names(result)))

  total <- result$win + result$loss + result$tie

  expect_true(abs(total - 1) < 1e-6)
})
