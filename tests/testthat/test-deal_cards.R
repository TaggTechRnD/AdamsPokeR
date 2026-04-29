test_that("deal_cards returns correct hand size and remaining deck", {
  deck <- create_deck()
  result <- deal_cards(deck, 5)

  expect_equal(nrow(result$hand), 5)
  expect_equal(nrow(result$remaining_deck), 47)
})
