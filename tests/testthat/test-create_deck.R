test_that("create_deck returns 52 unique cards", {
  deck <- create_deck()

  expect_equal(nrow(deck), 52)
  expect_equal(length(unique(paste(deck$rank, deck$suit))), 52)
})
