test_that("print_card returns readable output", {
  deck <- create_deck()
  hand <- deck[1:3, ]

  output <- print_card(hand)

  expect_equal(length(output), 3)
  expect_true(is.character(output))
})
