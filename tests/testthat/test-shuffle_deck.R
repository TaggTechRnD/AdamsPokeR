test_that("shuffle_deck changes order but not content", {
  deck <- create_deck()
  shuffled <- shuffle_deck(deck, seed = 42)

  expect_equal(nrow(shuffled), 52)
  expect_false(all(deck$rank == shuffled$rank & deck$suit == shuffled$suit))

  expect_equal(
    sort(paste(deck$rank, deck$suit)),
    sort(paste(shuffled$rank, shuffled$suit))
  )
})
