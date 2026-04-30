test_that("pair beats high card", {
  hand1 <- tibble::tibble(
    rank = c(10, 10, 4, 7, 2),
    suit = c("hearts", "clubs", "diamonds", "spades", "hearts")
  )

  hand2 <- tibble::tibble(
    rank = c(2, 5, 7, 9, 13),
    suit = c("hearts", "clubs", "diamonds", "spades", "hearts")
  )

  expect_equal(compare_hands(hand1, hand2), 1)
})

test_that("higher pair wins", {
  hand1 <- tibble::tibble(
    rank = c(11, 11, 4, 7, 2),
    suit = c("hearts", "clubs", "diamonds", "spades", "hearts")
  )

  hand2 <- tibble::tibble(
    rank = c(10, 10, 4, 7, 2),
    suit = c("hearts", "clubs", "diamonds", "spades", "hearts")
  )

  expect_equal(compare_hands(hand1, hand2), 1)
})

test_that("identical hands tie", {
  hand1 <- tibble::tibble(
    rank = c(10, 10, 4, 7, 2),
    suit = c("hearts", "clubs", "diamonds", "spades", "hearts")
  )

  hand2 <- hand1

  expect_equal(compare_hands(hand1, hand2), 0)
})

test_that("flush beats straight", {
  flush <- tibble::tibble(
    rank = c(2, 5, 7, 9, 13),
    suit = rep("hearts", 5)
  )

  straight <- tibble::tibble(
    rank = c(6, 7, 8, 9, 10),
    suit = c("hearts", "clubs", "diamonds", "spades", "hearts")
  )

  expect_equal(compare_hands(flush, straight), 1)
})

