test_that("detects high card", {
  cards <- tibble::tibble(
    rank = c(2, 5, 7, 9, 13),
    suit = c("hearts", "clubs", "diamonds", "spades", "hearts")
  )

  result <- evaluate_hand(cards)

  expect_equal(result$rank_class, "high_card")
  expect_equal(result$rank_value, 1)
})

test_that("detects pair", {
  cards <- tibble::tibble(
    rank = c(10, 10, 4, 7, 2),
    suit = c("hearts", "clubs", "diamonds", "spades", "hearts")
  )

  result <- evaluate_hand(cards)

  expect_equal(result$rank_class, "pair")
  expect_equal(result$rank_value, 2)
})

test_that("detects two pair", {
  cards <- tibble::tibble(
    rank = c(10, 10, 4, 4, 2),
    suit = c("hearts", "clubs", "diamonds", "spades", "hearts")
  )

  result <- evaluate_hand(cards)

  expect_equal(result$rank_class, "two_pair")
  expect_equal(result$rank_value, 3)
})

test_that("detects flush", {
  cards <- tibble::tibble(
    rank = c(2, 5, 7, 9, 13),
    suit = rep("hearts", 5)
  )

  result <- evaluate_hand(cards)

  expect_equal(result$rank_class, "flush")
  expect_equal(result$rank_value, 6)
})

test_that("detects straight", {
  cards <- tibble::tibble(
    rank = c(6, 7, 8, 9, 10),
    suit = c("hearts", "clubs", "diamonds", "spades", "hearts")
  )

  result <- evaluate_hand(cards)

  expect_equal(result$rank_class, "straight")
  expect_equal(result$rank_value, 5)
})

test_that("detects full house", {
  cards <- tibble::tibble(
    rank = c(3, 3, 3, 7, 7),
    suit = c("hearts", "clubs", "diamonds", "spades", "hearts")
  )

  result <- evaluate_hand(cards)

  expect_equal(result$rank_class, "full_house")
  expect_equal(result$rank_value, 7)
})

test_that("detects four of a kind", {
  cards <- tibble::tibble(
    rank = c(9, 9, 9, 9, 2),
    suit = c("hearts", "clubs", "diamonds", "spades", "hearts")
  )

  result <- evaluate_hand(cards)

  expect_equal(result$rank_class, "four_of_a_kind")
  expect_equal(result$rank_value, 8)
})

test_that("detects straight flush", {
  cards <- tibble::tibble(
    rank = c(6, 7, 8, 9, 10),
    suit = rep("spades", 5)
  )

  result <- evaluate_hand(cards)

  expect_equal(result$rank_class, "straight_flush")
  expect_equal(result$rank_value, 9)
})

test_that("detects royal flush", {
  cards <- tibble::tibble(
    rank = c(10,11,12,13,14),
    suit = rep("spades", 5)
  )

  result <- evaluate_hand(cards)

  expect_equal(result$rank_class, "royal_flush")
  expect_equal(result$rank_value, 10)
})
