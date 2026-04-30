#' Evaluate a 5-card poker hand
#'
#' @param cards Tibble with columns rank (int) and suit (factor/char)
#'
#' @return List containing rank_class, rank_value, and tiebreaker vector
#' @export
evaluate_hand <- function(cards) {

  if (nrow(cards) != 5) {
    stop("evaluate_hand currently requires exactly 5 cards")
  }

  ranks <- sort(cards$rank, decreasing = TRUE)
  suits <- cards$suit

  # Count ranks
  rank_counts <- table(ranks)

  # Sort by frequency first, then by rank
  rank_df <- data.frame(
    rank = as.integer(names(rank_counts)),
    count = as.integer(rank_counts)
  )

  rank_df <- rank_df[order(-rank_df$count, -rank_df$rank), ]

  count_values <- rank_df$count
  ordered_ranks <- rank_df$rank

  # Flush check
  is_flush <- length(unique(suits)) == 1

  # Straight check
  is_straight <- FALSE
  if (length(unique(ranks)) == 5) {
    if (max(ranks) - min(ranks) == 4) {
      is_straight <- TRUE
    }
    # Wheel: A-2-3-4-5
    if (all(sort(ranks) == c(2,3,4,5,14))) {
      is_straight <- TRUE
      ranks <- c(5,4,3,2,1)  # normalize for tie-breaking
    }
  }

  top_rank <- ordered_ranks[1]

  # Determine hand type
  if (is_straight && is_flush && top_rank == 14) {
    rank_class <- "royal_flush"
    rank_value <- 10
    tiebreaker <- ranks
  } else if (is_straight && is_flush) {
    rank_class <- "straight_flush"
    rank_value <- 9
    tiebreaker <- ranks

  } else if (count_values[1] == 4) {
    rank_class <- "four_of_a_kind"
    rank_value <- 8
    tiebreaker <- ordered_ranks

  } else if (identical(count_values, c(3L, 2L))) {
    rank_class <- "full_house"
    rank_value <- 7
    tiebreaker <- ordered_ranks

  } else if (is_flush) {
    rank_class <- "flush"
    rank_value <- 6
    tiebreaker <- ranks

  } else if (is_straight) {
    rank_class <- "straight"
    rank_value <- 5
    tiebreaker <- ranks

  } else if (count_values[1] == 3) {
    rank_class <- "three_of_a_kind"
    rank_value <- 4
    tiebreaker <- ordered_ranks

  } else if (identical(count_values, c(2L, 2L, 1L))) {
    rank_class <- "two_pair"
    rank_value <- 3
    tiebreaker <- ordered_ranks

  } else if (count_values[1] == 2) {
    rank_class <- "pair"
    rank_value <- 2
    tiebreaker <- ordered_ranks

  } else {
    rank_class <- "high_card"
    rank_value <- 1
    tiebreaker <- ranks
  }

  list(
    rank_class = rank_class,
    rank_value = rank_value,
    tiebreaker = tiebreaker
  )
}
