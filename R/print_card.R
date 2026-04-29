#' Convert cards to human-readable format
#'
#' @param cards A tibble of cards with rank and suit
#'
#' @return A character vector like "A♠", "10♥"
#' @export
print_card <- function(cards) {
  rank_map <- c(
    "2" = "2", "3" = "3", "4" = "4", "5" = "5",
    "6" = "6", "7" = "7", "8" = "8", "9" = "9",
    "10" = "10", "11" = "J", "12" = "Q",
    "13" = "K", "14" = "A"
  )

  suit_map <- c(
    "hearts" = "\u2665",
    "diamonds" = "\u2666",
    "clubs" = "\u2663",
    "spades" = "\u2660"
  )

  ranks <- rank_map[as.character(cards$rank)]
  suits <- suit_map[as.character(cards$suit)]

  paste0(ranks, suits)
}
