#' Deal cards from a deck
#'
#' @param deck A tibble representing a deck of cards
#' @param n Number of cards to deal
#'
#' @return A list with:
#' \describe{
#'   \item{hand}{Tibble of dealt cards}
#'   \item{remaining_deck}{Tibble of remaining cards}
#' }
#' @export
deal_cards <- function(deck, n) {
  if (n > nrow(deck)) {
    stop("Cannot deal more cards than remain in the deck")
  }

  hand <- deck[1:n, ]
  remaining_deck <- deck[(n + 1):nrow(deck), ]

  list(
    hand = hand,
    remaining_deck = remaining_deck
  )
}
