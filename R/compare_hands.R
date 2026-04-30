#' Compare two poker hands
#'
#' @param hand1 Tibble of 5 cards
#' @param hand2 Tibble of 5 cards
#'
#' @return 1 if hand1 wins, -1 if hand2 wins, 0 if tie
#' @export
compare_hands <- function(hand1, hand2) {

  h1 <- evaluate_hand(hand1)
  h2 <- evaluate_hand(hand2)

  # Compare rank class
  if (h1$rank_value > h2$rank_value) return(1)
  if (h1$rank_value < h2$rank_value) return(-1)

  # Same rank class → compare tiebreakers
  len <- min(length(h1$tiebreaker), length(h2$tiebreaker))

  for (i in seq_len(len)) {
    if (h1$tiebreaker[i] > h2$tiebreaker[i]) return(1)
    if (h1$tiebreaker[i] < h2$tiebreaker[i]) return(-1)
  }

  return(0)
}
