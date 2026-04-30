#' Simulate a hand against random opponents
#'
#' @param hand Tibble of 5 cards
#' @param n_sim Number of simulations
#' @param seed Optional seed for reproducibility
#'
#' @return List with win, loss, tie probabilities
#' @export
simulate_vs_random <- function(hand, n_sim = 10000, seed = NULL) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (nrow(hand) != 5) {
    stop("Hand must contain exactly 5 cards")
  }

  wins <- 0
  losses <- 0
  ties <- 0

  base_deck <- create_deck()

  # Remove player's hand from deck
  key <- paste(hand$rank, hand$suit)
  deck_key <- paste(base_deck$rank, base_deck$suit)
  remaining_deck <- base_deck[!(deck_key %in% key), ]

  for (i in seq_len(n_sim)) {

    # Shuffle and deal opponent hand
    deck <- shuffle_deck(remaining_deck)
    opp <- deal_cards(deck, 5)$hand

    result <- compare_hands(hand, opp)

    if (result == 1) {
      wins <- wins + 1
    } else if (result == -1) {
      losses <- losses + 1
    } else {
      ties <- ties + 1
    }
  }

  total <- wins + losses + ties

  list(
    win = wins / total,
    loss = losses / total,
    tie = ties / total
  )
}
