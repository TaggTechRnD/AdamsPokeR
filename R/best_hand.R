#' Get best possible 5-card hand from any set of cards
#'
#' @param cards Tibble of 5 to 7 cards
#'
#' @return evaluated hand (list from evaluate_hand)
#' @export
best_hand <- function(cards) {

  n <- nrow(cards)

  if (n < 5 || n > 7) {
    stop("best_hand supports between 5 and 7 cards")
  }

  # If exactly 5 → direct
  if (n == 5) {
    return(evaluate_hand(cards))
  }

  combos <- combn(n, 5)

  best <- NULL

  for (i in seq_len(ncol(combos))) {
    hand <- cards[combos[, i], , drop = FALSE]
    eval <- evaluate_hand(hand)

    if (is.null(best)) {
      best <- eval
    } else {
      cmp <- compare_hands_from_eval(eval, best)
      if (cmp == 1) best <- eval
    }
  }

  best
}
