#' Get best 5-card hand from 7 cards
#'
#' @param cards Tibble with 7 cards
#'
#' @return List from evaluate_hand()
#' @export
best_5_from_7 <- function(cards) {

  if (nrow(cards) != 7) {
    stop("Must provide exactly 7 cards")
  }

  combos <- combn(1:7, 5)

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
