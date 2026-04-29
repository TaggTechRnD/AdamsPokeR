#' Shuffle a deck of cards
#'
#' @param deck A tibble representing a deck of cards
#' @param seed Optional integer for reproducibility
#'
#' @return A shuffled tibble of the same structure
#' @export
shuffle_deck <- function(deck, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  deck[sample(nrow(deck)), ]
}
