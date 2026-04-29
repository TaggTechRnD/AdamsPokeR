#' Create a standard 52-card deck
#'
#' @return A tibble with 52 rows and columns:
#' \describe{
#'   \item{rank}{Integer from 2 to 14 (11 = J, 12 = Q, 13 = K, 14 = A)}
#'   \item{suit}{Factor with levels: hearts, diamonds, clubs, spades}
#' }
#' @export
create_deck <- function() {
  ranks <- 2:14
  suits <- c("hearts", "diamonds", "clubs", "spades")

  deck <- expand.grid(rank = ranks, suit = suits)

  deck$suit <- factor(deck$suit, levels = suits)

  tibble::as_tibble(deck)
}
