#' Compare evaluated hands
#'
#' @param h1 evaluated hand
#' @param h2 evaluated hand
#'
#' @return 1, -1, or 0
#' @export
compare_hands_from_eval <- function(h1, h2) {

  if (h1$rank_value > h2$rank_value) return(1)
  if (h1$rank_value < h2$rank_value) return(-1)

  tb1 <- h1$tiebreaker[[1]]
  tb2 <- h2$tiebreaker[[1]]

  len <- min(length(tb1), length(tb2))

  for (i in seq_len(len)) {
    if (tb1[i] > tb2[i]) return(1)
    if (tb1[i] < tb2[i]) return(-1)
  }

  0
}
