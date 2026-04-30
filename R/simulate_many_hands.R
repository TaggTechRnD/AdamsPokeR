#' Simulate many Texas Hold'em hands with state tracking
#'
#' @param n_sim Number of simulations
#' @param n_players Number of players
#' @param seed Optional seed
#'
#' @return Tibble of all simulations
#' @export
simulate_many_hands <- function(n_sim = 100, n_players = 2, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  sims <- vector("list", n_sim)

  for (i in seq_len(n_sim)) {
    res <- simulate_holdem_hand(n_players)
    res$sim_id <- i
    sims[[i]] <- res
  }

  dplyr::bind_rows(sims)
}
