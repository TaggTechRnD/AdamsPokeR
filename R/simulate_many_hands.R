#' Simulate many Texas Hold'em hands with optional player focus
#'
#' @param n_sim Number of simulations
#' @param n_players Number of players
#' @param focus_player Optional player to track (default = NULL = all players)
#' @param seed Optional seed
#'
#' @return Tibble of simulation results
#' @export
simulate_many_hands <- function(n_sim = 100, n_players = 2, focus_player = NULL, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  sims <- vector("list", n_sim)

  for (i in seq_len(n_sim)) {
    cat("\nSim", i, "of", n_sim)
    res <- simulate_holdem_hand(n_players)
    res$sim_id <- i
    sims[[i]] <- res
  }

  out <- dplyr::bind_rows(sims)

  if (!is.null(focus_player)) {
    out <- dplyr::mutate(out, is_focus = player == focus_player)
  } else {
    out$is_focus <- TRUE
  }

  out
}
