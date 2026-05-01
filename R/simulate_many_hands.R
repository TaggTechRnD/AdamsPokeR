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
  cat("Beginning simulation at", as.POSIXct(Sys.time()), "\n")
  sims <- vector("list", n_sim)
  cat("\nSimulating", n_sim, "hands:")

  for (i in seq_len(n_sim)) {
    if (i %% 50 == 0) cat(i, "|")
    res <- simulate_holdem_hand(n_players)
    res$sim_id <- i
    sims[[i]] <- res
  }
  cat("\n", n_sim, "sims complete.")
  out <- dplyr::bind_rows(sims)

  if (is.null(focus_player)) {
    focus_player <- 1
  }

  out <- dplyr::mutate(out, is_focus = player == focus_player)
  cat("\nFinishing simulation at", as.POSIXct(Sys.time()), "\n")
  out
}
