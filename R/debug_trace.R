debug_trace <- function() {
  sim <- simulate_many_hands(n_sim = 1, n_players = 6)
  trace_single_hand(sim)
}
