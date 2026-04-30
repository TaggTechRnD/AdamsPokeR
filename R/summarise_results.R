#' Summarise simulation outcomes
#'
#' @param sim_data Output from simulate_many_hands
#'
#' @return Summary tibble
#' @export
summarise_results <- function(sim_data) {

  sim_data |>
    dplyr::summarise(
      total_hands = dplyr::n(),
      wins = sum(winner),
      win_rate = wins / total_hands,

      avg_preflop = mean(preflop_value),
      avg_flop = mean(flop_rank),
      avg_turn = mean(turn_rank),
      avg_river = mean(river_rank)
    )
}
