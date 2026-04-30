#' Summarise simulation outcomes
#'
#' @param sim_data Output from simulate_many_hands
#'
#' @return Summary tibble
#' @export
summarise_results <- function(sim_data) {

  sim_data |>
    dplyr::group_by(player) |>
    dplyr::summarise(
      wins = sum(winner),
      total = dplyr::n(),
      win_rate = wins / total,
      .groups = "drop"
    )
}
