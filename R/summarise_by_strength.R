#' Summarise outcomes by river hand strength
#'
#' @param sim_data Simulation output
#'
#' @return Tibble grouped by hand strength
#' @export
summarise_by_strength <- function(sim_data) {

  sim_data |>
    dplyr::group_by(river_rank) |>
    dplyr::summarise(
      count = dplyr::n(),
      win_rate = mean(winner),
      .groups = "drop"
    )
}
