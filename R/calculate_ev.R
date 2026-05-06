#' Calculate EV for each simulated hand
#'
#' @param sim_data Output from classify_outcomes
#'
#' @return sim_data with EV column
#' @export
calculate_ev <- function(sim_data) {

  pot_table <- sim_data %>%
    dplyr::group_by(sim_id) %>%
    dplyr::summarise(
      pot = sum(invested, na.rm = TRUE),
      .groups = "drop"
    )

  sim_data %>%
    dplyr::left_join(pot_table, by = "sim_id") %>%
    dplyr::mutate(
      ev = dplyr::if_else(
        winner,
        pot - invested,
        -invested
      )
    )
}
