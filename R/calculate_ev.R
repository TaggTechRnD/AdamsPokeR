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

      n_players = dplyr::n(),

      n_turn_players = sum(active_turn, na.rm = TRUE),
      n_river_players = sum(active_river, na.rm = TRUE),

      effective_players = pmax(n_turn_players, n_river_players, 1),

      total_invested = sum(invested, na.rm = TRUE),

      # Revised pot estimate:
      # scale pot by meaningful late-stage participation
      pot = total_invested * (effective_players / n_players),

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
