#' Calculate EV for each simulated hand
#'
#' @param sim_data Output from classify_outcomes
#'
#' @return sim_data with EV column
#' @export
calculate_ev <- function(sim_data) {

  cost_preflop <- 1
  cost_flop    <- 2
  cost_turn    <- 4
  cost_river   <- 8

  sim_data <- sim_data %>%
    dplyr::mutate(

      # Stage participation flags
      played_preflop = TRUE,
      played_flop    = decision_preflop != "fold",
      played_turn    = decision_flop != "fold" & played_flop,
      played_river   = decision_turn != "fold" & played_turn,

      # Player investment
      invested =
        cost_preflop +
        played_flop  * cost_flop +
        played_turn  * cost_turn +
        played_river * cost_river
    )

  # Add blinds (simple model: player 1 = small blind, player 2 = big blind)
  sim_data <- sim_data %>%
    dplyr::mutate(
      blind = dplyr::case_when(
        player == 1 ~ 0.5,
        player == 2 ~ 1,
        TRUE ~ 0
      ),
      invested = invested + blind
    )

  # --------
  # Compute pot per hand (sim_id)
  # --------
  pot_table <- sim_data %>%
    dplyr::group_by(sim_id) %>%
    dplyr::summarise(
      pot = sum(invested, na.rm = TRUE),
      .groups = "drop"
    )

  # --------
  # Join pot back
  # --------
  sim_data <- sim_data %>%
    dplyr::left_join(pot_table, by = "sim_id")

  # --------
  # EV calculation
  # --------
  sim_data %>%
    dplyr::mutate(
      ev = dplyr::case_when(
        winner == TRUE  ~ pot - invested,
        winner == FALSE ~ -invested
      )
    )
}
