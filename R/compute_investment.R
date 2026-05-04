compute_investment <- function(sim_data) {

  # Base costs by street (keep your ladder, but now multiplied by bet level)
  cost_preflop <- 1
  cost_flop    <- 2
  cost_turn    <- 4
  cost_river   <- 8

  sim_data %>%
    dplyr::group_by(sim_id) %>%
    dplyr::mutate(

      # --------
      # PRE-FLOP
      # --------
      active_preflop = TRUE,

      price_preflop = cost_preflop * pmax(betlevel_preflop, 1),

      invest_preflop = dplyr::case_when(
        decision_preflop == "fold" ~ 0,
        TRUE ~ price_preflop
      ),

      # --------
      # FLOP
      # --------
      active_flop = decision_preflop != "fold",

      price_flop = cost_flop * pmax(betlevel_flop, 1),

      invest_flop = dplyr::case_when(
        !active_flop ~ 0,
        decision_flop == "fold" ~ 0,
        TRUE ~ price_flop
      ),

      # --------
      # TURN
      # --------
      active_turn = active_flop & decision_flop != "fold",

      price_turn = cost_turn * pmax(betlevel_turn, 1),

      invest_turn = dplyr::case_when(
        !active_turn ~ 0,
        decision_turn == "fold" ~ 0,
        TRUE ~ price_turn
      ),

      # --------
      # RIVER
      # --------
      active_river = active_turn & decision_turn != "fold",

      price_river = cost_river * pmax(betlevel_river, 1),

      invest_river = dplyr::case_when(
        !active_river ~ 0,
        decision_river == "fold" ~ 0,
        TRUE ~ price_river
      ),

      # --------
      # TOTAL
      # --------
      invested =
        invest_preflop +
        invest_flop +
        invest_turn +
        invest_river
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      blind = dplyr::case_when(
        player == 1 ~ 0.5,
        player == 2 ~ 1,
        TRUE ~ 0
      ),
      invested = invested + blind
    )
}
