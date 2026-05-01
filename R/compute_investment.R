compute_investment <- function(sim_data) {

  cost_preflop <- 1
  cost_flop    <- 2
  cost_turn    <- 4
  cost_river   <- 8

  raise_mult <- 2

  sim_data %>%
    dplyr::group_by(sim_id) %>%
    dplyr::mutate(

      # --------
      # PRE-FLOP
      # --------
      price_preflop = ifelse(
        any(decision_preflop == "raise"),
        cost_preflop * raise_mult,
        cost_preflop
      ),

      invest_preflop = dplyr::case_when(
        decision_preflop == "fold" ~ 0,
        TRUE ~ price_preflop
      ),

      # --------
      # FLOP
      # --------
      active_flop = decision_preflop != "fold",

      price_flop = ifelse(
        any(decision_flop == "raise" & active_flop),
        cost_flop * raise_mult,
        cost_flop
      ),

      invest_flop = dplyr::case_when(
        !active_flop ~ 0,
        decision_flop == "fold" ~ 0,
        TRUE ~ price_flop
      ),

      # --------
      # TURN
      # --------
      active_turn = active_flop & decision_flop != "fold",

      price_turn = ifelse(
        any(decision_turn == "raise" & active_turn),
        cost_turn * raise_mult,
        cost_turn
      ),

      invest_turn = dplyr::case_when(
        !active_turn ~ 0,
        decision_turn == "fold" ~ 0,
        TRUE ~ price_turn
      ),

      # --------
      # RIVER
      # --------
      active_river = active_turn & decision_turn != "fold",

      price_river = ifelse(
        any(decision_river == "raise" & active_river),
        cost_river * raise_mult,
        cost_river
      ),

      invest_river = dplyr::case_when(
        !active_river ~ 0,
        decision_river == "fold" ~ 0,
        TRUE ~ price_river
      ),

      # --------
      # TOTAL INVESTMENT
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
