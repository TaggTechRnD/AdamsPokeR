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
      # Determine if a raise happened at each stage
      # --------
      raise_preflop = any(decision_preflop == "raise"),
      raise_flop    = any(decision_flop == "raise" & decision_preflop != "fold"),
      raise_turn    = any(decision_turn == "raise" & decision_flop != "fold"),
      raise_river   = any(decision_river == "raise" & decision_turn != "fold"),

      # --------
      # Adjusted stage prices
      # --------
      price_preflop = ifelse(raise_preflop, cost_preflop * raise_mult, cost_preflop),
      price_flop    = ifelse(raise_flop,    cost_flop    * raise_mult, cost_flop),
      price_turn    = ifelse(raise_turn,    cost_turn    * raise_mult, cost_turn),
      price_river   = ifelse(raise_river,   cost_river   * raise_mult, cost_river),

      # --------
      # Investment per stage
      # --------
      invest_preflop = dplyr::case_when(
        decision_preflop == "fold" ~ 0,
        TRUE ~ price_preflop
      ),

      invest_flop = dplyr::case_when(
        decision_preflop == "fold" ~ 0,
        decision_flop == "fold"    ~ 0,
        TRUE ~ price_flop
      ),

      invest_turn = dplyr::case_when(
        decision_flop == "fold" ~ 0,
        decision_turn == "fold" ~ 0,
        TRUE ~ price_turn
      ),

      invest_river = dplyr::case_when(
        decision_turn == "fold" ~ 0,
        decision_river == "fold" ~ 0,
        TRUE ~ price_river
      ),

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
