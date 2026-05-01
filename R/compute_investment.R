compute_investment <- function(sim_data) {

  cost_preflop <- 1
  cost_flop    <- 2
  cost_turn    <- 4
  cost_river   <- 8

  sim_data %>%
    dplyr::mutate(

      invest_preflop = cost_preflop,

      invest_flop = dplyr::if_else(
        decision_preflop == "fold", 0, cost_flop
      ),

      invest_turn = dplyr::if_else(
        decision_flop == "fold", 0, cost_turn
      ),

      invest_river = dplyr::if_else(
        decision_turn == "fold", 0, cost_river
      ),

      invested = invest_preflop + invest_flop + invest_turn + invest_river
    )
}
