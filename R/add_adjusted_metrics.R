add_adjusted_metrics <- function(sim_data) {

  sim_data <- sim_data %>%
    dplyr::mutate(
      adj_preflop = preflop_value * pos_mult * type_mult,
      adj_flop    = flop_rank     * pos_mult * type_mult,
      adj_turn    = turn_rank     * pos_mult * type_mult,
      adj_river   = river_rank    * pos_mult * type_mult
    )

  if (all(c("price_preflop", "price_flop", "price_turn", "price_river") %in% names(sim_data))) {
    sim_data <- sim_data %>%
      dplyr::mutate(
        adj_preflop_price = adj_preflop / price_preflop,
        adj_flop_price    = adj_flop    / price_flop,
        adj_turn_price    = adj_turn    / price_turn,
        adj_river_price   = adj_river   / price_river
      )
  }

  sim_data
}
