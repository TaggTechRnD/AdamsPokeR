add_adjusted_metrics <- function(sim_data) {

  sim_data <- sim_data %>%
    dplyr::mutate(
      adj_preflop = preflop_value * pos_mult * type_mult,
      adj_flop    = flop_rank     * pos_mult * type_mult,
      adj_turn    = turn_rank     * pos_mult * type_mult,
      adj_river   = river_rank    * pos_mult * type_mult
    )

  # ---- NEW: player-count pressure ----
  sim_data <- sim_data %>%
    dplyr::mutate(
      adj_preflop_pc = adj_preflop / pmax(remaining_preflop, 1),
      adj_flop_pc    = adj_flop    / pmax(remaining_flop, 1),
      adj_turn_pc    = adj_turn    / pmax(remaining_turn, 1),
      adj_river_pc   = adj_river   / pmax(remaining_river, 1)
    )

  # ---- Existing price-aware logic (only if price exists) ----
  if (all(c("price_preflop", "price_flop", "price_turn", "price_river") %in% names(sim_data))) {
    sim_data <- sim_data %>%
      dplyr::mutate(
        adj_preflop_price = adj_preflop_pc / price_preflop,
        adj_flop_price    = adj_flop_pc    / price_flop,
        adj_turn_price    = adj_turn_pc    / price_turn,
        adj_river_price   = adj_river_pc   / price_river
      )
  }

  sim_data
}
