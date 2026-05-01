add_adjusted_metrics <- function(sim_data) {

  sim_data %>%
    dplyr::mutate(
      adj_preflop = preflop_value * pos_mult * type_mult,
      adj_flop    = flop_rank     * pos_mult * type_mult,
      adj_turn    = turn_rank     * pos_mult * type_mult,
      adj_river   = river_rank    * pos_mult * type_mult
    )
}
