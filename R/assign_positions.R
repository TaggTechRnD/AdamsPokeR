assign_positions <- function(sim_data) {
  sim_data %>%
    dplyr::group_by(sim_id) %>%
    dplyr::mutate(
      position = dplyr::row_number(),
      small_blind = position == 1,
      big_blind   = position == 2
    ) %>%
    dplyr::ungroup()
}
