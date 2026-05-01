assign_positions <- function(sim_data) {
  sim_data %>%
    dplyr::group_by(sim_id) %>%
    dplyr::mutate(

      # Basic position
      position = dplyr::row_number(),
      n_players = dplyr::n(),

      # Blinds (unchanged)
      small_blind = position == 1,
      big_blind   = position == 2,

      # Position grouping
      pos_group = dplyr::case_when(
        position <= ceiling(n_players / 3) ~ "early",
        position <= ceiling(2 * n_players / 3) ~ "mid",
        TRUE ~ "late"
      ),

      # Numeric modifier (this is the key addition)
      pos_mult = dplyr::case_when(
        pos_group == "early" ~ 0.8,
        pos_group == "mid"   ~ 1.0,
        pos_group == "late"  ~ 1.2
      )

    ) %>%
    dplyr::ungroup()
}
