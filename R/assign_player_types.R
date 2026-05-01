assign_player_types <- function(sim_data) {
  types <- c("tight", "loose", "passive", "aggressive")

  sim_data %>%
    dplyr::group_by(sim_id) %>%
    dplyr::mutate(
      player_type = sample(types, dplyr::n(), replace = TRUE)
    ) %>%
    dplyr::ungroup()
}
