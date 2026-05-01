add_type_modifiers <- function(sim_data) {

  sim_data %>%
    dplyr::mutate(
      type_mult = dplyr::case_when(
        player_type == "tight"      ~ 0.85,
        player_type == "passive"    ~ 0.95,
        player_type == "loose"      ~ 1.10,
        player_type == "aggressive" ~ 1.15,
        TRUE ~ 1.0
      )
    )
}
