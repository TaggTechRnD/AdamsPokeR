summarise_ev <- function(sim_data) {

  sim_data %>%
    dplyr::filter(is_focus) %>%
    dplyr::summarise(
      total_hands = dplyr::n(),
      total_ev = sum(ev),
      avg_ev = mean(ev),
      win_rate = mean(winner)
    )
}
