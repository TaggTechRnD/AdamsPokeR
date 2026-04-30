summarise_outcomes <- function(sim_data) {

  sim_data %>%
    dplyr::count(outcome) %>%
    dplyr::mutate(prop = n / sum(n))
}
