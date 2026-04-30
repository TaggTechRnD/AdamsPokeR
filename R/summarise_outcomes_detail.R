summarise_outcomes_detail <- function(sim_data) {

  sim_data %>%
    dplyr::count(outcome_detail) %>%
    dplyr::mutate(prop = n / sum(n))
}
