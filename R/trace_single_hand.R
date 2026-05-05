trace_single_hand <- function(sim) {

  x1 <- sim

  x2 <- x1 %>% assign_positions()
  x3 <- x2 %>% assign_player_types()
  x4 <- x3 %>% add_type_modifiers()

  x5 <- x4 %>% apply_decision_table(dt_base, dt_rival_baseline)
  x6 <- x5 %>% add_adjusted_metrics()
  x7 <- x6 %>% compute_investment()
  x8 <- x7 %>% classify_outcomes()
  x9 <- x8 %>% calculate_ev()

  list(
    x1 = x1,
    x2 = x2,
    x3 = x3,
    x4 = x4,
    x5 = x5,
    x6 = x6,
    x7 = x7,
    x8 = x8,
    x9 = x9
  )
}
