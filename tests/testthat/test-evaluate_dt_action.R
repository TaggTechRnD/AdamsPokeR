#### ===================================================== ####
#### test-evaluate_dt_action.R
#### ===================================================== ####

test_that("evaluate_dt_action returns valid structure", {

  sim <- simulate_many_hands(
    n_sim = 1,
    n_players = 6
  ) %>%

    assign_positions() %>%

    assign_player_types() %>%

    add_hand_context() %>%

    initialize_hand_state()

  row <- sim %>%
    dplyr::slice(1)

  out <- evaluate_dt_action(

    row = row,

    dt = dt_focus_simple,

    stage = "preflop",

    current_bet = 1,

    pot = 1.5,

    cycle = 1
  )

  expect_true(is.list(out))

  expect_true(all(

    c(
      "action",
      "invest",
      "matched_rule"
    ) %in% names(out)

  ))
})



test_that("evaluate_dt_action returns valid action", {

  sim <- simulate_many_hands(
    n_sim = 1,
    n_players = 6
  ) %>%

    assign_positions() %>%

    assign_player_types() %>%

    add_hand_context() %>%

    initialize_hand_state()

  row <- sim %>%
    dplyr::slice(1)

  out <- evaluate_dt_action(

    row = row,

    dt = dt_focus_simple,

    stage = "preflop",

    current_bet = 1,

    pot = 1.5,

    cycle = 1
  )

  valid_actions <- c(
    "fold",
    "check",
    "call",
    "bet",
    "raise",
    "all_in"
  )

  expect_true(
    out$action %in% valid_actions
  )
})



test_that("evaluate_dt_action returns non-negative investment", {

  sim <- simulate_many_hands(
    n_sim = 1,
    n_players = 6
  ) %>%

    assign_positions() %>%

    assign_player_types() %>%

    add_hand_context() %>%

    initialize_hand_state()

  row <- sim %>%
    dplyr::slice(1)

  out <- evaluate_dt_action(

    row = row,

    dt = dt_focus_simple,

    stage = "preflop",

    current_bet = 1,

    pot = 1.5,

    cycle = 1
  )

  expect_true(
    out$invest >= 0
  )
})



#### ===================================================== ####
#### test-run_betting_cycle.R
#### ===================================================== ####

test_that("run_betting_cycle returns valid structure", {

  sim <- simulate_many_hands(
    n_sim = 1,
    n_players = 6
  ) %>%

    assign_positions() %>%

    assign_player_types() %>%

    add_hand_context() %>%

    initialize_hand_state()

  idx <- which(sim$sim_id == 1)

  out <- run_betting_cycle(

    sim_data = sim,

    idx = idx,

    active_players = rep(TRUE, 6),

    dt_focus = dt_focus_simple,

    dt_rival = dt_rival_simple,

    stage = "preflop",

    cycle = 1,

    current_bet = 1,

    pot = 1.5,

    street_investments = rep(0, 6)
  )

  expect_true(is.list(out))

  expect_true(all(

    c(
      "sim_data",
      "active_players",
      "current_bet",
      "pot",
      "street_investments",
      "raise_occurred"
    ) %in% names(out)

  ))
})



test_that("run_betting_cycle preserves stack conservation", {

  sim <- simulate_many_hands(
    n_sim = 1,
    n_players = 6
  ) %>%

    assign_positions() %>%

    assign_player_types() %>%

    add_hand_context() %>%

    initialize_hand_state()

  idx <- which(sim$sim_id == 1)

  out <- run_betting_cycle(

    sim_data = sim,

    idx = idx,

    active_players = rep(TRUE, 6),

    dt_focus = dt_focus_simple,

    dt_rival = dt_rival_simple,

    stage = "preflop",

    cycle = 1,

    current_bet = 1,

    pot = 1.5,

    street_investments = rep(0, 6)
  )

  total_stack <- sum(out$sim_data$stack[idx])

  total_pot <- out$pot

  expect_true(

    abs(
      total_stack + total_pot - (6 * 100)
    ) < 1e-6

  )
})



test_that("run_betting_cycle updates pot correctly", {

  sim <- simulate_many_hands(
    n_sim = 1,
    n_players = 6
  ) %>%

    assign_positions() %>%

    assign_player_types() %>%

    add_hand_context() %>%

    initialize_hand_state()

  idx <- which(sim$sim_id == 1)

  out <- run_betting_cycle(

    sim_data = sim,

    idx = idx,

    active_players = rep(TRUE, 6),

    dt_focus = dt_focus_simple,

    dt_rival = dt_rival_simple,

    stage = "preflop",

    cycle = 1,

    current_bet = 1,

    pot = 1.5,

    street_investments = rep(0, 6)
  )

  expect_true(
    out$pot >= 1.5
  )
})



test_that("run_betting_cycle returns logical active_players", {

  sim <- simulate_many_hands(
    n_sim = 1,
    n_players = 6
  ) %>%

    assign_positions() %>%

    assign_player_types() %>%

    add_hand_context() %>%

    initialize_hand_state()

  idx <- which(sim$sim_id == 1)

  out <- run_betting_cycle(

    sim_data = sim,

    idx = idx,

    active_players = rep(TRUE, 6),

    dt_focus = dt_focus_simple,

    dt_rival = dt_rival_simple,

    stage = "preflop",

    cycle = 1,

    current_bet = 1,

    pot = 1.5,

    street_investments = rep(0, 6)
  )

  expect_true(
    is.logical(
      out$active_players
    )
  )
})
