test_that("decision engine v2 produces sane betting state outputs", {

  sim <- simulate_many_hands(
    n_sim = 10,
    n_players = 6,
    seed = 42
  )

  res <- sim %>%

    assign_positions() %>%
    assign_player_types() %>%
    add_hand_context() %>%

    apply_decision_table(
      dt_focus = dt_neutral_complex,
      dt_rival = dt_neutral_complex
    )

  #### core structural outputs ####

  expect_true("stack" %in% names(res))
  expect_true("pot_size" %in% names(res))
  expect_true("total_invested" %in% names(res))

  expect_true("decision_preflop" %in% names(res))
  expect_true("decision_flop" %in% names(res))
  expect_true("decision_turn" %in% names(res))
  expect_true("decision_river" %in% names(res))

  #### stack safety ####

  expect_true(all(res$stack >= 0))

  expect_true(all(
    res$total_invested >= 0
  ))

  #### action validity ####

  valid_actions <- c(
    "fold",
    "check",
    "call",
    "bet",
    "raise"
  )

  expect_true(all(
    na.omit(res$decision_preflop) %in% valid_actions
  ))

  expect_true(all(
    na.omit(res$decision_flop) %in% valid_actions
  ))

  expect_true(all(
    na.omit(res$decision_turn) %in% valid_actions
  ))

  expect_true(all(
    na.omit(res$decision_river) %in% valid_actions
  ))

  #### no ghost actions ####

  folded_preflop <- res$decision_preflop == "fold"

  expect_true(all(
    res$decision_flop[folded_preflop] == "fold"
  ))

  #### pot consistency ####

  pot_summary <- res %>%
    dplyr::group_by(sim_id) %>%
    dplyr::summarise(

      invested_total =
        sum(total_invested, na.rm = TRUE),

      final_pot =
        max(pot_size, na.rm = TRUE),

      .groups = "drop"
    )

  expect_true(all(
    abs(
      pot_summary$invested_total -
        pot_summary$final_pot
    ) < 1e-6
  ))

  #### exactly one winner ####

  winner_check <- res %>%
    dplyr::group_by(sim_id) %>%
    dplyr::summarise(

      n_winners = sum(winner),

      .groups = "drop"
    )

  expect_true(all(
    winner_check$n_winners == 1
  ))

  #### stack conservation ####
  #### total chips should remain constant ####

  stack_check <- res %>%
    dplyr::group_by(sim_id) %>%
    dplyr::summarise(

      total_stack = sum(stack),

      .groups = "drop"
    )

  expect_true(all(
    abs(total_stack - (6 * 100)) < 1e-6
  ))

})
