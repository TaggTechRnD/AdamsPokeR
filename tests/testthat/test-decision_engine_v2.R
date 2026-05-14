test_that("decision engine v2 produces sane betting state outputs", {

  sim <- simulate_many_hands(
    n_sim = 100,
    n_players = 6,
    seed = 42
  )

  res <- sim %>%

    assign_positions() %>%

    assign_player_types() %>%

    add_hand_context() %>%

    apply_decision_table(

      dt_focus = dt_focus_simple,

      dt_rival = dt_rival_simple
    )

  #### ===================================================== ####
  #### CORE STRUCTURAL OUTPUTS
  #### ===================================================== ####

  expect_true("stack" %in% names(res))

  expect_true("pot_size" %in% names(res))

  expect_true("total_invested" %in% names(res))

  expect_true("decision_preflop" %in% names(res))

  expect_true("decision_flop" %in% names(res))

  expect_true("decision_turn" %in% names(res))

  expect_true("decision_river" %in% names(res))

  expect_true("position" %in% names(res))

  expect_true("small_blind" %in% names(res))

  expect_true("big_blind" %in% names(res))


  #### ===================================================== ####
  #### STACK SAFETY
  #### ===================================================== ####

  expect_true(all(
    res$stack >= 0,
    na.rm = TRUE
  ))

  expect_true(all(
    res$total_invested >= 0,
    na.rm = TRUE
  ))


  #### ===================================================== ####
  #### ACTION VALIDITY
  #### ===================================================== ####

  valid_actions <- c(
    "fold",
    "check",
    "call",
    "bet",
    "raise"
  )

  expect_true(all(
    na.omit(res$decision_preflop) %in%
      valid_actions
  ))

  expect_true(all(
    na.omit(res$decision_flop) %in%
      valid_actions
  ))

  expect_true(all(
    na.omit(res$decision_turn) %in%
      valid_actions
  ))

  expect_true(all(
    na.omit(res$decision_river) %in%
      valid_actions
  ))


  #### ===================================================== ####
  #### NO INVALID POST-FOLD ACTIONS
  #### folded players may now legitimately remain NA
  #### ===================================================== ####

  folded_preflop <-

    res$decision_preflop == "fold"

  later_actions <-

    res$decision_flop[folded_preflop]

  expect_true(all(
    is.na(later_actions) |
      later_actions == "fold"
  ))


  #### ===================================================== ####
  #### POT CONSISTENCY
  #### ===================================================== ####

  pot_summary <- res %>%

    dplyr::group_by(sim_id) %>%

    dplyr::summarise(

      invested_total =

        sum(
          total_invested,
          na.rm = TRUE
        ),

      final_pot =

        max(
          pot_size,
          na.rm = TRUE
        ),

      .groups = "drop"
    )

  expect_true(all(

    abs(

      pot_summary$invested_total -

        pot_summary$final_pot

    ) < 1e-6

  ))


  #### ===================================================== ####
  #### EXACTLY ONE WINNER
  #### ===================================================== ####

  winner_check <- res %>%

    dplyr::group_by(sim_id) %>%

    dplyr::summarise(

      n_winners =

        sum(
          winner,
          na.rm = TRUE
        ),

      .groups = "drop"
    )

  expect_true(all(
    winner_check$n_winners == 1
  ))


  #### ===================================================== ####
  #### STACK CONSERVATION
  #### total chips should remain constant
  #### ===================================================== ####

  stack_check <- res %>%

    dplyr::group_by(sim_id) %>%

    dplyr::summarise(

      total_stack =

        sum(
          stack,
          na.rm = TRUE
        ),

      .groups = "drop"
    )

  expect_true(all(

    abs(

      stack_check$total_stack -

        (6 * 100)

    ) < 1e-6

  ))


  #### ===================================================== ####
  #### POSITION RANDOMIZATION CHECK
  #### each player should appear in >1 position
  #### ===================================================== ####

  pos_check <- res %>%

    dplyr::distinct(
      sim_id,
      player,
      position
    ) %>%

    dplyr::group_by(player) %>%

    dplyr::summarise(

      n_positions =

        dplyr::n_distinct(position),

      .groups = "drop"
    )

  expect_true(all(
    pos_check$n_positions > 1
  ))


  #### ===================================================== ####
  #### BLIND DISTRIBUTION CHECK
  #### ===================================================== ####

  blind_check <- res %>%

    dplyr::group_by(player) %>%

    dplyr::summarise(

      sb_count =

        sum(
          small_blind,
          na.rm = TRUE
        ),

      bb_count =

        sum(
          big_blind,
          na.rm = TRUE
        ),

      .groups = "drop"
    )

  expect_true(all(
    blind_check$sb_count > 0
  ))

  expect_true(all(
    blind_check$bb_count > 0
  ))

})
