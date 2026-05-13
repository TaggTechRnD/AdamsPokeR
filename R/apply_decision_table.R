#' Apply decision tables to simulation data
#'
#' Orchestrates bounded two-cycle betting across preflop, flop,
#' turn, and river using decision-table driven player behavior.
#'
#' @param sim_data Simulation dataframe
#' @param dt_focus Decision table for focus player
#' @param dt_rival Decision table for rival players
#' @param starting_stack Starting stack for all players
#'
#' @return Simulation dataframe with betting actions, stacks, pots, and winners
#' @export

apply_decision_table <- function(
    sim_data,
    dt_focus,
    dt_rival = NULL,
    starting_stack = 100
) {

  if (is.null(dt_rival)) {
    dt_rival <- dt_focus
  }

  stages <- c(
    "preflop",
    "flop",
    "turn",
    "river"
  )

  #### initialise hand economy ####

  sim_data <- initialize_hand_state(
    sim_data = sim_data,
    starting_stack = starting_stack
  )

  sim_ids <- unique(sim_data$sim_id)

  #### loop through hands ####

  for (sid in sim_ids) {

    idx <- which(sim_data$sim_id == sid)

    active_players <- rep(TRUE, length(idx))

    pot <- max(
      sim_data$pot_size[idx],
      na.rm = TRUE
    )

    #### loop through streets ####

    for (stage in stages) {

      if (sum(active_players) <= 1) {
        break
      }

      #### street setup ####

      if (stage == "preflop") {

        current_bet <- 1

        street_investments <-
          sim_data$invest_preflop[idx]

      } else {

        current_bet <- 0

        street_investments <- rep(
          0,
          length(idx)
        )
      }

      #### cycle 1 ####

      cycle1 <- run_betting_cycle(

        sim_data = sim_data,

        idx = idx,

        active_players = active_players,

        dt_focus = dt_focus,
        dt_rival = dt_rival,

        stage = stage,
        cycle = 1,

        current_bet = current_bet,
        pot = pot,

        street_investments =
          street_investments
      )

      sim_data <- cycle1$sim_data

      active_players <- cycle1$active_players

      current_bet <- cycle1$current_bet

      pot <- cycle1$pot

      street_investments <-
        cycle1$street_investments

      #### cycle 2 only if betting reopened ####

      if (
        cycle1$raise_occurred &&
        sum(active_players) > 1
      ) {

        cycle2 <- run_betting_cycle(

          sim_data = sim_data,

          idx = idx,

          active_players = active_players,

          dt_focus = dt_focus,
          dt_rival = dt_rival,

          stage = stage,
          cycle = 2,

          current_bet = current_bet,
          pot = pot,

          street_investments =
            street_investments
        )

        sim_data <- cycle2$sim_data

        active_players <- cycle2$active_players

        current_bet <- cycle2$current_bet

        pot <- cycle2$pot

        street_investments <-
          cycle2$street_investments
      }

      #### close street ####

      sim_data[[paste0(
        "pot_",
        stage
      )]][idx] <- pot

      sim_data$pot_size[idx] <- pot

      sim_data[[paste0(
        "remaining_",
        stage
      )]][idx] <- sum(active_players)

      #### mark inactive players as folded on later streets ####

      for (j in seq_along(idx)) {

        if (!active_players[j]) {

          sim_data[[paste0(
            "decision_",
            stage
          )]][idx[j]] <- "fold"
        }
      }
    }

    #### resolve winner ####

    remaining_idx <- idx[active_players]

    if (length(remaining_idx) == 0) {

      remaining_idx <- idx[1]
    }

    if (length(remaining_idx) == 1) {

      winner_idx <- remaining_idx

    } else {

      winner_idx <- remaining_idx[1]

      for (k in remaining_idx[-1]) {

        cmp <- compare_hands_from_eval(
          sim_data$final_eval[[k]],
          sim_data$final_eval[[winner_idx]]
        )

        if (cmp == 1) {

          winner_idx <- k
        }
      }
    }

    #### award pot ####

    sim_data$winner[idx] <- FALSE

    sim_data$winner[winner_idx] <- TRUE

    sim_data$stack[winner_idx] <-
      sim_data$stack[winner_idx] + pot

    sim_data$pot_size[idx] <- pot
  }

  sim_data
}
