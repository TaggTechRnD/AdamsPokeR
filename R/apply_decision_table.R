#' Apply decision table to simulation data with dropout tracking
#'
#' @param sim_data Output from simulate_many_hands
#' @param decision_table Tibble of rules
#'
#' @return sim_data with decisions and remaining player tracking
#' @export

apply_decision_table <- function(sim_data, dt_focus, dt_rival = NULL) {

  if (is.null(dt_rival)) {
    dt_rival <- dt_focus
  }

  stages <- c("preflop", "flop", "turn", "river")

  sim_data <- sim_data |>
    dplyr::mutate(
      active = TRUE,
      decision_preflop = NA_character_,
      decision_flop = NA_character_,
      decision_turn = NA_character_,
      decision_river = NA_character_,
      remaining_preflop = NA_integer_,
      remaining_flop = NA_integer_,
      remaining_turn = NA_integer_,
      remaining_river = NA_integer_,
      betlevel_preflop = NA_real_,
      betlevel_flop    = NA_real_,
      betlevel_turn    = NA_real_,
      betlevel_river   = NA_real_
    )

  sim_ids <- unique(sim_data$sim_id)

  for (sid in sim_ids) {

    idx <- which(sim_data$sim_id == sid)
    active_players <- rep(TRUE, length(idx))

    for (stage in stages) {

      stage_raises <- 0
      current_bet_level <- 1

      if (sum(active_players) <= 1) {

        for (j in seq_along(idx)) {

          decision <- ifelse(active_players[j], "call", "fold")

          sim_data[[paste0("remaining_", stage)]][idx[j]] <- sum(active_players)
          sim_data[[paste0("decision_", stage)]][idx[j]] <- decision
          sim_data[[paste0("betlevel_", stage)]][idx[j]] <- current_bet_level
          sim_data$active[idx[j]] <- active_players[j]
        }

        next
      }

      for (j in seq_along(idx)) {

        remaining <- sum(active_players)
        sim_data[[paste0("remaining_", stage)]][idx[j]] <- remaining

        row <- sim_data[idx[j], ]
        data_list <- as.list(row)

        # --- CONTEXT ---
        if (j == 1) {
          n_active_before <- 0
          n_to_act_after  <- sum(active_players) - 1
          n_folded_before <- 0
          n_called_before <- 0
          n_raised_before <- 0
        } else {
          n_active_before <- sum(active_players[1:(j-1)])
          n_to_act_after  <- sum(active_players) - n_active_before - 1

          decision_col <- paste0("decision_", stage)
          decisions_so_far <- sim_data[[decision_col]][idx][1:(j-1)]

          n_folded_before <- sum(decisions_so_far == "fold", na.rm = TRUE)
          n_called_before <- sum(decisions_so_far == "call", na.rm = TRUE)
          n_raised_before <- sum(decisions_so_far == "raise", na.rm = TRUE)
        }

        data_list$remaining_players <- remaining
        data_list$current_bet_level <- current_bet_level

        # Strategy selection
        player_id <- sim_data$player[idx[j]]
        if (player_id == 1) {
          rules <- dt_focus[dt_focus$stage == stage, ]
        } else {
          rules <- dt_rival[dt_rival$stage == stage, ]
        }

        # --- DECISION LOGIC ---
        if (active_players[j]) {

          decision <- "call"

          # --- RULES FIRST ---
          if (evaluate_rule(rules$fold, data_list)) {

            if (remaining > 1) {
              decision <- "fold"
              active_players[j] <- FALSE
              sim_data$active[idx[j]] <- FALSE
            }

          } else if (evaluate_rule(rules$raise, data_list)) {

            decision <- "raise"
            current_bet_level <- current_bet_level + 1

          } else {

            # --- NEW PRESSURE ENGINE ---

            strength <- data_list[[paste0("adj_", stage)]]
            if (is.null(strength) || is.na(strength)) strength <- 0

            # 🔴 CRITICAL: normalize strength
            relative_strength <- strength / max(remaining, 1)

            # --- HARD FOLD RULES ---
            if (current_bet_level >= 4 && relative_strength < 1.5) {

              decision <- "fold"
              active_players[j] <- FALSE
              sim_data$active[idx[j]] <- FALSE

            } else if (current_bet_level >= 3 && relative_strength < 2.0) {

              decision <- "fold"
              active_players[j] <- FALSE
              sim_data$active[idx[j]] <- FALSE

            } else if (current_bet_level >= 2 && relative_strength < 2.5) {

              decision <- "fold"
              active_players[j] <- FALSE
              sim_data$active[idx[j]] <- FALSE

            } else {

              # --- CONTROLLED CALL ---
              call_prob <- min(1, relative_strength / 3)

              if (runif(1) < call_prob) {
                decision <- "call"
              } else {
                decision <- "fold"
                active_players[j] <- FALSE
                sim_data$active[idx[j]] <- FALSE
              }
            }
          }

        } else {
          decision <- "fold"
          sim_data$active[idx[j]] <- FALSE
        }

        if (is.na(decision)) decision <- "fold"

        sim_data[[paste0("decision_", stage)]][idx[j]] <- decision
        sim_data[[paste0("betlevel_", stage)]][idx[j]] <- current_bet_level

        if (decision == "raise") {
          stage_raises <- stage_raises + 1
        }
      }
    }

    # --- WINNER LOGIC ---
    final_idx <- idx[active_players]

    if (length(final_idx) >= 1) {

      best_idx <- final_idx[1]

      for (k in final_idx[-1]) {
        cmp <- compare_hands_from_eval(
          sim_data$final_eval[[k]],
          sim_data$final_eval[[best_idx]]
        )
        if (cmp == 1) best_idx <- k
      }

      sim_data$winner[idx] <- FALSE
      sim_data$winner[best_idx] <- TRUE

    } else {
      sim_data$winner[idx] <- FALSE
      sim_data$winner[idx[1]] <- TRUE
    }
  }

  sim_data
}
