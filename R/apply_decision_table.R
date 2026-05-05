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

      current_bet_level <- 1

      # If only one player remains → auto resolve
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
        } else {
          n_active_before <- sum(active_players[1:(j-1)])
          n_to_act_after  <- sum(active_players) - n_active_before - 1
        }

        data_list$remaining_players <- remaining
        data_list$current_bet_level <- current_bet_level
        data_list$n_active_before <- n_active_before
        data_list$n_to_act_after  <- n_to_act_after

        # --- STRATEGY SELECTION ---
        player_id <- sim_data$player[idx[j]]
        if (player_id == 1) {
          rules <- dt_focus[dt_focus$stage == stage, ]
        } else {
          rules <- dt_rival[dt_rival$stage == stage, ]
        }

        # --- DECISION ENGINE (PURE RULE-BASED) ---
        if (active_players[j]) {

          decision <- NA_character_

          # Priority: fold → raise → call
          if (evaluate_rule(rules$fold, data_list)) {

            if (remaining > 1) {
              decision <- "fold"
              active_players[j] <- FALSE
              sim_data$active[idx[j]] <- FALSE
            }

          } else if (evaluate_rule(rules$raise, data_list)) {

            decision <- "raise"
            current_bet_level <- current_bet_level + 1

          } else if (evaluate_rule(rules$call, data_list)) {

            decision <- "call"

          } else {

            # 🔑 deterministic fallback (engine-safe)
            decision <- "call"
          }

        } else {

          decision <- "fold"
          sim_data$active[idx[j]] <- FALSE
        }

        if (is.na(decision)) decision <- "fold"

        sim_data[[paste0("decision_", stage)]][idx[j]] <- decision
        sim_data[[paste0("betlevel_", stage)]][idx[j]] <- current_bet_level
      }
    }

    # --- WINNER RESOLUTION ---
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
