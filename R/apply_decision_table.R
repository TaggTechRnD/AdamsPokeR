#' Apply decision table to simulation data with dropout tracking
#'
#' @param sim_data Output from simulate_many_hands
#' @param decision_table Tibble of rules
#'
#' @return sim_data with decisions and remaining player tracking
#' @export

apply_decision_table <- function(sim_data, decision_table) {

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

      remaining <- sum(active_players)
      sim_data[[paste0("remaining_", stage)]][idx] <- remaining

      if (remaining <= 1) {

        for (j in seq_along(idx)) {

          decision <- ifelse(active_players[j], "call", "fold")

          sim_data[[paste0("decision_", stage)]][idx[j]] <- decision
          sim_data[[paste0("betlevel_", stage)]][idx[j]] <- current_bet_level
        }

        next
      }

      for (j in seq_along(idx)) {

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
        data_list$stage_raises      <- stage_raises
        data_list$current_bet_level <- current_bet_level

        data_list$n_active_before   <- n_active_before
        data_list$n_folded_before   <- n_folded_before
        data_list$n_called_before   <- n_called_before
        data_list$n_raised_before   <- n_raised_before
        data_list$n_to_act_after    <- n_to_act_after

        rules <- decision_table[decision_table$stage == stage, ]

        # --- DECISION LOGIC ---
        if (active_players[j]) {

          decision <- "call"

          # 1. Rule-based decisions first
          if (evaluate_rule(rules$fold, data_list)) {

            decision <- "fold"
            active_players[j] <- FALSE

          } else if (evaluate_rule(rules$raise, data_list)) {

            decision <- "raise"
            current_bet_level <- current_bet_level + 1

          } else if (evaluate_rule(rules$call, data_list)) {

            decision <- "call"

          } else {

            # 2. NEW: PRESSURE-BASED FALLBACK

            strength <- data_list[[paste0("adj_", stage)]]

            if (is.null(strength) || is.na(strength)) {
              strength <- 0
            }

            if (current_bet_level >= 3 && strength < 2.5) {

              decision <- "fold"
              active_players[j] <- FALSE

            } else if (current_bet_level >= 2 && strength < 1.5) {

              decision <- "fold"
              active_players[j] <- FALSE

            } else {

              decision <- "call"
            }
          }

        } else {
          decision <- "fold"
        }

        # safety
        if (is.na(decision)) decision <- "fold"

        sim_data[[paste0("decision_", stage)]][idx[j]] <- decision

        # persist bet level
        sim_data[[paste0("betlevel_", stage)]][idx[j]] <- current_bet_level

        if (decision == "raise") {
          stage_raises <- stage_raises + 1
        }
      }
    }

    # --- WINNER LOGIC ---
    final_idx <- idx[active_players]

    if (length(final_idx) > 0) {

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
    }
  }

  sim_data
}
