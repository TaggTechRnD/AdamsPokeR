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
      remaining_river = NA_integer_
    )

  sim_ids <- unique(sim_data$sim_id)

  for (sid in sim_ids) {

    idx <- which(sim_data$sim_id == sid)

    # Track active players for this hand
    active_players <- rep(TRUE, length(idx))

    for (stage in stages) {

      # --- NEW: reset table pressure for this stage ---
      stage_raises <- 0

      # ✅ ALWAYS compute and store remaining players
      remaining <- sum(active_players)
      sim_data[[paste0("remaining_", stage)]][idx] <- remaining

      # If 0 or 1 players remain, no more decisions needed
      if (remaining <= 1) {

        for (j in seq_along(idx)) {

          if (active_players[j]) {
            decision <- "call"
          } else {
            decision <- "fold"
          }

          sim_data[[paste0("decision_", stage)]][idx[j]] <- decision
        }

        next
      }

      for (j in seq_along(idx)) {

        row <- sim_data[idx[j], ]
        data_list <- as.list(row)

        # --- UPDATED: inject interaction variables ---
        data_list$remaining_players <- remaining
        data_list$stage_raises <- stage_raises

        rules <- decision_table[decision_table$stage == stage, ]

        if (active_players[j]) {

          decision <- "call"

          if (evaluate_rule(rules$fold, data_list)) {
            decision <- "fold"
            active_players[j] <- FALSE

          } else if (evaluate_rule(rules$raise, data_list)) {
            decision <- "raise"

          } else if (evaluate_rule(rules$call, data_list)) {
            decision <- "call"
          }

        } else {
          decision <- "fold"
        }

        # --- WRITE DECISION ---
        sim_data[[paste0("decision_", stage)]][idx[j]] <- decision

        # --- NEW: update table pressure AFTER decision ---
        if (decision == "raise") {
          stage_raises <- stage_raises + 1
        }
      }
    }

    # Final winner logic (only among active players)
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
