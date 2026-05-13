#' Initialize hand state variables
#'
#' Adds economic and betting-state variables required
#' for the AdamsPokeR betting engine.
#'
#' @param sim_data Simulation dataframe
#' @param starting_stack Starting stack size per player
#' @param small_blind_value Small blind amount
#' @param big_blind_value Big blind amount
#'
#' @return sim_data with initialized betting variables
#' @export

initialize_hand_state <- function(

  sim_data,

  starting_stack = 100,

  small_blind_value = 0.5,

  big_blind_value = 1

) {

  #### basic state ####

  sim_data <- sim_data %>%

    dplyr::mutate(

      stack = starting_stack,

      total_invested = 0,

      pot_size = 0,

      active = TRUE,

      all_in = FALSE
    )

  #### per-street investment tracking ####

  streets <- c(
    "preflop",
    "flop",
    "turn",
    "river"
  )

  for (st in streets) {

    sim_data[[paste0(
      "invest_",
      st
    )]] <- 0

    sim_data[[paste0(
      "decision_",
      st
    )]] <- NA_character_

    sim_data[[paste0(
      "remaining_",
      st
    )]] <- NA_integer_

    sim_data[[paste0(
      "pot_",
      st
    )]] <- NA_real_
  }

  #### ================================================= ####
  #### initialize EACH HAND independently ####
  #### ================================================= ####

  sim_ids <- unique(sim_data$sim_id)

  for (sid in sim_ids) {

    #### rows for current hand ####

    idx <- which(
      sim_data$sim_id == sid
    )

    #### identify blinds ####

    sb_idx <- idx[
      sim_data$small_blind[idx]
    ]

    bb_idx <- idx[
      sim_data$big_blind[idx]
    ]

    #### apply SB ####

    if (length(sb_idx) == 1) {

      sim_data$stack[sb_idx] <-

        sim_data$stack[sb_idx] -
        small_blind_value

      sim_data$total_invested[sb_idx] <-

        sim_data$total_invested[sb_idx] +
        small_blind_value

      sim_data$invest_preflop[sb_idx] <-

        small_blind_value
    }

    #### apply BB ####

    if (length(bb_idx) == 1) {

      sim_data$stack[bb_idx] <-

        sim_data$stack[bb_idx] -
        big_blind_value

      sim_data$total_invested[bb_idx] <-

        sim_data$total_invested[bb_idx] +
        big_blind_value

      sim_data$invest_preflop[bb_idx] <-

        big_blind_value
    }

    #### initialize hand pot ####

    blind_total <-

      (length(sb_idx) * small_blind_value) +

      (length(bb_idx) * big_blind_value)

    #### assign ONLY to this hand ####

    sim_data$pot_size[idx] <-

      blind_total

    sim_data$pot_preflop[idx] <-

      blind_total
  }

  #### all-in safeguard ####

  sim_data$all_in <-

    sim_data$stack <= 0

  #### return ####

  return(sim_data)
}
