#' Apply decision table with stack + pot tracking
#'
#' @param sim_data Simulation dataframe
#' @param dt_focus Decision table for focus player
#' @param dt_rival Decision table for rival players
#' @param starting_stack Starting stack for all players
#'
#' @return Simulation dataframe with betting actions
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

  #### initialise columns ####

  sim_data <- sim_data %>%
    dplyr::mutate(

      active = TRUE,

      stack = starting_stack,

      total_invested = 0,

      pot_size = 0,

      current_bet = 0,

      amount_to_call = 0,

      facing_bet = FALSE,

      decision_preflop = NA_character_,
      decision_flop    = NA_character_,
      decision_turn    = NA_character_,
      decision_river   = NA_character_,

      invest_preflop = 0,
      invest_flop    = 0,
      invest_turn    = 0,
      invest_river   = 0,

      remaining_preflop = NA_integer_,
      remaining_flop    = NA_integer_,
      remaining_turn    = NA_integer_,
      remaining_river   = NA_integer_,

      pot_preflop = 0,
      pot_flop    = 0,
      pot_turn    = 0,
      pot_river   = 0
    )

  #### loop through simulations ####

  sim_ids <- unique(sim_data$sim_id)

  for (sid in sim_ids) {

    idx <- which(sim_data$sim_id == sid)

    active_players <- rep(TRUE, length(idx))

    pot <- 0

    #### blinds ####

    sb_idx <- which(sim_data$small_blind[idx])
    bb_idx <- which(sim_data$big_blind[idx])

    if (length(sb_idx) == 1) {

      sim_data$stack[idx[sb_idx]] <-
        sim_data$stack[idx[sb_idx]] - 0.5

      sim_data$total_invested[idx[sb_idx]] <-
        sim_data$total_invested[idx[sb_idx]] + 0.5

      pot <- pot + 0.5
    }

    if (length(bb_idx) == 1) {

      sim_data$stack[idx[bb_idx]] <-
        sim_data$stack[idx[bb_idx]] - 1

      sim_data$total_invested[idx[bb_idx]] <-
        sim_data$total_invested[idx[bb_idx]] + 1

      pot <- pot + 1
    }

    #### stage loop ####

    for (stage in stages) {

      #### preflop starts with blinds ####
      #### postflop starts unchecked ####

      current_bet <- ifelse(
        stage == "preflop",
        1,
        0
      )

      for (j in seq_along(idx)) {

        #### skip folded players ####

        if (!active_players[j]) {

          sim_data[[paste0(
            "decision_",
            stage
          )]][idx[j]] <- "fold"

          next
        }

        #### current row ####

        row <- sim_data[idx[j], ]

        #### choose correct rank ####

        rank_value <- switch(
          stage,
          preflop = row$preflop_value,
          flop    = row$flop_rank,
          turn    = row$turn_rank,
          river   = row$river_rank
        )

        #### select strategy ####

        player_id <- row$player

        if (player_id == 1) {

          dt <- dt_focus

        } else {

          dt <- dt_rival
        }

        #### filter stage ####

        dt_stage <- dt %>%
          dplyr::filter(street == stage)

        #### betting state ####

        facing_bet <- current_bet > 0

        amount_to_call <- max(
          current_bet,
          0
        )

        #### find matching rule ####

        matched_rule <- NULL

        for (r in seq_len(nrow(dt_stage))) {

          rr <- dt_stage[r, ]

          #### rank matching ####

          rank_match <-
            rank_value >= rr$min_rank &
            rank_value <= rr$max_rank

          #### wildcard-aware matching ####

          pp_match <-
            is.na(rr$pocket_pair) |
            row$pocket_pair == rr$pocket_pair

          ps_match <-
            is.na(rr$pocket_suited) |
            row$pocket_suited == rr$pocket_suited

          ffd_match <-
            is.na(rr$flop_flush_draw) |
            row$flop_flush_draw == rr$flop_flush_draw

          fsd_match <-
            is.na(rr$flop_straight_draw) |
            row$flop_straight_draw == rr$flop_straight_draw

          tfd_match <-
            is.na(rr$turn_flush_draw) |
            row$turn_flush_draw == rr$turn_flush_draw

          tsd_match <-
            is.na(rr$turn_straight_draw) |
            row$turn_straight_draw == rr$turn_straight_draw

          #### rule match ####

          if (
            rank_match &
            pp_match &
            ps_match &
            ffd_match &
            fsd_match &
            tfd_match &
            tsd_match
          ) {

            matched_rule <- rr

            break
          }
        }

        #### fallback ####

        if (is.null(matched_rule)) {

          decision <- ifelse(
            facing_bet,
            "fold",
            "check"
          )

          invest <- 0

        } else {

          decision <- matched_rule$action

          #### sizing ####

          if (
            matched_rule$sizing_type ==
            "stack_pct"
          ) {

            invest <-
              row$stack *
              matched_rule$sizing_value

          } else if (
            matched_rule$sizing_type ==
            "pot_pct"
          ) {

            invest <-
              pot *
              matched_rule$sizing_value

          } else {

            invest <- 0
          }
        }

        #### apply action ####

        if (
          decision == "fold" &&
          sum(active_players) > 1
        ) {

          active_players[j] <- FALSE

          invest <- 0
        }

        #### cannot fold last player ####

        if (
          decision == "fold" &&
          sum(active_players) == 1
        ) {

          decision <- "check"
        }

        #### calls/checks ####

        if (decision %in% c("call", "check")) {

          invest <- amount_to_call
        }

        #### bets/raises ####

        if (decision %in% c("bet", "raise")) {

          current_bet <- max(
            current_bet,
            invest
          )
        }

        #### stack cap ####

        invest <- min(
          invest,
          row$stack
        )

        #### update state ####

        sim_data$stack[idx[j]] <-
          sim_data$stack[idx[j]] - invest

        sim_data$total_invested[idx[j]] <-
          sim_data$total_invested[idx[j]] + invest

        pot <- pot + invest

        #### save outputs ####

        sim_data[[paste0(
          "decision_",
          stage
        )]][idx[j]] <- decision

        sim_data[[paste0(
          "invest_",
          stage
        )]][idx[j]] <- invest

        sim_data[[paste0(
          "remaining_",
          stage
        )]][idx[j]] <- sum(active_players)

        sim_data[[paste0(
          "pot_",
          stage
        )]][idx[j]] <- pot

        sim_data$pot_size[idx[j]] <- pot
      }

      #### if one player remains ####

      if (sum(active_players) == 1) {
        break
      }
    }

    #### resolve winner ####

    remaining_idx <- idx[active_players]

    #### emergency fallback ####

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
  }

  sim_data
}
