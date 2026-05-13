#' Evaluate DT action for a single player
#'
#' @param row Single-row dataframe/tibble for player
#' @param dt Decision table
#' @param stage Current stage
#' @param current_bet Current highest bet on street
#' @param pot Current pot size
#' @param cycle Betting cycle number
#'
#' @return Named list containing:
#' action,
#' invest,
#' matched_rule
#'
#' @export

evaluate_dt_action <- function(
    row,
    dt,
    stage,
    current_bet,
    pot,
    cycle = 1
) {

  #### select correct rank ####

  rank_value <- switch(

    stage,

    preflop = row$preflop_value,

    flop = row$flop_rank,

    turn = row$turn_rank,

    river = row$river_rank
  )

  #### filter DT for current street ####

  dt_stage <- dt %>%
    dplyr::filter(
      street == stage
    )

  #### determine betting context ####

  facing_bet <- current_bet > 0

  #### default outputs ####

  matched_rule <- NULL

  decision <- NA_character_

  invest <- 0

  #### search DT ####

  for (r in seq_len(nrow(dt_stage))) {

    rr <- dt_stage[r, ]

    #### rank matching ####

    rank_match <-

      rank_value >= rr$min_rank &

      rank_value <= rr$max_rank

    #### wildcard-aware matching ####

    pp_match <-

      is.na(rr$pocket_pair) |

      row$pocket_pair ==
      rr$pocket_pair

    ps_match <-

      is.na(rr$pocket_suited) |

      row$pocket_suited ==
      rr$pocket_suited

    ffd_match <-

      is.na(rr$flop_flush_draw) |

      row$flop_flush_draw ==
      rr$flop_flush_draw

    fsd_match <-

      is.na(rr$flop_straight_draw) |

      row$flop_straight_draw ==
      rr$flop_straight_draw

    tfd_match <-

      is.na(rr$turn_flush_draw) |

      row$turn_flush_draw ==
      rr$turn_flush_draw

    tsd_match <-

      is.na(rr$turn_straight_draw) |

      row$turn_straight_draw ==
      rr$turn_straight_draw

    #### cycle matching ####
    #### optional future expansion ####

    cycle_match <- TRUE

    #### full match ####

    if (

      rank_match &

      pp_match &

      ps_match &

      ffd_match &

      fsd_match &

      tfd_match &

      tsd_match &

      cycle_match

    ) {

      matched_rule <- rr

      break
    }
  }

  #### fallback behavior ####

  if (is.null(matched_rule)) {

    decision <- ifelse(
      facing_bet,
      "fold",
      "check"
    )

    invest <- 0

  } else {

    #### chosen action ####

    decision <- matched_rule$action

    #### sizing logic ####

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

  #### sanity checks ####

  valid_actions <- c(
    "fold",
    "check",
    "call",
    "bet",
    "raise",
    "all_in"
  )

  if (!decision %in% valid_actions) {

    decision <- ifelse(
      facing_bet,
      "fold",
      "check"
    )

    invest <- 0
  }

  #### cap investment ####

  invest <- min(
    invest,
    row$stack
  )

  #### return ####

  return(list(

    action = decision,

    invest = invest,

    matched_rule = matched_rule

  ))
}
