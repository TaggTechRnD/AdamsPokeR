#' Run one betting cycle for a street
#'
#' @param sim_data Simulation dataframe
#' @param idx Row indices for current hand
#' @param active_players Logical vector
#' @param dt_focus DT for focus player
#' @param dt_rival DT for rivals
#' @param stage Current street
#' @param cycle Betting cycle number
#' @param current_bet Current highest bet
#' @param pot Current pot
#' @param street_investments Numeric vector of
#' investments already committed this street
#'
#' @return List containing updated:
#' sim_data,
#' active_players,
#' current_bet,
#' pot,
#' street_investments,
#' raise_occurred
#'
#' @export

run_betting_cycle <- function(

  sim_data,
  idx,
  active_players,

  dt_focus,
  dt_rival,

  stage,
  cycle,

  current_bet,
  pot,

  street_investments

) {

  #### ======================================== ####
  #### INITIAL ECONOMY SNAPSHOT
  #### ======================================== ####

  initial_total_chips <-

    sum(sim_data$stack[idx]) +

    pot

  #### track whether aggression occurred ####

  raise_occurred <- FALSE

  #### ======================================== ####
  #### ACTION ORDER
  #### ======================================== ####

  for (j in seq_along(idx)) {

    #### skip folded players ####

    if (!active_players[j]) {

      next
    }

    #### current player row ####

    row <- sim_data[idx[j], ]

    #### determine strategy ####

    player_id <- row$player

    if (player_id == 1) {

      dt <- dt_focus

    } else {

      dt <- dt_rival
    }

    #### amount needed to continue ####

    amount_to_call <-

      max(
        current_bet -
          street_investments[j],
        0
      )

    #### inject live betting state ####

    row$current_bet <- current_bet

    row$pot_size <- pot

    row$amount_to_call <- amount_to_call

    row$facing_bet <- amount_to_call > 0

    row$cycle <- cycle

    #### ======================================== ####
    #### GET DT ACTION
    #### ======================================== ####

    action_out <- evaluate_dt_action(

      row = row,

      dt = dt,

      stage = stage,

      current_bet = current_bet,

      pot = pot,

      cycle = cycle
    )

    decision <- action_out$action

    invest <- action_out$invest

    #### ======================================== ####
    #### LEGALITY ENFORCEMENT
    #### ======================================== ####

    #### cannot check facing a bet ####

    if (

      decision == "check" &&

      amount_to_call > 0

    ) {

      decision <- "fold"
    }

    #### calls always match exactly ####

    if (decision == "call") {

      invest <- amount_to_call
    }

    #### checks invest nothing ####

    if (decision == "check") {

      invest <- 0
    }

    #### fold logic ####

    if (

      decision == "fold" &&

      sum(active_players) > 1

    ) {

      active_players[j] <- FALSE

      invest <- 0
    }

    #### cannot fold final player ####

    if (

      decision == "fold" &&

      sum(active_players) == 1

    ) {

      decision <- "check"

      invest <- 0
    }

    #### ======================================== ####
    #### BET / RAISE LOGIC
    #### ======================================== ####

    if (

      decision %in% c(
        "bet",
        "raise",
        "all_in"
      )

    ) {

      proposed_total <-

        street_investments[j] +
        invest

      #### minimum raise safeguard ####

      if (

        proposed_total <= current_bet

      ) {

        proposed_total <-

          current_bet +
          max(1, invest)
      }

      #### true raise amount ####

      invest <-

        proposed_total -
        street_investments[j]

      #### update table bet ####

      current_bet <-

        street_investments[j] +
        invest

      raise_occurred <- TRUE
    }

    #### ======================================== ####
    #### STACK CAP
    #### ======================================== ####

    invest <- min(

      invest,

      sim_data$stack[idx[j]]
    )

    #### ======================================== ####
    #### APPLY ECONOMY
    #### ======================================== ####

    sim_data$stack[idx[j]] <-

      sim_data$stack[idx[j]] -
      invest

    sim_data$total_invested[idx[j]] <-

      sim_data$total_invested[idx[j]] +
      invest

    #### street contribution ####

    street_investments[j] <-

      street_investments[j] +
      invest

    #### update pot ####

    pot <- pot + invest

    #### ======================================== ####
    #### SAVE OUTPUTS
    #### ======================================== ####

    sim_data[[paste0(
      "decision_",
      stage
    )]][idx[j]] <- decision

    sim_data[[paste0(
      "invest_",
      stage
    )]][idx[j]] <-

      street_investments[j]

    sim_data[[paste0(
      "remaining_",
      stage
    )]][idx[j]] <-

      sum(active_players)

    sim_data[[paste0(
      "pot_",
      stage
    )]][idx[j]] <- pot

    sim_data$pot_size[idx[j]] <- pot
  }

  #### ======================================== ####
  #### FINAL ECONOMY VALIDATION
  #### ======================================== ####

  final_total_chips <-

    sum(sim_data$stack[idx]) +

    pot

  #### hard safeguard ####

  if (

    abs(
      initial_total_chips -
      final_total_chips
    ) > 1e-6

  ) {

    warning(

      paste0(

        "Chip conservation violated in run_betting_cycle(): ",

        "initial = ",
        initial_total_chips,

        ", final = ",
        final_total_chips
      )
    )
  }

  #### ======================================== ####
  #### RETURN UPDATED STATE
  #### ======================================== ####

  return(list(

    sim_data = sim_data,

    active_players = active_players,

    current_bet = current_bet,

    pot = pot,

    street_investments = street_investments,

    raise_occurred = raise_occurred

  ))
}
