add_nuts_context <- function(sim_data) {

  #### initialize columns ####

  sim_data$is_nuts_flop <- FALSE

  sim_data$is_nuts_turn <- FALSE

  #### loop rows ####

  for (i in seq_len(nrow(sim_data))) {

    #### player hole cards ####

    hole_cards <- c(

      sim_data$card1[i],

      sim_data$card2[i]

    )

    #### flop board ####

    flop_board <- c(

      sim_data$flop1[i],

      sim_data$flop2[i],

      sim_data$flop3[i]

    )

    #### turn board ####

    turn_board <- c(

      sim_data$flop1[i],

      sim_data$flop2[i],

      sim_data$flop3[i],

      sim_data$turn[i]

    )

    #### evaluate flop nuts ####

    flop_nuts <- identify_nuts(

      hole_cards = hole_cards,

      board_cards = flop_board

    )

    #### evaluate turn nuts ####

    turn_nuts <- identify_nuts(

      hole_cards = hole_cards,

      board_cards = turn_board

    )

    #### save outputs ####

    sim_data$is_nuts_flop[i] <-

      flop_nuts$has_nuts

    sim_data$is_nuts_turn[i] <-

      turn_nuts$has_nuts
  }

  #### return ####

  return(sim_data)
}
