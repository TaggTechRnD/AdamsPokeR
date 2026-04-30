#' Simulate a single Texas Hold'em hand with state tracking
#'
#' @param n_players Number of players
#' @param seed Optional seed
#'
#' @return Tibble with full hand state per player
#' @export
simulate_holdem_hand <- function(n_players = 2, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)
  if (n_players < 2) stop("Need at least 2 players")

  deck <- shuffle_deck(create_deck())

  # Deal hole cards
  players <- vector("list", n_players)

  for (i in seq_len(n_players)) {
    dealt <- deal_cards(deck, 2)
    players[[i]] <- dealt$hand
    deck <- dealt$remaining_deck
  }

  # Deal board stepwise
  flop <- deal_cards(deck, 3)
  deck <- flop$remaining_deck

  turn <- deal_cards(deck, 1)
  deck <- turn$remaining_deck

  river <- deal_cards(deck, 1)

  board_flop <- flop$hand
  board_turn <- rbind(board_flop, turn$hand)
  board_river <- rbind(board_turn, river$hand)

  # Evaluate each stage
  results <- lapply(seq_len(n_players), function(i) {

    hole <- players[[i]]

    # Pre-flop score proxy (simple)
    pre_value <- sum(hole$rank)

    # Flop
    flop_eval <- best_hand(rbind(hole, board_flop, hole[1,])) # pad to 7

    # Turn
    turn_eval <- best_hand(rbind(hole, board_turn))

    # River
    river_eval <- best_hand(rbind(hole, board_river))

    tibble::tibble(
      player = i,

      hole_cards = list(hole),

      board_flop = list(board_flop),
      board_turn = list(board_turn),
      board_river = list(board_river),

      preflop_value = pre_value,

      flop_rank = flop_eval$rank_value,
      turn_rank = turn_eval$rank_value,
      river_rank = river_eval$rank_value,

      final_eval = list(river_eval)
    )
  })

  results <- dplyr::bind_rows(results)

  # Determine winner using final_eval
  best_idx <- 1

  for (i in 2:n_players) {
    cmp <- compare_hands_from_eval(
      results$final_eval[[i]],
      results$final_eval[[best_idx]]
    )
    if (cmp == 1) best_idx <- i
  }

  results$winner <- results$player == best_idx

  results
}
