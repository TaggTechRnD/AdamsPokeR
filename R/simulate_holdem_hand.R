#' Simulate a single Texas Hold'em hand
#'
#' @param n_players Number of players (2–10 recommended)
#' @param seed Optional seed
#'
#' @return Tibble with player results
#' @export
simulate_holdem_hand <- function(n_players = 2, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  if (n_players < 2) {
    stop("Need at least 2 players")
  }

  deck <- shuffle_deck(create_deck())

  # Deal hole cards
  players <- vector("list", n_players)

  for (i in seq_len(n_players)) {
    dealt <- deal_cards(deck, 2)
    players[[i]] <- dealt$hand
    deck <- dealt$remaining_deck
  }

  # Deal community cards
  flop <- deal_cards(deck, 3)
  deck <- flop$remaining_deck

  turn <- deal_cards(deck, 1)
  deck <- turn$remaining_deck

  river <- deal_cards(deck, 1)
  board <- rbind(flop$hand, turn$hand, river$hand)

  # Evaluate each player
  results <- lapply(seq_len(n_players), function(i) {
    full_hand <- rbind(players[[i]], board)
    best <- best_5_from_7(full_hand)

    tibble::tibble(
      player = i,
      rank_class = best$rank_class,
      rank_value = best$rank_value,
      tiebreaker = list(best$tiebreaker)
    )
  })

  results <- dplyr::bind_rows(results)

  # Determine winner
  best_idx <- 1
  for (i in 2:n_players) {
    cmp <- compare_hands_from_eval(results[i, ], results[best_idx, ])
    if (cmp == 1) best_idx <- i
  }

  results$winner <- results$player == best_idx

  results
}
