add_hand_context <- function(sim_data) {

  # ---------- helpers ----------

  get_ranks <- function(cards) {
    if (is.null(cards) || !is.data.frame(cards) || nrow(cards) == 0) return(integer(0))
    as.numeric(cards$rank)
  }

  get_suits <- function(cards) {
    if (is.null(cards) || !is.data.frame(cards) || nrow(cards) == 0) return(character(0))
    as.character(cards$suit)
  }

  detect_flush_draw <- function(suits_vec) {
    if (length(suits_vec) < 4) return(FALSE)
    max(table(suits_vec)) >= 4
  }

  detect_straight_draw <- function(ranks_vec) {
    if (length(ranks_vec) < 4) return(FALSE)

    r <- sort(unique(ranks_vec))

    # Ace low handling
    if (14 %in% r) r <- sort(unique(c(r, 1)))

    if (length(r) < 4) return(FALSE)

    for (i in 1:(length(r) - 3)) {
      window <- r[i:(i + 3)]
      if (max(window) - min(window) <= 4) return(TRUE)
    }

    FALSE
  }

  # ---------- main ----------

  sim_data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(

      # ---------- hole ----------
      hole_ranks = list(get_ranks(hole_cards)),
      hole_suits = list(get_suits(hole_cards)),

      pocket_pair   = length(hole_ranks) == 2 && hole_ranks[1] == hole_ranks[2],
      pocket_suited = length(hole_suits) == 2 && hole_suits[1] == hole_suits[2],

      # ---------- flop ----------
      flop_cards = list(rbind(hole_cards, board_flop)),

      flop_ranks = list(get_ranks(flop_cards)),
      flop_suits = list(get_suits(flop_cards)),

      flop_flush_draw    = detect_flush_draw(flop_suits),
      flop_straight_draw = detect_straight_draw(flop_ranks),

      # ---------- turn ----------
      turn_cards = list(rbind(hole_cards, board_turn)),

      turn_ranks = list(get_ranks(turn_cards)),
      turn_suits = list(get_suits(turn_cards)),

      turn_flush_draw    = detect_flush_draw(turn_suits),
      turn_straight_draw = detect_straight_draw(turn_ranks)

    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -hole_ranks, -hole_suits,
      -flop_cards, -flop_ranks, -flop_suits,
      -turn_cards, -turn_ranks, -turn_suits
    )
}
