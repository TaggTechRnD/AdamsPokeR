#' Win vs Focus Hand Analysis (Decision-Free)
#'
#' @param sim_data Output from simulate_many_hands()
#'
#' @return List of summary tables
#' @export
win_vs_focushand <- function(sim_data) {

  library(dplyr)

  cat("\n========================================\n")
  cat("Running win_vs_focushand analysis\n")
  cat("Total hands:", length(unique(sim_data$sim_id)), "\n")
  cat("Players per table (inferred):", length(unique(sim_data$player)), "\n")
  cat("========================================\n")

  # -------------------------------
  # SAFE EXTRACTORS
  # -------------------------------
  get_rank_class <- function(x) {
    sapply(x, function(e) {

      if (is.null(e)) return(NA_character_)

      # Case 1: proper named list
      if (!is.null(names(e)) && "rank_class" %in% names(e)) {
        return(as.character(e[["rank_class"]]))
      }

      # Case 2: numeric fallback → map to hand names
      if (is.numeric(e)) {
        return(dplyr::case_when(
          e == 1 ~ "high_card",
          e == 2 ~ "pair",
          e == 3 ~ "two_pair",
          e == 4 ~ "three_of_a_kind",
          e == 5 ~ "straight",
          e == 6 ~ "flush",
          e == 7 ~ "full_house",
          e == 8 ~ "four_of_a_kind",
          e == 9 ~ "straight_flush",
          TRUE ~ NA_character_
        ))
      }

      return(NA_character_)
    })
  }

  get_rank_value <- function(x) {
    sapply(x, function(e) {

      if (is.null(e)) return(NA_real_)

      if (!is.null(names(e)) && "rank_value" %in% names(e)) {
        return(as.numeric(e[["rank_value"]]))
      }

      if (is.numeric(e)) {
        return(as.numeric(e))
      }

      return(NA_real_)
    })
  }

  # -------------------------------
  # 1. FOCUS PLAYER DISTRIBUTION (RIVER)
  # -------------------------------
  focus_dist <- sim_data %>%
    filter(is_focus) %>%
    mutate(hand_type = get_rank_class(final_eval)) %>%
    filter(!is.na(hand_type)) %>%
    count(hand_type) %>%
    mutate(pct = n / sum(n)) %>%
    arrange(desc(pct))

  cat("-")
  # -------------------------------
  # 2. TABLE DISTRIBUTIONS
  # -------------------------------
  table_flop <- sim_data %>%
    mutate(hand_type = get_rank_class(flop_rank)) %>%
    filter(!is.na(hand_type)) %>%
    count(hand_type) %>%
    mutate(pct = n / sum(n)) %>%
    arrange(desc(pct))
  cat("-")

  table_turn <- sim_data %>%
    mutate(hand_type = get_rank_class(turn_rank)) %>%
    filter(!is.na(hand_type)) %>%
    count(hand_type) %>%
    mutate(pct = n / sum(n)) %>%
    arrange(desc(pct))
  cat("-")

  table_river <- sim_data %>%
    mutate(hand_type = get_rank_class(river_rank)) %>%
    filter(!is.na(hand_type)) %>%
    count(hand_type) %>%
    mutate(pct = n / sum(n)) %>%
    arrange(desc(pct))
  cat("-")

  # -------------------------------
  # 3. TRUE WINNING HAND (NO DECISIONS)
  # -------------------------------
  winners <- sim_data %>%
    mutate(rank_value = get_rank_value(final_eval)) %>%
    group_by(sim_id) %>%
    filter(rank_value == max(rank_value, na.rm = TRUE)) %>%
    ungroup()
  cat("-")

  winning_dist <- winners %>%
    mutate(hand_type = get_rank_class(final_eval)) %>%
    filter(!is.na(hand_type)) %>%
    count(hand_type) %>%
    mutate(pct = n / sum(n)) %>%
    arrange(desc(pct))
  cat("-")

  # -------------------------------
  # 4. FOCUS TRUE EQUITY BY STREET
  # -------------------------------
  focus_equity_flop <- sim_data %>%
    mutate(val = get_rank_value(flop_rank)) %>%
    group_by(sim_id) %>%
    mutate(best = max(val, na.rm = TRUE)) %>%
    summarise(focus_best = any(is_focus & val == best)) %>%
    summarise(pct = mean(focus_best)) %>%
    pull(pct)
  cat("-")

  focus_equity_turn <- sim_data %>%
    mutate(val = get_rank_value(turn_rank)) %>%
    group_by(sim_id) %>%
    mutate(best = max(val, na.rm = TRUE)) %>%
    summarise(focus_best = any(is_focus & val == best)) %>%
    summarise(pct = mean(focus_best)) %>%
    pull(pct)
  cat("-")

  focus_equity_river <- sim_data %>%
    mutate(val = get_rank_value(river_rank)) %>%
    group_by(sim_id) %>%
    mutate(best = max(val, na.rm = TRUE)) %>%
    summarise(focus_best = any(is_focus & val == best)) %>%
    summarise(pct = mean(focus_best)) %>%
    pull(pct)
  cat("-")

  equity_summary <- tibble(
    street = c("flop", "turn", "river"),
    focus_best_pct = c(focus_equity_flop, focus_equity_turn, focus_equity_river)

  )

  # -------------------------------
  # OUTPUT
  # -------------------------------
  cat("\n--- Focus Hand Distribution ---\n")
  print(focus_dist)

  cat("\n--- Table Hand Distribution (Flop) ---\n")
  print(table_flop)

  cat("\n--- Table Hand Distribution (Turn) ---\n")
  print(table_turn)

  cat("\n--- Table Hand Distribution (River) ---\n")
  print(table_river)

  cat("\n--- Winning Hand Distribution ---\n")
  print(winning_dist)

  cat("\n--- Focus True Equity by Street ---\n")
  print(equity_summary)

  return(list(
    focus_distribution = focus_dist,
    table_flop = table_flop,
    table_turn = table_turn,
    table_river = table_river,
    winning_distribution = winning_dist,
    focus_equity = equity_summary
  ))
}
