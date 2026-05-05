#' Conditional Flop Situation Analyzer
#'
#' @param sim_data raw simulation output (no decisions applied)
#' @param focus_hand character (e.g. "two_pair", "flush_draw", "pair")
#' @param n_players optional filter
#'
#' @return list of probabilities
#' @export
conditional_flop_analyzer <- function(sim_data,
                                      focus_hand = "two_pair",
                                      n_players = NULL) {

  library(dplyr)

  # -------------------------------
  # helpers (same as before)
  # -------------------------------
  get_rank_class <- function(x) {
    sapply(x, function(e) {
      if (is.null(e)) return(NA_character_)

      if (!is.null(names(e)) && "rank_class" %in% names(e)) {
        return(as.character(e[["rank_class"]]))
      }

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

      NA_character_
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

      NA_real_
    })
  }

  # -------------------------------
  # prep
  # -------------------------------
  df <- sim_data %>%
    mutate(
      flop_class  = get_rank_class(flop_rank),
      turn_class  = get_rank_class(turn_rank),
      river_class = get_rank_class(river_rank),

      flop_val  = get_rank_value(flop_rank),
      turn_val  = get_rank_value(turn_rank),
      river_val = get_rank_value(river_rank)
    )

  if (!is.null(n_players)) {
    df <- df %>%
      group_by(sim_id) %>%
      filter(n() == n_players) %>%
      ungroup()
  }

  # -------------------------------
  # isolate hands where focus has X at flop
  # -------------------------------
  focus_cases <- df %>%
    filter(is_focus, flop_class == focus_hand)

  if (nrow(focus_cases) == 0) {
    cat("No matching cases found.\n")
    return(NULL)
  }

  # -------------------------------
  # join back to full table
  # -------------------------------
  joined <- df %>%
    inner_join(focus_cases %>% select(sim_id, focus_flop_val = flop_val,
                                      focus_turn_val = turn_val,
                                      focus_river_val = river_val),
               by = "sim_id")

  # -------------------------------
  # 1. already beaten at flop
  # -------------------------------
  beaten_flop <- joined %>%
    group_by(sim_id) %>%
    summarise(
      beaten = any(flop_val > first(focus_flop_val), na.rm = TRUE)
    )

  p_beaten_flop <- mean(beaten_flop$beaten)

  # -------------------------------
  # 2. improve by river
  # -------------------------------
  improve <- focus_cases %>%
    summarise(
      improved = mean(river_val > flop_val, na.rm = TRUE)
    ) %>%
    pull(improved)

  # -------------------------------
  # 3. improve but still lose
  # -------------------------------
  improved_but_lost <- joined %>%
    group_by(sim_id) %>%
    summarise(
      focus_improved = first(focus_river_val) > first(focus_flop_val),
      still_beaten   = any(river_val > first(focus_river_val), na.rm = TRUE)
    ) %>%
    summarise(
      pct = mean(focus_improved & still_beaten)
    ) %>%
    pull(pct)

  # -------------------------------
  # 4. already best at flop
  # -------------------------------
  best_flop <- joined %>%
    group_by(sim_id) %>%
    summarise(
      best = all(flop_val <= first(focus_flop_val), na.rm = TRUE)
    )

  p_best_flop <- mean(best_flop$best)

  # -------------------------------
  # output
  # -------------------------------
  result <- tibble::tibble(
    focus_hand = focus_hand,
    cases = nrow(focus_cases),
    p_best_flop = p_best_flop,
    p_beaten_flop = p_beaten_flop,
    p_improve_by_river = improve,
    p_improve_but_lose = improved_but_lost
  )

  print(result)
  return(result)
}
