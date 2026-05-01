#' Classify outcomes based on decisions and actual results
#'
#' @param sim_data Output from apply_decision_table
#'
#' @return sim_data with outcome classification
#' @export
classify_outcomes <- function(sim_data) {

  sim_data %>%
    dplyr::group_by(sim_id) %>%
    dplyr::mutate(

      # --------
      # Determine last action stage
      # --------
      final_action = dplyr::case_when(
        decision_preflop == "fold" ~ "preflop",
        decision_flop == "fold"    ~ "flop",
        decision_turn == "fold"    ~ "turn",
        TRUE                       ~ "showdown"
      ),

      action_type = dplyr::case_when(
        final_action == "showdown" ~ "call",
        TRUE ~ "fold"
      ),

      # --------
      # Outcome quality (your existing logic)
      # --------
      outcome = dplyr::case_when(

        # GOOD decisions
        action_type == "fold" & winner == FALSE ~ "good_fold",
        action_type == "call" & winner == TRUE  ~ "good_call",

        # BAD decisions
        action_type == "fold" & winner == TRUE  ~ "bad_fold",
        action_type == "call" & winner == FALSE ~ "bad_call"
      ),

      outcome_detail = paste(outcome, final_action, sep = "_"),

      # --------
      # Active player counts per stage
      # --------
      active_preflop = sum(decision_preflop != "fold"),
      active_flop    = sum(decision_flop != "fold" & decision_preflop != "fold"),
      active_turn    = sum(decision_turn != "fold" & decision_flop != "fold"),
      active_river   = sum(decision_river != "fold" & decision_turn != "fold"),

      # --------
      # Win type (NEW)
      # --------
      win_type = dplyr::case_when(

        # Fold wins (ordered earliest → latest)
        winner & active_preflop == 1 ~ "fold_win_preflop",
        winner & active_flop    == 1 ~ "fold_win_flop",
        winner & active_turn    == 1 ~ "fold_win_turn",
        winner & active_river   == 1 ~ "fold_win_river",

        # Showdown win
        winner ~ "showdown_win",

        # Otherwise loss
        TRUE ~ "loss"
      )

    ) %>%
    dplyr::ungroup()
}
