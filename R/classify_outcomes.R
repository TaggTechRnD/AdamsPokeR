#' Classify outcomes based on decisions and actual results
#'
#' @param sim_data Output from apply_decision_table
#'
#' @return sim_data with outcome classification
#' @export
classify_outcomes <- function(sim_data) {

  sim_data %>%
    dplyr::mutate(

      # Determine last action stage
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

      outcome = dplyr::case_when(

        # GOOD decisions
        action_type == "fold" & winner == FALSE ~ "good_fold",
        action_type == "call" & winner == TRUE  ~ "good_call",

        # BAD decisions
        action_type == "fold" & winner == TRUE  ~ "bad_fold",
        action_type == "call" & winner == FALSE ~ "bad_call"
      ),

      # Optional: stage-aware label
      outcome_detail = paste(outcome, final_action, sep = "_")
    )
}
