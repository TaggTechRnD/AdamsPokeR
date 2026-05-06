validate_decision_table <- function(dt) {

  #### required columns ####
  required_cols <- c(
    "priority",
    "street",
    "facing_bet",
    "rank_min",
    "rank_max",
    "flush_draw",
    "straight_draw",
    "action",
    "bet_size"
  )

  #### valid values ####
  valid_streets <- c(
    "preflop",
    "flop",
    "turn",
    "river",
    "any"
  )

  valid_actions <- c(
    "fold",
    "check",
    "call",
    "bet",
    "raise"
  )

  #### structural checks ####

  if (!is.data.frame(dt)) {
    stop("dt must be a data.frame or tibble")
  }

  if (nrow(dt) == 0) {
    stop("dt has 0 rows")
  }

  if (anyDuplicated(names(dt))) {
    stop("dt contains duplicated column names")
  }

  missing_cols <- setdiff(required_cols, names(dt))

  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Missing required columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  #### type checks ####

  if (!is.numeric(dt$priority)) {
    stop("priority must be numeric/integer")
  }

  if (!is.character(dt$street)) {
    stop("street must be character")
  }

  if (!is.logical(dt$facing_bet)) {
    stop("facing_bet must be logical")
  }

  if (!is.numeric(dt$rank_min)) {
    stop("rank_min must be numeric")
  }

  if (!is.numeric(dt$rank_max)) {
    stop("rank_max must be numeric")
  }

  if (!is.logical(dt$flush_draw)) {
    stop("flush_draw must be logical")
  }

  if (!is.logical(dt$straight_draw)) {
    stop("straight_draw must be logical")
  }

  if (!is.character(dt$action)) {
    stop("action must be character")
  }

  if (!is.numeric(dt$bet_size)) {
    stop("bet_size must be numeric")
  }

  #### value checks ####

  bad_streets <- unique(
    dt$street[!dt$street %in% valid_streets]
  )

  if (length(bad_streets) > 0) {
    stop(
      paste(
        "Invalid street values:",
        paste(bad_streets, collapse = ", ")
      )
    )
  }

  bad_actions <- unique(
    dt$action[!dt$action %in% valid_actions]
  )

  if (length(bad_actions) > 0) {
    stop(
      paste(
        "Invalid action values:",
        paste(bad_actions, collapse = ", ")
      )
    )
  }

  #### logical consistency ####

  if (any(dt$rank_min > dt$rank_max, na.rm = TRUE)) {
    stop("Some rows have rank_min > rank_max")
  }

  if (anyDuplicated(dt$priority)) {
    stop("priority values must be unique")
  }

  if (any(is.na(dt$action))) {
    stop("action contains NA values")
  }

  if (any(dt$bet_size < 0, na.rm = TRUE)) {
    stop("bet_size contains negative values")
  }

  #### fallback rule check ####

  fallback_exists <- any(
    dt$street == "any"
  )

  if (!fallback_exists) {
    stop("No fallback rule found (street == 'any')")
  }

  #### success ####

  invisible(TRUE)

}
