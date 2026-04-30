#' Evaluate a rule string safely
#'
#' @param rule Character string
#' @param data Named list (row data)
#'
#' @return TRUE/FALSE
evaluate_rule <- function(rule, data) {
  eval(parse(text = rule), envir = data)
}
