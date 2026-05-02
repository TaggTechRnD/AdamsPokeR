#' Evaluate a rule string safely
#'
#' @param rule Character string
#' @param data Named list (row data)
#'
#' @return TRUE/FALSE
evaluate_rule <- function(rule, data) {

  # Handle empty or NA rules safely
  if (is.na(rule) || rule == "" || length(rule) == 0) {
    return(FALSE)
  }

  tryCatch(
    {
      result <- eval(parse(text = rule), envir = data)

      # Ensure logical output
      if (is.logical(result) && length(result) == 1) {
        return(result)
      } else {
        return(FALSE)
      }
    },
    error = function(e) {
      # Optional: print warning once per failure
      # message(paste("Rule failed:", rule))
      return(FALSE)
    }
  )
}
