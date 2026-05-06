#' Example Poker Strategies
#'
#' Predefined decision tables for testing strategy performance.
#'
#' @format A list of tibbles:
#' \describe{
#'   \item{dt_tight}{Conservative strategy}
#'   \item{dt_base}{Baseline strategy}
#'   \item{dt_loose}{Aggressive strategy}
#'   \item{dt_positional}{Position-aware strategy}
#'   \item{dt_neutral_complex}{Baseline model testing strategy}
#'   \item{example_strategies}{list object of all example strategy tibbles}
#' }
"dt_tight"

"dt_base"

"dt_loose"

"dt_positional"

"dt_neutral_complex"

"example_strategies"

#' Rival Poker Strategies
#'
#' Baseline opponent decision tables for simulation environments.
#'
#' @format A list of tibbles:
#' \describe{
#'   \item{baseline}{Pressure-aware baseline opponent}
#' }
"rival_strategies"
