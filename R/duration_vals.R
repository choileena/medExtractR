#' Keywords Specifying Duration
#'
#' A dictionary mapping duration expressions to numeric values representing
#' the corresponding number of doses per day. The form of each duration is
#' given as a regular expression.
#'
#' @format A data frame with XX observations on the following variables.
#' \describe{
#'   \item{expr}{A character vector, expressions to consider as duration}
#'   \item{value}{A numeric vector, numeric value of duration}?
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(duration_vals)
"duration_vals"
