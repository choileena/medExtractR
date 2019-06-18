#' Keywords Specifying Intake Time
#'
#' A dictionary mapping intake time expressions to numeric values representing
#' the corresponding number of doses. The form of each intake time is given as a
#' regular expression.
#'
#' @format A data frame with 23 observations on the following variables.
#' \describe{
#'   \item{expr}{A character vector, expressions to consider as intake time}
#'   \item{value}{A numeric vector, numeric value of intake time}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(intaketime_vals)
"intaketime_vals"
