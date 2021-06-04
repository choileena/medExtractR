#' Keywords Specifying Intake Time
#'
#' A dictionary with intake time expressions representing the approximate time of
#' day when a dose should be taken. Example expressions include "in the morning",
#' "with lunch", "at bedtime", and "qpm". The form of each intake time is
#' given as a regular expression.
#'
#' @format A data frame with intake time expressions (exact and/or regular expressions).
#' \describe{
#'   \item{expr}{A character vector, expressions to consider as intake time.}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(intaketime_vals)
"intaketime_vals"
