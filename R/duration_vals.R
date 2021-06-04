#' Keywords Specifying Duration
#'
#' A dictionary with phrases indicating how long the patient should take a particular
#' dose of the drug. Examples of duration expressions include "2 weeks", "14 days",
#' "another 3 days", "through mid-April", or a specific date. The form of each duration is
#' given as a regular expression.
#'
#' @format A data frame with duration expressions (exact and/or regular expressions).
#' \describe{
#'   \item{expr}{A character vector, expressions to consider as duration.}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(duration_vals)
"duration_vals"
