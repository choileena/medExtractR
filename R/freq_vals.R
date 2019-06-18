#' Keywords Specifying Frequency
#'
#' A dictionary mapping frequency expressions to numeric values representing 
#' the corresponding number of doses per day. The form of each frequency is
#' given as a regular expression.
#'
#' @format A data frame with 77 observations on the following variables.
#' \describe{
#'   \item{expr}{A character vector, expressions to consider as frequency}
#'   \item{value}{A numeric vector, numeric value of frequency}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(freq_vals)
"freq_vals"
