#' Keywords Specifying Frequency
#'
#' A dictionary mapping frequency expressions to numeric values representing
#' the corresponding number of doses per day. Example expressions include
#' "q12 hours", "bid", "daily", and "three times a day". The form of each frequency is
#' given as a regular expression.
#'
#' @format A data frame with frequency expressions (exact and/or regular expressions).
#' \describe{
#'   \item{expr}{A character vector, expressions to consider as frequency.}
#'   \item{value}{A numeric vector, numeric value of frequency represented as number of doses
#'   taken per day. For example, \dQuote{bid} and \dQuote{twice a day} would both have a numeric value of 2.}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(frequency_vals)
"frequency_vals"
