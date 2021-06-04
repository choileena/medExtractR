#' Keywords Specifying Time Keyword
#'
#' A dictionary with time keyword expressions representing whether the dosing regimen is
#' past, current, or future. Example expressions include "currently", "remain","not taking",
#' "yesterday", and "past".
#'
#' @format A data frame with time keyword expressions (exact and/or regular expressions).
#' \describe{
#'   \item{expr}{A character vector, expressions to consider as time keyword.}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(timekeyword_vals)
"timekeyword_vals"
