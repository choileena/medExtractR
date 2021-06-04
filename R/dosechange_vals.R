#' Keywords Specifying Dose Change
#'
#' A dictionary of words indicating a dose change, meaning that the associated
#' drug regimen may not be current. This includes phrases such as increase,
#' reduce, or switch. In the following example of clinical text, the word
#' \sQuote{increase} represents a dose change keyword: \dQuote{Increase prograf to 5mg bid.}
#'
#' @format A data frame with dose change expressions (exact and/or regular expressions).
#' \describe{
#'   \item{expr}{A character vector, expressions to consider as dose change.}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(dosechange_vals)
"dosechange_vals"
