#' Keywords/Symbols Specifying Transition
#'
#' A dictionary with transition symbols and expressions representing a break between consecutive
#' doses within a tapering regimen. This dictionary includes the expressions "then" and
#' "followed by", as well as the punctuation ",(?!\\\\s?then)" or ";(?!\\\\s?then)" (i.e., a
#' comma or semicolon not followed by the word "then").
#'
#' @format A data frame with transition expressions (exact and/or regular expressions).
#' \describe{
#'   \item{expr}{A character vector, expressions to consider as transitions.}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(transition_vals)
"transition_vals"
