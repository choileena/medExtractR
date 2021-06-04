#' Keywords Specifying Preposition
#'
#' A dictionary with preposition expressions. Such expressions often represent
#' a relationship with an adjacent entity. Since most expressions in this
#' dictionary are very short, we require word boundaries (any character other
#' than a letter or number) to appear on either side of the expression. Example
#' expressions include "for", "to", "until", and "in".
#'
#' @format A data frame with preposition expressions (exact and/or regular expressions).
#' \describe{
#'   \item{expr}{A character vector, expressions to consider as preposition.}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(preposition_vals)
"preposition_vals"
