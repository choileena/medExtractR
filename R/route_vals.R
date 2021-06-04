#' Keywords Specifying Route
#'
#' A dictionary mapping route expressions to standardized forms, specifying the
#' way in which a medication is administered. Example expressions include "oral",
#' "topical", "IV", and "intravenous".
#'
#' @format A data frame with route expressions (exact and/or regular expressions).
#' \describe{
#'   \item{expr}{A character vector, expressions to consider as route.}
#'   \item{value}{A standardized version of the raw expression. For example,
#'   "orally" and "by mouth" both have the standardized form "orally".}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(route_vals)
"route_vals"
