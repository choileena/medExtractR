#' Additional expressions for \code{drug_list}
#'
#' A dictionary with additional expressions that can be used to supplement the \code{drug_list}
#' argument of \code{\link{medExtractR}} and \code{\link{medExtractR_tapering}}.
#'
#' @format A data frame with the following variables:
#' \describe{
#'   \item{expr}{A character vector, additional optional expressions for the \code{drug_list} argument.}
#'   \item{type}{A character vector, what category the expression belongs to (e.g., symptom, lab name, medication abbreviation, or drug class).}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(addl_expr)
"addl_expr"
