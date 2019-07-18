#' Extract Generic Entities From Phrase
#'
#' This function searches a phrase for the position and length
#' of expressions specified in a dictionary.
#'
#' @param phrase Text to search.
#' @param dict data.frame, the first column should contain expressions to find.
#' These can be regular expressions or exact phrases.
#'
#' @details \code{extract_generic} is used to extract entities that are
#' identified with an associated dictionary of phrases or regular expressions,
#' such as frequency or intake time in \code{\link{medExtractR}}. This function
#' is called within \code{\link{extract_entities}}.
#'
#' @return A numeric matrix with position and expression length.
#' @export
#'
#' @examples
#' data(freq_vals)
#' extract_generic("take two every day", freq_vals)
#' extract_generic("take two every morning",
#'                   data.frame(c("morning", "every morning")))

extract_generic <- function(phrase, dict) {
  df <- do.call(rbind, lapply(dict[,1], function(r1) {
    expr <- gregexpr(paste0("\\b", r1, "\\b"), phrase, ignore.case=TRUE, perl=TRUE)[[1]]
    expr_len <- attributes(expr)$match.length
    cbind(expr, expr_len)
  }))
  colnames(df) <- c('pos','expr_len')
  df[df[,'pos'] != -1,,drop = FALSE]
}
