#' Extract Generic Attributes From Phrase
#'
#' This function searches a phrase for the position and length
#' of expressions specified in a dictionary.
#'
#' @param phrase Text to search.
#' @param dict data.frame, the first column should contain expressions to find.
#'
#' @return A numeric matrix with position and expression length.
#' @export

extract_generic <- function(phrase, dict) {
  df <- do.call(rbind, lapply(dict[,1], function(r1) {
    fq <- gregexpr(paste0(r1, "\\b"), phrase, ignore.case=T, perl=T)[[1]]
    fq_len <- attributes(fq)$match.length
    cbind(fq, fq_len)
  }))
  colnames(df) <- c('pos','expr_len')
  df[df[,'pos'] != -1,,drop = FALSE]
}
