#' Extract Generic Entities From Phrase
#'
#' This function searches a phrase for the position and length
#' of expressions specified in a dictionary. This is called within other main functions of
#' the package and generally not intended for use on its own.
#'
#' @param phrase Text to search.
#' @param dict data.frame, the first column should contain expressions to find.
#' These can be regular expressions or exact phrases.
#'
#' @details \code{extract_generic} is used to extract entities that are
#' identified with an associated dictionary of phrases or regular expressions,
#' such as dose change, frequency, intake time, route, or duration in
#' \code{\link{medExtractR}} and \code{\link{medExtractR_tapering}}, as well as
#' dose schedule, time keyword, transition, and preposition in \code{\link{medExtractR_tapering}}. This function
#' is called within \code{\link{extract_entities}}.
#'
#' @return A numeric matrix with position and expression length.
#' @export
#'
#' @examples
#' data(frequency_vals)
#' extract_generic("take two every day", dict = frequency_vals)
#' extract_generic("take two every morning",
#'                   dict = data.frame(c("morning", "every morning")))

extract_generic <- function (phrase, dict) {
  # faster to call `tolower` than ignore.case
  # only consider first string
  phrase <- tolower(phrase[1])
  x <- tolower(dict[,1])
  nc <- nchar(x)
  s1 <- substr(x, nc, nc)
  s2 <- substr(x, nc-1, nc-1)
  addEdge <- grepl("\\?|\\w|\\}", s1) & !grepl("\\\\", s2)
  x[addEdge] <- paste0(x[addEdge], "\\b")
  df <- do.call(rbind, lapply(x, function(r1) {
    expr <- gregexpr(r1, phrase, perl = TRUE)[[1]]
    expr_len <- attributes(expr)$match.length
    cbind(expr, expr_len)
  }))
  colnames(df) <- c("pos", "expr_len")
  df[df[, "pos"] != -1, , drop = FALSE]
}
