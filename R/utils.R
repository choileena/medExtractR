#' String Replace Tablet With Tabs
#'
#' This function standardizes various forms of \dQuote{tablet} or
#' \dQuote{capsule} with the string \dQuote{tabs}.
#'
#' @param phrase Text to search.
#'
#' @details Some entities use a keyword such as tablet, capsule, or pill to
#' identify the entities, e.g. dose amount. This standardizes possible
#' expressions of similar keywords.
#'
#' @return Updated phrase.
#' @keywords internal

replace_tab <- function(phrase) {
  gsub("(tab|cap|tablet|capsule|pill)(s?)", "tabs", phrase, ignore.case=TRUE)
}

#' Grab Entity Metadata
#'
#' This function builds the metadata based on position.
#'
#' @param phrase Text of interest.
#' @param p_offset Start position of text within greater document.
#' @param df data.frame with position and length of found medication entities
#'
#' @return data.frame with found medication entities and accompanying metadata.
#' @keywords internal

entity_metadata <- function(phrase, p_offset, df) {
  md <- NA
  if(nrow(df) > 0) {
    e_stop <- df[,'pos'] + df[,'expr_len']
    e_expr <- mapply(substr, phrase, df[,'pos'], e_stop - 1, USE.NAMES = FALSE)
    e_start_stop <- paste(df[,'pos'] + p_offset, e_stop + p_offset, sep = ':')
    df0 <- cbind(data.frame(expr = e_expr, start_stop = e_start_stop), df)

    # If some expressions overlap, it keeps the longest one
    # Happens when one expression is a subset of another and starts at the same place
    df1 <- do.call(rbind, lapply(split(df0, df0[['pos']]), function(i) {
      el <- i[,'expr_len']
      i[el == max(el),]
    }))
    df2 <- df1[order(df1[,'pos']),]
    end_prev <- unname(c(NA, rowSums(df2[-nrow(df2),c('pos','expr_len')])))
    df3 <- df2[is.na(end_prev) | df2[,'pos'] > end_prev,]

    md_str <- paste(df3$expr, df3$start_stop, sep = ";")
    if(length(md_str)) md <- md_str
  }
  md
}
