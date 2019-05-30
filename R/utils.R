#' String Replace Tablet With Tabs
#'
#' This function standardizes various forms of "tablet" with the string "tabs".
#'
#' @param phrase Text to search.
#'
#' @return Updated phrase.
#' @keywords internal

replace_tab <- function(phrase){
  gsub("(tab|cap|tablet|capsule|pill)(s?)", "tabs", phrase, ignore.case=T)
}

#' Grab Entity Metadata
#'
#' This function standardizes various forms of "tablet" with the string "tabs".
#'
#' @param phrase Text of interest.
#' @param p_offset Position of text within greater document.
#' @param df data.frame with position and length of found entities.
#'
#' @return data.frame with found entities and accompanying metadata.
#' @keywords internal

entity_metadata <- function(phrase, p_offset, df) {
  md <- NA
  if(nrow(df) > 0) {
    e_stop <- df[,'pos'] + df[,'expr_len']
    e_expr <- mapply(substr, phrase, df[,'pos'], e_stop - 1, USE.NAMES = FALSE)
    e_start_stop <- paste(df[,'pos'] + p_offset, e_stop + p_offset, sep = ':')
    df <- cbind(data.frame(expr = e_expr, start_stop = e_start_stop), df)

    # If some expressions overlap, it keeps the longest one
    # Happens when one expression is a subset of another and starts at the same place
    df1 <- df %>% dplyr::group_by(.data$pos) %>%
              dplyr::filter(.data$expr_len == max(.data$expr_len)) %>%
              dplyr::ungroup() %>%
              dplyr::arrange(.data$pos) %>%
              dplyr::mutate(end_prev = dplyr::lag(.data$pos) + dplyr::lag(.data$expr_len)) %>%
              dplyr::filter(is.na(.data$end_prev) | .data$pos > .data$end_prev) %>%
              dplyr::select(-.data$end_prev)

    md_str <- paste(df1$expr, df1$start_stop, sep = ";")
    if(length(md_str)) md <- md_str
  }
  md
}
