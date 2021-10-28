#' Find Strings and Suggest Misspellings
#'
#' This function searches for text within one or more phrases, and looks
#' for partial matches. An exact match of the text should be found in order
#' for a suggestion to made.
#'
#' @param strings character vector; value(s) to find
#' @param search_data character vector; phrase(s) where values may exist
#' @param max_dist numeric; edit distance to use for partial matches. The default value is 2.
#' @param ignore.case logical; indicates if spelling case matters, defaulting to \sQuote{TRUE}
#'
#' @return data.frame with two columns, \sQuote{suggestion} and \sQuote{match}
#'
#' @examples
#' string_suggestions('penicillin', 'penicillan, penicillin, or penicilin?')
#' @export

string_suggestions <- function(strings, search_data, max_dist = 2, ignore.case = TRUE) {
  if(max_dist != 0) {
    grp <- cut(nchar(strings), c(0, 3, 5, Inf), labels = FALSE)
    grp1 <- strings[grp == 1]
    if(max_dist == 1) {
      grp2 <- strings[grp > 1]
      grp3 <- character(0)
    } else {
      grp2 <- strings[grp == 2]
      grp3 <- strings[grp == 3]
    }
  } else {
    grp1 <- strings
    grp2 <- character(0)
    grp3 <- character(0)
  }

  srch <- vector('list', 3)
  if(length(grp1)) {
    srch[[1]] <- do.call(rbind, lapply(grp1, stringPlace, search_data, stringPlaceExact, ignore.case = ignore.case))
  }
  if(length(grp2)) {
    srch[[2]] <- do.call(rbind, lapply(grp2, stringPlace, search_data, stringPlaceFuzzy, max_dist = 1, ignore.case = ignore.case))
  }
  if(length(grp3)) {
    srch[[3]] <- do.call(rbind, lapply(grp3, stringPlace, search_data, stringPlaceFuzzy, max_dist = max_dist, ignore.case = ignore.case))
  }
  allsrch <- do.call(rbind, srch)

  if(ignore.case) strings <- tolower(strings)
  val <- unique(allsrch[,'value'])
  known <- val %in% strings
  ver <- val[known]
  add <- val[!known]
  # verify `add`
  vadd <- checkFuzzy(add, ver, max_dist)
  if(is.null(vadd)) {
    sugg <- NULL
  } else {
    add <- sort(vadd[,2])
    sugg <- checkFuzzy(add, ver, max_dist, TRUE)
  }
  sugg
}
