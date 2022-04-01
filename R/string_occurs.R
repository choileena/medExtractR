#' Find Strings in Text
#'
#' This function searches for text within one or more phrases. Text to look for will be grouped
#' into values that are found and not found.
#'
#' @param dict_list character vector; value(s) to find
#' @param haystack character vector; phrase(s) where values may exist
#' @param ignore.case logical; indicates if spelling case matters, defaulting to \sQuote{TRUE}
#' @param nClust Number of CPU cores to use, if available. This requires the \sQuote{parallel}
#' package.
#'
#' @return list with two elements, \sQuote{TRUE} and \sQuote{FALSE}, representing values
#' that are found or not found within the phrase to search.
#'
#' @examples
#' note1 <- "I am the very model of a modern major general
#' I've information vegetable, animal, and mineral
#' I know the kings of England, and I quote the fights historical
#' From marathon to Waterloo in order categorical;
#' I'm very well acquainted, too, with matters mathematical,
#' I understand equations both the simple and quadratical
#' About binomial theorem I'm teeming with a lot o' news,
#' With many cheerful facts about the square of the hypotenuse"
#' note2 <- "The quick brown fox jumps over the lazy dog"
#' string_occurs(c('kings','quick','couth','brown'), c(note1, note2))
#' @export

string_occurs <- function(dict_list, haystack, ignore.case = TRUE, nClust = 2) {
  patt <- sprintf("\\b%s\\b", dict_list)
  if(ignore.case) {
    haystack <- tolower(haystack)
    patt <- tolower(patt)
  }
  patt <- gsub('+', '\\+', patt, fixed = TRUE)
  patt <- gsub('(', '\\(', patt, fixed = TRUE)
  patt <- gsub(')', '\\)', patt, fixed = TRUE)

  isFound <- function(pattern) any(stringi::stri_detect_regex(haystack, pattern, max_count = 1))
  if(nClust > 1 && requireNamespace("parallel", quietly = TRUE)) {
    nCores <- getOption('cl.cores', nClust)
    cl <- tryCatch(parallel::makeCluster(nCores), error = function(e) {
      # setup_strategy may be deprecated, so investigate
      parallel::makeCluster(nCores, setup_strategy = "sequential")
    })
  }
  if(!is.null(cl)) {
    appears <- parallel::parSapply(cl, patt, isFound, USE.NAMES = FALSE)
    parallel::stopCluster(cl)
  } else {
    appears <- vapply(patt, isFound, logical(1), USE.NAMES = FALSE)
  }
  list('TRUE' = dict_list[appears], 'FALSE' = dict_list[!appears])
}
