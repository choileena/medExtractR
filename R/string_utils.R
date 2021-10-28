#' Internal String Functions
#'
#' code{stringPlaceExact}: find location of string, using exact match
#'
#' code{stringPlaceFuzzy}: find location of string, using partial match
#'
#' code{stringPlace}: find location of string, generic
#'
#' code{checkFuzzy}: check for "close" string matches
#'
#' @name strings-internal
#' @aliases stringPlaceExact stringPlaceFuzzy stringPlace checkFuzzy
#' @keywords internal

# haystack is single item
stringPlaceExact <- function(needle, haystack, ignore.case = TRUE) {
  patt <- sprintf("\\b%s\\b", needle)
  if(ignore.case) {
    haystack <- tolower(haystack)
    patt <- tolower(patt)
  }
  regmatches(haystack, gregexpr(patt, haystack))[[1]]
}

stringPlaceFuzzy <- function(needle, haystack, max_dist = 0, ignore.case = TRUE) {
  patt <- sprintf("\\b%s\\b", needle)
# `\b` seems to have a bug; use \W and sub out at end
#   patt <- sprintf("\\W%s\\W", needle)
  if(ignore.case) {
    haystack <- tolower(haystack)
    patt <- tolower(patt)
  }
  pos <- 1
  cnt <- 1
  res <- character(0)
  while(pos <= nchar(haystack)) {
    hay <- substring(haystack, pos)
    ans <- utils::aregexec(patt, hay, max.distance = max_dist)[[1]]
    if(ans == -1) break
    # if there's an exact match, anything prior to it will not be returned
    hay1 <- substring(hay, 1, c(ans) - 1)
    ans1 <- utils::aregexec(patt, hay1, max.distance = max_dist)[[1]]
    if(ans1 != -1) {
      ans <- ans1
    }
    pos <- pos + c(ans) + attr(ans, 'match.length') - 1
    res[cnt] <- regmatches(hay, ans)
    cnt <- cnt + 1
  }
#   gsub('(^\\W+|\\W+$)', '', res)
  res
}

stringPlace <- function(needle, haystack, FUN, ...) {
  do.call(rbind, lapply(seq_along(haystack), function(i) {
    m <- FUN(needle, haystack[i], ...)
    if(length(m)) {
      data.frame(place = i, value = m, stringsAsFactors = FALSE)
    }
  }))
}

checkFuzzy <- function(add, ver, max_dist = 2, returnMatches = FALSE) {
  orig_add <- add
  add <- gsub("^[^[:alnum:]]+|[^[:alnum:]]+$", "", add)
  df_add <- data.frame(orig_add, add, stringsAsFactors = FALSE)
  add <- unique(add[!(add %in% ver)])
  len <- cut(nchar(add), c(0, 3, 5, Inf), labels = FALSE)
  add1 <- add[len == 2]
  add2 <- add[len == 3]
  d1 <- utils::adist(ver, add1) == 1
  d2 <- utils::adist(ver, add2) <= max_dist
  m1 <- which(d1, arr.ind = TRUE)
  m2 <- which(d2, arr.ind = TRUE)
  r1 <- cbind(add1[m1[,2]], ver[m1[,1]])
  r2 <- cbind(add2[m2[,2]], ver[m2[,1]])
  a <- rbind(r1, r2)
  if(nrow(a) == 0) return(NULL)
  if(returnMatches) {
    a <- a[order(a[,1]),,drop = FALSE]
    colnames(a) <- c('suggestion', 'match')
    a
  } else {
    df_add[df_add[,2] %in% a[,1],]
  }
}
