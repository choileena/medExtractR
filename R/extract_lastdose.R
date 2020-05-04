#' Extract Last Dose Time From Phrase
#'
#' This function searches a phrase for the expression and postion
#' of the time at which the last dose of a drug was taken. It
#' is called within \code{\link{medExtractR}} and generally not intended for use outside
#' that function.
#'
#' @param phrase Text to search.
#' @param p_start Start position of phrase in larger text.
#' @param d_start Start position of drug name in larger text.
#' @param d_stop Start position of drug name in larger text.
#' @param time_exp Vector of regular expressions to identify time expressions.
#'
#' @details This function identifies the time at which the last dose of a drug of interest was taken.
#' The arguments \code{p_start}, \code{d_start}, and \code{d_stop} represent global start or stop
#' positions for the phrase or drug. These arguments are used to determine the position of any found
#' last dose time expressions relative to the overall clinical note, not just within \code{phrase}.
#'
#' The \code{time_exp} argument contains regular expressions for numeric or text representations of
#' last dose time. See \code{\link{time_regex}} for more information about the default regular
#' expressions used in \code{\link{medExtractR}}.
#'
#' @return data.frame with last dose time entity information. This output format is consistent with
#' the output of \code{\link{extract_entities}} \cr
#' Sample output for the phrase \dQuote{Last prograf at 5pm} would look like:\cr
#' \tabular{rr}{
#'  entity    \tab    expr\cr
#'  LastDose  \tab    5pm;17:20
#'  }
#' @export
#'
#' @examples
#' # Suppose this phrase begins at character 120 in the overall clinical note
#' extract_lastdose("took aspirin last night at 8pm", p_start = 120,
#'                   d_start = 125, d_stop = 131)

extract_lastdose <- function(phrase, p_start, d_start, d_stop,
                               time_exp = "default") {
  time_exp <- if(time_exp == "default"){
    e <- new.env()
    data("time_regex", package = 'medExtractR', envir = e)
    get("time_regex", envir = e)
  }else{time_exp}

  # Actual expression of time
  raw_time <- sapply(time_exp, function(t){
    tms <- unlist(str_extract_all(phrase, t))
    if(length(tms) == 0) tms <- "no match"

    # Only allow closest mention for each time expression type
    if(length(tms) > 1){
      time_starts <- sapply(tms, function(tm) regexec(tm, phrase, perl = TRUE)[[1]][1])
      tms <- tms[which.min(abs(time_starts - (d_start - p_start + 1)))]
    }

    return(tms)
  })

  # For all except duration, require a keyword to be present to indicate last dose
  raw_time_nodur <- raw_time[setdiff(names(raw_time), "duration")]
  raw_time_nodur <- sapply(raw_time_nodur, function(x){
    if(x != "no match"){
      is_last <- grepl("last|took|taken|previous", phrase, ignore.case=T)
      if(!is_last){x <- "no match"}else{x}
    }else{x}
  })

  raw_time <- c(raw_time_nodur, raw_time['duration'])

  # Only allow closest of "qualifier before" and "qualifier after
  if(raw_time['qualifier_after'] != "no match" &
     raw_time['qualifier_before'] != "no match"){
    vec <- raw_time[c('qualifier_after', 'qualifier_before')]
    time_starts <- sapply(vec,
                          function(tm) regexec(tm, phrase, perl = TRUE)[[1]][1])

    qual <- vec[which.min(abs(time_starts - (d_start - p_start + 1)))]

    if(names(qual) == "qualifier_after"){
      raw_time['qualifier_before'] <- "no match"
    }
    if(names(qual) == "qualifier_before"){
      raw_time['qualifier_after'] <- "no match"
    }
  }

  # Keep only non-missing times
  raw_time <- unlist(raw_time)
  found_time <- raw_time[raw_time != "no match"]

  # Remove some military time mentions that are most likely dates
  date_yrs <- c(paste0(197, 1:9), paste0(198, 1:9), paste0(199, 1:9),
                paste0(200, 1:9), paste0(201,0:8))
  found_time <- found_time[!(found_time %in% date_yrs)]

  # No matches at all
  if(length(found_time) == 0) {
    res <- data.frame("entity" = "LastDose", "expr" = NA)
  } else {
    # Position of time
    time_pos <- do.call(rbind, lapply(found_time, function(time_match){
      # Can have leading 0, but nothing else
      # Prevents algorithm from grabbing subset of time exp that is also valid time exp
      # e.g. grabbing 1:30 pm from 11:30 pm
      r <- regexec(paste0("(?<![1-9])", time_match), phrase, perl = TRUE)[[1]]

      c("start_pos" = r[1], "stop_pos" = r[1] + attributes(r)$match.length)
    }))
    time_pos <- as.data.frame(time_pos)
    time_pos$ft <- found_time

    # Only keep the closest time
    time_pos$dist <- abs(ifelse(time_pos$start_pos < (d_start-p_start),  # test if time expr starts before drug
                                time_pos$start_pos - (d_start-p_start), # time is before drug
                                time_pos$start_pos - (d_stop-p_start))) # time after drug
    tmps <- time_pos[which.min(time_pos$dist),]

    bp <- tmps$start_pos+p_start-1
    ep <- tmps$stop_pos+p_start-1

    res <- data.frame("entity" = "LastDose",
                      "expr" = paste(tmps$ft, paste(bp, ep, sep=":"), sep=";"))
  }
  return(res)
}
