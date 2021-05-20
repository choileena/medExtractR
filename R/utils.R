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

#' Preprocess Phrase for Date-times
#'
#' @keywords internal

internal_censor_dates <- function(phrase) {
  # Common issues: censor the expressions "24/7" and "24 hr"
  phrase <- gsub("24/7", "XX/X", phrase)
  phrase <- gsub("24(\\s?[hr|hour])", "XX\\1", phrase)

  # A bit of pre-processing: censor various date formats to prevent them being identified as drug information
  phrase <- gsub("\\d{2}[-/\\.]\\d{2}[-/\\.]\\d{4}", "##-##-####", phrase) # 11/11/1111
  phrase <- gsub("\\d{2}[-/\\.]\\d{1}[-/\\.]\\d{4}", "##-#-####", phrase) # 11/1/1111
  phrase <- gsub("\\d{1}[-/\\.]\\d{2}[-/\\.]\\d{4}", "#-##-####", phrase) # 1/11/1111
  phrase <- gsub("\\d{1}[-/\\.]\\d{1}[-/\\.]\\d{4}", "#-#-####", phrase) # 1/1/1111

  phrase <- gsub("\\d{2}[-/\\.]\\d{2}[-/\\.]\\d{2}", "##-##-##", phrase) # 11/11/11
  phrase <- gsub("\\d{2}[-/\\.]\\d{1}[-/\\.]\\d{2}", "##-#-##", phrase) # 11/1/11
  phrase <- gsub("\\d{1}[-/\\.]\\d{2}[-/\\.]\\d{2}", "#-##-##", phrase) # 1/11/11
  phrase <- gsub("\\d{1}[-/\\.]\\d{1}[-/\\.]\\d{2}", "#-#-##", phrase) # 1/1/11

  # check is some month/day formats are dates. If so, censor it
  possible_dates <- unique(unlist(stringr::str_extract_all(phrase, "\\b\\d{1,2}[-/]\\d{1,2}\\b")))
  if(length(possible_dates) > 0) {
    censor <- vapply(possible_dates, function(x) {
      y <- tryCatch(as.Date(paste0('2000', stringr::str_extract(x, "[-/]"), x)), error = function(e) e)
      class(y)[1] == 'Date'
    }, logical(1), USE.NAMES = FALSE)
    possible_dates <- possible_dates[censor]
    for(pd in possible_dates) {
      phrase <- gsub(pd, paste0(rep("X", nchar(pd)), collapse=''), phrase)
    }
  }

  # This could potentially be problematic (e.g., if a dose like 5/10 might be expected since it would be confused as a date)
  # censor partial dates (missing year)
  if(grepl("\\d{1,2}/\\d{1,2}", phrase)) {
    # could turn this into a helper function
    month <- '((0?(1(?!\\d)|[2-9]))|1[0-2])'
    day <- '((0?([1-3](?!\\d)|[4-9]))|1[0-9]|2[0-9]|3[0-1])'
    slash_date <- stringr::str_extract_all(phrase, paste(month, day, sep="/"))[[1]]
    dash_date <- stringr::str_extract_all(phrase, paste(month, day, sep="-"))[[1]]
    dates <- c(slash_date, dash_date)

    if(length(dates)>0) {
      for(i in 1:length(dates))
        phrase <- stringr::str_replace_all(phrase, dates[i], paste0(rep('X', nchar(dates[i])), collapse=""))
    }
  }

  time_present <- gregexpr("\\d[^a-z]+\\s?(?=((am)|(pm))\\b)", tolower(phrase), perl=TRUE)[[1]]
  if(any(time_present != -1)) {
    for(j in seq_along(time_present)) {
      tpi <- time_present[j]
      tpli <- attributes(time_present)$match.length[j]

#       s1 <- substr(phrase, 1, tpi-1)
#       s2 <- substr(phrase, tpi, tpi+tpli)
#       s3 <- substr(phrase, tpi+tpli+1, nchar(phrase))
#       replace_time <- gsub("\\d", "X", s2)
#       phrase <- paste0(s1, replace_time, s3)
      substr(phrase, tpi, tpi+tpli) <- gsub("\\d", "X", substr(phrase, tpi, tpi+tpli))
    }
  }
  phrase
}

#' Preprocess Phrase for Numbers
#'
#' @keywords internal

internal_find_numbers <- function(phrase, num_patt, num_then_ds, num_as_str) {
  # Numbers in phrase
  all_numbers <- unlist(stringr::str_extract_all(phrase, num_patt))
  num_positions <- gregexpr(num_patt, phrase, perl = TRUE)[[1]]

  # Don't allow 0 to be returned on its own
  iszero <- all_numbers == '0'
  num_positions <- num_positions[!iszero]
  all_numbers <- all_numbers[!iszero]

  if(length(all_numbers) > 0) {
    # ignore isolated "O2" for oxygen
    istwo <- all_numbers == '2'
    if(any(istwo)) {
      # check character before "2"
      p2 <- num_positions[istwo] - 1
      isoxy <- which(istwo)[grepl('o|O', substr(phrase, p2, p2))]
      keep_nums <- setdiff(seq_along(all_numbers), isoxy)
      num_positions <- num_positions[keep_nums]
      all_numbers <- all_numbers[keep_nums]
    }

    # ignore dosesequence numbers, percentages
    has_doseseq <- function(an, np) {
      grepl(paste0(an, num_then_ds), substr(phrase, np, np+nchar(an)+7), ignore.case = TRUE, perl=TRUE)
    }
    ds_id <- mapply(has_doseseq, all_numbers, num_positions)
    all_numbers <- all_numbers[!ds_id]
    num_positions <- num_positions[!ds_id]
  }

  # Ignore dates
  date_yrs <- as.character(seq(1970, 2018))
  # exclude all_numbers[1]
  is_year <- setdiff(which(all_numbers %in% date_yrs), 1)
  if(any(is_year)) {
    # If the preceeding numeric value is only separated by one space, remove it too (as it's probably a date)
    rm_year <- unlist(lapply(is_year, function(iy) {
      patt <- sprintf("(?<=%s)\\s(?=%s)", all_numbers[iy - 1], all_numbers[iy])
      if(grepl(patt, phrase, perl = TRUE)) {
        c(iy-1, iy)
      } else {
        iy
      }
    }))
    keep_nums <- setdiff(seq_along(all_numbers), rm_year)
    num_positions <- num_positions[keep_nums]
    all_numbers <- all_numbers[keep_nums]
  }

  # Only used for doseamt, unlikely that people would be taking too many pills at once
  lphrase <- tolower(phrase)
  text_numbers <- lapply(num_as_str, regexpr, text = lphrase)
  inum <- which(unlist(text_numbers) != -1)
  if(length(inum) > 0) {
    # which word expressions were found
    text_num <- num_as_str[inum]
    text_res <- lapply(text_num, function(n) { gregexpr(n, lphrase)[[1]] })
    # count matches within note
    n_matches <- vapply(text_res, length, numeric(1))
    tn_expr <- unlist(mapply(rep, text_num, n_matches, USE.NAMES = FALSE))
    tn_pos <- unlist(text_res)
  } else {
    tn_expr <- character(0)
    tn_pos <- numeric(0)
  }

  list(all_numbers = all_numbers, num_positions = num_positions, tn_expr = tn_expr, tn_pos = tn_pos)
}

#' Internal handler for generic entity extraction
#'
#' @keywords internal

extract_type <- function(phrase, type, fun = NULL, ...) {
  if(is.null(fun) || as.character(substitute(fun)) == "extract_generic") {
    addl <- list(...)
    expected_dictionary <- sprintf('%s_dict', type)
    dict <- addl[[expected_dictionary]]
    if(is.null(dict)) {
      def_vals <- sprintf('%s_vals', type)
      e <- new.env()
      data(list = def_vals, package = 'medExtractR', envir = e)
      dict <- e[[def_vals]]
    }
    df <- extract_generic(phrase, dict)
  } else {
    df <- fun(phrase, ...)
  }
  df
}
