#' Extract Medication Entities From Clinical Note
#'
#' This function extracts dose entities and returns found expressions with start
#' and stop positions.
#'
#' @param note Text to search.
#' @param drug_names Vector of drug names to locate.
#' @param drug_list Vector of known drugs that may end expression window.
#' @param window_length Length (number of characters) of window after drug
#' in which to look.
#' @param max_dist Numeric - edit distance to use when searching for drug_names.
#' @param lastdose Logical - whether or not last dose time entity should be
#' extracted
#' @param lastdose_window_ext Numeric - multiplicative factor by which
#' window_length should be extended when identifying last dose time
#' @param unit Strength unit to look for (e.g. \dQuote{mg}).
#' @param strength_sep Delimiter for contiguous medication strengths.
#' @param flag_window How far around drug (number of characters) to look for
#' dose change keyword - default fixed to 30.
#' @param dosechange_dict List of keywords used to determine if a dose change
#' occurs.
#' @param \dots Parameter settings that will be used in extracting
#' frequency and intake time. Potentially useful parameters include
#' \sQuote{freq_dict} and \sQuote{intaketime_dict} to specify frequency or
#' intake time keywords, as well as \sQuote{freq_fun} and
#' \sQuote{intaketime_fun} for user-specifed extraction functions.
#'
#' @details This function uses a combination of regular expressions, rule-based
#' approaches, and dictionaries to identify various drug entities of interest.
#' Specific medications to be found are specified with \code{drug_names}, which
#' is not case-sensitive. Entities to be extracted include drug name, strength,
#' dose amount, dose, frequency, intake time, and time of last dose. See
#' \code{\link{extract_entities}} for more details.
#' 
#' (**Once the RxNorm License issue is determined, I will add in a comment about
#' drug_list. Is there a default we can assign?)
#' 
#' When searching for medication names of interest, fuzzy matching may be used.
#' The \code{max_dist} argument determines the maximum edit distance allowed for
#' such matches. If using fuzzy matching, any drug name with less than 5
#' characters will only allow an edit distance of 1, regardless of the value of
#' \code{max_dist}.
#' 
#' Most medication entities are searched for in a window after the drug. The
#' dose change entity, or presence of a keyword to indicate a non-current drug
#' regimen, may occur before the drug name. The \code{flag_window} argument
#' adjusts the width of the pre-drug window.
#' 
#' The \code{stength_sep} argument is \code{NULL} by default, but can be used to
#' identify shorthand for morning and evening doses. For example, consider the
#' phrase \sQuote{Lamotrigine 300-200} (meaning 300 mg in the morning and 200 mg
#' in the evening). The argument \code{strength_sep = '-'} can identify both
#' \emph{300} and \emph{200} as \emph{dose} in this phrase.
#'
#' @return data.frame with entity information
#' @export
#'
#' @examples
#' note1 <- "Lamotrigine 25 mg tablet - 3 tablets oral twice daily"
#' note2 <- "Lamotrigine 25 mg tablet - 3 tablets oral twice daily
#' lamotrigine 50 mg cap before bedtime
#' follow up in three months
#' LTG 10 mg tablet - 2 tab daily"
#' note3 <- "Lamotrigine 300-200"
#' data(rxnorm_druglist)
#' medExtractR(note1, c("lamotrigine", "ltg"), rxnorm_druglist, 130, "mg", 1)
#' medExtractR(note2, c("lamotrigine", "ltg"), rxnorm_druglist, 130, "mg", 1)
#' medExtractR(note3, c("lamotrigine", "ltg"), rxnorm_druglist, 130, "mg", 1,
#'               strength_sep = '-')

medExtractR <- function(note,
                          drug_names,
                          drug_list,
                          window_length,
                          unit,
                          max_dist = 0,
                          lastdose = FALSE, # whether or not to extract time of last dose
                          lastdose_window_ext = 1.5, # multiplier of window_length - how much to extend when looking for last dose time
                          strength_sep = NULL,
                          flag_window = 30,
                          dosechange_dict = NULL, ...) {
  def.saf <- getOption('stringsAsFactors')
  on.exit(options(stringsAsFactors = def.saf))
  options(stringsAsFactors = FALSE)
  if(is.null(dosechange_dict)) {
    e <- new.env()
    data("dosechange_vals", envir = e)
    dosechange_dict <- get("dosechange_vals", envir = e)
  }

  # Find all mentions of any of the drug names listed above
  # aregexec performs approximate matching - allows for max.distance discrepancies
  match_info <- c()
  num_char <- nchar(note)
  for(drug in drug_names){
    # drug name should have no spaces (want results to be the same regardless
    #  of whether spaces are used in drug name or not)
    drug <- gsub(" ", "", drug)

    # Need to loop through note until all instances have been found
    last_pos <- 1

    while(last_pos < num_char){
      # Current portion of the note being examined
      current_string <- substr(note, start = last_pos, stop = num_char)
      # Check for a match to any drug name
      if((nchar(drug) <= 3)|(max_dist == 0)){
        # Exact match with boundaries on either side for these because they are so short
        is_match <- regexec(paste0("\\b", drug, "\\b"), current_string,
                            ignore.case = TRUE, fixed = FALSE)
      }else{
        is_match <- utils::aregexec(paste0(drug, "\\b"), current_string,
                                    max.distance = ifelse(nchar(drug) <= 5, 1, max_dist),
                                    ignore.case = TRUE, fixed = FALSE) 

      }


      # if there is a match, extract the matching drug name (exactly as is, even if misspelled)
      if(is_match > 0){
        current_pos <- is_match[[1]][1]
        len <- attributes(is_match[[1]])$match.length
        drug_match <- substr(current_string, start = current_pos, stop = current_pos+len-1)

        # need to make sure position is relative to full note
        pos <- is_match[[1]][1] + last_pos - 1
        match_info <- rbind(match_info, c(drug_match, pos, len))
      }

      if(!exists("pos")){pos <- 0}

      if(last_pos >= pos){
        # force while loop to quit once the end of the note has been reached
        last_pos <- num_char + 1
      }else if(last_pos < pos){
        # start looking after the last mention
        last_pos <- pos + len
      }
    }
  }

  # Quit now if no matches are found
  if(is.null(match_info)){return("No matches found for drug regimen")}

  colnames(match_info) <- c("drug", "start_pos", "length")

  # Make columns with position and length numeric
  match_info <- data.frame("drug" = as.character(match_info[,'drug']),
                           "start_pos" = as.numeric(match_info[,'start_pos']),
                           "length" = as.numeric(match_info[,'length']))

  nr <- nrow(match_info)
  # Quit now if no matches are found
  if(nr == 0){return("No matches found for drug regimen")}

  # If some drug names are subsets of another, this prevents entities being assigned to both longer *and* shorter
  # when longer is present (and thus correct)
  mi <- match_info[order(match_info[,'start_pos']),]
  lmi <- split(mi, mi[,'start_pos'])
  match_info <- do.call(rbind, lapply(lmi, function(dd) {
      ddl <- dd[,'length']
      dd[ddl == max(ddl),,drop = FALSE]
  }))

  nr <- nrow(match_info)

  # Mark mentions that indicate a regimen change
  match_info[, 'DoseChange'] <- NA
  match_info[, 'dc_pos'] <- NA

  note_lc <- tolower(note)
  for(i in seq_len(nr)) {
    sp <- as.numeric(match_info[i, 'start_pos'])
    rc_window <- substr(note_lc, start = sp - flag_window, stop = sp + flag_window)

    rc <- sapply(dosechange_dict, function(fw){
      flag <- regexpr(pattern = paste0(fw, "\\b"), text = rc_window)
      flag
    })

    if(!all(rc == -1)){
      # Pick closest word to drug
      flag_dist <- abs(rc - flag_window)
      min_flag <- which.min(flag_dist)
      flag_wd <- names(rc)[min_flag]
      rc_sp = sp - flag_window + rc[min_flag] - 1

      match_info[i, 'DoseChange'] <- substr(note, rc_sp, rc_sp + nchar(flag_wd)-1)
      match_info[i, 'dc_pos'] <- paste(rc_sp, rc_sp + nchar(flag_wd), sep = ":")
    }
  }

  # String to extract based on number of characters
  # Will search within this string to get drug regimen
  # Look for first occurrence of different phrases after drug name?
  # up to certain point (in case it's not actually a dose attr mention)
  wndw <- sapply(seq_len(nr), function(i){
    start_pos <- match_info[i,'start_pos']
    len <- match_info[i,'length']

    window_string <- substr(note, start = start_pos, stop = start_pos + len + window_length)
  })
  
  # Add in extended window if last dose time is desired
  if(lastdose){
    ld_wndw_ext <- do.call(rbind, lapply(seq(nr), function(i){
      start_pos <- match_info[i,'start_pos']
      len <- match_info[i,'length']
      
      #lastdose needs more of a buffer
      c(substr(note, start = start_pos-window_length*(lastdose_window_ext/2), stop = start_pos-1),
        substr(note, start = start_pos + len + window_length+1, start_pos + len + window_length*lastdose_window_ext))
    })) 
  }

  # Window around drug to look for entities
  drug_window <- data.frame(
    drug = match_info[,'drug'],
    drug_start = as.numeric(match_info[,'start_pos']),
    # ends immediately after drug
    drug_stop = as.numeric(match_info[,'start_pos']) + as.numeric(match_info[,'length']) - 1,
    window = wndw,
    dosechange = match_info[,'DoseChange'],
    dosechange_pos = match_info[,'dc_pos']
  )
  drug_window <- drug_window[order(drug_window$drug_start),]

  # Cut string short if *exact* same drug name is used later in the string
  drug_window$window <- sapply(1:nrow(drug_window), function(i){
    drg <- drug_window$drug[i]
    drg_wndw <- drug_window$window[i]

    after_drg <- substr(drg_wndw, nchar(drg)+1, nchar(wndw))
    if(grepl(drg, after_drg, ignore.case = T)){
      return(paste0(drg, sub(paste0(drg, ".+"), "", after_drg)))
    }else{return(drg_wndw)}
  })

  # Want to keep alternative drug names in same window, even if strength/dosesc appears between them
  drug_window$keep_drug <- sapply(1:nrow(drug_window), function(i){
    if(i==1){return(FALSE)}else{
      drug_window$drug_start[i] < drug_window$drug_stop[i-1] + window_length
    }
  })

  # exclude drug_names from drug_list
  dl_lc <- setdiff(tolower(drug_list), tolower(drug_names))
  dl_wb <- paste0("\\b", dl_lc, "\\b")
  wndw_lc <- tolower(drug_window[,'window'])
  other_drug_list <- lapply(dl_wb, regexec, text = wndw_lc)

  # Cut window short if another drug name appears
  drug_window$window <- sapply(seq_along(wndw_lc), function(ix) {
    wndw <- drug_window[ix,'window']
    other_drugs <- lapply(other_drug_list, `[[`, ix)
    # What other drug names occur in string?
    other_drugs <- other_drugs[other_drugs != -1]

    if(length(other_drugs) == 0) {
      #If no other drugs found, return original window
      # Shorten if crossing into new note
      new_wndw <- sub("PKPDnote.+", "", wndw)
      return(wndw)
    } else {
      # Only keep first one that appears
      other_drugs <- other_drugs[other_drugs==min(unlist(other_drugs))]

      # If multiple drugs match same earliest start position, pick the longer expression
      ml <- vapply(other_drugs, attr, numeric(1), 'match.length')
      drug_stop <- which.max(ml)
      sp <- as.numeric(other_drugs[drug_stop])
      l <- as.numeric(ml[drug_stop])

      # Extract other drug name as written in note
      ds_expr <- substr(wndw, sp, sp+l-1)
      # Shorten window to exclude other drugs
      # Use tryCatch in case the extracted expression contains invalid regex
      try_sub <- tryCatch(sub(paste0(ds_expr, ".+"), "", wndw), error=function(e) e)
      new_wndw <- if(class(try_sub)[1] == "simpleError"){wndw} else {
        sub(paste0(ds_expr, ".+"), "", wndw)
      }
      # Shorten if crossing into new note
      new_wndw <- sub("PKPDnote.+", "", new_wndw)

      return(new_wndw)
    }
  }, USE.NAMES = F)

  # Calibrate window if last dose time is desired
  if(lastdose){
    drug_window$ld_offset <-  sapply(1:nrow(drug_window), function(i){
      window_length - nchar(drug_window$window[i]) + 
        (drug_window$drug_stop[i] - drug_window$drug_start[i])
    })
    
    drug_window$phrase_stop <-  sapply(1:nrow(drug_window), function(i){
      nchar(drug_window$window[i]) + drug_window$drug_start[i]
    })
  }
  
  # Extract dose entities
  res <- lapply(seq_along(drug_window$window), function(i) {
    rdf <- extract_entities(phrase = drug_window$window[i], p_start = drug_window$drug_start[i],
                            p_stop = drug_window$drug_stop[i], unit = unit,
                            strength_sep = strength_sep, ...)
    rdf <- rdf[!is.na(rdf[,'expr']),]
    
    # Extract last dose time if desired
    if(lastdose){
      ld_rdf <- extract_lastdose(phrase = paste0(ld_wndw_ext[i,1], drug_window$window[i], ld_wndw_ext[i,2]),
                                 p_start = drug_window$drug_start[i]-window_length*(lastdose_window_ext/2), 
                                 p_stop = drug_window$phrase_stop[i], p_offset = drug_window$ld_offset[i],
                                 d_start = drug_window$drug_start[i], d_stop = drug_window$drug_stop[i])
      
      if(!all(is.na(ld_rdf$expr))){
        rdf <- if(nrow(rdf)==0){
          ld_rdf
        }else{
          rbind.data.frame(rdf, ld_rdf)
        }
      }
    }

    ## can this even happen if NA is filtered out?

    if(all(is.na(rdf$expr))){
      if(drug_window$keep_drug[i]){
        data.frame("entity" = 'DrugName',
                   "expr" = paste(drug_window$drug[i],
                                  paste(drug_window$drug_start[i],
                                        drug_window$drug_start[i] + nchar(drug_window$drug[i]),
                                        sep = ":"),
                                  sep = ";"))
      }
      return(NA)
    }

    rdf[nrow(rdf)+1,] <- c("DrugName", paste(drug_window$drug[i],
                                             paste(drug_window$drug_start[i],
                                                   drug_window$drug_start[i] + nchar(drug_window$drug[i]),
                                                   sep = ":"),
                                             sep = ";"))
    # Add in DoseChange info if it exists
    if(!is.na(drug_window$dosechange[i])){
      # check if dosechange mention occurs AFTER all other entities
      # if so, ignore it (probably talking about next drug in the list)
      dc_start_pos <- as.numeric(gsub(":.+", "", drug_window$dosechange_pos[i]))
      ent_last_pos <- as.numeric(max(gsub(".+:", "", rdf$expr)))

      # Only add if dosechange occurs before end of last entity
      if(dc_start_pos < ent_last_pos){
        rdf[nrow(rdf)+1,] <- c("DoseChange", paste(drug_window$dosechange[i], drug_window$dosechange_pos[i], sep=";"))
      }
    }

    return(rdf[,c("entity", "expr")])
  })

  res <- res[!is.na(res)]

  if(length(res) == 0){return(NA)}

  results <- do.call(rbind, res)

  # split expr into expr and entity position columns
  expr <- results[,'expr']
  results[,'expr'] <- sub(";.+", "", expr)
  results[,'pos'] <- str_extract(expr, "(?<=;).+")
  sp <- as.numeric(sub(":.+", "", results[,'pos']))
  results <- unique(results[order(sp),])
  row.names(results) <- NULL

  return(results)
}
