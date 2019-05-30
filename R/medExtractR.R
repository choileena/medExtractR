#' Extract Medication Entities From Clinical Note
#'
#' This function extracts dose attributes.
#'
#' @param note Text to search.
#' @param drug_names Vector of drug names to locate.
#' @param drug_list Vector of known drugs that may end expression window.
#' @param window_length Length (number of characters) of window after drug
#' in which to look.
#' @param max_dist Numeric - edit distance to use when searching for drug_names.
#' @param unit Strength unit to look for (e.g. "mg").
#' @param strength_sep Delimiter for contiguous medication strengths.
#' @param flag_window How far around drug (number of characters) to look for
#' dosechange keyword - default fixed to 30.
#' @param doseschange_dict List of keywords used to determine if a dose change occurs.
#' @param \dots Parameter settings that will be used in extracting
#' frequency and intake time. Potentially useful parameters include \sQuote{freq_dict}
#' and \sQuote{intaketime_dict} to specify frequency or intake time keywords, as well
#' as \sQuote{freq_fun} and \sQuote{intaketime_fun} for user-specifed extraction functions.
#'
#' @return data.frame with entity information
#' @export

medExtractR <- function(note,
                          drug_names,
                          drug_list,
                          window_length,
                          max_dist,
                          unit,
                          strength_sep = NULL,
                          flag_window = 30,
                          doseschange_dict = NULL, ...) {
  def.saf <- getOption('stringsAsFactors')
  on.exit(options(stringsAsFactors = def.saf))
  options(stringsAsFactors = FALSE)
  if(is.null(doseschange_dict)) {
    e <- new.env()
    data("doseschange_vals", envir = e)
    doseschange_dict <- get("doseschange_vals", envir = e)
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
      if(nchar(drug) <= 3){
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
  match_info <- as.data.frame(match_info %>% dplyr::arrange(.data$start_pos) %>%
                                dplyr::group_by(.data$start_pos) %>%
                                dplyr::filter(.data$length==max(.data$length)) %>%
                                dplyr::ungroup())
  nr <- nrow(match_info)

  # Mark mentions that indicate a regimen change
  match_info[, 'DoseChange'] <- NA
  match_info[, 'dc_pos'] <- NA

  for(i in seq(nr)) {
    sp <- as.numeric(match_info[i, 'start_pos'])
    rc_window <- tolower(substr(note, start = sp - flag_window, stop = sp + flag_window))

    rc <- sapply(doseschange_dict, function(fw){
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
  wndw <- sapply(seq(nr), function(i){
    start_pos <- match_info[i,'start_pos']
    len <- match_info[i,'length']

    window_string <- substr(note, start = start_pos, stop = start_pos + len + window_length)
  })


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
  # Cut window short if another drug name appears
  drug_window$window <- sapply(drug_window$window, function(wndw){
    # What other drug names occur in string?
    other_drugs <- sapply(dl_lc, function(dn){
      regexec(paste0("\\b", dn, "\\b"), tolower(wndw))
    })
    other_drugs <- other_drugs[other_drugs != -1]

    if(length(other_drugs)==0){
      #If no other drugs found, return original window
      # Shorten if crossing into new note
      new_wndw <- sub("PKPDnote.+", "", wndw)
      return(wndw)
    }else{
      # Only keep first one that appears
      other_drugs <- other_drugs[other_drugs==min(unlist(other_drugs))]

      # If multiple drugs match same earliset start position, pick the longer expression
      drug_stop <- other_drugs[which.max(sapply(other_drugs,
                                                function(od){attributes(od)$match.length}))]
      sp <- as.numeric(drug_stop)
      l <- as.numeric(attributes(drug_stop[[1]])$match.length)

      # Extract other drug name as written in note
      ds_expr <- substr(wndw, sp, sp+l-1)
      # Shorten window to exclude other drugs
      # Use tryCatch in case the extracted expression contains invalid regex
      try_sub <- tryCatch(sub(paste0(ds_expr, ".+"), "", wndw), error=function(e) e)
      new_wndw <- if(class(try_sub)[1] == "simpleError"){wndw}else{
        sub(paste0(ds_expr, ".+"), "", wndw)
      }
      # Shorten if crossing into new note
      new_wndw <- sub("PKPDnote.+", "", new_wndw)

      return(new_wndw)
    }
  }, USE.NAMES = F)

  # Extract dose attributes
  res <- lapply(seq_along(drug_window$window), function(i){
    #print(i)
#     rdf <- extract_entities(phrase = drug_window$window[i], p_start = drug_window$drug_start[i],
#                             p_stop = drug_window$drug_stop[i], unit = unit,
#                             freq_dict = freq_dict, intaketime_dict = intaketime_dict, strength_sep = strength_sep) %>%
#       dplyr::filter(!is.na(expr))
    rdf <- extract_entities(phrase = drug_window$window[i], p_start = drug_window$drug_start[i],
                            p_stop = drug_window$drug_stop[i], unit = unit,
                            strength_sep = strength_sep, ...) %>%
      dplyr::filter(!is.na(.data$expr))

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

    # add drug name as an entity? do it here or in extract_entities?
    inr <- nrow(rdf)
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
  results %<>% dplyr::mutate(entity_expr = sub(";.+", "", .data$expr),
                             pos = str_extract(.data$expr, "(?<=;).+"),
                             sp = as.numeric(sub(":.+", "", .data$pos))) %>%
    dplyr::arrange(.data$sp) %>% dplyr::select(-c(.data$expr, .data$sp)) %>% dplyr::rename(expr = .data$entity_expr) %>%
    dplyr::distinct()


  return(results)
}
