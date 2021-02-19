#' Extract Medication Entities From Clinical Note - Extension for Tapering applications
#'
#' This function identifies medication entities of interest and returns found
#' expressions with start and stop positions.
#'
#' @param note Text to search.
#' @param drug_names Vector of drug names to locate.
#' @param window_length Length (in number of characters) of window after drug
#' in which to look.
#' @param max_dist Numeric - edit distance to use when searching for \code{drug_names}.
#' @param drug_list Vector of known drugs that may end search window. By default calls
#' \code{\link{rxnorm_druglist}}.
#' @param lastdose Logical - whether or not last dose time entity should be
#' extracted.
#' @param lastdose_window_ext Numeric - multiplicative factor by which
#' \code{window_length} should be extended when identifying last dose time.
#' @param unit Strength unit to look for (e.g., \sQuote{mg}).
#' @param strength_sep Delimiter for contiguous medication strengths (e.g., \sQuote{-} for \dQuote{LTG 200-300}).
#' @param flag_window How far around drug (in number of characters) to look for
#' dose change keyword - default fixed to 30.
#' @param \dots Parameter settings used in extracting frequency and intake time. Potentially useful
#' parameters include \code{freq_dict} and \code{intaketime_dict} (see \code{\dots} argument in
#' \code{\link{extract_entities_tapering}}) to specify frequency or intake time dictionaries, as well as
#' \sQuote{freq_fun} and \sQuote{intaketime_fun} for user-specified extraction functions. See
#' \code{\link{extract_entities_tapering}} documentation for details.
#'
#' @details This function uses a combination of regular expressions, rule-based
#' approaches, and dictionaries to identify various drug entities of interest.
#' Specific medications to be found are specified with \code{drug_names}, which
#' is not case-sensitive or space-sensitive (e.g., \sQuote{lamotrigine XR} is treated
#' the same as \sQuote{lamotrigineXR}). Entities to be extracted include drug name, strength,
#' dose amount, dose strength, frequency, intake time, and time of last dose. See
#' \code{\link{extract_entities_tapering}} and \code{\link{extract_lastdose}} for more details.
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
#' in the evening). The argument \code{strength_sep = '-'} identifies
#' the full expression \emph{300-200} as \emph{dose strength} in this phrase.
#'
#' By default, the \code{drug_list} argument is \dQuote{rxnorm} which calls \code{data(rxnorm_druglist)}.
#' A custom drug list in the form of a character string can be supplied instead, or can be appended
#' to \code{rxnorm_druglist} by specifying \code{drug_list = c("rxnorm", custom_drug_list)}. This uses
#' publicly available data courtesy of the U.S. National Library of Medicine (NLM), National
#' Institutes of Health, Department of Health and Human Services; NLM is not responsible for the product and
#' does not endorse or recommend this or any other product. See \code{rxnorm_druglist} documentation for details.
#'
#'
#' @return data.frame with entity information\cr
#' Sample output:\cr
#' \tabular{rrr}{
#' entity    \tab  expr   \tab    pos\cr
#' DoseChange\tab  decrease \tab  66:74\cr
#' DrugName   \tab Prograf \tab   78:85\cr
#' Strength  \tab  2 mg   \tab    86:90\cr
#' DoseAmt   \tab  1     \tab     91:92\cr
#' Frequency \tab  bid    \tab    101:104\cr
#' LastDose  \tab  2100    \tab   121:125\cr
#' }
#'
#' @export
#'
#' @references
#' Nelson SJ, Zeng K, Kilbourne J, Powell T, Moore R. Normalized names for clinical drugs: RxNorm at 6 years.
#' J Am Med Inform Assoc. 2011 Jul-Aug;18(4)441-8. doi: 10.1136/amiajnl-2011-000116. Epub 2011 Apr 21.
#' PubMed PMID: 21515544; PubMed Central PMCID: PMC3128404.
#'
#' @examples
#' \donttest{
#'
#' }


## Currently, sections related to window length are commented out - will be updated once I start working on window length improvements
medExtractR_tapering <- function(note, drug_names,
                                 #window_length = NULL, ## !!
                                 unit, max_dist = 0,
                                 drug_list = "rxnorm", lastdose = FALSE, lastdose_window_ext = 1.5,
                                 strength_sep = NULL,
                                 ...) {
  ## !!
  #wndw <- note

  def.saf <- getOption('stringsAsFactors')
  on.exit(options(stringsAsFactors = def.saf))
  options(stringsAsFactors = FALSE)


  # Find all mentions of any of the drug names listed above
  # aregexec performs approximate matching - allows for `max_dist` discrepancies
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
      if((nchar(drug) <= 5)){
        # Exact match with boundaries on either side for these because they are so short
        is_match <- regexec(paste0("\\b", drug, "\\b"), current_string,
                            ignore.case = TRUE, fixed = FALSE)
      }else{
        is_match <- utils::aregexec(paste0(drug, "\\b"), current_string,
                                    max.distance = max_dist,
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

        # include misspelling in between current and previous, if it exists
        if(nchar(drug)>3){
          im <- utils::aregexec(paste0(drug, "\\b"),
                                substr(current_string, 1, current_pos),
                                max.distance = ifelse(nchar(drug) <=
                                                        5, 1, max_dist), ignore.case = TRUE, fixed = FALSE)
          if(im > 0){
            l <- attributes(im[[1]])$match.length

            match_info <- rbind(match_info, c(substr(current_string,
                                                     start = im[[1]][1],
                                                     stop = im[[1]][1] + l - 1),
                                              im[[1]][1] + last_pos - 1, l))
          }
        }

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

  # Quit now if no drug name matches are found
  if(is.null(match_info)){return(data.frame("entity" = NA,"expr"=NA,"pos"=NA))}

  colnames(match_info) <- c("drug", "start_pos", "length")

  # Make columns with position and length numeric
  match_info <- data.frame("drug" = as.character(match_info[,'drug']),
                           "start_pos" = as.numeric(match_info[,'start_pos']),
                           "length" = as.numeric(match_info[,'length']))

  nr <- nrow(match_info)
  # Quit now if no matches are found
  if(nr == 0){return(data.frame("entity" = NA,"expr"=NA,"pos"=NA))}

  # If some drug names are subsets of another, this prevents entities being assigned to both
  # longer *and* shorter when longer is present (and thus correct)
  mi <- match_info[order(match_info[,'start_pos']),]
  lmi <- split(mi, mi[,'start_pos'])
  match_info <- do.call(rbind, lapply(lmi, function(dd) {
    ddl <- dd[,'length']
    dd[ddl == max(ddl),,drop = FALSE]
  }))

  nr <- nrow(match_info)

  note_lc <- tolower(note)



  ## !! INITIAL WINDOW LENGTH
  wndw <- sapply(seq_len(nr), function(i){
    window_string <- substr(note,
                            start = if(i == 1){1}else{match_info[i-1,'start_pos'] + match_info[i-1,'length'] + 1},  # start after prev drug name
                            stop = if(i == nr){nchar(note)}else{match_info[i+1,'start_pos']}) # stop before next drug name
  })

  wndw_sp <- sapply(seq_len(nr), function(i){
    if(i == 1){
      1
    }else{
      match_info[i-1,'start_pos'] + match_info[i-1,'length'] + 1
    }
  })


  # Window around drug to look for entities
  drug_window <- data.frame(
    drug = match_info[,'drug'],
    drug_start = as.numeric(match_info[,'start_pos']),
    # ends immediately after drug
    drug_stop = as.numeric(match_info[,'start_pos']) + as.numeric(match_info[,'length']) - 1,
    wndw_start = wndw_sp,
    window = wndw
  )
  drug_window <- drug_window[order(drug_window$drug_start),]


  ## CHANGE TO RESTRICT BEFORE/AFTER WINDOW
  # exclude drug_names from drug_list
  if("rxnorm" %in% drug_list) {
    # default - using rxnorm drug list
    e <- new.env()
    data("rxnorm_druglist", package = 'medExtractR', envir = e)
    dl <- get("rxnorm_druglist", envir = e)

    # add additional terms if specified
    if(length(drug_list)>1){
      dl <- c(dl, setdiff(drug_list, "rxnorm"))
    }
  }else{
    # non-rxnorm drug list provided
    dl <- drug_list
  }
  # make sure drug names of interest are not in list
  rm_index <- unique(unlist(sapply(tolower(drug_names), function(x){
    grep(x, tolower(dl))
  })))
  dl_lc <- tolower(dl[-rm_index])

  dl_wb <- paste0("\\b", dl_lc, "\\b")
  wndw_lc <- tolower(drug_window[,'window'])
  other_drug_list <- lapply(dl_wb, regexec, text=wndw_lc)

  # Cut window short if another drug name appears
  drug_wndw_update <- do.call(rbind, lapply(seq_along(wndw_lc), function(ix) {
    wndw <- drug_window[ix,'window']
    other_drugs <- lapply(other_drug_list, `[[`, ix)
    other_drugs <- other_drugs[other_drugs != -1]

    if(length(other_drugs) == 0) {
      #If no other drugs found, return original window
      return(data.frame("wndw_start" = drug_window[ix,'wndw_start'],
                        "window" = wndw))
    } else {
      # Only keep the ones closest to the anchor drug
      drg_st <- drug_window[ix,'drug_start']
      drg_diff <- drg_st - unlist(other_drugs)
      before <- ifelse(drg_diff > 0, drg_diff, NA)
      after <- ifelse(drg_diff < 0, abs(drg_diff), NA)


      # Extract other drug name as written in note
      new_wndw <- wndw
      new_wndw_start <- drug_window[ix,'wndw_start']

      if(sum(!is.na(before)) > 0){
        drg_before<- other_drugs[which.min(before)][[1]]

        ds_before_expr <- substr(wndw, drg_before, drg_before[1]+attr(drg_before, "match.length")-1)

        # Shorten window to exclude other drugs
        # Use tryCatch in case the extracted expression contains invalid regex
        try_sub <- tryCatch(sub(paste0(".+", ds_before_expr), "", wndw), error=function(e) e)
        new_wndw <- if(class(try_sub)[1] == "simpleError"){new_wndw} else {
          sub(paste0(".+", ds_before_expr), "", new_wndw)
        }
        new_wndw_start <- if(class(try_sub)[1] == "simpleError"){new_wndw_start} else {
          drug_window[ix,'wndw_start'] + drg_before[1] + attr(drg_before, "match.length")-1
        }
      }

      if(sum(!is.na(after)) > 0){
        drg_after <- other_drugs[which.min(after)][[1]]

        ds_after_expr <- substr(wndw, drg_after, drg_after[1]+attr(drg_after, "match.length")-1)

        # Shorten window to exclude other drugs
        # Use tryCatch in case the extracted expression contains invalid regex
        try_sub <- tryCatch(sub(paste0(ds_after_expr, ".+"), "", wndw), error=function(e) e)
        new_wndw <- if(class(try_sub)[1] == "simpleError"){new_wndw} else {
          sub(paste0(ds_after_expr, ".+"), "", new_wndw)
        }
      }


      return(data.frame("wndw_start" = new_wndw_start,
                        "window" = new_wndw))
    }
  }))

  drug_window$wndw_start <- drug_wndw_update$wndw_start
  drug_window$wndw <- drug_wndw_update$wndw


  # Check if rows are still getting duplicated
  drug_window <- unique(drug_window)




  # Extract dose entities
  res <- lapply(seq_along(drug_window$window), function(i) {
    rdf <- extract_entities_tapering(phrase = drug_window$window[i],
                                     p_start = drug_wndw_update$wndw[i], ## !! NEEDS TO CHANGE - ASSUMES PHRASE STARTS WITH THE DRUG NAME - just 1 temporarily until window length sorted out
                                     d_stop = drug_window$drug_stop[i], unit = unit,
                                     strength_sep = strength_sep, ...)

    rdf <- rdf[!is.na(rdf[,'expr']),]

    # Extract last dose time if desired
    if(lastdose){
      # Only include extension after window if the window was not cut short by another drug
      phrase <- if(nchar(drug_window$window[i]) - nchar(drug_window$drug[i])-1 == window_length){
        paste0(ld_wndw_ext[i,1], drug_window$window[i], ld_wndw_ext[i,2])
      }else{
        paste0(ld_wndw_ext[i,1], drug_window$window[i])
      }
      ld_rdf <- extract_lastdose(phrase,
                                 p_start = max(1,drug_window$drug_start[i]-window_length*(lastdose_window_ext/2)),
                                 d_start = drug_window$drug_start[i], d_stop = drug_window$drug_stop[i])

      if(!all(is.na(ld_rdf$expr))){
        rdf <- if(nrow(rdf)==0){
          ld_rdf
        }else{
          rbind.data.frame(rdf, ld_rdf)
        }
      }
    }

    if(nrow(rdf)==0){return(NA)}

    rdf[nrow(rdf)+1,] <- c("DrugName", paste(drug_window$drug[i],
                                             paste(drug_window$drug_start[i],
                                                   drug_window$drug_start[i] + nchar(drug_window$drug[i]),
                                                   sep = ":"),
                                             sep = ";"))

    # If same drug name appears twice in the window, make sure repeated instances of drug name are extracted
    addl_drugname <- gregexpr(drug_window$drug[i],
                              substr(drug_window$window[i], start = drug_window$drug_stop[i], stop = nchar(drug_window$window[i])),
                              ignore.case = TRUE)
    if(sum(addl_drugname[[1]]) > 0){
      for(j in 1:length(addl_drugname[[1]])){
        bp <- drug_window$drug_stop[i] + addl_drugname[[1]][j]-1
        ep <- bp + attributes(addl_drugname[[1]])$match.length[j]
        rdf <- rbind(rdf, data.frame(entity = "DrugName",
                                     expr = paste(c(substr(drug_window$window[i], bp, ep-1),
                                                    paste(bp, ep, sep = ":")),
                                                  collapse = ";")))
      }
    }


    return(rdf[,c("entity", "expr")])
  })

  res <- res[!is.na(res)]

  if(length(res) == 0){return(data.frame("entity" = NA, "expr" = NA, "pos" = NA))}

  results <- do.call(rbind, res)

  # split expr into expr and entity position columns
  expr <- results[,'expr']
  results[,'expr'] <- sub(";\\d.+", "", expr)
  results[,'pos'] <- str_extract(expr, "(?<=;)\\d.+")
  sp <- as.numeric(sub(":.+", "", results[,'pos']))
  results <- unique(results[order(sp),])
  row.names(results) <- NULL

  # sometimes same expression gets extracted as strength and dose due to second drug name cutting off doseamt
  # check/correct for this here
  ix <- names(which(table(results$pos)>1))
  for(x in ix){
    rs <- results[results$pos==x,]
    if(all(sort(rs$entity)==c("DoseStrength", "Strength"))){
      results <- results[-which(results$entity=="DoseStrength" & results$pos==x),]
    }
  }

  # For preposition entity - only keep if it is adjacent to another extracted (non-preposition) entity
  if(any(results$entity == "Preposition")){
    res_temp <- results
    res_temp$bp <- as.numeric(gsub(":.+", "", res_temp$pos))
    res_temp$ep <- as.numeric(gsub(".+:", "", res_temp$pos))


    pred_keep <- c()
    for(i in 1:nrow(res_temp)){
      if(res_temp$entity[i] == "Preposition"){
        if(i == 1){
          pred_keep <- c(pred_keep, ((res_temp$bp[i+1] - res_temp$ep[i]) == 1))
        }else if(i == nrow(res_temp)){
          pred_keep <- c(pred_keep, ((res_temp$bp[i] - res_temp$ep[i-1]) == 1))
        }else{
          pred_keep <- c(pred_keep, ((res_temp$bp[i] - res_temp$ep[i-1]) == 1 |
                                       (res_temp$bp[i+1] - res_temp$ep[i]) == 1))
        }
      }else{
        pred_keep <- c(pred_keep, TRUE)
      }
    }
    results <- subset(results, pred_keep)

  }


  ## Transition entity - Initial attempt, require duration to occur before transition (not necessarily immediately before)
  if(any(results$entity == "Transition")){
    res_temp <- results
    res_temp$is_trans <- ifelse(res_temp$entity=="Transition", 1, 0)

    # Break up results into transition subgroups
    res_temp$trans_grp <- cumsum(res_temp$is_trans)
    res_temp$trans_grp[which(res_temp$is_trans==1)] <- res_temp$trans_grp[which(res_temp$is_trans==1)] - 1

    # If Duration not present in group, don't keep as transition

    trans_keep <- c()
    for(i in 1:nrow(res_temp)){
      if(res_temp$entity[i] == "Transition"){
        keep_trans_i <- any(res_temp$entity=="Duration" & res_temp$trans_grp==res_temp$trans_grp[i])
        trans_keep <- c(trans_keep, keep_trans_i)
      }else{
        trans_keep <- c(trans_keep, TRUE)
      }
    }
    results <- subset(results, trans_keep)
  }



  return(results)
}
