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
                                 drug_list = "rxnorm", lastdose = FALSE, #lastdose_window_ext = 1.5,
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
        if(grepl("[a-z0-9]", substr(drug, nchar(drug), nchar(drug)))){
          is_match <- utils::aregexec(paste0(drug, "\\b"), current_string,
                                      max.distance = max_dist,
                                      ignore.case = TRUE, fixed = FALSE)
        }else{
          is_match <- utils::aregexec(drug, current_string,
                                      max.distance = max_dist,
                                      ignore.case = TRUE, fixed = FALSE)
        }

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
        if(nchar(drug)>=5){
          im <- utils::aregexec(paste0(drug, "\\b"),
                                substr(current_string, 1, current_pos),
                                max.distance = max_dist, ignore.case = TRUE, fixed = FALSE)
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

  # If drug names overlap, keep the longer name
  if(nr > 1){
    x <- table(match_info$start_pos)
    if(any(x>1)){
      ix <- names(x)[which(x>1)]

      for(y in ix){
        rm_row <- which(match_info$start_pos==y &
                          ((match_info$start_pos + match_info$length) <
                             max(subset(match_info, start_pos == y)$start_pos + subset(match_info, start_pos == y)$length)))
        match_info <- match_info[-rm_row,]
      }

    }
    x <- table(match_info$start_pos + match_info$length)
    if(any(x>1)){
      ix <- names(x)[which(x>1)]

      for(y in ix){
        rm_row <- which((match_info$start_pos + match_info$length)==y &
                          match_info$start_pos > min(subset(match_info, start_pos + length == y)$start_pos))
        match_info <- match_info[-rm_row,]
      }

    }
  }
  # Update
  nr <- nrow(match_info)


  note_lc <- tolower(note)

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
  } else {
    # non-rxnorm drug list provided
    dl <- drug_list
  }
  # make sure drug names of interest are not in list
  rm_index <- unique(unlist(sapply(tolower(drug_names), function(x){
    grep(x, tolower(dl))
  })))
  dl <- dl[-rm_index]
  dl_wb <- paste0("\\b", tolower(dl), "\\b")
  # escape some problematic characters
  dl_wb <- gsub("([+()])", "\\\\\\1", dl_wb)
  ## dl_wb - not updating, still has Mg in the list
  other_drugs <- stringi::stri_locate_all_regex(note_lc, dl_wb, omit_no_match = TRUE)
  other_drugs <- other_drugs[lengths(other_drugs) > 0]
  other_drugs_m <- do.call(rbind, other_drugs)
  other_drugs_m <- if(is.null(other_drugs_m)){
    other_drugs_m
  }else{
    if(nrow(other_drugs_m)>1){other_drugs_m[order(other_drugs_m[,1]),]}else{other_drugs_m}
  }


  ## !! INITIAL WINDOW LENGTH
  wndw_sp <- c(1, match_info[-nr,'start_pos'] + match_info[-nr,'length'] + 1)
  wndw_ep <- c(match_info[-1,'start_pos'], nchar(note))
  #   orig_wndw <- mapply(substr, note, wndw_sp, wndw_ep, USE.NAMES = FALSE)

  # are other drugs within string window?
  # look before drug
  match_start <- vapply(seq(nr), function(i) {
    a <- wndw_sp[i]
    b <- match_info[i,'start_pos']
    od_start <- other_drugs_m[a < other_drugs_m[,1] & other_drugs_m[,1] < b,]
    if(!is.null(od_start)){
      if(nrow(matrix(od_start,ncol=2)) > 0) a <- max(od_start) + 1
    }
    a
  }, numeric(1))

  # look after drug
  match_stop <- vapply(seq(nr), function(i) {
    a <- match_info[i,'start_pos'] + match_info[i,'length'] - 1
    b <- wndw_ep[i]
    od_stop <- other_drugs_m[a < other_drugs_m[,1] & other_drugs_m[,1] < b,]
    if(!is.null(od_stop)){
      if(nrow(matrix(od_stop,ncol=2)) > 0) b <- min(od_stop) - 1
    }
    b
  }, numeric(1))

  wndw <- mapply(substr, note, match_start, match_stop, USE.NAMES = FALSE)

  # Window around drug to look for entities
  drug_window <- data.frame(
    drug = match_info[,'drug'],
    drug_start = as.numeric(match_info[,'start_pos']),
    # ends immediately after drug
    drug_stop = as.numeric(match_info[,'start_pos']) + as.numeric(match_info[,'length']) - 1,
    wndw_start = match_start,
    window = wndw
  )
  drug_window <- drug_window[order(drug_window$drug_start),]

  # Check if rows are still getting duplicated
  drug_window <- unique(drug_window)

  ######## TEMP - TO BE REMOVED ^^

  # Extract dose entities
  res <- lapply(seq_along(drug_window$window), function(i) {
    #print(i)
    rdf <- extract_entities_tapering(phrase = drug_window$window[i],
                                     p_start = drug_window$wndw_start[i],
                                     d_stop = drug_window$drug_stop[i], unit = unit,
                                     strength_sep = strength_sep,
                                     ...)

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


    ## !! Check for punctuation between drug name and preceding extracted entities. If yes, remove all preceding entities
    drg_diff <- drug_window$drug_start[i] - rdf$stop
    rix <- which.min(drg_diff[drg_diff > 0])
    if(length(rix)>0){
      # stop once first punctuation is hit
      break_punct <- FALSE
      while(!break_punct){
        if(rix < 1){break}

        ent_b4_dn <- rdf[rix,]
        is_punct <- grepl(",|\\.|-|;|\\n", substr(note, ent_b4_dn$stop-1, drug_window$drug_start[i]))
        if(is_punct){
          rdf <- rdf[(rix+1):nrow(rdf),]
          break_punct <- TRUE
        }else{
          rix <- rix-1
        }
      }




    }


    rdf[nrow(rdf)+1,] <- c("DrugName", paste(drug_window$drug[i],
                                             paste(drug_window$drug_start[i],
                                                   drug_window$drug_start[i] + nchar(drug_window$drug[i]),
                                                   sep = ":"),
                                             sep = ";"),
                           paste(drug_window$drug_start[i],
                                 drug_window$drug_start[i] + nchar(drug_window$drug[i]),
                                 sep = ":"),
                           drug_window$drug_start[i], drug_window$drug_start[i] + nchar(drug_window$drug[i]), NA)




    # If same drug name appears twice in the window, make sure repeated instances of drug name are extracted
    addl_drugname <- gregexpr(paste0(drug_window$drug[i], "\\b"),
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

    # Second check to make sure key entities are present
    if(!any(rdf$entity %in% c("DoseStrength", "Strength", "DoseAmt", "Duration"))){
      if(!any(tolower(gsub(";.+", "", rdf$expr)) %in% c("done", "off", "stop", "last", "completed",
                                                        "complete", "discontinue", "discontinuing",
                                                        "finished"))){
        return(NA)
      }else{
        return(rdf[,c("entity", "expr")])
      }
    }else{
      return(rdf[,c("entity", "expr")])
    }

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

  results <- results[!is.na(results$entity),]

  # For preposition entity - only keep if it is adjacent to another extracted (non-preposition) entity
  if(any(results$entity == "Preposition")){
    res_temp <- results
    res_temp$bp <- as.numeric(gsub(":.+", "", res_temp$pos))
    res_temp$ep <- as.numeric(gsub(".+:", "", res_temp$pos))


    pred_keep <- c()
    for(i in 1:nrow(res_temp)){
      if(res_temp$entity[i] == "Preposition"){
        if(i == 1){
          pred_keep <- c(pred_keep, ((res_temp$bp[i+1] - res_temp$ep[i]) %in% c(0,1)))
        }else if(i == nrow(res_temp)){
          pred_keep <- c(pred_keep, ((res_temp$bp[i] - res_temp$ep[i-1]) %in% c(0,1)))
        }else{
          pred_keep <- c(pred_keep, ((res_temp$bp[i] - res_temp$ep[i-1]) %in% c(0,1) |
                                       (res_temp$bp[i+1] - res_temp$ep[i]) %in% c(0,1)))
        }
      }else{
        pred_keep <- c(pred_keep, TRUE)
      }
    }
    results <- subset(results, pred_keep)

  }

  if(any(results$entity == "Transition")){
    res_temp <- results
    res_temp$bp <- as.numeric(gsub(":.+", "", res_temp$pos))
    res_temp$ep <- as.numeric(gsub(".+:", "", res_temp$pos))


    trans_keep <- c()
    for(i in 1:nrow(res_temp)){
      if(res_temp$entity[i] == "Transition" & tolower(res_temp$expr[i]) != "then"){
        if(i == 1){
          trans_keep <- c(trans_keep, ((res_temp$bp[i+1] - res_temp$ep[i]) %in% c(0,1)))
        }else if(i == nrow(res_temp)){
          trans_keep <- c(trans_keep, ((res_temp$bp[i] - res_temp$ep[i-1]) %in% c(0,1)))
        }else{
          trans_keep <- c(trans_keep, ((res_temp$bp[i] - res_temp$ep[i-1]) %in% c(0,1) |
                                         (res_temp$bp[i+1] - res_temp$ep[i]) %in% c(0,1)))
        }
      }else{
        trans_keep <- c(trans_keep, TRUE)
      }
    }
    results <- subset(results, trans_keep)

  }


  # Transition entity - Initial attempt, require duration to occur before transition (not necessarily immediately before)
  if(any(results$entity == "Transition")){
    res_temp <- results
    res_temp$is_trans <- ifelse(res_temp$entity=="Transition", 1, 0)

    # Break up results into transition subgroups
    res_temp$trans_grp <- cumsum(res_temp$is_trans)
    res_temp$trans_grp[which(res_temp$is_trans==1)] <- res_temp$trans_grp[which(res_temp$is_trans==1)] - 1

    # If Duration not present in group before, don't keep as transition
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

  # failsafe to stop while loop if needed - don't end on a transition, preposition, or timekeyword
  x<-0
  while(results$entity[nrow(results)] %in% c("Transition", "Preposition", "TimeKeyword")){
    results <- results[-nrow(results),]
    x<-x+1
    if(x>20){break}
  }

  return(results)
}
