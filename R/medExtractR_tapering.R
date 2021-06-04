#' Extract Medication Entities From Clinical Note - Extension of \code{\link{medExtractR}} for Tapering applications
#'
#' This function identifies medication entities of interest and returns found
#' expressions with start and stop positions.
#'
#' @param note Text to search.
#' @param drug_names Vector of drug names of interest to locate.
#' @param max_dist Numeric - edit distance to use when searching for \code{drug_names}.
#' @param drug_list Vector of known drugs that may end search window. By default calls
#' \code{\link{rxnorm_druglist}}. Can be supplemented with expressions in \code{\link{addl_expr}}.
#' @param lastdose Logical - whether or not last dose time entity should be extracted. See \sQuote{Details}
#' section below for more information.
#' @param unit Strength unit to look for (e.g., \sQuote{mg}).
#' @param strength_sep Delimiter for contiguous medication strengths (e.g., \sQuote{-} for \dQuote{LTG 200-300}).
#' @param \dots Parameter settings used in dictionary-based entities. For each dictionary-based
#' entity, the user can supply the optional arguments \code{<entity>_fun} and \code{<entity>_dict}
#' to provide custom extraction functions and dictionaries, respectively. If no additional arguments are provided,
#' \code{medExtractR_tapering} will use \code{\link{extract_generic}} and the default dictionary for each entity.
#' See \code{\link{extract_entities_tapering}} documentation for details.
#'
#' @details This function uses a combination of regular expressions, rule-based
#' approaches, and dictionaries to identify various drug entities of interest, with a
#' particular focus on drugs administered with a tapering schedule.
#' Specific medications to be found are specified with \code{drug_names}, which
#' is not case-sensitive or space-sensitive (e.g., \sQuote{lamotrigine XR} is treated
#' the same as \sQuote{lamotrigineXR}). Entities to be extracted include drug name, strength,
#' dose amount, dose strength, frequency, intake time, route, duration, dose schedule,
#' time keyword, preposition, transition, dispense amount, refill, and time of last dose.
#' While it is still an optional entity in \code{medExtractR_tapering}, if \code{lastdose=TRUE}
#' then \code{medExtractR_tapering} will search for time of last dose in the same search window used for all
#' other entities. As a result, there is no need for the \code{lastdose_window_ext} argument. See
#' \code{\link{extract_entities_tapering}} and \code{\link{extract_lastdose}} for more details.
#'
#' When searching for medication names of interest, fuzzy matching may be used.
#' The \code{max_dist} argument determines the maximum edit distance allowed for
#' such matches. If using fuzzy matching, any drug name with less than 7 characters
#' will force an exact match, regardless of the value of \code{max_dist}. The default value of 7 was
#' selected based on a set of training notes for the drug prednisone, and differs slightly from the default
#' values of 5 for \code{\link{medExtractR}}. The tapering extension does not use the \code{window_length} argument
#' to define the search window, since tapering schedules can be much longer than a static regimens.
#' Instead, \code{medExtractR_tapering} dynamically generates the search window based on competing drug names or
#' phrases, and the distance between consecutive entities. The \code{stength_sep} argument is \code{NULL} by
#' default, and operates in the same manner as it does in \code{medExtractR}.
#'
#' By default, the \code{drug_list} argument is \dQuote{rxnorm} which calls \code{data(rxnorm_druglist)}.
#' A custom drug list in the form of a character string can be supplied instead, or can be appended
#' to \code{rxnorm_druglist} by specifying \code{drug_list = c("rxnorm", custom_drug_list)}. This uses
#' publicly available data courtesy of the U.S. National Library of Medicine (NLM), National
#' Institutes of Health, Department of Health and Human Services; NLM is not responsible for the product and
#' does not endorse or recommend this or any other product. See \code{rxnorm_druglist} documentation for details.
#'
#'
#' @return data.frame with entity information. If no dosing
#' information for the drug of interest is found, the following output will be returned: \cr
#' \tabular{rrr}{
#' entity    \tab  expr   \tab    pos\cr
#' NA \tab  NA \tab  NA
#' }
#' The \dQuote{entity} column of the output contains the formatted label for that entity, according to
#' the following mapping.\cr
#' drug name: \dQuote{DrugName}\cr
#' strength: \dQuote{Strength}\cr
#' dose amount: \dQuote{DoseAmt}\cr
#' dose strength: \dQuote{DoseStrength}\cr
#' frequency: \dQuote{Frequency}\cr
#' intake time: \dQuote{IntakeTime}\cr
#' duration: \dQuote{Duration}\cr
#' route: \dQuote{Route}\cr
#' dose change: \dQuote{DoseChange}\cr
#' dose schedule: \dQuote{DoseScheule}\cr
#' time keyword: \dQuote{TimeKeyword}\cr
#' transition: \dQuote{Transition}\cr
#' preposition: \dQuote{Preposition}\cr
#' dispense amount: \dQuote{DispenseAmt}\cr
#' refill: \dQuote{Refill}\cr
#' time of last dose: \dQuote{LastDose}\cr
#' Sample output:\cr
#' \tabular{rrr}{
#' entity    \tab  expr   \tab    pos\cr
#' DoseChange\tab  decrease \tab  66:74\cr
#' DrugName   \tab Prograf \tab   78:85\cr
#' Strength  \tab  2 mg   \tab    86:90\cr
#' DoseAmt   \tab  1     \tab     91:92\cr
#' Frequency \tab  bid    \tab    101:104\cr
#' LastDose  \tab  2100    \tab   121:125
#' }
#'
#' @export
#'
#' @references
#' Nelson SJ, Zeng K, Kilbourne J, Powell T, Moore R. Normalized names for clinical drugs: RxNorm at 6 years.
#' J Am Med Inform Assoc. 2011 Jul-Aug;18(4)441-8. doi: 10.1136/amiajnl-2011-000116. Epub 2011 Apr 21.
#' PubMed PMID: 21515544; PubMed Central PMCID: PMC3128404.


medExtractR_tapering <- function(note, drug_names,
                                 unit, max_dist = 0,
                                 drug_list = "rxnorm", lastdose = FALSE,
                                 strength_sep = NULL, ...) {

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
      if((nchar(drug) <= 7)){
        # Exact match with boundaries on either side for these because they are short
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

      for(y in ix) {
        pos_at_y <- match_info[,'start_pos'] == y
        mi_end_pos <- match_info[,'start_pos'] + match_info[,'length']
        max_end_pos <- max(mi_end_pos[pos_at_y])
        rm_row <- which(pos_at_y & mi_end_pos < max_end_pos)
        match_info <- match_info[-rm_row,]
      }

    }
    x <- table(match_info$start_pos + match_info$length)
    if(any(x>1)){
      ix <- names(x)[which(x>1)]

      for(y in ix) {
        mi_end_pos <- match_info[,'start_pos'] + match_info[,'length']
        pos_at_y <- mi_end_pos == y
        min_start_pos <- min(match_info[pos_at_y,'start_pos'])
        rm_row <- which(pos_at_y & match_info[,'start_pos'] > min_start_pos)
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


  other_drugs <- stringi::stri_locate_all_regex(note_lc, dl_wb, omit_no_match = TRUE)
  other_drugs <- other_drugs[lengths(other_drugs) > 0]
  other_drugs_m <- do.call(rbind, other_drugs) # position of other drugs (possibly return for post-processing with other entities) - alternative to line 171 above (adjacent only from drug name forward to next drug name?)
  other_drugs_m <- if(is.null(other_drugs_m)){
    other_drugs_m
  }else{
    if(nrow(other_drugs_m)>1){other_drugs_m[order(other_drugs_m[,1]),]}else{other_drugs_m}
  }


  ## Initial window length
  wndw_sp <- c(1, match_info[-nr,'start_pos'] + match_info[-nr,'length'] + 1)
  wndw_ep <- c(match_info[-1,'start_pos'], nchar(note))

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
  drug_window <- unique(drug_window)

  # Extract dose entities
  res <- lapply(seq_along(drug_window$window), function(i) {
    rdf <- extract_entities_tapering(phrase = drug_window$window[i],
                                     p_start = drug_window$wndw_start[i],
                                     d_stop = drug_window$drug_stop[i], unit = unit,
                                     strength_sep = strength_sep, ...)
    rdf <- rdf[!is.na(rdf[,'expr']),]

    # Extract last dose time if desired
    if(lastdose) {
      # Only include extension after window if the window was not cut short by another drug
      orig_window_size <- wndw_sp[i] == match_start[i] && wndw_ep[i] == match_stop[i]
      phrase <- drug_window[i, 'window']
      # does it ever make sense to extend the window?
      # phrase <- substr(note, match_start[i], match_stop[i], USE.NAMES = FALSE)
      ld_rdf <- extract_lastdose(phrase,
                                 p_start = drug_window[i,'wndw_start'],
                                 d_start = drug_window[i,'drug_start'],
                                 d_stop = drug_window[i,'drug_stop']
      )

      if(!all(is.na(ld_rdf$expr))) {
        # need to add pos|start|stop|rn
        ld_pos <- gsub(".+;", "", ld_rdf[,'expr'])
        ld_start <- as.numeric(gsub(":.+", "", ld_pos))
        ld_stop <- as.numeric(gsub(".+:", "", ld_pos))
        ld_rn <- seq(nrow(ld_rdf))
        ld_rdf <- cbind(ld_rdf, pos = ld_pos, start = ld_start, stop = ld_stop, rn = ld_rn)

        if(nrow(rdf) == 0) {
          rdf <- ld_rdf
        } else {
          rdf <- rbind.data.frame(rdf, ld_rdf)
        }
      }
    }

    if(nrow(rdf)==0){return(NA)}

    ## Check for punctuation between drug name and preceding extracted entities. If yes, remove all preceding entities
    drg_diff <- drug_window$drug_start[i] - rdf$stop
    rix <- which.min(drg_diff[drg_diff > 0])
    if(length(rix) > 0) {
      # stop once first punctuation is hit
      break_punct <- FALSE
      # start by checking from the drug name
      # If there's a preposition right before the line break, shift to check before the preposition
      check_from <- drug_window$drug_start[i]

      while(!break_punct) {
        if(rix < 1){break}

        ent_b4_dn <- rdf[rix,]
        is_punct <- grepl(",|\\.|-|;|\\n", substr(note, ent_b4_dn$stop-1, check_from))

        if(is_punct) {
          # ONLY do if is_punct and it's right after a preposition
          is_prep <- ent_b4_dn$entity == "Preposition"
          b4_dist <- check_from - ent_b4_dn$stop
          # This will be true if entity is preceded immediately by a preposition, in which case it will not shorten the search window
          prep_check <- is_prep & b4_dist < 3
          if(prep_check) {
            check_from <- ent_b4_dn$stop - nchar(gsub(";.+", "", ent_b4_dn$expr))
            rix <- rix-1
          } else {
            nr_rdf <- nrow(rdf)
            if(rix == nr_rdf) {
              rdf <- data.frame()
            } else {
              rdf <- rdf[(rix+1):nr_rdf,]
            }
            break_punct <- TRUE
          }
        } else {
          rix <- rix - 1
        }
      }
    }

    if(nrow(rdf)==0){return(NA)}

    ## Remove entities that might be related to other drug name
    if(!is.null(other_drugs_m)){
      # If before drug name, remove any entities that are immediately adjacent to each other after other drug name.
      rdf_before <- rdf[rdf[,'start'] < drug_window[i, 'drug_start'],]
      od_ix <- which(abs(drug_window$wndw_start[i] - other_drugs_m[,'end']) <= 2)

      if(length(od_ix) > 0 & nrow(rdf_before) > 0){
        # stop position of entities associated with other drug
        od_ix <- od_ix[length(od_ix)]
        rm_other_drug <- other_drugs_m[od_ix,'end']

        for(j in 1:nrow(rdf_before)){
          # Three instead of 2 here allows for a punctuation character
          if(rdf_before$start[j] - rm_other_drug <= 3){
            rm_other_drug <- rdf_before$stop[j]
          }else if(rdf_before$entity[max(j-1,1)] %in% c("Strength", "DoseAmt") & j != 1){
            # If entity is strength or doseamt, allow tablet variation to occur in between this and next entity
            tab_phrase <- replace_tab(substr(note, rdf_before$start[j-1], rdf_before$start[j]))
            dist2next <- rdf_before$start[j]-rdf$stop[j-1]
            if(grepl("tabs", tab_phrase) & dist2next < 2*nchar("tablet")){
              rm_other_drug <- rdf_before$stop[j]
            }else{
              break
            }
          }else{
            # Stop looking
            break
          }
        }
        # Remove entities believed to belong to other drug
        rdf_before <- rdf_before[rdf_before[,'start'] >= rm_other_drug,]
      }

      # If after drug name, use same rule
      rdf_after <- rdf[rdf[,'start'] > drug_window[i, 'drug_stop'],]
      od_ix <- which(abs(other_drugs_m[,'start'] -
                           (drug_window$wndw_start[i] + nchar(drug_window$window[i]))) <= 2)

      if(length(od_ix) > 0 & nrow(rdf_after) > 0){
        # stop position of entities associated with other drug
        od_ix <- od_ix[1]
        rm_other_drug <- other_drugs_m[od_ix,'start']

        for(j in nrow(rdf_after):1){
          # Check for punctuation - if punctuation is present, break (could be separating end of pred info from start of next drug info)
          is_punct <- grepl(",|\\.|-|;|\\n", substr(note, rdf_after$stop[j], other_drugs_m[od_ix,'start']))
          if(is_punct){
            break
          }

          if(rm_other_drug - rdf_after$stop[j] <= 3){
            rm_other_drug <- rdf_after$start[j]
          }else{
            # Allow a word like "the" or a pronoun indicating the patient (e.g. "Discontinue the <other drug>")
            nogap <- nchar(gsub("the|his|hers?|theirs?|yours?", "",
                                substr(note, rdf_after$stop[j], rm_other_drug))) <= 3

            if(nogap){
              rm_other_drug <- rdf_after$start[j]
            }else{
              break
            }
          }
        }

        # Remove entities believed to belong to other drug
        rdf_after <- rdf_after[rdf_after[,'start'] < rm_other_drug,]
      }

      if(nrow(rdf_before) > 0 & nrow(rdf_after) > 0){
        rdf <- rbind(rdf_before, rdf_after)
      }else if(nrow(rdf_before) > 0 & nrow(rdf_after) == 0){
        rdf <- rdf_before
      }else if(nrow(rdf_before) == 0 & nrow(rdf_after) > 0){
        rdf <- rdf_after
      }else if(nrow(rdf_before) == 0 & nrow(rdf_after) == 0){
        # If this process removed all identified entities
        return(NA)
      }

    }


    # failsafe to stop while loop if needed - don't end on a transition or preposition
    x<-0
    while(rdf$entity[nrow(rdf)] %in% c("Preposition","Transition")){
      rdf <- rdf[-nrow(rdf),]
      x<-x+1
      if(x>20 | nrow(rdf) == 0){break}
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
                                                        "finished", "dosepack", "dose pack"))){
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
  results[,'pos'] <- stringr::str_extract(expr, "(?<=;)\\d.+")
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
    results <- results[pred_keep,]
  }

  # Similar for transition, but also require dose-critical info nearby
  if(any(results$entity == "Transition")){
    res_temp <- results
    res_temp$bp <- as.numeric(gsub(":.+", "", res_temp$pos))
    res_temp$ep <- as.numeric(gsub(".+:", "", res_temp$pos))


    trans_keep <- c()
    for(i in 1:nrow(res_temp)){
      if(res_temp$entity[i] == "Transition"){
        if(i == 1){
          ent_chk <- any(res_temp$entity[i+1] %in% c("Strength", "DoseStrength", "DoseAmt",
                                                     "Frequency", "Duration",
                                                     "DoseSchedule"))
          expr_chk <- any(res_temp$expr[i+1] %in% c("done", "off", "stop", "last", "completed",
                                                    "complete", "discontinue", "discontinuing",
                                                    "finished", "dosepack", "dose pack"))


          ent_dist <- res_temp$bp[i+1] - res_temp$ep[i]

          trans_keep <- c(trans_keep, ((ent_chk|expr_chk) & ent_dist <= 10))
        }else if(i == nrow(res_temp)){
          ent_chk <- any(res_temp$entity[i-1] %in% c("Strength", "DoseStrength", "DoseAmt",
                                                     "Frequency", "Duration",
                                                     "DoseSchedule"))
          expr_chk <- any(res_temp$expr[i-1] %in% c("done", "off", "stop", "last", "completed",
                                                    "complete", "discontinue", "discontinuing",
                                                    "finished", "dosepack", "dose pack"))

          ent_dist <- res_temp$bp[i] - res_temp$ep[i-1]
          trans_keep <- c(trans_keep, ((ent_chk|expr_chk) & ent_dist <= 10))
        }else{
          ent_chk <- res_temp$entity[c(i-1,i+1)]
          ent_dist <- c(res_temp$bp[i] - res_temp$ep[i-1], res_temp$bp[i+1] - res_temp$ep[i])
          trans_keep <- c(trans_keep, all(!(ent_chk %in% c("Preposition", "TimeKeyword", "Refill", "IntakeTime",
                                                           "DispenseAmt", "Route", "Keyword"))) &  # for stopping keywords
                            all(ent_dist <= 10))
        }
      }else{
        trans_keep <- c(trans_keep, TRUE)
      }
    }
    results <- results[trans_keep,]
  }

  # Similar but slightly more relaxed for TimeKeyword
  if(any(results$entity == "TimeKeyword")){
    res_temp <- results
    res_temp$bp <- as.numeric(gsub(":.+", "", res_temp$pos))
    res_temp$ep <- as.numeric(gsub(".+:", "", res_temp$pos))


    timekey_keep <- c()
    for(i in 1:nrow(res_temp)){
      if(res_temp$entity[i] == "TimeKeyword"){
        if(i == 1){
          timekey_keep <- c(timekey_keep, grepl(paste0(res_temp$expr[i], "\\s(\\w+\\s)?", res_temp$expr[i+1]),
                                                substr(note, res_temp$bp[i], res_temp$ep[i+1])))
        }else if(i == nrow(res_temp)){
          timekey_keep <- c(timekey_keep, grepl(paste0(res_temp$expr[i-1], "\\s(\\w+\\s)?", res_temp$expr[i]),
                                                substr(note, res_temp$bp[i-1], res_temp$ep[i])))
        }else{
          timekey_keep <- c(timekey_keep,
                            grepl(paste0(res_temp$expr[i], "\\s(\\w+\\s)?", gsub("\\*|\\.", "", res_temp$expr[i+1])),
                                  substr(note, res_temp$bp[i], gsub("\\*|\\.", "", res_temp$ep[i+1]))) |
                              grepl(paste0(gsub("\\*|\\.", "", res_temp$expr[i-1]),
                                           "\\s(\\w+\\s)?", res_temp$expr[i]),
                                    gsub("\\*|\\.", "", substr(note, res_temp$bp[i-1], res_temp$ep[i]))))
        }
      }else{
        timekey_keep <- c(timekey_keep, TRUE)
      }
    }
    results <- results[timekey_keep,]
  }

  # failsafe to stop while loop if needed - check again to make sure we don't end on a transition, preposition, or timekeyword
  x <- 0
  nr <- nrow(results)
  while(results[nr, 'entity'] %in% c("Transition", "Preposition")) {
    results <- results[-nr,]
    nr <- nr - 1
    x <- x + 1
    if(x > 20) break
  }
  results
}
