#' Extract Medication Entities From Clinical Note
#'
#' This function identifies medication entities of interest and returns found
#' expressions with start and stop positions.
#'
#' @param note Text to search.
#' @param drug_names Vector of drug names of interest to locate.
#' @param window_length Length (in number of characters) of window after drug
#' in which to look.
#' @param unit Strength unit to look for (e.g., \sQuote{mg}).
#' @param max_dist Numeric - edit distance to use when searching for \code{drug_names}.
#' @param drug_list Vector of known drugs that may end search window. By default calls
#' \code{\link{rxnorm_druglist}}.
#' @param lastdose Logical - whether or not last dose time entity should be
#' extracted.
#' @param lastdose_window_ext Numeric - multiplicative factor by which
#' \code{window_length} should be extended when identifying last dose time.
#' @param strength_sep Delimiter for contiguous medication strengths (e.g., \sQuote{-} for \dQuote{LTG 200-300}).
#' @param flag_window How far around drug (in number of characters) to look for
#' dose change keyword - default fixed to 30. See \sQuote{Details} section below for further explanation.
#' @param dosechange_dict List of keywords used to determine if a dose change entity is present.
#' @param \dots Parameter settings used in extracting frequency, intake time, route, and duration. Potentially useful
#' parameters include \code{freq_dict}, \code{intaketime_dict}, \code{route_dict}, and \code{duration_dict}
#' (see \code{\dots} argument in \code{\link{extract_entities}}) to specify frequency or intake time
#' dictionaries, as well as \sQuote{freq_fun}, \sQuote{intaketime_fun}, \sQuote{route_fun}, and
#' \sQuote{duration_fun} for user-specified extraction functions. If no additional arguments are provided,
#' \code{medExtractR_tapering} will use \code{\link{extract_generic}} and the default dictionary for each entity.
#' See \code{\link{extract_entities}} documentation for details.
#'
#' @details This function uses a combination of regular expressions, rule-based
#' approaches, and dictionaries to identify various drug entities of interest.
#' Specific medications to be found are specified with \code{drug_names}, which
#' is not case-sensitive or space-sensitive (e.g., \sQuote{lamotrigine XR} is treated
#' the same as \sQuote{lamotrigineXR}). Entities to be extracted include drug name, strength,
#' dose amount, dose, frequency, intake time, route, duration, and time of last dose. See
#' \code{\link{extract_entities}} and \code{\link{extract_lastdose}} for more details.
#'
#' When searching for medication names of interest, fuzzy matching may be used.
#' The \code{max_dist} argument determines the maximum edit distance allowed for
#' such matches. If using fuzzy matching, any drug name with less than 5
#' characters will only allow an edit distance of 1, regardless of the value of
#' \code{max_dist}.
#'
#' The purpose of the \code{drug_list} argument is to reduce false positives by removing information that is
#' likely to be related to a competing drug, not our drug of interest, By default, this is \dQuote{rxnorm} which
#' calls \code{data(rxnorm_druglist)}. A custom drug list in the form of a character string can be supplied instead,
#' or can be appended to \code{rxnorm_druglist} by specifying \code{drug_list = c("rxnorm", custom_drug_list)}.
#' \code{medExtractR} then uses this list to truncate the search window at the first appearance of an unrelated drug name.
#' This uses publicly available data courtesy of the U.S. National Library of Medicine (NLM), National
#' Institutes of Health, Department of Health and Human Services; NLM is not responsible for the product and
#' does not endorse or recommend this or any other product. See \code{rxnorm_druglist} documentation for details.
#'
#' Most medication entities are searched for in a window after the drug. The
#' dose change entity, or presence of a keyword to indicate a non-current drug
#' regimen, may occur before the drug name. The \code{flag_window} argument
#' adjusts the width of the pre-drug window. Both \code{flag_window} and \code{dosechange_dict}
#' are not default arguments to the extended function \code{\link{medExtractR_tapering}} since that
#' extension uses a more flexible search window and extraction procedure. In the tapering extension,
#' entity extraction is more flexible, and any entity can be extracted either before
#' or after the drug mention. Thus functionality for dose change identification is identical to all
#' other dictionary-based entities.
#'
#' The \code{stength_sep} argument is \code{NULL} by default, but can be used to
#' identify shorthand for morning and evening doses. For example, consider the
#' phrase \sQuote{Lamotrigine 300-200} (meaning 300 mg in the morning and 200 mg
#' in the evening). The argument \code{strength_sep = '-'} identifies
#' the full expression \emph{300-200} as \emph{dose strength} in this phrase.
#'
#'
#' @return data.frame with entity information. Only extractions from found entities are returned. If no dosing
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
#' time of last dose: \dQuote{LastDose}\cr
#' Sample output:\cr
#' \tabular{rrr}{
#' entity    \tab  expr   \tab    pos\cr
#' DoseChange\tab  decrease \tab  66:74\cr
#' DrugName   \tab Prograf \tab   78:85\cr
#' Strength  \tab  2 mg   \tab    86:90\cr
#' DoseAmt   \tab  1     \tab     91:92\cr
#' Route   \tab  by mouth     \tab     100:108\cr
#' Frequency \tab  bid    \tab    109:112\cr
#' LastDose  \tab  2100    \tab   129:133
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
#' note1 <- "Progrf Oral Capsule 1 mg 3 capsules by mouth twice a day - last
#' dose at 10pm"
#' medExtractR(note1, c("prograf", "tacrolimus"), 60, "mg", 2, lastdose=TRUE)
#' note2 <- "Currently on lamotrigine 150-200, but will increase to lamotrigine 200mg bid"
#' medExtractR(note2, c("lamotrigine", "ltg"), 130, "mg", 1, strength_sep = "-")
#' }

medExtractR <- function(note, drug_names, window_length, unit, max_dist = 0,
                        drug_list = "rxnorm", lastdose = FALSE, lastdose_window_ext = 1.5,
                        strength_sep = NULL, flag_window = 30, dosechange_dict = 'default',
                        ...) {
  def.saf <- getOption('stringsAsFactors')
  on.exit(options(stringsAsFactors = def.saf))
  options(stringsAsFactors = FALSE)
  if(inherits(dosechange_dict, 'character') && length(dosechange_dict) == 1L && dosechange_dict == 'default') {
    e <- new.env()
    data("dosechange_vals", package = 'medExtractR', envir = e)
    dosechange_dict <- get("dosechange_vals", envir = e)
  }

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

  # Mark mentions that indicate a regimen change
  match_info[, 'DoseChange'] <- NA
  match_info[, 'dc_pos'] <- NA

  note_lc <- tolower(note)
  for(i in seq_len(nr)) {
    sp <- as.numeric(match_info[i, 'start_pos'])
    rc_window <- substr(note_lc, start = sp - flag_window, stop = sp + flag_window)

    rc <- sapply(dosechange_dict$expr, function(fw){
      flag <- regexpr(pattern = paste0(fw, "\\b"), text = rc_window, perl = TRUE)
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
  if(length(rm_index)) {
    dl <- dl[-rm_index]
  }
  other_drugs_start <- numeric(0)
  if(length(dl)) {
    dl_wb <- paste0("\\b", tolower(dl), "\\b")
    dl_wb <- gsub("([+()])", "\\\\\\1", dl_wb)
    other_drugs <- stringi::stri_locate_all_regex(note_lc, dl_wb, omit_no_match = TRUE)
    other_drugs <- other_drugs[lengths(other_drugs) > 0]
    if(length(other_drugs)) {
      other_drugs_m <- do.call(rbind, other_drugs)
      other_drugs_start <- sort(other_drugs_m[,1])
    }
  }

  # are other drugs within string window?
  match_stop <- vapply(seq(nr), function(i) {
    a <- match_info[i,'start_pos']
    b <- match_info[i,'start_pos'] + match_info[i,'length'] + window_length
    od_stop <- other_drugs_start[a < other_drugs_start & other_drugs_start < b]
    if(length(od_stop)) b <- min(od_stop) - 1
    b
  }, numeric(1))
  # String to extract based on number of characters
  # Will search within this string to get drug regimen
  wndw <- mapply(substr, note, match_info[,'start_pos'], match_stop, USE.NAMES = FALSE)

  # Add in extended window if last dose time is desired
  if(lastdose){
    ld_wndw_ext <- do.call(rbind, lapply(seq(nr), function(i){
      start_pos <- match_info[i,'start_pos']
      len <- match_info[i,'length']

      #lastdose needs more of a buffer
      c(substr(note, start = max(1,start_pos-window_length*(lastdose_window_ext/2)),
               stop = start_pos-1),# before window
        substr(note, start = start_pos + len + window_length+1,
               stop = min(nchar(note), start_pos + len + window_length*lastdose_window_ext))) # after window
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

    after_drg <- substr(drg_wndw, nchar(drg)+1, nchar(drg_wndw))
    if(grepl(drg, after_drg, ignore.case = T)){
      return(paste0(drg, sub(paste0(drg, ".+"), "", after_drg)))
    }else{return(drg_wndw)}
  })

  # Want to keep alternative drug names in same window, even if strength/dose appears between them
  drug_window[,'keep_drug'] <- drug_window[,'drug_start'] < c(0, drug_window[-nrow(drug_window),'drug_stop']) + window_length

  # Extract dose entities
  res <- lapply(seq_along(drug_window$window), function(i) {
    rdf <- extract_entities(phrase = drug_window$window[i], p_start = drug_window$drug_start[i],
                            p_stop = drug_window$drug_stop[i], unit = unit,
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
    # Add in DoseChange info if it exists
    if(!is.na(drug_window$dosechange[i])){
      # check if dosechange mention occurs AFTER all other entities
      # if so, ignore it (probably talking about next drug in the list)
      dc_start_pos <- as.numeric(gsub(":.+", "", drug_window$dosechange_pos[i]))
      ent_last_pos <- as.numeric(max(gsub(".+:", "", rdf$expr)))

      # Only add if dosechange occurs before end of last entity (to be reasonably certain it belongs to the drug of interest)
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
  results[,'pos'] <- stringr::str_extract(expr, "(?<=;).+")
  sp <- as.numeric(sub(":.+", "", results[,'pos']))
  results <- unique(results[order(sp),])
  row.names(results) <- NULL

  # sometimes same expression gets extracted as strength and dose due to second drug name cutting off doseamt
  # check/correct for this here
  ix <- unique(results[duplicated(results[,'pos']),'pos'])
  for(x in ix){
    rs <- results[results$pos==x,]
    if(all(sort(rs$entity)==c("Dose", "Strength"))){
      results <- results[-which(results$entity=="Dose" & results$pos==x),]
    }
  }

  return(results)
}
