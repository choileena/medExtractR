#' Extract Medication Entities From Phrase - Extension of \code{\link{extract_entities}} for Tapering application
#'
#' This function searches a phrase for medication dosing entities of interest. It
#' is called within \code{\link{medExtractR_tapering}} and generally not intended for use outside
#' that function.
#'
#' @param phrase Text to search.
#' @param p_start Start position of phrase within original text.
#' @param d_stop End position of drug name within original text.
#' @param unit Unit of measurement for medication strength, e.g., \sQuote{mg}.
#' @param frequency_fun Function used to extract frequency.
#' @param intaketime_fun Function used to extract intake time.
#' @param duration_fun Function used to extract duration.
#' @param route_fun Function used to extract route.
#' @param doseschedule_fun Function used to extract dose schedule.
#' @param preposition_fun Function used to extract preposition.
#' @param timekeyword_fun Function used to extract time keyword.
#' @param transition_fun Function used to extract transition.
#' @param dosechange_fun Function used to extract dose change.
#' @param strength_sep Delimiter for contiguous medication strengths.
#' @param \dots Parameter settings used in extracting frequency and intake time,
#' including additional arguments to \code{frequency_fun} and
#' \code{intaketime_fun}. Use \code{frequency_dict} to identify custom frequency
#' dictionaries and \code{intaketime_dict } to identify custom intake time
#' dictionaries. Similarly, for all other entities with a corresponding \code{<entity>_fun},
#' a custom dictionary can be supplied with the argument \code{<entity>_dict}.
#'
#' @details Various medication dosing entities are extracted within this function
#' including the following:
#'
#' \emph{strength}: The amount of drug in a given dosage form (i.e., tablet, capsule).\cr
#' \emph{dose amount}: The number of tablets, capsules, etc. taken at a given intake time.\cr
#' \emph{dose strength}: The total amount of drug given intake. This quantity would be
#'   equivalent to strength x dose amount, and appears similar to strength when
#'   dose amount is absent.\cr
#' \emph{frequency}: The number of times per day a dose is taken, e.g.,
#'   \dQuote{once daily} or \sQuote{2x/day}.\cr
#' \emph{intaketime}: The time period of the day during which a dose is taken,
#'   e.g., \sQuote{morning}, \sQuote{lunch}, \sQuote{in the pm}.\cr
#' \emph{duration}: How long a patient is on a drug regimen, e.g., \sQuote{2 weeks},
#'   \sQuote{mid-April}, \sQuote{another 3 days}.\cr
#' \emph{route}: The administration route of the drug, e.g., \sQuote{by mouth},
#'   \sQuote{IV}, \sQuote{topical}.\cr
#' \emph{dose change}: Whether the dosage of the drug was changed, e.g.,
#'   \sQuote{increase}, \sQuote{adjust}, \sQuote{reduce}.\cr
#' \emph{dose schedule}: Keywords which represent special dosing regimens, such as tapering
#'   schedules, alternating doses, or stopping keywords, e.g., \sQuote{weaning},
#'   \sQuote{even days} or \sQuote{odd_days}, \sQuote{discontinue}.\cr
#' \emph{time keyword}: Whether the dosing regimen is a past dose, current dose,
#'   or future dose, e.g., \sQuote{currently}, \sQuote{remain}, \sQuote{yesterday}.\cr
#' \emph{transition}: Words or symbols that link consecutive doses of a tapering
#'   regimen, e.g., \sQuote{then}, \sQuote{followed by}, or a comma \sQuote{,}.\cr
#' \emph{preposition}: Prepositions that occur immediately next to another
#'   identified entity, e.g., \sQuote{to}, \sQuote{until}, \sQuote{for}.\cr
#' \emph{dispense amount}: The number of pills prescribed to the patient.\cr
#' \emph{refill}: The number of refills allowed for the patient's prescription.\cr
#'
#' Similar to the basic implementation, drug name and and time of last dose are not
#' handled by the \code{extract_entities_tapering} function. Those entities are extracted separately
#' and appended to the \code{extract_entities_tapering} output within the main \code{\link{medExtractR_tapering}}
#' function. In the tapering extension, however, dose change is treated the same as other dictionary-based
#' entities and extracted within \code{extract_entities_tapering}. Strength, dose amount, dose strength, dispense amount,
#' and refill are primarily numeric quantities, and are identified using a combination of
#' regular expressions and rule-based approaches. All other entities use dictionaries for
#' identification. For more information about the default dictionary for a specific entity,
#' view the documentation file for the object \code{<entity>_vals}.
#'
#' By default and when an argument \code{<entity>_fun} is \code{NULL}, the
#' \code{\link{extract_generic}} function will be used to extract that entity. This function
#' can also inherit user-defined entity dictionaries for each entity, supplied as arguments \code{<entity>_dict}
#' to \code{\link{medExtractR}} or \code{\link{medExtractR_tapering}} (see documentation files for main function(s) for details).
#'
#' Note that \code{extract_entities_tapering} has the argument \code{d_stop}. This differs
#' from \code{\link{extract_entities}}, which uses the end position of the full search window. This
#' is a consequence of \code{\link{medExtractR}} using a fixed search window length and \code{\link{medExtractR_tapering}}
#' dynamically constructing a search window.
#'
#' @return data.frame with entities information. At least one row per entity is returned,
#' using \code{NA} when no expression was found for a given entity.\cr
#' The \dQuote{entity} column of the output contains the formatted label for that entity, according to
#' the following mapping.\cr
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
#' Sample output for the phrase \dQuote{Lamotrigine 200mg bid for 14 days} would look like:\cr
#' \tabular{rr}{
#'  entity   \tab  expr\cr
#'  IntakeTime  \tab  <NA>\cr
#'  Strength  \tab   <NA>\cr
#'  DoseAmt   \tab  <NA>\cr
#'  DoseChange   \tab  <NA>\cr
#'  DoseSchedule   \tab  <NA>\cr
#'  TimeKeyword   \tab  <NA>\cr
#'  Transition   \tab  <NA>\cr
#'  Preposition   \tab  <NA>\cr
#'  DispenseAmt   \tab  <NA>\cr
#'  Refill   \tab  <NA>\cr
#'  Frequency \tab  bid;19:22\cr
#'  DoseStrength  \tab  200mg;13:18\cr
#'  Preposition \tab for;23:26\cr
#'  Duration \tab 14 days;27:34
#' }
#'
#' @export
#'
#' @examples
#' note <- "prednisone 20mg daily tapering to 5mg daily over 2 weeks"
#' extract_entities_tapering(note, 1, 11, "mg")
#' # A user-defined dictionary can be used instead of the default
#' my_dictionary <- data.frame(c("daily", "twice daily"))
#' extract_entities(note, 1, 11, "mg", frequency_dict = my_dictionary)

extract_entities_tapering <- function(phrase, p_start, d_stop, unit, frequency_fun = NULL,
                                      intaketime_fun = NULL,
                                      duration_fun = NULL,
                                      route_fun = NULL,
                                      doseschedule_fun = NULL,
                                      preposition_fun = NULL,
                                      timekeyword_fun = NULL,
                                      transition_fun = NULL,
                                      dosechange_fun = NULL,
                                      strength_sep = NULL, ...){
  empty_result <- data.frame(
    entity = c("Frequency", "IntakeTime", "Strength", "DoseAmt", "DoseStrength"),
    expr = NA, pos = NA, start = NA, stop = NA, rn = NA
  )

  phrase_orig <- phrase
  p_start <- p_start-1

  # generic extraction
  xtra_args <- list(...)
  ent_types <- sub('_dict', '', grep('_dict', names(xtra_args), value = TRUE))
  known_types <- c('dosechange','doseschedule','duration','frequency','intaketime','preposition','route','timekeyword','transition')
  oth_types <- setdiff(ent_types, known_types)
  oth_l <- length(oth_types)
  oth_ent <- vector('list', oth_l)
  if(oth_l) {
    for(i in seq(oth_l)) {
      ent_type <- oth_types[i]
      oth_args <- list(phrase = phrase, type = ent_type, fun = NULL, ...)
      df <- do.call(extract_type, oth_args)
      my_ent <- entity_metadata(phrase, p_start, df)
      oth_ent[[i]] <- data.frame(entity = ent_type, expr = my_ent)
    }
  }

  ### DURATION ####
  df <- extract_type(phrase, 'duration', duration_fun, ...)
  duration <- entity_metadata(phrase, p_start, df)

  # Reclassify "1/2" as a doseamt
  if(any(grepl("^1/2;", duration))){
    ix <- which(grepl("^1/2;", duration))
    doseamt <- duration[ix]

    # Allow correction "1/2" if it's an expression like "1 1/2"
    for(i in seq_along(doseamt)){
      bp <- as.numeric(sub("(.+;)", "", sub(":.+", "", doseamt[i])))-2
      ep <- as.numeric(sub(".+:", "", doseamt[i]))
      da_phrase <- substr(phrase, bp-p_start, ep-p_start-1)

      if(grepl("\\d\\s1/2", da_phrase)){
        doseamt[i] <- paste(da_phrase,
                            paste(bp, ep, sep=":"), sep = ";")
      }
    }


    # Update duration
    duration <- duration[-ix]
    if(length(duration)==0){duration <- NA}
  }

  ## DURATION - If a number is identified as part of a duration expression, we wouldn't want to
  # extract that in the next part. Censor extracted durations
  if(!all(is.na(duration))){
    for(i in 1:length(duration)){
      phrase <- paste0(substr(phrase, 1, df[i,'pos']-1),
                       paste0(rep("X", times=as.numeric(df[i,'expr_len'])),collapse=""),
                       substr(phrase, df[i,'pos']+df[i,'expr_len'], nchar(phrase)))
    }
  }

  # censor date expressions
  phrase <- internal_censor_dates(phrase)

  pfn <- internal_find_numbers(phrase,
    "(?<!q)((\\d{1,3},\\d{3})|(\\.?\\d+))(\\.\\d+)?(?!(st|th))",
    "\\s?(%|dose(s?)|hours|hrs|weeks|wks|days|years|yrs|am|pm)(?![a-zA-Z0-9])",
    c("\\bone", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
  )
  all_numbers <- pfn$all_numbers
  num_positions <- pfn$num_positions
  tn_expr <- pfn$tn_expr
  tn_pos <- pfn$tn_pos

  if(length(all_numbers) == 0 & length(tn_expr) == 0) {
    strength <- NA;dosestr <- NA;disp <- NA;refill <- NA
    if(!exists("doseamt")) {
      doseamt <- NA
    }
    remaining_numbers <- all_numbers
    num_pos <- num_positions
  } else { # only look for entities if they exist
    phrase_lc <- tolower(phrase)
    remaining_numbers <- all_numbers
    num_pos <- num_positions

    ### DISPENSE AMT ###
    if(length(remaining_numbers)==0){
      disp <- NA
    }else{

      disp <- mapply(function(rn, np){
        if(grepl("disp(ense)?", substr(phrase_lc, np-10, np+10))){

          paste(rn, paste(np + p_start, np + nchar(rn) + p_start, sep = ":"), sep = ";")

        }else{c(NA)}

      }, rn = remaining_numbers, np = num_pos, USE.NAMES=FALSE)

      # Numeric expressions that still need to be examined
      remaining_numbers <- remaining_numbers[which(is.na(disp))]
      num_pos <- num_pos[which(is.na(disp))]

      # Keep only non-NA values
      disp <- disp[!is.na(disp)]
      if(length(disp)==0){disp <- NA}
    }

    ### REFILLS ###
    if(length(remaining_numbers)==0){
      refill <- NA
    }else{

      refill <- mapply(function(rn, np){
        # for number before refill - make sure not allowing colon
        if(grepl(paste0("refill(s?):?\\s?",rn), substr(phrase_lc, np-10, np+10))){

          paste(rn, paste(np + p_start, np + nchar(rn) + p_start, sep = ":"), sep = ";")

        }else{c(NA)}

      }, rn = remaining_numbers, np = num_pos, USE.NAMES=FALSE)

      # Numeric expressions that still need to be examined
      remaining_numbers <- remaining_numbers[which(is.na(refill))]
      num_pos <- num_pos[which(is.na(refill))]

      # Keep only non-NA values
      refill <- refill[!is.na(refill)]
      if(length(refill)==0){refill <- NA}
    }

    any_refill_0 <- grepl("refills:\\s0(?!\\d)", phrase_lc, perl=TRUE)
    if(any_refill_0){
      r0_pos <- gregexpr("(?<=refills:\\s)0", phrase_lc, perl=TRUE)

      refill_0 <- paste("0", paste(r0_pos[[1]] + p_start, r0_pos[[1]] + p_start + 1, sep=":"), sep=";")

      if(is.na(refill)){refill <- refill_0}else{refill <- c(refill, refill_0)}
    }

    ### STRENGTH ####
    if(length(remaining_numbers)==0){
      strength <- NA
    }else{
      unit_len <- nchar(unit)
      # check if any numbers are followed by unit
      strength <- mapply(function(rn, np){

        # Need narrow window in case same number appears both with and without unit
        r <- regexpr(paste0(rn, "(\\s+)?", unit), substr(phrase_lc, np, np + nchar(rn) + unit_len + 3))

        if(r == -1){c(NA)}else{
          stp <- np + attributes(r)$match.length

          paste(substr(phrase, np, stp-1), paste(np + p_start, stp + p_start, sep = ":"), sep = ";")
        }

      }, rn = remaining_numbers, np = num_pos, USE.NAMES=FALSE)

      # Numeric expressions that still need to be examined
      remaining_numbers <- remaining_numbers[which(is.na(strength))]
      num_pos <- num_pos[which(is.na(strength))]

      # Keep only non-NA values
      strength <- strength[!is.na(strength)]
      if(length(strength)==0){strength <- NA}
    }

    if(length(tn_expr)>0){
      unit_len <- nchar(unit)
      # check if any numbers are followed by unit
      strength_txt <- mapply(function(rn, np){

        # Need narrow window in case same number appears both with and without unit
        r <- regexpr(paste0(rn, "(\\s+)?", unit), substr(phrase_lc, np, np + nchar(rn) + unit_len + 3))

        if(r == -1){c(NA)}else{
          stp <- np + attributes(r)$match.length

          paste(substr(phrase, np, stp-1), paste(np + p_start, stp + p_start, sep = ":"), sep = ";")
        }

      }, rn = tn_expr, np = tn_pos, USE.NAMES=FALSE)

      # Numeric expressions that still need to be examined
      tn_expr <- tn_expr[which(is.na(strength_txt))]
      tn_pos <- tn_pos[which(is.na(strength_txt))]

      # Keep only non-NA values
      if(all(is.na(strength)) & sum(!is.na(is.na(strength_txt))>0)){
        strength <- strength_txt
      }else if(!all(is.na(strength)) & sum(!is.na(is.na(strength_txt))>0)){
        strength <- c(strength, strength_txt)
      }

    }

    ### DOSEAMT ###
    # Sequence of numbers
    if(length(remaining_numbers) >= 2){
      seq_num <- which(abs(diff(as.numeric(remaining_numbers))) %in% c(1,0.5))
      if(length(seq_num) > 0){
        seq_num <- sapply(seq_num, function(p){
          # Look for a symbol that could indicate this is part of a list
          x <- grepl(",|;", substr(phrase, num_pos[p], num_pos[p+1]))
          # Just keep ones that appear within 2-3 characters of each other
          y <- num_pos[p+1] - num_pos[p] <= 3
          ifelse(x & y, p, NA)
        })

        if(!all(is.na(seq_num))){
          ix <- seq_num[!is.na(seq_num)]
          ix <- c(ix, max(ix)+1)
          da_seq <- paste(remaining_numbers[ix],
                          paste(num_pos[ix]+p_start,
                                num_pos[ix]+p_start+nchar(remaining_numbers[ix]),sep=":"), sep=";")


          remaining_numbers <- remaining_numbers[-ix]
          num_pos <- num_pos[-ix]

          if(!exists("doseamt")){
            doseamt <- da_seq
          }else{
            doseamt <- c(doseamt, da_seq)
          }
        }
      }
    }

    if(length(remaining_numbers) == 0) {
      if(!exists("doseamt")){doseamt <- NA}
    } else {

      doseamt_addl <- mapply(function(rn, np){
        # substr is used to define narrow search windows
        # this helps prevent overlap between adjacent numeric values
        da <- grepl(paste0(rn, "(\\s+)?(\\(\\w+\\)\\s+)?tabs"),
                    replace_tab(substr(phrase, np, np+15)), perl=T)

        # "take" notation
        if(!da){da <- grepl(paste0("(take|takes|taking)(\\s?)", rn),
                            substr(phrase, max(1, np-8), np+nchar(rn)), perl=TRUE)}

        # parenthetical notation
        if(!da){da <- grepl(paste0("\\(", rn, "\\s?(p|t|c)?\\)"),
                            substr(phrase, max(1, np-2), np+nchar(rn)+2), perl=T)}

        # number immediately after strength/dose mention, but not with another number immeiately after
        if(!da){da <- grepl(paste0("", unit, "\\)?\\s?", rn, "\\s(?!(\\d|hours?|hrs?))"),
                            substr(phrase, max(1, np-8), np+nchar(rn)+7),perl=T)}

        if(!da){da <- grepl(paste0(rn, "(\\son)?\\s(day|week)\\s\\d"),
                            substr(phrase_orig, np, np+nchar(rn)+10),perl=T)}

        if(!da){da <- grepl(paste0(rn, "(\\son)?\\s(day|week)\\s(one|two|three|four|five|six|seven|eight|nine|ten)"),
                            substr(phrase_orig, np, np+nchar(rn)+10),perl=T)}

        if(!da){da <- grepl(paste0(rn, "\\son\\s(first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth)\\s(day|week)"),
                            substr(phrase_orig, np, np+nchar(rn)+20),perl=T)}

        if(!da){da <- NA}else{
          da <- paste(rn, paste(np + p_start, np + nchar(rn) + p_start, sep = ":"), sep = ";")
        }

        return(da)
      }, rn = remaining_numbers, np = num_pos, USE.NAMES=FALSE)

      # Numeric expressions that still need to be examined
      remaining_numbers <- remaining_numbers[which(is.na(doseamt_addl))]
      num_pos <- num_pos[which(is.na(doseamt_addl))]

      # Keep only non-NA values
      if(!exists("doseamt")){
        doseamt <- doseamt_addl[!is.na(doseamt_addl)]
        if(length(doseamt)==0){doseamt <- NA}
      }else{
        doseamt <- c(doseamt, doseamt_addl[!is.na(doseamt_addl)])
      }

    }

    if(length(tn_expr)>0){
      text_doseamt <- mapply(function(tne, tnp){
        # tablet notation
        da <- regexpr(paste0(tne, "(?=(\\s+)?(\\(\\w*\\)\\s+)?tabs)"),
                      replace_tab(substr(phrase_lc, tnp, tnp+15)), perl=T)

        # "take" notation
        if(da == -1){da_expr <- stringr::str_extract(substr(phrase_lc, max(1, tnp-8), tnp+nchar(tne)),
                                            paste0("(?<=(take|takes|taking)(\\s?))", tne))
        if(!is.na(da_expr)){da <- regexpr(tne, substr(phrase_lc, tnp, tnp+nchar(tne)))}}

        # parenthetical notation
        if(da == -1){da <- regexpr(paste0("(?<=[(])", tne, "(?=[)])"),
                                   substr(phrase_lc, max(1, tnp-2), tnp+nchar(tne)+1), perl=T)}

        if(da == -1){da <- regexpr(paste0(tne, "(?=\\son\\s(day|week)\\s\\d)"),
                                   substr(phrase_orig, tnp, tnp+nchar(tne)+15),perl=T)}

        if(da == -1){da <- regexpr(paste0(tne, "(?=\\son\\s(first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth)\\s(day|week))"),
                                   substr(phrase_orig, tnp, tnp+nchar(tne)+25),perl=T)}

        if(da == -1){da <- NA}else{
          stp <- tnp + attributes(da)$match.length
          da <- paste(substr(phrase_lc, tnp, stp-1), paste(tnp + p_start, stp + p_start, sep = ":"), sep = ";")
        }

        return(da)
      }, tne = tn_expr, tnp = tn_pos, USE.NAMES = FALSE)

      na_text_doseamt <- is.na(text_doseamt)
      tn_expr <- tn_expr[na_text_doseamt]
      tn_pos <- tn_pos[na_text_doseamt]
      text_doseamt <- text_doseamt[!na_text_doseamt]

      if(length(text_doseamt) > 0) {
        doseamt <- c(doseamt[!is.na(doseamt)], text_doseamt)
      }
    }

    ## DOSE ##

    dosestr <- NA
    if(!is.null(strength_sep)) {
      if(length(remaining_numbers) > 0) {
        # Cases where times of doses are denoted as ##-##

        num_end <- num_pos + nchar(remaining_numbers)

        # Check if there is only distance of 1 between end of one word and start of the next
        maybe_dsc <- which(num_pos[-1]-num_end[-length(num_pos)] == 1)
        is_dsc <- sapply(maybe_dsc, function(j){
          substr(phrase, num_end[j], num_pos[j+1]-1)
        }) %in% strength_sep
        dsc_index <- sort(c(maybe_dsc[c(is_dsc)], maybe_dsc[c(is_dsc)]+1))

        # Add to dose results
        if(length(dsc_index) > 0) {
          # Extract full expression - allow for variable number of dose separated by marker (e.g. x/x or x/x/x)
          rpt <- which(dsc_index==c(NA,dsc_index[1:(length(dsc_index)-1)]))
          dsci <- if(length(rpt)>0){dsc_index[-c(rpt-1,rpt)]}else{dsc_index}


          # right now assumes only one dose expression like this in phrase (e.g. wouldn't account for "y/y" in "drug name x/x then y/y")
          dsc_split <- remaining_numbers[dsci]
          dsc_split_pos <- num_pos[dsci]

          dsc <- paste(substr(phrase, dsc_split_pos[1], dsc_split_pos[2]+nchar(dsc_split[2])-1),
                       paste(dsc_split_pos[1]+ p_start,
                             dsc_split_pos[2]+nchar(dsc_split[2])+ p_start, sep=":"), sep=";")

          num_pos <- num_pos[setdiff(1:length(remaining_numbers), dsc_index)]
          remaining_numbers <- remaining_numbers[setdiff(1:length(remaining_numbers), dsc_index)]


          dosestr <- dsc
        }
      }
    }

  }

  # non-numeric entities

  ### FREQ ####
  df <- extract_type(phrase, 'frequency', frequency_fun, ...)
  freq <- entity_metadata(phrase, p_start, df)

  ### INTAKETIME ###
  df <- extract_type(phrase, 'intaketime', intaketime_fun, ...)
  intaketime <- entity_metadata(phrase, p_start, df)

  ### ROUTE ####
  df <- extract_type(phrase, 'route', route_fun, ...)
  route <- entity_metadata(phrase, p_start, df)

  ## Check for any remaining numbers to occur immediately before freq, intaketime, or route - if yes, classify as doseamt
  if(length(remaining_numbers) > 0) {
    if(length(tn_expr) > 0) {
      rem_num <- c(remaining_numbers, gsub("\\b", "", tn_expr, fixed=TRUE))
      rm_num_pos <- c(num_pos, tn_pos)
    } else {
      rem_num <- remaining_numbers
      rm_num_pos <- num_pos
    }
    is_txt <- c(rep(FALSE, length(remaining_numbers)), rep(TRUE, length(tn_expr)))
    tmp_order <- order(rm_num_pos)
    rem_num <- rem_num[tmp_order]
    rm_num_pos <- rm_num_pos[tmp_order]
    is_txt <- is_txt[tmp_order]
  } else {
    rem_num <- tn_expr
    rm_num_pos <- tn_pos
    is_txt <- length(tn_expr)
  }

  if(length(rem_num) > 0 & (all(!is.na(freq)) | all(!is.na(intaketime)))) {
    freq_it <- NULL
    if(all(!is.na(freq))) freq_it <- c(freq_it, freq)
    if(all(!is.na(intaketime))) freq_it <- c(freq_it, intaketime)
    if(all(!is.na(route))) freq_it <- c(freq_it, route)

    # Start positions of any extracted freq/intake time
    freq_it_startpos <- as.numeric(gsub(".+;", "", gsub(":.+", "", freq_it)))

    # Stop positions of any remaining numbers
    np_stoppos <- rm_num_pos + nchar(gsub("\\b", "", rem_num, fixed=TRUE)) + p_start
    is_da_ds <- vapply(np_stoppos, function(np) {
      ent_dist <- freq_it_startpos - np
      any(ent_dist == 0 | ent_dist == 1)
    }, logical(1))

    if(sum(is_da_ds) > 0) {
      ix <- which(is_da_ds)
      num_expr <- suppressWarnings(as.numeric(rem_num[ix]))
      # Consider dose amount if small (or text - typically only smaller numbers are written as text)
      # Consider as dosestrength is larger
      ix_da <- ix[is.na(num_expr) | num_expr <= 10]
      ix_ds <- ix[!is.na(num_expr) & num_expr > 10]

      # Reclassify doseamt cases
      if(length(ix_da) > 0) {
        dap1 <- rm_num_pos[ix_da] + p_start
        dap2 <- rm_num_pos[ix_da] + nchar(gsub("\\b","", rem_num[ix_da], fixed=TRUE)) + p_start
        new_da <- sprintf("%s;%s:%s", rem_num[ix_da], dap1, dap2)
        if(all(is.na(doseamt))) {
          doseamt <- new_da
        } else {
          doseamt <- c(doseamt, new_da)
        }
      }

      # Reclassify dosestrength cases
      if(length(ix_ds) > 0) {
        dsp1 <- rm_num_pos[ix_ds] + p_start
        dsp2 <- rm_num_pos[ix_ds] + nchar(rem_num[ix_ds]) + p_start
        new_ds <- sprintf("%s;%s:%s", rem_num[ix_ds], dsp1, dsp2)
        if(all(is.na(dosestr))) {
          dosestr <- new_ds
        } else {
          dosestr <- c(dosestr, new_ds)
        }
      }
      remaining_numbers <- remaining_numbers[-ix]
      num_pos <- num_pos[-ix]
    }
  }
  # nothing happens to any "remaining_numbers"

  ### doseschedule ####
  df <- extract_type(phrase, 'doseschedule', doseschedule_fun, ...)
  doseschedule <- entity_metadata(phrase, p_start, df)

  ### preposition ####
  df <- extract_type(phrase, 'preposition', preposition_fun, ...)
  preposition <- entity_metadata(phrase, p_start, df)

  ### timekeyword ####
  df <- extract_type(phrase, 'timekeyword', timekeyword_fun, ...)
  timekeyword <- entity_metadata(phrase, p_start, df)

  ### transition ####
  df <- extract_type(phrase, 'transition', transition_fun, ...)
  transition <- entity_metadata(phrase, p_start, df)

  ### dosechange ####
  df <- extract_type(phrase, 'dosechange', dosechange_fun, ...)
  dosechange <- entity_metadata(phrase, p_start, df)

  ## ONE FINAL LOOK AT DOSE ##
  # Reclassify strength as dose when necessary
  keep_str <- sapply(strength, function(st){

    ## strength expression is in parentheses
    # expression
    expr <- sub(";.+", "", st)

    # positions
    bp1 <- sub(":.+", "", st)
    bp <- as.numeric(sub(".+;", "", bp1)) - p_start
    ep <- as.numeric(sub(".+:", "", st)) - p_start

    # doseamt occurs right before strength (allow <=1 in case number is in parentheses)
    after_da <- if(all(is.na(doseamt))){FALSE}else{
      any(abs((bp+p_start-1) - as.numeric(sapply(doseamt, gsub,
                                                 pattern = ".+:",
                                                 replacement = ""))) <= 1)
    }

    # strength has tablet after it, not necessarily in parentheses
    before_tab <- grepl(paste0(expr, ".?\\s?(\\w+\\s)?tabs"),
                        replace_tab(substr(phrase, bp-1, ep+20)), perl=T)

    return(after_da | before_tab)
  })
  keep_str[is.na(keep_str)] <- FALSE

  str_holdout <- strength[keep_str]

  # Anything not marked as keep_str gets converted to dosestr
  if(sum(!keep_str) > 0) {
    if(all(is.na(dosestr))) {
      dosestr <- strength[!keep_str]
    } else {
      dosestr <- c(dosestr, strength[!keep_str])
    }
    strength <- strength[keep_str]
  }

  ## !! In tapering setting, expected to have multiple repeated entities. Criteria of doseamt present => strength no longer holds
  if(all(is.na(doseamt))){ # If doseamt missing, reclassify strength as dose. works even if strength=NA
    if(all(is.na(dosestr))){dosestr <- strength}else{dosestr <- c(dosestr, strength)}
    strength <- NA
  } else {
    if(!all(is.na(strength)) & all(is.na(dosestr))) {
      # If both strength and doseamt are non-missing - get start positions
      da_sp <- as.numeric(stringr::str_extract(doseamt, "(?<=;).+(?=:)"))
      str_sp <- as.numeric(stringr::str_extract(strength, "(?<=;).+(?=:)"))

      # list strength/doseamt entities in order of start position
      df_sp <- data.frame(sp = c(da_sp, str_sp),
                          ent = c(rep("da", length(da_sp)), rep("str", length(str_sp))))
      df_sp <- df_sp[order(df_sp[,'sp']),]

      # entity indicators
      is_str <- which(df_sp$ent=="str")
      is_da <- which(df_sp$ent=="da")

      # if doseamt isn't after strength, should be dose
      is_dosestr <- sapply(is_str, function(i){
        !any(is_da == i + 1)
      })
      if(any(is_dosestr)) {
        dosestr <- strength[is_dosestr]
        strength <- ifelse(all(is_dosestr), NA, strength[!is_dosestr])
      }
    }
  }

  # Things that should not have been changed over
  if(length(str_holdout) > 0){
    # remove from dose
    switch_back <- dosestr %in% str_holdout
    dosestr <- dosestr[!switch_back]
    if(length(dosestr)==0){dose <- NA}

    # put back into strength
    if(all(is.na(strength))){
      strength <- str_holdout
    }else{
      strength <- c(strength, str_holdout)
    }

  }

  # if doseamt is too high, convert to dispense amount
  if(!all(is.na(doseamt))){
    da_expr <- gsub(";.+", "", doseamt)
    da_start <- as.numeric(gsub(".+;", "", gsub(":.+", "", doseamt)))
    da_stop <- as.numeric(gsub(".+:", "", doseamt))
    # doseamts that are too high - chose 10 becuase taking more than 10 pills of a single drug is unlikely
    is_disp <- suppressWarnings(as.numeric(da_expr) > 10) # suppress warnings for text dose amounts

    if(sum(is_disp,na.rm = TRUE)> 0 & all(is.na(disp))){
      is_disp[is.na(is_disp)] <- FALSE

      # Only consider as dispense amt if tablet or dispense appears nearby
      change_disp <- sapply(1:length(da_start), function(i){
        ifelse(is_disp[i], grepl("dispense|tab(let)?s?|pills?|cap(sule)?s?",
                                 substr(phrase, da_start[i]-p_start-15, da_stop[i]-p_start+15)), FALSE)
      })
      if(sum(change_disp)>0){disp <- doseamt[change_disp]}
      # Either way, too high to be doseamt
      doseamt <- doseamt[!is_disp]
    }else if(length(is_disp)> 0 & !all(is.na(disp))){
      is_disp[is.na(is_disp)] <- FALSE

      # Only consider as dispense amt if tablet or dispense appears nearby
      change_disp <- sapply(1:length(da_start), function(i){
        ifelse(is_disp[i], grepl("dispense|tab(let)?s?|pills?|cap(sule)?s?",
                                 substr(phrase, da_start[i]-p_start-15, da_stop[i]-p_start+15)), FALSE)
      })
      if(sum(change_disp)>0){disp <- c(disp, doseamt[change_disp])}
      doseamt <- doseamt[!is_disp]
    }
    if(length(doseamt)==0){doseamt<-NA}
  }
  # Correct doseamt expression if "\\bone" was identified
  doseamt <- gsub("\\b", "", doseamt, fixed=TRUE)


  #### Building results ###
  special_expr <- c("done", "off", "stop", "last", "completed", "complete", "discontinue", "discontinuing", "finished", "dosepack", "dose pack")

  # If no strength/dose was found, then set all values to NA (only want when associated dose info is present)
  if(length(dosestr)==0){dosestr <- NA}
  if(all(sapply(list(strength, doseamt, dosestr, duration), function(x) all(is.na(x))))){
    if(!any(tolower(gsub(";.+", "", doseschedule)) %in% special_expr)) {
      return(empty_result)
    }
  }


  ent_res <- list("Frequency" = freq, "IntakeTime" = intaketime,
                  "Strength" = strength, "DoseAmt" = doseamt, "DoseStrength" = dosestr,
                  "Duration" = duration, "Route" = route,"Transition" = transition,
                  "Preposition" = preposition, "TimeKeyword" = timekeyword, "DoseSchedule" = doseschedule,
                  "DoseChange" = dosechange, "DispenseAmt" = disp, "Refill" = refill)

  entities <- c("Frequency", "IntakeTime", "Strength", "DoseAmt", "DoseStrength", "Duration", "Route",
                "Transition", "Preposition", "TimeKeyword", "DoseSchedule", "DoseChange", "DispenseAmt", "Refill")

  lf <- sum(!is.na(freq))
  lit <- sum(!is.na(intaketime))
  lstr <- sum(!is.na(strength))
  lda <- sum(!is.na(doseamt))
  lds <- sum(!is.na(dosestr))
  ldur <- sum(!is.na(duration))
  lrt <- sum(!is.na(route))
  lt <- sum(!is.na(transition))
  lprep <- sum(!is.na(preposition))
  ltk <- sum(!is.na(timekeyword))
  ldsc <- sum(!is.na(doseschedule))
  ldch <- sum(!is.na(dosechange))
  ldisp <- sum(!is.na(disp))
  lref <- sum(!is.na(refill))

  not_found <- entities[which(c(lf, lit, lstr, lda, lds, ldur, lrt, lt, lprep, ltk, ldsc, ldch, ldisp, lref) == 0)]
  found <- setdiff(entities, not_found)

  res_f <- NULL
  res_nf <- NULL
  if(length(not_found) > 0) {
    res_nf <- data.frame(entity = not_found, expr = NA_character_)
  }
  if(length(found) > 0) {
    found_res <- ent_res[names(ent_res) %in% found]
    fr <- vector('list', length(found_res))
    for(i in seq_along(found_res)) {
      fr[[i]] <- data.frame(entity = found[i], expr = found_res[[i]])
    }
    # include other generic entities
    res_f <- do.call(rbind, c(fr, oth_ent))
  }
  res <- rbind.data.frame(res_nf, res_f)
  res <- unique(res)
  res <- res[!is.na(res[,'expr']) & !is.na(res[,'entity']),]

  ## !! RESTRICT EXTRACTED ENTITIES - only consider distances between dose-critical entities (str/dose, doseamt, duration)
  res$pos = gsub(".+;", "", res$expr)
  res$start = as.numeric(gsub(":.+", "", res$pos))
  res$stop = as.numeric(gsub(".+:", "", res$pos))
  res <- res[order(res[,'start'], res[,'stop'], !(res[,'entity'] %in% entities)),]
  # remove exact position duplicates
  posKey <- do.call(paste, c(res[,c('start', 'stop')], sep = '|'))
  res <- res[!duplicated(posKey),]

  # If expressions overlap, keep the longest match
  resN <- nrow(res)
  if(resN > 1) {
    start_match <- c(res[-resN,'start'] == res[-1,'start'], FALSE)
    stop_match <- c(FALSE, res[-1,'stop'] == res[-resN,'stop'])
    dup <- start_match | stop_match
    res <- res[!dup,]
  }

  res[,'rn'] <- seq(nrow(res))
  res1 <- res[!(res[,'entity'] %in% c("Preposition", "Transition")),]

  # Only look for gaps if there is more than one row
  if(nrow(res1)>1){
    res1$gap = c(NA, res1$start[2:length(res1$start)] - res1$stop[1:(length(res1$stop)-1)])

    # Need to add in adjusted distances for relation to drug stop since drug name not part of res table yet
    # entity right before drug name
    if(any(res1$start - d_stop < 0, na.rm = TRUE)){
      adj_gap <- max(which(res1$start - d_stop < 0))
      if(length(adj_gap) > 0){res1$gap[adj_gap] <- d_stop - res1$stop[adj_gap]}
    }
    # entity right after drug name
    if(any(res1$start - d_stop > 0, na.rm = TRUE)){
      adj_gap <- min(which(!is.na(res1$start) & res1$start - d_stop > 0))
      if(length(adj_gap) > 0){res1$gap[adj_gap] <- res1$start[adj_gap] - d_stop}
    }

    # Remove entities in gap before group starts
    gap_size=50 # (consider possibly shorter for before drug name and longer for after)
    start_ix <- which((res1$gap > gap_size*0.5) & (res1$start < d_stop))
    if(length(start_ix)>0){
      start_ix <- min(start_ix)

      # Keep excluded entities if close to where we start excluding
      rn <- res1$rn[start_ix]
      while(rn > 1){
        ent_gap <- res$start[rn] - res$stop[rn-1]
        if(res$entity[rn-1] %in% c("Preposition", "Transition")){
          # If preceding entity is a preposition or transition, require it to occur immediately before
          if(ent_gap <= 1){
            rn <- rn-1
          }else{
            break
          }
        }else{
          # If preceding entity is a time keyword, allow a short gap
          if(ent_gap <= 5){
            rn <- rn-1
          }else{
            break
          }
        }
      }

      if(rn > 1){
        res <- res[-c(1:(rn-1)),]
      }else{
        res <- res[-1,]
      }
    }


    # Remove entities in gap after drug group starts
    stop_ix <- which((res1$gap > gap_size) & (res1$start > d_stop))
    if(length(stop_ix)>0){
      stop_ix <- min(stop_ix)

      if(stop_ix==1){
        # All key entities are far away from drug name
        res <- res[res[,'start'] < d_stop,]
      }else{
        # Keep excluded entities if close to where we start excluding
        rn <- res1$rn[stop_ix-1]
        while(rn < res1$rn[stop_ix]){
          ent_gap <- res$start[res$rn == rn+1] - res$stop[res$rn == rn]
          if(length(ent_gap)==0){ent_gap <- 6}
          if(res$entity[res$rn == rn+1] %in% c("Preposition", "Transition")){
            # If next entity is a preposition or transition, require it to occur immediately before
            if(ent_gap <= 1){
              rn <- rn+1
            }else{
              break
            }
          }else{
            # If next entity is a time keyword, allow a short gap
            if(ent_gap <= 5){
              rn <- rn+1
            }else{
              break
            }
          }
        }

        res <- res[res$rn <= rn,]
      }


    }
  }

  # Check for key entities again
  if(!any(res$entity %in% c("Strength", "DoseAmt", "DoseStrength", "Duration"))){
    # if no key entity and no special expression, extraction failed
    if(!any(tolower(gsub(";.+", "", doseschedule)) %in% special_expr)) {
      return(empty_result)
    }
  }
  res
}
