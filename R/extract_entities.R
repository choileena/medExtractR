#' Extract Medication Entities From Phrase
#'
#' This function searches a phrase for medication dosing entities of interest. It
#' is called within \code{\link{medExtractR}} and generally not intended for use outside
#' that function. The \code{phrase} argument containing text to search corresponds to an
#' individual mention of the drug of interest.
#'
#' @param phrase Text to search.
#' @param p_start Start position of phrase within original text.
#' @param p_stop End position of phrase within original text.
#' @param unit Unit of measurement for medication strength, e.g. \sQuote{mg}.
#' @param frequency_fun Function used to extract frequency.
#' @param intaketime_fun Function used to extract intake time.
#' @param duration_fun Function used to extract duration.
#' @param route_fun Function used to extract route.
#' @param strength_sep Delimiter for contiguous medication strengths.
#' @param \dots Parameter settings used in extracting frequency and intake time,
#' including additional arguments to the \code{<entity>_fun} arguments. Use \code{frequency_dict},
#' \code{intaketime_dict}, \code{duration_dict}, and \code{route_dict} to identify
#' custom frequency, intake time, duration, and route dictionaries, respectively.
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
#'
#' Note that extraction of the entities drug name, dose change, and time of last dose are not
#' handled by the \code{extract_entities} function. Those entities are extracted separately
#' and appended to the \code{extract_entities} output within the main \code{medExtractR} function.
#' Strength, dose amount, and dose strength are primarily numeric quantities, and are identified
#' using a combination of regular expressions and rule-based approaches. Frequency, intake time,
#' route, and duration, on the other hand, use dictionaries for identification.
#'
#' By default and when an argument \code{<entity>_fun} is \code{NULL}, the
#' \code{\link{extract_generic}} function will be used to extract that entity. This function
#' can also inherit user-defined entity dictionaries, supplied as arguments \code{<entity>_dict}
#' to \code{\link{medExtractR}} or \code{\link{medExtractR_tapering}} (see documentation files for main function(s) for details).
#'
#' The \code{stength_sep} argument is \code{NULL} by default, but can be used to
#' identify shorthand for morning and evening doses. For example, consider the
#' phrase \dQuote{Lamotrigine 300-200} (meaning 300 mg in the morning and 200 mg
#' in the evening). The argument \code{strength_sep = '-'} identifies
#' the full expression \emph{300-200} as \emph{dose strength} in this phrase.
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
#' Sample output for the phrase \dQuote{Lamotrigine 200mg bid} would look like:\cr
#' \tabular{rr}{
#'  entity   \tab  expr\cr
#'  IntakeTime  \tab  <NA>\cr
#'  Strength  \tab   <NA>\cr
#'  DoseAmt   \tab  <NA>\cr
#'  Route   \tab  <NA>\cr
#'  Duration   \tab  <NA>\cr
#'  Frequency \tab  bid;19:22\cr
#'  DoseStrength  \tab  200mg;13:18
#' }
#'
#' @export
#'
#' @examples
#' note <- "Lamotrigine 25 mg tablet - 3 tablets oral twice daily"
#' extract_entities(note, 1, nchar(note), "mg")
#' # A user-defined dictionary can be used instead of the default
#' my_dictionary <- data.frame(c("daily", "twice daily"))
#' extract_entities(note, 1, 53, "mg", frequency_dict = my_dictionary)

extract_entities <- function(phrase, p_start, p_stop, unit, frequency_fun = NULL,
                             intaketime_fun = NULL, duration_fun = NULL, route_fun = NULL,
                             strength_sep = NULL, ...){
  p_start <- p_start-1
  # censor date expressions
  phrase <- internal_censor_dates(phrase)

  ### DURATION ####
  df <- extract_type(phrase, 'duration', duration_fun, ...)
  duration <- entity_metadata(phrase, p_start, df)

  pfn <- internal_find_numbers(phrase,
    "\\.?\\d+(\\.\\d+)?",
    "\\s?(%|dose(s?)|hours|hrs|weeks|wks|days|years|yrs|a(m?)|p(m?))(?![a-zA-Z0-9])",
    c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
  )
  all_numbers <- pfn$all_numbers
  num_positions <- pfn$num_positions
  tn_expr <- pfn$tn_expr
  tn_pos <- pfn$tn_pos

  if(length(all_numbers) == 0) {
    strength <- NA;doseamt <- NA;dosestr <- NA
    remaining_numbers <- all_numbers
    num_pos <- num_positions
  } else { # only look for entities if they exist

    ### STRENGTH ####

    unit_len <- nchar(unit)
    phrase_lc <- tolower(phrase)
    # check if any numbers are followed by unit
    strength <- mapply(function(an, np){

      # Need narrow window in case same number appears both with and without unit
      r <- regexpr(paste0(an, "(\\s+)?", unit), substr(phrase_lc, np, np + nchar(an) + unit_len + 3))

      if(r == -1){c(NA)}else{
        stp <- np + attributes(r)$match.length

        paste(substr(phrase, np, stp-1), paste(np + p_start, stp + p_start, sep = ":"), sep = ";")
      }

    }, an = all_numbers, np = num_positions, USE.NAMES=FALSE)

    # Numeric expressions that still need to be examined
    remaining_numbers <- all_numbers[which(is.na(strength))]
    num_pos <- num_positions[which(is.na(strength))]

    # Keep only non-NA values
    strength <- strength[!is.na(strength)]
    if(length(strength)==0){strength <- NA}


    ### DOSEAMT ###

    if(length(remaining_numbers) == 0) {
      doseamt <- NA
    } else {
      doseamt <- mapply(function(rn, np){
        # substr is used to define narrow search windows
        # this helps prevent overlap between adjacent numeric values
        da <- regexpr(paste0(rn, "(?=(\\s+)?(\\(\\w*\\)\\s+)?tabs)"),
                      replace_tab(substr(phrase, np, np+15)), perl=T)

        # "take" notation
        if(da == -1){da_expr <- stringr::str_extract(substr(phrase, max(1, np-8), np+nchar(rn)),
                                            paste0("(?<=(take|takes|taking)(\\s?))", rn))
        if(!is.na(da_expr)){da <- regexpr(rn, substr(phrase, np, np+nchar(rn)))}}

        # parenthetical notation
        if(da == -1){da <- regexpr(paste0("(?<=[(])", rn, "(?=[)])"),
                                   substr(phrase, max(1, np-2), np+nchar(rn)+1), perl=T)}

        # number immediately after strength/dose mention, but not with another number immeiately after
        if(da == -1){da_expr <- regexpr(paste0("(?<=", unit, ")\\)?\\s", rn, "\\s(?!(\\d|hours?|hrs?))"),
                                        substr(phrase, max(1, np-8), np+nchar(rn)+7),perl=T)

        if(da_expr!=-1){da <- regexpr(rn, substr(phrase, np, np+nchar(rn)))}}

        if(da == -1){da <- NA}else{
          stp <- np + attributes(da)$match.length
          da <- paste(substr(phrase, np, stp-1), paste(np + p_start, stp + p_start, sep = ":"), sep = ";")
        }

        return(da)
      }, rn = remaining_numbers, np = num_pos, USE.NAMES=FALSE)

      # Numeric expressions that still need to be examined
      remaining_numbers <- remaining_numbers[which(is.na(doseamt))]
      num_pos <- num_pos[which(is.na(doseamt))]

      # Keep only non-NA values
      doseamt <- doseamt[!is.na(doseamt)]
      if(length(doseamt)==0){doseamt <- NA}
    }

    # text numbers for doseamt
    if(length(tn_expr) > 0) {
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

        if(da == -1){da <- NA}else{
          stp <- tnp + attributes(da)$match.length
          da <- paste(substr(phrase_lc, tnp, stp-1), paste(tnp + p_start, stp + p_start, sep = ":"), sep = ";")
        }

        return(da)
      }, tne = tn_expr, tnp = tn_pos)

      text_doseamt <- text_doseamt[!is.na(text_doseamt)]

      if(length(text_doseamt) > 0){
        if(any(is.na(doseamt))){doseamt <- text_doseamt}else{doseamt <- c(doseamt, text_doseamt)}
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

  ## BACK TO DOSE ##

  # This is for cases where we have drug_name # freq, and # is dose
  if(!exists("dosestr")){dosestr <- NA}
  if(!all(is.na(freq)) & length(remaining_numbers) > 0) {
    # start position from frequency
    freq_sp <- as.numeric(sub('[^;]*;([0-9]+):.*', '\\1', freq))

    # if format is drug_name rn freq, classify as dose
    dsc <- mapply(function(rn, np){
      # get position of this number relative to the whole note
      np_note_pos <- c(np + p_start, np + p_start + nchar(rn)) # start, stop

      if((np_note_pos[1] - p_stop <= 2) & # Allows for 0-1 separating characters
         any(freq_sp - np_note_pos[2] <= 2)){

        paste(substr(phrase, np, np+nchar(rn)-1), paste(np + p_start, np+nchar(rn) + p_start, sep = ":"), sep = ";")

      }else{NA}

    }, rn = remaining_numbers, np = num_pos, USE.NAMES=FALSE)

    # If anything found, remove from further examination
    if(sum(!is.na(dsc)) > 0){
      # Keep the ones with NA (not recognized as Dose)
      remaining_numbers <- remaining_numbers[is.na(dsc)]
      num_pos <- num_pos[is.na(dsc)]
    }

    dsc <- dsc[!is.na(dsc)]

    # Add to dose results
    if(length(dsc) > 0){
      if(all(is.na(dosestr))){dosestr <- dsc}else{dosestr <- c(dosestr, dsc)}
    }
  }

  ## BACK TO DOSEAMT ##
  ## Try to classify other numbers found among existing entities ##
  if(length(remaining_numbers) > 0){
    # Find last position of any found entities
    ent_list <- list(freq, intaketime, strength, doseamt, dosestr)
    last_pos_byent <- sapply(ent_list, function(x){
      y <- gsub(x, pattern = ".+;", replacement = "")
      max(as.numeric(gsub(y, pattern = ":.+", replacement = "")))
    })
    last_pos <- if(all(is.na(last_pos_byent))){NA}else{max(last_pos_byent, na.rm=T)}

    # If remaining number is in the range of other found entities, record it
    keep_num_id <- which(num_pos < (last_pos - p_start))

    if(length(keep_num_id) > 0){
      rnums <- remaining_numbers[keep_num_id]
      npos <- num_pos[keep_num_id]

      # probably not doseamt if over 10
      da_ids <- which(as.numeric(rnums) <= 10)
      if(length(da_ids)>0){
        add_das <- sapply(da_ids, function(i){
          stp <- npos[i] + nchar(rnums[i])

          paste(substr(phrase, npos[i], stp-1),
                paste(npos[i] + p_start, stp + p_start, sep = ":"), sep = ";")

        })

        # Numeric expressions that still need to be examined
        remaining_numbers <- remaining_numbers[!da_ids]
        num_pos <- num_pos[!da_ids]

        if(all(is.na(doseamt))){doseamt <- add_das}else{doseamt <- c(doseamt, add_das)}
      }

    }

  }

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

    in_paren <- grepl(paste0("(?<=[(])", expr, "((\\s\\w\\s)?tabs)?"),
                      replace_tab(substr(phrase, bp-1, ep+20)), perl=T)

    # doseamt occurs right before strength (allow <=1 in case number is in parentheses)
    after_da <- any(abs((bp+p_start-1) - as.numeric(sapply(doseamt, gsub,
                                                           pattern = ".+:",
                                                           replacement = ""))) <= 1)

    # strength has tablet after it, not necessarily in parentheses
    before_tab <- grepl(paste0(expr, "(\\s\\w\\s)?tabs"),
                        replace_tab(substr(phrase, bp-1, ep+20)), perl=T)


    return(in_paren | after_da | before_tab)
  })
  str_holdout <- strength[keep_str]

  if(all(is.na(doseamt))){ # If doseamt missing, reclassify strength as dose. works even if strength=NA
    if(all(is.na(dosestr))){dosestr <- strength}else{dosestr <- c(dosestr, strength)}
    strength <- NA
  } else {
    if(!all(is.na(strength))) {
      # If both are non-missing - get start positions
      da_sp <- as.numeric(stringr::str_extract(doseamt, "(?<=;).+(?=:)"))
      str_sp <- as.numeric(stringr::str_extract(strength, "(?<=;).+(?=:)"))

      df_sp <- data.frame(sp = c(da_sp, str_sp),
                          ent = c(rep("da", length(da_sp)), rep("str", length(str_sp))))
      df_sp <- df_sp[order(df_sp[,'sp']),]

      # entity indicators
      is_str <- which(df_sp$ent=="str")
      is_da <- which(df_sp$ent=="da")

      # if doseamt isn't after strength, should be dose
      is_dose <- sapply(is_str, function(i){
        !any(is_da == i + 1)
      })
      if(any(is_dose)) {
        dosestr <- strength[is_dose]
        strength <- ifelse(all(is_dose), NA, strength[!is_dose])
      }
    }
  }

  # Things that should not have been changed over
  if(length(str_holdout) > 0){
    # remove from dose
    switch_back <- dosestr %in% str_holdout
    dosestr <- dosestr[!switch_back]
    if(length(dosestr)==0){dosestr <- NA}

    # put back into strength
    if(all(is.na(strength))){
      strength <- str_holdout
    }else{
      strength <- c(strength, str_holdout)
    }

  }


  #### Building results ###

  # If no strength/dose was found, then set all values to NA (only want when associated dose info is present)
  if(all(sapply(list(strength, doseamt, dosestr, duration), function(x) all(is.na(x))))){
    return(data.frame("entity" = c("Frequency", "IntakeTime", "Strength", "DoseAmt", "DoseStrength"),
                      "expr" = rep(NA, 5)))
  }


  ent_res <- list("Frequency" = freq, "IntakeTime" = intaketime,
                  "Strength" = strength, "DoseAmt" = doseamt, "DoseStrength" = dosestr,
                  "Duration" = duration, "Route" = route)

  entities <- c("Frequency", "IntakeTime", "Strength", "DoseAmt", "DoseStrength", "Duration", "Route")

  lf <- sum(!is.na(freq))
  lit <- sum(!is.na(intaketime))
  lstr <- sum(!is.na(strength))
  lda <- sum(!is.na(doseamt))
  lds <- sum(!is.na(dosestr))
  ldur <- sum(!is.na(duration))
  lrt <- sum(!is.na(route))


  not_found <- entities[which(c(lf, lit, lstr, lda, lds, ldur, lrt) == 0)]
  found <- setdiff(entities, not_found)


  res_nf <- if(length(not_found) > 0){
    data.frame(entity = not_found, expr = rep(NA, length(not_found)))
  }

  res_f <- if(length(found) > 0){
    found_res <- subset(ent_res, names(ent_res) %in% found)
    do.call(rbind, lapply(seq(found_res), function(i){
      fr <- found_res[[i]]

      data.frame(entity = rep(found[i], length(fr)),
                 expr = fr)
    }))
  }

  if(is.null(res_nf)){
    res <- res_f
  }else{
    res <- rbind.data.frame(res_nf, res_f)
  }

  return(res)
}
