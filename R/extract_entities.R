#' Extract Medication Entities From Phrase
#'
#' This function searches a phrase for medication dosing entities of interest. It
#' is called within \code{\link{medExtractR}} and generally not intended for use outside
#' that function.
#'
#' @param phrase Text to search.
#' @param p_start Start position of phrase within original text.
#' @param p_stop End position of phrase within original text.
#' @param unit Unit of measurement for medication strength, e.g. \sQuote{mg}.
#' @param freq_fun Function used to extract frequency.
#' @param intaketime_fun Function used to extract intaketime.
#' @param strength_sep Delimiter for contiguous medication strengths.
#' @param \dots Parameter settings used in extracting frequency and intake time,
#' including additional arguments to \code{freq_fun} and
#' \code{intaketime_fun}. Use \code{freq_dict} to identify custom frequency
#' dictionaries and \code{intaketime_dict } to identify custom intake time
#' dictionaries.
#'
#' @details Various medication dosing entities are extracted within this function
#' including the following:
#'
#' \emph{strength}: The strength of an individual unit (i.e. tablet, capsule) of
#'   the drug.\cr
#' \emph{dose amount}: The number of tablets, capsules, etc taken with each dose.\cr
#' \emph{dose}: The total strength given intake. This quantity would be
#'   equivalent to strength x dose amount, and appears similar to strength when
#'   dose amount is absent.\cr
#' \emph{frequency}: The number of times per day a dose is taken, e.g.
#'   \dQuote{once daily} or \sQuote{2x/day}.\cr
#' \emph{intaketime}: The time period of the day during which a dose is taken,
#'   e.g. \sQuote{morning}, \sQuote{lunch}, \sQuote{in the pm}.\cr
#'
#' Strength, dose amount, and dose are primarily numeric quantities, and are
#' identified using a combination of regular expressions and rule-based
#' approaches. Frequency and intake time, on the other hand, use dictionaries
#' for identification.
#'
#' By default and when \code{freq_fun} and/or \code{intaketime_fun} are \code{NULL}, the
#' \code{\link{extract_generic}} function will be used for these entities.
#'
#' The \code{stength_sep} argument is \code{NULL} by default, but can be used to
#' identify shorthand for morning and evening doses. For example, consider the
#' phrase \dQuote{Lamotrigine 300-200} (meaning 300 mg in the morning and 200 mg
#' in the evening). The argument \code{strength_sep = '-'} can identify both
#' \emph{300} and \emph{200} as \emph{dose} in this phrase.
#'
#' @return data.frame with entities information. At least one row per entity is returned,
#' using \code{NA} when no expression was found for a given entity.\cr
#' Sample output for the phrase \dQuote{Lamotrigine 200mg bid} would look like:\cr
#' \tabular{rr}{
#'  entity   \tab  expr\cr
#'  IntakeTime  \tab  <NA>\cr
#'    Strength  \tab   <NA>\cr
#'     DoseAmt   \tab  <NA>\cr
#'   Frequency \tab  bid;19:22\cr
#'        Dose  \tab  200mg;13:18
#' }
#'

#'
#' @export
#'
#' @examples
#' note <- "Lamotrigine 25 mg tablet - 3 tablets oral twice daily"
#' extract_entities(note, 1, nchar(note), "mg")
#' # A user-defined dictionary can be used instead of the default
#' my_dictionary <- data.frame(c("daily", "twice daily"))
#' extract_entities(note, 1, 53, "mg", freq_dict = my_dictionary)

extract_entities <- function(phrase, p_start, p_stop, unit, freq_fun = NULL,
                               intaketime_fun = NULL, strength_sep = NULL, ...){
  p_start <- p_start-1

  # A bit of pre-processing: ##-##-(## or ####) often represents a date, but can get picked up as Dose
  # Replace ##-##-## with XX-XX-XX to prevent these from being captured
  if(grepl("\\d{2}-\\d{2}-\\d{2,4}", phrase)){
    # Do longer one first or else you end up with XX-XX-XX##
    if(grepl("\\d{2}-\\d{2}-\\d{4}", phrase)){
      phrase <- str_replace_all(phrase, "\\d{2}-\\d{2}-\\d{4}","XX-XX-XXXX")
    }
    if(grepl("\\d{2}-\\d{2}-\\d{2}", phrase)){
      phrase <- str_replace_all(phrase, "\\d{2}-\\d{2}-\\d{2}","XX-XX-XX")
    }
  }

  time_present <- gregexpr("\\d[^a-zA-Z]+\\s?(?=((am)|(pm))\\b)",
                           phrase, perl=T, ignore.case=T)[[1]]
  if(any(time_present != -1)){
    for(j in seq_along(time_present)){
      tpi <- time_present[j]
      tpli <- attributes(time_present)$match.length[j]

      replace_time <- gsub("\\d", "X", substr(phrase, tpi, tpi+tpli))
      phrase <- paste0(substr(phrase, 1, tpi-1), replace_time,
                       substr(phrase, tpi+tpli+1, nchar(phrase)))
  }}

  # Numbers in phrase
  all_numbers <- unlist(str_extract_all(phrase, "(?<!\\d|q)(\\.)?\\d+(\\.\\d+)?(?!\\d)"))
  num_positions <- gregexpr("(?<!\\d|q)(\\.)?\\d+(\\.?)(\\d+)?(?!\\d)", phrase, perl=T)[[1]]

  # ignore dosesequence numbers
  if(length(all_numbers) > 0){
    ds_id <- mapply(function(an, np){
        grepl(paste0(an, "\\s?(weeks|wks|days|a(m?)|p(m?))\\b"), substr(phrase, np, np+nchar(an)+7), ignore.case = T)
      }, an=all_numbers, np=num_positions)
    all_numbers <- all_numbers[!ds_id]
    num_positions <- num_positions[!ds_id]
  }

  # Only used for doseamt, unlikely that people would be taking too many pills at once
  t_num <- c("one", "two", "three", "four", "five")

  text_numbers <- lapply(t_num, function(n){regexpr(n, phrase)})
  inum <- which(unlist(text_numbers) != -1)
  if(length(inum) > 0){
    # which word expressions were found
    text_num <- t_num[inum]

    text_res <- lapply(text_num, function(n){gregexpr(n, phrase)[[1]]})

    # extract from note
    tn_expr <- unlist(sapply(seq_along(text_res), function(i){
      rep(text_num[i], length(text_res[[i]]))
    }))
    tn_pos <- unlist(text_res)
  }

  # Ignore dates
  date_yrs <- c(paste0(197, 0:9), paste0(198, 0:9), paste0(199, 0:9),
                paste0(200, 0:9), paste0(201,0:8))

  is_year <- which(all_numbers %in% date_yrs)
  if(length(is_year)==0){is_year <- 0}
  if(any(is_year>1)){

    # If the preceeding numeric value is only separated by one space, will assume it's a date
    rm_year <- unlist(lapply(is_year, function(iy){
      x <- grepl(paste0("(?<=", all_numbers[iy - 1], ")\\s(?=", all_numbers[iy], ")"), phrase, perl=T)
      if(x){c(iy-1, iy)}else{iy}
    }))
    keep_nums <- setdiff(seq_along(all_numbers), rm_year)

    num_positions <- num_positions[keep_nums]
    all_numbers <- all_numbers[keep_nums]
  }

  if(length(all_numbers) == 0) {
    strength <- NA;doseamt <- NA;dose <- NA
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
        if(da == -1){da_expr <- str_extract(substr(phrase, max(1, np-8), np+nchar(rn)),
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
    if(exists("tn_expr")){
      text_doseamt <- mapply(function(tne, tnp){
        # tablet notation
        da <- regexpr(paste0(tne, "(?=(\\s+)?(\\(\\w*\\)\\s+)?tabs)"),
                      replace_tab(substr(phrase, tnp, tnp+15)), perl=T)

        # "take" notation
        if(da == -1){da_expr <- str_extract(substr(phrase, max(1, tnp-8), tnp+nchar(tne)),
                                            paste0("(?<=(take|takes|taking)(\\s?))", tne))
        if(!is.na(da_expr)){da <- regexpr(tne, substr(phrase, tnp, tnp+nchar(tne)))}}

        # parenthetical notation
        if(da == -1){da <- regexpr(paste0("(?<=[(])", tne, "(?=[)])"),
                                   substr(phrase, max(1, tnp-2), tnp+nchar(tne)+1), perl=T)}

        if(da == -1){da <- NA}else{
          stp <- tnp + attributes(da)$match.length
          da <- paste(substr(phrase, tnp, stp-1), paste(tnp + p_start, stp + p_start, sep = ":"), sep = ";")
        }

        return(da)
      }, tne = tn_expr, tnp = tn_pos)

      text_doseamt <- text_doseamt[!is.na(text_doseamt)]

      if(length(text_doseamt) > 0){
        if(any(is.na(doseamt))){doseamt <- text_doseamt}else{doseamt <- c(doseamt, text_doseamt)}
      }
    }

    ## DOSE ##

    dose <- NA
    if(!is.null(strength_sep)) {
      if(length(remaining_numbers) > 0) {
        # Cases where times of doses are denoted as ##-##

        num_end <- num_pos + nchar(remaining_numbers)

        # Check if there is only distance of 1 between end of one word and start of the next
        maybe_dsc <- which(num_pos[-1]-num_end[-length(num_pos)] == 1)
        is_dsc <- sapply(maybe_dsc, function(j){
          substr(phrase, num_end[j], num_pos[j+1]-1)
        }) %in% strength_sep
        dsc_index <- c(maybe_dsc[c(is_dsc)], maybe_dsc[c(is_dsc)]+1)

        is_time <- which(sapply(remaining_numbers, function(rn){
          regexpr(paste(rn, "\\s?(weeks|hours|hrs)"), phrase)
        }) != -1)
        if(any(is_time %in% dsc_index)){
          # Ignore ones that are actually time ranges
          dsc_index <- dsc_index[-c(is_time-1, is_time)]
        }

        # Add to dose results
        if(length(dsc_index) > 0) {
          dsc_split <- remaining_numbers[dsc_index]
          dsc_split_pos <- num_pos[dsc_index]

          dsc <- paste(dsc_split, paste(dsc_split_pos+ p_start,
                                        dsc_split_pos+nchar(dsc_split)+ p_start, sep=":"), sep=";")

          num_pos <- num_pos[setdiff(1:length(remaining_numbers), dsc_index)]
          remaining_numbers <- remaining_numbers[setdiff(1:length(remaining_numbers), dsc_index)]


          dose <- dsc
        }
      }
    }

  }

  # non-numeric entities

  ### FREQ ####

  addl <- list(...)
  if(is.null(freq_fun) || as.character(substitute(freq_fun)) == "extract_generic") {
    dict <- addl[['freq_dict']]
    if(is.null(dict)) {
      e <- new.env()
      data("freq_vals", envir = e)
      dict <- get("freq_vals", envir = e)
    }
    df <- extract_generic(phrase, dict)
  } else {
    df <- freq_fun(phrase, ...)
  }

  freq <- entity_metadata(phrase, p_start, df)

  ### INTAKETIME ###

  if(is.null(intaketime_fun) || as.character(substitute(intaketime_fun)) == "extract_generic") {
    dict <- addl[['intaketime_dict']]
    if(is.null(dict)) {
      e <- new.env()
      data("intaketime_vals", envir = e)
      dict <- get("intaketime_vals", envir = e)
    }
    df <- extract_generic(phrase, dict)
  } else {
    df <- intaketime_fun(phrase, ...)
  }

  intaketime <- entity_metadata(phrase, p_start, df)

  ## BACK TO DOSE ##

  # This is for cases where we have drug_name # freq, and # is dose
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
      if(all(is.na(dose))){dose <- dsc}else{dose <- c(dose, dsc)}
    }
  }

  ## BACK TO DOSEAMT ##
  ## Try to classify other numbers found among existing entities ##
  #print(num_pos)
  if(length(remaining_numbers) > 0){
    # Find last position of any found entities
    ent_list <- list(freq, intaketime, strength, doseamt, dose)
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
      da_ids <- which(as.numeric(rnums) < 11)
      if(length(da_ids)>0){
        add_das <- sapply(da_ids, function(i){
          stp <- npos[i] + length(rnums[i])

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
    if(all(is.na(dose))){dose <- strength}else{dose <- c(dose, strength)}
    strength <- NA
  } else {
    if(!all(is.na(strength))) {
      # If both are non-missing - get start positions
      da_sp <- as.numeric(str_extract(doseamt, "(?<=;).+(?=:)"))
      str_sp <- as.numeric(str_extract(strength, "(?<=;).+(?=:)"))

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
        dose <- strength[is_dose]
        strength <- ifelse(all(is_dose), NA, strength[!is_dose])
      }
    }
  }

  # Things that should not have been changed over
  if(length(str_holdout) > 0){
    # remove from dose
    switch_back <- dose %in% str_holdout
    dose <- dose[!switch_back]
    if(length(dose)==0){dose <- NA}

    # put back into strength
    if(all(is.na(strength))){
      strength <- str_holdout
    }else{
      strength <- c(strength, str_holdout)
    }

  }


  #### Building results ###

  # If no strength/dose was found, then set all values to NA (only want when associated dose info is present)
  if(all(sapply(list(strength, doseamt, dose), function(x) all(is.na(x))))){
    return(data.frame("entity" = c("Frequency", "IntakeTime", "Strength", "DoseAmt", "Dose"),
                      "expr" = rep(NA, 5)))
  }


  ent_res <- list("Frequency" = freq, "IntakeTime" = intaketime,
                  "Strength" = strength, "DoseAmt" = doseamt, "Dose" = dose)

  entities <- c("Frequency", "IntakeTime", "Strength", "DoseAmt", "Dose")

  lf <- sum(!is.na(freq))
  lit <- sum(!is.na(intaketime))
  lstr <- sum(!is.na(strength))
  lda <- sum(!is.na(doseamt))
  ldsc <- sum(!is.na(dose))

  not_found <- entities[which(c(lf, lit, lstr, lda, ldsc) == 0)]
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




