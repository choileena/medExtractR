#' Extract Medication Entities From Phrase - Extension for Tapering application
#'
#' This function searches a phrase for medication dosing entities of interest. It
#' is called within \code{\link{medExtractR}} and generally not intended for use outside
#' that function.
#'
#' @param phrase Text to search.
#' @param p_start Start position of phrase within original text.
#' @param d_stop End position of drug name within original text.
#' @param unit Unit of measurement for medication strength, e.g. \sQuote{mg}.
#' @param frequency_fun Function used to extract frequency.
#' @param intaketime_fun Function used to extract intaketime.
#' @param duration_fun Function used to extract duration
#' @param route_fun Function used to extract route
#' @param doseschedule_fun Function used to extract doseschedule
#' @param preposition_fun Function used to extract preposition
#' @param timekeyword_fun Function used to extract timekeyword
#' @param transition_fun Function used to extract transition
#' @param dosechange_fun Function used to extract dosechange
#' @param strength_sep Delimiter for contiguous medication strengths.
#' @param \dots Parameter settings used in extracting frequency and intake time,
#' including additional arguments to \code{frequency_fun} and
#' \code{intaketime_fun}. Use \code{frequency_dict} to identify custom frequency
#' dictionaries and \code{intaketime_dict } to identify custom intake time
#' dictionaries. Similarly, for all other entities with a corresponding \code{<entity>_fun},
#' a custom dictionary can be supplied with the argument \code{<entity>_dict} .
#'
#' @details Various medication dosing entities are extracted within this function
#' including the following:
#'
#' \emph{strength}: The strength of an individual unit (i.e. tablet, capsule) of
#'   the drug.\cr
#' \emph{dose amount}: The number of tablets, capsules, etc taken with each dose.\cr
#' \emph{dose strength}: The total strength given intake. This quantity would be
#'   equivalent to strength x dose amount, and appears similar to strength when
#'   dose amount is absent.\cr
#' \emph{frequency}: The number of times per day a dose is taken, e.g.
#'   \dQuote{once daily} or \sQuote{2x/day}.\cr
#' \emph{intaketime}: The time period of the day during which a dose is taken,
#'   e.g. \sQuote{morning}, \sQuote{lunch}, \sQuote{in the pm}.\cr
#' \emph{duration}: How long a patient is on a drug regimen, e.g. \sQuote{2 weeks},
#'   \sQuote{mid-April}, \sQuote{another 3 days}.\cr
#' \emph{route}: The administration route of the drug, e.g., \sQuote{by mouth},
#'   \sQuote{IV}, \sQuote{topical}\cr
#' \emph{dose change}: Whether the dosage of the drug was changed, e.g.,
#'   \sQuote{increase}, \sQuote{adjust}, \sQuote{reduce}.
#' \emph{dose schedule}: Indicators of special dosing regimens, such as tapering
#'   schedules, alternating doses, or stopping keywords, e.g., \sQuote{weaning},
#'   \sQuote{even days} or \sQuote{odd_days}, \sQuote{discontinue}.
#' \emph{time keyword}: Whether the dosing regimen is a past dose, current dose,
#'   or future dose, e.g., \sQuote{currently}, \sQuote{remain}, \sQuote{yesterday}.
#' \emph{transition}: Words or symbols that link consecutive doses of a tapering
#'   regimen, e.g. \sQuote{then}, \sQuote{followed by}, or a comma \sQuote{,}.
#' \emph{preposition}: Prepositions that occur immediately next to another
#'   identified entity, e.g. \sQuote{to}, \sQuote{until}, \sQuote{for}.
#' \emph{dispense amount}: The number of pills prescribed to the patient.
#' \emph{refill}: The number of refills allowed for the patient's prescription.
#'
#' Strength, dose amount, dose strength, dispense amount, and refill are primarily numeric
#' quantities, and are identified using a combination of regular expressions and rule-based
#' approaches. All other entities use dictionaries for identification. For more information
#' about the default dictionary for a specific entity, view the documentation file for the
#' object \code{<entity>_vals}.
#'
#' By default and when an argument \code{<entity>_fun} is \code{NULL}, the
#' \code{\link{extract_generic}} function will be used for that entity. This function
#' can also inherit user-defined entity dictionaries, supplied as arguments to
#' \code{medExtractR} or \code{medExtractR_tapering}.
#'
#' Note that \code{extract_entities_tapering} has the argument \code{d_stop}. This differs
#' from \code{extract_entities}, which uses the end position of the full search window. This
#' is a consequence of \code{medExtractR} using a fixed search window length and \code{medExtractR_tapering}
#' dynamically constructing a search window.
#'
#' @return data.frame with entities information. At least one row per entity is returned,
#' using \code{NA} when no expression was found for a given entity.\cr
#' Sample output for the phrase \dQuote{Lamotrigine 200mg bid} would look like:\cr
#' \tabular{rr}{
#'  entity   \tab  expr\cr
#'  IntakeTime  \tab  <NA>\cr
#'  Strength  \tab   <NA>\cr
#'  DoseAmt   \tab  <NA>\cr
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

  phrase_orig <- phrase
  p_start <- p_start-1


  ### DURATION ####
  addl <- list(...)
  if(is.null(duration_fun) || as.character(substitute(duration_fun)) == "extract_generic") {
    dict <- addl[['duration_dict']]
    if(is.null(dict)) {
      e <- new.env()
      data("duration_vals", package = 'medExtractR', envir = e)
      dict <- get("duration_vals", envir = e)
    }
    df <- extract_generic(phrase, dict)
  } else {
    df <- duration_fun(phrase, ...)
  }

  duration <- medExtractR:::entity_metadata(phrase, p_start, df)

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

  # Common issues: censor the expressions "24/7" and "24 hr"
  phrase <- gsub("24/7", "XX/X", phrase)
  phrase <- gsub("24(\\s?[hr|hour])", "XX\\1", phrase)

  # Additional date pre-processing: censor various date formats to prevent them being identified as drug information
  phrase <- gsub("\\d{2}[-/\\.]\\d{2}[-/\\.]\\d{4}", "##-##-####", phrase) # 11/11/1111
  phrase <- gsub("\\d{2}[-/\\.]\\d{1}[-/\\.]\\d{4}", "##-#-####", phrase) # 11/1/1111
  phrase <- gsub("\\d{1}[-/\\.]\\d{2}[-/\\.]\\d{4}", "#-##-####", phrase) # 1/11/1111
  phrase <- gsub("\\d{1}[-/\\.]\\d{1}[-/\\.]\\d{4}", "#-#-####", phrase) # 1/1/1111

  phrase <- gsub("\\d{2}[-/\\.]\\d{2}[-/\\.]\\d{2}", "##-##-##", phrase) # 11/11/11
  phrase <- gsub("\\d{2}[-/\\.]\\d{1}[-/\\.]\\d{2}", "##-#-##", phrase) # 11/1/11
  phrase <- gsub("\\d{1}[-/\\.]\\d{2}[-/\\.]\\d{2}", "#-##-##", phrase) # 1/11/11
  phrase <- gsub("\\d{1}[-/\\.]\\d{1}[-/\\.]\\d{2}", "#-#-##", phrase) # 1/1/11

  # check is some month/day formats are dates. If so, censor it
  possible_dates <- unique(unlist(str_extract_all(phrase, "\\b\\d{1,2}[-/]\\d{1,2}\\b")))
  if(length(possible_dates)>0){
    censor <- sapply(possible_dates, function(x){
      y <- tryCatch(as.Date(paste0('2000', str_extract(x, "[-/]"), x)),
                    error = function(e) e)
      if(class(y)[1]=="Date"){T}else{F}
    }, USE.NAMES = F)
    possible_dates <- possible_dates[censor]
    for(pd in possible_dates){
      phrase <- gsub(pd, paste0(rep("X", nchar(pd)), collapse=''), phrase)
    }
  }

  # This could potentially be problematic (e.g., if a dose like 5/10 might be expected since it would be confused as a date)
  # censor partial dates (missing year)
  if(grepl("\\d{1,2}/\\d{1,2}", phrase)){
    # could turn this into a helper function
    month <- '((0?(1(?!\\d)|[2-9]))|1[0-2])'
    day <- '((0?([1-3](?!\\d)|[4-9]))|1[0-9]|2[0-9]|3[0-1])'
    slash_date <- str_extract_all(phrase, paste(month, day, sep="/"))[[1]]
    dash_date <- str_extract_all(phrase, paste(month, day, sep="-"))[[1]]
    dates <- c(slash_date, dash_date)

    if(length(dates)>0){
      for(i in 1:length(dates))
        phrase <- str_replace_all(phrase, dates[i], paste0(rep('X', nchar(dates[i])), collapse=""))
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
  all_numbers <- unlist(str_extract_all(phrase, "(?<!q)((\\d{1,3},\\d{3})|(\\.?\\d+))(\\.\\d+)?(?!(st|th))"))
  num_positions <- gregexpr("(?<!q)((\\d{1,3},\\d{3})|(\\.?\\d+))(\\.\\d+)?(?!(st|th))", phrase, perl=T)[[1]]
  # Don't allow 0 to be returned on its own
  num_positions <- num_positions[all_numbers != "0"]
  all_numbers <- all_numbers[all_numbers != "0"]

  # ignore isolated 0, "O2" for oxygen
  # ignore dosesequence numbers, percentages,
  if(length(all_numbers) > 0){
    iszero <- which(all_numbers=="0")
    if(length(iszero)>0){
      all_numbers <- all_numbers[-iszero]
      num_positions <- num_positions[-iszero]
    }
    maybeoxy <- which(all_numbers=="2")
    if(length(maybeoxy)>0){
      isoxy <- sapply(maybeoxy, function(x){
        grepl("(o|O)2",substr(phrase,num_positions[x]-1, num_positions[x]))
      })
      if(sum(isoxy>0)){
        all_numbers <- all_numbers[-maybeoxy[isoxy]]
        num_positions <- num_positions[-maybeoxy[isoxy]]
      }
    }

    ds_id <- mapply(function(an, np){

      grepl(paste0(an, "\\s?(%|dose(s?)|hours|hrs|weeks|wks|days|years|yrs|am|pm)(?![a-zA-Z0-9])"),
            substr(phrase, np, np+nchar(an)+7), ignore.case = T, perl=TRUE)
    }, an=all_numbers, np=num_positions)

    all_numbers <- all_numbers[!ds_id]
    num_positions <- num_positions[!ds_id]
  }

  # Only used for doseamt, unlikely that people would be taking too many pills at once
  t_num <- c("\\bone", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")

  text_numbers <- lapply(t_num, function(n){regexpr(n, phrase, ignore.case=TRUE)})
  inum <- which(unlist(text_numbers) != -1)
  if(length(inum) > 0){
    # which word expressions were found
    text_num <- t_num[inum]

    text_res <- lapply(text_num, function(n){gregexpr(n, phrase, ignore.case=TRUE)[[1]]})

    # extract from note
    tn_expr <- unlist(sapply(seq_along(text_res), function(i){
      rep(text_num[i], length(text_res[[i]]))
    }))
    tn_pos <- unlist(text_res)
  }else{
    tn_expr <- c()
    tn_pos <- c()
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
    strength <- NA;dosestr <- NA;disp <- NA;refill <- NA
    if(!exists("doseamt")){
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
        # debug: note R267591982_2012-10-24_243411888_1
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
                    medExtractR:::replace_tab(substr(phrase, np, np+15)), perl=T)

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
                      medExtractR:::replace_tab(substr(phrase_lc, tnp, tnp+15)), perl=T)

        # "take" notation
        if(da == -1){da_expr <- str_extract(substr(phrase_lc, max(1, tnp-8), tnp+nchar(tne)),
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
      }, tne = tn_expr, tnp = tn_pos)

      tn_expr <- tn_expr[is.na(text_doseamt)]
      tn_pos <- tn_pos[is.na(text_doseamt)]

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

  addl <- list(...)
  if(is.null(frequency_fun) || as.character(substitute(frequency_fun)) == "extract_generic") {
    dict <- addl[['frequency_dict']]
    if(is.null(dict)) {
      e <- new.env()
      data("frequency_vals", package = 'medExtractR', envir = e)
      dict <- get("frequency_vals", envir = e)
    }
    df <- extract_generic(phrase, dict)
  } else {
    df <- frequency_fun(phrase, ...)
  }

  freq <- medExtractR:::entity_metadata(phrase, p_start, df)

  ### INTAKETIME ###

  if(is.null(intaketime_fun) || as.character(substitute(intaketime_fun)) == "extract_generic") {
    dict <- addl[['intaketime_dict']]
    if(is.null(dict)) {
      e <- new.env()
      data("intaketime_vals", package = 'medExtractR', envir = e)
      dict <- get("intaketime_vals", envir = e)
    }
    df <- extract_generic(phrase, dict)
  } else {
    df <- intaketime_fun(phrase, ...)
  }

  intaketime <- medExtractR:::entity_metadata(phrase, p_start, df)

  ### ROUTE ####

  addl <- list(...)
  if(is.null(route_fun) || as.character(substitute(route_fun)) == "extract_generic") {
    dict <- addl[['route_dict']]
    if(is.null(dict)) {
      e <- new.env()
      data("route_vals", package = 'medExtractR', envir = e)
      dict <- get("route_vals", envir = e)
    }
    df <- extract_generic(phrase, dict)
  } else {
    df <- route_fun(phrase, ...)
  }

  route <- medExtractR:::entity_metadata(phrase, p_start, df)


  ## Check for any remaining numbers to occur immediately before freq, intaketime, or route - if yes, classify as doseamt
  if(length(remaining_numbers)>0){
    rem_num <- if(length(tn_expr)>0){c(remaining_numbers,
                                       gsub("\\b","",tn_expr, fixed=TRUE))}else{remaining_numbers}
    rm_num_pos <- if(length(tn_expr)>0){c(num_pos, tn_pos)}else{num_pos}
    is_txt <- c(rep(FALSE, length(remaining_numbers)), rep(TRUE, length(tn_expr)))

    is_txt <- is_txt[order(rm_num_pos)]
    rem_num <- rem_num[order(rm_num_pos)]
    rm_num_pos <- rm_num_pos[order(rm_num_pos)]
  }else{
    rem_num <- tn_expr
    rm_num_pos <- tn_pos
    is_txt <- length(tn_expr)
  }

  if(length(rem_num)>0 & (all(!is.na(freq))|all(!is.na(intaketime)))){

    freq_it <- if(all(!is.na(freq))){freq}else{NULL}
    freq_it <- if(all(!is.na(intaketime))){c(freq_it, intaketime)}else{freq_it}
    freq_it <- if(all(!is.na(route))){c(freq_it, route)}else{freq_it}

    # Start positions of any extracted freq/intake time
    freq_it_startpos <- as.numeric(gsub(".+;", "", gsub(":.+", "", freq_it)))

    # Stop positions of any remaining numbers
    np_stoppos <- rm_num_pos + nchar(gsub("\\b","",rem_num,fixed=TRUE)) + p_start

    is_da_ds <- sapply(np_stoppos, function(np){
      any(freq_it_startpos - np == 0 | freq_it_startpos - np == 1)
    })

    if(sum(is_da_ds)>0){
      ix <- which(is_da_ds)
      num_expr <- suppressWarnings(as.numeric(rem_num[ix]))
      # Consider dose amount if small (or text - typically only smaller numbers are written as text)
      # Consider as dosestrength is larger
      ix_da <- ix[is.na(num_expr)|num_expr<=10]
      ix_ds <- ix[!is.na(num_expr) & num_expr > 10]

      # Reclassify doseamt cases
      if(all(is.na(doseamt)) & length(ix_da) > 0){
        doseamt <- paste(rem_num[ix_da],
                         paste(rm_num_pos[ix_da] + p_start,
                               rm_num_pos[ix_da] +
                                 nchar(gsub("\\b","",rem_num[ix_da],fixed=TRUE)) +
                                 p_start,
                               sep=":"), sep=";")
      }else if(!all(is.na(doseamt)) & length(ix_da) > 0){
        doseamt <- c(doseamt,
                     paste(rem_num[ix_da],
                           paste(rm_num_pos[ix_da] + p_start,
                                 rm_num_pos[ix_da] +
                                   nchar(gsub("\\b","",rem_num[ix_da],fixed=TRUE)) +
                                   p_start,
                                 sep=":"), sep=";"))
      }

      # Reclassify dosestrength cases
      if(all(is.na(dosestr)) & length(ix_ds) > 0){
        dosestr <- paste(rem_num[ix_ds],
                         paste(rm_num_pos[ix_ds] + p_start,
                               rm_num_pos[ix_ds] + nchar(rem_num[ix_ds]) + p_start,
                               sep=":"), sep=";")
      }else if(!all(is.na(dosestr)) & length(ix_ds) > 0){
        dosestr <- c(dosestr,
                     paste(rem_num[ix_ds],
                           paste(rm_num_pos[ix_ds] + p_start,
                                 rm_num_pos[ix_ds] + nchar(rem_num[ix_ds]) + p_start,
                                 sep=":"), sep=";"))
      }

      remaining_numbers <- remaining_numbers[-ix]
      num_pos <- num_pos[-ix]

    }


  }


  ### doseschedule ####
  addl <- list(...)
  if(is.null(doseschedule_fun) || as.character(substitute(doseschedule_fun)) == "extract_generic") {
    dict <- addl[['doseschedule_dict']]
    if(is.null(dict)) {
      e <- new.env()
      data("doseschedule_vals", package = 'medExtractR', envir = e)
      dict <- get("doseschedule_vals", envir = e)
    }
    df <- extract_generic(phrase, dict)
  } else {
    df <- doseschedule_fun(phrase, ...)
  }

  doseschedule <- medExtractR:::entity_metadata(phrase, p_start, df)

  ### preposition ####
  addl <- list(...)
  if(is.null(preposition_fun) || as.character(substitute(preposition_fun)) == "extract_generic") {
    dict <- addl[['preposition_dict']]
    if(is.null(dict)) {
      e <- new.env()
      data("preposition_vals", package = 'medExtractR', envir = e)
      dict <- get("preposition_vals", envir = e)
    }
    df <- extract_generic(phrase, dict)
  } else {
    df <- preposition_fun(phrase, ...)
  }

  preposition <- medExtractR:::entity_metadata(phrase, p_start, df)

  ### timekeyword ####
  addl <- list(...)
  if(is.null(timekeyword_fun) || as.character(substitute(timekeyword_fun)) == "extract_generic") {
    dict <- addl[['timekeyword_dict']]
    if(is.null(dict)) {
      e <- new.env()
      data("timekeyword_vals", package = 'medExtractR', envir = e)
      dict <- get("timekeyword_vals", envir = e)
    }
    df <- extract_generic(phrase, dict)
  } else {
    df <- timekeyword_fun(phrase, ...)
  }

  timekeyword <- medExtractR:::entity_metadata(phrase, p_start, df)

  ### transition ####
  addl <- list(...)
  if(is.null(transition_fun) || as.character(substitute(transition_fun)) == "extract_generic") {
    dict <- addl[['transition_dict']]
    if(is.null(dict)) {
      e <- new.env()
      data("transition_vals", package = 'medExtractR', envir = e)
      dict <- get("transition_vals", envir = e)
    }
    df <- extract_generic(phrase, dict)
  } else {
    df <- transition_fun(phrase, ...)
  }

  transition <- medExtractR:::entity_metadata(phrase, p_start, df)

  ### dosechange ####
  addl <- list(...)
  if(is.null(dosechange_fun) || as.character(substitute(dosechange_fun)) == "extract_generic") {
    dict <- addl[['dosechange_dict']]
    if(is.null(dict)) {
      e <- new.env()
      data("dosechange_vals", package = 'medExtractR', envir = e)
      dict <- get("dosechange_vals", envir = e)
    }
    df <- extract_generic(phrase, dict)
  } else {
    df <- dosechange_fun(phrase, ...)
  }

  dosechange <- medExtractR:::entity_metadata(phrase, p_start, df)


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
                        medExtractR:::replace_tab(substr(phrase, bp-1, ep+20)), perl=T)

    return(after_da | before_tab)
  })
  keep_str[which(is.na(keep_str))] <- FALSE

  str_holdout <- strength[keep_str]

  # Anything not marked as keep_str gets converted to dosestr
  if(sum(!keep_str) > 0){
    if(all(is.na(dosestr))){
      dosestr <- strength[!keep_str]
    }else{
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
      da_sp <- as.numeric(str_extract(doseamt, "(?<=;).+(?=:)"))
      str_sp <- as.numeric(str_extract(strength, "(?<=;).+(?=:)"))

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

  # If no strength/dose was found, then set all values to NA (only want when associated dose info is present)
  if(length(dosestr)==0){dosestr <- NA}
  if(all(sapply(list(strength, doseamt, dosestr, duration), function(x) all(is.na(x))))){
    if(!any(tolower(gsub(";.+", "", doseschedule)) %in% c("done", "off", "stop", "last", "completed",
                                                          "complete", "discontinue", "discontinuing",
                                                          "finished", "dose pack", "dosepack"))){
      return(data.frame("entity" = c("Frequency", "IntakeTime", "Strength", "DoseAmt", "DoseStrength"),
                        "expr" = rep(NA, 5)))
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
  res <- unique(res)
  res <- subset(res, !is.na(expr))


  ## !! RESTRICT EXTRACTED ENTITIES - only consider distances between dose-critical entities (str/dose, doseamt, duration)
  res$pos = gsub(".+;", "", res$expr)
  res$start = as.numeric(gsub(":.+", "", res$pos))
  res$stop = as.numeric(gsub(".+:", "", res$pos))
  res <- res[order(res$start, res$stop),]
  res <- subset(res, !is.na(entity))

  # If expressions overlap, keep the longest match
  if(nrow(res)>1){
    start_match <- c((res$start[1:(nrow(res)-1)] == res$start[2:nrow(res)]),FALSE)
    stop_match <- c(FALSE, (res$stop[2:nrow(res)] == res$stop[1:(nrow(res)-1)]))
    dup <- c(start_match | stop_match)
    res <- res[!dup,]
  }


  res$rn <- 1:nrow(res)
  res1 <- subset(res, !(entity %in% c("Preposition", "Transition")))

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
        res <- subset(res, start < d_stop)
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
    if(!any(tolower(gsub(";.+", "", doseschedule)) %in% c("done", "off", "stop", "last", "completed",
                                                          "complete", "discontinue", "discontinuing",
                                                          "finished", "dose pack", "dosepack"))){
      return(data.frame("entity" = c("Frequency", "IntakeTime", "Strength", "DoseAmt", "DoseStrength"),
                        "expr" = rep(NA, 5)))
    }
  }

  res <- res[,c("entity", "expr")]

  return(res)
}


