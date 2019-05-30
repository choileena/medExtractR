#' Extract Entities From Phrase
#'
#' This function searches a phrase for expressions of interest.
#'
#' @param phrase Text to search.
#' @param p_start Start position of phrase within original text.
#' @param p_stop End position of phrase within original text.
#' @param unit Unit of measurement for medication strength.
#' @param freq_fun Function used to extract frequency.
#' @param intaketime_fun Function used to extract intaketime.
#' @param strength_sep Delimiter for contiguous medication strengths.
#' @param \dots Parameter settings that will be used in extracting
#' frequency and intake time.
#'
#' @return data.frame with entity information
#' @export

extract_entities <- function(phrase, p_start, p_stop, unit, freq_fun = NULL, intaketime_fun = NULL, strength_sep = NULL, ...){
  p_start <- p_start-1

  # A bit of pre-processing: ##-##-(## or ####) often represents a date, but can get picked up as DoseSC
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

  # Replace am/pm time expressions?
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

  # should I also add in something to remove time expressions?
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

  if(length(all_numbers) == 0){
    strength <- NA;doseamt <- NA;dosesc <- NA
    remaining_numbers <- all_numbers
    num_pos <- num_positions
  }else{ # only look for entities if they exist


    ### STRENGTH ####

    # check if any numbers are followed by unit
    strength <- mapply(function(an, np){

      # Need narrow window in case same number appears both with and without unit
      r <- regexpr(paste0(an, "(\\s+)?", unit), tolower(substr(phrase, np, np+nchar(an)+nchar(unit)+3)))

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

    if(length(remaining_numbers) == 0){doseamt <- NA}else{
      doseamt <- mapply(function(rn, np){
        # substr is used to define narrow search windows
        # this helps prevent overlap between adjacent numeric values
        da <- regexpr(paste0(rn, "(?=(\\s+)?(\\(\\w*\\)\\s+)?tabs)"),
                      replace_tab(substr(phrase, np, np+15)), perl=T)

        # "take" notation
        if(da == -1){da_expr <- str_extract(substr(phrase, max(1, np-8), np+nchar(rn)),
                                            paste0("(?<=(take|takes|taking)(\\s?))", rn))
        # can't get regexpr to run with this notation - issue with lookbehind not of fixed width but str_extract above does fine
        if(!is.na(da_expr)){da <- regexpr(rn, substr(phrase, np, np+nchar(rn)))}}

        # parenthetical notation
        if(da == -1){da <- regexpr(paste0("(?<=[(])", rn, "(?=[)])"),
                                   substr(phrase, max(1, np-2), np+nchar(rn)+1), perl=T)}

        # number immediately after strength/dosesc mention, but not with another number immeiately after
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
        # can't get regexpr to run with this notation - issue with lookbehind not of fixed width but str_extract above does fine
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


    ## DOSESC ##

    if(!is.null(strength_sep)){
      if(length(remaining_numbers) > 0){
        # Cases where times of doses are denoted as ##-##

        # NOT separated with : because it could be a time
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


        # Add to dosesc results
        if(length(dsc_index) > 0){
          dsc_split <- remaining_numbers[dsc_index]
          dsc_split_pos <- num_pos[dsc_index]

          dsc <- paste(dsc_split, paste(dsc_split_pos+ p_start,
                                        dsc_split_pos+nchar(dsc_split)+ p_start, sep=":"), sep=";")

          num_pos <- num_pos[setdiff(1:length(remaining_numbers), dsc_index)]
          remaining_numbers <- remaining_numbers[setdiff(1:length(remaining_numbers), dsc_index)]


          #if(all(is.na(dosesc))){dosesc <- dsc}else{dosesc <- c(dosesc, dsc)}
          dosesc <- dsc
        }else{dosesc <- NA}
      }else{dosesc <- NA}
    }else{dosesc <- NA}

  }
  # non-numeric entities

#   df <- do.call(rbind.data.frame, apply(freq_dict, MARGIN=1, function(row){
#     r1 <- row[1];r2 <- row[2]
#     fq <- gregexpr(paste0(r1, "\\b"), phrase, ignore.case=T, perl=T)[[1]]
#
#     fq_len <- attributes(fq)$match.length
#     stp <- fq+fq_len
#
#     data.frame("expr" = sapply(1:length(fq), function(i){substr(phrase, fq[i], stp[i]-1)}),
#                "start_stop" = paste(fq + p_start, stp + p_start, sep=":"),
#                "pos" = fq,
#                "expr_len" = fq_len,
#                "freq" = as.numeric(r2))
#   }))
#   df$expr <- as.character(df$expr)
#   df$start_stop <- as.character(df$start_stop)
#
#   df <- subset(df, pos != -1)
#
#   # No freq matches
#   if(nrow(df) == 0){freq <- NA}
#
#   # If some expressions overlap, it keeps the longest one
#   # Happens when one expression is a subset of another and starts at the same place
#   freq_df <- df %>% dplyr::group_by(pos) %>% dplyr::filter(expr_len == max(expr_len)) %>% dplyr::ungroup() %>%
#     dplyr::arrange(pos) %>% dplyr::mutate(end_prev = dplyr::lag(pos) + dplyr::lag(expr_len)) %>%
#     dplyr::filter(is.na(end_prev) | pos > end_prev) %>% dplyr::select(-end_prev)
#
#   freq <- paste(freq_df$expr, freq_df$start_stop, sep = ";")
#
#   # If no results found, make NA
#   freq <- if(length(freq)==0){NA}else{freq}

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
  # df should be matrix with "pos" and "expr_len"
  freq <- entity_metadata(phrase, p_start, df)
  # format should be expr|start_stop|pos|expr_len

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
  # df should be matrix with "pos" and "expr_len"
  intaketime <- entity_metadata(phrase, p_start, df)

  ## BACK TO DOSESC ##

  # This is for cases where we have drug_name # freq, and # is dosesc
  if(!all(is.na(freq)) & length(remaining_numbers) > 0){
    freq_sp <- dplyr::pull(.data$freq_df %>% dplyr::mutate(sp = as.numeric(sub(":.+", "", .data$start_stop))), .data$sp)

    # if format is drug_name rn freq, classify as dosesc
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
      # Keep the ones with NA (not recognized as DoseSC)
      remaining_numbers <- remaining_numbers[is.na(dsc)]
      num_pos <- num_pos[is.na(dsc)]
    }


    dsc <- dsc[!is.na(dsc)]

    # Add to dosesc results
    if(length(dsc) > 0){
      if(all(is.na(dosesc))){dosesc <- dsc}else{dosesc <- c(dosesc, dsc)}
    }
  }



  ## BACK TO DOSEAMT ##
  ## Try to classify other numbers found among existing entities ##
  #print(num_pos)
  if(length(remaining_numbers) > 0){
    # Find last position of any found entities
    ent_list <- list(freq, intaketime, strength, doseamt, dosesc)
    #print(ent_list)
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







  ## ONE FINAL LOOK AT DOSESC ##
  # Reclassify strength as dosesc when necessary
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

  if(all(is.na(doseamt))){ # If doseamt missing, reclassify strength as dosesc. works even if strength=NA
    if(all(is.na(dosesc))){dosesc <- strength}else{dosesc <- c(dosesc, strength)}
    strength <- NA
  }else{
    if(!all(is.na(strength))){
      # If both are non-missing - get start positions
      da_sp <- as.numeric(str_extract(doseamt, "(?<=;).+(?=:)"))
      str_sp <- as.numeric(str_extract(strength, "(?<=;).+(?=:)"))

      df_sp <- data.frame(sp = c(da_sp, str_sp),
                          ent = c(rep("da", length(da_sp)), rep("str", length(str_sp)))) %>% dplyr::arrange(.data$sp)

      # entity indicators
      is_str <- which(df_sp$ent=="str")
      is_da <- which(df_sp$ent=="da")

      # if doseamt isn't after strength, should be dosesc
      is_dosesc <- sapply(is_str, function(i){
        !any(is_da == i + 1)
      })
      if(any(is_dosesc)){
        dosesc <- strength[is_dosesc]
        strength <- ifelse(all(is_dosesc), NA, strength[!is_dosesc])
      }
    }
  }

  # Things that should not have been changed over
  if(length(str_holdout) > 0){
    # remove from dosesc
    switch_back <- dosesc %in% str_holdout
    dosesc <- dosesc[!switch_back]
    if(length(dosesc)==0){dosesc <- NA}

    # put back into strength
    if(all(is.na(strength))){
      strength <- str_holdout
    }else{
      strength <- c(strength, str_holdout)
    }

  }


  #### Building results ###

  # If no strength/dosesc was found, then set all values to NA (only want when associated dose info is present)
  if(all(sapply(list(strength, doseamt, dosesc), function(x) all(is.na(x))))){
    return(data.frame("entity" = c("freq", "strength", "doseamt", "dosesc"),
                      "expr" = rep(NA, 4)))
  }


  ent_res <- list("Frequency" = freq, "IntakeTime" = intaketime,
                  "Strength" = strength, "DoseAmt" = doseamt, "DoseSC" = dosesc)

  entities <- c("Frequency", "IntakeTime", "Strength", "DoseAmt", "DoseSC")

  lf <- sum(!is.na(freq))
  lit <- sum(!is.na(intaketime))
  lstr <- sum(!is.na(strength))
  lda <- sum(!is.na(doseamt))
  ldsc <- sum(!is.na(dosesc))

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




