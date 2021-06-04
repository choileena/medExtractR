library(medExtractR)

t1 <- paste(scan('testnote1.txt', '', sep = '\n', quiet = TRUE), collapse = '\n')
alt_freq_dict <- data.frame(expr = 'every\\s?[0-9]+\\s?hours')
alt_fun <- function(phrase) data.frame(pos = 52, expr_len = 13)

# typical use cases for mxr
expect_equal(as.numeric(sub(':.*', '', medExtractR(t1, 'prograf', 100, "mg", 1, lastdose = TRUE)[,'pos'])), c(3, 11, 20, 31, 40, 62))
expect_equal(as.numeric(sub(':.*', '', medExtractR(t1, c('clonidine', 'catapres'), 100, "mg", 1)[,'pos'])), c(175, 185, 197, 207, 216, 225))
expect_equal(as.numeric(sub(':.*', '', medExtractR(t1, c('zithromax', 'zithromax z-pak'), 100, "mg", 1)[,'pos'])), c(529, 545, 556, 565, 574, 584))
expect_equal(as.numeric(sub(':.*', '', medExtractR(t1, c('furosemide', 'lasix'), 100, "mg", 1)[,'pos'])), c(427, 438, 449, 456, 465, 474))
expect_equal(as.numeric(sub(':.*', '', medExtractR(t1, 'biokera', 100, "mg")[,'pos'])), c(782,790,792))
expect_equal(as.numeric(sub(':.*', '', medExtractR(t1, 'zincoderm', 100, "mg")[,'pos'])), c(798,808,818,826))
expect_equal(as.numeric(sub(':.*', '', medExtractR(t1, 'lecozotan', 100, "mg", strength_sep = '-')[,'pos'])), c(841,851))

# typical use cases for mxr_t
## "last" (52) probably shouldn't be extracted as doseschedule
expect_equal(as.numeric(sub(':.*', '', medExtractR_tapering(t1, 'prograf', "mg", 1, lastdose = TRUE)[,'pos'])), c(3, 11, 20, 31, 40, 52, 62))
expect_equal(as.numeric(sub(':.*', '', medExtractR_tapering(t1, c('clonidine', 'catapres'), "mg", 1)[,'pos'])), c(175, 185, 197, 207, 216, 225))
expect_equal(as.numeric(sub(':.*', '', medExtractR_tapering(t1, c('zithromax', 'zithromax z-pak'), "mg", 1)[,'pos'])), c(529, 545, 556, 565, 574, 580, 584))
expect_equal(as.numeric(sub(':.*', '', medExtractR_tapering(t1, c('furosemide', 'lasix'), "mg", 1)[,'pos'])), c(427, 438, 449, 456, 465, 474))
expect_equal(as.numeric(sub(':.*', '', medExtractR_tapering(t1, 'biokera', "mg")[,'pos'])), c(782,790,792))
expect_equal(as.numeric(sub(':.*', '', medExtractR_tapering(t1, 'zincoderm', "mg")[,'pos'])), c(798,808,826))
expect_equal(as.numeric(sub(':.*', '', medExtractR_tapering(t1, 'lecozotan', "mg", strength_sep = '-')[,'pos'])), c(841,851))

# try alternate dictionary or extraction function
expect_equal(as.numeric(sub(':.*', '', medExtractR(t1, 'acetaminophen', 100, "mg", 1, frequency_dict = alt_freq_dict)[,'pos'])), c(303, 317, 324, 335, 344, 353))
# note last entity is off by one (354v353); side-effect of position adjustment with hard-coded value
expect_equal(as.numeric(sub(':.*', '', medExtractR(t1, 'acetaminophen', 100, "mg", 1, frequency_fun = alt_fun)[,'pos'])), c(303, 317, 324, 335, 344, 354))
expect_equal(as.numeric(sub(':.*', '', medExtractR_tapering(t1, 'acetaminophen', "mg", 1, frequency_dict = alt_freq_dict)[,'pos'])), c(303, 317, 324, 335, 344, 353))
expect_equal(as.numeric(sub(':.*', '', medExtractR_tapering(t1, 'acetaminophen', "mg", 1, frequency_fun = alt_fun)[,'pos'])), c(303, 317, 324, 335, 344, 353))

# drug with no dose info
expect_true(is.na(medExtractR(t1, 'hydrocodone', 100, "mg")))
expect_true(is.na(medExtractR_tapering(t1, 'hydrocodone', "mg")[1,'pos']))

# try a miss-spelling
v <- medExtractR(t1, 'metformin', 100, "mg", 2)
expect_equal(v[1,'expr'], 'metfromin')
v <- medExtractR_tapering(t1, 'metformin', "mg", 2)
expect_equal(v[1,'expr'], 'metfromin')
expect_equal(as.numeric(sub(':.*', '', v[,'pos'])), c(606,616,628,640,650,654,663,668,676,682,703,706))

# using expanded druglist should reduce window length & false positives
data("rxnorm_druglist", package="medExtractR")
data("addl_expr", package="medExtractR")
dl <- unique(tolower(c(rxnorm_druglist, addl_expr[,'expr'])))
expect_equal(as.numeric(sub(':.*', '', medExtractR_tapering(t1, 'advil', "mg")[,'pos'])), c(729, 762))
expect_true(is.na(medExtractR_tapering(t1, 'advil', "mg", drug_list = dl)[1,'expr']))
expect_true(is.na(medExtractR(t1, 'advil', 100, "mg", drug_list = dl)))

# debugonce(extract_entities_tapering)
## duplicate freq/dur & doseamt/dur?
# medExtractR(t1, 'metformin', 100, "mg", 2)
## why is doseamt missing?
# medExtractR_tapering(t1, 'zincoderm', "mg") # 5 times

t2 <- paste(scan('testnote2.txt', '', sep = '\n', quiet = TRUE), collapse = '\n')
expect_equal(as.numeric(sub(':.*', '', medExtractR(t2, c('lamotrigine','lamictal'), 100, "mg", 1)[,'pos'])), c(253,266,275,282,286))
