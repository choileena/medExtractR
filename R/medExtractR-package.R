#' Medication Extraction With R
#'
#' It provides a function \code{\link{medExtractR}} for extracting
#' dose attributes for medications within a given EHR note.
#'
#' @docType package
#' @aliases medExtractR-package
#'
#' @author Hannah Weeks \email{hannah.l.weeks@@Vanderbilt.Edu},\cr
#' Leena Choi \email{leena.choi@@Vanderbilt.Edu},\cr
#' Cole Beck \email{cole.beck@@vumc.org}
#'
#' Maintainer: Cole Beck \email{cole.beck@@vumc.org}
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom stringr str_extract str_extract_all str_replace_all
#' @importFrom rlang .data
#' @importFrom utils aregexec data
#'
#' @examples
#' note1 <- "Lamotrigine 25 mg tablet - 3 tablets oral twice daily"
#' note2 <- "Lamotrigine 25 mg tablet - 3 tablets oral twice daily
#' lamotrigine 50 mg cap before bedtime
#' follow up in three months
#' LTG 10 mg tablet - 2 tab daily"
#' note3 <- "Adderall - 30 mg tablet 1 tablet by mouth daily"
#' data(rxnorm_druglist)
#' medExtractR(note1, c("lamotrigine", "ltg"), rxnorm_druglist, 130, 1, "mg")
#' medExtractR(note2, c("lamotrigine", "ltg"), rxnorm_druglist, 130, 1, "mg")
#' medExtractR(note3, c("lamotrigine", "ltg"), rxnorm_druglist, 130, 1, "mg")
"_PACKAGE"
