#' Medication Extraction With R
#'
#' Provides a function \code{\link{medExtractR}} for extracting
#' dose attributes for medications within a given electronic health record (EHR) note.
#'
#' @docType package
#' @aliases medExtractR-package
#'
#' @author Hannah Weeks \email{hannah.l.weeks@@vanderbilt.edu},\cr
#' Cole Beck \email{cole.beck@@vumc.org},\cr
#' Leena Choi \email{leena.choi@@vumc.org}
#'
#' Maintainer: Leena Choi \email{leena.choi@@vumc.org}
#'
#' @importFrom stringr str_extract str_extract_all str_replace_all
#' @importFrom stringi stri_locate_all_regex
#' @importFrom utils adist aregexec data
#'
#' @examples
#' \donttest{
#' note1 <- "Progrf Oral Capsule 1 mg 3 capsules by mouth twice a day - last
#' dose at 10pm"
#' note2 <- "Currently on lamotrigine 150-200, but will increase to lamotrigine 200mg bid"
#' medExtractR(note1, c("prograf", "tacrolimus"), 60, "mg", 2, lastdose=TRUE)
#' medExtractR(note2, c("lamotrigine", "ltg"), 130, "mg", 1, strength_sep = "-")
#' }
"_PACKAGE"
