#' Keywords Specifying Dose Schedule
#'
#' A dictionary with words for indicating a tapering dosing schedule. These can explicitly
#' refer to such a schedule with phrases like "tapering" or "wean". It also includes words indicating
#' an alternating dose schedule (e.g., "alternate", "alt.", "even days", or "odd days") as well as
#' stopping keywords indicating the patient is going completely off the medication (e.g., "done", "gone",
#' "stop", "discontinue").
#'
#' @format A data frame with dose schedule expressions (exact and/or regular expressions).
#' \describe{
#'   \item{expr}{A character vector, expressions to consider as dose schedule.}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(doseschedule_vals)
"doseschedule_vals"
