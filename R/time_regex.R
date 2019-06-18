#' Keywords Specifying Time Expressions
#'
#' A vector of regular expressions to identify different forms of time
#' expressions for last dose time.
#'
#' @format A vector with 5 regular expressions for the following categories.
#' \describe{
#'   \item{am/pm}{Time is indicated by the presence of \dQuote{am} or
#'     \dQuote{pm} following a numeric expression.}
#'   \item{military}{Time is given in military time, for unambiguous times of
#'     13:00-23:59.}
#'   \item{qualifier_after}{Am/pm indication is implicit through a qualifying
#'     term like \dQuote{last night} or \dQuote{this morning}. The qualifier
#'     occurs after the time, e.g. \dQuote{10 last night.}}
#'   \item{qualifier_before}{Am/pm indication is implicit through a qualifying
#'     term like \dQuote{last night} or \dQuote{this morning}. The qualifier
#'     occurs before the time, e.g. \dQuote{last night at 10.}}
#'   \item{duration}{Time (in hours) between the last dose and most recent lab
#'     value}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(time_regex)
"time_regex"
