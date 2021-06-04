#' Keywords Specifying Time Expressions
#'
#' A vector of regular expressions to identify different forms of time
#' expressions for last dose time. These are the default values used in \code{link{extract_lastdose}}.
#'
#' Certain expressions which might be considered ambiguous are excluded from
#' the regular expressions presented here. For instance, expressions such as
#' \sQuote{600} could refer to either 6am or 6pm.
#'
#' @format A vector with 5 regular expressions for the following categories.
#' \describe{
#'   \item{am/pm}{Time is indicated by the presence of \sQuote{am} or
#'     \sQuote{pm} following a numeric expression.}
#'   \item{military}{Time is given in military time, for unambiguous times of
#'     13:00-23:59.}
#'   \item{qualifier_after}{Am/pm indication is implicit through a qualifying
#'     term like \sQuote{last night} or \sQuote{this morning}. The qualifier
#'     occurs after the time, e.g., \sQuote{10 last night.}}
#'   \item{qualifier_before}{Am/pm indication is implicit through a qualifying
#'     term like \sQuote{last night} or \sQuote{this morning}. The qualifier
#'     occurs before the time, e.g., \sQuote{last night at 10.}}
#'   \item{duration}{Time (in hours) between the last dose and most recent lab
#'     value}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' data(time_regex)
"time_regex"
