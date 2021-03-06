% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frequency_vals.R
\docType{data}
\name{frequency_vals}
\alias{frequency_vals}
\title{Keywords Specifying Frequency}
\format{
A data frame with frequency expressions (exact and/or regular expressions).
\describe{
  \item{expr}{A character vector, expressions to consider as frequency.}
  \item{value}{A numeric vector, numeric value of frequency represented as number of doses
  taken per day. For example, \dQuote{bid} and \dQuote{twice a day} would both have a numeric value of 2.}
}
}
\usage{
frequency_vals
}
\description{
A dictionary mapping frequency expressions to numeric values representing
the corresponding number of doses per day. Example expressions include
"q12 hours", "bid", "daily", and "three times a day". The form of each frequency is
given as a regular expression.
}
\examples{
data(frequency_vals)
}
\keyword{datasets}
