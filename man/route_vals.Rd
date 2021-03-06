% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/route_vals.R
\docType{data}
\name{route_vals}
\alias{route_vals}
\title{Keywords Specifying Route}
\format{
A data frame with route expressions (exact and/or regular expressions).
\describe{
  \item{expr}{A character vector, expressions to consider as route.}
  \item{value}{A standardized version of the raw expression. For example,
  "orally" and "by mouth" both have the standardized form "orally".}
}
}
\usage{
route_vals
}
\description{
A dictionary mapping route expressions to standardized forms, specifying the
way in which a medication is administered. Example expressions include "oral",
"topical", "IV", and "intravenous".
}
\examples{
data(route_vals)
}
\keyword{datasets}
