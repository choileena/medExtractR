% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string_suggestions.R
\name{string_suggestions}
\alias{string_suggestions}
\title{Find Strings and Suggest Misspellings}
\usage{
string_suggestions(strings, search_data, max_dist = 2, ignore.case = TRUE)
}
\arguments{
\item{strings}{character vector; value(s) to find}

\item{search_data}{character vector; phrase(s) where values may exist}

\item{max_dist}{numeric; edit distance to use for partial matches. The default value is 2.}

\item{ignore.case}{logical; indicates if spelling case matters, defaulting to \sQuote{TRUE}}
}
\value{
data.frame with two columns, \sQuote{suggestion} and \sQuote{match}
}
\description{
This function searches for text within one or more phrases, and looks
for partial matches. An exact match of the text should be found in order
for a suggestion to made.
}
\examples{
string_suggestions('penicillin', 'penicillan, penicillin, or penicilin?')
}
