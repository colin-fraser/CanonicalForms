% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/canonical_forms.R
\name{extract_canonical_form}
\alias{extract_canonical_form}
\title{Create a CanonicalForm from a data frame}
\usage{
extract_canonical_form(x, transformers = list(), checks = list())
}
\arguments{
\item{x}{dataframe-like}

\item{transformers}{a list of transformers}

\item{checks}{list of checks}
}
\value{
a canonical form
}
\description{
Create a CanonicalForm from a data frame
}
\examples{
extract_canonical_form(cars)
}
