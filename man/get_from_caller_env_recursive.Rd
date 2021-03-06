% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{get_from_caller_env_recursive}
\alias{get_from_caller_env_recursive}
\alias{get_property}
\alias{run_in_cf_env}
\title{Low-level accessors for getting properties}
\usage{
get_from_caller_env_recursive(nm, i = 1, recursion_limit = 100)

get_property(nm, cf = NULL)

run_in_cf_env(cf, f, ...)
}
\arguments{
\item{nm}{the name of the object to get}

\item{i}{the number of levels up to climb in the call stack. This should usually
not be set manually, but is incremented recursively by the function.}

\item{recursion_limit}{a limit that probably shouldn't be used, except for testing}

\item{cf}{a CanonicalForm object}

\item{f}{function to execute}

\item{...}{arguments to pass to f}
}
\value{
the property to get
}
\description{
\code{get_from_caller_env_recursive(nm)} gets the object called
\code{nm} from the call stack. \code{get_property} is a friendlier
api for specifically getting properties out of CanonicalForm environments.
}
\section{Functions}{
\itemize{
\item \code{get_property}: get a property from a CanonicalForm

\item \code{run_in_cf_env}: run function in CanonicalForm environment
}}

\examples{
e1 <- new.env()
e1$a <- "a"
do.call(CanonicalForms:::get_from_caller_env_recursive, list(nm = "a"), envir = e1)

cf <- extract_canonical_form(cars)
get_property(".col_names", cf)
f <- function() get_property(".col_names")
do.call(f, list(), envir = CanonicalForms:::get_check_env(cf))
CanonicalForms:::run_in_cf_env(cf, get_property, nm = ".col_names")
}
