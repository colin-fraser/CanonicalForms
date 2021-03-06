% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{check_between}
\alias{check_between}
\title{Check that values are within a set of bounds}
\usage{
check_between(..., .strict_lower = FALSE, .strict_upper = FALSE)
}
\arguments{
\item{...}{named arguments where each name is a column, and each value is a length-2 numeric
vector of the form c(lower_bound, upper_bound)}

\item{.strict_lower}{should the lower bound be strict?}

\item{.strict_upper}{should the upper bound be strict?}
}
\value{
a CheckResult object
}
\description{
Check that values are within a set of bounds
}
\examples{
df <- data.frame(a = c(1, 2, 3, NA), b = c(4, 5, 6, 7))
cb1 <- check_between(a = c(1, 4), b = c(3, 7))
cb1(df) # passes
cb2 <- check_between(a = c(1, 4), b = c(3, 7), .strict_lower = TRUE)
cb2(df) # fails
cb3 <- check_between(a = c(1, 4), b = c(3, 7), .strict_upper = TRUE)
cb3(df) # fails
cb4 <- check_between(a = c(1, 4), b = c(3, 7), .strict_lower = TRUE, .strict_upper = TRUE)
cb4(df) # fails
}
