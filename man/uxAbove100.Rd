% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LifeTable.R
\name{uxAbove100}
\alias{uxAbove100}
\title{Educate mx or qx on how to behave above age 100 if it gets in trouble
(with NA's, zero's and Inf)}
\usage{
uxAbove100(x, ux, omega = 100, verbose = FALSE)
}
\arguments{
\item{x}{Vector of ages at the beginning of the age interval.}

\item{ux}{A vector of mx or qx.}

\item{omega}{Threshold age. Default: 100.}

\item{verbose}{A logical value. Set \code{verbose = FALSE} to silent
the process that take place inside the function and avoid progress messages.}
}
\value{
A vector of rates
}
\description{
Educate mx or qx on how to behave above age 100 if it gets in trouble
(with NA's, zero's and Inf)
}
\keyword{internal}
