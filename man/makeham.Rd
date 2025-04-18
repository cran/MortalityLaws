% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MortalityLaw_models.R
\name{makeham}
\alias{makeham}
\title{Makeham Mortality Law - 1860}
\usage{
makeham(x, par = NULL)
}
\arguments{
\item{x}{vector of age at the beginning of the age classes}

\item{par}{parameters of the selected model. If NULL the
default values will be assigned automatically.}
}
\value{
A list of rates and model parameters
}
\description{
Makeham Mortality Law - 1860
}
\examples{
makeham(x = 45:90)
}
\keyword{internal}
