% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MortalityLaw_models.R
\name{kannisto_makeham}
\alias{kannisto_makeham}
\title{Kannisto-Makeham Mortality Law - 1998}
\usage{
kannisto_makeham(x, par = NULL)
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
Kannisto-Makeham Mortality Law - 1998
}
\examples{
kannisto_makeham(x = 85:120)
}
\keyword{internal}
