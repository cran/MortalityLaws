% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MortalityLaw_models.R
\name{kostaki}
\alias{kostaki}
\title{Kostaki Model - 1992}
\usage{
kostaki(x, par = NULL)
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
Kostaki Model - 1992
}
\examples{
kostaki(x = 0:100)
}
\keyword{internal}
