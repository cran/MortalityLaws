% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MortalityLaw_models.R
\name{thiele}
\alias{thiele}
\title{Thiele Mortality Law - 1871}
\usage{
thiele(x, par = NULL)
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
Thiele Mortality Law - 1871
}
\examples{
thiele(x = 0:100)
}
\keyword{internal}
