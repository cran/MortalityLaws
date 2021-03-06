% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/availableLF.R
\name{availableLF}
\alias{availableLF}
\title{Check Available Loss Function}
\usage{
availableLF()
}
\value{
A list of class \code{availableLF} with the components:
 \item{table}{Table with loss functions and codes to be used in \code{\link{MortalityLaw}}.}
 \item{legend}{Table with details about the abbreviation used.}
}
\description{
The function returns information about the implemented loss function used by the
optimization procedure in the \code{\link{MortalityLaw}} function.
}
\examples{
availableLF()
}
\seealso{
\code{\link{MortalityLaw}}
}
\author{
Marius D. Pascariu
}
