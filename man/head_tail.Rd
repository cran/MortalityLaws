% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{head_tail}
\alias{head_tail}
\title{Summary function - display head and tail in a single data.frame
The original code for this function was first written for 'psych' R package
here we have modified it a bit}
\usage{
head_tail(x, hlength = 4, tlength = 4, digits = 4, ellipsis = TRUE)
}
\arguments{
\item{x}{A matrix or data frame or free text}

\item{hlength}{The number of lines at the beginning to show}

\item{tlength}{The number of lines at the end to show}

\item{digits}{Round off the data to digits}

\item{ellipsis}{separate the head and tail with dots}
}
\value{
Print table's head and tail
}
\description{
Summary function - display head and tail in a single data.frame
The original code for this function was first written for 'psych' R package
here we have modified it a bit
}
\keyword{internal}
