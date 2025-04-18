% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readHMD.R
\name{ReadHMD.core}
\alias{ReadHMD.core}
\title{Function to Download Data for a one Country}
\usage{
ReadHMD.core(what, country, interval, username, password, link)
}
\arguments{
\item{what}{What type of data are you looking for? The following options
might be available for some or all the countries and regions: \itemize{
  \item{\code{"births"}} -- birth records;
  \item{\code{"Dx_lexis"}} -- deaths by Lexis triangles;
  \item{\code{"Ex_lexis"}} -- exposure-to-risk by Lexis triangles;
  \item{\code{"population"}} -- population size;
  \item{\code{"Dx"}} -- death counts;
  \item{\code{"Ex"}} -- exposure-to-risk;
  \item{\code{"mx"}} -- central death-rates;
  \item{\code{"LT_f"}} -- period life tables for females;
  \item{\code{"LT_m"}} -- period life tables for males;
  \item{\code{"LT_t"}} -- period life tables both sexes combined;
  \item{\code{"e0"}} -- period life expectancy at birth;
  \item{\code{"Exc"}} -- cohort exposures;
  \item{\code{"mxc"}} -- cohort death-rates;
  \item{\code{"LT_fc"}} -- cohort life tables for females;
  \item{\code{"LT_mc"}} -- cohort life tables for males;
  \item{\code{"LT_tc"}} -- cohort life tables both sexes combined;
  \item{\code{"e0c"}} -- cohort life expectancy at birth;
  }}

\item{country}{HMD country code for the selected country. Character;}

\item{interval}{Datasets are given in various age and time formats based on
which the records are agregated. Interval options:
\itemize{
  \item{\code{"1x1"}} -- by age and year;
  \item{\code{"1x5"}} -- by age and 5-year time interval;
  \item{\code{"1x10"}} -- by age and 10-year time interval;
  \item{\code{"5x1"}} -- by 5-year age group and year;
  \item{\code{"5x5"}} -- by 5-year age group and 5-year time interval;
  \item{\code{"5x10"}} --by 5-year age group and 10-year time interval.
  }}

\item{username}{Your HMD username. If you don't have one you can sign up
for free on the Human Mortality Database website.}

\item{password}{Your HMD password.}

\item{link}{the main link to the database.}
}
\value{
A data.frame containing demographic data
}
\description{
Function to Download Data for a one Country
}
\keyword{internal}
