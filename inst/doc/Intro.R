## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(MortalityLaws)

## -----------------------------------------------------------------------------
library(MortalityLaws)
data(ahmd)

## ----ReadHMD, eval=FALSE------------------------------------------------------
# HMD_Dx <- ReadHMD(
#   what      = "Dx",
#   countries = "SWE",
#   interval  = "1x1",
#   username  = "user@email.com",
#   password  = "password",
#   save      = FALSE
# )

## ----RegionalReaders, eval=FALSE----------------------------------------------
# JMD_LT <- ReadJMD(       # Japanese prefectures: female life tables
#   what     = "LT_f",
#   regions  = c("Aichi", "Tokyo"),
#   interval = "1x1",
#   save     = FALSE
# )
# 
# CHMD_mx <- ReadCHMD(     # Canadian regions: death rates
#   what     = "mx",
#   regions  = "CAN",
#   interval = "1x1",
#   save     = FALSE
# )
# 
# AHMD_Ex <- ReadAHMD(     # Australian states: exposures
#   what     = "Ex",
#   regions  = c("NSW", "VIC"),
#   interval = "1x1",
#   save     = FALSE
# )

## ----availableHMD, eval=FALSE-------------------------------------------------
# availableHMD()

## -----------------------------------------------------------------------------
names(HMD_sample)

## -----------------------------------------------------------------------------
year     <- 1950
ages     <- 0:100
deaths   <- ahmd$Dx[paste(ages), paste(year)]
exposure <- ahmd$Ex[paste(ages), paste(year)]

fit <- MortalityLaw(
  x          = ages,
  Dx         = deaths,
  Ex         = exposure,
  law        = "HP",
  opt.method = "LF2"
)

## -----------------------------------------------------------------------------
summary(fit)

## -----------------------------------------------------------------------------
plot(fit)

## -----------------------------------------------------------------------------
fit.subset <- MortalityLaw(
  x          = ages,
  Dx         = deaths,
  Ex         = exposure,
  law        = "HP",
  opt.method = "LF2",
  fit.this.x = 0:65
)
plot(fit.subset)

## -----------------------------------------------------------------------------
A <- availableLaws()$table
A[as.logical(A$SCALE_X), c("NAME", "CODE")]

## -----------------------------------------------------------------------------
ages.makeham <- 40:90
fit.makeham  <- MortalityLaw(
  x          = ages.makeham,
  Dx         = ahmd$Dx[paste(ages.makeham), "2010"],
  Ex         = ahmd$Ex[paste(ages.makeham), "2010"],
  law        = "makeham",
  opt.method = "LF2"
)
p <- coef(fit.makeham)
p

## -----------------------------------------------------------------------------
x_scaled <- ages.makeham - min(ages.makeham) + 1
by_hand  <- p["A"] * exp(p["B"] * x_scaled) + p["C"]
max(abs(by_hand - fitted(fit.makeham)))

by_hand_unscaled <- p["A"] * exp(p["B"] * ages.makeham) + p["C"]
max(abs(by_hand_unscaled - fitted(fit.makeham)))

## -----------------------------------------------------------------------------
missov <- function(x, par = c(b = 0.13, M = 45)) {
  hx <- with(as.list(par), b * exp(b * (x - M)))
  return(as.list(environment()))   # must return a list
}

## -----------------------------------------------------------------------------
year     <- 1950
ages     <- 45:85
deaths   <- ahmd$Dx[paste(ages), paste(year)]
exposure <- ahmd$Ex[paste(ages), paste(year)]

my_model <- MortalityLaw(
  x          = ages,
  Dx         = deaths,
  Ex         = exposure,
  custom.law = missov
)

## -----------------------------------------------------------------------------
summary(my_model)

## ----warning = FALSE, message = FALSE-----------------------------------------
lt <- LawTable(x = 0:100, par = fit$coefficients, law = "HP")
head(lt$lt)

## -----------------------------------------------------------------------------
citation(package = "MortalityLaws")

## -----------------------------------------------------------------------------
sessionInfo()

