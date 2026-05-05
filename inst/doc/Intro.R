## ----setup, include=FALSE-----------------------------------------------------
library(MortalityLaws)
library(knitr)
opts_chunk$set(collapse = TRUE)

## ----LoadPackages, message=FALSE----------------------------------------------
library(MortalityLaws)

## ----ReadHMD, eval=FALSE------------------------------------------------------
# # Download Swedish death counts (ages 0–110, years 1751–2014)
# HMD_Dx <- ReadHMD(
#   what      = "Dx",
#   countries = "SWE",
#   interval  = "1x1",
#   username  = "user@email.com",
#   password  = "password",
#   save      = FALSE
# )

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
ls(fit)   # components of the fitted object

## -----------------------------------------------------------------------------
summary(fit)

## ----fig.align='center', out.width='80%', fig.width=9-------------------------
plot(fit)

## ----fig.align='center', out.width='80%', fig.width=9-------------------------
fit.subset <- MortalityLaw(
  x          = ages,
  Dx         = deaths,
  Ex         = exposure,
  law        = "HP",
  opt.method = "LF2",
  fit.this.x = 0:65
)
plot(fit.subset)

## ----eval=FALSE---------------------------------------------------------------
# # Fit with automatic age scaling
# fit_scaled <- MortalityLaw(
#   x          = ages,
#   Dx         = deaths,
#   Ex         = exposure,
#   law        = "gompertz",
#   scale.age  = TRUE
# )

## ----eval=FALSE, warning=FALSE------------------------------------------------
# availableLaws()

## ----message=FALSE, warning=FALSE---------------------------------------------
availableLF()

## -----------------------------------------------------------------------------
my_gompertz <- function(x, par = c(b = 0.13, M = 45)){
  hx <- with(as.list(par), b * exp(b * (x - M)))
  return(as.list(environment()))   # must return a list
}

## -----------------------------------------------------------------------------
year     <- 1950
ages     <- 45:85
deaths   <- ahmd$Dx[paste(ages), paste(year)]
exposure <- ahmd$Ex[paste(ages), paste(year)]

## ----warning=FALSE, results='hide'--------------------------------------------
my_model <- MortalityLaw(
  x          = ages,
  Dx         = deaths,
  Ex         = exposure,
  custom.law = my_gompertz
)

## -----------------------------------------------------------------------------
summary(my_model)

## -----------------------------------------------------------------------------
plot(my_model)

## -----------------------------------------------------------------------------
y  <- 1900
x  <- as.numeric(rownames(ahmd$mx))
Dx <- ahmd$Dx[, paste(y)]
Ex <- ahmd$Ex[, paste(y)]

LT1 <- LifeTable(x, Dx = Dx, Ex = Ex)   # primary input
LT2 <- LifeTable(x, mx = LT1$lt$mx)     # from mx
LT3 <- LifeTable(x, qx = LT1$lt$qx)     # from qx
LT4 <- LifeTable(x, lx = LT1$lt$lx)     # from lx
LT5 <- LifeTable(x, dx = LT1$lt$dx)     # from dx

LT1

## -----------------------------------------------------------------------------
ls(LT1)   # components of the life table object

## -----------------------------------------------------------------------------
x  <- c(0, 1, seq(5, 110, by = 5))
mx <- c(.053, .005, .001, .0012, .0018, .002, .003, .004, 
       .004, .005, .006, .0093, .0129, .019, .031, .049, 
       .084, .129, .180, .2354, .3085, .390, .478, .551)

lt <- LifeTable(x, mx = mx, sex = "female")

## -----------------------------------------------------------------------------
lt

