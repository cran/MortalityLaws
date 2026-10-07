## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(MortalityLaws)

## ----types--------------------------------------------------------------------
availableLaws()$legend

## ----type1, fig.asp = 0.6-----------------------------------------------------
x   <- 0:10
mx  <- ahmd$mx[paste(x), paste(2010)]
fit <- MortalityLaw(x = x, mx = mx, law = "scholey_shifted_power")
plot(fit, which = "fit")

## ----type2, fig.asp = 0.6-----------------------------------------------------
x   <- 10:35
mx  <- ahmd$mx[paste(x), paste(2010)]
fit <- MortalityLaw(x = x, mx = mx, law = "invgompertz")
plot(fit, which = "fit")

## ----type3, fig.asp = 0.6-----------------------------------------------------
x   <- 40:80
mx  <- ahmd$mx[paste(x), paste(2010)]
fit <- MortalityLaw(x = x, mx = mx, law = "gompertz")
plot(fit, which = "fit")

## ----type4, fig.asp = 0.6-----------------------------------------------------
x   <- 40:110
mx  <- ahmd$mx[paste(x), paste(2010)]
fit <- MortalityLaw(x = x, mx = mx, law = "ggompertz")
plot(fit, which = "fit")

## ----type5, fig.asp = 0.6-----------------------------------------------------
x   <- 60:110
mx  <- ahmd$mx[paste(x), paste(2010)]
fit <- MortalityLaw(x = x, mx = mx, law = "kannisto")
plot(fit, which = "fit")

## ----type6, fig.asp = 0.6-----------------------------------------------------
x   <- 0:100
mx  <- ahmd$mx[paste(x), paste(2010)]
fit <- MortalityLaw(x = x, mx = mx, law = "siler")
plot(fit, which = "fit")

## ----fit-gompertz-------------------------------------------------------------
year     <- 2010
ages     <- 45:90
deaths   <- ahmd$Dx[paste(ages), paste(year)]
exposure <- ahmd$Ex[paste(ages), paste(year)]

fit <- MortalityLaw(
  x          = ages,
  Dx         = deaths,
  Ex         = exposure,
  law        = "gompertz",
  opt.method = "poissonL"
)
summary(fit)

## ----availableLF--------------------------------------------------------------
availableLF()

## ----starting-values----------------------------------------------------------
gompertz(x = 45:90)$par

## ----fit-parS-----------------------------------------------------------------
fit_parS <- MortalityLaw(
  x          = ages,
  Dx         = deaths,
  Ex         = exposure,
  law        = "gompertz",
  opt.method = "poissonL",
  parS       = c(A = 0.001, B = 0.05)
)
rbind(default = coef(fit), parS = coef(fit_parS))

## ----fit-window---------------------------------------------------------------
fit_window <- MortalityLaw(
  x          = ages,
  Dx         = deaths,
  Ex         = exposure,
  law        = "gompertz",
  opt.method = "poissonL",
  fit.this.x = 60:90
)
range(fit_window$input$fit.this.x)
length(fit_window$fitted.values)

## ----gof-count----------------------------------------------------------------
fit$goodness.of.fit
fit$df

## ----gof-rate-----------------------------------------------------------------
fit_rate <- MortalityLaw(
  x          = ages,
  mx         = ahmd$mx[paste(ages), paste(year)],
  law        = "gompertz",
  opt.method = "LF2"
)
fit_rate$goodness.of.fit   # NaN: LF2 is a loss, not a likelihood
c(deviance = fit_rate$deviance, dispersion = fit_rate$dispersion)

## ----session------------------------------------------------------------------
sessionInfo()

