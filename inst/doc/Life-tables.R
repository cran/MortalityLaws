## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(MortalityLaws)

## ----lt-counts----------------------------------------------------------------
x  <- 0:105
Dx <- ahmd$Dx[as.character(x), "1900"]
Ex <- ahmd$Ex[as.character(x), "1900"]

lt_counts <- LifeTable(x = x, Dx = Dx, Ex = Ex)
head(lt_counts$lt, 4)

## ----lt-rates-----------------------------------------------------------------
mx <- ahmd$mx[as.character(x), "1900"]

lt_rates <- LifeTable(x = x, mx = mx)
head(lt_rates$lt, 4)

## ----six-inputs---------------------------------------------------------------
e0 <- c(
  DxEx = lt_counts$lt$ex[1],
  mx   = LifeTable(x = x, mx = lt_counts$lt$mx)$lt$ex[1],
  qx   = LifeTable(x = x, qx = lt_counts$lt$qx)$lt$ex[1],
  lx   = LifeTable(x = x, lx = lt_counts$lt$lx)$lt$ex[1],
  dx   = LifeTable(x = x, dx = lt_counts$lt$dx)$lt$ex[1],
  ex   = LifeTable(x = x, ex = lt_counts$lt$ex)$lt$ex[1]
)
e0

## ----bridge-------------------------------------------------------------------
lt <- lt_rates$lt
q_exact <- (1 * lt$mx) / (1 + (1 - lt$ax) * lt$mx)
q_cfm   <- 1 - exp(-1 * lt$mx)

round(data.frame(
  age     = lt$x[1:3],
  mx      = lt$mx[1:3],
  ax      = lt$ax[1:3],
  qx      = lt$qx[1:3],
  q_exact = q_exact[1:3],
  q_cfm   = q_cfm[1:3]
  ), 4)

## ----recursions---------------------------------------------------------------
lt <- lt_rates$lt
Lx_hand <- 1 * lt$lx - (1 - lt$ax) * lt$dx

max(abs(Lx_hand - lt$Lx))

## ----ax-methods---------------------------------------------------------------
x_ab  <- c(0, 1, seq(5, 110, by = 5))
mx_ab <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
           .004, .005, .006, .0093, .0129, .019, .031, .049,
           .084, .129, .180, .2354, .3085, .390, .478, .551)

LT_ax <- lapply(c("andreev_kingkade", "cfm", "preston", "coale_demeny"), function(a)
  LifeTable(x = x_ab, mx = mx_ab, sex = "female", ax = a))
names(LT_ax) <- c("andreev_kingkade", "cfm", "preston", "coale_demeny")

ax_cmp <- t(sapply(LT_ax, function(L) L$lt$ax[1:2]))
colnames(ax_cmp) <- c("a0", "a1")
round(ax_cmp, 3)

## ----ax-methods-e0------------------------------------------------------------
e0_ax <- sapply(LT_ax, function(L) L$lt$ex[1])
round(e0_ax, 2)

## ----open-age-----------------------------------------------------------------
lt_ab <- LifeTable(x = x_ab, mx = mx_ab, sex = "female")
tail(lt_ab$lt, 2)

## ----close-omega--------------------------------------------------------------
x5  <- c(0, 1, seq(5, 75, by = 5))
mx5 <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
         .004, .005, .006, .0093, .0129, .019, .031, .049, .084)

lt_plain <- LifeTable(x = x5, mx = mx5)
lt_close <- LifeTable(x = x5, mx = mx5, close = "kannisto")
lt_ext   <- LifeTable(x = x5, mx = mx5, omega = 110)

c(plain = lt_plain$lt$ex[1], close = lt_close$lt$ex[1], omega = lt_ext$lt$ex[1])

## ----convertfx----------------------------------------------------------------
ex_1900 <- convertFx(x = x, data = mx, from = "mx", to = "ex")
round(ex_1900[1:3], 2)     # subsetting returns plain numbers

dx_1900 <- convertFx(x = x, data = lt_rates$lt$lx, from = "lx", to = "dx")
max(abs(unclass(dx_1900) - lt_rates$lt$dx))

## ----convertfx-plot-----------------------------------------------------------
plot(ex_1900)

## ----lawtable-----------------------------------------------------------------
fit <- MortalityLaw(x = 60:100, mx = mx[x >= 60 & x <= 100], law = "gompertz")
coef(fit)

lt_law <- LawTable(x = 60:100, par = coef(fit), law = "gompertz")
round(head(lt_law$lt[, -1], 3), 4)

## ----ex-inverse---------------------------------------------------------------
lt_back <- LifeTable(x = x, ex = lt_rates$lt$ex)

round(data.frame(
  age    = x[1:3],
  ex_in  = lt_rates$lt$ex[1:3],
  ex_out = lt_back$lt$ex[1:3],
  mx_out = lt_back$lt$mx[1:3]
  ), 5)

max(abs(lt_back$lt$ex - lt_rates$lt$ex))

## ----plot-lifetable-----------------------------------------------------------
lt_two <- LifeTable(
  x  = x,
  mx = ahmd$mx[as.character(x), c("1900", "2010")]
  )
plot(lt_two)

## ----plot-lifetable-one-------------------------------------------------------
plot(lt_two, which = "hazard")

## ----plot-lifetable-split, fig.asp = 0.35-------------------------------------
plot(lt_two, split = c(1, 4))

## ----session------------------------------------------------------------------
sessionInfo()

