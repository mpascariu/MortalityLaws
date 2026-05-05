# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-05 18:51:58
# --------------------------------------------
remove(list = ls())
library(MortalityLaws)
library(testthat)

# Setup: define a baseline age grid (x1) covering the full lifespan from
# infancy to age 100, with corresponding mortality rates (mx). This data
# is used throughout to test consistency of model fits across input formats.
x1 <- c(0, 1, seq(5, 100, by = 5))
mx <- c(.08592, .00341, .00099, .00073, .00169, .00296, .00364,
        .00544, .00539, .01460, .01277, .02694, .01703, .04331,
        .03713, .07849, .09307, .13990, .18750, .22500, .25000,
        .30000)
names(mx) <- x1


# ----------------------------------------------
# THE TEST
# Strategy: verify that fitting a mortality law to the full age range (x1)
# and subsetting via fit.this.x = x2 (M1) produces identical estimates as
# fitting directly to the subset data x2 (M2). Both should yield identical
# coefficients and fitted values because the model is optimized on exactly
# the same observed data points. This tests the internal consistency of
# MortalityLaw regarding the fit.this.x parameter.

M1 <- function() MortalityLaw(x = x1,
                              mx = mx,
                              law = law,
                              fit.this.x = x2,
                              opt.method = opt.method)

M2 <- function() MortalityLaw(x = x2,
                              mx = mx[paste(x2)],
                              law = law,
                              fit.this.x = x2,
                              opt.method = opt.method)
opt.method = "LF2"



# Helper: runs a battery of consistency checks for each mortality law.
# Checks:
#   (1) fitted(M1) for ages x2 == fitted(M2) — same subset, same fit.
#   (2) fitted(M1) for x2 == predict(M1, x = x2) — fitted vs predict match.
#   (3) fitted(M1) for all x1 == predict(M1, x = x1) — full-range consistency.
#   (4) fitted(M1) for all x1 == predict(M2, x = x1) — cross-model consistency.
#   (5) coefficients identical across both fitting approaches.
#   (6-7) plotting functions return non-NULL.
testFN <- function(M1, M2) {
  test_that(paste(law, "Model"), {
    expect_identical(fitted(M1)[paste(x2)], fitted(M2))
    expect_identical(fitted(M1)[paste(x2)], predict(M1, x = x2))
    expect_identical(fitted(M1), predict(M1, x = x1))
    expect_identical(fitted(M1), predict(M2, x = x1))
    expect_identical(coef(M1), coef(M2))
    expect_false(is.null(plot(M1)))
    expect_false(is.null(plot(M2)))
  })
}


# ----------------------------------------------
# Test gompertz -- Gompertz law, appropriate for adult mortality (ages 40-75).
law = "gompertz"
x2 = seq(40, 75, by = 5)

testFN(M1(), M2())

# ----------------------------------------------
# Test gompertz0 -- Gompertz with fixed Makeham term = 0 (same adult range).
law = "gompertz0"
x2 = seq(40, 75, by = 5)

testFN(M1(), M2())

# ----------------------------------------------
# Test invgompertz -- Inverse Gompertz, suited for young/accident-hump ages (5-30).
law = "invgompertz"
x2 = seq(5, 30, by = 5)

testFN(M1(), M2())
# ----------------------------------------------
# Test makeham -- Makeham law, appropriate for adult mortality (ages 35-90).
law = "makeham"
x2 = seq(35, 90, by = 5)

testFN(M1(), M2())
# ----------------------------------------------
# Test makeham0 -- Makeham with fixed constant term = 0 (same adult range).
law = "makeham0"
x2 = seq(35, 90, by = 5)

testFN(M1(), M2())
# ----------------------------------------------
# Test opperman -- Opperman, covers infancy through young adulthood (ages 0-25).
law = "opperman"
x2 = c(0,1, seq(5, 25, by = 5))

testFN(M1(), M2())
# ----------------------------------------------
# Test thiele -- Thiele: full lifespan model (ages 0-100).
law = "thiele"
x2 = x1

testFN(M1(), M2())
# ----------------------------------------------
# Test wittstein -- Wittstein: full lifespan model (ages 0-100).
law = "wittstein"
x2 = x1
testFN(M1(), M2())
# ----------------------------------------------
# Test perks -- Perks: middle-to-old ages (20-80).
law = "perks"
x2 = seq(20, 80, 5)
testFN(M1(), M2())
# ----------------------------------------------
# Test weibull -- Weibull: early ages (0-15) where failure-rate models apply.
law = "weibull"
x2 = c(0, 1, 5, 10, 15)
testFN(M1(), M2())
# ----------------------------------------------
# Test invweibull -- Inverse Weibull: young adult ages (10-30).
law = "invweibull"
x2 = seq(10, 30, 5)
testFN(M1(), M2())
# ----------------------------------------------
# Test vandermaen -- Van der Maen: adult and old ages (20-95).
law = "vandermaen"
x2 = seq(20, 95, by = 5)
testFN(M1(), M2())
# ----------------------------------------------
# Test vandermaen2 -- Van der Maen v2: older ages only (60-95).
law = "vandermaen2"
x2 = seq(60, 95, by = 5)
testFN(M1(), M2())
# ----------------------------------------------
# Test strehler_mildvan -- Strehler-Mildvan: adult ages (40-75).
law = "strehler_mildvan"
x2 = seq(40, 75, by = 5)
testFN(M1(), M2())
# ----------------------------------------------
# Test quadratic -- Quadratic: older ages (60-95).
law = "quadratic"
x2 = seq(60, 95, by = 5)
testFN(M1(), M2())
# ----------------------------------------------
# Test beard -- Beard: older ages (60-95).
law = "beard"
x2 = seq(60, 95, by = 5)
testFN(M1(), M2())
# ----------------------------------------------
# Test beard_makeham -- Beard-Makeham: older ages (60-95).
law = "beard_makeham"
x2 = seq(60, 95, by = 5)
testFN(M1(), M2())
# ----------------------------------------------
# Test ggompertz -- Gamma-Gompertz: older ages (60-95).
law = "ggompertz"
x2 = seq(60, 95, by = 5)
testFN(M1(), M2())
# ----------------------------------------------
# Test siler -- Siler: full lifespan (0-100), combines three components.
law = "siler"
x2 = x1
testFN(M1(), M2())
# ----------------------------------------------
# Test HP -- Heligman-Pollard (8-parameter): full lifespan.
law = "HP"
x2 = x1
testFN(M1(), M2())
# ----------------------------------------------
# Test HP2 -- Heligman-Pollard variant 2: full lifespan.
law = "HP2"
x2 = x1
testFN(M1(), M2())
# ----------------------------------------------
# Test HP3 -- Heligman-Pollard variant 3: full lifespan.
law = "HP3"
x2 = x1
testFN(M1(), M2())
# ----------------------------------------------
# Test HP4 -- Heligman-Pollard variant 4: full lifespan.
law = "HP4"
x2 = x1
testFN(M1(), M2())
# ----------------------------------------------
# Test rogersplanck -- Rogers-Planck: full lifespan.
law = "rogersplanck"
x2 = x1
testFN(M1(), M2())
# ----------------------------------------------
# Test martinelle -- Martinelle: infancy through middle ages (0-75).
law = "martinelle"
x2 = c(0, 1, seq(5, 75, 5))
testFN(M1(), M2())
# ----------------------------------------------
# Test carriere1 -- Carriere v1 (3-component mixture): full lifespan.
law = "carriere1"
x2 = x1
testFN(M1(), M2())
# ----------------------------------------------
# Test carriere2 -- Carriere v2 (3-component mixture, different param.): full lifespan.
law = "carriere2"
x2 = x1
testFN(M1(), M2())
# ----------------------------------------------
# Test kostaki -- Kostaki: full lifespan.
law = "kostaki"
x2 = x1
testFN(M1(), M2())
# ----------------------------------------------
# Test kannisto -- Kannisto: old-age mortality only (80-95).
law = "kannisto"
x2 = c(80, 85, 90, 95)
testFN(M1(), M2())
# ----------------------------------------------
# Test kannisto_makeham -- Kannisto-Makeham: old-age mortality only (80-95).
law = "kannisto_makeham"
x2 = c(80, 85, 90, 95)
testFN(M1(), M2())





