# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: MIT
# Last update: Thu Nov 07 10:37:35 2019
# --------------------------------------------------- #
remove(list = ls())
library(MortalityLaws)
library(testthat)

x1 <- c(0, 1, seq(5, 100, by = 5))
mx <- c(.08592, .00341, .00099, .00073, .00169, .00296, .00364,
        .00544, .00539, .01460, .01277, .02694, .01703, .04331,
        .03713, .07849, .09307, .13990, .18750, .22500, .25000,
        .30000)
names(mx) <- x1


# ----------------------------------------------
# THE TEST
# Here we would like to test what happens if the same model is fitted using
# data in different format. Let M1 and M2 be the 2 fitted objects. There
# should be no difference between the estimates produced.

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
# Test gompertz -- OK
law = "gompertz"
x2 = seq(40, 75, by = 5)

testFN(M1(), M2())

# ----------------------------------------------
# Test gompertz0 -- OK
law = "gompertz0"
x2 = seq(40, 75, by = 5)

testFN(M1(), M2())

# ----------------------------------------------
# Test invgompertz -- OK
law = "invgompertz"
x2 = seq(5, 30, by = 5)

testFN(M1(), M2())
# ----------------------------------------------
# Test makeham -- OK
law = "makeham"
x2 = seq(35, 90, by = 5)

testFN(M1(), M2())
# ----------------------------------------------
# Test makeham0 -- OK
law = "makeham0"
x2 = seq(35, 90, by = 5)

testFN(M1(), M2())
# ----------------------------------------------
law = "opperman"
x2 = c(0,1, seq(5, 25, by = 5))

testFN(M1(), M2())
# ----------------------------------------------
# Test thiele -- OK
law = "thiele"
x2 = x1

testFN(M1(), M2())
# ----------------------------------------------
# Test wittstein -- OK
law = "wittstein"
x2 = x1

testFN(M1(), M2())
# ----------------------------------------------
# Test perks -- OK
law = "perks"
x2 = seq(20, 80, 5)

testFN(M1(), M2())
# ----------------------------------------------
# Test weibull -- OK
law = "weibull"
x2 = c(0, 1, 5, 10, 15)

testFN(M1(), M2())
# ----------------------------------------------
# Test invweibull -- OK
law = "invweibull"
x2 = seq(10, 30, 5)

testFN(M1(), M2())
# ----------------------------------------------
# Test vandermaen -- OK
law = "vandermaen"
x2 = seq(20, 95, by = 5)

testFN(M1(), M2())
# ----------------------------------------------
# Test vandermaen2 -- OK
law = "vandermaen2"
x2 = seq(60, 95, by = 5)

testFN(M1(), M2())
# ----------------------------------------------
# Test strehler_mildvan -- OK
law = "strehler_mildvan"
x2 = seq(40, 75, by = 5)

testFN(M1(), M2())
# ----------------------------------------------
# Test quadratic -- OK
law = "quadratic"
x2 = seq(60, 95, by = 5)

testFN(M1(), M2())
# ----------------------------------------------
# Test beard -- OK
law = "beard"
x2 = seq(60, 95, by = 5)

testFN(M1(), M2())
# ----------------------------------------------
# Test beard_makeham -- OK
law = "beard_makeham"
x2 = seq(60, 95, by = 5)

testFN(M1(), M2())
# ----------------------------------------------
# Test ggompertz -- OK
law = "ggompertz"
x2 = seq(60, 95, by = 5)

testFN(M1(), M2())
# ----------------------------------------------
# Test siler -- OK
law = "siler"
x2 = x1

testFN(M1(), M2())
# ----------------------------------------------
# Test HP -- OK
law = "HP"
x2 = x1

testFN(M1(), M2())
# ----------------------------------------------
# Test HP2 -- OK
law = "HP2"
x2 = x1

testFN(M1(), M2())
# ----------------------------------------------
# Test HP3 -- OK
law = "HP3"
x2 = x1

testFN(M1(), M2())

# ----------------------------------------------
# Test HP4 -- OK
law = "HP4"
x2 = x1

testFN(M1(), M2())
# ----------------------------------------------
# Test rogersplanck -- OK
law = "rogersplanck"
x2 = x1

testFN(M1(), M2())
# ----------------------------------------------
# Test martinelle -- OK
law = "martinelle"
x2 = c(0, 1, seq(5, 75, 5))

testFN(M1(), M2())
# ----------------------------------------------
# Test carriere1 -- OK
law = "carriere1"
x2 = x1

testFN(M1(), M2())
# ----------------------------------------------
# Test carriere2 -- OK
law = "carriere2"
x2 = x1

testFN(M1(), M2())
# ----------------------------------------------
# Test kostaki -- OK
law = "kostaki"
x2 = x1

testFN(M1(), M2())
# ----------------------------------------------
# Test kannisto -- OK
law = "kannisto"
x2 = c(80, 85, 90, 95)

testFN(M1(), M2())

# ----------------------------------------------
# Test kannisto_makeham -- OK
law = "kannisto_makeham"
x2 = c(80, 85, 90, 95)

testFN(M1(), M2())





