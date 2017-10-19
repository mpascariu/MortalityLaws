rm(list = ls())

# Example 1 --- Full life table -----------------
x <- as.numeric(rownames(ahmd$mx))
year <- 1900

mx <- ahmd$mx[, paste(year)]
Dx <- ahmd$Dx[, paste(year)]
Ex <- ahmd$Ex[, paste(year)]

LT1 <- LifeTable(x, mx = mx, sex = "female")
LT2 <- LifeTable(x, Dx = Dx, Ex = Ex, sex = "female")

# Example 2 --- Abridge life table ------------
x  = c(0, 1, seq(5, 110, by = 5))
mx = c(.053, .005, .001, .0012, .0018, .002, .003, .004,
       .004, .005, .006, .0093, .0129, .019, .031, .049,
       .084, .129, .180, .2354, .3085, .390, .478, .551)
LT3 = LifeTable(x, mx = mx, sex = "female")

# TESTS ----------------------------------------------

foo.test.lt <- function(X) {
  test_that("LifeTable works fine", {
    expect_true(all(X$lt.exact >= 0))
    expect_false(all(is.na(X$lt.exact$ex)))
    expect_identical(class(X$lt.exact$ex), "numeric")
    expect_true(X$lt.exact$ex[1] >= 0)
    expect_true(X$lt.exact$ex[1] >= rev(X$lt.exact$ex)[1])
  })
}

foo.test.lt(LT1)
foo.test.lt(LT2)
foo.test.lt(LT3)