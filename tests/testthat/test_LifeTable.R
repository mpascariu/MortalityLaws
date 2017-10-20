rm(list = ls())

# Example 1 --- Full life table -----------------
y  <- 1900
x  <- as.numeric(rownames(ahmd$mx))
Dx <- ahmd$Dx[, paste(y)]
Ex <- ahmd$Ex[, paste(y)]

LT1 <- LifeTable(x, Dx = Dx, Ex = Ex)
LT2 <- LifeTable(x, mx = LT1$lt.exact$mx)
LT3 <- LifeTable(x, qx = LT1$lt.exact$qx)
LT4 <- LifeTable(x, lx = LT1$lt.exact$lx)
LT5 <- LifeTable(x, dx = LT1$lt.exact$dx)

# Example 2 --- Abridge life table ------------
x  = c(0, 1, seq(5, 110, by = 5))
mx = c(.053, .005, .001, .0012, .0018, .002, .003, .004,
       .004, .005, .006, .0093, .0129, .019, .031, .049,
       .084, .129, .180, .2354, .3085, .390, .478, .551)
LT6 = LifeTable(x, mx = mx, sex = "female")

# TESTS ----------------------------------------------

foo.test.lt <- function(X) {
  test_that("LifeTable works fine", {
    # expect_true(all(X$lt.exact >= 0))
    expect_false(all(is.na(X$lt.exact$ex)))
    expect_identical(class(X$lt.exact$ex), "numeric")
    expect_true(X$lt.exact$ex[1] >= 0)
    expect_true(X$lt.exact$ex[1] >= rev(X$lt.exact$ex)[1])
  })
}

foo.test.lt(LT1)
foo.test.lt(LT2)
foo.test.lt(LT3)
foo.test.lt(LT4)
foo.test.lt(LT5)

test_that("Identical LTs", {
  expect_identical(LT1$lt$ex, LT2$lt$ex)
  expect_identical(LT1$lt$ex, LT3$lt$ex)
  expect_identical(LT1$lt$ex, LT4$lt$ex)
  expect_identical(LT1$lt$ex, LT5$lt$ex)
})
