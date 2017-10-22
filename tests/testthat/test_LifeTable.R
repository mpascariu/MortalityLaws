rm(list = ls())

# Example 1 --- Full life table -----------------
y  <- 1900
x  <- as.numeric(rownames(ahmd$mx))

Dx <- ahmd$Dx[, paste(y)]
Ex <- ahmd$Ex[, paste(y)]

LT1 <- LifeTable(x, Dx = Dx, Ex = Ex)
LT2 <- LifeTable(x, mx = LT1$lt$mx)
LT3 <- LifeTable(x, qx = LT1$lt$qx)
LT4 <- LifeTable(x, lx = LT1$lt$lx)
LT5 <- LifeTable(x, dx = LT1$lt$dx)

LT1
LT2
LT3
LT4
LT5

mx = ahmd$mx
LTs = LifeTable(x, mx = ahmd$mx)
LTs
expect_warning(LifeTable(x, mx = mx))


# Example 2 --- Abridge life table ------------
x  = c(0, 1, seq(5, 110, by = 5))
mx = c(.053, .005, .001, .0012, .0018, .002, .003, .004,
       .004, .005, .006, .0093, .0129, .019, .031, .049,
       .084, .129, .180, .2354, .3085, .390, .478, .551)
LT6 = LifeTable(x, mx = mx, sex = "female")
LT6

# TESTS ----------------------------------------------

foo.test.lt <- function(X) {
  cn = c("x", "mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex")
  test_that("LifeTable works fine", {
    expect_true(all(X$lt[, cn] >= 0))
    expect_false(all(is.na(X$lt$ex)))
    expect_identical(class(X$lt$ex), "numeric")
    expect_true(X$lt$ex[1] >= 0)
    expect_true(X$lt$ex[1] >= rev(X$lt$ex)[1])
    expect_output(print(X))
  })
}

foo.test.lt(LT1)
foo.test.lt(LT2)
foo.test.lt(LT3)
foo.test.lt(LT4)
foo.test.lt(LT5)
foo.test.lt(LTs)

test_that("Identical LTs", {
  expect_identical(round(LT1$lt$ex,2), round(LT2$lt$ex, 2))
  expect_identical(round(LT1$lt$ex,2), round(LT3$lt$ex, 2))
  expect_identical(round(LT1$lt$ex,2), round(LT4$lt$ex, 2))
  expect_identical(round(LT1$lt$ex,2), round(LT5$lt$ex, 2))
})


