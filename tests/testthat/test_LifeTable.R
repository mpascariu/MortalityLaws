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

mx1 = ahmd$mx
LT10 = LifeTable(x, mx = mx1)
LT10
expect_warning(LifeTable(x, mx = mx1))


# Example 2 --- Abridge life table ------------
x2  = c(0, 1, seq(5, 110, by = 5))
mx2 = c(.053, .005, .001, .0012, .0018, .002, .003, .004,
       .004, .005, .006, .0093, .0129, .019, .031, .049,
       .084, .129, .180, .2354, .3085, .390, .478, .551)
LT6 = LifeTable(x2, mx = mx2, sex = "female")
LT7 = LifeTable(x2, qx = LT6$lt$qx, sex = NULL)
LT8 = LifeTable(x2, lx = LT6$lt$lx, sex = "male")
LT9 = LifeTable(x2, dx = LT6$lt$dx, sex = "total")

LT6
LT7
LT8
LT9


x3 = c(0, 1, 2, 3, 4, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70)
dx = c(11728, 1998, 2190, 1336, 637, 1927, 420, 453, 475, 905, 1168, 
       2123, 2395, 3764, 5182, 6555, 8652, 10687, 37405)
LT11 <- LifeTable(x = x3, dx = dx)


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

for (j in 1:11) foo.test.lt(X = get(paste0("LT",j)))

test_that("Identical LTs", {
  expect_identical(round(LT1$lt$ex,2), round(LT2$lt$ex, 2))
  expect_identical(round(LT1$lt$ex,2), round(LT3$lt$ex, 2))
  expect_identical(round(LT1$lt$ex,2), round(LT4$lt$ex, 2))
  expect_identical(round(LT1$lt$ex,2), round(LT5$lt$ex, 2))
})


