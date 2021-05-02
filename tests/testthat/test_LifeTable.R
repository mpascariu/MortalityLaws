# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Sun May 02 11:35:52 2021
# --------------------------------------------------- #
remove(list = ls())
library(MortalityLaws)

# Example 1 --- Full life table -----------------
y  <- 1900
x  <- 0:107

Dx <- ahmd$Dx[paste(x), paste(y)]
Ex <- ahmd$Ex[paste(x), paste(y)]

LT1 <- LifeTable(x, Dx = Dx, Ex = Ex)
LT2 <- LifeTable(x, mx = LT1$lt$mx)
LT3 <- LifeTable(x, qx = LT1$lt$qx)
LT4 <- LifeTable(x, lx = LT1$lt$lx)
LT5 <- LifeTable(x, dx = LT1$lt$dx)

LT6  <- LifeTable(x, Dx = Dx, Ex = Ex, ax = 0.5)
LT7  <- LifeTable(x, mx = LT6$lt$mx, ax = 0.5)
LT8  <- LifeTable(x, qx = LT6$lt$qx, ax = 0.5)
LT9  <- LifeTable(x, lx = LT6$lt$lx, ax = 0.5)
LT10 <- LifeTable(x, dx = LT6$lt$dx, ax = 0.5)

# Example 2 --- Abridge life table ------------
x2  = c(0, 1, seq(5, 110, by = 5))
mx2 = c(.053, .005, .001, .0012, .0018, .002, .003, .004,
       .004, .005, .006, .0093, .0129, .019, .031, .049,
       .084, .129, .180, .2354, .3085, .390, .478, .551)
LT11 = LifeTable(x2, mx = mx2, sex = "female")
LT12 = LifeTable(x2, qx = LT11$lt$qx, sex = NULL)
LT13 = LifeTable(x2, lx = LT11$lt$lx, sex = "male")
LT14 = LifeTable(x2, dx = LT11$lt$dx, sex = "total")


x3 = c(0, 1, 2, 3, 4, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70)
dx = c(11728, 1998, 2190, 1336, 637, 1927, 420, 453, 475, 905, 1168,
       2123, 2395, 3764, 5182, 6555, 8652, 10687, 37405)
LT15 <- LifeTable(x = x3, dx = dx)


# Example 3 --- Abridge life table w ax ------------
ax <- LT15$lt$ax
ax[1] <- 0.1
LT16 <- LifeTable(x = x3, dx = dx, ax = ax)


# TESTS ----------------------------------------------
# expect_warning((LT16 = LifeTable(x = 0:110, mx = ahmd$mx)))

foo.test.lt <- function(X) {
  cn = c("x", "mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex")
  test_that("LifeTable works fine", {
    expect_true(all(X$lt[, cn] >= 0))           # All values in LT are positive
    expect_false(all(is.na(X$lt$ex)))           # ex does not contain NA's
    expect_identical(class(X$lt$ex), "numeric") # All ex is of the class numeric
    expect_true(X$lt$ex[1] >= rev(X$lt$ex)[1])  # e[x] at the beginnig is greater or equat with e[x] at the end
    expect_equal(sum(X$lt$dx), X$lt$lx[1])      # The distribution of deaths sums up to unity
    expect_true(X$lt$qx[nrow(X$lt)] >= 0.99999)  # The life table should always close with q[x] = 1
    expect_output(print(X))                     # The print function works
  })
}

for (j in 1:16) {
  print(j)
  foo.test.lt(X = get(paste0("LT", j)))
}


# round(LT1$lt$mx - LT2$lt$mx, 10)
# round(LT1$lt$mx - LT3$lt$mx, 10)
# round(LT1$lt$mx - LT4$lt$mx, 10)
# round(LT1$lt$mx - LT5$lt$mx, 10)

test_lt_consistency <- function(benchmark_LT, LT) {
  n <- nrow(benchmark_LT$lt)   # The last row can be different depending how the LT is closed. Do not test last row.
  B <- round(benchmark_LT$lt[-n, -1], 7)
  L <- round(LT$lt[-n, -1], 7)
  test_that("Identical LT estimates", {
    expect_identical(B$mx, L$mx)
    expect_identical(B$qx, L$qx)
    expect_identical(B$dx, L$dx)
    expect_identical(B$lx, L$lx)
    expect_identical(B$ex, L$ex)
  })
}

for (k in 2:5) test_lt_consistency(LT1, get(paste0("LT", k)))

for (k in 7:10) test_lt_consistency(LT6, get(paste0("LT", k)))




# ----------------------------------------------
# Test messages

# Error: 'ax' must be a numeric scalar (or NULL)
expect_error(
  LifeTable(x, mx = mx, ax = "ax")
)

# Error: 'ax' must be a scalar of lenght 1 or a vector of the same
# dimension as 'x'
expect_error(
  LifeTable(x, mx = mx, ax = rep(0.5, 3))
)

expect_error(
  # If you input 'Dx' you must input 'Ex' as well, and viceversa
  LifeTable(x, Dx = Dx)
)

expect_error(
  # The input is not specified correctly.
  LifeTable(x, Dx = Dx, Ex = Ex, qx = Ex, mx = Ex)
)

# Error: 'sex' should be: 'male', 'female', 'total' or 'NULL'.
mx <- LT1$lt$mx
expect_error(
  LifeTable(x, mx = mx, sex = "Male")
)

# 'Dx'contains missing values. These have been replaced with 0
Dxi <- Dx
Dxi[2] <- NA
expect_warning(
  LifeTable(x, Dx = Dxi, Ex = Ex)
)

# 'Ex'contains missing values
Exi <- Ex
Exi[12] <- NA
expect_warning(
  LifeTable(x, Dx = Dx, Ex = Exi)
)


# 'lx'contains missing values. These have been replaced with 0
lx <- LT1$lt$lx
lx[106] <- NA
expect_warning(
  LifeTable(x, lx = lx)
)

# 'dx'contains missing values.
dx <- LT1$lt$dx
dx[30] <- NA
expect_warning(
  LifeTable(x, dx = dx)
)



# # The input data contains NA's, Inf
# mx <- LT1$lt$mx
# mx[100] <- Inf
# expect_warning(
#   LifeTable(x, mx = mx)
# )


# ----------------------------------------------------------------------------
# Test print function for multiple tables

expect_output(
  print(LifeTable(x = 0:110, mx = ahmd$mx))
)






























