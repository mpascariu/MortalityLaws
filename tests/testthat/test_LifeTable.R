# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-05 18:51:35
# --------------------------------------------

# Setup: Load AHMD mortality data for Sweden in the year 1900, ages 0-107.
# This serves as the baseline for constructing life tables from various
# input types (Dx+Ex, mx, qx, lx, dx) and verifying they yield identical results.

# Example 1 --- Full life table (single-year ages 0-107) -----------------
y  <- 1900
x  <- 0:107

Dx <- ahmd$Dx[paste(x), paste(y)]
Ex <- ahmd$Ex[paste(x), paste(y)]

LT1 <- LifeTable(x, Dx = Dx, Ex = Ex)
LT2 <- LifeTable(x, mx = LT1$lt$mx)
LT3 <- LifeTable(x, qx = LT1$lt$qx)
LT4 <- LifeTable(x, lx = LT1$lt$lx)
LT5 <- LifeTable(x, dx = LT1$lt$dx)

# LT6-LT10 repeat the exercise with a constant user-supplied ax. The exercise
# runs on ages 0-105: above age 105 this population has mx > 2, where ax = 0.5
# is incompatible with the exact identity qx = nx*mx/(1 + (nx - ax)*mx) (it
# would return qx > 1). The regular intervals all use ax = 0.5; the open
# interval must satisfy the contract rule ax[N] = 1/mx[N], so it is set to
# that value (its rate cannot be recovered from a closed qx/lx/dx input).
x6  <- 0:105
Dx6 <- ahmd$Dx[paste(x6), paste(y)]
Ex6 <- ahmd$Ex[paste(x6), paste(y)]
ax6 <- c(rep(0.5, length(x6) - 1), Ex6[length(x6)]/Dx6[length(x6)])
LT6  <- suppressWarnings(LifeTable(x6, Dx = Dx6, Ex = Ex6, ax = ax6))
LT7  <- suppressWarnings(LifeTable(x6, mx = LT6$lt$mx, ax = ax6))
LT8  <- suppressWarnings(LifeTable(x6, qx = LT6$lt$qx, ax = ax6))
LT9  <- suppressWarnings(LifeTable(x6, lx = LT6$lt$lx, ax = ax6))
LT10 <- suppressWarnings(LifeTable(x6, dx = LT6$lt$dx, ax = ax6))

# Example 2 --- Abridged life table (irregular intervals: 0,1, then 5-year
# groups up to 110) ---------------------------------------------------------
# Tests LifeTable with an abridged age structure using hypothetical mortality rates (mx2),
# and verifies that different primary inputs (mx, qx, lx, dx) with sex specification
# all produce consistent life table estimates.
x2  <- c(0, 1, seq(5, 110, by = 5))
mx2 <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
         .004, .005, .006, .0093, .0129, .019, .031, .049,
         .084, .129, .180, .2354, .3085, .390, .478, .551)
# LT11-LT14: Abridged life tables built from the same underlying mortality but with
# different primary inputs (mx, qx, lx, dx). Also tests that the sex argument is
# correctly passed through (female, NULL, male, total).
LT11 <- LifeTable(x2, mx = mx2, sex = "female")
LT12 <- LifeTable(x2, qx = LT11$lt$qx, sex = NULL)
LT13 <- LifeTable(x2, lx = LT11$lt$lx, sex = "male")
LT14 <- LifeTable(x2, dx = LT11$lt$dx, sex = "total")


# LT15: Abridged life table with irregular age groups (single-year for early childhood,
# then 5-year intervals up to age 70). Built from death counts (dx) only.
x3  <- c(0, 1, 2, 3, 4, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70)
dx  <- c(11728, 1998, 2190, 1336, 637, 1927, 420, 453, 475, 905, 1168,
         2123, 2395, 3764, 5182, 6555, 8652, 10687, 37405)
LT15 <- suppressWarnings(LifeTable(x = x3, dx = dx))


# Example 3 --- Abridged life table with custom ax ------------
# Tests that a user-specified ax for the first age (infant) works correctly.
ax    <- LT15$lt$ax
ax[1] <- 0.1  # Override the default infant separation factor
LT16  <- suppressWarnings(LifeTable(x = x3, dx = dx, ax = ax))


# TESTS ----------------------------------------------
# Run two sets of tests:
#   1. foo.test.lt() - validates basic life table properties for all 16 LTs.
#   2. test_lt_consistency() - verifies that LTs built from different input types
#      (Dx+Ex, mx, qx, lx, dx) produce identical life table columns.

foo.test.lt <- function(X) {
  cn <- c("x", "mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex")
  test_that("LifeTable works fine", {
    expect_true(all(X$lt[, cn] >= 0))            # All values in LT are positive
    expect_false(all(is.na(X$lt$ex)))            # ex does not contain NA's
    expect_identical(class(X$lt$ex), "numeric")  # All ex is of the class numeric
    expect_true(X$lt$ex[1] >= rev(X$lt$ex)[1])   # e[x] at the beginning is the largest
    expect_equal(sum(X$lt$dx), X$lt$lx[1])       # Deaths sum to the radix
    expect_true(X$lt$qx[nrow(X$lt)] >= 0.99999)  # The life table closes with q[x] = 1
    expect_output(print(X))                      # The print function works
  })
}

for (j in 1:16) foo.test.lt(X = get(paste0("LT", j)))


# Helper: verifies that a life table constructed from an alternative input (mx, qx, lx, dx)
# matches the benchmark life table (built from Dx+Ex). The last row is excluded because
# the closure method may differ depending on the input type. ex is compared with a looser
# tolerance: Tx/ex always carry the open-interval rate, and that rate is unidentifiable
# from a closed qx[N] = 1 input (the two repairs differ by ~1e-4 at row N-1).
test_lt_consistency <- function(benchmark_LT, LT) {
  # The last row may differ depending how the LT is closed. Do not test it.
  n <- nrow(benchmark_LT$lt)
  B <- benchmark_LT$lt[-n, -1]
  L <- LT$lt[-n, -1]
  test_that("Identical LT estimates", {
    expect_equal(B$mx, L$mx, tolerance = 1e-8)
    expect_equal(B$qx, L$qx, tolerance = 1e-8)
    expect_equal(B$dx, L$dx, tolerance = 1e-8)
    expect_equal(B$lx, L$lx, tolerance = 1e-8)
    expect_equal(B$ex, L$ex, tolerance = 1e-6)
  })
}

for (k in 2:5) test_lt_consistency(LT1, get(paste0("LT", k)))

for (k in 7:10) test_lt_consistency(LT6, get(paste0("LT", k)))


# Input validation: verify LifeTable catches incorrect usage ------------------
# Each test below checks a specific invalid input scenario:

test_that("LifeTable validates the 'ax' argument", {
  mx_lt <- LT1$lt$mx
  # Error: 'ax' must be a numeric scalar (or NULL) - here it's a string.
  expect_error(LifeTable(x, mx = mx_lt, ax = "ax"),
               regexp = "'ax' must be a numeric")
  # Error: 'ax' must be a scalar of length 1 or a vector of the same
  # dimension as 'x'
  expect_error(LifeTable(x, mx = mx_lt, ax = rep(0.5, 3)),
               regexp = "scalar of length 1")
})

test_that("LifeTable forces ax[N] = 1/mx[N] at the closing age", {
  # A constant ax = 0.5 is admissible on this abridged schedule (mx <= 0.551),
  # so the only override is the contract rule for the open interval.
  xa  <- c(0, 1, seq(5, 100, by = 5))
  mxa <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
           .004, .005, .006, .0093, .0129, .019, .031, .049,
           .084, .129, .180, .2354, .3085, .390)
  Na  <- length(xa)
  expect_warning(LT <- LifeTable(xa, mx = mxa, ax = rep(0.5, Na)))
  # Open age interval (contract 3): ax[N] = ex[N] = 1/mx[N] = Lx[N]/lx[N]
  expect_equal(LT$lt$ax[Na], 1/LT$lt$mx[Na], tolerance = 1e-12)
  expect_equal(LT$lt$ex[Na], 1/LT$lt$mx[Na], tolerance = 1e-12)
  expect_equal(LT$lt$Lx[Na], LT$lt$lx[Na]/LT$lt$mx[Na], tolerance = 1e-12)
})

test_that("LifeTable validates the input combination", {
  expect_error(
    # If you input 'Dx' you must input 'Ex' as well, and viceversa
    LifeTable(x, Dx = Dx)
  )
  expect_error(
    # The input is not specified correctly.
    LifeTable(x, Dx = Dx, Ex = Ex, qx = Ex, mx = Ex)
  )
})

test_that("LifeTable validates the 'sex' argument", {
  mx_lt <- LT1$lt$mx
  # Error: 'sex' should be: 'male', 'female', 'total' or 'NULL'.
  expect_error(LifeTable(x, mx = mx_lt, sex = "Male"))
})

test_that("LifeTable warns on missing values", {
  # 'Dx' contains missing values. These have been replaced with 0
  Dxi <- Dx
  Dxi[2] <- NA
  expect_warning(LifeTable(x, Dx = Dxi, Ex = Ex))

  # 'Ex' contains missing values
  Exi <- Ex
  Exi[12] <- NA
  expect_warning(LifeTable(x, Dx = Dx, Ex = Exi))

  # 'lx' contains missing values. These have been replaced with 0
  lx <- LT1$lt$lx
  lx[106] <- NA
  expect_warning(LifeTable(x, lx = lx))

  # 'dx' contains missing values.
  dx <- LT1$lt$dx
  dx[30] <- NA
  expect_warning(LifeTable(x, dx = dx))
})

test_that("LifeTable handles an NA in mx locally", {
  # Contract 3: a missing mx corrupts only the missing interval and ages below it.
  k  <- 31                    # age 30
  mx <- rep(0.01, length(x))
  mx[k] <- NA
  expect_warning(LT <- LifeTable(x = x, mx = mx))
  expect_true(is.na(LT$lt$mx[k]))
  expect_true(is.na(LT$lt$qx[k]))
  expect_true(is.na(LT$lt$Lx[k]))
  expect_true(all(is.finite(LT$lt$lx)))
  expect_true(all(is.na(LT$lt$ex[1:k])))
  expect_true(all(is.finite(LT$lt$ex[(k + 1):length(x)])))
})

# ----------------------------------------------------------------------------
# Test print function for life tables with multiple columns (multi-population).
# When mx is a matrix (multiple columns), LifeTable returns multiple life tables.
# The print function should handle this case without error.
test_that("print works for multi-column life tables", {
  # ahmd$mx carries NA at ages 107-110, which contract 3 surfaces as a warning
  expect_warning(LT_multi <- LifeTable(x = 0:110, mx = ahmd$mx))
  expect_output(print(LT_multi))
})
