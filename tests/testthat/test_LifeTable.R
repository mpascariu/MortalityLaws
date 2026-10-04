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
# matches the benchmark life table (built from Dx+Ex). The last two rows are excluded:
# the closing interval and the one below it, where the input probability is 1 and the
# rate behind it is not recoverable, so the ax rule cannot be applied identically on
# both sides. ex is compared with a looser tolerance because Tx and ex carry that same
# open-interval rate into every row below it.
test_lt_consistency <- function(benchmark_LT, LT) {
  n <- nrow(benchmark_LT$lt)
  i <- seq_len(n - 2)
  B <- benchmark_LT$lt[i, -1]
  L <- LT$lt[i, -1]
  test_that("Identical LT estimates", {
    expect_equal(B$mx, L$mx, tolerance = 1e-6)
    expect_equal(B$qx, L$qx, tolerance = 1e-8)
    expect_equal(B$dx, L$dx, tolerance = 1e-8)
    expect_equal(B$lx, L$lx, tolerance = 1e-8)
    expect_equal(B$ex, L$ex, tolerance = 1e-3)
  })
}

for (k in 2:5) test_lt_consistency(LT1, get(paste0("LT", k)))

for (k in 7:10) test_lt_consistency(LT6, get(paste0("LT", k)))


# Input validation: verify LifeTable catches incorrect usage ------------------
# Each test below checks a specific invalid input scenario:

test_that("LifeTable validates the 'ax' argument", {
  mx_lt <- LT1$lt$mx
  # Error: 'ax' must be NULL, a numeric scalar/vector, or a method name;
  # here it is neither a known method nor numeric.
  expect_error(LifeTable(x, mx = mx_lt, ax = "ax"),
               regexp = "should be one of")
  # Error: 'ax' must be a scalar of length 1 or a vector of the same
  # dimension as 'x'
  expect_error(LifeTable(x, mx = mx_lt, ax = rep(0.5, 3)),
               regexp = "scalar of length 1")
})

test_that("the 'cfm' ax method is the plain lifetable identity", {
  xa  <- c(0, 1, seq(5, 100, by = 5))
  mxa <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
           .004, .005, .006, .0093, .0129, .019, .031, .049,
           .084, .129, .180, .2354, .3085, .390)
  N   <- length(xa)
  cfm <- LifeTable(xa, mx = mxa, ax = "cfm")$lt

  # ax[2..N-1] is exactly n + 1/m - n/q, with q under the CFM assumption.
  n <- c(diff(xa), NA)
  q <- 1 - exp(-n * mxa)
  a <- n + 1/mxa - n/q
  expect_equal(cfm$ax[2:(N - 1)], a[2:(N - 1)], tolerance = 1e-10)

  # Without a sex, cfm and preston coincide everywhere except the open row.
  pr <- LifeTable(xa, mx = mxa, ax = "preston")$lt
  expect_equal(cfm$ax[-N], pr$ax[-N], tolerance = 1e-12)

  # With a sex the childhood rule bites: cfm differs from preston at 0 and 1-4.
  expect_false(isTRUE(all.equal(
    LifeTable(xa, mx = mxa, sex = "female", ax = "cfm")$lt$ax[1:2],
    LifeTable(xa, mx = mxa, sex = "female", ax = "preston")$lt$ax[1:2])))

  # The two childhood conventions still agree above the m0 = 0.107 cutoff.
  mx2    <- mxa; mx2[1] <- 0.12
  expect_identical(LifeTable(xa, mx = mx2, sex = "female", ax = "preston")$lt$ax[1:2],
                   LifeTable(xa, mx = mx2, sex = "female",
                             ax = "coale_demeny")$lt$ax[1:2])
})

test_that("the 'andreev_kingkade' ax method follows the HMD Methods Protocol v6", {
  xa  <- c(0, 1, seq(5, 100, by = 5))
  mxa <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
           .004, .005, .006, .0093, .0129, .019, .031, .049,
           .084, .129, .180, .2354, .3085, .390)
  N   <- length(xa)

  # 'andreev_kingkade' is the shipped default.
  expect_identical(LifeTable(xa, mx = mxa)$lt,
                   LifeTable(xa, mx = mxa, ax = "andreev_kingkade")$lt)

  # Andreev-Kingkade (2015) a0 from m0, HMD Methods Protocol v6 Table 3,
  # total row = mean of the male and female branches (m0 = 0.053 is in the
  # middle branch):
  a0M <- 0.02832 + 3.26021 * mxa[1]
  a0F <- 0.04667 + 3.88089 * mxa[1]
  hmd <- LifeTable(xa, mx = mxa)$lt
  expect_equal(hmd$ax[1], (a0M + a0F)/2, tolerance = 1e-12)
  expect_equal(LifeTable(xa, mx = mxa, sex = "male")$lt$ax[1], a0M,
               tolerance = 1e-12)
  expect_equal(LifeTable(xa, mx = mxa, sex = "female")$lt$ax[1], a0F,
               tolerance = 1e-12)

  # On this abridged schedule only the first interval (0,1) is one year wide,
  # so it is the only one that takes the midpoint; the wider intervals keep
  # the constant force of mortality value.
  n <- c(diff(xa), NA)
  wide <- n[2:(N - 1)] > 1
  cfm_wide <- LifeTable(xa, mx = mxa, ax = "cfm")$lt$ax[2:(N - 1)]
  expect_equal(hmd$ax[2:(N - 1)][wide], cfm_wide[wide], tolerance = 1e-12)

  # The open interval keeps the house rule.
  expect_equal(hmd$ax[N], 1/hmd$mx[N], tolerance = 1e-12)

  # Because 'andreev_kingkade' builds a numeric ax, the conversion uses the
  # exact identity qx = n*mx/(1 + (n - ax)*mx), the protocol's equation 74,
  # which is why the default does not reproduce the cfm death probabilities.
  idx <- 1:(N - 1)
  expect_equal(hmd$qx[idx],
               n[idx] * mxa[idx] / (1 + (n[idx] - hmd$ax[idx]) * mxa[idx]),
               tolerance = 1e-12)
  expect_false(isTRUE(all.equal(hmd$qx[1],
    LifeTable(xa, mx = mxa, ax = "cfm")$lt$qx[1])))
})

test_that("the 'andreev_kingkade' rule needs a table starting at birth", {
  # A table starting at birth with a one-year first interval: the
  # Andreev-Kingkade a0 applies at age 0.
  x1 <- c(0, 1, seq(5, 100, by = 5))
  m1 <- c(.053, .005, rep(.01, length(x1) - 2))
  a1 <- LifeTable(x1, mx = m1)$lt
  ak <- (0.02832 + 3.26021*.053 + 0.04667 + 3.88089*.053)/2
  expect_equal(a1$ax[1], ak, tolerance = 1e-12)

  # A table starting above age 0 has no m0: the first interval is the
  # ordinary midpoint, whatever the age, so the method is identical to the
  # plain n/2 rule there.
  x2 <- 3:110
  m2 <- rep(0.01, length(x2))
  a2 <- LifeTable(x2, mx = m2)$lt
  expect_equal(a2$ax[1], 0.5, tolerance = 1e-12)

  # A table starting at birth with a wide first interval has no m0 either, so
  # its first interval keeps the constant force of mortality value there too.
  x3 <- c(0, seq(5, 100, by = 5))
  m3 <- c(.053, rep(.01, length(x3) - 1))
  a3 <- LifeTable(x3, mx = m3)$lt
  c3 <- LifeTable(x3, mx = m3, ax = "cfm")$lt
  expect_equal(a3$ax, c3$ax, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(a3$ax[1], 2.5)))
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

# ----------------------------------------------------------------------------
# Closing at a chosen omega (issue #8): extend the open interval beyond the
# age at which the input stops, extrapolating with a mortality law.
test_that("LifeTable leaves the result unchanged when omega is NULL", {
  xo  <- c(0, 1, seq(5, 75, by = 5))
  mxo <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
           .004, .005, .006, .0093, .0129, .019, .031, .049, .084)
  A <- LifeTable(xo, mx = mxo)
  B <- LifeTable(xo, mx = mxo, omega = NULL)
  C <- LifeTable(xo, mx = mxo, close = NULL)
  expect_identical(A$lt, B$lt)
  expect_identical(A$lt, C$lt)
})

# ----------------------------------------------------------------------------
# Accurate closing without extension (issue #8): 'close' replaces the observed
# open-interval rate with the model-implied average over the tail, keeping the
# age grid unchanged.
test_that("LifeTable closes in place with a mortality law", {
  xo  <- c(0, 1, seq(5, 75, by = 5))
  mxo <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
           .004, .005, .006, .0093, .0129, .019, .031, .049, .084)
  N   <- length(xo)
  A   <- LifeTable(xo, mx = mxo)$lt
  C   <- LifeTable(xo, mx = mxo, close = "kannisto")$lt

  # The grid is untouched: same ages, same number of rows.
  expect_identical(A$x, C$x)
  expect_equal(nrow(A), nrow(C))

  # Only the open row's rate changes; everything below is preserved exactly.
  expect_equal(C$mx[-N], A$mx[-N], tolerance = 1e-12)
  expect_false(isTRUE(all.equal(C$mx[N], A$mx[N])))

  # The open row keeps the standard identities on the corrected rate, and the
  # lives below the open age are the same.
  expect_equal(C$qx[N], 1, tolerance = 1e-12)
  expect_equal(C$ax[N], 1/C$mx[N], tolerance = 1e-12)
  expect_equal(C$ex[N], 1/C$mx[N], tolerance = 1e-12)
  expect_equal(C$lx, A$lx, tolerance = 1e-12)

  # A young open interval is closed more accurately than the reciprocal rule:
  # the observed 1/mx[75] overshoots because the hazard is still rising.
  expect_true(C$mx[N] > A$mx[N])
  expect_true(C$ex[1] < A$ex[1])
})

test_that("LifeTable closes in place for every input index", {
  xo  <- c(0, 1, seq(5, 75, by = 5))
  mxo <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
           .004, .005, .006, .0093, .0129, .019, .031, .049, .084)
  A <- LifeTable(xo, mx = mxo)$lt
  mx_new <- vapply(c("qx", "lx", "dx"), function(idx) {
    L <- switch(idx,
                qx = LifeTable(xo, qx = A$qx, close = "kannisto"),
                lx = LifeTable(xo, lx = A$lx, close = "kannisto"),
                dx = LifeTable(xo, dx = A$dx, close = "kannisto"))
    L$lt$mx[nrow(L$lt)]
  }, 0)
  expect_equal(unname(mx_new), rep(unname(mx_new[1]), 3), tolerance = 1e-6)
})

test_that("LifeTable validates the close argument", {
  xo  <- c(0, 1, seq(5, 75, by = 5))
  mxo <- rep(0.01, length(xo))
  expect_error(LifeTable(xo, mx = mxo, close = "notalaw"),
               regexp = "'close' must be")
  expect_error(LifeTable(xo, mx = mxo, close = c("kannisto", "gompertz")),
               regexp = "'close' must be")
  # "standard" is the default reciprocal close
  expect_identical(LifeTable(xo, mx = mxo, close = "standard")$lt,
                   LifeTable(xo, mx = mxo)$lt)
})

test_that("LifeTable warns and keeps the observed rate when the close fails", {
  # Two ages cannot support a three-parameter-plus fit.
  expect_warning(LifeTable(c(0, 1), mx = c(0.05, 0.005), close = "kannisto"),
                 regexp = "could not be fitted")
})

test_that("LifeTable extends and closes the table at omega", {
  xo  <- c(0, 1, seq(5, 75, by = 5))
  mxo <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
           .004, .005, .006, .0093, .0129, .019, .031, .049, .084)
  LT <- LifeTable(xo, mx = mxo, omega = 110)$lt
  N  <- nrow(LT)

  # the table now runs to omega in 5-year steps above age 5
  expect_equal(tail(LT$x, 1), 110)
  expect_equal(diff(LT$x[-c(1, 2)]), rep(5, N - 3))
  # and it is closed by the same rule as before, at the new open age
  expect_equal(LT$qx[N], 1, tolerance = 1e-12)
  expect_equal(LT$ax[N], 1/LT$mx[N], tolerance = 1e-12)
  expect_equal(LT$ex[N], 1/LT$mx[N], tolerance = 1e-12)
  # the observed rates below the old open age are untouched
  expect_equal(LT$mx[1:(length(mxo) - 1)], mxo[1:(length(mxo) - 1)],
               tolerance = 1e-12)
  # the extrapolated rates keep rising
  expect_true(all(diff(LT$mx[(length(mxo) - 1):N]) > 0))
})

test_that("LifeTable closes at omega for every input index", {
  xo  <- c(0, 1, seq(5, 75, by = 5))
  mxo <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
           .004, .005, .006, .0093, .0129, .019, .031, .049, .084)
  A <- LifeTable(xo, mx = mxo)$lt
  e0 <- vapply(c("qx", "lx", "dx"), function(idx) {
    L <- switch(idx,
                qx = LifeTable(xo, qx = A$qx, omega = 110),
                lx = LifeTable(xo, lx = A$lx, omega = 110),
                dx = LifeTable(xo, dx = A$dx, omega = 110))
    L$lt$ex[1]
  }, 0)
  expect_equal(unname(e0), rep(unname(e0[1]), 3), tolerance = 1e-8)
})

test_that("LifeTable validates and warns about omega", {
  xo  <- c(0, 1, seq(5, 75, by = 5))
  mxo <- rep(0.01, length(xo))
  expect_error(LifeTable(xo, mx = mxo, omega = "x"),
               regexp = "'omega' must be a single finite number")
  expect_warning(LifeTable(xo, mx = mxo, omega = 75),
                 regexp = "not greater than the last age")
  # the extension is dropped, with a warning, when the law cannot be fitted
  expect_warning(LifeTable(c(0, 1), mx = c(0.05, 0.005), omega = 110),
                 regexp = "could not be computed")
})

test_that("LifeTable closes or extends abridged tables for every column", {
  xo  <- c(0, 1, seq(5, 75, by = 5))
  mxo <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
           .004, .005, .006, .0093, .0129, .019, .031, .049, .084)
  M   <- cbind(a = mxo, b = mxo * 1.1)

  # in place: the grid is unchanged, every column closed
  LC <- LifeTable(x = xo, mx = M, close = "kannisto")$lt
  expect_equal(sort(unique(LC$LT)), c("a", "b"))
  expect_equal(nrow(LC), 2 * length(xo))
  expect_true(all(LC$qx[LC$x == 75] == 1))

  # extended: the grid grows to omega for every column
  LE <- LifeTable(x = xo, mx = M, omega = 110)$lt
  expect_equal(nrow(LE), 2 * 24)
  expect_true(all(LE$qx[LE$x == 110] == 1))
})


# The ex input (issue #6): build a life table that reproduces a supplied curve
# of life expectancy. The round trip mx -> table -> ex -> table must recover the
# table, because the inverse uses the same interval identity the forward build
# does, and the ax the forward rule would have assigned.
test_that("LifeTable reproduces a table entered from its own e(x)", {
  for (gr in list(0:105, c(0, 1, seq(5, 105, by = 5)),
                  c(0, 1, seq(5, 75, by = 5)), 0:95, 60:105)) {
    xs <- gr
    ms <- ahmd$mx[paste0(xs), 1]
    A  <- LifeTable(xs, mx = ms)
    B  <- LifeTable(xs, ex = A$lt$ex)
    n  <- length(xs) - 1

    expect_equal(B$lt$ex, A$lt$ex, tolerance = 1e-8)
    expect_equal(B$lt$qx, A$lt$qx, tolerance = 1e-8)
    expect_equal(B$lt$mx[seq_len(n)], A$lt$mx[seq_len(n)], tolerance = 1e-8)
    expect_equal(B$lt$ax, A$lt$ax, tolerance = 1e-6)
  }
})

test_that("the ex input honours a supplied ax exactly", {
  xs <- 0:105
  ms <- ahmd$mx[paste0(xs), 1]
  A  <- LifeTable(xs, mx = ms, ax = rep(0.5, length(xs)))
  B  <- LifeTable(xs, ex = A$lt$ex, ax = rep(0.5, length(xs)))
  n  <- length(xs) - 1

  expect_equal(B$lt$ex, A$lt$ex, tolerance = 1e-9)
  expect_equal(B$lt$mx[seq_len(n)], A$lt$mx[seq_len(n)], tolerance = 1e-9)
})

test_that("the ex input reproduces the table under every ax method", {
  # The Coale-Demeny childhood rule makes the second interval's ax a function
  # of the first interval's rate, so this pins the non-local case as well as
  # the local ones. The round trip is exact for every method and sex, in the
  # ratio columns (ex, qx, ax); the mx column is compared too for the default
  # method, where the forward table is internally consistent.
  for (gr in list(0:105, c(0, 1, seq(5, 105, by = 5)),
                  c(0, 1, seq(5, 75, by = 5)))) {
    for (am in c("andreev_kingkade", "cfm", "preston", "coale_demeny")) {
      for (sx in list(NULL, "female", "male")) {
        A <- suppressWarnings(LifeTable(gr, mx = ahmd$mx[paste0(gr), 1],
                                        ax = am, sex = sx))
        B <- suppressWarnings(LifeTable(gr, ex = A$lt$ex, ax = am, sex = sx))
        expect_equal(B$lt$ex, A$lt$ex, tolerance = 1e-8)
        expect_equal(B$lt$qx, A$lt$qx, tolerance = 1e-8)
        expect_equal(B$lt$ax, A$lt$ax, tolerance = 1e-6)
      }
    }
  }
})

test_that("the ex input reproduces the rate column for the default method", {
  # A rate column that satisfies the table's own interval identity, which the
  # default method does, must be recovered exactly, including the fast closing
  # intervals where the 1/mx cap binds.
  for (gr in list(0:105, c(0, 1, seq(5, 105, by = 5)),
                  as.numeric(rownames(ahmd$mx)))) {
    A <- suppressWarnings(LifeTable(gr, mx = ahmd$mx[paste0(gr), 1]))
    B <- suppressWarnings(LifeTable(gr, ex = A$lt$ex))
    expect_equal(B$lt$mx, A$lt$mx, tolerance = 1e-8)
  }
})

test_that("the ex input accepts a matrix and keeps the shape", {
  xs <- c(0, 1, seq(5, 105, by = 5))
  A  <- LifeTable(xs, mx = ahmd$mx[paste0(xs), 1])
  E  <- cbind(a = A$lt$ex, b = A$lt$ex + 0.5)

  M <- LifeTable(xs, ex = E)$lt
  expect_equal(sort(unique(M$LT)), c("a", "b"))
  expect_equal(nrow(M), 2 * length(xs))
})

test_that("the ex input is not required to fall with age", {
  # Life expectancy at birth below life expectancy at age one is normal when
  # infant mortality is high; the inverse must not reject it.
  xs <- 0:105
  A  <- LifeTable(xs, mx = ahmd$mx[paste0(xs), 1])
  expect_true(A$lt$ex[1] < A$lt$ex[2])
  expect_silent(LifeTable(xs, ex = A$lt$ex))
})

test_that("an infeasible ex is an error naming the age", {
  xs <- 0:105
  A  <- LifeTable(xs, mx = ahmd$mx[paste0(xs), 1])

  # a curve that rises where no rise is possible
  eb <- A$lt$ex
  eb[50] <- eb[51] + 2
  expect_error(LifeTable(xs, ex = eb), regexp = "not a feasible life table")

  # a missing value in the curve, at any age, including the oldest ages where
  # a rate would instead be repaired
  for (age in c(10, 99, 100, 105)) {
    en <- A$lt$ex
    en[age + 1] <- NA
    expect_error(LifeTable(xs, ex = en), regexp = "missing or non-finite")
  }
})

test_that("the ex input forwards close and omega", {
  xo <- c(0, 1, seq(5, 75, by = 5))
  mo <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
          .004, .005, .006, .0093, .0129, .019, .031, .049, .084)
  A  <- LifeTable(xo, mx = mo)

  # in place: the grid is unchanged
  C  <- LifeTable(xo, ex = A$lt$ex, close = "kannisto")$lt
  expect_equal(nrow(C), length(xo))
  expect_true(C$qx[nrow(C)] == 1)

  # extended: the grid grows to omega
  O  <- LifeTable(xo, ex = A$lt$ex, omega = 110)$lt
  expect_true(nrow(O) > length(xo))
  expect_true(all(O$qx[O$x == 110] == 1))
})
