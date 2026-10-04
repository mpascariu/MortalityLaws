# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-04 18:20:11
# --------------------------------------------

# Branch coverage for the LifeTable pipeline: input validation, the ax
# estimators, the two closing mechanisms and the inverse life table. Each
# table below is built to reach one specific guard or fallback; what is
# asserted is the user-visible consequence - an error naming the offending
# input, a warning, or an identity the finished table must satisfy - never an
# internal intermediate value.

x   <- 0:5
m   <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06)
xo  <- c(0, 1, seq(5, 75, by = 5))
mxo <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
         .004, .005, .006, .0093, .0129, .019, .031, .049, .084)

test_that("the input case is detected when it is not supplied", {
  # LifeTable() always hands the detected case down; compute_life_table() must
  # detect it on its own as well, so the two entry paths cannot drift apart.
  expect_identical(compute_life_table(x = x, mx = m),
                   compute_life_table(x = x, mx = m, case = "C2_mx"))
})

test_that("a one-dimensional array is flattened to a single life table", {
  # A 1-d array is not a vector for is.vector(), so it has no dimension
  # structure to count tables over: it must be flattened to one numeric table,
  # which is the class the input gate accepts. The internal is called directly
  # because the public route flattens only a local copy and then fails on the
  # array shape further down the pipeline (repair_above_omega() asks a 1-d
  # array for ncol(), which is NA).
  K <- detect_case(mx = array(m))
  expect_identical(K$case, "C2_mx")
  expect_identical(K$iclass, "numeric")
  expect_identical(K$nLT, 1)
})

test_that("the class of the input is validated", {
  # A character vector is not one of the accepted input classes.
  expect_error(LifeTable(x, mx = "0.01"),
               regexp = "class of the input should be")
})

test_that("the input length is validated against the age vector", {
  # Every input column must carry one value per age before any value is
  # repaired, otherwise a shorter vector would silently be recycled.
  expect_error(LifeTable(0:105, mx = rep(0.01, 10)),
               regexp = "must have one value per age in 'x' \\(106 expected, got 10\\)")
})

test_that("the ax argument must name one method or be numeric", {
  # Two method names cannot select an ax rule.
  expect_error(LifeTable(x, mx = m, ax = c("cfm", "preston")),
               regexp = "'ax' must name a single method, not a vector")
  # A logical is neither a numeric ax nor a known method name.
  expect_error(LifeTable(x, mx = m, ax = TRUE),
               regexp = "'ax' must be a numeric scalar or vector")
})

test_that("a scalar ax is recycled over the age intervals", {
  # The scalar form is the documented shorthand for "the same value
  # everywhere"; it must cover every closed interval, while the open interval
  # keeps the closing rule (1/mx) and is reported as replaced.
  expect_warning(LT <- LifeTable(x, mx = m, ax = 0.5),
                 regexp = "open age interval")
  expect_equal(LT$lt$ax[LT$lt$x < max(x)], rep(0.5, length(x) - 1),
               tolerance = 1e-12)
  expect_equal(LT$lt$ax[length(x)], 1/LT$lt$mx[length(x)], tolerance = 1e-12)
  expect_equal(sum(LT$lt$dx), LT$lt$lx[1], tolerance = 1e-8)

  # The same recycling has to happen when the table is entered from e(x),
  # where the scalar is expanded before the inverse step runs.
  ex5 <- c(70, 69, 68, 67, 66, 65)
  expect_warning(A <- LifeTable(x, ex = ex5, ax = 0.5),
                 regexp = "open age interval")
  B <- suppressWarnings(LifeTable(x, ex = ex5, ax = rep(0.5, length(x))))
  expect_identical(A$lt, B$lt)
})

test_that("an all-zero rate column is returned without derived values", {
  # Zero rates mean nobody dies, so every derived column would be infinite;
  # the table is reported as uninformative (NA) instead of as infinite values.
  LT <- LifeTable(x, mx = rep(0, length(x)))
  expect_true(all(is.na(LT$lt[, !names(LT$lt) %in% c("x.int", "x")])))
  expect_identical(LT$lt$x, as.numeric(x))
})

test_that("a non-finite rate is replaced by the last finite rate", {
  # The rates above a non-finite entry cannot be repaired from it, so the
  # interval takes the last usable rate before the break and the table stays
  # computable. The 'cfm' ax keeps the default method's open-interval note,
  # which is unrelated to the repair, out of the run.
  expect_warning(LT <- LifeTable(x, mx = c(0.01, Inf, 0.03, 0.04, 0.05, 0.06),
                                 ax = "cfm"),
                 regexp = "missing or non-finite")
  expect_equal(LT$lt$mx[2], LT$lt$mx[1], tolerance = 1e-12)
  expect_true(all(is.finite(LT$lt$mx)))
  expect_equal(LT$lt$qx[length(x)], 1, tolerance = 1e-12)
})

test_that("an unusable open-interval rate falls back to half the last closed interval", {
  # A zero rate in the open interval cannot give the reciprocal ax and is not
  # missing either, so the interval must take half of the last closed interval
  # (2.5 years here) instead of an infinite average.
  LT <- LifeTable(xo, mx = c(mxo[-length(mxo)], 0), ax = "cfm")$lt
  N  <- nrow(LT)
  expect_equal(LT$ax[N], (xo[N] - xo[N - 1])/2, tolerance = 1e-12)
  expect_equal(LT$ex[N], LT$ax[N], tolerance = 1e-12)
  expect_equal(LT$Lx[N], LT$ax[N] * LT$dx[N], tolerance = 1e-12)
  expect_equal(LT$qx[N], 1, tolerance = 1e-12)
})

test_that("the open-interval fallback survives a missing preceding rate", {
  # With the interval before the open age missing, the open interval has no
  # finite neighbour to inherit from either; it must still land on the
  # half-width rule instead of Inf, while the rows above the gap stay unknown.
  mx <- c(mxo[-c(16, 17)], NA, 0)
  expect_warning(LT <- LifeTable(xo, mx = mx, ax = "cfm")$lt,
                 regexp = "missing or non-finite")
  N <- nrow(LT)
  expect_false(any(is.infinite(LT$ax) | is.nan(LT$ax)))
  expect_equal(LT$ax[N], 2.5, tolerance = 1e-12)
  expect_equal(LT$ex[N], 2.5, tolerance = 1e-12)
  expect_true(all(is.na(LT$ex[1:(N - 2)])))
})

test_that("a zero-width interval inherits the next interval's ax", {
  # A repeated age makes an interval of width zero, whose closed-form ax is
  # 0/0. The fallback must give it the next finite value: a NaN here would
  # poison every person-years column of the table.
  LT <- LifeTable(c(0, 0, 1, 2, 3, 4, 5),
                  mx = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07),
                  ax = "cfm")$lt
  expect_true(all(is.finite(LT$ax)))
  expect_equal(LT$ax[1], LT$ax[2], tolerance = 1e-12)
  expect_equal(LT$qx[1], 0, tolerance = 1e-12)   # no time to die
  expect_equal(LT$dx[1], 0, tolerance = 1e-12)
  expect_equal(LT$lx[1], LT$lx[2], tolerance = 1e-12)
})

test_that("a rate too small for the closed form keeps the midpoint ax", {
  # At mx = 1e-8 the closed form n + 1/m - n/q is pure cancellation (it would
  # return ~1.5 here); the series expansion is what keeps the value at the
  # n/2 limit, so the person-years of the interval stay equal to its survivors.
  LT <- LifeTable(x, mx = c(1e-8, 0.01, 0.02, 0.03, 0.04, 0.05), ax = "cfm")$lt
  expect_equal(LT$ax[1], 0.5, tolerance = 1e-6)
  expect_equal(LT$Lx[1], LT$lx[1], tolerance = 1e-6)
})

test_that("the Coale-Demeny rules reject a negative first-interval rate", {
  # The separation factors are a function of the infant rate; a negative m0
  # has no separation factor, so it is rejected rather than fitted.
  expect_error(
    LifeTable(x, mx = c(-0.01, 0.02, 0.03, 0.04, 0.05, 0.06),
              ax = "preston", sex = "female"),
    regexp = "must be greater than 0")
})

test_that("the closing law defaults to Kannisto", {
  # LifeTable() always resolves a law code before calling these helpers, so
  # the default is reached through the internals: it must be the same law.
  expect_identical(lt_close_model(xo, mxo),
                   lt_close_model(xo, mxo, law = "kannisto"))
  E <- lt_extend_omega(xo, mxo, omega = 110)
  K <- lt_extend_omega(xo, mxo, omega = 110, law = "kannisto")
  expect_identical(E, K)
  expect_true(all(diff(E$x) > 0))
})

test_that("the close keeps the observed rate when the law cannot be fitted", {
  # HP has more parameters than the three closing ages can identify, so the
  # close is refused and the table stays exactly the reciprocal-close table.
  A <- LifeTable(xo, mx = mxo)$lt
  expect_warning(LT <- LifeTable(xo, mx = mxo, close = "HP"),
                 regexp = "closing law 'HP' could not be fitted")
  expect_identical(LT$lt, A)
})

test_that("the close keeps the observed rate when the law predicts unusable values", {
  # Fitted over the whole table (fit_from = 0), the Wittstein law extrapolates
  # to rates the interval cannot use; the open interval keeps its observed
  # rate rather than a value the law cannot support.
  A <- LifeTable(xo, mx = mxo)$lt
  expect_warning(LT <- LifeTable(xo, mx = mxo, close = "wittstein",
                                 fit_from = 0),
                 regexp = "could not be fitted")
  expect_identical(LT$lt, A)
})

test_that("the close is abandoned when the survival integral underflows", {
  # Rates of 1e3 above the fitted window make the fitted Gompertz predict
  # ~1e5 at age 75, so the survival integral rounds to zero. The close must be
  # dropped (the observed rate is kept), not converted into an infinite rate.
  mx <- mxo
  mx[14:16] <- 1e3   # ages 60, 65 and 70, the closing law's fit window
  expect_warning(LT <- LifeTable(xo, mx = mx, close = "gompertz"),
                 regexp = "could not be fitted")
  expect_equal(LT$lt$mx[length(xo)], mxo[length(xo)], tolerance = 1e-12)
})

test_that("an omega before the next grid point extends nothing", {
  # The extension grid is built with the input's own step, so an omega that
  # does not reach the next age gives a single point: there is nothing to
  # extrapolate on and the table keeps its own open age.
  expect_warning(LT <- LifeTable(xo, mx = mxo, omega = 77),
                 regexp = "extension could not be computed")
  expect_identical(LT$lt$x, as.numeric(xo))
  expect_equal(tail(LT$lt$mx, 1), mxo[length(mxo)], tolerance = 1e-12)
})

test_that("the extension is dropped when the law cannot be fitted", {
  # Same refusal on the extension path: the grid may not grow when the law
  # that would fill it is not estimable.
  A <- LifeTable(xo, mx = mxo)$lt
  expect_warning(LT <- LifeTable(xo, mx = mxo, omega = 110, close = "HP"),
                 regexp = "extension could not be computed")
  expect_identical(LT$lt, A)
})

test_that("the extension is dropped when the law predicts unusable rates", {
  # Opperman is fitted to the falling rates 0-4 and turns negative just above
  # them, so the extrapolation to age 12 is refused and the table closes at 5.
  xs <- 0:5
  ms <- c(0.2, 0.1, 0.05, 0.03, 0.02, 0.01)
  expect_warning(LT <- LifeTable(xs, mx = ms, omega = 12, close = "opperman",
                                 fit_from = 0, ax = "cfm"),
                 regexp = "extension could not be computed")
  expect_identical(LT$lt$x, as.numeric(xs))
  expect_equal(LT$lt$mx[6], ms[6], tolerance = 1e-12)
})

test_that("the old-age repair reports and repairs the unusable rates", {
  # No caller passes verbose = TRUE (lt_repair_input() and lt_case_rates()
  # use the silent default), so the documented warning is reached directly.
  # The replacement is the highest usable rate observed at or above omega, not
  # the highest rate anywhere in the vector.
  ux <- rep(0.05, 106)
  ux[1]   <- 0.5    # larger, but below omega: must not be used
  ux[101] <- NA
  ux[102] <- 0
  ux[103] <- Inf
  ux[104] <- 0.2    # the largest usable rate above omega
  ux[105] <- NaN
  ux[106] <- 0
  expect_warning(out <- repair_above_omega(x = 0:105, ux = ux, omega = 100,
                                           verbose = TRUE),
                 regexp = "maximum observed value: 0.2")
  expect_equal(out[c(101, 102, 103, 105, 106)], rep(0.2, 5), tolerance = 1e-12)
  expect_equal(out[104], 0.2, tolerance = 1e-12)
  expect_equal(out[1:100], ux[1:100], tolerance = 1e-12)
})

test_that("the ex inverse recycles a scalar ax", {
  # compute_life_table() expands a scalar ax before the inverse runs, so the
  # recycling inside ex_inverse() is reached through the internal. The open
  # interval follows the inverse's own rule, a_N = e_N.
  ex5 <- c(70, 69.5, 69, 68.5, 68, 67.5)
  nx  <- rep(1, length(ex5))
  expect_warning(out <- ex_inverse(x = x, nx = nx, ex = ex5, ax = 0.5),
                 regexp = "open age interval")
  ref <- ex_inverse(x = x, nx = nx, ex = ex5, ax = c(rep(0.5, 5), ex5[6]))
  expect_identical(out, ref)
  expect_equal(out$ax[length(ex5)], ex5[length(ex5)], tolerance = 1e-12)
})

test_that("the ex errors name the offending interval", {
  # e(x) falling by far more than the one-year interval implies a negative
  # death probability at that age.
  ex5 <- c(70, 69.5, 69.1, 60, 59.5, 59)
  expect_error(LifeTable(x, ex = ex5, ax = c(rep(0.5, 5), ex5[6])),
               regexp = "probabilities outside \\[0, 1\\] at age\\(s\\) 2")

  # An ax above the person-years the interval can carry leaves a non-positive
  # balance, and the error names the interval that starts at age 4.
  ex6 <- c(70, 69, 68, 67, 66, 65)
  expect_error(LifeTable(x, ex = ex6, ax = c(0.5, 0.5, 0.5, 0.5, 70, ex6[6])),
               regexp = "starting at age 4 has a non-positive")
})
