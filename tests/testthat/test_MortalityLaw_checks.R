# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-04
# --------------------------------------------

# Tests for the input-validation helpers behind MortalityLaw(): the value,
# data and age checks in R/MortalityLaw_check.R, the starting-parameter checks
# in R/MortalityLaw_models.R and the two printing/string helpers in R/utils.R.
# Each check is reached through MortalityLaw() wherever a user-visible route
# exists, so the assertion pins the message a user actually sees; the two
# helpers that have no user-facing caller (bring_parameters()'s unknown-law
# guard and substr_right()) are called by name and the comment says why.

# A short but realistic adult schedule. The values only need to be valid numeric
# input: every case below errors before the optimiser is reached.
x6  <- 0:5
mx6 <- c(0.005, 0.001, 0.0008, 0.0009, 0.0011, 0.0014)


test_that("MortalityLaw rejects a non-numeric data vector", {
  # check_values() must report the offending vector by name instead of letting
  # a character Ex reach the optimiser. Ex is the only vector that can be
  # reached this way: it is checked after Dx, which keeps the case numeric and
  # so routes the call through the single-curve fit (R/MortalityLaw_check.R:17).
  expect_error(
    MortalityLaw(x = x6, Dx = rep(100, 6), Ex = rep("1000", 6),
                 law = "gompertz"),
    regexp = "'Ex' must be a numeric vector"
  )
})


test_that("MortalityLaw rejects missing or infinite data values", {
  # A missing rate carries no information about the fit; it must be reported
  # rather than silently propagated (R/MortalityLaw_check.R:21-22).
  mxa <- mx6
  mxa[3] <- NA
  expect_error(
    MortalityLaw(x = x6, mx = mxa, law = "gompertz"),
    regexp = "'mx' contains missing or infinite values"
  )

  mxi <- mx6
  mxi[3] <- Inf
  expect_error(
    MortalityLaw(x = x6, mx = mxi, law = "gompertz"),
    regexp = "'mx' contains missing or infinite values"
  )
})


test_that("MortalityLaw requires strictly positive exposure counts", {
  # Ex is a person-years count: a zero exposure makes the rate undefined, so
  # the positive check rejects it (R/MortalityLaw_check.R:26-27).
  Ex <- rep(1000, 6)
  Ex[4] <- 0
  expect_error(
    MortalityLaw(x = x6, Dx = rep(100, 6), Ex = Ex, law = "gompertz"),
    regexp = "'Ex' must contain strictly positive values"
  )
})


test_that("MortalityLaw rejects negative rate values", {
  # A hazard cannot be negative; the non-positive check catches it for the
  # rates that only require non-negativity (R/MortalityLaw_check.R:31-32).
  mxneg <- mx6
  mxneg[2] <- -0.001
  expect_error(
    MortalityLaw(x = x6, mx = mxneg, law = "gompertz"),
    regexp = "'mx' must not contain negative values"
  )
})


test_that("MortalityLaw checks that qx matches the age vector", {
  # The qx path has its own length guard, separate from the mx one
  # (R/MortalityLaw_check.R:58).
  expect_error(
    MortalityLaw(x = x6, qx = c(0.01, 0.02, 0.03), law = "gompertz"),
    regexp = "x and qx do not have the same length"
  )
})


test_that("MortalityLaw validates the age vector", {
  # 'x' must be numeric (R/MortalityLaw_check.R:94).
  expect_error(
    MortalityLaw(x = as.character(x6), mx = mx6, law = "gompertz"),
    regexp = "'x' must be a numeric vector"
  )

  # ... finite: a missing age makes the schedule unidentifiable
  # (R/MortalityLaw_check.R:98).
  xn <- x6
  xn[6] <- NA
  expect_error(
    MortalityLaw(x = xn, mx = mx6, law = "gompertz"),
    regexp = "'x' contains missing or infinite values"
  )

  # ... non-negative (R/MortalityLaw_check.R:102).
  xneg <- x6
  xneg[1] <- -1
  expect_error(
    MortalityLaw(x = xneg, mx = mx6, law = "gompertz"),
    regexp = "'x' must not contain negative values"
  )

  # ... and unique: a repeated age would count the same observation twice
  # (R/MortalityLaw_check.R:106).
  xdup <- x6
  xdup[3] <- 1
  expect_error(
    MortalityLaw(x = xdup, mx = mx6, law = "gompertz"),
    regexp = "'x' must not contain duplicated ages"
  )
})


test_that("MortalityLaw validates user-supplied starting parameters", {
  # parS is passed straight through to bring_parameters() -> check_parameters()
  # inside choose_optim(), so the public route reaches both guards.
  # A character parS is not a numeric vector (R/MortalityLaw_models.R:762).
  expect_error(
    MortalityLaw(x = x6, mx = mx6, law = "gompertz", parS = "start"),
    regexp = "'par' for law 'gompertz' must be a numeric vector"
  )

  # gompertz takes A and B; an unnamed vector of the wrong length is rejected
  # with the expected names and the count supplied
  # (R/MortalityLaw_models.R:776-780).
  expect_error(
    MortalityLaw(x = x6, mx = mx6, law = "gompertz",
                 parS = c(0.001, 0.1, 0.2)),
    regexp = "must have 2 elements \\(A, B\\); got 3"
  )
})


test_that("bring_parameters rejects an unknown law name", {
  # No public route reaches this guard: MortalityLaw() resolves the law name
  # through availableLaws() first, and the law functions (gompertz() and the
  # rest) each pass their own hard-coded name. It is therefore called by name
  # (R/MortalityLaw_models.R:859).
  expect_error(
    bring_parameters(law = "not_a_law"),
    regexp = "Unknown mortality law 'not_a_law'"
  )

  # The guard is the exceptional path: a known law still yields its named
  # defaults, which is what the law functions rely on.
  expect_named(bring_parameters(law = "gompertz"), c("A", "B"))
})


test_that("head_tail shows the head and tail of a matrix", {
  # head_tail() is the printing helper behind summary.MortalityLaw() when the
  # coefficient table is a matrix (a multi-curve fit). The matrix branch
  # converts the matrix to a data frame before taking head and tail
  # (R/utils.R:30). It is exercised directly on a matrix; the shape it returns
  # is the contract, not the internal conversion.
  M   <- matrix(1:12, nrow = 6, dimnames = list(NULL, c("A", "B")))
  out <- head_tail(M, hlength = 2, tlength = 2)

  expect_s3_class(out, "data.frame")
  expect_equal(dim(out), c(5, 2))       # 2 head + 1 ellipsis + 2 tail rows
  expect_true(all(out[3, ] == "..."))   # the separator row
  expect_equal(unname(out[1:2, 1]), as.character(M[1:2, 1]))
  expect_equal(unname(out[4:5, 1]), as.character(M[5:6, 1]))
})


test_that("substr_right returns the last n characters", {
  # substr_right() has no caller inside the package; it is the string helper
  # used to build short codes out of longer strings, so it is called directly
  # (R/utils.R:73-74).
  expect_equal(substr_right("abcdef", 3), "def")
  expect_equal(substr_right(c("abc", "wxyz"), 2), c("bc", "yz"))

  # fewer characters than requested returns the whole string ...
  expect_equal(substr_right("abc", 10), "abc")

  # ... and the empty string stays empty.
  expect_equal(substr_right("", 2), "")
})
