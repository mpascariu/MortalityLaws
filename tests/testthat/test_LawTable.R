# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-05 18:51:22
# --------------------------------------------

# Test that LawTable() produces results consistent with direct law computation
# and LifeTable(). Logic:
#   L1  = LawTable from HP law parameters -> life table.
#   qx1 = Direct hazard computation from the HP function at the same parameters.
#   L1b = LifeTable constructed from qx1 (should match L1 exactly).
#   L2  = LawTable on a restricted age range (3:110) - ex at age 3 should match L1.
# Together these verify that LawTable is a correct wrapper that (a) evaluates the
# law function and (b) feeds it into LifeTable correctly.
law <- "HP"
C2  <- c(0.00223, 0.01461, 0.12292, 0.00091,
         2.75201, 29.01877, 0.00002, 1.11411)
# The q[x] input is unclosed at the last age, which LifeTable closes with a
# warning (contract 3); the closure is pinned in its own test below.
L1  <- suppressWarnings(LawTable(x = 0:110, par = C2, law = law)$lt)
qx1 <- HP(x = 0:110, par = C2)$hx
L1b <- suppressWarnings(LifeTable(x = 0:110, qx = qx1)$lt)
L2  <- suppressWarnings(LawTable(x = 3:110, par = C2, law = law)$lt)


test_that("LawTable results are compatible with LifeTable results", {
  n <- length(qx1)
  # The life table closes with q[N] = 1, so the raw law value is only
  # comparable below the closing age.
  expect_equal(qx1[-n], L1$qx[-n], tolerance = 1e-12)
  expect_identical(L1$qx[n], 1)
  expect_equal(L1, L1b)                            # Same qx, same table
  expect_equal(L1$ex[L1$x == 3], L2$ex[L2$x == 3]) # ex at age 3 consistent
})

test_that("LifeTable closes an unclosed qx input with a warning", {
  expect_warning(LifeTable(x = 0:110, qx = qx1), regexp = "'qx' is not closed")
})


# ----------------------------------------------
# Test LawTable with coefficients estimated from real data (Thiele law on AHMD).
# This verifies the end-to-end workflow: estimate -> parameter table -> life table.
test_that("LawTable works with a matrix of estimated parameters", {
  x  <- 0:80
  mx <- ahmd$mx[paste(x), 3:4]
  # the optimiser may warn while probing extreme parameter regions
  M  <- suppressWarnings(MortalityLaw(x, mx = mx, law = "thiele"))
  C3 <- coef(M)
  expect_s3_class(LawTable(x, par = C3, law = "thiele"), "LifeTable")
})
