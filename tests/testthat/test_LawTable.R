# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-05 18:51:22
# --------------------------------------------
remove(list = ls())


# Test that LawTable() produces results consistent with direct law computation
# and LifeTable(). Logic:
#   L1  = LawTable from HP law parameters -> life table.
#   qx1 = Direct hazard computation from the HP function at the same parameters.
#   L1b = LifeTable constructed from qx1 (should match L1 exactly).
#   L2  = LawTable on a restricted age range (3:110) — ex at age 3 should match L1.
# Together these verify that LawTable is a correct wrapper that (a) evaluates the
# law function and (b) feeds it into LifeTable correctly.
law = "HP"
C2 = c(0.00223, 0.01461, 0.12292, 0.00091,
       2.75201, 29.01877, 0.00002, 1.11411)
L1  <- LawTable(x = 0:110, par = C2, law = law)$lt
qx1 <- HP(x = 0:110, par = C2)$hx
L1b <- LifeTable(x = 0:110, qx = qx1)$lt
L2  <- LawTable(x = 3:110, par = C2, law = law)$lt


test_that("Test that LawTable results are compatible with LifeTable results", {
  expect_identical(qx1, L1$qx)           # LawTable hazard matches direct HP evaluation
  expect_identical(L1, L1b)               # LawTable + LifeTable agree on same qx
  expect_equal(L1[L1$x == 3, "ex"], L2[L2$x == 3, "ex"])  # ex at age 3 consistent
})


# ----------------------------------------------
# Test LawTable with coefficients estimated from real data (Thiele law on AHMD).
# This verifies the end-to-end workflow: estimate -> parameter table -> life table.
x = 0:80
mx = ahmd$mx[paste(x), 3:4]
M <- MortalityLaw(x, mx = mx, law = "thiele")
C3 <- coef(M)

expect_s3_class(LawTable(x, par = C3, law = "thiele"), "LifeTable")
