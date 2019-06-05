# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: MIT
# Last update: Wed Jun 05 14:42:27 2019
# --------------------------------------------------- #
remove(list = ls())


law = "HP"
C2 = c(0.00223, 0.01461, 0.12292, 0.00091,
       2.75201, 29.01877, 0.00002, 1.11411)
L1  <- LawTable(x = 0:110, par = C2, law = law)$lt
qx1 <- HP(x = 0:110, par = C2)$hx
L1b <- LifeTable(x = 0:110, qx = qx1)$lt
L2  <- LawTable(x = 3:110, par = C2, law = law)$lt


test_that("Test that LawTable results are compatible with LifeTable results", {
  expect_identical(qx1, L1$qx)
  expect_identical(L1, L1b)
  expect_equal(L1[L1$x == 3, "ex"], L2[L2$x == 3, "ex"])
})


# ----------------------------------------------

x = 0:80
mx = ahmd$mx[paste(x), 3:4]
M <- MortalityLaw(x, mx = mx, law = "thiele")
C3 <- coef(M)

expect_s3_class(LawTable(x, par = C3, law = "thiele"), "LifeTable")
