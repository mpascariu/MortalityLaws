library(testthat)
library(MortalityLaws)

# Example 3: ---------------------------------------
# Fit Heligman-Pollard model for every single year in the dataset between age 0 and 100.

ages <- 0:100
mx   <- ahmd$mx[paste(ages), ] # select data
qx   <- convertFx(mx, x = ages, type = 'mx', output = 'qx') # transform mx into qx
model3 <- MortalityLaw(x = ages, qx = qx, law = 'HP', how = 'LF2') # fit qx values

test_that("Fitted values and cofficients are positive", {
  expect_true(all(fitted(model3) >= 0))
  expect_true(all(coef(model3) >= 0))
  expect_true(all(qx >= 0))
})


# ----------------------------------------------
x <- as.numeric(rownames(ahmd$mx))
year <- 1900

mx <- ahmd$mx[, paste(year)]
Dx <- ahmd$Dx[, paste(year)]
Nx <- ahmd$Ex[, paste(year)]

LT1 <- LifeTable(x, mx = mx)$lt
LT2 <- LifeTable(x, Dx = Dx, Ex = Nx)$lt

test_that("LifeTable works fine", {
  expect_true(all(LT1 >= 0 | is.na(LT1)))
  expect_true(all(LT2 >= 0 | is.na(LT2)))
  expect_false(all(is.na(LT1$ex)))
  expect_identical(class(LT1$ex), "numeric")
  expect_true(LT1$ex[1] >= 0)
  expect_true(LT1$ex[1] >= rev(LT1$ex)[1])
})


