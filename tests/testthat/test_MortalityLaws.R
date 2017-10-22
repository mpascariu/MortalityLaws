rm(list = ls())


# Example 1: ---------------------------------------
# Fit Makeham model for year of 1950.

yr <- 1950
x  <- 45:75
Dx <- ahmd$Dx[paste(x), paste(yr)]
Ex <- ahmd$Ex[paste(x), paste(yr)]

M1 <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = 'makeham')

expect_false(is.null(plot(M1)))
expect_output(print(summary(M1)))
expect_output(print(M1))

# Example 3: ---------------------------------------

x  <- 0:100
mx <- ahmd$mx[paste(x), ] # select data

M3 <- MortalityLaw(x = x, mx = mx, law = 'HP', opt.method = 'LF2') # fit qx values

expect_error(plot(M3))
test_that("Test MortalityLaw function", {
  expect_s3_class(M3, "MortalityLaw")
  expect_output(print(M3))
  expect_true(all(fitted(M3) >= 0))
  expect_true(all(coef(M3) >= 0))
})



# ----------------------------------------------
# Test that all the laws return positive values
L <- availableLaws()
laws <- L$table$CODE

for (i in laws) {
  hx = eval(call(i, x = 1:20))$hx
  cond = all(hx >= 0)
  cat(i, ": ", cond, sep = "", "\n")
  expect_true(cond)
}

