rm(list = ls())

# Example 3: ---------------------------------------

x  <- 0:100
mx <- ahmd$mx[paste(x), ] # select data
qx <- convertFx(mx, x = x, type = 'mx', output = 'qx') # transform mx into qx

test_that("convertFx", {
  expect_false(any(is.na(qx)))
  expect_true(all(qx >= 0))
})


M3 <- MortalityLaw(x = x, qx = qx, law = 'HP', opt.method = 'LF2') # fit qx values

test_that("Test for positive values", {
  expect_true(all(fitted(M3) >= 0))
  expect_true(all(coef(M3) >= 0))
})

test_that("Generics", {
  expect_s3_class(M3, "MortalityLaw")
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

