rm(list = ls())


# Example 1: ---------------------------------------
# Fit Makeham model for year of 1950.

yr <- 1950
x  <- 45:75
Dx <- ahmd$Dx[paste(x), paste(yr)]
Ex <- ahmd$Ex[paste(x), paste(yr)]

M1 <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = 'makeham', fit.this.x = 48:72)
expect_false(is.null(plot(M1)))

# Example 2: ---------------------------------------

my_gompertz <- function(x, par = c(b = 0.13, m = 45)){
  hx  <- with(as.list(par), b*exp(b*(x - m)) )
  return(as.list(environment())) # return everything inside this function
}

M2 <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, custom.law = my_gompertz)
expect_false(is.null(plot(M2)))

# Example 3: ---------------------------------------

x  <- 0:100
mx <- ahmd$mx[paste(x), ] # select data

M3 <- MortalityLaw(x = x, mx = mx, law = 'HP', opt.method = 'LF2') # fit qx values
expect_error(plot(M3))

qx <- LifeTable(x, mx = mx[, 4])$lt$qx
M4 <- MortalityLaw(x = x, qx = qx, law = 'kostaki', opt.method = 'LF2') # fit qx values
expect_output(print(plot(M4)))

testMortalityLaw <- function(Y){
  test_that("Test MortalityLaw function", {
    expect_s3_class(Y, "MortalityLaw")
    expect_output(print(Y))
    expect_output(print(summary(Y)))
    expect_true(all(fitted(Y) >= 0))
    expect_true(all(coef(Y) >= 0))
  })
}

testMortalityLaw(M1)
testMortalityLaw(M2)
testMortalityLaw(M3)
testMortalityLaw(M4)



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

