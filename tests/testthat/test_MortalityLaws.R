rm(list = ls())
library(MortalityLaws)

# test 1: ---------------------------------------
# Test all models on with ages
yr <- 1950
ages <- list(infancy = 0:15, 
             hump = 16:30, 
             adulthood = 30:75,
             adult_old = 30:100, 
             old_age = 76:100, 
             full = 0:100)

aLaws <- availableLaws()
N     <- nrow(aLaws$table)

for (k in 1:N) {
  type <- aLaws$table$TYPE[k]
  X    <- c(ages[type][[1]])
  mx   <- ahmd$mx[paste(X), paste(yr)]
  if (min(X) > 1) X <- X - min(X) + 1
  LAW  <- as.character(aLaws$table$CODE[k])
  M    <- paste0("M", k)
  assign(M, MortalityLaw(X, mx = mx, law = LAW, opt.method = 'LF2'))
}


# for (p in 1:27) {
#   model = paste0("M",p)
#   print(model)
#   plot(get(model))
#   legend("topleft", legend = model)
#   Sys.sleep(2)
# }


testMortalityLaw <- function(Y){
  test_that("Test MortalityLaw function", {
    expect_s3_class(Y, "MortalityLaw")
    expect_output(print(Y))
    expect_output(print(summary(Y)))
    expect_false(is.null(plot(M1)))
    expect_true(all(fitted(Y) >= 0))
    expect_true(all(coef(Y) >= 0))
    p = predict(Y, 1:100)
    expect_true(all(p >= 0))
  })
}

for (i in 1:N) testMortalityLaw(Y = get(paste0("M",i)))
  

# test 2: ---------------------------------------
# fit.this.x

x  <- 45:75
Dx <- ahmd$Dx[paste(x), paste(yr)]
Ex <- ahmd$Ex[paste(x), paste(yr)]

Mmak <- MortalityLaw(x = x - 44, Dx = Dx, Ex = Ex, 
                     law = 'makeham', fit.this.x = 50:70 - 44)

expect_false(is.null(plot(Mmak)))

expect_error(MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = 'makeham', 
                          fit.this.x = 48:52))
expect_error(MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = 'makeham', 
                          fit.this.x = 40:80))

# Test 3: ---------------------------------------
# custom.law
my_gompertz <- function(x, par = c(b = 0.13, m = 45)){
  hx  <- with(as.list(par), b*exp(b*(x - m)) )
  return(as.list(environment())) # return everything inside this function
}

Mmygom <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, custom.law = my_gompertz)
expect_false(is.null(plot(Mmygom)))

# test 4: ---------------------------------------
# matrix input
x  <- 0:100
mx <- ahmd$mx[paste(x), ] # select data

MHP <- MortalityLaw(x = x, mx = mx, law = 'HP', opt.method = 'LF3') # fit qx values
testMortalityLaw(MHP)
expect_error(plot(MHP))
expect_error(MortalityLaw(x = x, mx = mx, law = 'law_not_available'))
expect_error(MortalityLaw(x = x, mx = mx, law = 'HP', opt.method = "LF_not_available"))

# test 5: ---------------------------------------
# qx - kostaki
qx <- LifeTable(x, mx = mx[, 4])$lt$qx
Mkos <- MortalityLaw(x = x, qx = qx, law = 'kostaki', opt.method = 'binomialL') # fit qx values
testMortalityLaw(Mkos)
expect_output(print(summary(Mkos)))
expect_false(is.na(logLik(Mkos)))


# test 6: ---------------------------------------
# Test that all the laws return positive values
L <- availableLaws()
laws <- L$table$CODE

for (i in laws) {
  hx = eval(call(i, x = 1:100))$hx
  cond = all(hx >= 0)
  cat(i, ": ", cond, sep = "", "\n")
  expect_true(cond)
  
  cond2 = any(is.na(hx))
  expect_false(cond2)
}

