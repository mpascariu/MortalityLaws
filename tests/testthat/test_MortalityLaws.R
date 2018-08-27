rm(list = ls())

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

# Build M models
for (k in 1:N) {
  type <- aLaws$table$TYPE[k]
  X    <- c(ages[type][[1]])
  mx   <- ahmd$mx[paste(X), ]
  sx   <- ifelse(min(X) > 1, TRUE, FALSE)
  LAW  <- as.character(aLaws$table$CODE[k])
  cat("M", k,": ", LAW, "\n", sep = "")
  assign(paste0("M", k), MortalityLaw(X, mx = mx[, 1:1], law = LAW, 
                                      opt.method = 'LF2', scale.x = sx))
  assign(paste0("P", k), MortalityLaw(X, mx = mx[, 1:2], law = LAW, 
                                      opt.method = 'LF2', scale.x = sx))
}


testMortalityLaw <- function(Y){
  test_that("Test MortalityLaw function", {
    expect_s3_class(Y, "MortalityLaw")
    expect_output(print(Y))
    expect_output(print(summary(Y)))
    expect_true(all(fitted(Y) >= 0))
    expect_true(all(coef(Y) >= 0))
    pred = predict(Y, x = Y$input$x)
    expect_true(all(pred >= 0))
    
    if (is.matrix(fitted(Y))) {
      expect_error(plot(Y))
      expect_true(is.matrix(pred))
    } else {
      expect_false(is.null(plot(Y)))
    }
  })
}


for (i in 1:N) testMortalityLaw(get(paste0("M", i)))

for (j in 1:N) testMortalityLaw(get(paste0("P", j)))


# test 2: ---------------------------------------
# fit.this.x

x  <- 45:75
Dx <- ahmd$Dx[paste(x), paste(yr)]
Ex <- ahmd$Ex[paste(x), paste(yr)]
T2 <- MortalityLaw(x = x - 44, Dx = Dx, Ex = Ex, law = 'makeham', fit.this.x = 50:70 - 44)
testMortalityLaw(T2)
expect_error(MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = 'makeham', fit.this.x = 48))
expect_error(MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = 'makeham', fit.this.x = 40:80))

# Test 3: ---------------------------------------
# custom.law
my_gompertz <- function(x, par = c(b = 0.13, m = 45)){
  hx <- with(as.list(par), b*exp(b*(x - m)) )
  return(as.list(environment()))
}

T3 = MortalityLaw(x = x, Dx = Dx, Ex = Ex, custom.law = my_gompertz)
testMortalityLaw(T3)

# test 4: ---------------------------------------
mx  <- ahmd$mx[paste(0:100), 1] # select data
expect_message((HP4 = MortalityLaw(x = 0:100, mx = mx, law = 'HP', opt.method = "poissonL")))

expect_error(predict(HP4, x = -1:100))
expect_true(is.numeric(AIC(HP4)))
expect_true(is.numeric(logLik(HP4)))
expect_true(is.numeric(df.residual(HP4)))
expect_true(is.numeric(deviance(HP4)))
expect_true(class(summary(HP4)) == "summary.MortalityLaw")

# test 5: ---------------------------------------
# Test that all the laws return positive values
L <- availableLaws()
laws <- L$table$CODE

for (i in laws) {
  hx = eval(call(i, x = 1:100))$hx
  expect_true(all(hx >= 0))
  expect_false(any(is.na(hx)))
}

# test 6: ---------------------------------------
# Test error messages
x <- 0:100
mx <- ahmd$mx[paste(x), 1] # select data
Dx = ahmd$Dx[paste(x), 1]
Ex = ahmd$Ex[paste(x), 1]

expect_error(MortalityLaw(x, mx = mx))
expect_error(MortalityLaw(x, mx = mx, law = 'law_not_available'))
expect_error(MortalityLaw(x, mx = mx, law = 'HP', opt.method = "LF_not_available"))
expect_error(MortalityLaw(x, mx = mx, law = 'HP', show = "TRUEx"))
expect_error(MortalityLaw(0:1000, mx = mx, law = 'HP'))
expect_error(MortalityLaw(x, Dx = Dx, Ex = Ex[-1], law = 'HP'))
expect_error(MortalityLaw(x, Dx = Dx[-1], Ex = Ex, law = 'HP'))















