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


for (i in 1:N) {
  testMortalityLaw(get(paste0("M", i)))
}

for (j in 1:N) {
  testMortalityLaw(get(paste0("P", j)))
}


# test 2: ---------------------------------------
# fit.this.x

x  <- 45:75
Dx <- ahmd$Dx[paste(x), paste(yr)]
Ex <- ahmd$Ex[paste(x), paste(yr)]
T2 <- MortalityLaw(x = x - 44, Dx = Dx, Ex = Ex, law = 'makeham', fit.this.x = 50:70 - 44)
testMortalityLaw(T2)
expect_error(MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = 'makeham', fit.this.x = 48:52))
expect_error(MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = 'makeham', fit.this.x = 40:80))

# Test 3: ---------------------------------------
# custom.law
x  <- 45:75
Dx <- ahmd$Dx[paste(x), paste(yr)]
Ex <- ahmd$Ex[paste(x), paste(yr)]
my_gompertz <- function(x, par = c(b = 0.13, m = 45)){
  hx <- with(as.list(par), b*exp(b*(x - m)) )
  return(as.list(environment()))
}

expect_warning((T3 = MortalityLaw(x = x, Dx = Dx, Ex = Ex, custom.law = my_gompertz, scale.x = FALSE)))
testMortalityLaw(T3)

# test 4: ---------------------------------------
# test for invalid laws and optimization methods
mx  <- ahmd$mx[paste(0:100), 1] # select data
expect_error(MortalityLaw(x = 0:100, mx = mx, law = 'law_not_available'))
expect_error(MortalityLaw(x = 0:100, mx = mx, law = 'HP', opt.method = "LF_not_available"))
expect_message((HP4 = MortalityLaw(x = 0:100, mx = mx, law = 'HP', 
                                  opt.method = "poissonL")))
expect_true(is.numeric(AIC(HP4)))
expect_true(is.numeric(logLik(HP4)))

expect_error(predict(M27, x = 60:100)) # kannisto

# test 5: ---------------------------------------
# Test that all the laws return positive values
L <- availableLaws()
laws <- L$table$CODE

for (i in laws) {
  hx = eval(call(i, x = 1:100))$hx
  expect_true(all(hx >= 0))
  expect_false(any(is.na(hx)))
}







