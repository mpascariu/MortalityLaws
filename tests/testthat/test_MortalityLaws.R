# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-05 18:51:47
# --------------------------------------------
remove(list = ls())

# Test 1: Fit ALL available mortality laws to appropriate age ranges ----------
# Logic: Each mortality law has a TYPE (1=full, 2=adult, 3=old-age, etc.) which
# determines what age range is suitable. We:
#   (a) Loop over all laws in availableLaws()
#   (b) Select the age range appropriate for that law's TYPE
#   (c) Fit single-column data (M models) and two-column data (P models)
# This tests that MortalityLaw converges for every law on real mortality data.
yr <- 1950
ages <- list(infancy   = 0:15,
             hump      = 16:30,
             adulthood = 30:75,
             adult_old = 30:100,
             old_age   = 76:100,
             full      = 0:100)

aLaws <- availableLaws()
N     <- nrow(aLaws$table)

k = 30
# Build M models (single-column mortality data) and P models (two-column data)
for (k in 1:N) {
  type <- as.numeric(aLaws$table$TYPE[k])
  X    <- c(ages[type][[1]])
  mx   <- ahmd$mx[paste(X), ]
  sx   <- ifelse(min(X) > 1, TRUE, FALSE)
  LAW  <- as.character(aLaws$table$CODE[k])
  cat("M", k,": ", LAW, "\n", sep = "")

  assign(paste0("M", k), MortalityLaw(x   = X,
                                      mx  = mx[, 1:1],
                                      law = LAW,
                                      opt.method = 'LF2'))

  assign(paste0("P", k), MortalityLaw(x   = X,
                                      mx  = mx[, 1:2],
                                      law = LAW,
                                      opt.method = 'LF2'))
}


# Helper function: validates basic properties of a MortalityLaw fitted object.
# Checks: (1) correct S3 class, (2) print/summary work, (3) fitted values and
# coefficients are non-negative, (4) predictions are non-negative, and (5) plot
# either works (single column) or errors gracefully (multi-column data).
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

# ----------------------------------------------------------------------------
# Additional check: fit on the last law (HP/rogersplanck) using qx instead of mx,
# and with show = TRUE to test the verbose output. This verifies the function
# works with qx input and the show flag.
testMortalityLaw(
  MortalityLaw(x   = X,
               qx  = mx,
               law = LAW,
               opt.method = 'LF2',
               show = TRUE)
)

# Test 2: fit.this.x parameter ------------------------------------------------
# Logic: fit.this.x allows fitting the model on a subset of ages while evaluating
# the fitted curve at the full age range. This test verifies:
#   (a) fit.this.x works correctly when it is a contiguous sub-range of x.
#   (b) fit.this.x as a single value (not a range) should error — model cannot
#       be identified from one data point.
#   (c) fit.this.x extending outside the input x range should error — cannot
#       fit where there is no data.

x  <- 45:75
Dx <- ahmd$Dx[paste(x), paste(yr)]
Ex <- ahmd$Ex[paste(x), paste(yr)]
T2 <- MortalityLaw(x   = x - 44,
                   Dx  = Dx,
                   Ex  = Ex,
                   law = 'makeham',
                   fit.this.x = 50:70 - 44)

testMortalityLaw(T2)
expect_error(
  MortalityLaw(x   = x,
               Dx  = Dx,
               Ex  = Ex,
               law = 'makeham',
               fit.this.x = 48)
  )


expect_error(
  MortalityLaw(x   = x,
    Dx  = Dx,
    Ex  = Ex,
    law = 'makeham',
    fit.this.x = 40:80)
  )

# Test 3: Custom user-defined law ---------------------------------------------
# Logic: The function should accept a user-defined mortality law via custom.law.
# Here we define a simple Gompertz function and verify that MortalityLaw can
# estimate its parameters from data without having the law pre-registered.
my_gompertz <- function(x, par = c(b = 0.13, m = 45)){
  hx <- with(as.list(par), b*exp(b*(x - m)) )
  return(as.list(environment()))
}

T3 = MortalityLaw(x  = x,
                  Dx = Dx,
                  Ex = Ex,
                  custom.law = my_gompertz)
testMortalityLaw(T3)

# Test 4: Poisson optimization method and model utility functions ------------
# Logic: Tests (a) that using opt.method = "poissonL" with the HP law produces
# a message (since Poisson likelihood may have convergence notes), (b) that
# predict errors on negative ages, and (c) that generic model functions like
# AIC, logLik, df.residual, deviance, and summary are all correctly implemented
# for MortalityLaw objects.
mx  <- ahmd$mx[paste(0:100), 1] # select data
expect_message((HP4 = MortalityLaw(x   = 0:100,
                                   mx  = mx,
                                   law = 'HP',
                                   opt.method = "poissonL")))

expect_error(predict(HP4, x = -1:100))
expect_true(is.numeric(AIC(HP4)))
expect_true(is.numeric(logLik(HP4)))
expect_true(is.numeric(df.residual(HP4)))
expect_true(is.numeric(deviance(HP4)))
expect_true(class(summary(HP4)) == "summary.MortalityLaw")

# Test 5: All law functions return non-negative hazard values ------------------
# Logic: Calling each law function directly (e.g., HP(x=1:100)) should return
# non-negative hazard rates with no NA values. This tests the law implementations
# independently from the fitting routine.
L <- availableLaws()
laws <- L$table$CODE

for (i in laws) {
  hx = eval(call(i, x = 1:100))$hx
  expect_true(all(hx >= 0))
  expect_false(any(is.na(hx)))
}

# Test 6: Input validation error messages -----------------------------------
# Logic: Verify that MortalityLaw raises appropriate errors for:
#   (1) No law specified (mx provided but no law argument)
#   (2) Non-existent law name
#   (3) Non-existent optimization method
#   (4) Invalid show argument type (character instead of logical)
#   (5) x range too large (0:1000) relative to the number of data points
#   (6-7) Mismatched lengths between Dx and Ex arguments
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












