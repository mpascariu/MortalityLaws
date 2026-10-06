# Example 1: Fitting the Makeham model --------------------------
x  <- 45:75
Dx <- ahmd$Dx[paste(x), "1950"]
Ex <- ahmd$Ex[paste(x), "1950"]

M1 <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = 'makeham')

M1
ls(M1)
coef(M1)
summary(M1)
fitted(M1)
predict(M1, x = 45:95)
plot(M1, which = 'fit')
plot(M1, which = 'diagnostics')


# Example 2: --------------------------
# We can fit the same model using a different data format
# and a different optimization method.
x  <- 45:75
mx <- ahmd$mx[paste(x), ]
M2 <- MortalityLaw(x = x, mx = mx, law = 'makeham', opt.method = 'LF1')
M2
fitted(M2)
predict(M2, x = 55:90)

# Example 3: --------------------------
# Now let's fit a mortality law that is not defined
# in the package, say a reparameterized Gompertz in
# terms of modal age at death
# hx = b*exp(b*(x-m)) (here b and m are the parameters to be estimated)

# A function with 'x' and 'par' as input has to be defined, which returns
# at least an object called 'hx' (hazard rate).
missov <- function(x, par = c(b = 0.13, M = 45)){
  hx  <- with(as.list(par), b*exp(b*(x - M)) )
  return(as.list(environment()))
}

M3 <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, custom.law = missov)
summary(M3)
plot(M3)
# predict M3 for different ages
predict(M3, x = 85:130)


# Example 4: --------------------------
# Fit Heligman-Pollard model for a single
# year in the dataset between age 0 and 100 and build a life table.

x  <- 0:100
mx <- ahmd$mx[paste(x), "1950"] # select data
M4 <- MortalityLaw(x = x, mx = mx, law = 'HP', opt.method = 'LF2')
M4
plot(M4, which = 'fit')
plot(M4, which = 'diagnostics')
plot(M4, which = 'both')

LifeTable(x = x, qx = fitted(M4))
