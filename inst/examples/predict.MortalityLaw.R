# Extrapolate old-age mortality with the Kannisto model
# Fit ages 80-94 and extrapolate up to 120.

Mx <- ahmd$mx[paste(80:94), "1950"]
M1 <- MortalityLaw(x = 80:94, mx  = Mx, law = 'kannisto')
fitted(M1)
predict(M1, x = 80:120)

# See more examples in MortalityLaw function help page.
