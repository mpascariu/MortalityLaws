# convert death rates (mx) to life expectancies (ex)
x  <- 0:105
mx <- ahmd$mx[paste(x), "2010"]
ex <- convertFx(x = x, data = mx, from = "mx", to = "ex")
plot(ex)
plot(ex, split = c(2, 1))

# convert life expectancy (ex) to 1-year probability of dying (qx)
qx <- convertFx(x = x, data = ex, from = "ex", to = "qx")
plot(qx)
