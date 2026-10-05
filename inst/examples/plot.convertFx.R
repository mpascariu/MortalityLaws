x  <- 0:105
mx <- ahmd$mx[paste(x), "1950"]
ex <- convertFx(x = x, data = mx, from = "mx", to = "ex")
plot(ex)
plot(ex, split = c(2, 1))
