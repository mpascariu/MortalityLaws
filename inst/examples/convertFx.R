# ---- Basic conversions ----

x  <- 0:110
mx <- ahmd$mx

# Convert death rates to death probabilities
qx <- convertFx(x, data = mx, from = "mx", to = "qx")

# Convert death rates to death distribution
dx <- convertFx(x, data = mx, from = "mx", to = "dx")

# Convert death rates to survivorship
lx <- convertFx(x, data = mx, from = "mx", to = "lx")

# Convert death rates to life expectancy
ex <- convertFx(x, data = mx, from = "mx", to = "ex")

