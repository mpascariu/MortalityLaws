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

# ---- All 35 possible conversions ----

from <- c("mx", "qx", "dx", "lx", "ex")
to   <- c("mx", "qx", "dx", "lx", "Lx", "Tx", "ex")
K    <- expand.grid(from = from, to = to)

for (i in 1:nrow(K)) {
  In  <- as.character(K[i, "from"])
  Out <- as.character(K[i, "to"])
  N   <- paste0(Out, "_from_", In)
  cat(i, " Create", N, "\n")
  assign(N, convertFx(x = x, data = get(In), from = In, to = Out))
}

