# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Sun May 02 16:37:15 2021
# --------------------------------------------------- #
remove(list = ls())

# Data ---
x  <- 0:105
mx <- ahmd$mx[paste0(x),]

# mx to qx
qx <- convertFx(x, data = mx, from = "mx", to = "qx")
# mx to dx
dx <- convertFx(x, data = mx, from = "mx", to = "dx")
# mx to lx
lx <- convertFx(x, data = mx, from = "mx", to = "lx")


# There are 28 possible combinations --------------------------------
# Let generate all of them.
from <- c("mx", "qx", "dx", "lx")
to   <- c("mx", "qx", "dx", "lx", "Lx", "Tx", "ex")
K    <- expand.grid(from = from, to = to) # all possible cases/combinations

for (i in 1:nrow(K)) {
  In  <- as.character(K[i, "from"])
  Out <- as.character(K[i, "to"])
  N <- paste0(Out, "_from_", In)
  cat(i, " Create", N, "\n")
  # Create the 28 sets of results
  assign(N, convertFx(x = x, data = get(In), from = In, to = Out))
}


# Tests -------------
expect_true(all(round(ex_from_dx - ex_from_lx, 10) == 0))
expect_true(all(round(ex_from_dx - ex_from_qx, 10) == 0))
expect_true(all(round(ex_from_dx - ex_from_mx, 10) == 0))


# Expect errors --------
expect_error(convertFx(x, data = mx, from = "mx", to = "qxx"))
expect_error(convertFx(x, data = mx, from = "mxx", to = "qx"))
expect_error(convertFx(10:15, data = mx, from = "mx", to = "qx"))
expect_error(convertFx(x, data = x, from = "mx", to = "qx"))

# ----------------------------------------------

expect_true(all(convertFx(x, data = mx[, 1], from = "mx", to = "qx") >= 0))


# All the possible errors associated with this functions should be solved
# in LifeTable(). convertFx is just a wrapper.

# ----------------------------------------------------------------------------
# Test messages

# Error: The length of 'x' must be equal to the numebr of rows in 'data'
expect_error(
  convertFx(x[1], data = mx, from = "mx", to = "qx")
)

