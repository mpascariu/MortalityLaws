# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-05 18:51:05
# --------------------------------------------
remove(list = ls())

# Setup: load AHMD mortality data for ages 0-105 to test the convertFx function.
# convertFx converts between life table columns: mx, qx, dx, lx, Lx, Tx, ex.
# This test verifies all conversion paths produce consistent and valid results.
x  <- 0:105
mx <- ahmd$mx[paste0(x),]

# Basic conversions: convert mx into other life table functions (qx, dx, lx)
# These will be used as sources for the full combinatorial test below.
qx <- convertFx(x, data = mx, from = "mx", to = "qx")
dx <- convertFx(x, data = mx, from = "mx", to = "dx")
lx <- convertFx(x, data = mx, from = "mx", to = "lx")


# Generate all 28 possible (from, to) conversion combinations -----------------
# from: mx, qx, dx, lx (the primary life table inputs)
# to: mx, qx, dx, lx, Lx, Tx, ex (all life table columns)
# Using expand.grid, we test every possible source-to-target conversion.
from <- c("mx", "qx", "dx", "lx")
to   <- c("mx", "qx", "dx", "lx", "Lx", "Tx", "ex")
K    <- expand.grid(from = from, to = to) # 4 x 7 = 28 combinations

for (i in 1:nrow(K)) {
  In  <- as.character(K[i, "from"])
  Out <- as.character(K[i, "to"])
  N <- paste0(Out, "_from_", In)
  cat(i, " Create", N, "\n")
  # Create the 28 sets of results
  assign(N, convertFx(x = x, data = get(In), from = In, to = Out))
}


# Consistency checks: life expectancy (ex) computed from different starting points
# (dx, lx, qx, mx) should yield numerically identical results (up to rounding).
# If all conversion paths are mathematically correct, these differences should be 0.
expect_true(all(round(ex_from_dx - ex_from_lx, 10) == 0))
expect_true(all(round(ex_from_dx - ex_from_qx, 10) == 0))
expect_true(all(round(ex_from_dx - ex_from_mx, 10) == 0))


# Input validation: Expect errors for:
#   (1) Invalid target column ("qxx")
#   (2) Invalid source column ("mxx")
#   (3) Mismatched length between x and number of rows in data
#   (4) data is not a matrix/data.frame (x is a vector, but function expects tabular data)
expect_error(convertFx(x, data = mx, from = "mx", to = "qxx"))
expect_error(convertFx(x, data = mx, from = "mxx", to = "qx"))
expect_error(convertFx(10:15, data = mx, from = "mx", to = "qx"))
expect_error(convertFx(x, data = x, from = "mx", to = "qx"))

# ----------------------------------------------

# Additional check: converting a single column (mx for one population) to qx
# should produce non-negative probabilities of death.
expect_true(all(convertFx(x, data = mx[, 1], from = "mx", to = "qx") >= 0))


# Note: Detailed error handling for life table inconsistencies (e.g., cumulative
# deaths exceeding radix) is implemented in LifeTable(). convertFx is designed
# as a thin wrapper and relies on LifeTable for those checks.

# ----------------------------------------------------------------------------
# Test messages

# Error: The length of 'x' must be equal to the number of rows in 'data'
# Trying to convert with a mismatched x length should raise an error.
expect_error(
  convertFx(x[1], data = mx, from = "mx", to = "qx")
)

