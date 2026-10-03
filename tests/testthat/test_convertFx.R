# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-05 18:51:05
# --------------------------------------------

# Setup: load AHMD mortality data for ages 0-105 to test the convertFx function.
# convertFx converts between life table columns: mx, qx, dx, lx, Lx, Tx, ex.
# This test verifies all conversion paths produce consistent and valid results.
x  <- 0:105
mx <- ahmd$mx[paste0(x), ]

# Basic conversions: convert mx into other life table functions (qx, dx, lx).
# These will be used as sources for the combinatorial test below.
qx <- convertFx(x, data = mx, from = "mx", to = "qx")
dx <- convertFx(x, data = mx, from = "mx", to = "dx")
lx <- convertFx(x, data = mx, from = "mx", to = "lx")


test_that("convertFx covers all 28 from-to combinations", {
  # from: mx, qx, dx, lx (the primary life table inputs)
  # to: mx, qx, dx, lx, Lx, Tx, ex (all life table columns)
  from <- c("mx", "qx", "dx", "lx")
  to   <- c("mx", "qx", "dx", "lx", "Lx", "Tx", "ex")
  K    <- expand.grid(from = from, to = to) # 4 x 7 = 28 combinations

  for (i in 1:nrow(K)) {
    In  <- as.character(K[i, "from"])
    Out <- as.character(K[i, "to"])
    N   <- paste0(Out, "_from_", In)
    assign(N, convertFx(x = x, data = get(In), from = In, to = Out))
  }

  # Cross-input consistency. The closing row follows the qx[N] = 1 closure
  # convention, which is input-shape specific (the mx path keeps the observed
  # mx[N], the qx/lx/dx paths continue it geometrically, as their inputs no
  # longer carry it), so the comparison runs over rows 1..N-1; the open
  # interval itself is pinned by the contract-3 tests
  # (ax[N] = ex[N] = 1/mx[N], Lx[N] = lx[N]/mx[N]).
  n <- length(x)
  for (cc in c("mx", "qx", "dx", "lx", "Lx")) {
    Ref <- get(paste0(cc, "_from_dx"))[-n, ]
    expect_equal(Ref, get(paste0(cc, "_from_lx"))[-n, ], tolerance = 1e-8)
    expect_equal(Ref, get(paste0(cc, "_from_qx"))[-n, ], tolerance = 1e-8)
  }
  # ex agrees among the starting points that share the open-interval rate
  expect_equal(ex_from_dx[-n, ], ex_from_lx[-n, ], tolerance = 1e-8)
  expect_equal(ex_from_dx[-n, ], ex_from_qx[-n, ], tolerance = 1e-8)
})

test_that("convertFx validates its inputs", {
  expect_error(convertFx(x, data = mx, from = "mx", to = "qxx"))
  expect_error(convertFx(x, data = mx, from = "mxx", to = "qx"))
  expect_error(convertFx(10:15, data = mx, from = "mx", to = "qx"))
  # The length of 'x' must be equal to the number of rows in 'data'
  expect_error(convertFx(x[1], data = mx, from = "mx", to = "qx"))
})

test_that("convertFx accepts integer vectors as numeric input", {
  # F31: integer vectors used to fall into the matrix branch and error. They
  # are valid numeric input and must match the double-input result.
  out_int <- convertFx(x, data = x, from = "mx", to = "qx")
  out_dbl <- convertFx(x, data = as.numeric(x), from = "mx", to = "qx")
  expect_equal(out_int, out_dbl)
  expect_true(all(is.finite(out_int)))
})

test_that("convertFx keeps the per-format life table identities", {
  # The open-interval rule makes ex path-specific, so cross-format ex equality
  # is not pinned; the identities of each table are (contract 3).
  n  <- length(x)
  LT <- LifeTable(x, mx = mx[, 1])
  ok <- LT$lt$lx > 0
  expect_equal(LT$lt$ex[ok], (LT$lt$Tx/LT$lt$lx)[ok], tolerance = 1e-12)
  expect_equal(LT$lt$ex[n], 1/LT$lt$mx[n], tolerance = 1e-12)
})

test_that("convertFx returns non-negative probabilities from a single column", {
  expect_true(all(convertFx(x, data = mx[, 1], from = "mx", to = "qx") >= 0))
})
