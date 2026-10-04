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
# These will be used as sources for the combinatorial test below. A single
# constant ax is carried through every conversion in this file: the default
# ax is input-shape dependent by design, and this test is about the
# conversion identity, which needs the same ax on both sides.
ax50 <- rep(0.5, length(x))
qx <- convertFx(x, data = mx, from = "mx", to = "qx", ax = ax50)
dx <- convertFx(x, data = mx, from = "mx", to = "dx", ax = ax50)
lx <- convertFx(x, data = mx, from = "mx", to = "lx", ax = ax50)
ex <- convertFx(x, data = mx, from = "mx", to = "ex", ax = ax50)


test_that("convertFx covers all 35 from-to combinations", {
  # from: mx, qx, dx, lx, ex (the primary life table inputs, ex added by issue #6)
  # to: mx, qx, dx, lx, Lx, Tx, ex (all life table columns)
  from <- c("mx", "qx", "dx", "lx", "ex")
  to   <- c("mx", "qx", "dx", "lx", "Lx", "Tx", "ex")
  K    <- expand.grid(from = from, to = to) # 5 x 7 = 35 combinations

  for (i in 1:nrow(K)) {
    In  <- as.character(K[i, "from"])
    Out <- as.character(K[i, "to"])
    N   <- paste0(Out, "_from_", In)
    # A single ax for every input kind. The conversion identity is what
    # convertFx promises, and it holds exactly only when the ax is the same
    # on both sides; the default ax is input-shape dependent by design (the
    # rate cases read the Andreev-Kingkade a0 from m0, the probability cases
    # from q0, and the closed intervals carry no recoverable rate). The ex
    # case is inverted under the same ax, so it agrees with the rate cases.
    assign(N, convertFx(x = x, data = get(In), from = In, to = Out,
                        ax = rep(0.5, length(x))))
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
    expect_equal(Ref, get(paste0(cc, "_from_ex"))[-n, ], tolerance = 1e-8)
  }
  # ex agrees among the starting points that share the open-interval rate; the
  # ex input is excluded because it pins the open rate as m_N = 1/e_N, which the
  # other inputs recover geometrically, so their cumulative columns differ in
  # the open interval and every row below it (see the dedicated ex test).
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

test_that("convertFx takes ex as a source and round-trips it", {
  # ex -> the columns it implies. mx in this file is a 4-column data frame, so
  # the comparison runs on one column to keep the shapes aligned.
  n   <- length(x)
  ex1 <- ex[, 1]
  qx_e <- convertFx(x, data = ex1, from = "ex", to = "qx", ax = ax50)
  mx_e <- convertFx(x, data = ex1, from = "ex", to = "mx", ax = ax50)
  mx1  <- convertFx(x, data = mx[, 1], from = "mx", to = "mx", ax = ax50)

  expect_equal(qx_e, qx[, 1], tolerance = 1e-8)
  expect_equal(unname(mx_e), unname(mx1), tolerance = 1e-8)

  # a matrix source keeps its shape and names
  M   <- cbind(a = ex1, b = ex1 + 1)
  Mex <- convertFx(x, data = M, from = "ex", to = "mx", ax = ax50)
  expect_equal(dim(Mex), c(n, 2))
  expect_equal(colnames(Mex), c("a", "b"))
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

test_that("convertFx forwards omega and relabels the extended grid", {
  xo  <- c(0, 1, seq(5, 75, by = 5))
  mxo <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
           .004, .005, .006, .0093, .0129, .019, .031, .049, .084)
  names(mxo) <- xo

  # Vector: the output keeps the extended ages as names.
  ex <- convertFx(xo, data = mxo, from = "mx", to = "ex", omega = 110)
  expect_equal(length(ex), 24)
  expect_equal(as.integer(names(ex)), c(0, 1, seq(5, 110, by = 5)))
  expect_equal(ex[["110"]], 1/LifeTable(xo, mx = mxo, omega = 110)$lt$mx[24],
               tolerance = 1e-12)

  # Matrix: the shape follows the extended grid.
  M  <- cbind(a = mxo, b = mxo * 1.1)
  exm <- convertFx(xo, data = M, from = "mx", to = "ex", omega = 110)
  expect_equal(dim(exm), c(24, 2))
  expect_equal(rownames(exm)[24], "110")
  expect_equal(colnames(exm), c("a", "b"))
})

test_that("convertFx forwards close without changing the grid", {
  xo  <- c(0, 1, seq(5, 75, by = 5))
  mxo <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
           .004, .005, .006, .0093, .0129, .019, .031, .049, .084)
  names(mxo) <- xo

  ex <- convertFx(xo, data = mxo, from = "mx", to = "ex", close = "kannisto")
  expect_equal(length(ex), length(xo))
  expect_equal(names(ex), names(mxo))
  expect_equal(unname(ex), unname(LifeTable(xo, mx = mxo, close = "kannisto")$lt$ex),
               tolerance = 1e-12)

  M   <- cbind(a = mxo, b = mxo * 1.1)
  exm <- convertFx(xo, data = M, from = "mx", to = "ex", close = "kannisto")
  expect_equal(dim(exm), c(length(xo), 2))
  expect_equal(rownames(exm), names(mxo))
})
