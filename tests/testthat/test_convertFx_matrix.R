# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-04
# --------------------------------------------

# Setup: the matrix branch of convertFx. A multi-column input is converted
# column by column. With no ax and no closing argument the single identities
# (mx->qx, qx->mx, dx->lx, lx->dx) are resolved on the matrix itself; every
# other pair goes through LifeTable, which stacks the columns of a multi-column
# input and returns N rows for a one-column one. AHMD supplies real input:
# mx for the rates and Dx for the death distribution, ages 0-105.
x    <- 0:105
N    <- length(x)
M    <- as.matrix(ahmd$mx[paste0(x), ])
D    <- as.matrix(ahmd$Dx[paste0(x), ])
Q    <- convertFx(x, data = M, from = "mx", to = "qx")
L    <- convertFx(x, data = M, from = "mx", to = "lx")
ax50 <- rep(0.5, N)


# Column-by-column agreement is what the matrix branch promises: a whole matrix
# converts to whatever each of its columns converts to on its own. It has to
# hold on the identities resolved on the matrix and on the LifeTable fallback,
# and it is the reason the fallback keeps the input's shape and names.
matrix_matches_columns <- function(In, Out, MM) {
  mat <- convertFx(x, data = MM, from = In, to = Out)
  expect_equal(dim(mat), dim(MM))
  for (j in seq_len(ncol(MM))) {
    vec <- convertFx(x, data = MM[, j], from = In, to = Out)
    expect_equal(unname(mat[, j]), unname(vec), tolerance = 1e-12)
  }
  invisible(mat)
}


test_that("convertFx converts a matrix column by column", {
  # No ax and no closing argument, so the four single identities are taken here.
  matrix_matches_columns("mx", "qx", M)
  matrix_matches_columns("qx", "mx", Q)
  matrix_matches_columns("dx", "lx", D)
  matrix_matches_columns("lx", "dx", L)
})


test_that("the dx/lx identities use the lx0 radix by default", {
  # These two identities need a radix. Left NULL, lx0 must mean LifeTable's
  # default 1e5, so asking for it explicitly cannot change the answer. The
  # identities themselves are pinned with it: dx->lx starts at the radix and
  # lx->dx closes with a death distribution summing back to it.
  lx_default <- convertFx(x, data = D, from = "dx", to = "lx")
  lx_radix   <- convertFx(x, data = D, from = "dx", to = "lx", lx0 = 1e5)
  expect_equal(lx_default, lx_radix)
  expect_equal(unname(lx_default[1, ]), rep(1e5, ncol(D)))

  dx_default <- convertFx(x, data = L, from = "lx", to = "dx")
  dx_radix   <- convertFx(x, data = L, from = "lx", to = "dx", lx0 = 1e5)
  expect_equal(dx_default, dx_radix)
  expect_equal(unname(colSums(dx_default)), rep(1e5, ncol(L)), tolerance = 1e-8)
})


test_that("convertFx labels an unnamed matrix by position", {
  # The fallback hands the whole matrix to LifeTable, which needs column names,
  # so an input carrying none is labelled by index there; the ages stay the row
  # labels. The unnamed input must still convert to the named input's values.
  Mn <- M
  dimnames(Mn) <- NULL
  unnamed <- convertFx(x, data = Mn, from = "mx", to = "qx")

  expect_equal(dim(unnamed), dim(Mn))
  expect_equal(colnames(unnamed), as.character(seq_len(ncol(Mn))))
  expect_equal(rownames(unnamed), as.character(x))
  expect_equal(unname(unnamed), unname(Q), tolerance = 1e-12)
})


test_that("convertFx returns one life table as an N-row matrix", {
  # A one-column input yields exactly one life table, so the generic fallback
  # has to reshape its N rows into an N-row matrix; a multi-column input comes
  # back stacked and takes the other branch. The reshape keeps the input's row
  # and column labels, and the column agrees with the plain vector conversion.
  m1 <- M[, 1, drop = FALSE]
  dimnames(m1) <- list(paste0("age.", x), "SWE")

  ex1 <- convertFx(x, data = m1, from = "mx", to = "ex", ax = ax50)
  expect_true(is.matrix(ex1))
  expect_equal(dim(ex1), c(N, 1))
  expect_equal(dimnames(ex1), dimnames(m1))
  expect_equal(unname(ex1[, 1]),
               unname(convertFx(x, data = m1[, 1], from = "mx", to = "ex",
                                ax = ax50)),
               tolerance = 1e-12)

  # The ex input is the inverse problem on the same one-column shape.
  mx1 <- convertFx(x, data = ex1, from = "ex", to = "mx", ax = ax50)
  expect_true(is.matrix(mx1))
  expect_equal(dim(mx1), c(N, 1))
  expect_equal(dimnames(mx1), dimnames(m1))
  expect_equal(unname(mx1[, 1]),
               unname(convertFx(x, data = ex1[, 1], from = "ex", to = "mx",
                                ax = ax50)),
               tolerance = 1e-12)
})


test_that("convertFx reports a length mismatch on a vector input", {
  # The vector branch guards its own shape: a mismatched x/data pair is a
  # length mismatch and says so, rather than falling through to the row-count
  # message the matrix branch uses.
  expect_error(
    convertFx(x = 2:11, data = 1:5, from = "mx", to = "qx"),
    regexp = "do not have the same length"
  )
})
