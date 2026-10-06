# --------------------------------------------
# LifeTable(): the ax estimators, closing and extension rules, ex inverse, convertFx.
# --------------------------------------------
# Contract 3: the open interval keeps qx[n] = 1 and ax[n] = ex[n] = 1/mx[n].
expect_open_interval <- function(lt, n = nrow(lt), label = NULL) {
  expect_equal(lt$qx[n], 1, tolerance = 1e-12, label = label)
  expect_equal(lt$ax[n], 1/lt$mx[n], tolerance = 1e-12, label = label)
  expect_equal(lt$ex[n], 1/lt$mx[n], tolerance = 1e-12, label = label)
  expect_equal(lt$Lx[n], lt$lx[n]/lt$mx[n], tolerance = 1e-12, label = label)
}
# The column properties of any table.
expect_table_identities <- function(lt, label = NULL) {
  cn <- c("x", "mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex")
  expect_true(all(lt[, cn] >= 0), label = label)
  expect_false(all(is.na(lt$ex)), label = label)
  expect_identical(class(lt$ex), "numeric", label = label)
  expect_true(lt$ex[1] >= rev(lt$ex)[1], label = label)
  expect_equal(sum(lt$dx), lt$lx[1], label = label)
  expect_true(lt$qx[nrow(lt)] >= 0.99999, label = label)
}

# ---- Shared inputs -----------------------------------------------------------
# Grids and the Swedish 1950 column from helper-data.R.
x    <- grid_single; mx <- mx_1950(x); Dx <- Dx_1950(x); Ex <- Ex_1950(x)
xo   <- grid_ab_75;  mxo <- mx_ab_75
mxon <- mxo; names(mxon) <- xo          # named copy: convertFx keeps the names
xs   <- grid_small;  ms <- mx_small
xa   <- grid_ab_100; mxa <- mx_ab_100; Na <- length(xa)
xb   <- grid_ab_110; mxb <- mx_ab_110
# The ex round trips run on the 1850 column (the inverse recovers it exactly).
EXG  <- list(x, c(0, 1, seq(5, 105, by = 5)), c(0, 1, seq(5, 75, by = 5)),
             0:95, 60:105)
mx_ex <- function(gr) ahmd$mx[paste0(gr), "1850"]
# lt()/fx() silence the open-interval ax note; it is pinned on its own below.
lt <- function(...) quiet(LifeTable(...))
fx <- function(...) quiet(convertFx(...))
A0 <- lt(x = xo, mx = mxo)$lt           # the reciprocal-close reference table

# ---- The 16-table construction matrix ----------------------------------------
# Five primary inputs at two ax conventions, plus the abridged, irregular and
# custom-ax tables.
build_lt <- function(xx, input, ref, ax = "andreev_kingkade", sex = NULL) {
  switch(input,
    mx = lt(x = xx, mx = ref$lt$mx, ax = ax, sex = sex),
    qx = lt(x = xx, qx = ref$lt$qx, ax = ax, sex = sex),
    lx = lt(x = xx, lx = ref$lt$lx, ax = ax, sex = sex),
    dx = lt(x = xx, dx = ref$lt$dx, ax = ax, sex = sex))
}
# The user ax: 0.5 on the closed intervals, 1/mx[N] on the open one. x3:
# single-year intervals to age 5, then five-year groups with a0 overridden.
ax6  <- c(rep(0.5, length(x) - 1), 1/mx[length(mx)])
x3   <- c(0, 1, 2, 3, 4, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70)
dx3  <- c(11728, 1998, 2190, 1336, 637, 1927, 420, 453, 475, 905, 1168,
          2123, 2395, 3764, 5182, 6555, 8652, 10687, 37405)
C15  <- lt(x = x3, dx = dx3)
ax15 <- C15$lt$ax; ax15[1] <- 0.1
C16  <- lt(x = x3, dx = dx3, ax = ax15)
TABLES <- list(abridged = lt(x = xb, mx = mxb, sex = "female"),
               irregular = C15, infant_ax = C16)
AXG    <- list(default = "andreev_kingkade", user = ax6)
for (gl in names(AXG)) {
  B <- lt(x = x, Dx = Dx, Ex = Ex, ax = AXG[[gl]])
  TABLES[[paste0("DxEx_", gl)]] <- B
  for (k in c("mx", "qx", "lx", "dx")) {
    TABLES[[paste0(k, "_", gl)]] <- build_lt(x, k, ref = B, ax = AXG[[gl]])
  }
}
test_that("every constructed life table satisfies the column properties", {
  for (nm in names(TABLES)) expect_table_identities(TABLES[[nm]]$lt, label = nm)
  # print works for one and for a stacked multi-column table (NA at 107-110 noted)
  expect_output(print(TABLES$DxEx_default))
  expect_message(LT_multi <- LifeTable(x = 0:110, mx = ahmd$mx))
  expect_output(print(LT_multi))
})
test_that("the alternative inputs reproduce the Dx+Ex benchmark", {
  # Rows n-1 and n are excluded: the qx[N] = 1 convention hides the rate there.
  CONS <- list(
    list(base = "DxEx_default",
         other = c("mx_default", "qx_default", "lx_default", "dx_default")),
    list(base = "DxEx_user",
         other = c("mx_user", "qx_user", "lx_user", "dx_user")))
  for (g in CONS) {
    B <- TABLES[[g$base]]$lt
    i <- seq_len(nrow(B) - 2)
    B <- B[i, -1]
    for (k in g$other) {
      L <- TABLES[[k]]$lt[i, -1]
      expect_equal(B$mx, L$mx, tolerance = 1e-6, label = k)
      expect_equal(B$qx, L$qx, tolerance = 1e-8, label = k)
      expect_equal(B$dx, L$dx, tolerance = 1e-8, label = k)
      expect_equal(B$lx, L$lx, tolerance = 1e-8, label = k)
      expect_equal(B$ex, L$ex, tolerance = 1e-3, label = k)
    }
  }
})

# ---- Input validation and the missing-value rules ----------------------------
test_that("LifeTable validates its 'ax', input combination and 'sex' arguments", {
  # 'ax' must be a method name or numeric; one input kind only; 'sex' restricted.
  expect_error(LifeTable(x = x, mx = mx, ax = "ax"), regexp = "should be one of")
  expect_error(LifeTable(x = x, mx = mx, ax = rep(0.5, 3)),
               regexp = "scalar of length 1")
  expect_error(LifeTable(x = x, Dx = Dx))
  expect_error(LifeTable(x = x, Dx = Dx, Ex = Ex, qx = Ex, mx = Ex))
  expect_error(LifeTable(x = x, mx = mx, sex = "Male"))
})
test_that("LifeTable notes the missing values and localises them", {
  # 'Dx' misses a value (replaced with 0), 'Ex' one (0.01), 'lx' and 'dx' one (0)
  Dxi <- Dx; Dxi[2] <- NA
  Exi <- Ex; Exi[12] <- NA
  lxv <- TABLES$DxEx_default$lt$lx; lxv[length(lxv)] <- NA
  dxv <- TABLES$DxEx_default$lt$dx; dxv[30] <- NA
  expect_message(LifeTable(x = x, Dx = Dxi, Ex = Ex))
  expect_message(LifeTable(x = x, Dx = Dx, Ex = Exi))
  expect_message(LifeTable(x = x, lx = lxv))
  expect_message(LifeTable(x = x, dx = dxv))
  # Contract 3 NA rule: its interval and every row below are NA; lx stays finite.
  k   <- 31                      # age 30
  mxv <- rep(0.01, length(x)); mxv[k] <- NA
  expect_message(LT <- LifeTable(x = x, mx = mxv))
  expect_true(is.na(LT$lt$mx[k]) && is.na(LT$lt$qx[k]))
  expect_true(is.na(LT$lt$dx[k]) && is.na(LT$lt$Lx[k]))
  expect_true(all(is.finite(LT$lt$lx)))
  expect_true(all(is.na(LT$lt$ex[1:k])))
  expect_true(all(is.finite(LT$lt$ex[(k + 1):length(x)])))
  expect_true(all(LT$lt$ex[(k + 1):length(x)] > 0))
  # Contract 3: mx[1] = 0 does not cost a year of life expectancy.
  x0  <- 0:100
  LT0 <- lt(x = x0, mx = c(0, rep(0.01, length(x0) - 1)))
  LTz <- lt(x = x0, mx = c(1e-12, rep(0.01, length(x0) - 1)))
  expect_equal(LT0$lt$ex[1], LTz$lt$ex[1], tolerance = 1e-6)
})

# ---- The ax estimators -------------------------------------------------------
# Shared fits on m = mxa (the default, cfm, preston and the childhood pairs).
m     <- mxa
n     <- c(diff(xa), NA)
wide  <- n[2:(Na - 1)] > 1
m12   <- c(0.12, rep(0.01, Na - 1))
cfm   <- lt(x = xa, mx = m, ax = "cfm")$lt
md    <- lt(x = xa, mx = m)$lt
mdM   <- lt(x = xa, mx = m, sex = "male")$lt
mdF   <- lt(x = xa, mx = m, sex = "female")$lt
pr    <- lt(x = xa, mx = m, ax = "preston")$lt
prF   <- lt(x = xa, mx = m, sex = "female", ax = "preston")$lt
cdF   <- lt(x = xa, mx = m, sex = "female", ax = "coale_demeny")$lt
prM12 <- lt(x = xa, mx = m12, sex = "male", ax = "preston")$lt
prF12 <- lt(x = xa, mx = m12, sex = "female", ax = "preston")$lt
cdF12 <- lt(x = xa, mx = m12, sex = "female", ax = "coale_demeny")$lt
test_that("the 'cfm' ax method is the plain lifetable identity", {
  # ax[2..N-1] = n + 1/m - n/q under CFM; cfm equals preston off the open row.
  q <- 1 - exp(-n * m)
  expect_equal(cfm$ax[2:(Na - 1)], (n + 1/m - n/q)[2:(Na - 1)],
               tolerance = 1e-10)
  expect_equal(cfm$ax[-Na], pr$ax[-Na], tolerance = 1e-12)
  expect_false(isTRUE(all.equal(
    lt(x = xa, mx = m, sex = "female", ax = "cfm")$lt$ax[1:2], prF$ax[1:2])))
})
test_that("the 'andreev_kingkade' method follows the HMD Methods Protocol v6", {
  # a0 from m0 (Protocol Table 3, m0 = 0.053 is in the middle branch); the numeric
  # ax it builds forces qx = n*mx/(1 + (n - ax)*mx), the protocol's eq. 74.
  a0M <- 0.02832 + 3.26021 * m[1]
  a0F <- 0.04667 + 3.88089 * m[1]
  expect_identical(md, lt(x = xa, mx = m, ax = "andreev_kingkade")$lt)
  expect_equal(md$ax[1], (a0M + a0F)/2, tolerance = 1e-12)
  expect_equal(mdM$ax[1], a0M, tolerance = 1e-12)
  expect_equal(mdF$ax[1], a0F, tolerance = 1e-12)
  # only the interval (0,1) is one year wide, so it alone takes the midpoint
  expect_equal(md$ax[2:(Na - 1)][wide], cfm$ax[2:(Na - 1)][wide],
               tolerance = 1e-12)
  idx <- 1:(Na - 1)
  expect_equal(md$qx[idx],
               n[idx] * m[idx] / (1 + (n[idx] - md$ax[idx]) * m[idx]),
               tolerance = 1e-12)
  expect_false(isTRUE(all.equal(md$qx[1], cfm$qx[1])))
  # the rule needs a birth-starting, one-year first interval; otherwise n/2 applies
  x1 <- grid_ab_100
  m1 <- c(0.053, 0.005, rep(0.01, length(x1) - 2))
  expect_equal(lt(x = x1, mx = m1)$lt$ax[1],
               (0.02832 + 3.26021*0.053 + 0.04667 + 3.88089*0.053)/2,
               tolerance = 1e-12)
  x2 <- 3:110
  expect_equal(lt(x = x2, mx = rep(0.01, length(x2)))$lt$ax[1], 0.5,
               tolerance = 1e-12)
  x4 <- grid_ab_100[-2]
  m4 <- c(0.053, rep(0.01, length(x4) - 1))
  a4 <- lt(x = x4, mx = m4)$lt
  expect_equal(a4$ax, lt(x = x4, mx = m4, ax = "cfm")$lt$ax, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(a4$ax[1], 2.5)))
})
test_that("Contract 3: the closing age, and a supplied ax kept as given", {
  # A constant 0.5 is admissible here (mx <= 0.551): only the open interval moves.
  expect_message(LU <- LifeTable(x = xa, mx = m, ax = rep(0.5, Na)))
  ax <- c(0.1, 1.5, rep(2, 18), 1, 1)
  expect_message(LT <- LifeTable(x = xa, mx = m, ax = ax))

  # A derived ax follows the closing rule; a supplied one is the caller's, so
  # at that age its ax and mx columns disagree by choice.
  expect_open_interval(md)
  for (L in list(LU$lt, LT$lt)) {
    expect_equal(L$qx[Na], 1, tolerance = 1e-12)
    expect_equal(L$ex[Na], L$ax[Na], tolerance = 1e-12)
  }
  expect_equal(LU$lt$ax[Na], 0.5, tolerance = 1e-12)
  expect_equal(LT$lt$ax[Na], 1, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(LT$lt$mx[Na], 1/LT$lt$ax[Na])))

  # user ax over the closed intervals: qx = nx*mx/(1 + (nx - ax)*mx) and
  # mx = qx/(ax*qx + nx*(1 - qx)) == dx/Lx
  idx <- 1:(Na - 1)
  expect_equal(LT$lt$qx[idx],
               n[idx] * m[idx] / (1 + (n[idx] - LT$lt$ax[idx]) * m[idx]),
               tolerance = 1e-12)
  expect_equal(LT$lt$mx[idx], LT$lt$dx[idx]/LT$lt$Lx[idx], tolerance = 1e-10)
})
test_that("Contract 3: Coale-Demeny child ax constants and continuity", {
  # m0 >= 0.107: male a0 = 0.330, a1 = 1.352; female a0 = 0.350, a1 = 1.361.
  expect_equal(prM12$ax[1:2], c(0.330, 1.352), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(
    lt(x = xa, mx = m12, sex = "male")$lt$ax[1:2], c(0.330, 1.352))))
})
test_that("Coale-Demeny variants: default unchanged, both conventions correct", {
  # the default is A-K a0 at 0 and n/2 elsewhere: it differs from cfm at (0,1) only
  expect_identical(mdF, lt(x = xa, mx = m, sex = "female",
                           ax = "andreev_kingkade")$lt)
  expect_false(isTRUE(all.equal(mdF$ax[1], cfm$ax[1])))
  expect_equal(mdF$ax[2:(Na - 1)][wide], cfm$ax[2:(Na - 1)][wide],
               tolerance = 1e-12)
  # CFM and the Coale-Demeny rules differ in the first two intervals (with a sex)
  expect_false(isTRUE(all.equal(cfm$ax[1:2], prF$ax[1:2])))
  expect_equal(cfm$ax[-(1:2)], prF$ax[-(1:2)], tolerance = 1e-12)
  expect_false(isTRUE(all.equal(cdF$ax[1:2], prF$ax[1:2])))
  expect_lt(max(abs(cdF$ax[1:2] - prF$ax[1:2])), 0.005)
  expect_equal(cdF$ax[-(1:2)], prF$ax[-(1:2)], tolerance = 1e-12)
  expect_identical(prF12$ax[1:2], cdF12$ax[1:2])
  expect_equal(prF12$ax[1:2], c(0.350, 1.361), tolerance = 1e-12)
  # without a sex the method is ignored; a numeric ax is used verbatim
  expect_identical(md$ax, lt(x = xa, mx = m, ax = "andreev_kingkade")$lt$ax)
  my_ax <- c(0.1, 1.5, rep(2, Na - 5), 1, 1, 1)
  expect_equal(lt(x = xa, mx = m, ax = my_ax)$lt$ax[1:2], my_ax[1:2],
               tolerance = 1e-12)
  expect_error(LifeTable(x = xa, mx = m, sex = "female", ax = "west"),
               regexp = "should be one of")
})

# ---- Closing in place and extending to omega (issue #8) ----------------------
test_that("life tables are unchanged when omega and close are NULL", {
  expect_identical(A0, lt(x = xo, mx = mxo, omega = NULL)$lt)
  expect_identical(A0, lt(x = xo, mx = mxo, close = NULL)$lt)
})
test_that("LifeTable closes in place with a mortality law", {
  N <- length(xo)
  C <- lt(x = xo, mx = mxo, close = "kannisto")$lt
  # only the open row's rate moves; the young interval closes above the observed 1/mx
  expect_identical(A0$x, C$x)
  expect_equal(nrow(A0), nrow(C))
  expect_equal(C$mx[-N], A0$mx[-N], tolerance = 1e-12)
  expect_false(isTRUE(all.equal(C$mx[N], A0$mx[N])))
  expect_equal(C$lx, A0$lx, tolerance = 1e-12)
  expect_open_interval(C)
  expect_true(C$mx[N] > A0$mx[N])
  expect_true(C$ex[1] < A0$ex[1])
  m_new <- vapply(c("qx", "lx", "dx"), function(idx) {
    L <- switch(idx, qx = lt(x = xo, qx = A0$qx, close = "kannisto"),
                lx = lt(x = xo, lx = A0$lx, close = "kannisto"),
                dx = lt(x = xo, dx = A0$dx, close = "kannisto"))
    L$lt$mx[nrow(L$lt)]
  }, 0)
  expect_equal(unname(m_new), rep(unname(m_new[1]), 3), tolerance = 1e-6)
})
test_that("LifeTable validates 'close' and keeps the rate when it fails", {
  mrep <- rep(0.01, length(xo))
  expect_error(LifeTable(x = xo, mx = mrep, close = "notalaw"),
               regexp = "'close' must be")
  expect_error(LifeTable(x = xo, mx = mrep, close = c("kannisto", "gompertz")),
               regexp = "'close' must be")
  # "standard" is the default; two ages cannot support a three-parameter-plus fit
  expect_identical(lt(x = xo, mx = mrep, close = "standard")$lt,
                   lt(x = xo, mx = mrep)$lt)
  expect_warning(LifeTable(x = c(0, 1), mx = c(0.05, 0.005), close = "kannisto"),
                 regexp = "could not be fitted")
})
test_that("LifeTable extends and closes at omega", {
  LT <- lt(x = xo, mx = mxo, omega = 110)$lt
  N  <- nrow(LT)
  expect_equal(tail(LT$x, 1), 110)
  expect_equal(diff(LT$x[-c(1, 2)]), rep(5, N - 3))
  expect_open_interval(LT)
  expect_equal(LT$mx[1:(length(mxo) - 1)], mxo[1:(length(mxo) - 1)],
               tolerance = 1e-12)
  expect_true(all(diff(LT$mx[(length(mxo) - 1):N]) > 0))
  e0 <- vapply(c("qx", "lx", "dx"), function(idx) {
    L <- switch(idx, qx = lt(x = xo, qx = A0$qx, omega = 110),
                lx = lt(x = xo, lx = A0$lx, omega = 110),
                dx = lt(x = xo, dx = A0$dx, omega = 110))
    L$lt$ex[1]
  }, 0)
  expect_equal(unname(e0), rep(unname(e0[1]), 3), tolerance = 1e-8)
})
test_that("LifeTable validates 'omega' and drops a failing extension", {
  mrep <- rep(0.01, length(xo))
  expect_error(LifeTable(x = xo, mx = mrep, omega = "x"),
               regexp = "'omega' must be a single finite number")
  expect_warning(LifeTable(x = xo, mx = mrep, omega = 75),
                 regexp = "not greater than the last age")
  # the extension is dropped, with a warning, when the law cannot be fitted
  expect_warning(LifeTable(x = c(0, 1), mx = c(0.05, 0.005), omega = 110),
                 regexp = "could not be computed")
})
test_that("LifeTable closes or extends abridged tables for every column", {
  M <- cbind(a = mxo, b = mxo * 1.1)
  # in place: the grid is unchanged, every column closed
  LC <- lt(x = xo, mx = M, close = "kannisto")$lt
  expect_equal(sort(unique(LC$LT)), c("a", "b"))
  expect_equal(nrow(LC), 2 * length(xo))
  expect_true(all(LC$qx[LC$x == 75] == 1))
  LE <- lt(x = xo, mx = M, omega = 110)$lt
  expect_equal(nrow(LE), 2 * 24)
  expect_true(all(LE$qx[LE$x == 110] == 1))
})

# ---- The ex input (issue #6) -------------------------------------------------
# The round trip must recover the table (exact in ex, qx, ax; preston/coale_demeny
# adjust ax after converting the rates).
X0 <- lt(x = x, mx = mx_ex(x))$lt
test_that("the ex input reproduces the table it was built from", {
  for (gr in EXG) {
    A <- lt(x = gr, mx = mx_ex(gr))
    B <- lt(x = gr, ex = A$lt$ex)
    n <- length(gr) - 1
    expect_equal(B$lt$ex, A$lt$ex, tolerance = 1e-8)
    expect_equal(B$lt$qx, A$lt$qx, tolerance = 1e-8)
    expect_equal(B$lt$mx[seq_len(n)], A$lt$mx[seq_len(n)], tolerance = 1e-8)
    expect_equal(B$lt$ax, A$lt$ax, tolerance = 1e-6)
  }
  # and under every ax method and sex (the childhood rule's a1 takes the m0 rate)
  for (gr in EXG[1:3]) {
    for (am in c("andreev_kingkade", "cfm", "preston", "coale_demeny")) {
      for (sx in list(NULL, "female", "male")) {
        A <- lt(x = gr, mx = mx_ex(gr), ax = am, sex = sx)
        B <- lt(x = gr, ex = A$lt$ex, ax = am, sex = sx)
        expect_equal(B$lt$ex, A$lt$ex, tolerance = 1e-8)
        expect_equal(B$lt$qx, A$lt$qx, tolerance = 1e-8)
        expect_equal(B$lt$ax, A$lt$ax, tolerance = 1e-6)
      }
    }
  }
})
test_that("the ex input reproduces the rate column of the default method", {
  # a rate column on the table's own identity is recovered exactly, cap included
  for (gr in list(EXG[[1]], EXG[[2]], 0:110)) {
    A <- lt(x = gr, mx = mx_ex(gr))
    expect_equal(lt(x = gr, ex = A$lt$ex)$lt$mx, A$lt$mx, tolerance = 1e-8)
  }
})
test_that("the ex input accepts a matrix and keeps the shape", {
  gr <- EXG[[2]]
  Am <- lt(x = gr, mx = mx_ex(gr))
  M  <- lt(x = gr, ex = cbind(a = Am$lt$ex, b = Am$lt$ex + 0.5))$lt
  expect_equal(sort(unique(M$LT)), c("a", "b"))
  expect_equal(nrow(M), 2 * length(gr))
})
test_that("the ex input is not required to fall with age", {
  # e0 below e1 is normal when infant mortality is high; the inverse must accept it
  expect_true(X0$ex[1] < X0$ex[2])
  expect_silent(LifeTable(x = x, ex = X0$ex))
})
test_that("an infeasible ex is an error naming the age", {
  eb <- X0$ex
  eb[50] <- eb[51] + 2                 # a rise where none is possible
  expect_error(LifeTable(x = x, ex = eb), regexp = "not a feasible life table")
  # a missing value in the curve is an error at any age, oldest ages included
  for (age in c(10, 99, 100, 105)) {
    en <- X0$ex
    en[age + 1] <- NA
    expect_error(LifeTable(x = x, ex = en), regexp = "missing or non-finite")
  }
})
test_that("the ex input forwards close and omega", {
  C <- lt(x = xo, ex = A0$ex, close = "kannisto")$lt
  expect_equal(nrow(C), length(xo))
  expect_true(C$qx[nrow(C)] == 1)
  O <- lt(x = xo, ex = A0$ex, omega = 110)$lt
  expect_true(nrow(O) > length(xo))
  expect_true(all(O$qx[O$x == 110] == 1))
})

# ---- Guards and fallbacks (former test_LifeTable_branches.R) -----------------
test_that("guards: the input case, class, length and ax form", {
  # compute_life_table() must detect the input case on its own
  expect_identical(compute_life_table(x = xs, mx = ms),
                   compute_life_table(x = xs, mx = ms, case = "C2_mx"))
  # a 1-d array is not a vector for is.vector(): flatten it to one numeric table
  K <- detect_case(mx = array(ms))
  expect_identical(K$case, "C2_mx")
  expect_identical(K$iclass, "numeric")
  expect_identical(K$nLT, 1)
  # a character vector is not one of the accepted input classes
  expect_error(LifeTable(x = xs, mx = "0.01"),
               regexp = "class of the input should be")
  # a shorter vector would silently be recycled: one value per age is required
  expect_error(LifeTable(x = x, mx = rep(0.01, 10)),
               regexp = "must have one value per age in 'x' \\(106 expected, got 10\\)")
  # two method names cannot select an ax rule; a logical is no ax
  expect_error(LifeTable(x = xs, mx = ms, ax = c("cfm", "preston")),
               regexp = "'ax' must name a single method, not a vector")
  expect_error(LifeTable(x = xs, mx = ms, ax = TRUE),
               regexp = "'ax' must be a numeric scalar or vector")
})
test_that("guards: the ax estimators", {
  # a supplied ax covers every interval, the open one included, and is kept
  # even where the closing rule implies something else
  expect_message(LT <- LifeTable(x = xs, mx = ms, ax = 0.5),
                 regexp = "open age interval")
  expect_equal(LT$lt$ax[LT$lt$x < max(xs)], rep(0.5, length(xs) - 1),
               tolerance = 1e-12)
  expect_equal(LT$lt$ax[length(xs)], 0.5, tolerance = 1e-12)
  expect_equal(sum(LT$lt$dx), LT$lt$lx[1], tolerance = 1e-8)
  ex5 <- c(70, 69, 68, 67, 66, 65)
  # a table entered from ex has its open interval fixed by that curve, so the
  # supplied open-interval value is not kept; the inverse says so
  expect_message(A <- LifeTable(x = xs, ex = ex5, ax = 0.5),
                 regexp = "open age interval")
  expect_identical(A$lt, lt(x = xs, ex = ex5, ax = rep(0.5, length(xs)))$lt)
  # at mx = 1e-8 the closed form is pure cancellation: the series keeps the n/2 limit
  Lt <- lt(x = xs, mx = c(1e-8, 0.01, 0.02, 0.03, 0.04, 0.05), ax = "cfm")$lt
  expect_equal(Lt$ax[1], 0.5, tolerance = 1e-6)
  expect_equal(Lt$Lx[1], Lt$lx[1], tolerance = 1e-6)
  # the separation factors are a function of the infant rate: m0 < 0 is refused
  expect_error(
    LifeTable(x = xs, mx = c(-0.01, 0.02, 0.03, 0.04, 0.05, 0.06),
              ax = "preston", sex = "female"),
    regexp = "must be greater than 0")
})
test_that("guards: the rate repair and the open-interval fallbacks", {
  # zero rates: every derived column would be infinite, so the table is NA instead
  LT <- lt(x = xs, mx = rep(0, length(xs)))
  expect_true(all(is.na(LT$lt[, !names(LT$lt) %in% c("x.int", "x")])))
  expect_identical(LT$lt$x, as.numeric(xs))
  # the interval takes the last usable rate before a non-finite entry
  expect_message(
    Lt <- LifeTable(x = xs, mx = c(0.01, Inf, 0.03, 0.04, 0.05, 0.06),
                    ax = "cfm"),
    regexp = "missing or non-finite")
  expect_equal(Lt$lt$mx[2], Lt$lt$mx[1], tolerance = 1e-12)
  expect_true(all(is.finite(Lt$lt$mx)))
  expect_equal(Lt$lt$qx[length(xs)], 1, tolerance = 1e-12)
  # a zero open-interval rate cannot give 1/mx: it takes half the last interval
  Lz <- lt(x = xo, mx = c(mxo[-length(mxo)], 0), ax = "cfm")$lt
  N  <- nrow(Lz)
  expect_equal(Lz$ax[N], (xo[N] - xo[N - 1])/2, tolerance = 1e-12)
  expect_equal(Lz$ex[N], Lz$ax[N], tolerance = 1e-12)
  expect_equal(Lz$Lx[N], Lz$ax[N] * Lz$dx[N], tolerance = 1e-12)
  expect_equal(Lz$qx[N], 1, tolerance = 1e-12)
  # with the preceding rate missing there is no neighbour either: 2.5 still applies
  expect_message(
    Lm <- LifeTable(x = xo, mx = c(mxo[-c(16, 17)], NA, 0), ax = "cfm")$lt,
    regexp = "missing or non-finite")
  N <- nrow(Lm)
  expect_false(any(is.infinite(Lm$ax) | is.nan(Lm$ax)))
  expect_equal(Lm$ax[N], 2.5, tolerance = 1e-12)
  expect_equal(Lm$ex[N], 2.5, tolerance = 1e-12)
  expect_true(all(is.na(Lm$ex[1:(N - 2)])))
  # a repeated age makes a zero-width interval: it must inherit the next finite ax
  Lw <- lt(x = c(0, 0, 1, 2, 3, 4, 5),
           mx = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07), ax = "cfm")$lt
  expect_true(all(is.finite(Lw$ax)))
  expect_equal(Lw$ax[1], Lw$ax[2], tolerance = 1e-12)
  expect_equal(Lw$qx[1], 0, tolerance = 1e-12)   # no time to die
  expect_equal(Lw$dx[1], 0, tolerance = 1e-12)
  expect_equal(Lw$lx[1], Lw$lx[2], tolerance = 1e-12)
})
test_that("guards: the close keeps the observed rate", {
  # the internal default must be the law LifeTable() resolves before calling it
  expect_identical(lt_close_model(xo, mxo),
                   lt_close_model(xo, mxo, law = "kannisto"))
  E <- lt_extend_omega(xo, mxo, omega = 110)
  expect_identical(E, lt_extend_omega(xo, mxo, omega = 110, law = "kannisto"))
  expect_true(all(diff(E$x) > 0))
  # HP / whole-table Wittstein / an underflowing Gompertz integral: each refusal
  # leaves the reciprocal-close table untouched
  mxu <- mxo
  mxu[14:16] <- 1e3                     # ages 60, 65 and 70: the fit window
  expect_warning(LT <- LifeTable(x = xo, mx = mxo, close = "HP"),
                 regexp = "closing law 'HP' could not be fitted")
  expect_identical(LT$lt, A0)
  expect_warning(LT <- LifeTable(x = xo, mx = mxo, close = "wittstein",
                                 fit_from = 0),
                 regexp = "could not be fitted")
  expect_identical(LT$lt, A0)
  expect_warning(LT <- LifeTable(x = xo, mx = mxu, close = "gompertz"),
                 regexp = "could not be fitted")
  expect_equal(LT$lt$mx[length(xo)], mxo[length(xo)], tolerance = 1e-12)
})
test_that("guards: a failed extension leaves the table alone", {
  # an omega that does not reach the next grid point gives a single point
  expect_warning(LT <- LifeTable(x = xo, mx = mxo, omega = 77),
                 regexp = "extension could not be computed")
  expect_identical(LT$lt$x, as.numeric(xo))
  expect_equal(tail(LT$lt$mx, 1), mxo[length(mxo)], tolerance = 1e-12)
  # the grid may not grow when the law that would fill it is not estimable
  expect_warning(LT <- LifeTable(x = xo, mx = mxo, omega = 110, close = "HP"),
                 regexp = "extension could not be computed")
  expect_identical(LT$lt, A0)
  # Opperman turns negative just above the fitted rates, so age 12 is refused
  ms2 <- c(0.2, 0.1, 0.05, 0.03, 0.02, 0.01)
  expect_warning(LT <- LifeTable(x = xs, mx = ms2, omega = 12, close = "opperman",
                                 fit_from = 0, ax = "cfm"),
                 regexp = "extension could not be computed")
  expect_identical(LT$lt$x, as.numeric(xs))
  expect_equal(LT$lt$mx[6], ms2[6], tolerance = 1e-12)
})
test_that("guards: the old-age repair and the ex inverse", {
  # no caller passes verbose = TRUE: the replacement is the highest rate >= omega
  ux <- rep(0.05, 106)
  ux[1]   <- 0.5    # larger, but below omega: must not be used
  ux[101] <- NA
  ux[102] <- 0
  ux[103] <- Inf
  ux[104] <- 0.2    # the largest usable rate above omega
  ux[105] <- NaN
  ux[106] <- 0
  expect_message(out <- repair_above_omega(x = x, ux = ux, omega = 100,
                                           verbose = TRUE),
                 regexp = "maximum observed value: 0.2")
  expect_equal(out[c(101, 102, 103, 105, 106)], rep(0.2, 5), tolerance = 1e-12)
  expect_equal(out[104], 0.2, tolerance = 1e-12)
  expect_equal(out[1:100], ux[1:100], tolerance = 1e-12)
  # compute_life_table() expands a scalar ax first, so ex_inverse() recycles it
  ex5 <- c(70, 69.5, 69, 68.5, 68, 67.5)
  nx1 <- rep(1, length(ex5))
  expect_message(out <- ex_inverse(x = xs, nx = nx1, ex = ex5, ax = 0.5),
                 regexp = "open age interval")
  expect_identical(out, ex_inverse(x = xs, nx = nx1, ex = ex5,
                                   ax = c(rep(0.5, 5), ex5[6])))
  expect_equal(out$ax[length(ex5)], ex5[length(ex5)], tolerance = 1e-12)
  # e(x) falling by more than the interval implies a negative death probability
  ex6 <- c(70, 69.5, 69.1, 60, 59.5, 59)
  expect_error(LifeTable(x = xs, ex = ex6, ax = c(rep(0.5, 5), ex6[6])),
               regexp = "probabilities outside \\[0, 1\\] at age\\(s\\) 2")
  ex7 <- c(70, 69, 68, 67, 66, 65)
  expect_error(
    LifeTable(x = xs, ex = ex7, ax = c(0.5, 0.5, 0.5, 0.5, 70, ex7[6])),
    regexp = "starting at age 4 has a non-positive")
})

test_that("an ax method adjusting the open interval stays silent, a supplied one is kept", {
  # The open interval follows the closing rule, so an ax method assigning it
  # is implied; only a user-supplied value is announced when it is replaced.
  expect_no_warning(LT <- LifeTable(x = grid_single, mx = mx_1950(grid_single)))
  expect_no_warning(LTM <- LifeTable(x = grid_single,
                                     mx = cbind(a = mx_1950(grid_single),
                                                b = mx_1950(grid_single) * 1.1)))
  expect_no_warning(LTE <- LifeTable(x = grid_single, ex = LT$lt$ex))
  # a user-supplied value is kept for the open interval, and announced
  expect_message(LT8 <- LifeTable(x = grid_small, mx = mx_small, ax = 0.5),
                 regexp = "open age interval")
  expect_equal(LT8$lt$ax[nrow(LT8$lt)], 0.5, tolerance = 1e-12)
})

# ---- convertFx ---------------------------------------------------------------
# The 0:105 schedule with the four AHMD columns; matrices convert column by column.
SRC  <- list(mx = ahmd$mx[paste0(x), ])
# A supplied ax is kept at the closing interval, which makes the table's ax and
# mx columns disagree there; these round trips need a table that is consistent,
# so they take the default ax method. The supplied-ax contract is pinned above.
for (to in c("qx", "dx", "lx", "ex")) {
  SRC[[to]] <- fx(x = x, data = SRC$mx, from = "mx", to = to)
}
XM <- as.matrix(SRC$mx)                # the matrix branch of the same panel
XD <- as.matrix(ahmd$Dx[paste0(x), ])
XQ <- fx(x = x, data = XM, from = "mx", to = "qx")
XL <- fx(x = x, data = XM, from = "mx", to = "lx")
test_that("convertFx covers all 35 from-to combinations", {
  # one ax on both sides: the identity holds exactly only then; rows 1..N-1 are compared
  # expand.grid returns factors; match.arg() needs plain character.
  K   <- expand.grid(from = c("mx", "qx", "dx", "lx", "ex"),
                     to = c("mx", "qx", "dx", "lx", "Lx", "Tx", "ex"),
                     stringsAsFactors = FALSE)
  OUT <- lapply(seq_len(nrow(K)), function(i) {
    fx(x = x, data = SRC[[K$from[i]]], from = K$from[i], to = K$to[i])
  })
  names(OUT) <- paste0(K$to, "_from_", K$from)
  n <- length(x)
  for (cc in c("mx", "qx", "dx", "lx", "Lx")) {
    Ref <- OUT[[paste0(cc, "_from_dx")]][-n, ]
    expect_equal(Ref, OUT[[paste0(cc, "_from_lx")]][-n, ], tolerance = 1e-8)
    expect_equal(Ref, OUT[[paste0(cc, "_from_qx")]][-n, ], tolerance = 1e-8)
    # the ex source is an inverse problem: with an ax method the rates are
    # solved by iteration, so it settles to a tolerance rather than exactly
    expect_equal(Ref, OUT[[paste0(cc, "_from_ex")]][-n, ], tolerance = 5e-5)
  }
  # the ex input pins the open rate as m_N = 1/e_N, so it is excluded here
  expect_equal(OUT$ex_from_dx[-n, ], OUT$ex_from_lx[-n, ], tolerance = 1e-8)
  expect_equal(OUT$ex_from_dx[-n, ], OUT$ex_from_qx[-n, ], tolerance = 1e-8)
})
test_that("convertFx validates its inputs and takes plain numeric data", {
  # the vector branch guards its own shape: a mismatched pair is a length mismatch
  expect_error(fx(x = 2:11, data = 1:5, from = "mx", to = "qx"),
               regexp = "do not have the same length")
  # the matrix branch reports the row count instead
  expect_error(fx(x = 10:15, data = SRC$mx, from = "mx", to = "qx"),
               regexp = "must be equal to the number of rows")
  # F31: integer vectors used to error in the matrix branch; they are valid input
  out_int <- fx(x = x, data = x, from = "mx", to = "qx")
  expect_equal(vals(out_int),
               vals(fx(x = x, data = as.numeric(x), from = "mx", to = "qx")))
  expect_true(all(is.finite(out_int)))
  expect_true(all(fx(x = x, data = SRC$mx[, 1], from = "mx", to = "qx") >= 0))
})
test_that("convertFx takes ex as a source and round-trips it", {
  # compared on one column to keep the shapes aligned; a matrix keeps shape and names
  n    <- length(x)
  e1   <- SRC$ex[, 1]
  qx_e <- fx(x = x, data = e1, from = "ex", to = "qx")
  mx1  <- fx(x = x, data = SRC$mx[, 1], from = "mx", to = "mx")
  expect_equal(vals(qx_e), SRC$qx[, 1], tolerance = 1e-8)
  expect_equal(vals(unname(fx(x = x, data = e1, from = "ex", to = "mx"))),
               vals(unname(mx1)), tolerance = 1e-8)
  Mex <- fx(x = x, data = cbind(a = e1, b = e1 + 1), from = "ex", to = "mx")
  expect_equal(dim(Mex), c(n, 2))
  expect_equal(colnames(Mex), c("a", "b"))
})
test_that("convertFx forwards omega and close, relabelling the grid", {
  # omega keeps the extended ages as names; close keeps the grid and the rows
  e1 <- fx(x = xo, data = mxon, from = "mx", to = "ex", omega = 110)
  expect_equal(length(e1), 24)
  expect_equal(as.integer(names(e1)), c(0, 1, seq(5, 110, by = 5)))
  expect_equal(e1[["110"]], 1/lt(x = xo, mx = mxon, omega = 110)$lt$mx[24],
               tolerance = 1e-12)
  M   <- cbind(a = mxon, b = mxon * 1.1)
  exm <- fx(x = xo, data = M, from = "mx", to = "ex", omega = 110)
  expect_equal(dim(exm), c(24, 2))
  expect_equal(rownames(exm)[24], "110")
  expect_equal(colnames(exm), c("a", "b"))
  e2 <- fx(x = xo, data = mxon, from = "mx", to = "ex", close = "kannisto")
  expect_equal(length(e2), length(xo))
  expect_equal(names(e2), names(mxon))
  expect_equal(vals(unname(e2)),
               unname(lt(x = xo, mx = mxon, close = "kannisto")$lt$ex),
               tolerance = 1e-12)
  exc <- fx(x = xo, data = M, from = "mx", to = "ex", close = "kannisto")
  expect_equal(dim(exc), c(length(xo), 2))
  expect_equal(rownames(exc), names(mxon))
})
test_that("convertFx converts a matrix column by column", {
  # the matrix/single-column agreement (the C4 bridge pin's premise) is proved here
  mcols <- function(In, Out, MM) {
    mat <- fx(x = x, data = MM, from = In, to = Out)
    expect_equal(dim(mat), dim(MM))
    for (j in seq_len(ncol(MM))) {
      vec <- fx(x = x, data = MM[, j], from = In, to = Out)
      expect_equal(unname(mat[, j]), vals(unname(vec)), tolerance = 1e-12)
    }
  }
  mcols("mx", "qx", XM)
  mcols("qx", "mx", XQ)
  mcols("dx", "lx", XD)
  mcols("lx", "dx", XL)
  # a one-column matrix keeps its N x 1 shape, dimnames and vector-conversion values
  m1 <- XM[, 1, drop = FALSE]
  dimnames(m1) <- list(paste0("age.", x), "SWE")
  one_col <- function(In, Out, data = m1, ax = "andreev_kingkade") {
    m <- fx(x = x, data = data, from = In, to = Out, ax = ax)
    v <- fx(x = x, data = data[, 1], from = In, to = Out, ax = ax)
    expect_true(is.matrix(m))
    expect_equal(dim(m), c(length(x), 1))
    expect_identical(dimnames(m), dimnames(m1))
    expect_equal(unname(m[, 1]), vals(unname(v)), tolerance = 1e-12)
    m
  }
  qx1 <- one_col("mx", "qx")
  one_col("qx", "mx", data = qx1)
  ex1 <- one_col("mx", "ex")
  one_col("ex", "mx", data = ex1)
  # lx0 left NULL must mean LifeTable's default 1e5: asking for it cannot change it
  lx_def <- convertFx(x = x, data = XD, from = "dx", to = "lx")
  expect_equal(lx_def,
               convertFx(x = x, data = XD, from = "dx", to = "lx", lx0 = 1e5))
  expect_equal(unname(lx_def[1, ]), rep(1e5, ncol(XD)))
  dx_def <- convertFx(x = x, data = XL, from = "lx", to = "dx")
  expect_equal(dx_def,
               convertFx(x = x, data = XL, from = "lx", to = "dx", lx0 = 1e5))
  expect_equal(unname(colSums(dx_def)), rep(1e5, ncol(XL)), tolerance = 1e-8)
  # a matrix with no column names is labelled by index; ages stay the row labels
  Mn <- XM; dimnames(Mn) <- NULL
  unnamed <- fx(x = x, data = Mn, from = "mx", to = "qx")
  expect_equal(dim(unnamed), dim(Mn))
  expect_equal(colnames(unnamed), as.character(seq_len(ncol(Mn))))
  expect_equal(rownames(unnamed), as.character(x))
  expect_equal(vals(unname(unnamed)), vals(unname(XQ)), tolerance = 1e-12)
})
test_that("Contract 10: convertFx round-trips its columns and the matrix shape", {
  qx1 <- fx(x = x, data = mx, from = "mx", to = "qx")
  mx2 <- fx(x = x, data = qx1, from = "qx", to = "mx")
  # the forward leg closes with qx[N] = 1, so mx[N] is the extrapolated closure
  expect_equal(qx1[length(x)], 1)
  expect_equal(mx2[-length(x)], mx[-length(x)], tolerance = 1e-8)
  expect_true(is.finite(mx2[length(x)]) && mx2[length(x)] > 0)
  lx1 <- fx(x = x, data = mx, from = "mx", to = "lx")
  dx1 <- fx(x = x, data = lx1, from = "lx", to = "dx")
  expect_equal(vals(fx(x = x, data = dx1, from = "dx", to = "lx")), vals(lx1),
               tolerance = 1e-8)
  M   <- ahmd$mx[paste0(x), c("1950", "2010")]
  out <- fx(x = x, data = M, from = "mx", to = "qx")
  expect_true(is.matrix(out))
  expect_identical(dim(out), dim(M))
  expect_identical(dimnames(out), dimnames(M))
})

# ---- Edge and matrix-bridge pins ---------------------------------------------
test_that("Edge: a mid-table q = 1 and an integer lx input", {
  # q = 1 at age 50: everyone dies there, but the table must stay finite
  qx1 <- lt(x = x, mx = mx)$lt$qx
  qx1[51] <- 1
  LT  <- lt(x = x, qx = qx1)
  num <- c("mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex")
  expect_true(all(is.finite(as.matrix(LT$lt[, num]))))
  expect_gt(LT$lt$ex[51], 0)
  # an integer radix-scaled survivorship is valid numeric input
  xi <- 0:90
  LT3 <- lt(x = xi, lx = as.integer(round(lt(x = xi, mx = mx_1950(xi))$lt$lx)))
  expect_s3_class(LT3, "LifeTable")
  expect_true(all(is.finite(LT3$lt$ex)))
})
test_that("C4: the mx/qx and dx/lx bridges stay matrix-safe", {
  xc <- 0:10
  nx <- c(diff(xc), 1)
  M  <- cbind(a = seq(0.01, 0.20, length.out = 11),
              b = seq(0.02, 0.30, length.out = 11))
  # q[x] = 1 in each column; the old vector index wrote it only at M[11, 2]
  qx <- mx_qx(x = xc, nx = nx, ux = M, out = "qx")
  expect_identical(dim(qx), dim(M))
  expect_equal(unname(qx[11, ]), c(1, 1))
  expect_lt(max(qx[1:10, ]), 1)
  # column 1's non-finite closing rate: mx[11] = mx[10]^2 / mx[9]; column 2 untouched
  M[11, 1] <- Inf
  r <- repair_mx(mx = M, nx = nx)
  expect_true(all(is.finite(r)))
  expect_equal(r[11, 1], M[10, 1]^2/M[9, 1], tolerance = 1e-12)
  expect_equal(r[, 2], M[, 2], tolerance = 1e-12)
  dx <- cbind(a = seq(20, 5, length.out = 11),
              b = seq(10, 2, length.out = 11))
  lx <- dx_lx(ux = dx, out = "lx")
  expect_identical(dim(lx), dim(dx))
  expect_equal(lx[1, ], colSums(dx), tolerance = 1e-12)
  expect_equal(lx[, 1], dx_lx(ux = dx[, 1], out = "lx"), tolerance = 1e-12)
  expect_equal(dx_lx(ux = lx, out = "dx"), dx, tolerance = 1e-12)
})
