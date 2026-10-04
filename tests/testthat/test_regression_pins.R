# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-03
# --------------------------------------------
# Regression pins for the defects fixed by the 2026-10 review wave.
# Each test_that() block names the shared contract it pins and derives its
# expected values from the published formula (shown in the comment), never
# from the package internals, so an implementation change is caught here.
# --------------------------------------------


# ---- Contract 2: goodness of fit --------------------------------------------

test_that("Contract 2: poissonL logLik equals the objective at the optimum", {
  x  <- 45:75
  Dx <- ahmd$Dx[paste(x), "1950"]
  Ex <- ahmd$Ex[paste(x), "1950"]
  M  <- MortalityLaw(x = x,
                     Dx = Dx,
                     Ex = Ex,
                     law = "makeham",
                     opt.method = "poissonL")
  mu <- as.numeric(fitted(M))

  # Poisson log-likelihood at the optimum (contract 2):
  #   logLik = -sum(loss), loss = -(Dx*log(mu) - mu*Ex)
  #          = sum(Dx*log(mu) - mu*Ex) over the fitting ages
  logLik_hand <- sum(Dx * log(mu) - mu * Ex)
  # AIC = 2k - 2*logLik with k = 3 makeham parameters
  expect_equal(as.numeric(logLik(M)), logLik_hand, tolerance = 1e-6)
  expect_equal(as.numeric(AIC(M)), 2 * 3 - 2 * logLik_hand, tolerance = 1e-6)
})

test_that("Contract 2: loss-function fits keep logLik and AIC at NaN", {
  x  <- 45:75
  Dx <- ahmd$Dx[paste(x), "1950"]
  Ex <- ahmd$Ex[paste(x), "1950"]
  M  <- MortalityLaw(x = x,
                     Dx = Dx,
                     Ex = Ex,
                     law = "makeham",
                     opt.method = "LF2")
  expect_true(is.nan(AIC(M)))
  expect_true(is.nan(logLik(M)))
})


# ---- Contract 3: life table identities --------------------------------------

test_that("Contract 3: user ax drives the exact qx identity and mx round-trip", {
  x  <- c(0, 1, seq(5, 100, by = 5))
  mx <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
          .004, .005, .006, .0093, .0129, .019, .031, .049,
          .084, .129, .180, .2354, .3085, .390)
  ax <- c(0.1, 1.5, rep(2, 18), 1, 1)
  N  <- length(x)
  nx <- c(diff(x), diff(x)[N - 1])

  # The user's closing-age ax is overridden by 1/mx[N] (contract 3).
  expect_warning(LT <- LifeTable(x = x, mx = mx, ax = ax))
  axr <- LT$lt$ax

  # Exact identities with a user-supplied ax (contract 3):
  #   qx = nx*mx / (1 + (nx - ax)*mx)
  #   mx = qx / (ax*qx + nx*(1 - qx))  ==  dx / Lx
  idx <- 1:(N - 1)
  expect_equal(LT$lt$qx[idx],
               nx[idx] * mx[idx] / (1 + (nx[idx] - axr[idx]) * mx[idx]),
               tolerance = 1e-12)
  expect_equal(LT$lt$mx, LT$lt$dx / LT$lt$Lx, tolerance = 1e-10)

  # Open age interval: ax[N] = ex[N] = 1/mx[N] and Lx[N] = lx[N]/mx[N]
  expect_equal(axr[N], 1 / LT$lt$mx[N], tolerance = 1e-12)
  expect_equal(LT$lt$ex[N], 1 / LT$lt$mx[N], tolerance = 1e-12)
  expect_equal(LT$lt$Lx[N], LT$lt$lx[N] / LT$lt$mx[N], tolerance = 1e-12)
})

test_that("Contract 3: Coale-Demeny child ax constants and continuity", {
  x  <- c(0, 1, seq(5, 100, by = 5))
  m0 <- 0.12
  mx <- c(m0, rep(0.01, length(x) - 1))

  LTm <- LifeTable(x = x, mx = mx, sex = "male")
  LTf <- LifeTable(x = x, mx = mx, sex = "female")
  # For m0 >= 0.107 (contract 3):
  #   male:   a0 = 0.330, a1 = 1.352
  #   female: a0 = 0.350, a1 = 1.361
  expect_equal(LTm$lt$ax[1:2], c(0.330, 1.352), tolerance = 1e-12)
  expect_equal(LTf$lt$ax[1:2], c(0.350, 1.361), tolerance = 1e-12)

  # a1 must be continuous across the m0 = 0.107 switch (within 0.02)
  lo <- 0.1069
  hi <- 0.1071
  lo_m <- LifeTable(x = x, mx = c(lo, rep(0.01, length(x) - 1)), sex = "male")
  hi_m <- LifeTable(x = x, mx = c(hi, rep(0.01, length(x) - 1)), sex = "male")
  lo_f <- LifeTable(x = x, mx = c(lo, rep(0.01, length(x) - 1)), sex = "female")
  hi_f <- LifeTable(x = x, mx = c(hi, rep(0.01, length(x) - 1)), sex = "female")
  expect_lt(abs(lo_m$lt$ax[2] - hi_m$lt$ax[2]), 0.02)
  expect_lt(abs(lo_f$lt$ax[2] - hi_f$lt$ax[2]), 0.02)
})

test_that("Coale-Demeny variants: default unchanged, both conventions correct", {
  x  <- c(0, 1, seq(5, 110, by = 5))
  mx <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
          .004, .005, .006, .0093, .0129, .019, .031, .049,
          .084, .129, .180, .2354, .3085, .390, .478, .551)

  # The default is the Preston parameterisation: byte-identical to spelling
  # it out, and to the pre-argument behaviour.
  base <- LifeTable(x, mx = mx, sex = "female")
  pr   <- LifeTable(x, mx = mx, sex = "female", ax_method = "preston")
  cd   <- LifeTable(x, mx = mx, sex = "female", ax_method = "coale_demeny")
  expect_identical(base$lt$ax, pr$lt$ax)
  expect_identical(base$lt, pr$lt)

  # The two conventions really differ below m0 = 0.107, in the first two
  # intervals only, and by no more than a few thousandths of a year.
  expect_false(isTRUE(all.equal(cd$lt$ax[1:2], pr$lt$ax[1:2])))
  expect_lt(max(abs(cd$lt$ax[1:2] - pr$lt$ax[1:2])), 0.005)
  expect_equal(cd$lt$ax[-(1:2)], pr$lt$ax[-(1:2)], tolerance = 1e-12)

  # Above the cutoff both conventions collapse to the same constants.
  mx2    <- mx
  mx2[1] <- 0.12
  hi_pr  <- LifeTable(x, mx = mx2, sex = "female")
  hi_cd  <- LifeTable(x, mx = mx2, sex = "female", ax_method = "coale_demeny")
  expect_identical(hi_pr$lt$ax[1:2], hi_cd$lt$ax[1:2])
  expect_equal(hi_pr$lt$ax[1:2], c(0.350, 1.361), tolerance = 1e-12)

  # The variant is ignored without a sex and when ax is supplied.
  expect_identical(LifeTable(x, mx = mx)$lt$ax,
                   LifeTable(x, mx = mx, ax_method = "coale_demeny")$lt$ax)
  my_ax <- c(0.1, 1.5, rep(2, length(x) - 5), 1, 1, 1)
  expect_identical(suppressWarnings(LifeTable(x, mx = mx, ax = my_ax))$lt$ax,
                   suppressWarnings(LifeTable(x, mx = mx, ax = my_ax,
                             ax_method = "coale_demeny"))$lt$ax)

  # Matrix input carries the variant per column.
  M <- cbind(f = mx, m = mx * 1.15)
  colnames(M) <- c("f", "m")
  expect_identical(LifeTable(x, mx = M, sex = "female")$lt$ax,
                   LifeTable(x, mx = M, sex = "female",
                             ax_method = "preston")$lt$ax)
  expect_false(identical(LifeTable(x, mx = M, sex = "female")$lt$ax,
                         LifeTable(x, mx = M, sex = "female",
                                   ax_method = "coale_demeny")$lt$ax))

  # An unknown value is rejected.
  expect_error(LifeTable(x, mx = mx, sex = "female", ax_method = "west"),
               regexp = "'arg' should be one of")
})

test_that("Contract 3: a single NA in mx corrupts the table only locally", {
  x  <- 0:60
  k  <- 31                       # age 30
  mx <- rep(0.01, length(x))
  mx[k] <- NA
  expect_warning(LT <- LifeTable(x = x, mx = mx))
  # Contract 3 NA rule: mx/qx/ax/dx[k] and Lx[k] are NA, Tx/ex are NA for
  # j <= k and finite for j > k; lx stays finite across the gap.
  expect_true(is.na(LT$lt$mx[k]))
  expect_true(is.na(LT$lt$qx[k]))
  expect_true(is.na(LT$lt$dx[k]))
  expect_true(is.na(LT$lt$Lx[k]))
  expect_true(all(is.finite(LT$lt$lx)))
  expect_true(all(is.na(LT$lt$ex[1:k])))
  expect_true(all(is.finite(LT$lt$ex[(k + 1):length(x)])))
  expect_true(all(LT$lt$ex[(k + 1):length(x)] > 0))
})

test_that("Contract 3: mx[1] = 0 does not cost a year of life expectancy", {
  x  <- 0:100
  LT0 <- LifeTable(x = x, mx = c(0, rep(0.01, length(x) - 1)))
  LTz <- LifeTable(x = x, mx = c(1e-12, rep(0.01, length(x) - 1)))
  expect_equal(LT0$lt$ex[1], LTz$lt$ex[1], tolerance = 1e-6)
})


# ---- Contract 4: published law formulas -------------------------------------

test_that("Contract 4: perks implements the published Perks (1932) formula", {
  par <- c(A = 0.002, B = 0.13, C = 1.1, D = 0.01)
  x   <- c(0, 20, 60)
  # Perks (1932), as fixed by the review (contract 4):
  #   hx = (A + B*C^x) / (1 + D*C^x)
  expected <- (par["A"] + par["B"] * par["C"]^x) / (1 + par["D"] * par["C"]^x)
  expect_equal(perks(x, par)$hx, as.numeric(expected), tolerance = 1e-12)
})

test_that("Contract 4: steffensen is the Steffensen (1930) Perks variant", {
  par <- c(A = 0.002, B = 0.13, C = 1.1, D = 0.01)
  x   <- c(0, 20, 60)
  # The formula MortalityLaws shipped as 'perks' before the split:
  expected <- (par["A"] + par["B"] * par["C"]^x) /
    (par["B"] * par["C"]^-x + 1 + par["D"] * par["C"]^x)
  expect_equal(steffensen(x, par)$hx, as.numeric(expected), tolerance = 1e-12)
  # Identity: steffensen == perks / (1 + B*C^-x/(1 + D*C^x)), so it is
  # dampened at young ages and converges to the Perks hazard at old ages.
  damp <- 1 + par["B"] * par["C"]^-x / (1 + par["D"] * par["C"]^x)
  expect_equal(steffensen(x, par)$hx, perks(x, par)$hx / damp,
               tolerance = 1e-12)
  expect_lt(steffensen(0, par)$hx, perks(0, par)$hx)
})

test_that("Contract 4: makeham returns the survival function", {
  par <- c(A = .0002, B = .13, C = .001)
  x   <- c(0, 20, 60)
  # Makeham (1860):
  #   Hx = (A/B) * (exp(B*x) - 1) + C*x ;  Sx = exp(-Hx)
  Hx <- (par["A"] / par["B"]) * (exp(par["B"] * x) - 1) + par["C"] * x
  expect_equal(makeham(x, par)$Sx, as.numeric(exp(-Hx)), tolerance = 1e-12)
})

test_that("Contract 4: thiele applies the full formula at age 0", {
  par <- c(A = .02474, B = .3, C = .004, D = .5, E = 25, F_ = .0001, G = .13)
  x   <- c(0, 20, 60)
  # Thiele (1871), no special case at x = 0:
  #   hx = A*exp(-B*x) + C*exp(-0.5*D*(x - E)^2) + F_*exp(G*x)
  expected <- par["A"] * exp(-par["B"] * x) +
              par["C"] * exp(-0.5 * par["D"] * (x - par["E"])^2) +
              par["F_"] * exp(par["G"] * x)
  expect_equal(thiele(x, par)$hx, as.numeric(expected), tolerance = 1e-12)
})

test_that("Contract 4: opperman keeps its documented x + 1 age shift", {
  par <- c(A = .04, B = .0004, C = .001)
  x   <- c(0, 20, 60)
  # Opperman (1870) is evaluated at the shifted age x + 1:
  #   hx = A/sqrt(x + 1) - B + C*sqrt(x + 1)
  expected <- par["A"] / sqrt(x + 1) - par["B"] + par["C"] * sqrt(x + 1)
  expect_equal(opperman(x, par)$hx, as.numeric(pmax(0, expected)), tolerance = 1e-12)
})

test_that("Contract 4: strehler_mildvan is the published 3-parameter form", {
  par <- c(A = 1e-4, B = 0.1, V = 0.02)
  x   <- c(0, 20, 60)
  # Strehler-Mildvan (1960), as fixed by the review (contract 4):
  #   hx = A * exp(B*x) * exp(-(V/B) * (1 - exp(-B*x)))
  expected <- par["A"] * exp(par["B"] * x) *
              exp(-(par["V"] / par["B"]) * (1 - exp(-par["B"] * x)))
  expect_equal(strehler_mildvan(x, par)$hx, as.numeric(expected), tolerance = 1e-12)
  expect_length(bring_parameters("strehler_mildvan"), 3)
  expect_identical(names(bring_parameters("strehler_mildvan")), c("A", "B", "V"))
})

test_that("Contract 4: kannisto cumulative hazard and survival", {
  par <- c(A = 0.5, B = 0.13)
  x   <- c(0, 20, 60)
  # Kannisto (1998), as fixed by the review (contract 4):
  #   hx = A*exp(B*x) / (1 + A*exp(B*x))
  #   Hx = (1/B) * log((1 + A*exp(B*x)) / (1 + A))
  #   Sx = exp(-Hx)
  hx <- par["A"] * exp(par["B"] * x) / (1 + par["A"] * exp(par["B"] * x))
  Hx <- (1 / par["B"]) * log((1 + par["A"] * exp(par["B"] * x)) / (1 + par["A"]))
  out <- kannisto(x, par)
  expect_equal(out$hx, as.numeric(hx), tolerance = 1e-12)
  expect_equal(out$Sx, as.numeric(exp(-Hx)), tolerance = 1e-12)
})

test_that("Contract 4: neggompertz is Thiele/Siler negative Gompertz", {
  par <- c(A = .02, B = .4)
  x   <- c(0, 20, 60)
  # Thiele (1871, p. 326); reused by Siler (1979) as the immaturity term:
  #   hx = A * exp(-B*x)
  expect_equal(neggompertz(x, par)$hx, as.numeric(par["A"] * exp(-par["B"] * x)),
               tolerance = 1e-12)
  expect_identical(names(bring_parameters("neggompertz")), c("A", "B"))
})

test_that("Contract 4: pareto_2 is the Pareto II (Lomax) hazard of de Beer-Janssen", {
  par <- c(A = .01, C = .001)
  x   <- c(0, 20, 60)
  # de Beer and Janssen (2016); Lomax (1954); a shifted power with exponent 1:
  #   hx = A / (x + C)
  expect_equal(pareto_2(x, par)$hx, as.numeric(par["A"] / (x + par["C"])),
               tolerance = 1e-12)
  expect_identical(names(bring_parameters("pareto_2")), c("A", "C"))
})

test_that("Contract 4: scholey_shifted_power is the Scholey (2019) shifted power hazard", {
  par <- c(A = .01, B = .7, C = .01)
  x   <- c(0, 20, 60)
  # Scholey (2019), the flexibly-shifted power hazard; a shifted Weibull hazard:
  #   hx = A * (x + C)^(-B)
  expect_equal(scholey_shifted_power(x, par)$hx,
               as.numeric(par["A"] * (x + par["C"])^(-par["B"])),
               tolerance = 1e-12)
  expect_identical(names(bring_parameters("scholey_shifted_power")), c("A", "B", "C"))
})

test_that("Contract 4: scholey is the truncated power hazard and nests its children", {
  par <- c(A = .01, B = .7, C = .01, D = .1)
  x   <- c(0, 20, 60)
  # Scholey (2019), exponentially-truncated shifted power:
  #   hx = A * (x + C)^(-B) * exp(-D*x)
  expected <- par["A"] * (x + par["C"])^(-par["B"]) * exp(-par["D"] * x)
  expect_equal(scholey(x, par)$hx, as.numeric(expected), tolerance = 1e-12)
  expect_identical(names(bring_parameters("scholey")), c("A", "B", "C", "D"))

  # The family nests its children as limits. Parameters must be strictly
  # positive here, so approach the boundary rather than setting it: as D -> 0
  # the truncated power collapses onto the shifted power hazard.
  par_d <- c(A = .01, B = .7, C = .01, D = 1e-12)
  sp    <- scholey_shifted_power(x, c(A = .01, B = .7, C = .01))$hx
  expect_equal(scholey(x, par_d)$hx, as.numeric(sp), tolerance = 1e-10)
  # As B -> 1 it collapses onto the Pareto II hazard.
  par_b <- c(A = .01, B = 1 + 1e-12, C = .001, D = 1e-12)
  pt    <- pareto_2(x, c(A = .01, C = .001))$hx
  expect_equal(scholey(x, par_b)$hx, as.numeric(pt), tolerance = 1e-10)
})

test_that("Contract 2: count deviance, residuals and dispersion match a Poisson GLM", {
  x  <- 45:95
  Dx <- ahmd$Dx[paste(x), "1950"]
  Ex <- ahmd$Ex[paste(x), "1950"]

  # gompertz (mu = A exp(Bx)) is log-linear, so glm(Dx ~ x, offset(log Ex))
  # has the same mean structure and is an exact reference.
  M <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = "gompertz",
                    opt.method = "poissonL")
  g <- glm(Dx ~ x, offset = log(Ex), family = poisson(link = "log"))

  # The reported deviance is the Poisson deviance the optimiser minimised.
  expect_equal(deviance(M), deviance(g), tolerance = 1e-6)
  # Pearson and deviance residuals match the GLM's.
  expect_equal(unname(M$pearson.residuals), unname(residuals(g, "pearson")),
               tolerance = 1e-4)
  expect_equal(unname(M$deviance.residuals), unname(residuals(g)),
               tolerance = 1e-4)
  # Dispersion is the Pearson chi-square over the residual df (the GLM
  # dispersion estimate; summary(glm)$dispersion is fixed at 1 for the Poisson
  # family and is not this quantity).
  x2 <- sum(residuals(g, "pearson")^2)
  expect_equal(dispersion(M), x2 / df.residual(g), tolerance = 1e-4)
  # Residual df excludes the fitted parameters.
  expect_equal(unname(df.residual(M)), length(x) - 2)
})

test_that("Contract 2: rate fits keep the squared log-residual deviance", {
  x  <- 45:95
  mx <- ahmd$mx[paste(x), "1950"]
  M  <- MortalityLaw(x = x, mx = mx, law = "gompertz", opt.method = "LF2")

  mu  <- as.numeric(fitted(M))
  sse <- sum((log(mx) - log(mu))^2)
  expect_equal(deviance(M), sse, tolerance = 1e-10)
  # For a rate fit the residuals are the log-residuals and the dispersion is
  # their mean square.
  expect_equal(unname(M$pearson.residuals), log(mx) - log(mu), tolerance = 1e-12)
  expect_equal(dispersion(M), sse / (length(x) - 2), tolerance = 1e-10)
})

test_that("Contract 2: count dispersion is 1 for a saturated Poisson fit", {
  # A Poisson model that fits the data exactly (n parameters, n points) leaves
  # the Pearson chi-square at ~0 and the dispersion at ~0.
  x  <- 45:50
  Dx <- ahmd$Dx[paste(x), "1950"]
  Ex <- ahmd$Ex[paste(x), "1950"]
  M  <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = "custom.law",
                     custom.law = function(x, par = c(a = 1)) {
                       list(hx = Dx / Ex, par = c(a = 1))
                     },
                     opt.method = "poissonL")
  # fitted equals the observed rate, so every Poisson residual vanishes.
  expect_lt(dispersion(M), 1e-6)
  expect_lt(deviance(M), 1e-6)
})

test_that("Contract 4: scholey warns when the truncation is unidentified", {
  x  <- 0:15
  mx <- ahmd$mx[paste(x), "1950"]
  # On coarse (year-resolution) infant data D collapses to the boundary and the
  # exponential term contributes nothing; the fit must say so.
  expect_warning(
    MortalityLaw(x = x, mx = mx, law = "scholey", opt.method = "LF2"),
    regexp = "truncation parameter 'D' of 'scholey' fitted at the boundary"
  )
  # At day resolution with a genuine truncated-power hazard, D is identified
  # and no warning is raised.
  xd <- 0:365
  mu <- 2e-2 * (xd + 3e-4)^(-0.7) * exp(-7e-3 * xd)
  expect_no_warning(
    MortalityLaw(x = xd, mx = mu, law = "scholey", opt.method = "LF2")
  )
})

test_that("Contract 4: HP returns the published q(x) form via eta/(1 + eta)", {
  par <- c(A = .0005, B = .004, C = .08, D = .001,
           E = 10, F_ = 17, G = .00005, H = 1.1)
  x   <- c(0, 20, 60)
  # Heligman-Pollard (1980):
  #   eta = A^((x + B)^C) + D*exp(-E*(log(x/F_))^2) + G*H^x
  #   qx  = eta / (1 + eta)
  eta <- par["A"]^((x + par["B"])^par["C"]) +
         par["D"] * exp(-par["E"] * (log(x / par["F_"]))^2) +
         par["G"] * par["H"]^x
  expect_equal(HP(x, par)$hx, as.numeric(eta / (1 + eta)), tolerance = 1e-12)
})

test_that("Contract 4: carriere1 uses sigma2 in the inverse-Weibull component", {
  par1 <- c(P1 = .003, sigma1 = 15, M1 = 2.7,
            P2 = .007, sigma2 = 6, M2 = 3,
            sigma3 = 9.5, M3 = 88)
  par2 <- par1
  par2["sigma2"] <- 50
  x <- 10:80
  # sigma2 must not be an unidentified parameter anymore (F7)
  expect_false(isTRUE(all.equal(carriere1(x, par1)$hx, carriere1(x, par2)$hx)))
})


# ---- Contract 5: bring_parameters -------------------------------------------

test_that("Contract 5: named par is matched by name, in any order", {
  p1 <- c(A = .002, B = .13, C = .001)
  p2 <- c(C = .001, B = .13, A = .002)
  expect_identical(bring_parameters("makeham", p1), bring_parameters("makeham", p2))
  expect_equal(unname(bring_parameters("makeham", p1)), unname(p1))
})

test_that("Contract 5: bring_parameters rejects invalid par", {
  expect_error(bring_parameters("makeham", c(A = .002, B = .13, Z = .001)))
  expect_error(bring_parameters("makeham", c(A = .002, B = .13)))
  expect_error(bring_parameters("makeham", c(A = -1, B = .13, C = .001)))
  expect_error(bring_parameters("makeham", c(A = 0, B = .13, C = .001)))
})

test_that("Contract 5: unnamed par keeps the positional convention", {
  p  <- c(.002, .13, .001)
  bp <- bring_parameters("makeham", p)
  expect_equal(unname(bp), p)
  expect_identical(names(bp), c("A", "B", "C"))
})


# ---- Contract 6: S3 methods --------------------------------------------------

test_that("Contract 6: predict works for a single age", {
  x  <- 45:75
  mx <- ahmd$mx[paste(x), "1950"]
  M  <- MortalityLaw(x = x, mx = mx, law = "makeham")
  p90 <- predict(M, x = 90)
  expect_type(p90, "double")
  expect_length(p90, 1)
  expect_identical(names(p90), "90")
  expect_equal(p90, predict(M, x = 85:90)["90"])
})

test_that("Contract 6: logLik is a classed logLik usable by stats::BIC", {
  x  <- 45:75
  Dx <- ahmd$Dx[paste(x), "1950"]
  Ex <- ahmd$Ex[paste(x), "1950"]
  M  <- MortalityLaw(x = x,
                     Dx = Dx,
                     Ex = Ex,
                     law = "makeham",
                     opt.method = "poissonL")
  ll <- logLik(M)
  expect_s3_class(ll, "logLik")
  # k = 3 makeham parameters; n = 31 fitted ages -> df = 3, nobs = 31
  expect_equal(unname(attr(ll, "df")), 3)
  expect_equal(unname(attr(ll, "nobs")), 31)
  expect_true(is.finite(as.numeric(stats::BIC(ll))))
})

test_that("Contract 6: df.residual works for the matrix shape of df", {
  x  <- 45:75
  mx <- ahmd$mx[paste(x), c("1950", "2010")]
  M  <- MortalityLaw(x = x, mx = mx, law = "makeham")
  # makeham has 3 parameters; every column is fitted on all length(x) ages
  expect_equal(unname(df.residual(M)), rep(length(x) - 3, 2))
})


# ---- Contract 7: fitting engine ---------------------------------------------

test_that("Contract 7: fit.this.x is order-insensitive", {
  x   <- 45:75
  Dx  <- ahmd$Dx[paste(x), "1950"]
  Ex  <- ahmd$Ex[paste(x), "1950"]
  sub <- 50:70
  M1  <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = "makeham", fit.this.x = sub)
  M2  <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = "makeham", fit.this.x = rev(sub))
  expect_equal(unname(coef(M1)), unname(coef(M2)), tolerance = 1e-6)
})

test_that("Contract 7: multi-fit opt.diagnosis holds one element per column", {
  x  <- 45:75
  mx <- ahmd$mx[paste(x), c("1950", "2010")]
  M  <- MortalityLaw(x = x, mx = mx, law = "makeham")
  expect_type(M$opt.diagnosis, "list")
  expect_length(M$opt.diagnosis, ncol(mx))
  expect_false(is.null(M$opt.diagnosis[[1]]))
})

test_that("Contract 7: a non-converged fit warns but still returns", {
  # nlminb branch: a strictly decreasing objective cannot converge
  expect_warning(
    opt <- run_optimiser(foo = function(par) -par[1], start = c(a = 0), law = "makeham"),
    regexp = "did not converge \\(code 1\\)"
  )
  expect_equal(opt$convergence, 1)

  # optim branch (invweibull). The 1-D Nelder-Mead routine also emits an
  # unreliability note, so both warnings are collected explicitly.
  ws <- capture_warnings(
    opt2 <- run_optimiser(foo = function(par) -par[1], start = c(a = 0), law = "invweibull")
  )
  expect_true(any(grepl("did not converge \\(code 1\\)", ws)))
  expect_equal(opt2$convergence, 1)

  # end-to-end: vandermaen on ages 30-100 reports singular convergence
  x  <- 30:100
  mx <- ahmd$mx[paste(x), "1950"]
  expect_warning(
    M <- MortalityLaw(x = x, mx = mx, law = "vandermaen", opt.method = "LF2"),
    regexp = "did not converge"
  )
  expect_s3_class(M, "MortalityLaw")
})


# ---- Contract 8: LawTable ----------------------------------------------------

test_that("Contract 8: LawTable echoes the requested ages for scaled laws", {
  par <- c(A = .002, B = .13, C = .001)
  LT  <- LawTable(x = 45:100, par = par, law = "makeham")
  expect_s3_class(LT, "LifeTable")
  expect_equal(LT$lt$x, 45:100)
})

test_that("Contract 8: LawTable accepts a matrix of parameters", {
  par <- matrix(c(0.00717, 0.07789, 0.00363,
                  0.01018, 0.07229, 0.00001),
                nrow = 2, byrow = TRUE,
                dimnames = list(c("m1", "m2"), c("A", "B", "C")))
  LT <- LawTable(x = 45:100, par = par, law = "makeham")
  expect_s3_class(LT, "LifeTable")
  expect_equal(nrow(LT$lt), 2 * length(45:100))
  expect_equal(LT$lt$x, rep(45:100, 2))
})


# ---- Contract 9: plot guard --------------------------------------------------

test_that("Contract 9: multi-fit plot errors with the exact message", {
  x  <- 45:75
  mx <- ahmd$mx[paste(x), c("1950", "2010")]
  M  <- MortalityLaw(x = x, mx = mx, law = "makeham")
  msg <- tryCatch(plot(M), error = conditionMessage)
  expect_identical(msg, "Plot function not available for multiple mortality curves")
})


# ---- Contract 10: convertFx --------------------------------------------------

test_that("Contract 10: convertFx round-trips mx -> qx -> mx and mx -> lx -> dx -> lx", {
  x  <- 0:105
  N  <- length(x)
  mx <- ahmd$mx[paste0(x), "1950"]
  qx  <- convertFx(x, data = mx, from = "mx", to = "qx")
  mx2 <- convertFx(x, data = qx, from = "qx", to = "mx")
  # the forward leg closes the table with qx[N] = 1 by convention
  expect_equal(qx[N], 1)
  # The closing age is q[N] = 1 by life-table convention, so the recovered
  # mx[N] is the extrapolated closure value; the round trip is pinned below it.
  expect_equal(mx2[-N], mx[-N], tolerance = 1e-8)
  expect_true(is.finite(mx2[N]) && mx2[N] > 0)

  lx  <- convertFx(x, data = mx, from = "mx", to = "lx")
  dx  <- convertFx(x, data = lx, from = "lx", to = "dx")
  lx2 <- convertFx(x, data = dx, from = "dx", to = "lx")
  expect_equal(lx2, lx, tolerance = 1e-8)
})

test_that("Contract 10: convertFx returns a matrix for matrix input", {
  x  <- 0:105
  mx <- ahmd$mx[paste0(x), c("1950", "2010")]
  out <- convertFx(x, data = mx, from = "mx", to = "qx")
  expect_true(is.matrix(out))
  expect_identical(dim(out), dim(mx))
  expect_identical(dimnames(out), dimnames(mx))
})


# ---- Edge pins from the review ----------------------------------------------

test_that("Edge: q = 1 at a mid-table age keeps the table finite", {
  x  <- 0:100
  mx <- ahmd$mx[paste(x), "1950"]
  qx <- LifeTable(x = x, mx = mx)$lt$qx
  qx[51] <- 1                       # age 50: everyone dies
  LT <- LifeTable(x = x, qx = qx)
  num <- c("mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex")
  expect_true(all(is.finite(as.matrix(LT$lt[, num]))))
  expect_gt(LT$lt$ex[51], 0)
})

test_that("Edge: unnamed multi-column mx matrix works", {
  x  <- 0:100
  mx <- unname(as.matrix(ahmd$mx[paste(x), 1:3]))
  LT <- LifeTable(x = x, mx = mx)
  expect_s3_class(LT, "LifeTable")
  expect_equal(nrow(LT$lt), 3 * length(x))
})

test_that("Edge: integer lx input is accepted", {
  x   <- 0:90
  mx  <- ahmd$mx[paste(x), "1950"]
  lx  <- as.integer(round(LifeTable(x = x, mx = mx)$lt$lx))
  LT  <- LifeTable(x = x, lx = lx)
  expect_s3_class(LT, "LifeTable")
  expect_true(all(is.finite(LT$lt$ex)))
})

test_that("Edge: the LifeTable custom-ax example with 24 ages runs", {
  x  <- c(0, 1, seq(5, 110, by = 5))
  mx <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
          .004, .005, .006, .0093, .0129, .019, .031, .049,
          .084, .129, .180, .2354, .3085, .390, .478, .551)
  my_ax <- c(0.1, 1.5, rep(2, 19), 1, 1, 1)
  expect_length(x, 24)
  expect_length(my_ax, 24)
  expect_warning(LT <- LifeTable(x = x, mx = mx, ax = my_ax))
  expect_s3_class(LT, "LifeTable")
  expect_true(all(is.finite(LT$lt$ex)))
})

test_that("Coverage: every available loss function executes", {
  x  <- 45:75
  mx <- ahmd$mx[paste(x), "1950"]
  for (m in availableLF()$table$CODE) {
    if (m == "LF6") {
      # LF6 on this grid reports false convergence (contract 7 warning)
      expect_warning(
        M <- MortalityLaw(x = x, mx = mx, law = "makeham", opt.method = m),
        regexp = "did not converge"
      )
    } else {
      M <- MortalityLaw(x = x, mx = mx, law = "makeham", opt.method = m)
    }
    expect_s3_class(M, "MortalityLaw")
    expect_true(all(is.finite(coef(M))))
    if (m %in% c("poissonL", "binomialL")) {
      expect_false(is.nan(AIC(M)))
    } else {
      # LF1-LF6 keep logLik = AIC = BIC = NaN (contract 2)
      expect_true(all(is.nan(M$goodness.of.fit)))
    }
  }
})


# ---- C4: the mx/qx and dx/lx bridges are matrix-safe -------------------------

test_that("C4: mx_qx closes the last age of every column, not the last element", {
  x  <- 0:10
  nx <- c(diff(x), 1)
  M  <- cbind(a = seq(0.01, 0.20, length.out = 11),
              b = seq(0.02, 0.30, length.out = 11))

  qx <- mx_qx(x = x, nx = nx, ux = M, out = "qx")

  # q[x] = 1 in each column; the old vector index wrote it only at M[11, 2].
  expect_identical(dim(qx), dim(M))
  expect_equal(unname(qx[11, ]), c(1, 1))
  expect_lt(max(qx[1:10, ]), 1)
})

test_that("C4: repair_mx follows each column's own rates", {
  x  <- 0:10
  nx <- c(diff(x), 1)
  M  <- cbind(a = seq(0.01, 0.20, length.out = 11),
              b = seq(0.02, 0.30, length.out = 11))
  M[11, 1] <- Inf   # a non-finite closing rate in column 1 only

  r <- repair_mx(mx = M, nx = nx)

  expect_true(all(is.finite(r)))
  # Geometric continuation of the two previous rates of column 1:
  #   mx[11] = mx[10]^2 / mx[9]
  expect_equal(r[11, 1], M[10, 1]^2/M[9, 1], tolerance = 1e-12)
  # Column 2 was finite, so it is untouched.
  expect_equal(r[, 2], M[, 2], tolerance = 1e-12)
})

test_that("C4: dx_lx converts a whole matrix, column by column", {
  x  <- 0:10
  dx <- cbind(a = seq(20, 5, length.out = 11),
              b = seq(10, 2, length.out = 11))

  lx <- dx_lx(ux = dx, out = "lx")
  expect_identical(dim(lx), dim(dx))
  expect_equal(lx[1, ], colSums(dx), tolerance = 1e-12)
  expect_equal(lx[, 1], dx_lx(ux = dx[, 1], out = "lx"), tolerance = 1e-12)

  # Round trip back to dx
  expect_equal(dx_lx(ux = lx, out = "dx"), dx, tolerance = 1e-12)
})

test_that("C4: convertFx matrix output equals the single-column output", {
  x  <- 0:60
  M  <- ahmd$mx[paste(x), c("1950", "2010")]

  for (pair in list(c("mx", "dx"), c("dx", "lx"), c("lx", "dx"),
                    c("mx", "qx"), c("qx", "mx"))) {
    # The qx round trip warns that the derived qx was not closed; that is the
    # documented closure rule, not a defect, so it must not fail the pin.
    out <- suppressWarnings(
      convertFx(x = x, data = M, from = pair[1], to = pair[2], lx0 = 1e5)
    )

    for (j in seq_len(ncol(M))) {
      one <- suppressWarnings(
        convertFx(x = x, data = M[, j], from = pair[1], to = pair[2], lx0 = 1e5)
      )
      expect_equal(unname(out[, j]), unname(one), tolerance = 1e-10)
    }
  }
})


# ---- C3: the HMD availability table parses without an HTML dependency --------

test_that("C3: parse_html_table reads a table with a header and padded rows", {
  html <- paste0(
    "<html><body><table class=\"x\">",
    "<thead><tr><th>Country and data series</th><th>Code</th>",
    "<th>Period Life Tables</th></tr></thead>",
    "<tbody><tr><td><a href=\"/x\">Australia</a></td><td>AUS</td>",
    "<td>1921 - 2021</td></tr>",
    "<tr><td>Iceland</td><td>ISL</td></tr></tbody>",
    "</table></body></html>"
  )

  tab <- parse_html_table(html = html)

  expect_s3_class(tab, "data.frame")
  expect_identical(dim(tab), c(2L, 3L))
  expect_identical(colnames(tab),
                   c("Country and data series", "Code", "Period Life Tables"))
  # Markup inside a cell is dropped; the link text survives.
  expect_identical(tab[[1]], c("Australia", "Iceland"))
  expect_identical(tab[[2]], c("AUS", "ISL"))
  # A row with fewer cells than the header is padded with missing values.
  expect_identical(tab[[3]], c("1921 - 2021", NA_character_))

  # An empty cell is an empty string, not a missing value.
  empty <- parse_html_table(
    html = "<table><tr><th>a</th><th>b</th></tr><tr><td>x</td><td></td></tr></table>"
  )
  expect_identical(empty[[2]], "")
})

test_that("C3: parse_html_table resolves entities and collapses whitespace", {
  html <- paste0(
    "<table><tr><th>Country</th><th>Range</th></tr>",
    "<tr><td>Trinidad &amp; Tobago</td><td>1921 &#8211; 2021</td></tr></table>"
  )

  tab <- parse_html_table(html = html)

  expect_identical(tab[[1]], "Trinidad & Tobago")
  expect_identical(tab[[2]], "1921 \u2013 2021")
})

test_that("C3: parse_html_table returns NULL when there is no table", {
  expect_null(parse_html_table(html = "<html><body>no table here</body></html>"))
  expect_null(parse_html_table(html = NULL))
})
