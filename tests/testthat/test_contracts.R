# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-04
# --------------------------------------------
# Contract pins for the 2026-10 review wave; Contracts 3, 6, 8, 9 and 10 live elsewhere.
# --------------------------------------------

# ---- Contract 2: goodness of fit --------------------------------------------

test_that("Contract 2: poissonL logLik equals the objective at the optimum", {
  x  <- 45:75
  Dx <- Dx_1950(x)
  Ex <- Ex_1950(x)
  M  <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = "makeham", opt.method = "poissonL")
  mu <- as.numeric(fitted(M))
  # logLik at the optimum = sum(Dx*log(mu) - mu*Ex); AIC = 2k - 2*logLik, k = 3
  # makeham parameters. A loss fit reports no likelihood: logLik and AIC stay NaN.
  logLik_hand <- sum(Dx * log(mu) - mu * Ex)
  expect_equal(as.numeric(logLik(M)), logLik_hand, tolerance = 1e-6)
  expect_equal(as.numeric(AIC(M)), 2 * 3 - 2 * logLik_hand, tolerance = 1e-6)

  M2 <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = "makeham", opt.method = "LF2")
  expect_true(is.nan(AIC(M2)))
  expect_true(is.nan(logLik(M2)))
})

test_that("Contract 2: count deviance, residuals and dispersion match a Poisson GLM", {
  x  <- 45:95
  Dx <- Dx_1950(x)
  Ex <- Ex_1950(x)
  # gompertz is log-linear, so glm(Dx ~ x, offset(log Ex)) is an exact reference;
  # dispersion is the Pearson chi-square over the residual df, not the GLM's 1.
  M <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = "gompertz", opt.method = "poissonL")
  g <- glm(Dx ~ x, offset = log(Ex), family = poisson(link = "log"))
  res <- residuals(g, "pearson")
  expect_equal(deviance(M), deviance(g), tolerance = 1e-6)
  expect_equal(unname(M$pearson.residuals), unname(res), tolerance = 1e-4)
  expect_equal(unname(M$deviance.residuals), unname(residuals(g)), tolerance = 1e-4)
  expect_equal(dispersion(M), sum(res^2) / df.residual(g), tolerance = 1e-4)
  expect_equal(unname(df.residual(M)), length(x) - 2)
})

test_that("Contract 2: rate fits keep the squared log-residual deviance", {
  x  <- 45:95
  mx <- mx_1950(x)
  M  <- MortalityLaw(x = x, mx = mx, law = "gompertz", opt.method = "LF2")
  mu  <- as.numeric(fitted(M))
  sse <- sum((log(mx) - log(mu))^2)
  expect_equal(deviance(M), sse, tolerance = 1e-10)
  # For a rate fit the residuals are the log-residuals, the dispersion their mean square.
  expect_equal(unname(M$pearson.residuals), log(mx) - log(mu), tolerance = 1e-12)
  expect_equal(dispersion(M), sse / (length(x) - 2), tolerance = 1e-10)
})

test_that("Contract 2: count dispersion is 1 for a saturated Poisson fit", {
  x  <- 45:50
  Dx <- Dx_1950(x)
  Ex <- Ex_1950(x)
  # A Poisson fit with n parameters for n points: every Poisson residual vanishes.
  saturate <- function(x, par = c(a = 1)) list(hx = Dx / Ex, par = c(a = 1))
  M <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = "custom.law",
                    custom.law = saturate, opt.method = "poissonL")
  expect_lt(dispersion(M), 1e-6)
  expect_lt(deviance(M), 1e-6)
})

# ---- Contract 4: the published law formulas ---------------------------------
# A law pin: the law, its parameter vector and the published formula, quoted so the
# loop evaluates it on the row's ages and parameters and compares it with the named
# output; `extra` holds the row's nesting, limit, identification and warning pins.

law_pin <- function(law, par, expected = NULL, x = c(0, 20, 60),
                    extra = function(par, x) NULL) {

  list(law = law, par = par, x = x, expected = expected, extra = extra)
}

LAW_PINS <- list(
  # Perks (1932), as fixed by the review (contract 4): hx = (A + B*C^x) / (1 + D*C^x)
  law_pin(law = "perks", par = c(A = 0.002, B = 0.13, C = 1.1, D = 0.01),
          expected = list(hx = quote((A + B * C^x) / (1 + D * C^x)))),
  # Steffensen (1930), the Perks variant MortalityLaws shipped before the split
  law_pin(law = "steffensen", par = c(A = 0.002, B = 0.13, C = 1.1, D = 0.01),
          expected = list(hx = quote((A + B * C^x) / (B * C^-x + 1 + D * C^x))),
          extra    = function(par, x) {
            # steffensen == perks / (1 + B*C^-x/(1 + D*C^x)): dampened at young ages.
            damp <- 1 + par[["B"]] * par[["C"]]^-x / (1 + par[["D"]] * par[["C"]]^x)
            expect_equal(steffensen(x = x, par = par)$hx,
                         perks(x = x, par = par)$hx / damp, tolerance = 1e-12)
            expect_lt(steffensen(x = 0, par = par)$hx, perks(x = 0, par = par)$hx)
          }),
  # Makeham (1860), as fixed by the review (contract 4): Hx = (A/B)*(exp(B*x)-1) + C*x
  law_pin(law = "makeham", par = c(A = .0002, B = .13, C = .001),
          expected = list(Sx = quote(exp(-(A / B * (exp(B * x) - 1) + C * x))))),
  # Thiele (1871), no special case at x = 0, as fixed by the review (contract 4)
  law_pin(law = "thiele",
          par      = c(A = .02474, B = .3, C = .004, D = .5, E = 25, F_ = .0001, G = .13),
          expected = list(hx = quote(A * exp(-B * x) + C * exp(-0.5 * D * (x - E)^2) +
                                       F_ * exp(G * x)))),
  # Opperman (1870), at the shifted age x + 1, as fixed by the review (contract 4)
  law_pin(law = "opperman", par = c(A = .04, B = .0004, C = .001),
          expected = list(hx = quote(pmax(0, A / sqrt(x + 1) - B + C * sqrt(x + 1))))),
  # Strehler-Mildvan (1960), as fixed by the review (contract 4)
  law_pin(law = "strehler_mildvan", par = c(A = 1e-4, B = 0.1, V = 0.02),
          expected = list(hx = quote(A * exp(B * x) *
                                       exp(-(V / B) * (1 - exp(-B * x)))))),
  # Kannisto (1998), as fixed by the review (contract 4): hx and Sx = exp(-Hx)
  law_pin(law = "kannisto", par = c(A = 0.5, B = 0.13),
          expected = list(
            hx = quote(A * exp(B * x) / (1 + A * exp(B * x))),
            Sx = quote(exp(-(1 / B) * log((1 + A * exp(B * x)) / (1 + A))))
          )),
  # Thiele (1871, p. 326); the Siler (1979) immaturity term: hx = A * exp(-B*x)
  law_pin(law = "neggompertz", par = c(A = .02, B = .4),
          expected = list(hx = quote(A * exp(-B * x)))),
  # de Beer and Janssen (2016); Lomax (1954), as fixed by the review: hx = A / (x + C)
  law_pin(law = "pareto_2", par = c(A = .01, C = .001),
          expected = list(hx = quote(A / (x + C)))),
  # Scholey (2019), the flexibly-shifted power hazard, as fixed by the review
  law_pin(law = "scholey_shifted_power", par = c(A = .01, B = .7, C = .01),
          expected = list(hx = quote(A * (x + C)^(-B)))),
  # Scholey (2019), the exponentially-truncated shifted power, as fixed by the review
  law_pin(law = "scholey", par = c(A = .01, B = .7, C = .01, D = .1),
          expected = list(hx = quote(A * (x + C)^(-B) * exp(-D * x))),
          extra    = function(par, x) {
            # The family nests its children as limits: approach the boundary, not set.
            par_d <- c(A = .01, B = .7, C = .01, D = 1e-12)
            par_b <- c(A = .01, B = 1 + 1e-12, C = .001, D = 1e-12)
            sp    <- scholey_shifted_power(x = x, par = c(A = .01, B = .7, C = .01))$hx
            pt    <- pareto_2(x = x, par = c(A = .01, C = .001))$hx
            expect_equal(scholey(x = x, par = par_d)$hx, as.numeric(sp),
                         tolerance = 1e-10)                          # D -> 0
            expect_equal(scholey(x = x, par = par_b)$hx, as.numeric(pt),
                         tolerance = 1e-10)                          # B -> 1
            # Year-resolution infant data leave D at the boundary and the fit must warn.
            expect_warning(MortalityLaw(x = 0:15, mx = mx_1950(0:15), law = "scholey",
                                        opt.method = "LF2"),
                           regexp = "truncation parameter 'D' of 'scholey' fitted at the boundary")
            xd <- 0:365
            mu <- 2e-2 * (xd + 3e-4)^(-0.7) * exp(-7e-3 * xd)
            expect_no_warning(MortalityLaw(x = xd, mx = mu, law = "scholey",
                                           opt.method = "LF2"))
          }),
  # Forfar-McCutcheon-Wilkie (1988), member GM(1,3): hx = A0 + K*exp(B1*x - B2*x^2)
  law_pin(law = "makeham_logquad", par = c(A0 = .001, K = .001, B1 = .1, B2 = .001),
          x        = c(30, 60, 100),
          expected = list(hx = quote(A0 + K * exp(B1 * x - B2 * x^2))),
          extra    = function(par, x) {
            # B2 -> 0 gives Makeham, A0 -> 0 gives Gompertz; the boundary is approached.
            par_mk <- c(A0 = .001, K = .001, B1 = .1, B2 = 1e-16)
            par_gp <- c(A0 = 1e-16, K = .001, B1 = .1, B2 = 1e-16)
            mk     <- makeham(x = x, par = c(A = .001, B = .1, C = .001))$hx
            gp     <- gompertz(x = x, par = c(A = .001, B = .1))$hx
            expect_equal(makeham_logquad(x = x, par = par_mk)$hx, as.numeric(mk),
                         tolerance = 1e-9)
            expect_equal(makeham_logquad(x = x, par = par_gp)$hx, as.numeric(gp),
                         tolerance = 1e-9)
          }),
  # GM(0,3), the same law without the Makeham constant: hx = K*exp(B1*x - B2*x^2)
  law_pin(law = "gompertz_logquad", par = c(K = .001, B1 = .1, B2 = .001),
          x        = c(30, 60, 100),
          expected = list(hx = quote(K * exp(B1 * x - B2 * x^2))),
          extra    = function(par, x) {
            # As A0 -> 0 the GM(1,3) hazard collapses onto GM(0,3).
            par0 <- c(A0 = 1e-16, K = .001, B1 = .1, B2 = .001)
            expect_equal(makeham_logquad(x = x, par = par0)$hx,
                         as.numeric(gompertz_logquad(x = x, par = par)$hx),
                         tolerance = 1e-9)
          }),
  # De Moivre (1725): survivorship is linear, l_x = N - x, so hx = 1/(N - x)
  law_pin(law = "demoivre", par = c(N = 110),
          x        = c(0, 50, 90),
          expected = list(hx = quote(1 / (N - x))),
          extra    = function(par, x) {
            # Defined only below N, so the fit warns: it parks N just above the top age.
            expect_warning(MortalityLaw(x = 60:100, mx = mx_1950(60:100), law = "demoivre",
                                        opt.method = "LF2"),
                           regexp = "defined only below its limiting age")
          }),
  # Weibull (1939): age 0 is missing, not a placeholder (0 above shape 1, unbounded below)
  law_pin(law = "weibull", par = c(sigma = 2, M = 1),
          x        = c(0, 1, 10),
          expected = list(hx = quote({
            hx <- 1 / sigma * (x / M)^(M / sigma - 1)
            hx[x == 0] <- NA_real_
            hx
          })),
          extra    = function(par, x) {
            expect_true(is.na(weibull(x = x, par = par)$hx[1]))
            expect_true(all(is.finite(weibull(x = c(1, 10), par = par)$hx)))
            # The missing age carries no weight: fitting from 0 and from 1 agree.
            d0 <- deviance(suppressWarnings(MortalityLaw(x = 0:15, mx = mx_1950(0:15),
                                                         law = "weibull", opt.method = "LF2")))
            d1 <- deviance(suppressWarnings(MortalityLaw(x = 1:15, mx = mx_1950(1:15),
                                                         law = "weibull", opt.method = "LF2")))
            expect_equal(as.numeric(d0), as.numeric(d1), tolerance = 1e-6)
          }),
  # Heligman-Pollard (1980): qx = eta / (1 + eta) of the three-term hazard eta
  law_pin(law = "HP",
          par      = c(A = .0005, B = .004, C = .08, D = .001, E = 10, F_ = 17,
                       G = .00005, H = 1.1),
          expected = list(hx = quote({
            eta <- A^((x + B)^C) + D * exp(-E * (log(x / F_))^2) + G * H^x
            eta / (1 + eta)
          }))),
  # Carriere (1992) publishes no closed form: the pin is identification (F7)
  law_pin(law = "carriere1",
          par      = c(P1 = .003, sigma1 = 15, M1 = 2.7, P2 = .007, sigma2 = 6, M2 = 3,
                       sigma3 = 9.5, M3 = 88),
          x        = 10:80,
          extra    = function(par, x) {
            par2 <- par
            par2["sigma2"] <- 50
            expect_false(isTRUE(all.equal(carriere1(x = x, par = par)$hx,
                                          carriere1(x = x, par = par2)$hx)))
          })
)

for (pin in LAW_PINS) {
  test_that(paste("Contract 4:", pin$law, "matches the published formula"), {
    law_fn <- get(pin$law, mode = "function")
    out    <- law_fn(x = pin$x, par = pin$par)

    for (fn in names(pin$expected)) {
      want <- eval(pin$expected[[fn]], c(as.list(pin$par), list(x = pin$x)), baseenv())
      expect_equal(out[[fn]], as.numeric(want), tolerance = 1e-12)
    }
    # The law accepts the row's parameter names and canonicalises their order.
    bp <- bring_parameters(law = pin$law, par = pin$par)
    expect_identical(names(bp), names(pin$par))
    pin$extra(par = pin$par, x = pin$x)
  })
}

# ---- Contract 5: bring_parameters -------------------------------------------

test_that("Contract 5: named par is matched by name, in any order", {
  p1 <- c(A = .002, B = .13, C = .001)
  p2 <- c(C = .001, B = .13, A = .002)
  expect_identical(bring_parameters(law = "makeham", par = p1),
                   bring_parameters(law = "makeham", par = p2))
  expect_equal(unname(bring_parameters(law = "makeham", par = p1)), unname(p1))
})

test_that("Contract 5: bring_parameters rejects invalid par", {
  expect_error(bring_parameters(law = "makeham", par = c(A = .002, B = .13, Z = .001)))
  expect_error(bring_parameters(law = "makeham", par = c(A = .002, B = .13)))
  expect_error(bring_parameters(law = "makeham", par = c(A = -1, B = .13, C = .001)))
  expect_error(bring_parameters(law = "makeham", par = c(A = 0, B = .13, C = .001)))
})

test_that("Contract 5: unnamed par keeps the positional convention", {
  p  <- c(.002, .13, .001)
  bp <- bring_parameters(law = "makeham", par = p)
  expect_equal(unname(bp), p)
  expect_identical(names(bp), c("A", "B", "C"))
})

# ---- Contract 7: fitting engine ---------------------------------------------

test_that("Contract 7: fit.this.x is order-insensitive", {
  x  <- 45:75
  Dx <- Dx_1950(x)
  Ex <- Ex_1950(x)
  M1 <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = "makeham", fit.this.x = 50:70)
  M2 <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = "makeham", fit.this.x = rev(50:70))
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
  # nlminb branch: a strictly decreasing objective cannot converge.
  expect_warning(
    opt <- run_optimiser(foo = function(par) -par[1], start = c(a = 0), law = "makeham"),
    regexp = "did not converge \\(code 1\\)"
  )
  expect_equal(opt$convergence, 1)

  # optim branch (invweibull); Nelder-Mead also emits an unreliability note.
  ws <- capture_warnings(
    opt2 <- run_optimiser(foo = function(par) -par[1], start = c(a = 0), law = "invweibull")
  )
  expect_true(any(grepl("did not converge \\(code 1\\)", ws)))
  expect_equal(opt2$convergence, 1)
  # end-to-end: vandermaen on ages 30-100 reports singular convergence.
  x  <- 30:100
  mx <- mx_1950(x)
  expect_warning(M <- MortalityLaw(x = x, mx = mx, law = "vandermaen", opt.method = "LF2"),
                 regexp = "did not converge")
  expect_s3_class(M, "MortalityLaw")
})
