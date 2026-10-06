# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-06 20:29:39
# --------------------------------------------
# Engine tests for MortalityLaw() and its S3 surface. Absorbs the seven old test
# files plus Contracts 6, 8, 9 and the loss-function sweep.
# --------------------------------------------

# ---- The catalogue: one fit per law ------------------------------------------
# Ages per law TYPE, read off the legend of availableLaws().
law_catalogue <- availableLaws()
law_codes     <- as.character(law_catalogue$table$CODE)
type_ages <- list(`1` = 0:15, `2` = 16:30, `3` = 30:75,
                  `4` = 30:100, `5` = 76:100, `6` = 0:100)

# A law may warn on an extreme optimiser probe; pinned in test_contracts.R.
law_fits <- lapply(setNames(law_codes, law_codes), function(code) {
  row  <- law_catalogue$table[law_catalogue$table$CODE == code, ]
  ages <- type_ages[[as.character(row$TYPE)]]
  tryCatch(suppressWarnings(MortalityLaw(x = ages, mx = mx_1950(ages),
                                         law = code, opt.method = "LF2")),
           error = function(e) e)
})

for (code in law_codes) {
  test_that(paste("MortalityLaw fit:", code), {
    fit <- law_fits[[code]]
    # a failed fit is reported against the law named in the title
    if (inherits(fit, "error")) stop(conditionMessage(fit))
    expect_s3_class(fit, "MortalityLaw")
    expect_true(all(is.finite(coef(fit))))
    # a law may be undefined at some ages (the Weibull at birth): allowed
    expect_true(all(is.na(fitted(fit)) | fitted(fit) >= 0))
  })
}

test_that("print, summary, predict and plot work on a fitted law", {
  # once, on one fit: the old loops ran this battery on every single object
  fit <- law_fits[["thiele"]]
  expect_output(print(fit))
  expect_output(print(summary(fit)))
  pred <- predict(fit, x = fit$input$x)
  expect_length(pred, length(fit$input$x))
  expect_true(all(is.na(pred) | pred >= 0))
  with_pdf_device(expect_no_error(plot(fit)))
})

# ---- fit.this.x: fit a subset, evaluate over the whole range ------------------
# Four laws spanning the parameter counts the optimiser handles (2, 7, 8, 9)
# and the kostaki boundary hack: fit.this.x = x_sub must give the same
# estimates as fitting the subset directly, over the same observed points.
adult_ages  <- c(0, 1, seq(5, 75, by = 5))
subset_laws <- list(gompertz = seq(40, 75, by = 5), thiele = adult_ages,
                    HP = adult_ages, kostaki = adult_ages)

for (code in names(subset_laws)) {
  test_that(paste("fit.this.x gives the direct subset fit:", code), {
    x_sub   <- subset_laws[[code]]
    fit_all <- suppressWarnings(MortalityLaw(x = grid_ab_100, mx = mx_ab_100,
                                             law = code, fit.this.x = x_sub))
    fit_sub <- suppressWarnings(MortalityLaw(x = x_sub,
                                             mx = mx_ab_100[grid_ab_100 %in% x_sub],
                                             law = code, fit.this.x = x_sub))
    expect_identical(fitted(fit_all)[paste(x_sub)], fitted(fit_sub))
    expect_identical(coef(fit_all), coef(fit_sub))
    expect_identical(fitted(fit_all), predict(fit_sub, x = grid_ab_100))
    with_pdf_device(expect_no_error(plot(fit_all)))
    with_pdf_device(expect_no_error(plot(fit_sub)))
  })
}

# ---- The matrix branch, a custom law, and the LF2 hint ------------------------

two_col_ages <- 76:100
two_col_mx   <- ahmd$mx[paste(two_col_ages), c("1950", "2010")]
fit_2col <- suppressWarnings(MortalityLaw(x = two_col_ages, mx = two_col_mx,
                                          law = "kannisto_makeham"))

test_that("a two-column fit returns one curve per column", {
  expect_true(is.matrix(coef(fit_2col)))
  expect_equal(rownames(coef(fit_2col)), c("1950", "2010"))
  expect_equal(dim(fitted(fit_2col)), c(length(two_col_ages), 2))
  expect_true(is.matrix(predict(fit_2col, x = two_col_ages)))
  # Contract 6: df.residual keeps the matrix shape of df (3 kannisto params)
  expect_equal(unname(df.residual(fit_2col)), rep(length(two_col_ages) - 3, 2))
  # Contract 9: several curves have no single series to draw
  with_pdf_device(
    expect_error(plot(fit_2col), fixed = TRUE,
                 regexp = "Plot function not available for multiple mortality curves")
    )
  # show = TRUE drives the multi-curve progress bar (main.R:373-381)
  invisible(capture.output(
    show_2col <- MortalityLaw(x = two_col_ages, mx = two_col_mx,
                              law = "kannisto_makeham", show = TRUE)))
  expect_s3_class(show_2col, "MortalityLaw")
  expect_identical(coef(show_2col), coef(fit_2col))
})

test_that("MortalityLaw fits a custom law", {
  # a reparameterised Gompertz on the modal age at death, from custom.law
  my_gompertz <- function(x, par = c(b = 0.13, m = 45)) {
    hx <- with(as.list(par), b * exp(b * (x - m)))
    return(as.list(environment()))
  }
  fit <- MortalityLaw(x = 45:75, Dx = Dx_1950(45:75), Ex = Ex_1950(45:75),
                      custom.law = my_gompertz)
  expect_s3_class(fit, "MortalityLaw")
  expect_true(all(is.finite(coef(fit))))
  # print, summary and plot each take a separate branch for a custom law
  expect_output(print(fit), "Custom Mortality Law")
  expect_output(print(summary(fit)), "Custom Mortality Law")
  with_pdf_device(expect_no_error(plot(fit)))
})

# Fitted here and kept: the S3 tests below reuse it (no second HP fit).
hp_fit <- NULL

test_that("poissonL on HP reports the LF2 hint, and predict rejects negative ages", {
  # high-parameter laws are more reliable on LF2; a hazard has nothing below 0
  expect_message(
    hp_fit <<- MortalityLaw(x = 0:100, mx = mx_1950(0:100), law = "HP",
                            opt.method = "poissonL"))
  expect_s3_class(hp_fit, "MortalityLaw")
  expect_error(predict(hp_fit, x = -1:100),
               regexp = "'x' must be greater or equal to zero")
})

# ---- Input validation ---------------------------------------------------------

test_that("MortalityLaw validates its input", {
  x  <- 0:100
  mx <- mx_1950(x)
  Dx <- Dx_1950(x)
  Ex <- Ex_1950(x)
  # no law, unknown law, unknown loss function, non-logical 'show', an age
  # vector longer than the data, mismatched Dx/Ex lengths, an invalid subset
  expect_error(MortalityLaw(x, mx = mx))
  expect_error(MortalityLaw(x, mx = mx, law = "law_not_available"))
  expect_error(MortalityLaw(x, mx = mx, law = "HP", opt.method = "LF_nope"))
  expect_error(MortalityLaw(x, mx = mx, law = "HP", show = "TRUEx"))
  expect_error(MortalityLaw(0:1000, mx = mx, law = "HP"))
  expect_error(MortalityLaw(x, Dx = Dx, Ex = Ex[-1], law = "HP"))
  expect_error(MortalityLaw(x, Dx = Dx[-1], Ex = Ex, law = "HP"))

  # one age cannot identify the model; the subset must lie inside x (127, 132)
  expect_error(MortalityLaw(x, Dx = Dx, Ex = Ex, law = "makeham", fit.this.x = 48),
               regexp = "More observations needed in order to start the fitting")
  expect_error(MortalityLaw(x, Dx = Dx, Ex = Ex, law = "makeham",
                            fit.this.x = 101:105),
               regexp = "'fit.this.x' should be a subset of 'x'")
})

# ---- LawTable: parameters to a life table -------------------------------------

hp_par <- c(0.00223, 0.01461, 0.12292, 0.00091, 2.75201, 29.01877, 0.00002, 1.11411)
hp_qx  <- HP(x = 0:110, par = hp_par)$hx

test_that("LawTable reproduces the direct law evaluation and LifeTable", {
  hp_lt <- quiet(LawTable(x = 0:110, par = hp_par, law = "HP")$lt)
  n  <- length(hp_qx)
  # q[n] = 1 by convention, so only the ages below it are comparable
  expect_equal(hp_qx[-n], hp_lt$qx[-n], tolerance = 1e-12)
  expect_identical(hp_lt$qx[n], 1)
  expect_equal(hp_lt, quiet(LifeTable(x = 0:110, qx = hp_qx)$lt))
  hp_lt3 <- quiet(LawTable(x = 3:110, par = hp_par, law = "HP")$lt)
  expect_equal(hp_lt$ex[hp_lt$x == 3], hp_lt3$ex[hp_lt3$x == 3])
  expect_message(LifeTable(x = 0:110, qx = hp_qx),
                 regexp = "'qx' is not closed at the last age")
  est_lt <- quiet(LawTable(x = two_col_ages, par = coef(fit_2col),
                                      law = "kannisto_makeham"))
  expect_s3_class(est_lt, "LifeTable")
  expect_equal(nrow(est_lt$lt), 2 * length(two_col_ages))
  # makeham rescales internally; ages and per-row stacking survive (Contract 8)
  vec_lt <- quiet(LawTable(x = 45:100, par = c(A = .002, B = .13, C = .001),
                                      law = "makeham"))
  expect_equal(vec_lt$lt$x, 45:100)
  par2 <- matrix(c(0.00717, 0.07789, 0.00363,
                   0.01018, 0.07229, 0.00001),
                 nrow = 2, byrow = TRUE,
                 dimnames = list(c("m1", "m2"), c("A", "B", "C")))
  mat_lt <- quiet(LawTable(x = 45:100, par = par2, law = "makeham"))
  expect_s3_class(mat_lt, "LifeTable")
  expect_equal(nrow(mat_lt$lt), 2 * length(45:100))
  expect_equal(mat_lt$lt$x, rep(45:100, 2))
})

# ---- S3 methods ---------------------------------------------------------------
# A five-curve likelihood fit drives the head2/tail2 truncation below.
s3_x  <- 45:75
s3_mx <- cbind(as.matrix(ahmd$mx[paste(s3_x), c("1850", "1900", "1950", "2010")]),
               obs = Dx_1950(s3_x) / Ex_1950(s3_x))
fit_multi <- suppressWarnings(MortalityLaw(x = s3_x, mx = s3_mx,
                                           law = "makeham",
                                           opt.method = "poissonL"))

test_that("summary truncates a wide coefficient matrix to head 2 + tail 2", {
  # Above four rows summary keeps the first and last two, middle "..."
  coefs <- coef(fit_multi)
  s     <- summary(fit_multi)
  expect_true(is.matrix(coefs) && !s$L2)
  expect_equal(nrow(s$param), 5)           # 2 head + ellipsis + 2 tail
  expect_equal(ncol(s$param), ncol(coefs))
  expect_equal(rownames(s$param)[3], "...")
  expect_equal(unname(unlist(s$param[3, ])), rep("...", ncol(coefs)))
  expect_equal(rownames(s$param)[c(1, 2, 4, 5)],
               rownames(coefs)[c(1, 2, nrow(coefs) - 1, nrow(coefs))])
  # the goodness-of-fit table is truncated by the same contract
  expect_equal(nrow(s$gof), 5)
  expect_equal(unname(unlist(s$gof[3, ])), rep("...", ncol(s$gof)))
  expect_equal(as.numeric(unlist(s$gof[1, ])),
               unname(round(fit_multi$goodness.of.fit[1, ], s$digits)))
  # thiele keeps seven coefficients in a vector: no truncation (nrow <= 4)
  ch <- coef(law_fits[["thiele"]])
  s  <- summary(law_fits[["thiele"]])
  expect_false(is.matrix(ch))
  expect_gt(length(ch), 4)
  expect_true(s$L2)
  expect_equal(sort(names(s$param)), sort(names(ch)))
  expect_true(all(is.finite(s$param)))
})

test_that("print.summary, logLik and predict follow the fit shape", {
  # the gof block is guarded by L3: a loss fit has no likelihood, so no block
  expect_output(print(summary(hp_fit)), "Goodness of fit")
  out <- capture.output(print(summary(law_fits[["makeham"]])))
  expect_false(any(grepl("Goodness of fit", out)))
  ll <- logLik(hp_fit)
  expect_s3_class(ll, "logLik")
  expect_equal(attr(ll, "df"), length(coef(hp_fit)))
  expect_equal(attr(ll, "nobs"), length(hp_fit$input$x))
  expect_equal(as.numeric(ll), unname(hp_fit$goodness.of.fit["logLik"]))
  # many fits: no single df/nobs pair, so one named log-likelihood per curve
  llm <- logLik(fit_multi)
  expect_false(inherits(llm, "logLik"))
  expect_equal(names(llm), rownames(coef(fit_multi)))
  expect_equal(unname(llm), unname(fit_multi$goodness.of.fit[, "logLik"]))
  # Contract 6: a single age is named and equals the vector prediction
  p90 <- predict(law_fits[["thiele"]], x = 90)
  expect_type(p90, "double")
  expect_length(p90, 1)
  expect_identical(names(p90), "90")
  expect_equal(p90, predict(law_fits[["thiele"]], x = 85:90)["90"])
  # the multi-curve summary takes the L2 = FALSE branch (MortalityLaw_S3.R:134)
  expect_output(print(summary(fit_multi)), "Average dispersion")
})

test_that("summary reports the fit window, the method and the fit measures", {
  out <- capture.output(print(summary(law_fits[["makeham"]])))
  expect_true(any(grepl("fitted on", out)))
  expect_true(any(grepl("^  method ", out)))
  expect_true(any(grepl("optimiser converged", out)))
  expect_true(any(grepl("R-squared", out)))
  expect_true(any(grepl("RMSE", out)))
  # residuals on both scales, in one table
  expect_true(any(grepl("^raw", out)))
  expect_true(any(grepl("^deviance ", out)))
  # the multi-curve summary carries the per-curve fit table and curve count
  outm <- capture.output(print(summary(fit_multi)))
  expect_true(any(grepl("curves", outm)))
  expect_true(any(grepl("R.squared", outm)))
  # the summary object keeps the new fields for programmatic use
  s <- summary(law_fits[["makeham"]])
  expect_true(all(c("deviance", "rq", "method", "optim", "dres") %in% names(s)))
  expect_true(all(is.finite(s$rq)))
})

test_that("plot draws the fitted curve for both qx and mx input", {
  # plot() reads the observed series from qx if entered as qx, else from mx
  qx <- quiet(convertFx(45:75, data = mx_1950(45:75),
                                   from = "mx", to = "qx"))
  fit_qx <- suppressWarnings(MortalityLaw(x = 45:75, qx = qx, law = "makeham"))
  fit_mx <- law_fits[["makeham"]]
  expect_true(!is.null(fit_qx$input$qx) && is.null(fit_qx$input$mx))
  expect_true(!is.null(fit_mx$input$mx) && is.null(fit_mx$input$qx))
  with_pdf_device(expect_no_error(plot(fit_qx)))
  with_pdf_device(expect_no_error(plot(fit_mx)))
})

# ---- The verbose flag, the kostaki boundary, the loss functions ----------------

test_that("show = TRUE is side output; kostaki's E2 sits on the E1/50 boundary", {
  # 'show' is pure side output: the bar must not touch the optimisation
  quiet <- law_fits[["makeham"]]
  invisible(capture.output(
    verbose <- MortalityLaw(x = 30:75, mx = mx_1950(30:75), law = "makeham",
                            show = TRUE)
    ))
  expect_s3_class(verbose, "MortalityLaw")
  expect_identical(coef(verbose), coef(quiet))
  expect_identical(fitted(verbose), fitted(quiet))

  # kostaki() replaces E2 with E1/50 whenever E1 >= 50*E2, which makes the
  # objective flat there: the raw optimiser returns E2 well below the boundary,
  # so choose_optim reports C[6] = C[5]/50, i.e. E2 = E1/50.
  fit <- suppressWarnings(MortalityLaw(x = 0:100, Dx = Dx_1950(0:100),
                                       Ex = Ex_1950(0:100), law = "kostaki"))
  E1 <- unname(coef(fit)["E1"])
  E2 <- unname(coef(fit)["E2"])
  expect_true(E1 >= 50 * E2)
  expect_identical(E2, E1 / 50)
})

test_that("every available loss function executes, and both catalogues print", {
  # One makeham fit per objective of availableLF(): LF6 reports false
  # convergence here, the likelihoods alone yield a finite AIC, and the six
  # loss functions leave logLik, AIC and BIC at NaN.
  ml <- function(m) {
    MortalityLaw(x = 45:75, mx = mx_1950(45:75), law = "makeham", opt.method = m)
  }
  for (m in availableLF()$table$CODE) {
    if (m == "LF6") {
      expect_warning(fit <- ml(m), regexp = "did not converge")
    } else {
      fit <- ml(m)
    }
    expect_s3_class(fit, "MortalityLaw")
    expect_true(all(is.finite(coef(fit))))
    if (m %in% c("poissonL", "binomialL")) {
      expect_false(is.nan(AIC(fit)))
    } else {
      expect_true(all(is.nan(fit$goodness.of.fit)))
    }
  }
  # the catalogues themselves, and their two print methods
  expect_s3_class(law_catalogue, "availableLaws")
  expect_false(is.null(law_catalogue$table))
  expect_false(is.null(law_catalogue$legend))
  expect_output(print(law_catalogue))
  al <- availableLaws(law = "rogersplanck")
  expect_s3_class(al, "availableLaws")
  expect_identical(unique(al$table$CODE), "rogersplanck")
  expect_output(print(al))
  expect_error(availableLaws(law = "notavailable"), regexp = "not available")
  af <- availableLF()
  expect_s3_class(af, "availableLF")
  expect_false(is.null(af$table))
  expect_false(is.null(af$legend))
  expect_output(print(af))
})

# ---- The input checks ---------------------------------------------------------
# The guards behind MortalityLaw(): one malformed input per case, the message a
# user sees and the R/ line that raises it, all before the optimiser is reached.

x6  <- 0:5
mx6 <- c(0.005, 0.001, 0.0008, 0.0009, 0.0011, 0.0014)

# A case is the expected message plus the arguments that must raise it. It uses
# modifyList, not list(x = x6, ...): the x cases override x, and a duplicate name
# makes do.call fail with an argument-matching error, not the message under test.
case <- function(msg, ...) {
  list(args = utils::modifyList(x = list(x = x6, law = "gompertz"),
                                val = list(...)), msg = msg)
}

bad_input <- list(
  # check_values() names the offending vector of the single-curve fit (17)
  case("'Ex' must be a numeric vector", Dx = rep(100, 6), Ex = rep("1000", 6)),
  # a missing or an infinite rate carries no information about the fit (21)
  case("'mx' contains missing or infinite values", mx = replace(mx6, 3, NA)),
  case("'mx' contains missing or infinite values", mx = replace(mx6, 3, Inf)),
  # a hazard cannot be negative (31)
  case("'mx' must not contain negative values", mx = replace(mx6, 2, -0.001)),
  # the qx path has its own length guard, separate from the mx one (58)
  case("x and qx do not have the same length", qx = c(0.01, 0.02, 0.03)),
  # 'x' must be numeric, finite, non-negative and unique (94, 98, 102, 106)
  case("'x' must be a numeric vector", x = as.character(x6), mx = mx6),
  case("'x' contains missing or infinite values", x = replace(x6, 6, NA), mx = mx6),
  case("'x' must not contain negative values", x = replace(x6, 1, -1), mx = mx6),
  case("'x' must not contain duplicated ages", x = replace(x6, 3, 1), mx = mx6),
  # parS reaches bring_parameters() and check_parameters() (models.R:762, 777)
  case("'par' for law 'gompertz' must be a numeric vector", mx = mx6, parS = "start"),
  case("must have 2 elements \\(A, B\\); got 3", mx = mx6, parS = c(0.001, 0.1, 0.2))
)

test_that("MortalityLaw reports every input check, and the helpers their contract", {
  for (row in bad_input) {
    expect_error(do.call(MortalityLaw, row$args), regexp = row$msg)
  }
  # no public route reaches bring_parameters()' unknown-law guard (models.R:859)
  expect_error(bring_parameters(law = "not_a_law"),
               regexp = "Unknown mortality law 'not_a_law'")
  # head_tail() prints a wide coefficient matrix in summary.MortalityLaw()
  M   <- matrix(1:12, nrow = 6, dimnames = list(NULL, c("A", "B")))
  out <- head_tail(M, hlength = 2, tlength = 2)
  expect_s3_class(out, "data.frame")
  expect_equal(dim(out), c(5, 2))         # 2 head + 1 ellipsis + 2 tail rows
  expect_true(all(out[3, ] == "..."))     # the separator row
  expect_equal(unname(out[1:2, 1]), as.character(M[1:2, 1]))
  expect_equal(unname(out[4:5, 1]), as.character(M[5:6, 1]))
  # substr_right() has no caller in the package, so it is called directly
  expect_equal(substr_right("abcdef", 3), "def")
  expect_equal(substr_right(c("abc", "wxyz"), 2), c("bc", "yz"))
  expect_equal(substr_right("abc", 10), "abc")
  expect_equal(substr_right("", 2), "")
})

test_that("zero exposure is left out of the fit, not rejected", {
  # An age with no exposure carries no information about the hazard. vital, a
  # reverse dependency, fits data with zero population at the oldest ages, so
  # this must not stop the fit: the age is dropped and the fit says so.
  Dx <- rep(100, 6)
  Ex <- c(rep(1000, 3), 0, 1000, 1000)

  expect_message(
    M <- suppressWarnings(
      MortalityLaw(x = x6, Dx = Dx, Ex = Ex, law = "gompertz")
    ),
    regexp = "1 age\\(s\\) with zero exposure"
  )
  expect_s3_class(M, "MortalityLaw")
  expect_true(all(is.finite(coef(M))))
  # out of the fit, so out of the residual degrees of freedom: 5 ages - 2 par
  expect_equal(unname(M$df["df.residual"]), 3)

  # dropping every informative age leaves nothing to fit
  expect_error(
    suppressMessages(
      MortalityLaw(x = x6, Dx = Dx, Ex = c(rep(0, 5), 1000), law = "gompertz")
    ),
    regexp = "fewer than two ages carry exposure"
  )
})
