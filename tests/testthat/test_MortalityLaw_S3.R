# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-04 18:20:00
# --------------------------------------------

# S3 methods of the "MortalityLaw" class: the summary and its print method, the
# log-likelihood accessor, and the diagnostic plot. These tests exercise the
# paths the rest of the suite leaves untouched: the head2/tail2 truncation of a
# wide summary, the likelihood-only goodness-of-fit block, the multi-fit
# log-likelihood vector, and the qx-input branch of the plot.

x  <- 0:100
Dx <- ahmd$Dx[paste(x), "1950"]
Ex <- ahmd$Ex[paste(x), "1950"]

# ahmd ships four calendar years. The head2/tail2 truncation of
# summary.MortalityLaw applies to a coefficient *matrix* with more than four
# rows, i.e. to a fit over more than four curves, so a fifth genuine curve is
# added: the 2010 death/exposure ratio, the unsmoothed observed rate on the
# same age grid.
Mx <- cbind(
  as.matrix(ahmd$mx[paste(x), c("1850", "1900", "1950", "2010")]),
  obs2010 = ahmd$Dx[paste(x), "2010"] / ahmd$Ex[paste(x), "2010"]
  )

# A likelihood fit (reports the goodness of fit), a loss-method fit (does not),
# and a five-curve likelihood fit (coefficient and gof matrices wider than four
# rows). Fits may warn while the optimiser probes extreme parameter regions;
# those warnings are pinned elsewhere and are not the subject here.
fitL <- suppressWarnings(MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = "makeham",
                                      opt.method = "poissonL"))
fitR <- suppressWarnings(MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = "makeham",
                                      opt.method = "LF2"))
fitM <- suppressWarnings(MortalityLaw(x = x, mx = Mx, law = "makeham",
                                      opt.method = "poissonL"))

# qx input and mx input of the same single curve: the plot picks the observed
# series from qx in the first case and from mx in the second.
qx <- suppressWarnings(
  convertFx(x, data = ahmd$mx[paste(x), "1950"], from = "mx", to = "qx")
  )
fitQ  <- suppressWarnings(MortalityLaw(x = x, qx = qx, law = "wittstein",
                                       opt.method = "LF2"))
fitMx <- suppressWarnings(MortalityLaw(x = x, mx = ahmd$mx[paste(x), "1950"],
                                       law = "makeham", opt.method = "LF2"))


test_that("summary truncates a wide coefficient matrix to head 2 + tail 2", {
  # A multi-curve fit keeps one coefficient row per curve in a matrix. When the
  # matrix has more than four rows, summary.MortalityLaw keeps the first two and
  # the last two rows and replaces the middle by a single "..." row: the
  # head2/tail2 contract that keeps a wide table on one screen. The ellipsis row
  # is inserted by the code itself, so that is the contract asserted here.
  P <- coef(fitM)
  expect_true(is.matrix(P))
  expect_gt(nrow(P), 4)

  s <- summary(fitM)
  expect_false(s$L2)
  # 2 head rows + the ellipsis row + 2 tail rows
  expect_equal(nrow(s$param), 5)
  expect_equal(ncol(s$param), ncol(P))
  expect_equal(rownames(s$param)[3], "...")
  expect_equal(unname(unlist(s$param[3, ])), rep("...", ncol(P)))
  # the tail keeps the last two curves, not curves 3 and 4
  expect_equal(rownames(s$param)[c(1, 2, 4, 5)],
               rownames(P)[c(1, 2, nrow(P) - 1, nrow(P))])

  # the goodness-of-fit table is truncated by the same contract
  expect_equal(nrow(s$gof), 5)
  expect_equal(unname(unlist(s$gof[3, ])), rep("...", ncol(s$gof)))
  # the head row is the rounded raw gof of the first curve
  expect_equal(as.numeric(unlist(s$gof[1, ])),
               unname(round(fitM$goodness.of.fit[1, ], s$digits)))
})

test_that("a single fit of a seven-parameter law reports every coefficient", {
  # Thiele has seven parameters, so the fit is a genuinely wide model. A single
  # fit keeps its coefficients in a plain vector, not in a matrix, so the
  # head2/tail2 contract above (driven by nrow(coef) > 4) does not apply and all
  # seven coefficients are summarised.
  fitT <- suppressWarnings(MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = "thiele",
                                        opt.method = "LF2"))
  ch <- coef(fitT)
  expect_gt(length(ch), 4)
  expect_false(is.matrix(ch))

  s <- summary(fitT)
  expect_true(s$L2)
  expect_equal(length(s$param), length(ch))
  expect_equal(sort(names(s$param)), sort(names(ch)))
  expect_true(all(is.finite(s$param)))
})

test_that("print.summary reports the goodness of fit only for likelihood fits", {
  # The block is guarded by L3 (opt.method "poissonL"/"binomialL"). A fit that
  # minimised a loss function has no likelihood, so the header must be absent.
  expect_output(print(summary(fitL)), "Goodness of fit")
  out <- capture.output(print(summary(fitR)))
  expect_false(any(grepl("Goodness of fit", out)))
})

test_that("logLik returns a logLik object for one fit and a vector for many", {
  ll <- logLik(fitL)
  expect_s3_class(ll, "logLik")
  expect_equal(attr(ll, "df"), length(coef(fitL)))
  # nobs is the number of fitted observations: the parameters plus the residual
  # degrees of freedom reconstructed back to the ages the law was fitted on.
  expect_equal(attr(ll, "nobs"), length(x))
  expect_equal(as.numeric(ll), unname(fitL$goodness.of.fit["logLik"]))

  # A multi-curve fit returns one named log-likelihood per curve instead of a
  # logLik object: there is no single df/nobs pair to attach to.
  llm <- logLik(fitM)
  expect_false(inherits(llm, "logLik"))
  expect_equal(length(llm), nrow(coef(fitM)))
  expect_equal(names(llm), rownames(coef(fitM)))
  expect_equal(unname(llm), unname(fitM$goodness.of.fit[, "logLik"]))
})

test_that("plot draws the fitted curve for both qx and mx input", {
  # plot.MortalityLaw reads the observed series from qx when the fit was entered
  # as qx, otherwise from mx (or Dx/Ex). Draw to a temporary device so nothing
  # is left on screen; the pdf device accepts the layout() the function sets up.
  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_false(is.null(fitQ$input$qx))
  expect_null(fitQ$input$mx)
  expect_no_error(plot(fitQ))

  expect_false(is.null(fitMx$input$mx))
  expect_null(fitMx$input$qx)
  expect_no_error(plot(fitMx))
})
