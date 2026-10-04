# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-04
# --------------------------------------------

# Setup: AHMD Swedish mortality, single-year ages 0-100, year 1950 -- the
# canonical example of the MortalityLaw documentation. Death counts with
# exposures give the count problem case (C1_DxEx), which is what both fits
# below use.
x  <- 0:100
Dx <- ahmd$Dx[paste(x), "1950"]
Ex <- ahmd$Ex[paste(x), "1950"]


test_that("show = TRUE draws the progress bar without changing the fit", {
  # 'show' is pure side output: the progress bar must not touch the
  # optimisation, so the verbose and the quiet fit have to agree to the last
  # bit. The bar writes to the console, so the verbose call is wrapped to
  # keep the test log clean.
  fit_quiet <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = "makeham")

  fit_show <- NULL
  invisible(capture.output(
    fit_show <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = "makeham",
                             show = TRUE)
  ))

  expect_s3_class(fit_show, "MortalityLaw")
  expect_true(all(is.finite(coef(fit_show))))
  expect_identical(coef(fit_show), coef(fit_quiet))
  expect_identical(fitted(fit_show), fitted(fit_quiet))
})


test_that("choose_optim reports kostaki's E2 at the E1/50 boundary", {
  # kostaki() replaces E2 with E1/50 whenever E1 >= 50*E2, which makes the
  # objective flat in E2 over that region: on this Swedish 1950 curve the raw
  # optimiser returns E2 well below the boundary, and every value in the flat
  # region is equally optimal. choose_optim's hack therefore reports the
  # boundary value C[6] = C[5]/50, i.e. E2 = E1/50, instead of the
  # directionless raw estimate. The invariant is checked on the reported
  # coefficients.
  fit <- suppressWarnings(MortalityLaw(x = x, Dx = Dx, Ex = Ex,
                                       law = "kostaki"))
  E1 <- unname(coef(fit)["E1"])
  E2 <- unname(coef(fit)["E2"])

  expect_true(E1 >= 50 * E2)
  expect_identical(E2, E1 / 50)
})
