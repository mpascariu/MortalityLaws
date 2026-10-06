# --------------------------------------------
# The graphics surface: plot.MortalityLaw, plot.LifeTable, plot.convertFx.
# --------------------------------------------

# One fit, one multi-table life table, one vector and one matrix conversion;
# built once here, exercised below.
gx  <- 45:90
fit <- suppressWarnings(
  MortalityLaw(x = gx,
               Dx = ahmd$Dx[as.character(gx), "1950"],
               Ex = ahmd$Ex[as.character(gx), "1950"],
               law = "makeham",
               fit.this.x = 45:75)
  )

lt_multi  <- suppressWarnings(
  LifeTable(x = as.numeric(rownames(ahmd$mx)), mx = ahmd$mx[, c("1900", "1950")])
  )
lt_single <- quiet(LifeTable(x = 0:105, mx = mx_1950(0:105)))

fx_vec <- suppressWarnings(
  convertFx(x = 0:105, data = mx_1950(0:105), from = "mx", to = "ex")
  )
fx_mat <- suppressWarnings(
  convertFx(x = 0:105,
            data = as.matrix(ahmd$mx[paste0(0:105), c("1950", "2010")]),
            from = "mx", to = "ex")
  )

# ---- plot.MortalityLaw ------------------------------------------------------
test_that("plot.MortalityLaw draws the fit chart and the diagnostics", {
  with_pdf_device({
    expect_invisible(plot(fit, which = "fit"))
    expect_invisible(plot(fit, which = "diagnostics"))
    expect_invisible(plot(fit, which = "both"))
    expect_invisible(plot(fit))
    expect_no_error(plot(fit, which = "diagnostics", split = c(1, 4)))
    expect_no_error(plot(fit, which = "diagnostics", split = c(4, 1)))
    expect_no_error(plot(fit, which = "diagnostics", split = c(2, 2)))
    expect_no_error(plot(fit, which = "both", split = c(1, 4)))
  })
})

test_that("plot.MortalityLaw validates the split", {
  with_pdf_device({
    expect_error(plot(fit, which = "diagnostics", split = c(2, 3)),
                 regexp = "split")
    expect_error(plot(fit, which = "diagnostics", split = "row"),
                 regexp = "split")
    expect_error(plot(fit, which = "diagnostics", split = "sideways"),
                 regexp = "split")
  })
})

# ---- plot.LifeTable ---------------------------------------------------------
test_that("plot.LifeTable draws all panels and single panels", {
  with_pdf_device({
    expect_invisible(plot(lt_single))
    expect_invisible(plot(lt_multi))
    for (w in c("lx", "hazard", "dx", "ex")) {
      expect_no_error(plot(lt_single, which = w))
    }
    expect_no_error(plot(lt_multi, split = c(1, 4)))
    expect_no_error(plot(lt_multi, split = c(4, 1)))
    expect_no_error(plot(lt_multi, split = c(2, 2)))
  })
})

test_that("plot.LifeTable validates the split", {
  with_pdf_device({
    expect_error(plot(lt_multi, split = c(2, 3)), regexp = "split")
    expect_error(plot(lt_multi, split = "column"), regexp = "split")
    expect_error(plot(lt_single, which = "lx", split = c(2, 2)),
                 regexp = "split")
  })
})

# ---- plot.convertFx ---------------------------------------------------------
test_that("plot.convertFx draws the conversion, vector and matrix", {
  with_pdf_device({
    expect_invisible(plot(fx_vec))
    expect_invisible(plot(fx_mat))
    expect_no_error(plot(fx_vec, split = c(2, 1)))
    expect_no_error(plot(fx_vec, split = c(1, 2)))
  })
})

test_that("plot.convertFx validates the split", {
  with_pdf_device({
    expect_error(plot(fx_vec, split = c(1, 3)), regexp = "split")
    expect_error(plot(fx_vec, split = "grid"), regexp = "split")
  })
})

# ---- the classed convertFx return -------------------------------------------
test_that("convertFx returns a classed result that plots and round-trips", {
  qx <- suppressWarnings(
    convertFx(x = 0:105, data = mx_1950(0:105), from = "mx", to = "qx")
    )
  expect_s3_class(qx, "convertFx")
  expect_true(is.numeric(qx))
  expect_identical(attr(qx, "from"), "mx")
  expect_identical(attr(qx, "to"), "qx")
  expect_identical(attr(qx, "x"), 0:105)
  expect_equal(attr(qx, "input")$data, mx_1950(0:105))
  # subsetting returns the bare values
  expect_false(inherits(qx[1:10], "convertFx"))
  # a classed result feeds back into the converters and the table builders
  back <- suppressWarnings(
    convertFx(x = 0:105, data = qx, from = "qx", to = "mx")
    )
  expect_s3_class(back, "convertFx")
  expect_s3_class(quiet(LifeTable(x = 0:105, qx = qx)), "LifeTable")
  qx2 <- suppressWarnings(
    convertFx(x = 45:75, data = mx_1950(45:75), from = "mx", to = "qx")
    )
  expect_s3_class(suppressWarnings(MortalityLaw(x = 45:75, qx = qx2,
                                                law = "makeham")),
                  "MortalityLaw")
})

test_that("print.convertFx reports the conversion without attributes", {
  out <- capture.output(print(fx_vec))
  expect_match(out[1], "convertFx result: mx -> ex")
  expect_false(any(grepl("attr", out)))
  outm <- capture.output(print(fx_mat))
  expect_match(outm[1], "2 columns")
})
