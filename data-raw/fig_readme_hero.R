# --------------------------------------------------- #
# README hero figure for MortalityLaws
# Author: the docs wave, 2026-10-05
# License: MIT
# --------------------------------------------------- #
#
# Draws the two-panel hero image used in README.md: a multi-law fit to one
# observed curve, and the life table that the headline fit implies. Base
# graphics only, so the package gains no dependencies. Run from the repo root:
#
#   Rscript data-raw/fig_readme_hero.R
#
# Output: man/figures/README-hero.png (1800 x 800, same canvas as ungroup).

library(MortalityLaws)

# Same palette as the plot methods in R/graphics.R.
ml_series <- c("#000000", "#3C8C00", "#1F6FB2", "#B3541E")
ml_grid <- "#D8E0E4"
ml_rule <- "#B7C2C8"

# England & Wales females, 1950: the workhorse example of the package.
get_observed <- function() {
  ages <- 0:100
  mx <- ahmd$mx[as.character(ages), "1950"]

  out <- list(ages = ages, mx = mx)

  return(out)
}

# Fit four laws on the age ranges where each one is meant to work.
fit_panel_laws <- function(ages, mx) {
  fits <- list(
    hp   = MortalityLaw(x = ages, mx = mx, law = "HP", opt.method = "LF2"),
    sil  = MortalityLaw(x = ages, mx = mx, law = "siler", opt.method = "poissonL"),
    km   = MortalityLaw(x = ages[ages >= 40], mx = mx[ages >= 40],
                        law = "kannisto_makeham", opt.method = "poissonL"),
    gom  = MortalityLaw(x = ages[ages >= 40], mx = mx[ages >= 40],
                        law = "gompertz", opt.method = "poissonL")
  )

  return(fits)
}

draw_fit_panel <- function(ages, mx, fits) {
  shown <- c(
    mx, fits$hp$fitted.values, fits$sil$fitted.values,
    fits$km$fitted.values, fits$gom$fitted.values
  )
  shown <- shown[is.finite(shown) & shown > 0]

  plot(ages, mx,
    type = "n", log = "y", axes = FALSE,
    xlab = "Age, x", ylab = "m(x), log scale",
    ylim = range(shown)
  )
  grid(col = ml_grid, lty = 1)
  points(ages, mx, pch = 1, cex = 0.55, col = "grey55")
  lines(ages, fits$hp$fitted.values, col = ml_series[1], lwd = 2)
  lines(ages, fits$sil$fitted.values, col = ml_series[2], lwd = 2)
  lines(ages[ages >= 40], fits$km$fitted.values, col = ml_series[3], lwd = 2)
  lines(ages[ages >= 40], fits$gom$fitted.values, col = ml_series[4], lwd = 2)
  axis(1, col = ml_rule)
  axis(2, col = ml_rule)
  box(col = ml_rule)
  legend("topleft",
    bty = "n", lwd = 2, col = ml_series,
    legend = c("Heligman-Pollard", "Siler", "Kannisto-Makeham", "Gompertz")
  )
  title(main = "One observed curve, four parametric laws", adj = 0,
        font.main = 1, col.main = "grey20")
}

draw_lt_panel <- function(fit) {
  # The fitted q(x) does not reach 1 at the last age; LifeTable closes the
  # table and warns, which is exactly what the vignette explains. Quiet here.
  lt <- suppressWarnings(
    LawTable(x = 0:100, par = fit$coefficients, law = "HP")
  )$lt

  plot(lt$x, lt$lx,
    type = "n", axes = FALSE,
    xlab = "Age, x", ylab = "Survivors, l(x)", ylim = c(0, 1e5)
  )
  grid(col = ml_grid, lty = 1)
  lines(lt$x, lt$lx, col = ml_series[1], lwd = 2)
  axis(1, col = ml_rule)
  axis(2, col = ml_rule)
  par(new = TRUE)
  plot(lt$x, lt$ex,
    type = "l", lwd = 2, col = ml_series[2], axes = FALSE,
    xlab = "", ylab = "", ylim = range(lt$ex)
  )
  axis(4, col = ml_rule)
  mtext("Life expectancy, e(x)", side = 4, line = 2.2, las = 3,
        col = ml_series[2])
  box(col = ml_rule)
  legend("topright",
    bty = "n", lwd = 2, col = ml_series[1:2],
    legend = c("l(x), radix 100 000", "e(x), years")
  )
  title(main = "The life table the fit implies", adj = 0,
        font.main = 1, col.main = "grey20")
}

main <- function() {
  dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)

  obs <- get_observed()
  fits <- fit_panel_laws(ages = obs$ages, mx = obs$mx)

  png("man/figures/README-hero.png", width = 1800, height = 800, res = 150)
  par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.8, 3.4), cex.lab = 0.95,
      cex.axis = 0.9, las = 1)
  draw_fit_panel(ages = obs$ages, mx = obs$mx, fits = fits)
  draw_lt_panel(fit = fits$hp)
  dev.off()
}

main()
