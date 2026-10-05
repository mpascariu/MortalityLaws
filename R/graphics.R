# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-05
# --------------------------------------------

# ---- Design system ---------------------------------------------------------
# House colours shared by all plot methods: black text and primary series, a
# saturated green for the model/converted series, two further series hues,
# light panel rules and two soft tints (fit range, residual band).
ML_TEXT     <- "#000000"
ML_GRID     <- "#D8E0E4"
ML_RULE     <- "#B7C2C8"
ML_TINT_FIT <- "#EDF4DA"
ML_TINT_BND <- "#E6ECEE"
ML_SERIES   <- c("#000000", "#3C8C00", "#1F6FB2", "#B3541E")


#' Series colours for n curves
#' @return A character vector of n colours.
#' @noRd
ml_series_cols <- function(n) {
  if (n <= length(ML_SERIES)) ML_SERIES[seq_len(n)]
  else grDevices::colorRampPalette(ML_SERIES)(n)
}


#' Log10 axis label expressions
#' @param k Integer decade exponents.
#' @return An expression vector plotting 10^k.
#' @noRd
ml_log_expr <- function(k) {
  as.expression(lapply(k, function(i) bquote(10^.(i))))
}


#' Log-scale y limits padded to full decades
#' @param v Values to be placed on a log10 axis.
#' @return A length-2 numeric vector spanning at least one decade.
#' @noRd
ml_log_ylim <- function(v) {
  lg <- log10(v[is.finite(v) & v > 0])
  if (!length(lg)) {
    stop("No positive finite values to place on a log scale.", call. = FALSE)
  }
  lo <- floor(min(lg))
  hi <- ceiling(max(lg))
  c(lo, max(lo + 1, hi))
}


#' Linear y limits padded by a fraction of the range
#' @param v Values to be plotted.
#' @param pad Padding as a fraction of the range.
#' @param zero If TRUE the lower limit is fixed at zero.
#' @return A length-2 numeric vector.
#' @noRd
ml_lin_ylim <- function(v, pad = 0.06, zero = FALSE) {
  r <- range(v[is.finite(v)])
  d <- diff(r)
  if (!is.finite(d) || d == 0) d <- max(abs(r), 1)
  lo <- if (zero) 0 else r[1] - pad * d
  c(lo, r[2] + pad * d)
}


#' Empty panel with grid, box and axes
#'
#' Draws the plot region of one panel in the house style. The grid and the
#' shading go down before the data, the rules and the axis labels after.
#' @param xlim,ylim Panel ranges.
#' @param xlab,ylab Axis labels.
#' @param log_y Draw the y axis on a log10 scale with decade and minor ticks.
#' @param xat,yat Tick positions. Decades/pretty breaks when NULL.
#' @param shade Optional x range to shade behind the data.
#' @return NULL; called for the panel it draws.
#' @noRd
ml_frame <- function(xlim, ylim, xlab = "", ylab = "", log_y = FALSE,
                     xat = NULL, yat = NULL, shade = NULL) {
  plot(NA, xlim = xlim, ylim = ylim, axes = FALSE, xlab = "", ylab = "",
       xaxs = "i", yaxs = "i")

  if (!is.null(shade)) {
    rect(xleft = shade[1], xright = shade[2], ybottom = ylim[1],
         ytop = ylim[2], col = ML_TINT_FIT, border = FALSE)
  }

  if (log_y) {
    k <- seq(ceiling(ylim[1]), floor(ylim[2]))
    k <- k[k >= ylim[1] & k <= ylim[2]]
    if (!length(k)) k <- ylim
    minor <- unlist(lapply(k, function(i) log10(2:9) + i))
    minor <- minor[minor > ylim[1] & minor < ylim[2]]
    abline(h = k, col = ML_GRID)
    if (length(minor)) {
      axis(2, at = minor, labels = FALSE, tcl = -0.1,
           col = ML_RULE, col.ticks = ML_RULE)
    }
    if (is.null(yat)) yat <- k
    axis(2, at = yat, labels = ml_log_expr(yat), las = 1, tcl = -0.22,
         col = ML_RULE, col.ticks = ML_RULE, cex.axis = 0.85,
         col.axis = ML_TEXT)
  } else {
    if (is.null(yat)) yat <- pretty(ylim, n = 5)
    yat <- yat[yat >= ylim[1] & yat <= ylim[2]]
    abline(h = yat, col = ML_GRID)
    axis(2, at = yat, las = 1, tcl = -0.22, col = ML_RULE,
         col.ticks = ML_RULE, cex.axis = 0.85, col.axis = ML_TEXT)
  }

  if (is.null(xat)) xat <- pretty(xlim, n = 6)
  xat <- xat[xat >= xlim[1] & xat <= xlim[2]]
  abline(v = xat, col = ML_GRID)
  axis(1, at = xat, tcl = -0.22, col = ML_RULE, col.ticks = ML_RULE,
       cex.axis = 0.85, col.axis = ML_TEXT)
  box(col = ML_RULE)
  title(xlab = xlab, ylab = ylab, col.lab = ML_TEXT, cex.lab = 0.95,
        mgp = c(2.6, 0.6, 0))
  invisible(NULL)
}


#' Left-aligned title block over the whole figure
#' @param main,sub Title and subtitle lines.
#' @return NULL; called for the text it draws.
#' @noRd
ml_title <- function(main, sub = NULL) {
  mtext(main, side = 3, line = 1.6, outer = TRUE, adj = 0, cex = 1.25,
        font = 2, col = ML_TEXT)
  if (!is.null(sub)) {
    mtext(sub, side = 3, line = 0.25, outer = TRUE, adj = 0, cex = 0.85,
          col = ML_TEXT)
  }
  invisible(NULL)
}


#' Panel tag drawn inside the top margin of the current panel
#' @param tag Label such as "(a)  survivorship".
#' @return NULL; called for the text it draws.
#' @noRd
ml_tag <- function(tag) {
  mtext(tag, side = 3, line = 0.35, adj = 0, cex = 0.8, font = 2,
        col = ML_TEXT)
  invisible(NULL)
}


#' Emptiest corner of the panel for a legend
#'
#' Counts the data points falling in each of the four corner boxes (30 percent
#' of the panel range each) and returns the emptiest corner.
#' @param x,y Plot coordinates of the data shown in the panel.
#' @param xlim,ylim Panel ranges.
#' @return A legend position string understood by \code{\link{legend}}.
#' @noRd
ml_legend_pos <- function(x, y, xlim, ylim) {
  ok <- is.finite(x) & is.finite(y)
  x  <- x[ok]
  y  <- y[ok]
  dx <- diff(xlim)
  dy <- diff(ylim)
  corners <- c("topright", "bottomright", "bottomleft", "topleft")
  boxes <- list(
    topright     = c(xlim[2] - 0.3 * dx, xlim[2], ylim[2] - 0.3 * dy, ylim[2]),
    bottomright  = c(xlim[2] - 0.3 * dx, xlim[2], ylim[1], ylim[1] + 0.3 * dy),
    bottomleft   = c(xlim[1], xlim[1] + 0.3 * dx, ylim[1], ylim[1] + 0.3 * dy),
    topleft      = c(xlim[1], xlim[1] + 0.3 * dx, ylim[2] - 0.3 * dy, ylim[2])
  )
  hits <- vapply(boxes, function(b) {
    sum(x >= b[1] & x <= b[2] & y >= b[3] & y <= b[4])
  }, numeric(1))
  corners[which.min(hits)]
}


#' Legend in the house style
#' @param pos Position string from \code{ml_legend_pos}.
#' @param legend, col, lty, pch Passed to \code{\link{legend}}.
#' @return NULL; called for the legend it draws.
#' @noRd
ml_legend <- function(pos, legend, col, lty = NULL, pch = NULL) {
  if (is.null(lty)) lty <- rep(NA, length(legend))
  legend(pos, legend = legend, col = col, lty = lty, pch = pch, bty = "n",
         cex = 0.9, text.col = ML_TEXT, seg.len = 1.6,
         lwd = ifelse(is.na(lty), NA, 2.4))
  invisible(NULL)
}


#' Validate a panel split
#'
#' Normalises the \code{split} argument of the plot methods to a c(nrow, ncol)
#' matrix layout holding exactly one slot per panel. \code{NULL} takes the
#' default layout of the figure.
#' @param split NULL, or a length-2 integer c(nrow, ncol).
#' @param n Number of panels to place.
#' @param default The layout used when \code{split} is NULL.
#' @return A length-2 integer vector.
#' @noRd
check_split <- function(split, n, default) {
  if (is.null(split)) {
    return(as.integer(default))
  }
  ok <- is.numeric(split) && length(split) == 2 && all(is.finite(split)) &&
    all(split >= 1) && all(split == round(split)) && prod(split) == n
  if (!ok) {
    stop("'split' must be NULL or a length-2 integer c(nrow, ncol) with ",
         "nrow * ncol = ", n, " (the number of panels).", call. = FALSE)
  }
  as.integer(split)
}


# ---- plot.MortalityLaw -----------------------------------------------------

#' Plot a Fitted Mortality Law
#'
#' Draws the figures of a \code{"MortalityLaw"} fit in the house style: the
#' fit chart puts the observed and the fitted mortality on a log scale with
#' the fitted age range shaded and a goodness-of-fit subtitle; the residual
#' diagnostics give the deviance residuals against age and against the fitted
#' values (each with a +/- 2 standard deviation band and a lowess smooth),
#' a normal Q-Q plot and the residual distribution with a normal density
#' overlay.
#' @param x An object of class \code{"MortalityLaw"}.
#' @param which Which figure to draw: \code{"both"} (the default; one figure
#'   carrying the fit chart and the residual panels together), \code{"fit"}
#'   (the fit chart alone) or \code{"diagnostics"} (the four residual panels
#'   alone).
#' @param split How to arrange the four diagnostic panels when they are
#'   drawn: \code{NULL} (the default) draws them in a c(2, 2) grid; give a
#'   length-2 integer c(nrow, ncol) to split them yourself, such as
#'   \code{c(1, 4)} for one row or \code{c(4, 1)} for one column. Ignored
#'   when only the fit chart is drawn.
#' @param ... Further arguments; currently ignored.
#' @return The object \code{x}, invisibly. Called for the plots it draws.
#' @seealso \code{\link{MortalityLaw}}.
#' @author Marius D. Pascariu
#' @example inst/examples/plot.MortalityLaw.R
#' @export
plot.MortalityLaw <- function(x,
                              which = c("both", "fit", "diagnostics"),
                              split = NULL,
                              ...) {
  which <- match.arg(which)
  with(
    data = x$input,
    if (!any(detect_case(Dx = Dx, Ex = Ex, mx = mx, qx = qx)$iclass ==
             "numeric")) {
      stop("Plot function not available for multiple mortality curves",
           call. = FALSE)
      }
    )

  # Validate before touching any graphical parameter: an argument error must
  # leave the device exactly as it was found.
  if (which %in% c("both", "diagnostics")) {
    split <- check_split(split, 4, default = c(2L, 2L))
  }

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  if (which == "both") {
    ml_fit_and_diagnostics(x, split = split)
  } else if (which == "fit") {
    ml_fit(x)
  } else {
    ml_diagnostics(x, split = split)
  }
  return(invisible(x))
}


#' Law label of a fitted mortality law
#' @param x An object of class \code{"MortalityLaw"}.
#' @return The law name, suffixed with "law".
#' @noRd
ml_law_label <- function(x) {
  law <- x$input$law
  lawN <- if (law == "custom.law") {
    "Custom Mortality"
  } else {
    unlist(availableLaws(law)$table["NAME"])
  }
  paste(lawN, "law")
}


#' Fit chart panel: observed versus fitted mortality
#' @param x An object of class \code{"MortalityLaw"}.
#' @param tag Optional panel tag letter drawn in the panel's top margin.
#' @return The goodness-of-fit subtitle line, invisibly.
#' @noRd
ml_fit_panel <- function(x, tag = NULL) {
  age  <- x$input$x
  age2 <- x$input$fit.this.x
  lawN <- ml_law_label(x)

  y <- observed_values(x)
  lab <- if (!is.null(x$input$qx)) "Death probability  q(x)" else
    "Death rate  m(x)"

  fit_y <- x$fitted.values
  # The chart is on a log scale: values the model cannot place on it
  # (non-positive rates from out-of-range extrapolation) are omitted there.
  y[!is.finite(y) | y <= 0]     <- NA_real_
  fit_y[!is.finite(fit_y) | fit_y <= 0] <- NA_real_

  ylim <- ml_log_ylim(c(y, fit_y))
  ml_frame(range(age), ylim, xlab = "Age  x", ylab = lab, log_y = TRUE,
           shade = range(age2) + c(-0.5, 0.5))
  points(age, log10(y), pch = 16, cex = 1, col = ML_SERIES[1])
  lines(age, log10(fit_y), lwd = 2.4, col = ML_SERIES[2])

  # Goodness of fit on the observed scale over the fitted age range
  q    <- fit_quality(x)
  keep <- age %in% age2 & is.finite(y) & is.finite(fit_y)
  sub  <- sprintf(
    "n = %d  |  fit range %g-%g (shaded)  |  R-squared = %.4f, RMSE = %.4g",
    sum(keep), min(age2), max(age2), q["R.squared"], q["RMSE"]
    )

  pos <- ml_legend_pos(c(age, age), c(log10(y), log10(fit_y)), range(age), ylim)
  ml_legend(pos,
            legend = c("Observed", paste("Fitted", lawN)),
            col = ML_SERIES[1:2], lty = c(NA, 1), pch = c(16, NA))
  if (!is.null(tag)) {
    ml_tag(paste0("(", tag, ")  observed vs fitted"))
  }
  invisible(sub)
}


#' Fit chart figure: the fit panel with its title block
#' @param x An object of class \code{"MortalityLaw"}.
#' @return NULL; called for the figure it draws.
#' @noRd
ml_fit <- function(x) {
  # mfrow = c(1, 1) also releases a layout left by the combined figure.
  par(mfrow = c(1, 1), mar = c(4.4, 5.2, 1.2, 1.6), oma = c(0, 0, 3.2, 0),
      mgp = c(3.0, 0.75, 0))
  sub <- ml_fit_panel(x)
  ml_title(paste("Fitted model:", ml_law_label(x)), sub)
  invisible(NULL)
}


#' Residual statistics shared by the diagnostic panels
#' @param x An object of class \code{"MortalityLaw"}.
#' @return A list with the residuals and the scales they are drawn on.
#' @noRd
ml_diag_stats <- function(x) {
  d <- as.numeric(x$deviance.residuals)
  n <- length(d)
  if (n < 2) {
    stop("At least two residuals are needed for the diagnostic panels.",
         call. = FALSE)
  }
  list(d = d, n = n, sd_d = sd(d), age = x$input$x,
       fit_y = as.numeric(x$fitted.values), lawN = ml_law_label(x))
}


#' One residual diagnostic panel
#' @param S The residual statistics from \code{ml_diag_stats}.
#' @param p Which panel: \code{"age"}, \code{"fitted"}, \code{"qq"} or
#'   \code{"hist"}.
#' @param tag Optional panel tag letter drawn in the panel's top margin.
#' @return NULL; called for the panel it draws.
#' @noRd
ml_diag_panel <- function(S, p, tag = NULL) {
  d <- S$d
  n <- S$n
  sd_d <- S$sd_d
  age <- S$age
  fit_y <- S$fit_y
  ylim <- ml_lin_ylim(c(d, 2.2 * sd_d, -2.2 * sd_d))

  if (p == "age") {
    ml_frame(range(age), ylim, "Age  x", "Deviance residual")
    rect(age[1] - 1, -2 * sd_d, age[n] + 1, 2 * sd_d,
         col = ML_TINT_BND, border = FALSE)
    abline(h = 0, lty = 2, col = ML_RULE)
    abline(h = c(-2, 2) * sd_d, lty = 3, col = ML_RULE)
    points(age, d, pch = 16, cex = 1, col = ML_SERIES[1])
    lines(lowess(age, d, f = 0.75), lwd = 2.4, col = ML_SERIES[2])
    pos <- ml_legend_pos(age, d, range(age), ylim)
    ml_legend(pos, legend = c("Deviance residual", "Lowess smooth"),
              col = ML_SERIES[1:2], lty = c(NA, 1), pch = c(16, NA))
    lab <- "vs age"

  } else if (p == "fitted") {
    xlim <- ml_lin_ylim(fit_y)
    ml_frame(xlim, ylim, "Fitted values", "Deviance residual")
    rect(xlim[1], -2 * sd_d, xlim[2], 2 * sd_d, col = ML_TINT_BND,
         border = FALSE)
    abline(h = 0, lty = 2, col = ML_RULE)
    abline(h = c(-2, 2) * sd_d, lty = 3, col = ML_RULE)
    points(fit_y, d, pch = 16, cex = 1, col = ML_SERIES[1])
    lines(lowess(fit_y, d, f = 0.75), lwd = 2.4, col = ML_SERIES[2])
    lab <- "vs fitted"

  } else if (p == "qq") {
    qq <- qqnorm(d, plot.it = FALSE)
    ml_frame(ml_lin_ylim(qq$x), ml_lin_ylim(qq$y),
             "Theoretical quantiles", "Sample quantiles")
    abline(h = 0, v = 0, col = ML_GRID)
    qs <- quantile(d, c(0.25, 0.75))
    xs <- qnorm(c(0.25, 0.75))
    abline(qs[1] - diff(qs) / diff(xs) * xs[1], diff(qs) / diff(xs),
           col = ML_SERIES[2], lwd = 2.4)
    points(qq$x, qq$y, pch = 16, cex = 1, col = ML_SERIES[1])
    lab <- "normal QQ"

  } else {
    brks <- if (diff(range(d)) > 0) {
      "FD"
    } else {
      seq(d[1] - 0.5, d[1] + 0.5, length.out = 6)
    }
    h  <- hist(d, breaks = brks, plot = FALSE)
    bw <- mean(diff(h$breaks))
    xd <- seq(min(h$breaks), max(h$breaks), length.out = 200)
    yd <- dnorm(xd, 0, sd_d) * n * bw
    ml_frame(range(h$breaks), ml_lin_ylim(c(h$counts, yd), zero = TRUE),
             "Deviance residual", "Frequency")
    rect(h$breaks[-length(h$breaks)], 0, h$breaks[-1], h$counts,
         col = grDevices::adjustcolor(ML_SERIES[1], 0.4), border = "white",
         lwd = 0.8)
    lines(xd, yd, lwd = 2.4, col = ML_SERIES[2])
    lab <- "distribution"
  }

  if (!is.null(tag)) {
    ml_tag(paste0("(", tag, ")  ", lab))
  }
  invisible(NULL)
}


#' Diagnostics subtitle line
#' @param S The residual statistics from \code{ml_diag_stats}.
#' @return A single subtitle string.
#' @noRd
ml_diag_sub <- function(S) {
  sprintf("deviance residuals  |  n = %d  |  band: +/- 2 sd", S$n)
}


#' Residual diagnostics figure: four panels in the selected split
#' @param x An object of class \code{"MortalityLaw"}.
#' @param split Panel layout; see \code{plot.MortalityLaw}.
#' @return NULL; called for the figure it draws.
#' @noRd
ml_diagnostics <- function(x, split = NULL) {
  split <- check_split(split, 4, default = c(2L, 2L))
  S <- ml_diag_stats(x)
  par(mfrow = split, mar = c(4.2, 4.8, 3.2, 1.2), oma = c(0, 0, 3.2, 0),
      mgp = c(2.8, 0.7, 0))
  ps <- c("age", "fitted", "qq", "hist")
  for (i in seq_along(ps)) {
    ml_diag_panel(S, ps[i], tag = letters[i])
  }
  ml_title(paste("Residual diagnostics:", S$lawN), ml_diag_sub(S))
  invisible(NULL)
}


#' Combined figure: the fit chart on top, the residual panels split below
#' @param x An object of class \code{"MortalityLaw"}.
#' @param split Panel layout for the residual panels; see
#'   \code{plot.MortalityLaw}.
#' @return NULL; called for the figure it draws.
#' @noRd
ml_fit_and_diagnostics <- function(x, split = NULL) {
  split <- check_split(split, 4, default = c(2L, 2L))
  S <- ml_diag_stats(x)
  mat <- matrix(NA_integer_, nrow = 1L + split[1], ncol = split[2])
  mat[1, ] <- 1L
  mat[-1, ] <- seq_len(prod(split)) + 1L
  layout(mat, heights = c(1.4, rep(1, split[1])))
  # Small margins: the figure stacks 1 + split[1] panel rows on one device.
  # The layout is released on the success path only; a drawing error must
  # surface on its own, not be replaced by a failing cleanup call.
  par(mar = c(3.2, 4.4, 2.4, 1.0), oma = c(0, 0, 3.2, 0),
      mgp = c(2.6, 0.7, 0))
  sub_fit <- ml_fit_panel(x, tag = "a")
  ps <- c("age", "fitted", "qq", "hist")
  for (i in seq_along(ps)) {
    ml_diag_panel(S, ps[i], tag = letters[i + 1])
  }
  ml_title(
    paste("Fitted model:", S$lawN),
    paste0(sub_fit, "  |  deviance residuals, band: +/- 2 sd")
    )
  layout(1)
  invisible(NULL)
}


# ---- plot.LifeTable --------------------------------------------------------

#' Plot a Life Table
#'
#' Draws the classic life table figures in the house style: the survivorship
#' \eqn{l_x}, the hazard \eqn{m_x} on a log scale, the death distribution
#' \eqn{d_x} and the life expectancy \eqn{e_x}. When the table holds several
#' life tables (matrix input, or a \code{\link{LawTable}} built from several
#' parameter sets) each table is drawn as one curve and the legend labels the
#' tables.
#' @param x An object of class \code{"LifeTable"}.
#' @param which Which panels to draw: \code{"all"} (the default; the four
#'   panels), or a single panel: \code{"lx"}, \code{"hazard"}, \code{"dx"} or
#'   \code{"ex"}.
#' @param split How to arrange the panels when more than one is drawn:
#'   \code{NULL} (the default) draws the four panels in a c(2, 2) grid; give
#'   a length-2 integer c(nrow, ncol) to split them yourself, such as
#'   \code{c(1, 4)} for one row or \code{c(4, 1)} for one column.
#' @param ... Further arguments; currently ignored.
#' @return The object \code{x}, invisibly. Called for the plot it draws.
#' @seealso \code{\link{LifeTable}}, \code{\link{LawTable}}.
#' @author Marius D. Pascariu
#' @example inst/examples/plot.LifeTable.R
#' @export
plot.LifeTable <- function(x,
                           which = c("all", "lx", "hazard", "dx", "ex"),
                           split = NULL,
                           ...) {
  which <- match.arg(which)
  lt    <- x$lt
  grp   <- if ("LT" %in% names(lt)) as.character(lt$LT) else rep("all", nrow(lt))
  tab   <- unique(grp)
  cols  <- ml_series_cols(length(tab))

  panels <- if (which == "all") c("lx", "hazard", "dx", "ex") else which
  split  <- check_split(
    split, length(panels),
    default = if (length(panels) == 4) c(2L, 2L) else c(1L, 1L)
    )

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = split, mar = c(4.2, 4.8, 3.2, 1.2), oma = c(0, 0, 3.2, 0),
      mgp = c(2.8, 0.7, 0))

  tags <- c(lx = "survivorship", hazard = "hazard  m(x)",
            dx = "deaths  d(x)", ex = "life expectancy  e(x)")
  labs <- c(lx = "Survivorship  l(x)", hazard = "Death rate  m(x)",
            dx = "Death distribution  d(x)", ex = "Life expectancy  e(x)")

  for (p in panels) {
    vals <- lapply(tab, function(g) lt[grp == g, , drop = FALSE])
    ylim <- if (p == "hazard") {
      ml_log_ylim(unlist(lapply(vals, function(v) v$mx)))
    } else {
      ml_lin_ylim(unlist(lapply(vals, function(v) v[[p]])),
                  zero = p %in% c("lx", "dx"))
    }
    xr   <- range(lt$x)
    ml_frame(xr, ylim, xlab = "Age  x", ylab = labs[[p]],
             log_y = p == "hazard")

    for (i in seq_along(vals)) {
      v <- vals[[i]]
      if (p == "hazard") {
        mx <- v$mx
        mx[!is.finite(mx) | mx <= 0] <- NA_real_
        lines(v$x, log10(mx), lwd = 2.4, col = cols[i])
      } else {
        y <- v[[p]]
        y[!is.finite(y)] <- NA_real_
        lines(v$x, y, lwd = 2.4, col = cols[i])
      }
    }

    if (length(panels) > 1) {
      ml_tag(paste0("(", letters[match(p, panels)], ")  ", tags[[p]]))
    }
    if (length(tab) > 1 && p == panels[1]) {
      xx  <- unlist(lapply(vals, function(v) v$x))
      yy  <- unlist(lapply(vals, function(v) {
        if (p == "hazard") {
          y <- v$mx
          y[!is.finite(y) | y <= 0] <- NA_real_
          log10(y)
        } else {
          y <- v[[p]]
          y[!is.finite(y)] <- NA_real_
          y
        }
      }))
      pos <- ml_legend_pos(xx, yy, xr, ylim)
      ml_legend(pos, legend = tab, col = cols, lty = rep(1, length(tab)))
    }
  }

  main <- if (which == "all") {
    "Life table: survivorship, hazard, deaths, expectancy"
  } else {
    paste("Life table:", tags[[which]])
  }
  ml_title(
    main,
    sprintf("%d life table%s  |  ages %g-%g  |  one curve per table",
            length(tab), if (length(tab) == 1) "" else "s",
            min(lt$x), max(lt$x))
    )
  return(invisible(x))
}


# ---- plot.convertFx --------------------------------------------------------

#' Indicator labels and symbols
#' @param ind Indicator code: mx, qx, dx, lx, Lx, Tx or ex.
#' @return The axis label or the short symbol of the indicator.
#' @noRd
ml_ind_label <- function(ind) {
  switch(ind,
    mx = "Death rate  m(x)",
    qx = "Death probability  q(x)",
    dx = "Death distribution  d(x)",
    lx = "Survivorship  l(x)",
    Lx = "Person-years lived  L(x)",
    Tx = "Person-years remaining  T(x)",
    ex = "Life expectancy  e(x)"
  )
}

#' @rdname ml_ind_label
#' @noRd
ml_ind_sym <- function(ind) {
  switch(ind,
    mx = "m(x)", qx = "q(x)", dx = "d(x)", lx = "l(x)",
    Lx = "L(x)", Tx = "T(x)", ex = "e(x)"
  )
}


#' Plot a Life Table Indicator Conversion
#'
#' Draws the result of \code{\link{convertFx}} in the house style: the input
#' indicator in one panel and the converted output indicator in the other,
#' sharing the age axis when they are stacked. Rates and probabilities
#' (\code{mx}, \code{qx}) go on a log scale. A matrix input draws one curve
#' per column in both panels and labels the columns in the legend.
#' @param x An object of class \code{"convertFx"}, as returned by
#'   \code{\link{convertFx}}.
#' @param split How to arrange the two panels: \code{NULL} (the default) puts
#'   them side by side, c(1, 2); give a length-2 integer c(nrow, ncol) to
#'   split them yourself, such as \code{c(2, 1)} to stack the output under
#'   the input on a shared age axis.
#' @param ... Further arguments; currently ignored.
#' @return The object \code{x}, invisibly. Called for the plot it draws.
#' @seealso \code{\link{convertFx}}.
#' @author Marius D. Pascariu
#' @example inst/examples/plot.convertFx.R
#' @export
plot.convertFx <- function(x, split = NULL, ...) {
  from <- attr(x, "from")
  to   <- attr(x, "to")
  inp  <- attr(x, "input")
  ages_in  <- inp$x
  ages_out <- attr(x, "x")

  to_mat <- function(z) {
    if (is.null(dim(z))) {
      matrix(as.numeric(z), ncol = 1)
    } else {
      matrix(as.numeric(z), nrow = nrow(z), ncol = ncol(z),
             dimnames = dimnames(z))
    }
  }
  S_in  <- to_mat(inp$data)
  S_out <- to_mat(x)
  K     <- ncol(S_out)
  cols  <- if (K == 1) ML_SERIES[1:2] else ml_series_cols(K)

  split   <- check_split(split, 2, default = c(1L, 2L))
  stacked <- split[2] == 1

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = split, mar = c(4.0, 5.2, 3.2, 1.6), oma = c(0, 0, 3.2, 0),
      mgp = c(3.0, 0.75, 0))

  # panel 1: the input indicator
  log_in <- from %in% c("mx", "qx")
  ylim <- if (log_in) ml_log_ylim(S_in) else ml_lin_ylim(S_in)
  ml_frame(range(ages_in), ylim,
           xlab = if (stacked) "" else "Age  x", ylab = ml_ind_label(from),
           log_y = log_in)
  for (i in seq_len(ncol(S_in))) {
    v <- S_in[, i]
    if (log_in) {
      v[!is.finite(v) | v <= 0] <- NA_real_
      lines(ages_in, log10(v), lwd = 2.4, col = cols[i])
    } else {
      lines(ages_in, v, lwd = 2.4, col = cols[i])
    }
  }
  ml_tag(paste("input:", ml_ind_sym(from)))

  # panel 2: the converted output indicator
  log_out <- to %in% c("mx", "qx")
  ylim <- if (log_out) ml_log_ylim(S_out) else ml_lin_ylim(S_out)
  ml_frame(range(ages_out), ylim, xlab = "Age  x", ylab = ml_ind_label(to),
           log_y = log_out)
  for (i in seq_len(K)) {
    v <- S_out[, i]
    if (log_out) {
      v[!is.finite(v) | v <= 0] <- NA_real_
      lines(ages_out, log10(v), lwd = 2.4, col = cols[i])
    } else {
      lines(ages_out, v, lwd = 2.4, col = cols[i])
    }
  }
  ml_tag(paste("output:", ml_ind_sym(to)))

  if (K > 1) {
    yl <- unlist(lapply(seq_len(K), function(i) {
      v <- S_out[, i]
      if (log_out) log10(replace(v, !is.finite(v) | v <= 0, NA_real_)) else v
    }))
    pos <- ml_legend_pos(rep(ages_out, K), yl, range(ages_out), ylim)
    labs <- colnames(S_out)
    if (is.null(labs)) labs <- as.character(seq_len(K))
    ml_legend(pos, legend = labs, col = cols, lty = rep(1, K))
  }

  ml_title(
    sprintf("convertFx:  %s  ->  %s", ml_ind_sym(from), ml_ind_sym(to)),
    sprintf("%d ages  |  input (panel 1) converted to output (panel 2)%s",
            length(ages_out),
            if (K > 1) sprintf("  |  %d curves, one per column", K) else "")
    )
  return(invisible(x))
}
