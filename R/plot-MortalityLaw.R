# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-06 22:31:36
# --------------------------------------------

# Figures of a fitted mortality law: the fit chart, the four residual
# diagnostics, and the two of them stacked into one figure. plot() dispatches
# here for a "MortalityLaw" object.


# ---- plot.MortalityLaw -----------------------------------------------------

#' Plot a Fitted Mortality Law
#'
#' Draws the figures of a \code{"MortalityLaw"} fit in the house style. The fit
#' chart puts the observed and the fitted mortality on a log scale, with the
#' fitted age range shaded and a goodness-of-fit subtitle. The residual
#' diagnostics give the deviance residuals against age and against the fitted
#' values, each with a band of two standard deviations and a lowess smooth,
#' plus a normal Q-Q plot and the residual distribution with a normal density.
#' @param x An object of class \code{"MortalityLaw"}.
#' @param which Which figure to draw: \code{"both"} (the default; the fit chart
#'   and the residual panels in one figure), \code{"fit"} (the fit chart alone)
#'   or \code{"diagnostics"} (the four residual panels alone).
#' @param split How to arrange the four diagnostic panels: \code{NULL} (the
#'   default) draws them in a c(2, 2) grid; give a length-2 integer
#'   c(nrow, ncol) to split them yourself, such as \code{c(1, 4)} for one row
#'   or \code{c(4, 1)} for one column. Ignored when only the fit chart is
#'   drawn.
#' @param ... Further arguments; currently ignored.
#' @return The object \code{x}, invisibly. Called for the figures it draws.
#' @seealso \code{\link{MortalityLaw}}.
#' @author Marius D. Pascariu
#' @example inst/examples/plot.MortalityLaw.R
#' @export
plot.MortalityLaw <- function(x,
                              which = c("both", "fit", "diagnostics"),
                              split = NULL,
                              ...) {
  which <- match.arg(which)
  cases <- with(x$input, detect_case(Dx = Dx, Ex = Ex, mx = mx, qx = qx))

  if (!any(cases$iclass == "numeric")) {
    stop(
      "Plot function not available for multiple mortality curves",
      call. = FALSE
    )
  }

  # Validate before touching a graphical parameter, so that an argument error
  # leaves the device exactly as it was found.

  if (which %in% c("both", "diagnostics")) {
    split <- check_split(split = split, n = 4, default = c(2L, 2L))
  }

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  if (which == "both") {
    ml_fit_and_diagnostics(x = x, split = split)
  } else if (which == "fit") {
    ml_fit(x = x)
  } else {
    ml_diagnostics(x = x, split = split)
  }

  return(invisible(x))
}


#' Law label of a fitted mortality law
#' @param x An object of class \code{"MortalityLaw"}.
#' @return The law name followed by the word "law".
#' @noRd
ml_law_label <- function(x) {
  law <- x$input$law

  if (law == "custom.law") {
    lawN <- "Custom Mortality"
  } else {
    lawN <- unlist(availableLaws(law = law)$table["NAME"])
  }

  out <- paste(lawN, "law")

  return(out)
}


#' Fit chart panel: observed mortality against fitted
#' @param x An object of class \code{"MortalityLaw"}.
#' @param tag Optional panel tag letter, drawn in the panel's top margin.
#' @return The goodness-of-fit subtitle line, invisibly.
#' @noRd
ml_fit_panel <- function(x, tag = NULL) {
  age   <- x$input$x
  age2  <- x$input$fit.this.x
  lawN  <- ml_law_label(x = x)
  y     <- observed_values(x = x)
  fit_y <- x$fitted.values
  lab   <- if (!is.null(x$input$qx)) {
    "Death probability  q(x)"
  } else {
    "Death rate  m(x)"
  }

  # The chart is on a log scale, so values the model cannot place on it are
  # omitted. Out-of-range extrapolation produces them.
  y[!is.finite(y) | y <= 0] <- NA_real_
  fit_y[!is.finite(fit_y) | fit_y <= 0] <- NA_real_

  ylim <- ml_log_ylim(v = c(y, fit_y))
  ml_frame(
    xlim  = range(age),
    ylim  = ylim,
    xlab  = "Age  x",
    ylab  = lab,
    log_y = TRUE,
    shade = range(age2) + c(-0.5, 0.5)
  )
  points(
    age, log10(y),
    pch = 16,
    cex = 1,
    col = ML_SERIES[1]
  )
  lines(
    age, log10(fit_y),
    lwd = 2.4,
    col = ML_SERIES[2]
  )

  # Goodness of fit on the observed scale, over the fitted age range.
  q    <- fit_quality(x = x)
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
    ml_tag(paste0("(", tag, ")  observed (dots) vs fitted (line)"))
  }

  return(invisible(sub))
}


#' Fit chart figure: the fit panel with its title block
#' @param x An object of class \code{"MortalityLaw"}.
#' @return NULL; called for the figure it draws.
#' @noRd
ml_fit <- function(x) {
  # mfrow = c(1, 1) also releases a layout left by the combined figure.
  par(mfrow = c(1, 1))
  par(
    mar = c(4.4, 5.2, 1.2, 1.6),
    oma = c(1, 1, 3.2, 1),
    mgp = c(3.0, 0.75, 0)
  )
  sub <- ml_fit_panel(x = x)
  ml_title(main = paste("Fitted model:", ml_law_label(x = x)), sub = sub)

  return(invisible(NULL))
}


#' Residual statistics shared by the diagnostic panels
#' @param x An object of class \code{"MortalityLaw"}.
#' @return The residuals and the scales they are drawn on.
#' @noRd
ml_diag_stats <- function(x) {
  d <- as.numeric(x$deviance.residuals)
  n <- length(d)

  if (n < 2) {
    stop(
      "At least two residuals are needed for the diagnostic panels.",
      call. = FALSE
    )
  }

  out <- list(
    d     = d,
    n     = n,
    sd_d  = sd(d),
    age   = x$input$x,
    fit_y = as.numeric(x$fitted.values),
    lawN  = ml_law_label(x = x)
  )

  return(out)
}


#' Panel labels of the residual diagnostics
#'
#' Fixed, so a figure can name its panels before it draws them.
#' @noRd
ml_diag_labs <- c(
  age    = "vs age, lowess smooth",
  fitted = "vs fitted, lowess smooth",
  qq     = "normal Q-Q of the residuals",
  hist   = "distribution of the residuals"
)


#' One residual diagnostic panel
#' @param S Residual statistics from \code{ml_diag_stats}.
#' @param p Which panel: "age", "fitted", "qq" or "hist".
#' @param tag Optional panel tag letter, drawn in the panel's top margin.
#' @return NULL; called for the panel it draws.
#' @noRd
ml_diag_panel <- function(S, p, tag = NULL) {

  if (p == "age") {
    ml_diag_age(S = S)
  } else if (p == "fitted") {
    ml_diag_fitted(S = S)
  } else if (p == "qq") {
    ml_diag_qq(S = S)
  } else {
    ml_diag_hist(S = S)
  }

  if (!is.null(tag)) {
    ml_tag(paste0("(", tag, ")  ", ml_diag_labs[p]))
  }

  return(invisible(NULL))
}


#' Diagnostic panel: residuals against age
#' @param S Residual statistics from \code{ml_diag_stats}.
#' @return NULL; called for the panel it draws.
#' @noRd
ml_diag_age <- function(S) {
  ylim <- ml_lin_ylim(v = c(S$d, 2.2 * S$sd_d, -2.2 * S$sd_d))
  ml_frame(
    xlim = range(S$age),
    ylim = ylim,
    xlab = "Age  x",
    ylab = "Deviance residual"
  )
  rect(
    xleft   = S$age[1] - 1,
    ybottom = -2 * S$sd_d,
    xright  = S$age[S$n] + 1,
    ytop    = 2 * S$sd_d,
    col     = ML_TINT_BND,
    border  = FALSE
  )
  abline(
    h   = 0,
    lty = 2,
    col = ML_RULE
  )
  abline(
    h   = c(-2, 2) * S$sd_d,
    lty = 3,
    col = ML_RULE
  )
  points(
    S$age, S$d,
    pch = 16,
    cex = 1,
    col = ML_SERIES[1]
  )
  lines(
    lowess(S$age, S$d, f = 0.75),
    lwd = 2.4,
    col = ML_SERIES[2]
  )
  pos <- ml_legend_pos(S$age, S$d, range(S$age), ylim)
  ml_legend(pos, legend = c("Deviance residual", "Lowess smooth"),
            col = ML_SERIES[1:2], lty = c(NA, 1), pch = c(16, NA))

  return(invisible(NULL))
}


#' Diagnostic panel: residuals against fitted values
#' @param S Residual statistics from \code{ml_diag_stats}.
#' @return NULL; called for the panel it draws.
#' @noRd
ml_diag_fitted <- function(S) {
  xlim <- ml_lin_ylim(v = S$fit_y)
  ylim <- ml_lin_ylim(v = c(S$d, 2.2 * S$sd_d, -2.2 * S$sd_d))
  ml_frame(
    xlim = xlim,
    ylim = ylim,
    xlab = "Fitted values",
    ylab = "Deviance residual"
  )
  rect(
    xleft   = xlim[1],
    ybottom = -2 * S$sd_d,
    xright  = xlim[2],
    ytop    = 2 * S$sd_d,
    col     = ML_TINT_BND,
    border  = FALSE
  )
  abline(
    h   = 0,
    lty = 2,
    col = ML_RULE
  )
  abline(
    h   = c(-2, 2) * S$sd_d,
    lty = 3,
    col = ML_RULE
  )
  points(
    S$fit_y, S$d,
    pch = 16,
    cex = 1,
    col = ML_SERIES[1]
  )
  lines(
    lowess(S$fit_y, S$d, f = 0.75),
    lwd = 2.4,
    col = ML_SERIES[2]
  )

  return(invisible(NULL))
}


#' Diagnostic panel: normal Q-Q plot of the residuals
#' @param S Residual statistics from \code{ml_diag_stats}.
#' @return NULL; called for the panel it draws.
#' @noRd
ml_diag_qq <- function(S) {
  qq <- qqnorm(S$d, plot.it = FALSE)
  ml_frame(
    xlim = ml_lin_ylim(v = qq$x),
    ylim = ml_lin_ylim(v = qq$y),
    xlab = "Theoretical quantiles",
    ylab = "Sample quantiles"
  )
  abline(
    h   = 0,
    v   = 0,
    col = ML_GRID
  )
  qs <- quantile(S$d, c(0.25, 0.75))
  xs <- qnorm(c(0.25, 0.75))
  abline(
    a = qs[1] - diff(qs) / diff(xs) * xs[1],
    b = diff(qs) / diff(xs),
    col = ML_SERIES[2],
    lwd = 2.4
  )
  points(
    qq$x, qq$y,
    pch = 16,
    cex = 1,
    col = ML_SERIES[1]
  )

  return(invisible(NULL))
}


#' Diagnostic panel: distribution of the residuals
#' @param S Residual statistics from \code{ml_diag_stats}.
#' @return NULL; called for the panel it draws.
#' @noRd
ml_diag_hist <- function(S) {
  brks <- if (diff(range(S$d)) > 0) {
    "FD"
  } else {
    seq(
      S$d[1] - 0.5, S$d[1] + 0.5,
      length.out = 6
    )
  }
  h  <- hist(S$d, breaks = brks, plot = FALSE)
  bw <- mean(diff(h$breaks))
  xd <- seq(
    min(h$breaks), max(h$breaks),
    length.out = 200
  )
  yd <- dnorm(xd, 0, S$sd_d) * S$n * bw
  ml_frame(
    xlim = range(h$breaks),
    ylim = ml_lin_ylim(v = c(h$counts, yd), zero = TRUE),
    xlab = "Deviance residual",
    ylab = "Frequency"
  )
  rect(
    xleft   = h$breaks[-length(h$breaks)],
    ybottom = 0,
    xright  = h$breaks[-1],
    ytop    = h$counts,
    col     = grDevices::adjustcolor(ML_SERIES[1], 0.4),
    border  = "white",
    lwd     = 0.8
  )
  lines(
    xd, yd,
    lwd = 2.4,
    col = ML_SERIES[2]
  )

  return(invisible(NULL))
}


#' Diagnostics subtitle line
#' @param S Residual statistics from \code{ml_diag_stats}.
#' @return A length-1 character vector.
#' @noRd
ml_diag_sub <- function(S) {
  out <- sprintf("deviance residuals  |  n = %d  |  band: +/- 2 sd", S$n)

  return(out)
}


#' Residual diagnostics figure: four panels in the selected split
#' @param x An object of class \code{"MortalityLaw"}.
#' @param split Panel layout; see \code{plot.MortalityLaw}.
#' @return NULL; called for the figure it draws.
#' @noRd
ml_diagnostics <- function(x, split = NULL) {
  split <- check_split(split = split, n = 4, default = c(2L, 2L))
  S     <- ml_diag_stats(x = x)
  ps    <- c("age", "fitted", "qq", "hist")
  par(mfrow = split)
  par(
    mar = c(3.9, 4.8, 2.0, 1.2),
    oma = c(1, 1, 3.2, 1),
    mgp = c(2.8, 0.7, 0)
  )

  for (i in seq_along(ps)) {
    ml_diag_panel(S = S, p = ps[i], tag = letters[i])
  }

  ml_title(
    main = paste("Residual diagnostics:", S$lawN),
    sub  = ml_diag_sub(S = S)
  )

  return(invisible(NULL))
}


#' Combined figure: the fit chart above the residual panels
#' @param x An object of class \code{"MortalityLaw"}.
#' @param split Panel layout for the residual panels; see
#'   \code{plot.MortalityLaw}.
#' @return NULL; called for the figure it draws.
#' @noRd
ml_fit_and_diagnostics <- function(x, split = NULL) {
  split <- check_split(split = split, n = 4, default = c(2L, 2L))
  S     <- ml_diag_stats(x = x)
  ps    <- c("age", "fitted", "qq", "hist")
  mat   <- matrix(NA_integer_, nrow = 1L + split[1], ncol = split[2])
  mat[1, ] <- 1L
  mat[-1, ] <- seq_len(prod(split)) + 1L

  layout(mat, heights = c(1.4, rep(1, split[1])))
  # Small margins, because the figure stacks 1 + split[1] panel rows on one
  # device. Every row's bottom margin must clear its axis title: mgp[1] lines,
  # scaled by cex.lab, plus the title's own line height. Anything tighter and
  # the next panel paints over the label it overflows into.
  par(
    mar = c(4.8, 4.6, 2.0, 1.0),
    oma = c(1, 1, 3.2, 1),
    mgp = c(2.5, 0.7, 0)
  )
  sub_fit <- ml_fit_panel(x = x, tag = "a")

  for (i in seq_along(ps)) {
    ml_diag_panel(S = S, p = ps[i], tag = letters[i + 1])
  }

  ml_title(
    main = paste("Fitted model:", S$lawN),
    sub  = paste0(sub_fit, "  |  deviance residuals, band: +/- 2 sd")
  )
  # The layout is released on the success path only. A drawing error has to
  # surface on its own, not be replaced by a failing cleanup call.
  layout(1)

  return(invisible(NULL))
}
