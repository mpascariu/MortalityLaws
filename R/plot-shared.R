# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-06 22:37:59
# --------------------------------------------

# House drawing style shared by every plot method: the palette, the panel
# skeleton and the text sizing. plot.MortalityLaw, plot.LifeTable and
# plot.convertFx all build their figures out of these pieces, so a change here
# reaches every figure.


# ---- Design system ---------------------------------------------------------
# Colours shared by all plot methods. Black carries the text and the primary
# series, a saturated green the model or the converted series, and two further
# hues the remaining series. Two soft tints fill the fitted age range and the
# residual band.
ML_TEXT     <- "#000000"
ML_GRID     <- "#D8E0E4"
ML_RULE     <- "#B7C2C8"
ML_TINT_FIT <- "#EDF4DA"
ML_TINT_BND <- "#E6ECEE"
ML_SERIES   <- c("#000000", "#3C8C00", "#1F6FB2", "#B3541E")


#' Series colours for n curves
#'
#' Interpolates the house palette once a figure holds more curves than hues.
#' @param n Number of curves.
#' @return A character vector of n colours.
#' @noRd
ml_series_cols <- function(n) {

  if (n <= length(ML_SERIES)) {
    out <- ML_SERIES[seq_len(n)]
  } else {
    out <- grDevices::colorRampPalette(ML_SERIES)(n)
  }

  return(out)
}


#' Log10 axis labels
#' @param k Decade exponents, drawn as 10^k.
#' @return An expression vector of labels.
#' @noRd
ml_log_expr <- function(k) {
  out <- as.expression(lapply(k, function(i) bquote(10^.(i))))

  return(out)
}


#' Log-scale y limits, padded out to whole decades
#' @param v Values to be placed on a log10 axis.
#' @return A length-2 numeric vector spanning at least one decade.
#' @noRd
ml_log_ylim <- function(v) {
  lg <- log10(v[is.finite(v) & v > 0])

  if (!length(lg)) {
    stop("No positive finite values to place on a log scale.", call. = FALSE)
  }

  lo  <- floor(min(lg))
  hi  <- ceiling(max(lg))
  out <- c(lo, max(lo + 1, hi))

  return(out)
}


#' Linear y limits, padded by a fraction of the range
#' @param v Values to be plotted.
#' @param pad Padding as a fraction of the range.
#' @param zero If TRUE the lower limit is zero.
#' @return A length-2 numeric vector.
#' @noRd
ml_lin_ylim <- function(v, pad = 0.06, zero = FALSE) {
  r <- range(v[is.finite(v)])
  d <- diff(r)

  if (!is.finite(d) || d == 0) {
    d <- max(abs(r), 1)
  }

  lo <- if (zero) {
    0
  } else {
    r[1] - pad * d
  }

  out <- c(lo, r[2] + pad * d)

  return(out)
}


#' Short, readable tick labels
#'
#' Base R writes a large value as "1e+05", which is long and runs into the axis
#' title. Thousands take a k and millions an M, with the value scaled to match,
#' so a survivorship axis reads 0, 20k, 40k, 100k.
#' @param v Tick positions.
#' @return A character vector of labels.
#' @noRd
ml_num <- function(v) {
  big <- max(abs(v), na.rm = TRUE)

  if (big >= 1e6) {
    unit <- 1e6
    suff <- "M"
  } else if (big >= 1e3) {
    unit <- 1e3
    suff <- "k"
  } else {
    unit <- 1
    suff <- ""
  }

  out <- format(v / unit, trim = TRUE, scientific = FALSE, digits = 3)
  out[v == 0] <- "0"
  out <- paste0(out, suff)

  return(out)
}


#' Height of the current panel, in label heights
#'
#' A panel of a four-panel figure is short, so the labels it can hold are
#' limited by its height rather than by its range.
#' @param factor Label heights kept free per label.
#' @return A length-1 numeric count of labels.
#' @noRd
ml_label_room <- function(factor = 1.6) {
  out <- par("pin")[2] / (par("csi") * factor)

  return(out)
}


#' Log y axis of a panel: decade grid, minor ticks and labels
#' @param ylim Panel range, in log10 units.
#' @param yat Tick positions; decades when NULL.
#' @return The tick positions that were drawn.
#' @noRd
ml_axis_y_log <- function(ylim, yat = NULL) {
  k <- seq(ceiling(ylim[1]), floor(ylim[2]))
  k <- k[k >= ylim[1] & k <= ylim[2]]

  if (!length(k)) {
    k <- ylim
  }

  # Only as many decades as the panel has room for, so their labels do not
  # print on top of each other.
  room <- max(2, floor(ml_label_room()))

  if (length(k) > room) {
    k <- k[seq(1, length(k), length.out = room)]
  }

  minor <- unlist(lapply(k, function(i) log10(2:9) + i))
  minor <- minor[minor > ylim[1] & minor < ylim[2]]
  abline(h = k, col = ML_GRID)

  if (length(minor)) {
    axis(
      side      = 2,
      at        = minor,
      labels    = FALSE,
      tcl       = -0.1,
      col       = ML_RULE,
      col.ticks = ML_RULE
    )
  }

  if (is.null(yat)) {
    yat <- k
  }

  axis(
    side      = 2,
    at        = yat,
    labels    = ml_log_expr(yat),
    las       = 1,
    tcl       = -0.22,
    col       = ML_RULE,
    col.ticks = ML_RULE,
    cex.axis  = 0.85,
    col.axis  = ML_TEXT
  )

  return(yat)
}


#' Linear y axis of a panel: grid and labels
#' @param ylim Panel range.
#' @param yat Tick positions; pretty breaks when NULL.
#' @return The tick positions that were drawn.
#' @noRd
ml_axis_y_lin <- function(ylim, yat = NULL) {
  room <- ml_label_room()

  if (is.null(yat)) {
    yat <- pretty(ylim, n = max(2, min(6, floor(room))))
  }

  yat <- yat[yat >= ylim[1] & yat <= ylim[2]]
  abline(h = yat, col = ML_GRID)
  axis(
    side      = 2,
    at        = yat,
    labels    = ml_num(yat),
    las       = 1,
    tcl       = -0.22,
    col       = ML_RULE,
    col.ticks = ML_RULE,
    cex.axis  = 0.85,
    col.axis  = ML_TEXT
  )

  return(yat)
}


#' X axis of a panel: grid and labels
#' @param xlim Panel range.
#' @param xat Tick positions; pretty breaks when NULL.
#' @return NULL; called for the axis it draws.
#' @noRd
ml_axis_x <- function(xlim, xat = NULL) {

  if (is.null(xat)) {
    xat <- pretty(xlim, n = 6)
  }

  xat <- xat[xat >= xlim[1] & xat <= xlim[2]]
  abline(v = xat, col = ML_GRID)
  axis(
    side      = 1,
    at        = xat,
    tcl       = -0.22,
    col       = ML_RULE,
    col.ticks = ML_RULE,
    cex.axis  = 0.85,
    col.axis  = ML_TEXT
  )

  return(invisible(NULL))
}


#' Empty panel with grid, box and axes
#'
#' Draws the plot region of one panel in the house style. Grid and shading go
#' down before the data, the box and the axis labels after.
#' @param xlim,ylim Panel ranges.
#' @param xlab,ylab Axis labels.
#' @param log_y Draw the y axis on a log10 scale with decade and minor ticks.
#' @param xat,yat Tick positions; pretty breaks when NULL.
#' @param shade Optional x range shaded behind the data.
#' @return NULL; called for the panel it draws.
#' @noRd
ml_frame <- function(xlim, ylim, xlab = "", ylab = "", log_y = FALSE,
                     xat = NULL, yat = NULL, shade = NULL) {
  plot(NA, xlim = xlim, ylim = ylim, axes = FALSE, xlab = "", ylab = "",
       xaxs = "i", yaxs = "i")

  if (!is.null(shade)) {
    rect(
      xleft  = shade[1],
      xright = shade[2],
      ybottom = ylim[1],
      ytop   = ylim[2],
      col    = ML_TINT_FIT,
      border = FALSE
    )
  }

  if (log_y) {
    ml_axis_y_log(ylim = ylim, yat = yat)
  } else {
    ml_axis_y_lin(ylim = ylim, yat = yat)
  }

  ml_axis_x(xlim = xlim, xat = xat)
  box(col = ML_RULE)
  title(xlab = xlab, ylab = ylab, col.lab = ML_TEXT, cex.lab = 0.95,
        mgp = c(2.6, 0.6, 0))

  return(invisible(NULL))
}


#' Left-aligned title block over the whole figure
#' @param main,sub Title and subtitle lines.
#' @return NULL; called for the text it draws.
#' @noRd
ml_title <- function(main, sub = NULL) {
  mtext(
    text  = main,
    side  = 3,
    line  = 1.6,
    outer = TRUE,
    adj   = 0,
    cex   = 1.25,
    font  = 2,
    col   = ML_TEXT
  )

  if (!is.null(sub)) {
    # A long subtitle is scaled down rather than allowed off the device.
    cex  <- 1.25
    room <- par("din")[1] - 0.5
    wide <- graphics::strwidth(sub, units = "inches", cex = cex)

    if (wide > room) {
      cex <- cex * room / wide
    }

    mtext(
      text  = sub,
      side  = 3,
      line  = 0.25,
      outer = TRUE,
      adj   = 0,
      cex   = cex,
      col   = ML_TEXT
    )
  }

  return(invisible(NULL))
}


#' Panel tag drawn inside the top margin of the current panel
#'
#' Drawn the way an axis title is: one size, no measuring and no fitting. A tag
#' wider than its panel simply runs on, as a long xlab does.
#' @param tag Label such as "(a)  survivorship".
#' @return NULL; called for the text it draws.
#' @noRd
ml_tag <- function(tag) {
  mtext(
    text = tag,
    side = 3,
    line = 0.35,
    adj  = 0,
    cex  = 0.8,
    font = 2,
    col  = ML_TEXT
  )

  return(invisible(NULL))
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
#' Normalises the split argument of the plot methods to a c(nrow, ncol) layout
#' holding exactly one slot per panel.
#' @param split NULL, or a length-2 integer c(nrow, ncol).
#' @param n Number of panels to place.
#' @param default The layout used when split is NULL.
#' @return A length-2 integer vector.
#' @noRd
check_split <- function(split, n, default) {

  if (is.null(split)) {
    out <- as.integer(default)
  } else {
    ok <- is.numeric(split) && length(split) == 2 && all(is.finite(split)) &&
      all(split >= 1) && all(split == round(split)) && prod(split) == n

    if (!ok) {
      stop("'split' must be NULL or a length-2 integer c(nrow, ncol) with ",
           "nrow * ncol = ", n, " (the number of panels).", call. = FALSE)
    }

    out <- as.integer(split)
  }

  return(out)
}
