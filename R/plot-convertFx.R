# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-06 22:31:36
# --------------------------------------------

# Figure of an indicator conversion: the indicator that went in and the one
# that came out. plot() dispatches here for a "convertFx" object.


# Names and symbols of the life table indicators a conversion can carry.
ML_IND <- list(
  mx = c("death rate", "death rates", "m(x)"),
  qx = c("death probability", "death probabilities", "q(x)"),
  dx = c("death distribution", "death distributions", "d(x)"),
  lx = c("survivorship", "survivorship", "l(x)"),
  Lx = c("person-years lived", "person-years lived", "L(x)"),
  Tx = c("person-years remaining", "person-years remaining", "T(x)"),
  ex = c("life expectancy", "life expectancies", "e(x)")
)


#' Name of a life table indicator
#' @param ind Indicator code: mx, qx, dx, lx, Lx, Tx or ex.
#' @param plural Use the plural form.
#' @return The name in lower case.
#' @noRd
ml_ind_name <- function(ind, plural = FALSE) {
  nm  <- ML_IND[[ind]]
  out <- nm[if (plural) 2L else 1L]

  return(out)
}


#' Symbol of a life table indicator
#' @param ind Indicator code: mx, qx, dx, lx, Lx, Tx or ex.
#' @return The symbol, such as "m(x)".
#' @noRd
ml_ind_sym <- function(ind) {
  nm  <- ML_IND[[ind]]
  out <- nm[3L]

  return(out)
}


#' Axis title of a life table indicator
#' @param ind Indicator code: mx, qx, dx, lx, Lx, Tx or ex.
#' @return The name and the symbol, such as "Death rate  m(x)".
#' @noRd
ml_ind_label <- function(ind) {
  out <- paste0(
    ml_cap(x = ml_ind_name(ind = ind)),
    "  ",
    ml_ind_sym(ind = ind)
  )

  return(out)
}


#' Capitalise the first letter of a string
#' @param x A character vector.
#' @return \code{x} with its first letter in upper case.
#' @noRd
ml_cap <- function(x) {
  out <- paste0(toupper(substring(x, 1, 1)), substring(x, 2))

  return(out)
}


#' Numeric matrix view of a conversion input or output
#' @param z Stored conversion data: a vector, a matrix or a data frame.
#' @return A numeric matrix, one column when \code{z} is a vector.
#' @noRd
ml_fx_mat <- function(z) {

  if (is.null(dim(z))) {
    out <- matrix(as.numeric(z), ncol = 1)
  } else {
    # A multi-column input is stored as a data frame, so it has to become a
    # numeric matrix before as.numeric() can read it.
    z   <- as.matrix(z)
    out <- matrix(
      data     = as.numeric(z),
      nrow     = nrow(z),
      ncol     = ncol(z),
      dimnames = dimnames(z)
    )
  }

  return(out)
}


#' Plot a Life Table Indicator Conversion
#'
#' Draws the result of \code{\link{convertFx}} in the house style: the input
#' indicator in one panel and the converted output indicator in the other,
#' sharing the age axis when they are stacked. Rates and probabilities
#' (\code{mx}, \code{qx}) go on a log scale. A matrix input draws one curve per
#' column in both panels.
#' @param x An object of class \code{"convertFx"}, as returned by
#'   \code{\link{convertFx}}.
#' @param split How to arrange the two panels: \code{NULL} (the default) puts
#'   them side by side, c(1, 2); give a length-2 integer c(nrow, ncol) to split
#'   them yourself, such as \code{c(2, 1)} to stack the output under the input
#'   on a shared age axis.
#' @param ... Further arguments; currently ignored.
#' @return The object \code{x}, invisibly. Called for the plot it draws.
#' @seealso \code{\link{convertFx}}.
#' @author Marius D. Pascariu
#' @example inst/examples/plot.convertFx.R
#' @export
plot.convertFx <- function(x, split = NULL, ...) {
  from     <- attr(x, "from")
  to       <- attr(x, "to")
  inp      <- attr(x, "input")
  ages_in  <- inp$x
  ages_out <- attr(x, "x")
  S_in     <- ml_fx_mat(z = inp$data)
  S_out    <- ml_fx_mat(z = x)
  K        <- ncol(S_out)
  cols     <- if (K == 1) ML_SERIES[1:2] else ml_series_cols(n = K)
  split    <- check_split(split = split, n = 2, default = c(1L, 2L))
  stacked  <- split[2] == 1

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = split)
  par(
    mar = c(3.9, 5.2, 2.2, 1.6),
    oma = c(1, 1, 3.2, 1),
    mgp = c(3.0, 0.75, 0)
  )

  ml_fx_panel(
    mat  = S_in,
    x    = ages_in,
    xlab = if (stacked) "" else "Age  x",
    ylab = ml_ind_label(ind = from),
    log  = from %in% c("mx", "qx"),
    cols = cols
  )
  ml_tag(paste0(
    "input: ",
    ml_ind_sym(ind = from),
    ", ",
    ml_ind_name(ind = from)
  ))

  ylim <- ml_fx_panel(
    mat  = S_out,
    x    = ages_out,
    xlab = "Age  x",
    ylab = ml_ind_label(ind = to),
    log  = to %in% c("mx", "qx"),
    cols = cols
  )
  ml_tag(paste0(
    "output: ",
    ml_ind_sym(ind = to),
    ", ",
    ml_ind_name(ind = to)
  ))

  if (K > 1) {
    yl <- unlist(lapply(seq_len(K), function(i) {
      ml_fx_y(y = S_out[, i], log = to %in% c("mx", "qx"))
    }))
    pos  <- ml_legend_pos(rep(ages_out, K), yl, range(ages_out), ylim)
    labs <- colnames(S_out)
    if (is.null(labs)) labs <- as.character(seq_len(K))
    ml_legend(pos, legend = labs, col = cols, lty = rep(1, K))
  }

  ml_title(
    main = sprintf(
      "convertFx:  %s  ->  %s",
      ml_ind_sym(ind = from),
      ml_ind_sym(ind = to)
    ),
    sub  = ml_fx_sub(from = from, to = to, ages = ages_out, K = K, S_out = S_out)
  )

  return(invisible(x))
}


#' One panel of a conversion: the grid, the axis titles and the curves
#' @param mat One column per converted series.
#' @param x The ages to draw against.
#' @param xlab,ylab Axis titles.
#' @param log Draw the y axis on a log10 scale.
#' @param cols One colour per series.
#' @return NULL; called for the panel it draws.
#' @noRd
ml_fx_panel <- function(mat, x, xlab, ylab, log, cols) {
  ylim <- if (log) ml_log_ylim(v = mat) else ml_lin_ylim(v = mat)
  ml_frame(
    xlim  = range(x),
    ylim  = ylim,
    xlab  = xlab,
    ylab  = ylab,
    log_y = log
  )

  for (i in seq_len(ncol(mat))) {
    lines(
      x, ml_fx_y(y = mat[, i], log = log),
      lwd = 2.4,
      col = cols[i]
    )
  }

  return(invisible(ylim))
}


#' One converted series, transformed for the panel scale
#' @param y The series values.
#' @param log Draw on a log10 scale.
#' @return The numeric vector to draw.
#' @noRd
ml_fx_y <- function(y, log) {

  if (log) {
    y[!is.finite(y) | y <= 0] <- NA_real_
    y <- log10(y)
  } else {
    y[!is.finite(y)] <- NA_real_
  }

  return(y)
}


#' Subtitle of the conversion figure
#' @param from,to Indicator codes converted from and to.
#' @param ages The ages of the output.
#' @param K Number of converted series.
#' @param S_out The converted series, for its column names.
#' @return A length-1 character vector.
#' @noRd
ml_fx_sub <- function(from, to, ages, K, S_out) {
  more <- if (K > 1) {
    nm <- if (is.null(colnames(S_out))) seq_len(K) else colnames(S_out)
    paste0(
      "  |  ", K, " columns: ",
      paste(nm, collapse = ", ")
    )
  } else {
    ""
  }
  out <- sprintf(
    "%s over %d ages (%g-%g) are converted to %s%s",
    ml_cap(x = ml_ind_name(ind = from, plural = TRUE)),
    length(ages),
    min(ages),
    max(ages),
    ml_ind_name(ind = to, plural = TRUE),
    more
  )

  return(out)
}
