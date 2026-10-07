# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-06 22:31:36
# --------------------------------------------

# Figure of a life table: survivorship, hazard, deaths and life expectancy.
# plot() dispatches here for a "LifeTable" object, single table or many.


# Panel tags and axis labels of the four life table panels.
ml_lt_tags <- c(
  lx     = "survivorship  l(x)",
  hazard = "hazard  m(x)",
  dx     = "deaths  d(x)",
  ex     = "life expectancy  e(x)"
)

ml_lt_labs <- c(
  lx     = "Survivorship  l(x)",
  hazard = "Death rate  m(x)",
  dx     = "Death distribution  d(x)",
  ex     = "Life expectancy  e(x)"
)


#' Plot a Life Table
#'
#' Draws the classic life table figures in the house style: the survivorship
#' \eqn{l_x}, the hazard \eqn{m_x} on a log scale, the death distribution
#' \eqn{d_x} and the life expectancy \eqn{e_x}. When the table holds several
#' life tables, from a matrix input or from a \code{\link{LawTable}} built on
#' several parameter sets, each table is drawn as one curve and the legend
#' labels the tables.
#' @param x An object of class \code{"LifeTable"}.
#' @param which Which panels to draw: \code{"all"} (the default; the four
#'   panels), or a single panel: \code{"lx"}, \code{"hazard"}, \code{"dx"} or
#'   \code{"ex"}.
#' @param split How to arrange the panels when more than one is drawn:
#'   \code{NULL} (the default) draws the four panels in a c(2, 2) grid; give a
#'   length-2 integer c(nrow, ncol) to split them yourself, such as
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
  which  <- match.arg(which)
  lt     <- x$lt
  grp    <- if ("LT" %in% names(lt)) {
    as.character(lt$LT)
  } else {
    rep("all", nrow(lt))
  }
  tab    <- unique(grp)
  cols   <- ml_series_cols(n = length(tab))
  panels <- if (which == "all") c("lx", "hazard", "dx", "ex") else which
  split  <- check_split(
    split   = split,
    n       = length(panels),
    default = if (length(panels) == 4) c(2L, 2L) else c(1L, 1L)
  )

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = split)
  par(
    mar = c(3.9, 4.8, 2.0, 1.2),
    oma = c(1, 1, 3.2, 1),
    mgp = c(2.8, 0.7, 0)
  )
  vals <- lapply(tab, function(g) lt[grp == g, , drop = FALSE])
  xr   <- range(lt$x)

  for (p in panels) {
    ylim <- ml_lt_panel(p = p, vals = vals, cols = cols, xlim = xr)

    if (length(panels) > 1) {
      ml_tag(paste0("(", letters[match(p, panels)], ")  ", ml_lt_tags[[p]]))
    }

    if (length(tab) > 1 && p == panels[1]) {
      xx  <- unlist(lapply(vals, function(v) v$x))
      yy  <- unlist(lapply(vals, function(v) ml_lt_y(v = v, p = p)))
      pos <- ml_legend_pos(xx, yy, xr, ylim)
      ml_legend(pos, legend = tab, col = cols, lty = rep(1, length(tab)))
    }
  }

  main <- if (which == "all") {
    "Life table: survivorship, hazard, deaths, expectancy"
  } else {
    paste("Life table:", ml_lt_tags[[which]])
  }
  ml_title(main = main, sub = ml_lt_sub(lt = lt, tab = tab))

  return(invisible(x))
}


#' One life table panel: its grid, its axis titles and its curves
#' @param p Which panel: "lx", "hazard", "dx" or "ex".
#' @param vals The tables to draw, one data frame each.
#' @param cols One colour per table.
#' @param xlim Age range of the panel.
#' @return NULL; called for the panel it draws.
#' @noRd
ml_lt_panel <- function(p, vals, cols, xlim) {

  if (p == "hazard") {
    ylim <- ml_log_ylim(v = unlist(lapply(vals, function(v) v$mx)))
  } else {
    ylim <- ml_lin_ylim(
      v    = unlist(lapply(vals, function(v) v[[p]])),
      zero = p %in% c("lx", "dx")
    )
  }

  ml_frame(
    xlim  = xlim,
    ylim  = ylim,
    xlab  = "Age  x",
    ylab  = ml_lt_labs[[p]],
    log_y = p == "hazard"
  )

  for (i in seq_along(vals)) {
    ml_lt_series(v = vals[[i]], p = p, col = cols[i])
  }

  return(invisible(ylim))
}


#' Panel values of one life table, transformed for the panel scale
#' @param v One life table, as returned by \code{LifeTable}.
#' @param p Which panel: "lx", "hazard", "dx" or "ex".
#' @return The numeric vector to draw.
#' @noRd
ml_lt_y <- function(v, p) {

  if (p == "hazard") {
    y <- v$mx
    y[!is.finite(y) | y <= 0] <- NA_real_
    y <- log10(y)
  } else {
    y <- v[[p]]
    y[!is.finite(y)] <- NA_real_
  }

  return(y)
}


#' One curve of a life table panel
#' @param v One life table, as returned by \code{LifeTable}.
#' @param p Which panel: "lx", "hazard", "dx" or "ex".
#' @param col Colour of the curve.
#' @return NULL; called for the curve it draws.
#' @noRd
ml_lt_series <- function(v, p, col) {

  lines(
    v$x, ml_lt_y(v = v, p = p),
    lwd = 2.4,
    col = col
  )

  return(invisible(NULL))
}


#' Subtitle of the life table figure
#' @param lt The life table data frame.
#' @param tab The table labels.
#' @return A length-1 character vector.
#' @noRd
ml_lt_sub <- function(lt, tab) {
  out <- sprintf(
    "%d life table%s  |  ages %g-%g  |  one curve per table",
    length(tab),
    if (length(tab) == 1) "" else "s",
    min(lt$x),
    max(lt$x)
  )

  return(out)
}
