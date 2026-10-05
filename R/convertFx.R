# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-04 17:46:32
# --------------------------------------------

#' Convert Life Table Indicators
#'
#' Easily convert between different life table indicators (e.g., from death 
#' rates \code{mx} to death probabilities \code{qx}, or from survivorship 
#' \code{lx} to life expectancy \code{ex}). The function wraps 
#' \code{\link{LifeTable}} internally, so the conversion relies on the 
#' same constant-force-of-mortality (CFM) assumption and life-table 
#' methodology used throughout the package.
#'
#' @details
#' This function provides a convenient interface for converting a single 
#' mortality indicator into another, without having to call 
#' \code{\link{LifeTable}} directly and extract the desired column.
#'
#' The supported \strong{input} types (\code{from}) are:
#' \code{mx}, \code{qx}, \code{dx}, \code{lx}, and \code{ex}.
#'
#' The supported \strong{output} types (\code{to}) are:
#' \code{mx}, \code{qx}, \code{dx}, \code{lx}, \code{Lx}, \code{Tx}, and 
#' \code{ex}.
#'
#' There are 35 possible \code{from}-\code{to} combinations (5 inputs 
#' \eqn{\times} 7 outputs). Conversions that need a single life-table 
#' identity, such as \code{mx} to \code{qx} or \code{dx} to \code{lx}, are 
#' computed directly from that relation. All the other conversions are 
#' obtained from the full life-table computation; for example, converting 
#' \code{mx} to \code{ex} will internally compute \code{qx}, \code{lx}, 
#' \code{dx}, \code{Lx}, and \code{Tx} in sequence. A \code{ex} input is 
#' converted by building the life table that reproduces the supplied curve 
#' (see \code{\link{LifeTable}}).
#'
#' When \code{data} is a \code{vector}, the function returns a named vector. 
#' When \code{data} is a \code{matrix} or \code{data.frame} with multiple 
#' columns, the function applies the conversion column-wise and returns a 
#' matrix with the same row and column names as the input.
#'
#' @inheritParams LifeTable
#'
#' @param data A numeric \code{vector}, \code{matrix}, or \code{data.frame} 
#'   containing the mortality indicator to be converted. Each row should 
#'   correspond to an age, each column to a separate population or time 
#'   period.
#'
#' @param from The type of indicator supplied in \code{data}. One of:
#'   \code{"mx"}, \code{"qx"}, \code{"dx"}, \code{"lx"}, or \code{"ex"}.
#'
#' @param to The desired output indicator. One of:
#'   \code{"mx"}, \code{"qx"}, \code{"dx"}, \code{"lx"}, \code{"Lx"}, 
#'   \code{"Tx"}, or \code{"ex"}.
#'
#' @param ... Further arguments passed to \code{\link{LifeTable}} that may 
#'   effect the results, such as \code{sex}, \code{lx0}, \code{ax}, or the 
#'   closing arguments \code{close}, \code{omega} and \code{fit_from}. When 
#'   \code{omega} extends the table beyond the input's open age, the result 
#'   carries the extended ages (vector names, or matrix row names) rather 
#'   than the input ages.
#'
#' @return A numeric vector or matrix of class \code{"convertFx"} containing
#'   the converted life table indicator. If the input was a named object, the
#'   output retains those names. The result carries the ages it is indexed by
#'   (\code{x}), the conversion (\code{from}, \code{to}) and the input curve
#'   (\code{input}) as attributes, which is what
#'   \code{\link{plot.convertFx}} draws. It behaves like the underlying
#'   numeric vector or matrix in every other respect; subsetting returns the
#'   bare values.
#'
#' @seealso
#' \code{\link{LifeTable}} for the underlying life-table construction;
#' \code{\link{LawTable}} for generating life tables from parametric
#'   mortality laws; \code{\link{plot.convertFx}} for plotting a conversion.
#'
#' @author Marius D. Pascariu
#'
#' @example inst/examples/convertFx.R
#' @export
convertFx <- function(x,
                      data,
                      from = c("mx", "qx", "dx", "lx", "ex"),
                      to = c("mx", "qx", "dx", "lx", "Lx", "Tx", "ex"),
                      ...) {

  from <- match.arg(from)
  to   <- match.arg(to)
  dots <- list(...)
  lx0  <- dots[["lx0"]]
  ax_u <- dots[["ax"]]
  ext  <- !is.null(dots[["omega"]]) || !is.null(dots[["close"]])

  LT <- switch(
    from,
    mx = function(w) LifeTable(x = x, mx = w, ...),
    qx = function(w) LifeTable(x = x, qx = w, ...),
    dx = function(w) LifeTable(x = x, dx = w, ...),
    lx = function(w) LifeTable(x = x, lx = w, ...),
    ex = function(w) LifeTable(x = x, ex = w, ...)
    )

  # A classed bare vector (e.g. a convertFx result fed back in) is vector
  # input; only objects with dimensions take the matrix branch.
  if (is.null(dim(data))) {
    if (length(x) != length(data)) {
      stop("The 'x' and 'data' do not have the same length", call. = FALSE)
    }

    LTt <- LT(data)$lt
    out <- LTt[, to]
    # An extension argument (omega) adds rows above the input's open age; the
    # output is then labelled by the extended ages rather than by the input.
    out_x <- if (nrow(LTt) == length(data)) x else LTt$x
    names(out) <- if (nrow(LTt) == length(data)) names(data) else out_x

  } else {
    if (length(x) != nrow(data)) {
      stop("The length of 'x' must be equal to the number of rows in 'data'",
           call. = FALSE)
    }

    out <- convert_fx_matrix(x = x, data = data, from = from, to = to,
                             LT = LT, lx0 = lx0, ax = ax_u, ext = ext)
    out_x <- attr(out, "x")
  }

  # Tag the result with what was converted so that plot.convertFx() can
  # rebuild the conversion view; the values stay plain numerics underneath
  # and subsetting returns them bare.
  out <- structure(
    out,
    class = c("convertFx", class(out)),
    x = out_x,
    from = from,
    to = to,
    input = list(x = x, data = data)
    )

  return(out)
}


#' Print a Converted Life Table Indicator
#'
#' Prints the conversion that produced the object and the converted values.
#' @param x An object of class \code{"convertFx"}.
#' @param ... Further arguments passed to or from other methods.
#' @return The object \code{x}, invisibly. Called for its printed output.
#' @seealso \code{\link{convertFx}}.
#' @keywords internal
#' @export
print.convertFx <- function(x, ...) {
  from <- attr(x, "from")
  to   <- attr(x, "to")
  nr   <- if (is.null(dim(x))) length(x) else nrow(x)
  nc   <- if (is.null(dim(x))) 1L else ncol(x)
  cat(sprintf("convertFx result: %s -> %s  |  %d age%s%s\n",
              from, to, nr, if (nr == 1) "" else "s",
              if (nc > 1) sprintf(", %d columns", nc) else ""))
  y <- unclass(x)
  attributes(y) <- attributes(y)[c("names", "dim", "dimnames")]
  print(y)
  return(invisible(x))
}


#' Convert a Matrix of Life Table Indicators Column by Column
#'
#' Internal workhorse of \code{\link{convertFx}} for matrix and data frame
#' input. It uses the life-table primitives directly when the requested
#' \code{from}-\code{to} pair is a single identity and falls back on one call
#' to \code{\link{LifeTable}} for the whole matrix otherwise.
#'
#' @inheritParams LifeTable
#' @param data A numeric matrix or data frame, one column per life table.
#' @param from The type of indicator supplied in \code{data}.
#' @param to The desired output indicator.
#' @param LT A function calling \code{\link{LifeTable}} with the argument
#' named after \code{from}.
#' @param ax The \code{ax} argument supplied to \code{\link{convertFx}}, or
#'   \code{NULL}. The closing identities in this function are the constant
#'   force of mortality ones and carry no \code{ax}, so any supplied \code{ax}
#'   (numeric or a method name) disables them and the generic
#'   \code{\link{LifeTable}} path is used, which honours it.
#' @return A numeric matrix with the converted indicator.
#' @noRd
convert_fx_matrix <- function(x, data, from, to, LT, lx0, ax = NULL,
                              ext = FALSE) {

  M    <- as.matrix(data)
  N    <- length(x)
  nx   <- c(diff(x), diff(x)[N - 1])
  case <- paste0(from, "_to_", to)
  # A one-step identity needs a finite input and at least two age intervals.
  # The mx/qx identities assume the constant force of mortality conversion and
  # carry no ax, so they are only valid when no ax was supplied; otherwise the
  # generic LifeTable path below is used, so that a matrix gives the same
  # answer as a single column. The dx/lx pair is pure arithmetic and has no
  # ax dependence.
  ok   <- !ext && is.null(ax) && length(nx) == N && all(is.finite(M))
  one  <- switch(
    case,
    mx_to_qx = FALSE,
    qx_to_mx = FALSE,
    dx_to_lx = ok && all(M >= 0) && all(colSums(M) > 0),
    lx_to_dx = ok && all(M >= 0) && all(M[1, ] > 0),
    FALSE
    )

  if (one && case == "mx_to_qx") {
    out <- mx_qx(x = x, nx = nx, ux = M, out = "qx")
    out[N, ] <- 1

  } else if (one && case == "qx_to_mx") {
    out <- mx_qx(x = x, nx = nx, ux = M, out = "mx")

  } else if (one && case == "dx_to_lx") {
    if (is.null(lx0)) lx0 <- 1e5
    M   <- sweep(M * lx0, 2, colSums(M), "/")
    out <- dx_lx(ux = M, out = "lx")

  } else if (one && case == "lx_to_dx") {
    if (is.null(lx0)) lx0 <- 1e5
    M   <- sweep(M * lx0, 2, M[1, ], "/")
    out <- dx_lx(ux = M, out = "dx")

  } else {
    if (is.null(colnames(M))) colnames(M) <- seq_len(ncol(M))
    LTm <- LT(M)$lt
    # An extension argument (omega) lengthens every column above the input's
    # open age; keep the matrix shape on the extended grid and relabel rows.
    if (nrow(LTm) != N) {
      out <- matrix(LTm[, to], ncol = ncol(M),
                    dimnames = list(unique(LTm$x), colnames(M)))
      attr(out, "x") <- unique(LTm$x)
      return(out)
    }
    out <- matrix(LTm[, to], nrow = N)
  }

  dimnames(out) <- dimnames(data)
  attr(out, "x") <- x
  return(out)
}
