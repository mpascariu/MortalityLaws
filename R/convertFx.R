# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-05 18:53:46
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
#' \code{mx}, \code{qx}, \code{dx}, and \code{lx}.
#'
#' The supported \strong{output} types (\code{to}) are:
#' \code{mx}, \code{qx}, \code{dx}, \code{lx}, \code{Lx}, \code{Tx}, and 
#' \code{ex}.
#'
#' There are 28 possible \code{from}-\code{to} combinations (4 inputs 
#' \eqn{\times} 7 outputs). Conversions that need a single life-table 
#' identity, such as \code{mx} to \code{qx} or \code{dx} to \code{lx}, are 
#' computed directly from that relation. All the other conversions are 
#' obtained from the full life-table computation; for example, converting 
#' \code{mx} to \code{ex} will internally compute \code{qx}, \code{lx}, 
#' \code{dx}, \code{Lx}, and \code{Tx} in sequence.
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
#'   \code{"mx"}, \code{"qx"}, \code{"dx"}, or \code{"lx"}.
#'
#' @param to The desired output indicator. One of:
#'   \code{"mx"}, \code{"qx"}, \code{"dx"}, \code{"lx"}, \code{"Lx"}, 
#'   \code{"Tx"}, or \code{"ex"}.
#'
#' @param ... Further arguments passed to \code{\link{LifeTable}} that may 
#'   affect the results, such as \code{sex}, \code{lx0}, or \code{ax}.
#'
#' @return A numeric vector or matrix containing the converted life table 
#'   indicator. If the input was a named object, the output retains those 
#'   names.
#'
#' @seealso
#' \code{\link{LifeTable}} for the underlying life-table construction; 
#' \code{\link{LawTable}} for generating life tables from parametric 
#'   mortality laws.
#'
#' @author Marius D. Pascariu
#'
#' @examples
#' # ---- Basic conversions ----
#'
#' x  <- 0:110
#' mx <- ahmd$mx
#'
#' # Convert death rates to death probabilities
#' qx <- convertFx(x, data = mx, from = "mx", to = "qx")
#'
#' # Convert death rates to death distribution
#' dx <- convertFx(x, data = mx, from = "mx", to = "dx")
#'
#' # Convert death rates to survivorship
#' lx <- convertFx(x, data = mx, from = "mx", to = "lx")
#'
#' # ---- All 28 possible conversions ----
#'
#' from <- c("mx", "qx", "dx", "lx")
#' to   <- c("mx", "qx", "dx", "lx", "Lx", "Tx", "ex")
#' K    <- expand.grid(from = from, to = to)
#'
#' for (i in 1:nrow(K)) {
#'   In  <- as.character(K[i, "from"])
#'   Out <- as.character(K[i, "to"])
#'   N   <- paste0(Out, "_from_", In)
#'   cat(i, " Create", N, "\n")
#'   assign(N, convertFx(x = x, data = get(In), from = In, to = Out))
#' }
#'
#' @export
convertFx <- function(x,
                      data,
                      from = c("mx", "qx", "dx", "lx"),
                      to = c("mx", "qx", "dx", "lx", "Lx", "Tx", "ex"),
                      ...) {

  from <- match.arg(from)
  to   <- match.arg(to)
  lx0  <- list(...)[["lx0"]]

  LT <- switch(
    from,
    mx = function(w) LifeTable(x = x, mx = w, ...),
    qx = function(w) LifeTable(x = x, qx = w, ...),
    dx = function(w) LifeTable(x = x, dx = w, ...),
    lx = function(w) LifeTable(x = x, lx = w, ...)
    )

  if (is.vector(data)) {
    if (length(x) != length(data)) {
      stop("The 'x' and 'data' do not have the same length", call. = FALSE)
    }

    out <- LT(data)$lt[, to]
    names(out) <- names(data)

  } else {
    if (length(x) != nrow(data)) {
      stop("The length of 'x' must be equal to the number of rows in 'data'",
           call. = FALSE)
    }

    out <- convert_fx_matrix(x = x, data = data, from = from, to = to,
                             LT = LT, lx0 = lx0)
  }

  return(out)
}


#' Convert a Matrix of Life Table Indicators Column by Column
#'
#' Internal workhorse of \code{\link{convertFx}} for matrix and data frame
#' input. It uses the life-table primitives directly when the requested
#' \code{from}-\code{to} pair is a single identity and falls back on one call
#' to \code{\link{LifeTable}} for the whole matrix otherwise.
#'
#' @param x Numeric vector of ages.
#' @param data A numeric matrix or data frame, one column per life table.
#' @param from The type of indicator supplied in \code{data}.
#' @param to The desired output indicator.
#' @param LT A function calling \code{\link{LifeTable}} with the argument
#' named after \code{from}.
#' @param lx0 The radix, or \code{NULL} to use the \code{\link{LifeTable}}
#' default.
#' @return A numeric matrix with the converted indicator.
#' @noRd
convert_fx_matrix <- function(x, data, from, to, LT, lx0) {

  M    <- as.matrix(data)
  N    <- length(x)
  nx   <- c(diff(x), diff(x)[N - 1])
  case <- paste0(from, "_to_", to)
  # A one-step identity needs a finite input and at least two age intervals.
  ok   <- length(nx) == N && all(is.finite(M))
  one  <- switch(
    case,
    mx_to_qx = ok && all(M > 0),
    qx_to_mx = ok && all(M > 0) && all(M <= 1) && all(M[N, ] == 1),
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
    out <- matrix(LT(M)$lt[, to], nrow = N)
  }

  dimnames(out) <- dimnames(data)
  return(out)
}
