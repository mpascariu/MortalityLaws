# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-04 22:59:32
# --------------------------------------------

#' Compute Life Tables from Mortality Data
#'
#' Construct either a full (single-year age intervals) or an abridged 
#' (wider age intervals) life table from a variety of input data types. 
#' The function accepts:
#' \itemize{
#'   \item Death counts and mid-interval population estimates (\code{Dx, Ex})
#'   \item Age-specific death rates (\code{mx})
#'   \item Death probabilities (\code{qx})
#'   \item Survivorship curve (\code{lx})
#'   \item Distribution of deaths (\code{dx})
#' }
#' Exactly one of these input options must be provided; supplying more 
#' than one is an error. The input can be a numeric \code{vector}, 
#' \code{matrix}, or \code{data.frame}. When a \code{matrix} or 
#' \code{data.frame} with multiple columns is supplied, the function 
#' computes one life table per column.
#'
#' @details
#' A life table (also called a mortality table or actuarial table) 
#' summarises the mortality experience of a population. For each age 
#' (or age interval) it reports:
#' \itemize{
#'   \item Death rates (\code{mx}) and death probabilities (\code{qx})
#'   \item Survivorship (\code{lx})
#'   \item Distribution of deaths (\code{dx})
#'   \item Person-years lived (\code{Lx}) and total person-years remaining 
#'         (\code{Tx})
#'   \item Life expectancy (\code{ex})
#' }
#' The life table is constructed sequentially: from the input data the 
#' function derives \code{mx}, then \code{qx}, then \code{lx}, \code{dx}, 
#' \code{Lx}, \code{Tx}, and finally \code{ex}. The constant-force-of-mortality 
#' (CFM) assumption is used to convert between \code{mx} and \code{qx}. 
#' If the \code{sex} argument is supplied, the first two values of the 
#' \code{ax} column are adjusted using the Coale-Demeny method, which 
#' accounts for the different infant mortality patterns between males 
#' and females. Two published parameterisations of that adjustment are 
#' available through \code{ax_method}: the default one, expressed in 
#' terms of \code{mx}, and the original one, expressed in terms of 
#' \code{qx} and retrieved from \code{mx} by the PAS inversion. The 
#' second reproduces the coefficients used by the Coale-Demeny 1983 
#' regional model tables and by the PAS software; the two differ by a 
#' few thousandths of a year in the first two intervals when infant 
#' mortality is low.
#'
#' @references
#' Coale, A. J., Demeny, P., and Vaughan, B. (1983). \emph{Regional 
#' Model Life Tables and Stable Populations}. 2nd ed. New York: 
#' Academic Press.
#'
#' Preston, S. H., Heuveline, P., and Guillot, M. (2001). 
#' \emph{Demography: Measuring and Modeling Population Processes}. 
#' Oxford: Blackwell Publishers.
#'
#' When \code{ax} is supplied by the user the conversion between 
#' \code{mx} and \code{qx} uses the exact interval identity 
#' \code{qx = nx * mx / (1 + (nx - ax) * mx)} (and its inverse) instead 
#' of the CFM approximation, so that the \code{mx}, \code{qx} and 
#' \code{ax} columns of the result are mutually consistent. An \code{ax} 
#' that an interval cannot support (\code{ax * mx > 1}) is replaced with 
#' the implied average, \code{1/mx}, and the affected ages are reported.
#'
#' The open (closing) age interval follows its own rule: 
#' \code{ax[N] = 1/mx[N]}, \code{ex[N] = 1/mx[N]} and 
#' \code{Lx[N] = lx[N]/mx[N]}, which keeps the closed table consistent 
#' with \code{qx[N] = 1}. A user-supplied \code{ax[N]} is therefore 
#' replaced, with a warning.
#'
#' Missing values in \code{mx} or \code{qx} are never masked. They 
#' localise to the interval quantities of the affected row (which are 
#' returned as \code{NA}) and to the cumulative \code{Tx} and \code{ex} 
#' columns at and before that row, while the survivorship chain 
#' \code{lx} is bridged across the gap, so the ages above it remain 
#' computable. A warning names the affected ages.
#'
#' @usage
#' LifeTable(x, Dx = NULL, Ex = NULL,
#'              mx = NULL,
#'              qx = NULL,
#'              lx = NULL,
#'              dx = NULL,
#'              sex = NULL,
#'              lx0 = 1e5,
#'              ax  = NULL,
#'              ax_method = c("preston", "coale_demeny"))
#'
#' @param x Numeric vector of ages at the beginning of each age interval. 
#'   For a full life table, use single-year ages (e.g., \code{0:110}). 
#'   For an abridged life table, use the lower bound of each interval 
#'   (e.g., \code{c(0, 1, 5, 10, ..., 110)}).
#'
#' @param Dx Death counts. Each element represents the total number of 
#'   deaths during the calendar year to persons aged \code{x} to 
#'   \code{x + n} (where \code{n} is the length of the age interval). 
#'   Must be provided together with \code{Ex}.
#'
#' @param Ex Exposure-to-risk in the period. This is usually approximated 
#'   by the mid-year population aged \code{x} to \code{x + n}. Must be 
#'   provided together with \code{Dx}.
#'
#' @param mx Age-specific death rate in the age interval \code{[x, x+n)}. 
#'   Defined as \code{Dx / Ex}.
#'
#' @param qx Probability of dying within the age interval \code{[x, x+n)}.
#'
#' @param lx Probability of surviving to exact age \code{x} (if \code{lx0 = 1}), 
#'   or the number of survivors at exact age \code{x} (if \code{lx0 > 1}). 
#'   When \code{lx} is the sole input, the values are re-scaled to the 
#'   chosen radix \code{lx0}.
#'
#' @param dx Number of deaths in the life-table population occurring in 
#'   the age interval \code{[x, x+n)}. When \code{dx} is the sole input, 
#'   the values are re-scaled to sum to \code{lx0}.
#'
#' @param sex Sex of the population. Options are \code{NULL} (default), 
#'   \code{"male"}, \code{"female"}, or \code{"total"}. When specified, 
#'   the first two entries of the \code{ax} column are adjusted using 
#'   Coale-Demeny coefficients, producing more accurate life-table values 
#'   at the youngest ages. The adjustment differs slightly between males 
#'   and females.
#'
#' @param lx0 Radix, the starting population (or probability scale) at 
#'   age 0. Default is \code{100,000}. All subsequent life-table columns 
#'   (\code{lx}, \code{dx}, \code{Lx}, \code{Tx}) are scaled accordingly.
#'
#' @param ax Numeric vector representing the average number of person-years 
#'   lived in the age interval by those who die in that interval. If 
#'   \code{NULL} (the default), \code{ax} is estimated internally using 
#'   a standard formula. You may supply a single value (applied to all 
#'   intervals) or a vector of the same length as \code{x}. A common 
#'   assumption is \code{ax = 0.5}, which places deaths at the midpoint 
#'   of each interval. The value supplied for the open age interval is 
#'   ignored and replaced with \code{1/mx}; see \code{Details}.
#'
#' @param ax_method The published parameterisation used to adjust the 
#'   first two values of \code{ax} when \code{sex} is given. 
#'   \code{"preston"} (the default) uses the coefficients of Preston, 
#'   Heuveline and Guillot (2001), table 3.3, which are expressed in 
#'   terms of \code{mx}. \code{"coale_demeny"} uses the original 1983 
#'   Coale-Demeny rule, expressed in terms of \code{qx} and reproduced 
#'   by the PAS software. The two agree exactly once \code{mx[1]} 
#'   reaches 0.107 and differ by at most a few thousandths of a year 
#'   below it. Ignored when \code{sex = NULL} or when \code{ax} is 
#'   supplied.
#'
#' @return An object of class \code{"LifeTable"} containing the following 
#'   components:
#'   \item{lt}{A \code{data.frame} with the complete life table, including 
#'     columns for age interval (\code{x.int}), exact age (\code{x}), 
#'     death rate (\code{mx}), death probability (\code{qx}), person-years 
#'     lived by decedents (\code{ax}), survivorship (\code{lx}), death 
#'     distribution (\code{dx}), person-years lived (\code{Lx}), total 
#'     person-years remaining (\code{Tx}), and life expectancy (\code{ex}).}
#'   \item{call}{The matched function call.}
#'   \item{process_date}{Timestamp of when the life table was computed.}
#'
#' @seealso
#' \code{\link{LawTable}} for generating life tables from a fitted 
#'   parametric mortality law; 
#'   \code{\link{convertFx}} for converting between mortality measures.
#'
#' @author Marius D. Pascariu
#'
#' @examples
#' # Example 1 --- Full life tables with different inputs ------------
#'
#' y  <- 1900
#' x  <- as.numeric(rownames(ahmd$mx))
#' Dx <- ahmd$Dx[, paste(y)]
#' Ex <- ahmd$Ex[, paste(y)]
#'
#' LT1 <- LifeTable(x, Dx = Dx, Ex = Ex)
#' LT2 <- LifeTable(x, mx = LT1$lt$mx)
#' LT3 <- LifeTable(x, qx = LT1$lt$qx)
#' LT4 <- LifeTable(x, lx = LT1$lt$lx)
#' LT5 <- LifeTable(x, dx = LT1$lt$dx)
#'
#' LT1
#' LT5
#' ls(LT5)
#'
#' # Example 2 --- Compute multiple life tables at once ------------
#'
#' LTs <- LifeTable(x, mx = ahmd$mx)
#' LTs
#' # A warning is printed if the input contains missing values.
#' # Some of the missing values can be handled automatically.
#'
#' # Example 3 --- Abridged life table -----------------------------
#'
#' x  <- c(0, 1, seq(5, 110, by = 5))
#' mx <- c(.053, .005, .001, .0012, .0018, .002, .003, .004,
#'         .004, .005, .006, .0093, .0129, .019, .031, .049,
#'         .084, .129, .180, .2354, .3085, .390, .478, .551)
#' LT6 <- LifeTable(x, mx = mx, sex = "female")
#' LT6
#'
#' # Example 4 --- Abridged life table using a custom 'ax' --------
#' # This example reuses the ages (x) and death rates (mx) from Example 3.
#' # Note that 'ax' must have the same length as 'x', otherwise an error
#' # will be returned.
#'
#' my_ax <- c(0.1, 1.5, rep(2, 19), 1, 1, 1)
#'
#' LT7 <- LifeTable(x = x, mx = mx, ax = my_ax)
#'
#' # Example 5 --- The two Coale-Demeny parameterisations ---------
#' # The default ('preston') uses the coefficients expressed in m0.
#' # 'coale_demeny' uses the original q0-based rule; the two differ only
#' # below m0 = 0.107 (here m0 = 0.053) and converge above it.
#'
#' LT8  <- LifeTable(x, mx = mx, sex = "female")
#' LT9  <- LifeTable(x, mx = mx, sex = "female", ax_method = "coale_demeny")
#' LT8$lt$ax[1:2]
#' LT9$lt$ax[1:2]
#'
#' @export
LifeTable <- function(x,
                      Dx = NULL,
                      Ex = NULL,
                      mx = NULL,
                      qx = NULL,
                      lx = NULL,
                      dx = NULL,
                      sex = NULL,
                      lx0 = 1e5,
                      ax = NULL,
                      ax_method = c("preston", "coale_demeny")){

  ax_method <- match.arg(ax_method)
  input <- c(as.list(environment()))
  X     <- check_life_table_input(input)
  x     <- X$x
  x.int <- paste0("[", x, ",", c(x[-1], "+"), ")")

  if (any(X$iclass == "numeric")) {
    LT <- compute_life_table(x = x,
                         Dx = X$Dx,
                         Ex = X$Ex,
                         mx = X$mx,
                         qx = X$qx,
                         lx = X$lx,
                         dx = X$dx,
                         sex = X$sex,
                         lx0 = X$lx0,
                         ax = X$ax,
                         ax_method = X$ax_method,
                         case = X$case,
                         x.int = x.int)

  } else {
    nm <- X$LTnames
    LT <- vector(mode = "list", length = X$nLT)

    for (i in seq_len(X$nLT)) {
      LTi <- compute_life_table(x = x,
                            Dx = X$Dx[, i],
                            Ex = X$Ex[, i],
                            mx = X$mx[, i],
                            qx = X$qx[, i],
                            lx = X$lx[, i],
                            dx = X$dx[, i],
                            sex = X$sex,
                            lx0 = X$lx0,
                            ax = X$ax,
                            ax_method = X$ax_method,
                            case = X$case,
                            x.int = x.int)

      LTn     <- if (is.null(nm) || is.na(nm[i])) i else nm[i]
      LT[[i]] <- cbind(LT = LTn, LTi)
    }

    LT <- do.call(rbind, LT)
  }

  # Exit
  out <- list(
    lt = LT,
    call = match.call(),
    process_date = date()
    )
  out <- structure(class = "LifeTable", out)
  return(out)
}


#' Compute a single life table
#'
#' Resolves the input case into the canonical mx/qx pair, builds the
#' survivorship chain and derives the remaining life-table columns. The
#' case and the age-interval labels can be supplied by \code{LifeTable}
#' to avoid recomputing them for every column.
#' @noRd
compute_life_table <- function(x,
                           Dx = NULL,
                           Ex = NULL,
                           mx = NULL,
                           qx = NULL,
                           lx = NULL,
                           dx = NULL,
                           sex = NULL,
                           lx0 = 1e5,
                           ax = NULL,
                           ax_method = "preston",
                           case = NULL,
                           x.int = NULL) {

  if (is.null(case)) {
    case <- detect_case(Dx = Dx, Ex = Ex, mx = mx, qx = qx,
                         lx = lx, dx = dx)$case
  }

  if (is.null(x.int)) {
    x.int <- paste0("[", x, ",", c(x[-1], "+"), ")")
  }

  N       <- length(x)
  df      <- diff(x)
  nx      <- c(df, df[N - 1])
  user_ax <- !is.null(ax)

  if (user_ax && length(ax) == 1) {
    ax <- rep(ax, N)
  }

  R <- lt_case_rates(case = case, x = x, nx = nx, Dx = Dx, Ex = Ex,
                     mx = mx, qx = qx, lx = lx, dx = dx, lx0 = lx0,
                     ax = ax)

  if (user_ax) {
    ax <- R$ax
  }

  mx <- repair_mx(mx = R$mx, nx = nx)
  qx <- R$qx

  if (is.null(R$lx)) {
    L <- lx_dx(qx = qx, lx0 = lx0)

  } else {
    L <- list(lx = R$lx, dx = R$dx)
  }

  lx <- L$lx
  dx <- L$dx

  # A missing rate leaves the deaths of its interval unknown: that row and
  # every younger age become unknown in the cumulative columns, while the
  # ages above it remain computable.
  if (any(R$miss)) {
    dx[R$miss] <- NA
  }

  ax <- lt_ax(x = x, ax = ax, mx = mx, qx = qx, nx = nx, sex = sex,
              user = user_ax, ax_method = ax_method)

  C <- lt_columns(mx = mx, ax = ax, lx = lx, dx = dx, nx = nx)

  out <- data.frame(x.int = x.int,
                    x = x,
                    mx = mx,
                    qx = qx,
                    ax = ax,
                    lx = lx,
                    dx = dx,
                    Lx = C$Lx,
                    Tx = C$Tx,
                    ex = C$ex)

  if (lt_degenerate(mx = mx)) {
    out[, !names(out) %in% c("x.int", "x")] <- NA
  }

  return(out)
}


#' Assign the average person-years lived in each age interval
#'
#' Derives ax from the rates when it is not supplied by the user, adjusts
#' the first two intervals with the Coale-Demeny coefficients when a sex is
#' given, and applies the rule of the open age interval.
#' @noRd
lt_ax <- function(x, ax, mx, qx, nx, sex, user = FALSE,
                  ax_method = "preston") {

  if (!user) {
    ax <- compute_ax(x = x, mx = mx, qx = qx)

    if (!is.null(sex)) {
      ax <- coale_demeny_ax(x = x, mx = mx, ax = ax, sex = sex,
                            method = ax_method)
    }
  }

  ax <- lt_open_ax(x = x, ax = ax, mx = mx, nx = nx, warn = user)
  return(ax)
}


#' Derive the person-years, total person-years and life expectancy columns
#'
#' Keeps the table closed in the open age interval (Lx = lx/mx, ex = ax)
#' and assigns zero person-years and zero life expectancy where the table
#' has already closed (lx = 0).
#' @noRd
lt_columns <- function(mx, ax, lx, dx, nx) {
  N  <- length(lx)
  Lx <- nx * lx - (nx - ax) * dx
  Lx[N] <- if (is.finite(mx[N]) && mx[N] > 0) lx[N]/mx[N] else ax[N] * dx[N]

  closed <- !is.na(lx) & lx == 0
  Lx[closed] <- 0

  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx/lx
  ex[N] <- ax[N]
  ex[closed] <- 0

  out <- list(Lx = Lx, Tx = Tx, ex = ex)
  return(out)
}


#' Detect a vector of rates carrying no information at all
#'
#' Returns \code{TRUE} when every rate is missing or non-finite, or when
#' all of them are zero.
#' @noRd
lt_degenerate <- function(mx) {
  out <- all(is.na(mx)) || all(is.nan(mx)) || all(is.infinite(mx)) ||
    (!any(is.na(mx)) && all(mx == 0))
  return(out)
}


#' Resolve the input case into canonical life-table vectors
#'
#' Converts any of the five accepted inputs into mortality rates (mx) and
#' death probabilities (qx) and, for the survivorship and death
#' distribution inputs, into the corresponding lx and dx columns. The
#' identity between mx and qx uses the supplied ax when available and the
#' constant force of mortality assumption otherwise. An ax that the
#' interval cannot support is capped, and rows holding a missing input are
#' flagged so that the caller can propagate them.
#' @noRd
lt_case_rates <- function(case, x, nx, Dx, Ex, mx, qx, lx, dx, lx0, ax) {

  miss <- rep(FALSE, length(x))

  if (case == "C1_DxEx") {
    mx <- as.numeric(Dx)/as.numeric(Ex)
    mx <- repair_above_omega(x = x, ux = mx)
    ax <- lt_feasible_ax(x = x, ax = ax, mx = mx)
    qx <- mx_qx(x = x, nx = nx, ux = mx, out = "qx", ax = ax)
  }

  if (case == "C2_mx") {
    mx   <- as.numeric(mx)
    miss <- is.na(mx)
    mx   <- repair_mx(mx = mx, nx = nx)
    ax   <- lt_feasible_ax(x = x, ax = ax, mx = mx)
    qx   <- mx_qx(x = x, nx = nx, ux = mx, out = "qx", ax = ax)
  }

  if (case == "C3_qx") {
    miss <- is.na(qx)
    qx   <- as.numeric(qx)
    mx   <- mx_qx(x = x, nx = nx, ux = qx, out = "mx", ax = ax)
    ax   <- lt_feasible_ax(x = x, ax = ax, mx = mx)
    qx   <- lt_close_qx(qx = qx)
  }

  if (case == "C4_lx") {
    miss <- is.na(lx)
    lx   <- as.numeric(lx)
    lx   <- lx * lx0/lx[1]
    dx   <- dx_lx(ux = lx, out = "dx")
    qx   <- lt_snap_qx(lt_qx(dx = dx, lx = lx))
    mx   <- mx_qx(x = x, nx = nx, ux = qx, out = "mx", ax = ax)
    ax   <- lt_feasible_ax(x = x, ax = ax, mx = mx)
  }

  if (case == "C5_dx") {
    miss <- is.na(dx)
    dx   <- as.numeric(dx)
    dx   <- dx * lx0/sum(dx)
    lx   <- dx_lx(ux = dx, out = "lx")
    qx   <- lt_snap_qx(lt_qx(dx = dx, lx = lx))
    mx   <- mx_qx(x = x, nx = nx, ux = qx, out = "mx", ax = ax)
    ax   <- lt_feasible_ax(x = x, ax = ax, mx = mx)
  }

  out <- list(mx = mx, qx = qx, lx = lx, dx = dx, ax = ax, miss = miss)
  return(out)
}

#' Print LifeTable
#' @param x An object of class \code{"LifeTable"}
#' @param ... Further arguments passed to or from other methods.
#' @return Print data on the console
#' @keywords internal
#' @export
print.LifeTable <- function(x, ...){

  LT <- x$lt
  lt <- with(
    LT,
    data.frame(
      x.int = x.int,
      x = x,
      mx = round(mx, 6),
      qx = round(qx, 6),
      ax = round(ax, 2),
      lx = round(lx),
      dx = round(dx),
      Lx = round(Lx),
      Tx = round(Tx),
      ex = round(ex, 2)
      )
    )

  if (colnames(LT)[1] == "LT") lt <- data.frame(LT = LT$LT, lt)
  dimnames(lt) <- dimnames(LT)
  nx    <- length(unique(LT$x))
  nlt   <- nrow(LT) / nx
  out   <- head_tail(lt, hlength = 6, tlength = 3, ...)
  step  <- diff(LT$x)
  step  <- step[step > 0]
  type1 <- if (all(step == 1)) "Full" else "Abridged"
  type2 <- if (nlt == 1) "Life Table" else "Life Tables"

  cat("\n", type1, " ", type2, "\n\n", sep = "")
  cat("Number of life tables:", nlt, "\n")
  cat("Dimension:", nrow(LT), "x", ncol(LT), "\n")
  cat("Age intervals:", head_tail(lt$x.int, hlength = 3, tlength = 3), "\n\n")
  print(out, row.names = FALSE)
}
