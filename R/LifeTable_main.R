# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-06 21:03:27
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
#'   \item Remaining life expectancy (\code{ex})
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
#' \code{Lx}, \code{Tx}, and finally \code{ex}. The conversion between 
#' \code{mx} and \code{qx} follows the \code{ax} method. The default, 
#' \code{ax = "andreev_kingkade"}, is the rule the Human Mortality Database 
#' applies to its period life tables (Methods Protocol, version 6): the 
#' Andreev-Kingkade (2015) formula for the first year of life and half the 
#' interval width elsewhere, converted through the exact 
#' interval identity so that the \code{mx}, \code{qx} and \code{ax} columns 
#' are mutually consistent. The constant-force-of-mortality (CFM) 
#' assumption, \code{ax = n + 1/m - n/q}, is available with \code{ax = 
#' "cfm"}. When \code{sex} is given, the first two values of the \code{ax} 
#' column can instead be adjusted using the Coale-Demeny method, which 
#' accounts for the different infant mortality patterns between males and 
#' females; two published parameterisations of that adjustment are available 
#' through \code{ax = "preston"}, expressed in terms of \code{mx}, and 
#' \code{ax = "coale_demeny"}, expressed in terms of \code{qx} and retrieved 
#' from \code{mx} by the PAS inversion. The latter reproduces the 
#' coefficients used by the Coale-Demeny 1983 regional model tables and by 
#' the PAS software.
#'
#' The \code{ex} input runs the table backwards. The survivorship ratio of 
#' each closed interval follows from the identity
#' \deqn{e_x l_x - e_{x+n} l_{x+n} = nL_x = a_x l_x + (n - a_x) l_{x+n}}, so
#' that \eqn{r_x = l_{x+n}/l_x = (e_x - a_x)/(e_{x+n} + n - a_x)}. When 
#' \code{ax} is supplied as a numeric vector the recovery is a single sweep. 
#' When it is not, each interval is solved so that the ax the method in force 
#' would assign to the recovered interval reproduces the ratio it came from; 
#' the recovered table is then identical to the one the same \code{ex} curve 
#' describes under that method. The open interval closes by the standard rule, 
#' \eqn{a_N = e_N} and \eqn{m_N = 1/e_N}. A curve of life expectancy is 
#' allowed to rise with age at the youngest ages (life expectancy at birth is 
#' lower than at age one when infant mortality is high), so a rise is not an 
#' error; a curve that falls by more than the interval width, or below its own 
#' \eqn{a_x}, is not a feasible life table and is reported by age. Because 
#' \eqn{e_x} alone does not pin \eqn{a_x}, pass \code{ax} alongside \code{ex} 
#' when the ax convention matters. One nuance follows from the forward build: 
#' when \code{ax} is \code{"preston"} or \code{"coale_demeny"} and a \code{sex} 
#' is given, the forward table adjusts \code{ax} in the first two intervals 
#' after converting the rates, so its stored \code{mx} there does not satisfy 
#' the exact identity on its own \code{ax}. The inverse returns the 
#' identity-consistent \code{mx}, so in that one family the recovered 
#' \code{mx} in the first two rows can differ from the forward table's at the 
#' third decimal while \code{ex}, \code{qx} and \code{ax} still reproduce 
#' exactly.
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
#' When \code{ax} is supplied by the user as a numeric vector the conversion 
#' between \code{mx} and \code{qx} uses the exact interval identity 
#' \code{qx = nx * mx / (1 + (nx - ax) * mx)} (and its inverse) instead 
#' of the CFM approximation, so that the \code{mx}, \code{qx} and 
#' \code{ax} columns of the result are mutually consistent. The 
#' \code{"andreev_kingkade"} method does the same, because it builds a 
#' numeric \code{ax} before the conversion. An \code{ax} 
#' that an interval cannot support (\code{ax * mx > 1}) is replaced with 
#' the implied average, \code{1/mx}, and the affected ages are reported.
#'
#' The open (closing) age interval follows its own rule: 
#' \code{ax[N] = 1/mx[N]}, \code{ex[N] = 1/mx[N]} and 
#' \code{Lx[N] = lx[N]/mx[N]}, which keeps the closed table consistent 
#' with \code{qx[N] = 1}. A user-supplied \code{ax[N]} is therefore 
#' replaced, with a warning.
#'
#' That rule assumes the force of mortality is roughly constant above the 
#' open age, which holds when the open interval is old (85+ or 90+) but 
#' not when the input stops early (70+ or 75+). The \code{close} argument 
#' addresses that directly: given a mortality-law code, the observed 
#' \code{mx[N]} is replaced by the model-implied average over the open 
#' interval, \code{1/ex[N]}, without changing the age grid. The 
#' \code{omega} argument is a different response to the same problem: it 
#' extends the table to \code{omega} by extrapolating the rates and closes 
#' there. Either argument alone leaves the other's default in place, and 
#' the default (\code{close = NULL}, \code{omega = NULL}) is the standard 
#' reciprocal close, so results are unchanged.
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
#'              ex = NULL,
#'              sex = NULL,
#'              lx0 = 1e5,
#'              ax  = "andreev_kingkade",
#'              close = NULL,
#'              omega = NULL,
#'              fit_from = NULL)
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
#' @param ex Remaining life expectancy at age \code{x}, in years. When 
#'   \code{ex} is the sole input the function builds the life table that 
#'   reproduces the supplied curve: the survivorship, death probabilities and 
#'   death rates are recovered from \code{ex} and the \code{ax} convention 
#'   (see \code{Details}). A curve that rises with age at the youngest ages is 
#'   allowed; a missing value anywhere in the curve, or a curve that is not a 
#'   feasible life table (falling faster than the interval width, or below its 
#'   own \code{ax}), is an error naming the affected ages. A missing 
#'   \code{ex} is never repaired the way a missing rate is.
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
#' @param ax The average number of person-years lived in each age interval
#'   by those who die in it, given either as values or as the method that
#'   produces them. Accepts two forms: 
#'   \itemize{ 
#'   \item a numeric scalar or vector: a scalar is applied to all intervals, 
#'         a vector must have the same length as \code{x}. A common 
#'         assumption is \code{ax = 0.5}, which places deaths at the 
#'         midpoint of each interval; 
#'   \item a method name, one of \code{"andreev_kingkade"} (the default), 
#'         \code{"cfm"}, \code{"preston"} or \code{"coale_demeny"} (see 
#'         below). 
#'   } 
#' 
#'   \code{"andreev_kingkade"} follows the rule the Human Mortality Database
#'   applies to its period life tables (Methods Protocol, version 6, section
#'   7.1): the Andreev-Kingkade (2015) formula sets the value for the first
#'   year of life from \code{m0}, every other closed interval uses half its
#'   length (\code{n/2}), and the open interval uses
#'   \code{1/mx}. It is the most accurate of the four methods and the one
#'   that reproduces the published HMD life tables most closely. Because it
#'   produces a numeric \code{ax} before the rates are converted, the
#'   \code{mx} to \code{qx} step uses the exact identity
#'   \code{qx = n*mx/(1 + (n - ax)*mx)}, which is the identity the protocol
#'   uses (equation 74). The Andreev-Kingkade value is a property of a
#'   one-year first interval that starts at birth, so when the first interval
#'   is wider or the table starts above age 0 the interval keeps the ordinary
#'   midpoint value instead.
#' 
#'   The other three methods share the same basis, the standard lifetable 
#'   identity \code{ax = n + 1/m - n/q} under a constant force of mortality 
#'   (Preston, Heuveline and Guillot 2001, eq. 3.15), and differ in how 
#'   they treat the first two intervals: 
#'   \itemize{ 
#'   \item \code{"cfm"}: the identity alone, for every interval; 
#'   \item \code{"preston"}: the identity, with the first two intervals 
#'         replaced by the Coale-Demeny West separation factors published 
#'         by Preston et al. (2001), table 3.3, when \code{sex} is given; 
#'   \item \code{"coale_demeny"}: the identity, with the first two intervals 
#'         replaced by the original 1983 Coale-Demeny rule expressed in 
#'         \code{qx}, reproduced by the PAS software, when \code{sex} is 
#'         given. 
#'   } 
#'   \code{"preston"} and \code{"coale_demeny"} agree exactly once 
#'   \code{mx[1]} reaches 0.107, and differ by at most a few thousandths of 
#'   a year below it. Both coincide with \code{"cfm"} when \code{sex} is 
#'   \code{NULL}. \code{"cfm"} and the Coale-Demeny variants adjust
#'   \code{ax} after the rates have been converted, so they leave the
#'   constant force of mortality conversion in place; only
#'   \code{"andreev_kingkade"} changes the conversion itself.
#' 
#'   A value supplied for the open age interval is kept as given. Everybody
#'   alive there dies in that interval, so its own rate implies what the
#'   average time lived in it should be (\code{1/mx}); a value that differs
#'   is reported and left alone, which leaves the \code{ax} and \code{mx}
#'   columns of the table disagreeing at that age, or the model-implied
#'   value when \code{close} is set. The one exception is a table entered
#'   from \code{ex}, where the curve being inverted fixes that interval.
#'   See \code{Details}.
#'
#' @param close The method used to close the open age interval, named by 
#'   the mortality-law code that implements it. \code{NULL} (the default) 
#'   keeps the standard reciprocal close, \code{mx[N] = (the observed 
#'   rate)}. A code from \code{\link{availableLaws}} (e.g. \code{"kannisto"}) 
#'   closes the table instead with the model-implied average force of 
#'   mortality over the open interval: the law is fitted to the closed 
#'   intervals and the observed \code{mx[N]} is replaced by \code{1/ex[N]}. 
#'   This corrects the bias of the reciprocal close when the open interval 
#'   begins at a young age, and it does not change the age grid. The 
#'   standard row identities (\code{qx[N] = 1}, \code{ax[N] = ex[N] = 1/mx[N]}) 
#'   still hold on the corrected rate. The same code is used when 
#'   \code{omega} extends the table; when \code{omega} is set and \code{close} 
#'   is \code{NULL}, the extrapolation defaults to \code{"kannisto"}.
#'
#' @param omega The age at which to close the table when it should be 
#'   extended beyond the input's open age. \code{NULL} (the default) keeps 
#'   the table closed at the input's own open age. When \code{omega} is 
#'   greater than the last age in \code{x}, the death rates from the open 
#'   age up to \code{omega} are obtained by extrapolating the \code{close} 
#'   law (see also \code{fit_from}) and the table is closed at \code{omega}. 
#'   The input's own open interval is replaced by the extrapolated values, 
#'   so a user-supplied \code{ax} is re-estimated on the extended grid. An 
#'   \code{omega} that does not exceed the last age in \code{x} leaves the 
#'   table unchanged, with a warning. Ignored by the in-place close, which 
#'   operates on the input's own open age.
#'
#' @param fit_from The age from which the closing law is fitted. 
#'   \code{NULL} (the default) uses 60 when the input reaches age 85, and 
#'   the last 20 years of the input otherwise. Ignored when neither 
#'   \code{close} nor \code{omega} is set.
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
#' @example inst/examples/LifeTable.R
#' @export
LifeTable <- function(x,
                      Dx = NULL,
                      Ex = NULL,
                      mx = NULL,
                      qx = NULL,
                      lx = NULL,
                      dx = NULL,
                      ex = NULL,
                      sex = NULL,
                      lx0 = 1e5,
                      ax  = "andreev_kingkade",
                      close = NULL,
                      omega = NULL,
                      fit_from = NULL){

  A         <- check_ax(x = x, ax = ax)
  ax        <- A$ax
  ax_method <- A$ax_method
  close     <- check_close(close)
  omega     <- check_omega(x = x, omega = omega)
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
                         ex = X$ex,
                         sex = X$sex,
                         lx0 = X$lx0,
                         ax = X$ax,
                         ax_method = ax_method,
                         close = close,
                         omega = omega,
                         fit_from = fit_from,
                         case = X$case,
                         x.int = if (is.null(omega)) x.int else NULL)

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
                            ex = X$ex[, i],
                            sex = X$sex,
                            lx0 = X$lx0,
                            ax = X$ax,
                            ax_method = ax_method,
                            close = close,
                            omega = omega,
                            fit_from = fit_from,
                            case = X$case,
                            x.int = if (is.null(omega)) x.int else NULL)

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
#' @inheritParams LifeTable
#' @param ax The \code{ax} argument already resolved to a numeric vector, or
#'   \code{NULL} when it still has to be derived.
#' @param ax_method The resolved ax method name, see \code{\link{LifeTable}}.
#' @param case The problem case, one of \code{C1_DxEx}, \code{C2_mx},
#'   \code{C3_qx}, \code{C4_lx}, \code{C5_dx}, \code{C6_ex}, or \code{NULL}
#'   to detect it from the data.
#' @param x.int Optional character vector of interval labels; computed when
#'   \code{NULL}.
#' @return A \code{data.frame} with one row per age and the columns
#'   \code{x.int}, \code{x}, \code{mx}, \code{qx}, \code{ax}, \code{lx},
#'   \code{dx}, \code{Lx}, \code{Tx} and \code{ex}.
#' @noRd
compute_life_table <- function(x,
                           Dx = NULL,
                           Ex = NULL,
                           mx = NULL,
                           qx = NULL,
                           lx = NULL,
                           dx = NULL,
                           ex = NULL,
                           sex = NULL,
                           lx0 = 1e5,
                           ax = NULL,
                           ax_method = "cfm",
                           close = NULL,
                           omega = NULL,
                           fit_from = NULL,
                           case = NULL,
                           x.int = NULL) {

  if (is.null(case)) {
    case <- detect_case(Dx = Dx, Ex = Ex, mx = mx, qx = qx,
                         lx = lx, dx = dx, ex = ex)$case
  }

  N       <- length(x)
  df      <- diff(x)
  nx      <- c(df, df[N - 1])
  user_ax <- !is.null(ax)
  # A supplied open-interval ax is the caller's to keep, except where it is
  # derived rather than chosen: an ax method (those arrive as a name, so
  # user_ax is TRUE for them too) and a table entered from ex, where the
  # open interval's ax is ex there by definition.
  keep_ax <- user_ax && case != "C6_ex"

  if (user_ax && length(ax) == 1) {
    ax <- rep(ax, N)
  }

  # The ex case is an inverse problem: the survivorship has to be recovered
  # from the curve of life expectancy before the usual pipeline can run. The
  # inverse resolves its own rates and ax, then joins the standard machinery
  # below as a rate case.
  ex_case <- case == "C6_ex"

  if (ex_case) {
    ex_use <- lt_repair_ex(x = x, ex = ex)

    E2 <- ex_inverse(x = x, nx = nx, ex = ex_use,
                     ax = if (user_ax) ax else NULL,
                     sex = sex, ax_method = ax_method)

    mx      <- E2$mx
    qx      <- E2$qx
    ax      <- E2$ax
    user_ax <- TRUE
    case    <- "C2_mx"
  }

  # Closing methods (issue #8). Both are opt-in responses to the same problem:
  # the reciprocal 1/mx close is biased when the open interval starts young.
  #   - close = "<law>" corrects mx[N] in place, on the same age grid;
  #   - omega = <age>    extends the rates to omega and closes there.
  # 'close' names the law for either; the extension defaults to Kannisto when
  # 'close' is NULL, while the in-place close is then simply not requested.
  law_use <- if (!is.null(close)) close else if (!is.null(omega)) "kannisto"

  # Extension of the open interval: resolve the input to rates, extrapolate
  # them to omega with a mortality law, and rebuild the table on the extended
  # grid as an mx case. A user-supplied ax cannot survive the grid change and
  # is re-estimated.
  if (!is.null(omega)) {
    mx0 <- lt_case_rates(case = case, x = x, nx = nx, Dx = Dx, Ex = Ex,
                         mx = mx, qx = qx, lx = lx, dx = dx, lx0 = lx0,
                         ax = ax, notice = FALSE)$mx
    mx0 <- repair_mx(mx = mx0, nx = nx)

    E <- lt_extend_omega(x = x, mx = mx0, omega = omega,
                         law = law_use, fit_from = fit_from)

    if (is.null(E)) {
      warning("The 'omega' extension could not be computed from the ",
              "supplied data; the life table closes at ", x[N], " as before.",
              call. = FALSE)

    } else {
      if (user_ax) {
        warning("'ax' is re-estimated on the extended age grid; the ",
                "supplied values no longer match the new intervals.",
                call. = FALSE)
      }

      x    <- E$x
      mx   <- E$mx
      Dx   <- Ex <- qx <- lx <- dx <- NULL
      ax   <- NULL
      case <- "C2_mx"
      N    <- length(x)
      df   <- diff(x)
      nx   <- c(df, df[N - 1])
      user_ax <- FALSE
      keep_ax <- FALSE
    }
  }

  if (is.null(x.int)) {
    x.int <- paste0("[", x, ",", c(x[-1], "+"), ")")
  }

  # The Andreev-Kingkade method is defined by its ax rule, and that rule has
  # to be in place before the rates are turned into probabilities, because
  # the method converts them through the exact identity (protocol eq. 74)
  # rather than the constant force of mortality approximation. Resolve the
  # rates once, build the ax vector from them, and keep it as a numeric user
  # ax so the rest of the pipeline treats it exactly like one.
  #
  # A table entered from probabilities (qx, lx, dx) recovers its rates
  # through that same identity, so the rate the ax rule needs depends on the
  # ax it is about to produce. The two are solved together by iteration,
  # which settles in two or three passes (tables entered from rates do not
  # move at all and stop after the second). The iteration is over the
  # interval probabilities, which are the smoothed, feasible quantities, so
  # it cannot wander.
  if (identical(ax_method, "andreev_kingkade") && !user_ax) {
    ax <- NULL

    for (k in seq_len(5)) {
      Rk  <- lt_case_rates(case = case, x = x, nx = nx, Dx = Dx, Ex = Ex,
                           mx = mx, qx = qx, lx = lx, dx = dx, lx0 = lx0,
                           ax = ax, notice = FALSE)
      axn <- hmd_ax_vector(x = x, mx = Rk$mx, qx = Rk$qx, sex = sex)

      moved <- is.null(ax) || max(abs(axn - ax)) > 1e-14
      ax    <- axn

      if (!moved) {
        break
      }
    }

    user_ax <- TRUE
  }

  R <- lt_case_rates(case = case, x = x, nx = nx, Dx = Dx, Ex = Ex,
                     mx = mx, qx = qx, lx = lx, dx = dx, lx0 = lx0,
                     ax = ax)

  if (user_ax) {
    ax <- R$ax
  }

  mx <- repair_mx(mx = R$mx, nx = nx)
  qx <- R$qx

  # In-place accurate close, on the input's own open age (when the table was
  # not extended, in which case the open age is already omega and the
  # reciprocal close is adequate there). The downstream identities
  # (qx = 1, ax = ex = 1/mx, Lx = lx/mx) hold on the corrected rate.
  if (!is.null(close) && is.null(omega) && !any(R$miss[N])) {
    mcl <- lt_close_model(x = x, mx = mx, law = law_use, fit_from = fit_from)

    if (is.null(mcl)) {
      warning("The closing law '", law_use, "' could not be fitted to the ",
              "supplied data; the open interval keeps its observed rate ",
              format(mx[N], digits = 4), ".", call. = FALSE)

    } else {
      mx[N]    <- mcl
      R$mx[N]  <- mcl
    }
  }

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
              user = user_ax, keep = keep_ax, ax_method = ax_method)

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
#' @inheritParams LifeTable
#' @param nx Numeric vector of interval widths, one per age.
#' @param user Logical; \code{TRUE} when \code{ax} was supplied or built by
#'   the caller, so it is kept rather than derived.
#' @param ax_method The ax method name, see \code{\link{LifeTable}}.
#' @param user Logical; TRUE keeps the \code{ax} given rather than deriving
#'   it from the rates.
#' @param keep Logical; report a supplied open-interval value that disagrees
#'   with the one its rate implies, and keep it. Kept apart from
#'   \code{user}: the ax methods and a table entered from \code{ex} adjust
#'   that interval silently, because there it is derived rather than chosen.
#' @return A numeric vector of the average person-years lived in each
#'   interval by those who die in it.
#' @noRd
lt_ax <- function(x, ax, mx, qx, nx, sex, user = FALSE, keep = FALSE,
                  ax_method = "preston") {

  if (!user) {
    ax <- compute_ax(x = x, mx = mx, qx = qx)

    if (!is.null(sex) && ax_method %in% c("preston", "coale_demeny")) {
      ax <- coale_demeny_ax(x = x, mx = mx, ax = ax, sex = sex,
                            method = ax_method)
    }
  }

  ax <- lt_open_ax(x = x, ax = ax, mx = mx, nx = nx, keep = keep)
  return(ax)
}


#' Derive the person-years, total person-years and life expectancy columns
#'
#' Keeps the table closed in the open age interval (Lx = lx/mx, ex = ax)
#' and assigns zero person-years and zero life expectancy where the table
#' has already closed (lx = 0).
#' @inheritParams LifeTable
#' @param nx Numeric vector of interval widths, one per age.
#' @return A list with \code{Lx} (person-years lived), \code{Tx} (total
#'   person-years remaining) and \code{ex} (life expectancy).
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
#' @inheritParams LifeTable
#' @return A single logical value.
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
#' @inheritParams LifeTable
#' @param case The problem case, see \code{compute_life_table}.
#' @param nx Numeric vector of interval widths, one per age.
#' @param ax The resolved numeric \code{ax} vector, or \code{NULL}.
#' @return A list with the canonical \code{mx}, \code{qx}, \code{lx},
#'   \code{dx} and \code{ax} vectors, plus \code{miss}, a logical vector
#'   flagging the rows that hold a missing input.
#' @noRd
lt_case_rates <- function(case, x, nx, Dx, Ex, mx, qx, lx, dx, lx0, ax,
                        notice = TRUE) {

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
    qx   <- lt_close_qx(qx = qx, notice = notice)
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

#' Print a Life Table
#'
#' Prints a life table in a readable form: a header with the type (full or
#' abridged), the number of tables and the age intervals, then the first and
#' the last rows of every column, with the middle rows elided.
#' @param x An object of class \code{"LifeTable"}.
#' @param ... Further arguments passed to or from other methods.
#' @return The object \code{x}, invisibly. Called for its printed output.
#' @seealso \code{\link{LifeTable}}.
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
