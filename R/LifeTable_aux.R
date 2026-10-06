# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-06 20:59:21
# --------------------------------------------

# Internal helpers behind LifeTable(): the mx/qx bridge, the ax
# estimators, the survivorship chain and the input validation.

#' Identify the input case to be solved
#'
#' Detects which of the five accepted inputs was supplied and returns the
#' case name together with the input class, the number of tables to
#' compute and the names to be assigned to them.
#' @inheritParams LifeTable
#' @return A list with \code{case}, the input class, the number of tables
#'   \code{nLT} and the names \code{LTnames} to be assigned to them.
#' @noRd
detect_case <- function(Dx = NULL,
                         Ex = NULL,
                         mx = NULL,
                         qx = NULL,
                         lx = NULL,
                         dx = NULL,
                         ex = NULL) {

  input   <- c(as.list(environment()))

  # Matrix of possible cases --------------------
  rn  <- c("C1_DxEx", "C2_mx", "C3_qx", "C4_lx", "C5_dx", "C6_ex")
  cn  <- c("Dx", "Ex", "mx", "qx", "lx", "dx", "ex")
  mat <- matrix(
    ncol = 7,
    byrow = TRUE,
    dimnames = list(rn, cn),
    data = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
             FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
             FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
             FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
             FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
             FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
    )
  # ----------------------------------------------
  L1 <- !unlist(lapply(input, is.null))
  L2 <- apply(mat, 1, function(x) all(L1 == x))
  my_case <- rn[L2]

  if (sum(L1[c(1, 2)]) == 1) {
    stop("If you input 'Dx' you must input 'Ex' as well, and vice versa",
         call. = FALSE)
  }

  if (!any(L2)) {
    stop("The input is not specified correctly. Check again the function ",
         "arguments and make sure the input data is added properly.",
         call. = FALSE)
  }

  X       <- input[L1][[1]]

  # A one-dimensional array is not detected with is.vector(), so it is
  # flattened to a vector before the number of tables is counted.
  if (length(dim(X)) == 1){
    X <- c(X)
  }

  nLT     <- 1
  LTnames <- NA

  if (length(dim(X)) == 2) {
    nLT     <- ncol(X)     # number of LTs to be created
    LTnames <- colnames(X) # the names to be assigned to LTs
  }

  # An integer vector is a valid numeric input
  iclass <- if (is.numeric(X) && is.null(dim(X))) "numeric" else class(X)

  out <- list(case = my_case,
              iclass = iclass,
              nLT = nLT,
              LTnames = LTnames)
  return(out)
}


#' Convert between mx and qx
#'
#' Converts a vector or a matrix of mortality rates into death probabilities
#' and back. A matrix is treated column by column: the closing rule, the
#' non-finite repair and the omega repair apply to the last age of every
#' column, never to the last element of the flattened matrix.
#' When the average person-years lived in the interval (ax) is supplied the
#' exact identities \code{qx = nx * mx / (1 + (nx - ax) * mx)} and 
#' \code{mx = qx / (ax * qx + nx * (1 - qx))} are used, otherwise the
#' constant force of mortality assumption applies. The last probability is
#' always 1 so that the table closes, and a non-finite rate in the last
#' interval is replaced with the geometric continuation of the two
#' previous rates.
#' @inheritParams LifeTable
#' @param nx Numeric vector of interval widths, one per age.
#' @param ux The input vector or matrix of mx or qx values.
#' @param out Which direction to convert: \code{"qx"} or \code{"mx"}.
#' @return A vector or matrix of the converted probabilities or rates.
#' @noRd
mx_qx <- function(x, nx, ux, out = c("qx", "mx"), ax = NULL) {
  out   <- match.arg(out)
  dims  <- dim(ux)
  N     <- if (is.null(dims)) length(ux) else dims[1]
  exact <- !is.null(ax)

  if (out == "qx") {
    eta <- if (exact) {
      nx * ux / (1 + (nx - ax) * ux)
    } else {
      1 - exp(-nx * ux)
    }

    # The life table always closes with q[x] = 1, in every column.
    if (is.null(dims)) {
      eta[N] <- 1
    } else {
      eta[N, ] <- 1
    }

  } else {
    eta <- if (exact) {
      ux / (ax * ux + nx * (1 - ux))
    } else {
      suppressWarnings(-log(1 - ux)/nx)
    }

    # A qx = 1 leaves mx = Inf, which distorts the subsequent processes.
    eta <- repair_mx(mx = eta, nx = nx)
  }

  eta <- repair_above_omega(x = x, ux = eta)
  return(eta)
}


#' Snap derived death probabilities to the closed value
#'
#' The last probability derived from a survivorship or a death
#' distribution is 1 up to floating point rounding. Values within a small
#' tolerance of 1 are set to exactly 1 so that every input shape closes
#' the table in the same way.
#' @inheritParams LifeTable
#' @return The same vector with values within \code{1e-8} of 1 set to 1.
#' @noRd
lt_snap_qx <- function(qx) {
  near <- !is.na(qx) & abs(qx - 1) < 1e-8
  qx[near] <- 1
  return(qx)
}


#' Cap the supplied ax at the value the interval can support
#'
#' The identity between mx and qx returns a probability only while
#' \code{ax * mx <= 1}. A value above that bound is replaced with the
#' implied average, 1/mx, which closes the interval, and the affected ages
#' are reported. Returns the input untouched when no ax was supplied.
#' @inheritParams LifeTable
#' @return The \code{ax} vector with the infeasible entries capped at
#'   \code{1/mx}, or \code{NULL} when no ax was supplied.
#' @noRd
lt_feasible_ax <- function(x, ax, mx) {

  if (!is.null(ax)) {
    bound <- 1/mx
    over  <- !is.na(ax) & is.finite(mx) & mx > 0 & ax > bound

    if (any(over)) {
      warning("The 'ax' value supplied is not feasible at age(s) ",
              paste(x[over], collapse = ", "), ": it exceeds 1/mx there and ",
              "has been replaced with the implied average, 1/mx.",
              call. = FALSE)
      ax[over] <- bound[over]
    }
  }

  return(ax)
}


#' Close a set of death probabilities
#'
#' Forces the probability of dying in the last interval to 1, so that the
#' life table closes, saying so when the supplied values were not closed.
#' @param qx Death probabilities.
#' @param notice Whether to report the closure. The ax iteration in
#'   \code{compute_life_table} and the omega probe evaluate the same input
#'   repeatedly, and a note per pass would say the same thing five times over.
#' @return The input vector with its last element set to 1.
#' @noRd
lt_close_qx <- function(qx, notice = TRUE) {
  N  <- length(qx)
  ok <- !is.na(qx[N]) && qx[N] != 1

  if (ok && notice) {
    message("'qx' is not closed at the last age; it has been set to 1. That ",
            "is the usual way to close a life table and is applied here, so ",
            "closing the input yourself is optional.")
  }

  if (!is.na(qx[N])) {
    qx[N] <- 1
  }

  return(qx)
}


#' Replace non-finite mortality rates
#'
#' Rates that mark an interval closing the table (qx = 1) are not finite,
#' so they are replaced with a finite stand-in: the geometric continuation
#' of the two previous rates when these are available, the last finite rate
#' otherwise, and the rate implied by a uniform distribution of deaths as a
#' last resort. Entries that are NA mark missing input and are preserved.
#' A matrix is repaired column by column, so each column follows its own
#' rates rather than the tail of the flattened matrix.
#' @inheritParams LifeTable
#' @param nx Numeric vector of interval widths, one per age.
#' @return The \code{mx} vector, or matrix, with the non-finite entries
#'   replaced by finite stand-ins.
#' @noRd
repair_mx <- function(mx, nx) {
  if (is.matrix(mx)) {
    for (j in seq_len(ncol(mx))) {
      mx[, j] <- repair_mx(mx = mx[, j], nx = nx)
    }

    return(mx)
  }

  N <- length(mx)

  for (i in seq_len(N)) {
    if ((is.na(mx[i]) && !is.nan(mx[i])) || is.finite(mx[i])) {
      next
    }

    if (i > 2 && is.finite(mx[i - 1]) && is.finite(mx[i - 2]) &&
        mx[i - 2] > 0) {
      mx[i] <- mx[i - 1]^2/mx[i - 2]

    } else if (i > 1 && is.finite(mx[i - 1]) && mx[i - 1] > 0) {
      mx[i] <- mx[i - 1]

    } else {
      mx[i] <- 2/nx[i]
    }
  }

  return(mx)
}


#' Assign the value of ax in the open age interval
#'
#' Everybody alive at the start of the open interval dies in it, so the
#' survivors live 1/mx years on average. When mx is missing the interval
#' quantity is missing too, and when it is not usable the interval falls
#' back to half of the last closed interval.
#'
#' The interval's own rate implies the average time lived in it, so a value
#' supplied for it is used as given - the table is the caller's to build -
#' and the disagreement is reported. Two callers cannot keep one: an ax
#' method, whose values are derived, and a table entered from ex, where the
#' open interval's ax equals ex there by definition. Both adjust it silently.
#' @inheritParams LifeTable
#' @param nx Numeric vector of interval widths, one per age.
#' @param keep Logical; \code{TRUE} keeps a supplied open-interval value
#'   instead of the implied one.
#' @return The \code{ax} vector with the open interval assigned
#'   \code{1/mx}.
#' @noRd
lt_open_ax <- function(x, ax, mx, nx, keep = FALSE) {
  N    <- length(ax)
  old  <- ax[N]
  val  <- if (is.finite(mx[N]) && mx[N] > 0) {
    1/mx[N]

  } else if (is.na(mx[N])) {
    NA_real_

  } else {
    nx[N - 1]/2
  }

  # A value supplied for the closing interval is the caller's to keep; the
  # interval's own rate implies the value it "should" have, and the two
  # disagreeing is reported rather than silently corrected.
  if (keep && !is.na(old) && !isTRUE(all.equal(old, val))) {
    message("'ax' at the open age interval (age ", x[N], ") is used as ",
            "supplied, ", round(old, 4), ". Everyone alive at ", x[N],
            " dies in that interval, so the average time lived in it is ",
            "implied by its own death rate, 1/mx = ", round(val, 4),
            ". Keeping the supplied value leaves the table's 'ax' and 'mx' ",
            "columns disagreeing at that age.")
    return(ax)
  }

  ax[N] <- val
  return(ax)
}


#' Resolve the ax argument
#'
#' The single public argument \code{ax} carries two meanings: a numeric
#' scalar or vector (user-supplied person-years lived), or a method name
#' selecting how the internal values are derived. This splits it into the
#' numeric \code{ax} and the internal method name.
#'
#' The method names are:
#' \itemize{
#'   \item \code{"andreev_kingkade"} (the default): the rule the Human
#'         Mortality Database applies to its period life tables (Methods
#'         Protocol, version 6, section 7.1). The Andreev-Kingkade (2015)
#'         formula sets a0 from m0, every other closed interval uses
#'         \code{n/2}, and the open interval uses \code{1/mx};
#'   \item \code{"cfm"}: the standard lifetable identity
#'         \code{ax = n + 1/m - n/q} under the constant force of mortality
#'         assumption (Preston, Heuveline and Guillot 2001, eq. 3.15), with a0
#'         left at the midpoint value;
#'   \item \code{"preston"}: \code{"cfm"} for all intervals, with the first
#'         two intervals replaced by the Coale-Demeny West separation
#'         factors given by Preston et al. (2001), table 3.3, when \code{sex}
#'         is supplied;
#'   \item \code{"coale_demeny"}: \code{"cfm"} for all intervals, with the
#'         first two intervals replaced by the original 1983 Coale-Demeny
#'         rule reproduced by the PAS software, when \code{sex} is supplied.
#' }
#' \code{"preston"} and \code{"coale_demeny"} differ from \code{"cfm"} only
#' in the first two intervals, and only when \code{sex} is given.
#'
#' \code{"andreev_kingkade"} is the only method that replaces the whole
#' \code{ax} vector with a numeric one, so it is also the only method that
#' drives the exact \code{mx} to \code{qx} identity. The others adjust
#' \code{ax} after the fact and leave the constant force of mortality
#' conversion in place.
#' @inheritParams LifeTable
#' @return A list with the numeric \code{ax} (or \code{NULL}) and the
#'   resolved \code{ax_method}. Raises an error when \code{ax} is neither
#'   numeric nor a known method name, or has a length other than 1 or
#'   \code{length(x)}.
#' @noRd
check_ax <- function(x, ax = "andreev_kingkade") {

  if (is.character(ax)) {
    if (length(ax) != 1) {
      stop("'ax' must name a single method, not a vector.", call. = FALSE)
    }
    ax_method <- match.arg(ax, c("andreev_kingkade", "cfm", "preston",
                                 "coale_demeny"))
    return(list(ax = NULL, ax_method = ax_method))
  }

  if (!is.numeric(ax)) {
    stop("'ax' must be a numeric scalar or vector, or one of ",
         "\"andreev_kingkade\" / \"cfm\" / \"preston\" / \"coale_demeny\".",
         call. = FALSE)
  }

  if (!any(length(ax) %in% c(1, length(x)))) {
    stop("'ax' must be a scalar of length 1 or a ",
         "vector of the same dimension as 'x'", call. = FALSE)
  }

  return(list(ax = ax, ax_method = "cfm"))
}


#' Close the open interval with a mortality law
#'
#' Fits a parametric mortality law to the closed age intervals and returns
#' the model-implied average force of mortality over the open interval,
#' \code{1/e(x[N])}, where \code{e(x[N])} is obtained by integrating the
#' fitted survival curve. Replacing the observed open-interval rate
#' \code{mx[N]} with this value closes the table accurately without changing
#' the age grid: the open interval absorbs the rise in the hazard that the
#' reciprocal rule \code{1/mx} ignores.
#'
#' The law is fitted to the observed closed intervals from age
#' \code{fit_from} (60 by default when the input reaches age 85, otherwise
#' the last 20 years) up to the last closed interval, excluding the open
#' interval itself, whose aggregate rate is not a point on the hazard curve.
#' The default law is the Kannisto logistic, the field standard for old-age
#' mortality.
#'
#' @inheritParams LifeTable
#' @param law The mortality law used to close. Default \code{"kannisto"}.
#' @param fit_from The age from which the law is fitted. \code{NULL} chooses
#'   60, or the last 20 years of the input when the input is shorter.
#' @param horizon The age up to which the fitted survival curve is
#'   integrated. Default 130, the standard old-age ceiling.
#' @return The model-implied open-interval rate, or \code{NULL} when the law
#'   cannot be fitted (the caller then keeps the observed rate and warns).
#' @noRd
lt_close_model <- function(x, mx, law = NULL, fit_from = NULL,
                           horizon = 130) {

  if (is.null(law)) {
    law <- "kannisto"
  }

  N    <- length(x)
  x_o  <- x[N]
  fit  <- lt_fit_ages(x = x, fit_from = fit_from)

  if (sum(fit) < 3 || horizon <= x_o) {
    return(NULL)
  }

  M <- lt_fit_law(x = x[fit], mx = mx[fit], law = law)
  if (is.null(M)) {
    return(NULL)
  }

  # Integrate the fitted survival curve S(t) = exp(-H(t)) from the open age
  # to the horizon; e(x_o) = int S, since S(x_o) = 1 by construction.
  g    <- 0.05
  grid <- seq(x_o, horizon, by = g)

  mu <- tryCatch(predict(M, x = grid), error = function(e) NULL)
  if (is.null(mu) || any(!is.finite(mu)) || any(mu < 0)) {
    return(NULL)
  }

  S <- exp(-cumsum(mu) * g)
  e <- g * (sum(S) - 0.5 * S[1] - 0.5 * S[length(S)])

  if (!is.finite(e) || e <= 0) {
    return(NULL)
  }

  return(1/e)
}


#' Fit a mortality law to a set of rates
#'
#' Wraps \code{\link{MortalityLaw}} and returns \code{NULL} instead of an
#' error or a warning when the fit is not usable.
#' @inheritParams LifeTable
#' @param law The mortality-law code to fit, see \code{\link{availableLaws}}.
#' @return A fitted \code{"MortalityLaw"} object, or \code{NULL} when the
#'   fit fails.
#' @noRd
lt_fit_law <- function(x, mx, law) {

  M <- tryCatch(
    MortalityLaw(x = x, mx = mx, law = law),
    error = function(e) NULL,
    warning = function(w) NULL
    )

  if (is.null(M) || any(!is.finite(coef(M)))) {
    return(NULL)
  }

  return(M)
}


#' Ages used to fit the closing law
#'
#' By default 60 and above when the input reaches age 85, otherwise the last
#' 20 years of the input. Always excludes the open interval itself.
#' @inheritParams LifeTable
#' @return A logical vector, \code{TRUE} at the ages used to fit the law.
#' @noRd
lt_fit_ages <- function(x, fit_from = NULL) {

  N   <- length(x)
  x_o <- x[N]

  if (is.null(fit_from)) {
    fit_from <- if (x_o >= 85) 60 else x_o - 20
  }

  return(x >= fit_from & x < x_o)
}


#' Extend the open age interval to a chosen omega
#'
#' Fits a parametric mortality law to the closed age intervals below the
#' open interval and predicts the death rates from the open age up to
#' \code{omega}, so that the table can be closed at \code{omega} instead of
#' at the age where the input stops.
#'
#' The law is fitted to the observed closed intervals from age
#' \code{fit_from} (60 by default when the input reaches age 85, otherwise
#' the last 20 years) up to the last closed interval, excluding the open
#' interval itself, whose aggregate rate is not a point on the hazard curve.
#' The default law is the Kannisto logistic, the field standard for old-age
#' mortality. Values below the open age are kept as supplied.
#'
#' @inheritParams LifeTable
#' @param law The mortality law used to extrapolate. Default
#'   \code{"kannisto"}.
#' @param fit_from The age from which the law is fitted. \code{NULL} chooses
#'   60, or the last 20 years of the input when the input is shorter.
#' @return A list with the extended \code{x} and \code{mx}, or \code{NULL}
#'   when the law cannot be fitted (the caller then keeps the table closed at
#'   the input's open age and warns).
#' @noRd
lt_extend_omega <- function(x, mx, omega, law = NULL, fit_from = NULL) {

  if (is.null(law)) {
    law <- "kannisto"
  }

  N    <- length(x)
  x_o  <- x[N]
  step <- x[N] - x[N - 1]
  fit  <- lt_fit_ages(x = x, fit_from = fit_from) & is.finite(mx)

  if (sum(fit) < 3) {
    return(NULL)
  }

  xext <- seq(x_o, omega, by = step)
  if (length(xext) < 2) {
    return(NULL)
  }

  M <- lt_fit_law(x = x[fit], mx = mx[fit], law = law)
  if (is.null(M)) {
    return(NULL)
  }

  mext <- tryCatch(predict(M, x = xext),
                   error = function(e) NULL)
  if (is.null(mext) || any(!is.finite(mext)) || any(mext <= 0)) {
    return(NULL)
  }

  keep <- x < x_o
  out  <- list(x = c(x[keep], xext), mx = c(mx[keep], as.numeric(mext)))
  return(out)
}


#' Validate the closing method
#'
#' Returns \code{NULL} for the standard reciprocal close and the validated
#' law code otherwise. Raises an error when \code{close} is neither
#'   \code{"standard"} nor a code listed by \code{\link{availableLaws}}.
#' @inheritParams LifeTable
#' @return \code{NULL} for the standard close, or the validated law code.
#' @noRd
check_close <- function(close) {

  if (is.null(close) || identical(close, "standard")) {
    return(NULL)
  }

  if (!is.character(close) || length(close) != 1) {
    stop("'close' must be \"standard\" or a single mortality-law code.",
         call. = FALSE)
  }

  codes <- availableLaws()$table[, "CODE"]
  if (!close %in% codes) {
    stop("'close' must be \"standard\" or one of the codes listed by ",
         "availableLaws(), got '", close, "'.", call. = FALSE)
  }

  return(close)
}


#' Validate the omega closing argument
#'
#' Returns \code{NULL} when no extension is requested and the validated,
#' numeric omega otherwise.
#' @inheritParams LifeTable
#' @return \code{NULL} when no extension is requested, the validated
#'   numeric \code{omega} otherwise; warns when \code{omega} does not
#'   exceed the last age.
#' @noRd
check_omega <- function(x, omega) {

  if (is.null(omega)) {
    return(NULL)
  }

  if (!is.numeric(omega) || length(omega) != 1 || !is.finite(omega)) {
    stop("'omega' must be a single finite number.", call. = FALSE)
  }

  if (omega <= max(x)) {
    warning("'omega' (", omega, ") is not greater than the last age in 'x' (",
            max(x), "). The life table keeps its current open interval.",
            call. = FALSE)
    return(NULL)
  }

  return(omega)
}


#' Educate mx or qx on how to behave above age omega
#'
#' Replaces missing, zero and non-finite rates from age \code{omega}
#' onwards with the highest usable rate observed in that same age range.
#' @inheritParams LifeTable
#' @param ux A vector or a matrix of mx or qx values.
#' @param omega Threshold age. Default: 100.
#' @param verbose A logical value. Set \code{verbose = FALSE} to silence
#'   the process that takes place inside the function and avoid progress
#'   messages.
#' @inheritParams LifeTable
#' @return The input with the repaired rows, on the same shape.
#' @noRd
repair_above_omega <- function(x,
                       ux,
                       omega = 100,
                       verbose = FALSE) {

  # A classed bare vector is vector input; only objects with dimensions take
  # the column-by-column branch.
  if (is.null(dim(ux))) {
    L    <- x >= omega & (is.na(ux) | is.infinite(ux) | ux == 0)
    good <- !L & !is.na(ux) & is.finite(ux) & ux > 0

    if (any(L) && any(good)) {
      seg   <- good & x >= omega
      mux   <- if (any(seg)) max(ux[seg]) else max(ux[good])
      ux[L] <- mux

      if (verbose) {
        message("The input data contains missing, zero or non-finite values ",
                "over the age of ", omega, ". These have been replaced with ",
                "the maximum observed value: ", round(mux, 4))
      }
    }

  } else {
    for (i in 1:ncol(ux)) {
      ux[, i] <- repair_above_omega(x = x, ux = ux[, i], omega = omega,
                            verbose = verbose)
    }

  }

  return(ux)
}


#' dx to lx
#'
#' Function to convert dx into lx and back
#' @param ux A vector or a matrix of dx or lx data. A matrix is converted
#'   column by column.
#' @param out Type of the output: dx or lx.
#' @return A vector or matrix of the converted values.
#' @noRd
dx_lx <- function(ux, out = c("dx", "lx")) {
  out <- match.arg(out)

  if (is.matrix(ux)) {
    for (j in seq_len(ncol(ux))) {
      ux[, j] <- dx_lx(ux = ux[, j], out = out)
    }

    return(ux)
  }

  if (out == "dx") {
    ux_ <- rev(diff(rev(ux)))
    d   <- ux[1] - sum(ux_)
    eta <- c(ux_, d)

  } else {
    eta <- rev(cumsum(rev(ux)))
  }
  return(eta)
}


#' Survivorship and death distribution
#'
#' Builds the closed-form survivorship chain \code{lx[j + 1] = lx[j] *
#' (1 - qx[j])} from the radix \code{lx0} and derives the death
#' distribution from it. Missing probabilities add no deaths to the chain;
#' the caller marks the affected rows in the derived columns.
#' @inheritParams LifeTable
#' @return A list with \code{lx} (survivorship) and \code{dx} (death
#'   distribution).
#' @noRd
lx_dx <- function(qx, lx0) {
  N  <- length(qx)
  qc <- qx
  qc[is.na(qc)] <- 0
  lx <- lx0 * c(1, cumprod(1 - qc)[seq_len(N - 1)])
  dx <- dx_lx(ux = lx, out = "dx")
  return(list(lx = lx, dx = dx))
}


#' Death probabilities from a death distribution
#'
#' The ratio dx/lx where the table is still open and 1 where the table has
#' already closed (lx = 0).
#' @inheritParams LifeTable
#' @return A numeric vector of death probabilities.
#' @noRd
lt_qx <- function(dx, lx) {
  qx <- dx/lx
  qx[!is.na(lx) & lx == 0] <- 1
  return(qx)
}


#' Find ax indicator
#'
#' Computes the average number of person-years lived in each age interval
#' by those who die in it, from the mx and qx values. Intervals with no
#' deaths are assigned half of the interval length, and the open age
#' interval is assigned 1/mx. NA entries mark missing input and are
#' propagated.
#' @inheritParams LifeTable
#' @return A numeric vector of the average person-years lived in each
#'   interval by those who die in it.
#' @noRd
compute_ax <- function(x, mx, qx) {
  nx <- c(diff(x), Inf)
  N  <- length(x)
  ax <- nx + 1/mx - nx/qx

  # For very small rates the two large terms of the closed form cancel
  # each other; the first terms of its series expansion in z = nx * mx,
  # ax = nx * (1/2 - z/12 + z^3/720 - ...), are exact there.
  z  <- nx[-N] * mx[-N]
  sm <- which(is.finite(z) & z > 0 & z < 1e-3)

  if (length(sm)) {
    ax[sm] <- nx[sm] * (0.5 - z[sm]/12)
  }

  # No deaths in the interval: the person-years lived cover the interval.
  no_deaths <- !is.na(mx) & mx == 0
  ax[no_deaths] <- nx[no_deaths]/2

  # Non-finite entries inherit the closest finite value.
  bad <- is.infinite(ax) | is.nan(ax)

  for (i in seq_len(N)) {
    if (!bad[i]) {
      next
    }

    if (i > 1 && is.finite(ax[i - 1])) {
      ax[i] <- ax[i - 1]

    } else if (i < N && is.finite(ax[i + 1])) {
      ax[i] <- ax[i + 1]

    } else {
      ax[i] <- nx[i]/2
    }
  }

  # Open age interval: the survivors live 1/mx years on average.
  ax[N] <- if (is.finite(mx[N]) && mx[N] > 0) {
    1/mx[N]

  } else if (is.na(mx[N])) {
    NA_real_

  } else {
    nx[N - 1]/2
  }

  return(ax)
}


#' Recover q0 from m0 for the Coale-Demeny separation factors
#'
#' The 1983 coefficients are a function of q0, while a life table built
#' from rates is keyed on m0. The quadratic published by the PAS software
#' (LTPOPDTH) inverts the constant force of mortality relation
#' \code{q0 = 1 - exp(-m0)} to first order and is the inversion the
#' Coale-Demeny rule expects. A non-positive discriminant only occurs well
#' above the q0 = 0.1 cutoff, where the constant branch applies anyway; such
#' rows return 0.2 to select it, as in the original implementation.
#' @param m0 The death rate in the first year of life.
#' @param alpha,beta The published Coale-Demeny separation-factor
#'   coefficients of the sex in question.
#' @return A numeric vector of the implied \code{q0} values.
#' @noRd
cd_q0_from_m0 <- function(m0, alpha, beta) {

  a    <- m0 * beta
  b    <- 1 + m0 * (1 - alpha)
  disc <- b^2 - 4 * a * m0
  ok   <- a > 0 & disc > 0

  q0 <- rep(0, length(m0))

  if (any(ok)) {
    q0[ok] <- (b[ok] - sqrt(disc[ok])) / (2 * a[ok])
  }

  q0[!ok & m0 > 0] <- 0.2

  return(q0)
}


#' The West separation factors a0 and 4a1
#'
#' Returns the average person-years lived before the first birthday (a0) and
#' between ages 1 and 5 (4a1) for the West model, as a male and a female
#' pair. Both conventions return identical values once m0 reaches 0.107,
#' because from there the constants apply.
#' @param m0 The death rate in the first year of life.
#' @param method \code{"preston"} for the coefficients published in
#'   Preston, Heuveline and Guillot (2001), table 3.3, which are expressed
#'   in terms of m0; \code{"coale_demeny"} for the original 1983 rule,
#'   expressed in terms of q0, recovered from m0 by
#'   \code{cd_q0_from_m0}.
#' @return A list with the \code{male} and \code{female} pairs
#'   \code{c(a0, 4a1)}.
#' @noRd
coale_demeny_ax_coefs <- function(m0, method) {

  if (method == "preston") {
    a0M <- ifelse(m0 >= 0.107, 0.330, 0.045 + 2.684 * m0)
    a1M <- ifelse(m0 >= 0.107, 1.352, 1.651 - 2.816 * m0)
    a0F <- ifelse(m0 >= 0.107, 0.350, 0.053 + 2.800 * m0)
    a1F <- ifelse(m0 >= 0.107, 1.361, 1.522 - 1.518 * m0)

  } else {
    q0M <- cd_q0_from_m0(m0 = m0, alpha = 0.0425, beta = 2.875)
    q0F <- cd_q0_from_m0(m0 = m0, alpha = 0.0500, beta = 3.000)

    a0M <- ifelse(q0M > 0.1, 0.330, 0.0425 + 2.875 * q0M)
    a1M <- ifelse(q0M > 0.1, 1.352, 1.653 - 3.013 * q0M)
    a0F <- ifelse(q0F > 0.1, 0.350, 0.0500 + 3.000 * q0F)
    a1F <- ifelse(q0F > 0.1, 1.361, 1.524 - 1.627 * q0F)
  }

  out <- list(male = c(a0M, a1M), female = c(a0F, a1F))

  return(out)
}


#' The Andreev-Kingkade a0 factor
#'
#' Returns the average person-years lived before the first birthday (a0) from
#' the death rate in the first year of life, using the piecewise linear rule
#' of Andreev and Kingkade (2015) adopted by version 6 of the HMD Methods
#' Protocol (Table 3). It is used only for a table that starts at birth with a
#' one-year first interval, where the rate is a genuine m0.
#' @param m0 The death rate in the first year of life.
#' @param sex One of \code{"male"}, \code{"female"} or \code{"total"}. The
#'   total row takes the death-weighted average convention of
#'   \code{\link{coale_demeny_ax_coefs}}: the arithmetic mean of the male and
#'   female a0 at the given m0, which is HMD equation (77) with equal deaths.
#' @return The value of a0.
#' @references
#' Andreev, E. M. and Kingkade, W. W. (2015). Average age at death in infancy
#' and infant mortality level: Reconsidering the Coale-Demeny formulas at
#' current low levels of infant mortality. \emph{Demographic Research} 33,
#' 727-756.
#'
#' Wilmoth, J. R., Andreev, K., Jdanov, D., Glei, D. A. and Riffe, T. (2025).
#' \emph{Methods Protocol for the Human Mortality Database}, Version 6,
#' section 7.1 and Table 3.
#' @noRd
ak_a0_coefs <- function(m0, sex = c("male", "female", "total")) {
  sex <- match.arg(sex)

  a0M <- if (m0 < 0.02300) {
    0.14929 - 1.99545 * m0
  } else if (m0 < 0.08307) {
    0.02832 + 3.26021 * m0
  } else {
    0.29915
  }

  a0F <- if (m0 < 0.01724) {
    0.14903 - 2.05527 * m0
  } else if (m0 < 0.06891) {
    0.04667 + 3.88089 * m0
  } else {
    0.31411
  }

  out <- switch(sex,
                male   = a0M,
                female = a0F,
                total  = (a0M + a0F)/2)

  return(out)
}


#' The Andreev-Kingkade a0 of a table
#'
#' Returns the average person-years lived before the first birthday (a0) for a
#' table that starts at birth with a one-year first interval, where the first
#' interval is a genuine first year of life. A table that starts above age 0 or
#' whose first interval is wider has no such interval, so the function returns
#' \code{NULL} and the caller keeps the ordinary value.
#' @inheritParams LifeTable
#' @param m0 The first-interval death rate.
#' @return The value of a0, or \code{NULL} when the rule does not apply.
#' @noRd
andreev_kingkade_a0 <- function(x, m0, sex = NULL) {
  starts_at_birth <- isTRUE(all.equal(x[1], 0))
  one_year        <- isTRUE(all.equal(x[2] - x[1], 1))

  if (!starts_at_birth || !one_year || !is.finite(m0) || m0 < 0) {
    return(NULL)
  }

  sx <- if (is.null(sex)) "total" else sex
  a0 <- ak_a0_coefs(m0 = m0, sex = sx)

  return(a0)
}


#' Build the ax vector of the Andreev-Kingkade method
#'
#' Builds the average person-years lived in each age interval with the rule
#' the Human Mortality Database applies to its period life tables (Methods
#' Protocol version 6, section 7.1): the Andreev-Kingkade (2015) formula for
#' the first year of life and \code{n/2} for every other closed interval. That
#' is exact when the table is by single years of age, which is how the HMD
#' publishes (abridged HMD tables are extracted from the single-age ones, not
#' built from five-year rates).
#'
#' On wider intervals the midpoint value can exceed the interval's implied
#' average \code{1/mx} once the rate passes \code{2/n}, which would collapse
#' the interval's death probability to one and close the table early. Above
#' such a rate, and on every interval wider than one year, the interval keeps
#' the constant force of mortality value, which is where the HMD's own rates
#' would land too.
#'
#' A table that does not start at birth with a one-year first interval carries
#' no m0, so its first interval keeps the ordinary value as well.
#' @inheritParams LifeTable
#' @param qx Numeric vector of death probabilities, one per age in \code{x},
#'   used for the wide intervals, where the constant force of mortality value
#'   \code{n + 1/m - n/q} takes over from the midpoint.
#' @return A numeric vector the same length as \code{x}.
#' @noRd
hmd_ax_vector <- function(x, mx, qx, sex = NULL) {
  N  <- length(x)
  nx <- c(diff(x), diff(x)[N - 1])

  # Single-year intervals: the midpoint. Wider intervals: the constant force
  # of mortality value, which the exact identity reproduces for them.
  ax           <- nx + 1/mx - nx/qx
  one_year     <- nx == 1
  ax[one_year] <- nx[one_year]/2

  # Never let an interval carry more than its own implied average.
  bound <- ifelse(is.finite(mx) & mx > 0, 1/mx, Inf)
  ax    <- pmin(ax, bound)

  # The Andreev-Kingkade a0 needs a table that starts at birth.
  a0 <- andreev_kingkade_a0(x = x, m0 = mx[1], sex = sex)

  if (!is.null(a0)) {
    ax[1] <- a0
  }

  return(ax)
}


#' Find ax[1:2] indicators using Coale-Demeny coefficients
#'
#' Adjusts the first two values of ax to account for infant mortality more
#' accurately, using the West model separation factors. Two published
#' parameterisations are available through \code{method}: the m0-based
#' coefficients of Preston, Heuveline and Guillot (2001), table 3.3, and
#' the original q0-based rule of Coale and Demeny (1983), reproduced by the
#' PAS software. They agree exactly once m0 reaches 0.107 and differ by at
#' most a few thousandths of a year below it. The total population row
#' averages the male and female values.
#' @inheritParams LifeTable
#' @param method \code{"preston"} or \code{"coale_demeny"}.
#' @return The \code{ax} vector with the first two intervals adjusted.
#' @noRd
coale_demeny_ax <- function(x, mx, ax, sex, method = "preston") {

  if (!is.na(mx[1]) && mx[1] < 0) {
    stop("'m[1]' must be greater than 0", call. = FALSE)
  }

  nx  <- c(diff(x), Inf)
  C   <- coale_demeny_ax_coefs(m0 = mx[1], method = method)
  a02 <- switch(sex,
                male   = C$male,
                female = C$female,
                total  = (C$male + C$female)/2)

  f  <- nx[1:2] / c(1, 4)
  ax[1:2] <- a02 * f

  return(ax)
}


#' Normalise a missing mortality input and say what was done
#'
#' Turns NaN into NA and says so about the missing or non-finite rate values,
#' naming the affected ages and the treatment they receive. Values from age
#' 100 onwards are repaired by \code{repair_above_omega}; the remaining affected
#' rows are returned as NA.
#' @inheritParams LifeTable
#' @param ux A vector or a matrix of mx or qx values.
#' @param what The name of the input, used in the warning message.
#' @param omega Threshold age. Default: 100.
#' @return The input with the repairs applied.
#' @noRd
lt_repair_input <- function(x, ux, what, omega = 100) {

  if (is.data.frame(ux)) {
    ux <- as.matrix(ux)
  }

  ux[is.nan(ux)] <- NA

  bad  <- !is.finite(ux)
  rows <- if (is.matrix(ux)) which(rowSums(bad) > 0) else which(bad)

  if (length(rows)) {
    message("'", what, "' contains missing or non-finite values at age(s) ",
            paste(x[rows], collapse = ", "), ". Rows below age ", omega,
            " are returned as NA with the survivorship bridged across the ",
            "gap; rows from age ", omega, " are replaced with the highest ",
            "rate observed there.")
  }

  ux <- repair_above_omega(x = x, ux = ux, omega = omega)
  return(ux)
}


#' Check LifeTable input
#'
#' Validates the input data and the auxiliary arguments, repairs the values
#' that can be repaired and says so about the missing ones.
#' @param input A list containing the input arguments of the LifeTable
#'   functions.
#' @return A list of life table validated data
#' @noRd
check_life_table_input <- function(input) {

  with(input, {
    # ----------------------------------------------
    K <- detect_case(Dx = Dx, Ex = Ex, mx = mx, qx = qx, lx = lx, dx = dx,
                     ex = ex)
    C <- K$case
    valid_classes <- c("numeric", "matrix", "data.frame", NULL)

    if (!any(K$iclass %in% valid_classes)) {
      stop(paste0("The class of the input should be: ",
                  paste(valid_classes, collapse = ", ")), call. = FALSE)
    }

    # The data must line up with the age vector before any repair runs.
    L <- list(Dx = Dx, Ex = Ex, mx = mx, qx = qx, lx = lx, dx = dx, ex = ex)

    for (nm in names(L)) {
      ux <- L[[nm]]

      if (!is.null(ux)) {
        n <- if (is.null(dim(ux))) length(ux) else nrow(ux)

        if (n != length(x)) {
          stop("'", nm, "' must have one value per age in 'x' (", length(x),
               " expected, got ", n, ")", call. = FALSE)
        }
      }
    }
    # ----------------------------------------------
    SMS <- "contains missing values. These have been replaced with "

    if (!is.null(sex)) {
      if (!any(sex %in% c("male", "female", "total"))) {
        stop("'sex' should be: 'male', 'female', 'total' or 'NULL'.",
             call. = FALSE)
      }
    }

    if (C == "C1_DxEx") {
      if (any(is.na(Dx))) message("'Dx' ", SMS, 0)
      if (any(is.na(Ex))) message("'Ex' ", SMS, 0.01)
      Dx[is.na(Dx)] <- 0
      Ex[is.na(Ex) | Ex == 0] <- 0.01
    }

    if (C == "C2_mx") {
      mx <- lt_repair_input(x = x, ux = mx, what = "mx")
    }

    if (C == "C3_qx") {
      qx <- lt_repair_input(x = x, ux = qx, what = "qx")
    }

    if (C == "C4_lx") {
      if (any(is.na(lx))) message("'lx' ", SMS, 0)
      lx[is.na(lx) & x >= 100] <- 0
    }

    if (C == "C5_dx") {
      if (any(is.na(dx))) message("'dx' ", SMS, 0)
      dx[is.na(dx)] <- 0
    }

    if (C == "C6_ex") {
      # A curve of life expectancy cannot be repaired the way a rate can: the
      # inverse needs a complete curve, so a missing value is reported, never
      # filled with a rate heuristic (which would silently change the answer).
      ex <- lt_repair_ex(x = x, ex = ex)
    }

    # 'ax' is validated by check_ax() before this function runs.

    # Exit
    out <- list(x = as.numeric(x),
                Dx = Dx,
                Ex = Ex,
                mx = mx,
                qx = qx,
                lx = lx,
                dx = dx,
                ex = ex,
                sex = sex,
                lx0 = lx0,
                ax = ax,
                close = close,
                omega = omega,
                fit_from = fit_from,
                case = C,
                iclass = K$iclass,
                nLT = K$nLT,
                LTnames = K$LTnames)
    return(out)
  })
}
