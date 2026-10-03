# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-04 22:59:32
# --------------------------------------------

# Internal helpers behind LifeTable(): the mx/qx bridge, the ax
# estimators, the survivorship chain and the input validation.

#' Identify the input case to be solved
#'
#' Detects which of the five accepted inputs was supplied and returns the
#' case name together with the input class, the number of tables to
#' compute and the names to be assigned to them.
#' @noRd
detect_case <- function(Dx = NULL,
                         Ex = NULL,
                         mx = NULL,
                         qx = NULL,
                         lx = NULL,
                         dx = NULL) {

  input   <- c(as.list(environment()))

  # Matrix of possible cases --------------------
  rn  <- c("C1_DxEx", "C2_mx", "C3_qx", "C4_lx", "C5_dx")
  cn  <- c("Dx", "Ex", "mx", "qx", "lx", "dx")
  mat <- matrix(
    ncol = 6,
    byrow = TRUE,
    dimnames = list(rn, cn),
    data = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
             FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
             FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
             FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
             FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
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
#' life table closes, warning when the supplied values were not closed.
#' @noRd
lt_close_qx <- function(qx) {
  N  <- length(qx)
  ok <- !is.na(qx[N]) && qx[N] != 1

  if (ok) {
    warning("'qx' is not closed. The probability of dying in the last ",
            "interval has been set to 1.", call. = FALSE)
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
#' back to half of the last closed interval. A user-supplied value is
#' replaced, with a warning.
#' @noRd
lt_open_ax <- function(x, ax, mx, nx, warn = FALSE) {
  N   <- length(ax)
  old <- ax[N]
  val <- if (is.finite(mx[N]) && mx[N] > 0) {
    1/mx[N]

  } else if (is.na(mx[N])) {
    NA_real_

  } else {
    nx[N - 1]/2
  }

  if (warn && !is.na(old) && !isTRUE(all.equal(old, val))) {
    warning("The 'ax' value supplied for the open age interval (age ",
            x[N], ") has been replaced with ", round(val, 4),
            " to keep the closed life table consistent.", call. = FALSE)
  }

  ax[N] <- val
  return(ax)
}


#' Educate mx or qx on how to behave above age omega
#'
#' Replaces missing, zero and non-finite rates from age \code{omega}
#' onwards with the highest usable rate observed in that same age range.
#' @param x Numeric vector of ages at the beginning of the age intervals.
#' @param ux A vector or a matrix of mx or qx values.
#' @param omega Threshold age. Default: 100.
#' @param verbose A logical value. Set \code{verbose = FALSE} to silence
#'   the process that takes place inside the function and avoid progress
#'   messages.
#' @noRd
repair_above_omega <- function(x,
                       ux,
                       omega = 100,
                       verbose = FALSE) {

  if (is.vector(ux)) {
    L    <- x >= omega & (is.na(ux) | is.infinite(ux) | ux == 0)
    good <- !L & !is.na(ux) & is.finite(ux) & ux > 0

    if (any(L) && any(good)) {
      seg   <- good & x >= omega
      mux   <- if (any(seg)) max(ux[seg]) else max(ux[good])
      ux[L] <- mux

      if (verbose) {
        warning("The input data contains missing, zero or non-finite values ",
                "over the age of ", omega, ". These have been replaced with ",
                "the maximum observed value: ", round(mux, 4), call. = FALSE)
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
#' @return A vector of the average person-years lived in the interval by
#'   those who die in the interval.
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


#' Find ax[1:2] indicators using Coale-Demeny coefficients
#'
#' Adjusts the first two values of ax to account for infant mortality more
#' accurately, using the West model coefficients of the Coale-Demeny
#' regional model as published in UN (1983), \emph{Manual X: Indirect
#' Techniques for Demographic Estimation}, table 3.3 (also reproduced by
#' the PAS software). Below m0 = 0.107 the coefficients are interpolated
#' from the same table.
#' @noRd
coale_demeny_ax <- function(x, mx, ax, sex) {

  if (!is.na(mx[1]) && mx[1] < 0) {
    stop("'m[1]' must be greater than 0", call. = FALSE)
  }

  nx  <- c(diff(x), Inf)
  m0  <- mx[1]
  a0M <- ifelse(m0 >= 0.107, 0.330, 0.045 + 2.684 * m0)
  a1M <- ifelse(m0 >= 0.107, 1.352, 1.651 - 2.816 * m0)
  a0F <- ifelse(m0 >= 0.107, 0.350, 0.053 + 2.800 * m0)
  a1F <- ifelse(m0 >= 0.107, 1.361, 1.522 - 1.518 * m0)
  a0T <- (a0M + a0F)/2
  a1T <- (a1M + a1F)/2

  f  <- nx[1:2] / c(1, 4)

  if (sex == "male")   ax[1:2] <- c(a0M, a1M) * f
  if (sex == "female") ax[1:2] <- c(a0F, a1F) * f
  if (sex == "total")  ax[1:2] <- c(a0T, a1T) * f

  return(ax)
}


#' Normalise and warn about a missing mortality input
#'
#' Turns NaN into NA and warns about the missing or non-finite rate values,
#' naming the affected ages and the treatment they receive. Values from age
#' 100 onwards are repaired by \code{repair_above_omega}; the remaining affected
#' rows are returned as NA.
#' @noRd
lt_repair_input <- function(x, ux, what, omega = 100) {

  if (is.data.frame(ux)) {
    ux <- as.matrix(ux)
  }

  ux[is.nan(ux)] <- NA

  bad  <- !is.finite(ux)
  rows <- if (is.matrix(ux)) which(rowSums(bad) > 0) else which(bad)

  if (length(rows)) {
    warning("'", what, "' contains missing or non-finite values at age(s) ",
            paste(x[rows], collapse = ", "), ". Rows below age ", omega,
            " are returned as NA with the survivorship bridged across the ",
            "gap; rows from age ", omega, " are replaced with the highest ",
            "rate observed there.", call. = FALSE)
  }

  ux <- repair_above_omega(x = x, ux = ux, omega = omega)
  return(ux)
}


#' Check LifeTable input
#'
#' Validates the input data and the auxiliary arguments, repairs the values
#' that can be repaired and warns about the missing ones.
#' @param input A list containing the input arguments of the LifeTable
#'   functions.
#' @return A list of life table validated data
#' @noRd
check_life_table_input <- function(input) {

  with(input, {
    # ----------------------------------------------
    K <- detect_case(Dx = Dx, Ex = Ex, mx = mx, qx = qx, lx = lx, dx = dx)
    C <- K$case
    valid_classes <- c("numeric", "matrix", "data.frame", NULL)

    if (!any(K$iclass %in% valid_classes)) {
      stop(paste0("The class of the input should be: ",
                  paste(valid_classes, collapse = ", ")), call. = FALSE)
    }

    # The data must line up with the age vector before any repair runs.
    L <- list(Dx = Dx, Ex = Ex, mx = mx, qx = qx, lx = lx, dx = dx)

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
      if (any(is.na(Dx))) warning("'Dx'", SMS, 0, call. = FALSE)
      if (any(is.na(Ex))) warning("'Ex'", SMS, 0.01, call. = FALSE)
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
      if (any(is.na(lx))) warning("'lx'", SMS, 0, call. = FALSE)
      lx[is.na(lx) & x >= 100] <- 0
    }

    if (C == "C5_dx") {
      if (any(is.na(dx))) warning("'dx'", SMS, 0, call. = FALSE)
      dx[is.na(dx)] <- 0
    }

    if (!is.null(ax)) {
      if (!is.numeric(ax)) {
        stop("'ax' must be a numeric scalar (or NULL)", call. = FALSE)
      }

      if (!any(length(ax) %in% c(1, length(x)))) {
        stop("'ax' must be a scalar of length 1 or a ",
             "vector of the same dimension as 'x'",
             call. = FALSE)
      }
    }

    # Exit
    out <- list(x = as.numeric(x),
                Dx = Dx,
                Ex = Ex,
                mx = mx,
                qx = qx,
                lx = lx,
                dx = dx,
                sex = sex,
                lx0 = lx0,
                ax = ax,
                case = C,
                iclass = K$iclass,
                nLT = K$nLT,
                LTnames = K$LTnames)
    return(out)
  })
}
