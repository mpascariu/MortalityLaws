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
#' Only one of these input options needs to be provided; the others 
#' are ignored if present. The input can be a numeric \code{vector}, 
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
#' and females.
#'
#' @usage
#' LifeTable(x, Dx = NULL, Ex = NULL,
#'              mx = NULL,
#'              qx = NULL,
#'              lx = NULL,
#'              dx = NULL,
#'              sex = NULL,
#'              lx0 = 1e5,
#'              ax  = NULL)
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
#'   of each interval.
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
#' my_ax <- c(0.1, 1.5, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
#'            2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1)
#'
#' LT7 <- LifeTable(x = x, mx = mx, ax = my_ax)
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
                      ax = NULL){

  input <- c(as.list(environment()))
  X     <- LifeTable.check(input)
  LT    <- NULL

  if (any(X$iclass == "numeric")) {
    LT <- with(X, LifeTable.core(x, Dx, Ex, mx, qx, lx, dx, sex, lx0, ax))

  } else {
    for (i in 1:X$nLT) {
      LTi <- with(
        X,
        LifeTable.core(
          x,
          Dx = Dx[, i],
          Ex = Ex[, i],
          mx = mx[, i],
          qx = qx[, i],
          lx = lx[, i],
          dx = dx[, i],
          sex = sex,
          lx0 = lx0,
          ax = ax
          )
        )

      N   <- X$LTnames
      LTn <- if (is.na(N[i])) i else N[i]
      LTi <- cbind(LT = LTn, LTi)
      LT  <- rbind(LT, LTi)
    }
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


#' LifeTable.core
#' @inheritParams LifeTable
#' @return A data.frame containing life table results
#' @keywords internal
LifeTable.core <- function(x, Dx, Ex, mx, qx, lx, dx, sex, lx0, ax){

  my.case  <- find.my.case(Dx, Ex, mx, qx, lx, dx)$case
  gr_names <- paste0("[", x,",", c(x[-1], "+"), ")")
  N        <- length(x)
  df       <- diff(x)
  nx       <- c(df, df[N - 1])

  if (my.case == "C1_DxEx") {
    Dx <- as.numeric(Dx)
    Ex <- as.numeric(Ex)
    mx <- Dx/Ex
    mx <- uxAbove100(x, mx)
    qx <- mx_qx(x, nx, mx, out = "qx")
    lx <- lx0 * c(1, cumprod(1 - qx)[1:(N - 1)])
    dx <- dx_lx(lx, out = "dx")
  }
  if (my.case == "C2_mx") {
    mx <- as.numeric(mx)
    qx <- mx_qx(x, nx, mx, out = "qx")
    lx <- lx0 * c(1, cumprod(1 - qx)[1:(N - 1)])
    dx <- dx_lx(lx, out = "dx")
  }
  if (my.case == "C3_qx") {
    qx <- as.numeric(qx)
    mx <- mx_qx(x, nx, qx, out = "mx")
    lx <- lx0 * c(1, cumprod(1 - qx)[1:(N - 1)])
    dx <- dx_lx(lx, out = "dx")
  }
  if (my.case == "C4_lx") {
    lx <- as.numeric(lx)
    lx <- lx * lx0/lx[1]
    dx <- dx_lx(lx, out = "dx")
    qx <- dx/lx
    qx <- uxAbove100(x, qx)
    mx <- mx_qx(x, nx, qx, out = "mx")
  }
  if (my.case == "C5_dx") {
    dx <- as.numeric(dx)
    dx <- dx * lx0/sum(dx)
    lx <- dx_lx(dx, out = "lx")
    qx <- dx/lx
    qx <- uxAbove100(x, qx)
    mx <- mx_qx(x, nx, qx, out = "mx")
  }

  if (is.null(ax)) {
    ax <- compute.ax(x, mx, qx)
    if (!is.null(sex)) ax <- coale.demeny.ax(x, mx, ax, sex)

  } else if (length(ax) == 1){
    ax <- rep(ax, N)

  } else if (length(ax) == N){
    ax <- as.numeric(ax)
  }

  Lx    <- nx * lx - (nx - ax) * dx
  Lx[N] <- ax[N] * dx[N]
  Lx[is.na(Lx)] <- 0
  Tx    <- rev(cumsum(rev(Lx)))
  ex    <- Tx/lx
  ex[is.na(ex)] <- 0
  ex[N] <- if (ex[N - 1] == 0) 0 else ax[N]

  last_check = all(is.na(mx)) | all(is.nan(mx)) | all(is.infinite(mx)) | all(mx == 0)
  if (last_check) mx = qx = ax = lx = dx = Lx = Tx = ex <- NA

  out <- data.frame(x.int = gr_names,
                    x = x,
                    mx = mx,
                    qx = qx,
                    ax = ax,
                    lx = lx,
                    dx = dx,
                    Lx = Lx,
                    Tx = Tx,
                    ex = ex)
  return(out)
}


#' Function that identifies the case/problem we have to solve
#' @inheritParams LifeTable
#' @return A list containing problem solving details
#' @keywords internal
find.my.case <- function(Dx = NULL,
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
    data = c(T,T,F,F,F,F,
             F,F,T,F,F,F,
             F,F,F,T,F,F,
             F,F,F,F,T,F,
             F,F,F,F,F,T)
    )
  # ----------------------------------------------
  L1 <- !unlist(lapply(input, is.null))
  L2 <- apply(mat, 1, function(x) all(L1 == x))
  my_case <- rn[L2]

  if (sum(L1[c(1, 2)]) == 1) {
    stop("If you input 'Dx' you must input 'Ex' as well, and viceversa",
         call. = FALSE)
  }

  if (!any(L2)) {
    stop("The input is not specified correctly. Check again the function ",
         "arguments and make sure the input data is added properly.",
         call. = FALSE)
  }

  X       <- input[L1][[1]]

  if (length(dim(X)) == 1){
    # TR changed here:
    # The following input isn't detected with is.vector()
    # X = structure(c(0.0036542739116619, 0.000150960486092765, 2.77881983521598e-05,
    # 0.000136941279579316, 0.00018946827083136, 0.00026606712873658,
    # 0.000258838755220895, 0.000557913403869528, 0.000665917509468515,
    # 0.00114979916336982, 0.0021040113433655, 0.00384681333263752,
    # 0.00632225868307671, 0.00937214337262448, 0.0154497258185624,
    # 0.023117866694913, 0.0363617070194365, 0.0609184108563059, 0.120398389987367,
    # 0.221461187214612, 0.420152946468736), .Dim = 21L)
    # nLT would come as NA
    # and iclass would be array
    X <- c(X)
  }

  nLT     <- 1
  LTnames <- NA

  # TR: change from !is.vector
  if (length(dim(X)) == 2 ) {

    nLT     <- ncol(X)     # number of LTs to be created
    LTnames <- colnames(X) # the names to be assigned to LTs
  }

  out <- list(case = my_case,
              iclass = class(X), # TR: if inputs are matrix,
                                 # then this is two elements
              nLT = nLT,
              LTnames = LTnames)
  return(out)
}


#' mx to qx
#'
#' Function to convert mx into qx and back, using the constant force of
#' mortality assumption (CFM).
#' @inheritParams LifeTable
#' @param nx Length of the age-intervals.
#' @param ux A vector of mx or qx.
#' @param out Type of the output: mx or qx.
#' @return A vector of rates
#' @keywords internal
mx_qx <- function(x, nx, ux, out = c("qx", "mx")){
  out <- match.arg(out)

  if (out == "qx") {
    eta <- 1 - exp(-nx * ux)
    eta[length(nx)] <- 1  # The life table should always close with q[x] = 1

  } else {
    eta <- suppressWarnings(-log(1 - ux)/nx)
    # If qx[last-age] = 1 then mx[last-age] = Inf. Not nice to have Inf's;
    # they distort the results in the subsequent processes.
    # We apply a simple extrapolation method of the last mx.
    N <- length(x)
    eta[N] <- eta[N - 1]^2 / eta[N - 2]
  }

  eta <- uxAbove100(x, eta)
  return(eta)
}


#' Educate mx or qx on how to behave above age 100 if it gets in trouble
#' (with NA's, zero's and Inf)
#' @inheritParams LifeTable
#' @inheritParams mx_qx
#' @param omega Threshold age. Default: 100.
#' @param verbose A logical value. Set \code{verbose = FALSE} to silent
#' the process that take place inside the function and avoid progress messages.
#' @return A vector of rates
#' @keywords internal
uxAbove100 <- function(x,
                       ux,
                       omega = 100,
                       verbose = FALSE) {

  if (is.vector(ux)) {
    L <- x >= 100 & (is.na(ux) | is.infinite(ux) | ux == 0)

    if (any(L)) {
      mux   <- max(ux[!L])
      ux[L] <- mux

      if (verbose)
        warning("The input data contains NA's, Inf or zero's over the age of ",
                "100. These have been replaced with maximum observed value: ",
                round(mux, 4), call. = FALSE)
    }

  } else {
    for (i in 1:ncol(ux)) {
      ux[, i] = uxAbove100(x, ux[, i], omega, verbose)
    }

  }

  return(ux)
}


#' dx to lx
#'
#' Function to convert dx into lx and back
#' @param ux A vector of dx or lx data.
#' @param out Type of the output: dx or lx.
#' @return A vector containing dx or lx values
#' @keywords internal
dx_lx <- function(ux, out = c("dx", "lx")) {
  out <- match.arg(out)

  if (out == "dx") {
    ux_ <- rev(diff(rev(ux)))
    d   <- ux[1] - sum(ux_)
    eta <- c(ux_, d)

  } else {
    eta <- rev(cumsum(rev(ux)))
  }
  return(eta)
}


#' Find ax indicator
#'
#' @inheritParams LifeTable
#' @return \code{ax} - the point in the age interval where 50% of the deaths
#' have already occurred
#' @keywords internal
compute.ax <- function(x, mx, qx) {
  nx <- c(diff(x), Inf)
  N  <- length(x)
  ax <- nx + 1/mx - nx/qx

  for (i in 1:(N - 1)) {
    if (is.infinite(ax[i + 1]) | is.na(ax[i + 1])) ax[i + 1] = ax[i]
  }
  return(ax)
}


#' Find ax[1:2] indicators using Coale-Demeny coefficients
#' Here we adjust the first two values of ax to account for infant
#' mortality more accurately
#' @inheritParams LifeTable
#' @return A vector of coefficients
#' @keywords internal
coale.demeny.ax <- function(x, mx, ax, sex) {

  if (mx[1] < 0) stop("'m[1]' must be greater than 0", call. = FALSE)

  nx  <- c(diff(x), Inf)
  m0  <- mx[1]
  a0M <- ifelse(m0 >= 0.107, 0.330, 0.045 + 2.684 * m0)
  a1M <- ifelse(m0 >= 0.107, 0.330, 1.651 - 2.816 * m0)
  a0F <- ifelse(m0 >= 0.107, 0.350, 0.053 + 2.800 * m0)
  a1F <- ifelse(m0 >= 0.107, 0.350, 1.522 - 1.518 * m0)
  a0T <- (a0M + a0F)/2
  a1T <- (a1M + a1F)/2

  f  <- nx[1:2] / c(1, 4)

  if (sex == "male")   ax[1:2] <- c(a0M, a1M) * f
  if (sex == "female") ax[1:2] <- c(a0F, a1F) * f
  if (sex == "total")  ax[1:2] <- c(a0T, a1T) * f

  return(ax)
}


#' Check LifeTable input
#' @param input A list containing the input arguments of the LifeTable functions.
#' @return A list of life table validated data
#' @keywords internal
LifeTable.check <- function(input) {

  with(input, {
    # ----------------------------------------------
    K <- find.my.case(Dx, Ex, mx, qx, lx, dx)
    C <- K$case
    valid_classes <- c("numeric", "matrix", "data.frame", NULL)

    if (!any(K$iclass %in% valid_classes)) {
      stop(paste0("The class of the input should be: ",
                  paste(valid_classes, collapse = ", ")), call. = FALSE)
    }
    # ----------------------------------------------
    SMS <- "contains missing values. These have been replaced with "

    if (!is.null(sex)) {
      if (!any(sex %in% c("male", "female", "total")))
        stop("'sex' should be: 'male', 'female', 'total' or 'NULL'.",
             call. = FALSE)
    }

    if (C == "C1_DxEx") {
      if (any(is.na(Dx))) warning("'Dx'", SMS, 0, call. = FALSE)
      if (any(is.na(Ex))) warning("'Ex'", SMS, 0.01, call. = FALSE)
      Dx[is.na(Dx)] <- 0
      Ex[is.na(Ex) | Ex == 0] <- 0.01
    }

    if (C == "C2_mx") {
      mx <- uxAbove100(x, mx)

    }
    if (C == "C3_qx") {
      qx <- uxAbove100(x, qx)
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
      if (!is.numeric(ax))
        stop("'ax' must be a numeric scalar (or NULL)", call. = FALSE)

      if (!any(length(ax) %in% c(1, length(x))))
        stop("'ax' must be a scalar of length 1 or a ",
             "vector of the same dimension as 'x'",
             call. = FALSE)
    }

    # Exit
    out <- list(x = x,
                Dx = Dx,
                Ex = Ex,
                mx = mx,
                qx = qx,
                lx = lx,
                dx = dx,
                sex = sex,
                lx0 = lx0,
                ax = ax,
                iclass = K$iclass,
                nLT = K$nLT,
                LTnames = K$LTnames)
    return(out)
  })
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


