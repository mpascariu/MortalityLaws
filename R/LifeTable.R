#' Compute Life Tables from Mortality Data
#' 
#' Construct either a full or abridged life table with various input choices like:
#' death counts and mid-interval population estimates \code{(Dx, Ex)} or 
#' age-specific death rates \code{(mx)} or death probabilities \code{(qx)}
#' or survivorship curve \code{(lx)} or a distribution of deaths \code{(dx)}.
#' If one of these options is specified, the other can be ignored. The input 
#' data can be an object of class: numerical \code{vector}, \code{matrix} or 
#' \code{data.frame}.
#' 
#' @details 
#' The "life table" is also called "mortality table" or "actuarial table".
#' This shows, for each age, what the probability is that a person of that 
#' age will die before his or her next birthday, the expectation of life across 
#' different age ranges or the survivorship of people from a certain population.
#' @usage 
#' LifeTable(x, Dx = NULL, Ex = NULL,
#'              mx = NULL,
#'              qx = NULL,
#'              lx = NULL,
#'              dx = NULL,
#'              sex = NULL,
#'              lx0 = 1e5,
#'              ax  = NULL)
#' @param x Vector of ages at the beginning of the age interval.
#' @param Dx Object containing death counts. An element of the \code{Dx} object 
#' represents the number of deaths during the year to persons aged x to x+n. 
#' @param Ex Exposure in the period. \code{Ex} can be approximated by the 
#' mid-year population aged x to x+n.
#' @param mx Death rate in age interval [x, x+n).
#' @param qx Probability of dying in age interval [x, x+n).
#' @param lx Probability of survival up until age x.
#' @param dx Deaths by life-table population in the age interval [x, x+n).
#' @param sex Sex of the population considered here. Default: \code{NULL}. 
#' This argument affects the first two values in the life table ax column. 
#' If sex is specified the values are computed based on the Coale-Demeny method 
#' and are slightly different for males than for females. 
#' Options: \code{NULL, male, female, total}.
#' @param lx0 Radix. Default: 100 000.
#' @param ax Numeric scalar. Subject-time alive in age-interval for those who 
#' die in the same interval. If \code{NULL} this will be estimated. A common 
#' assumption is \code{ax = 0.5}, i.e. the deaths occur in the middle of 
#' the interval. Default: \code{NULL}.
#' @return The output is of the \code{"LifeTable"} class with the components:
#'  \item{lt}{Computed life table;}
#'  \item{call}{\code{Call} in which all of the specified arguments are 
#'  specified by their full names;}
#'  \item{process_date}{Time stamp.}
#' @seealso 
#' \code{\link{LawTable}}
#' \code{\link{convertFx}}
#' @author Marius D. Pascariu
#' @examples 
#' # Example 1 --- Full life tables with different inputs ---
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
#' # Example 2 --- Compute multiple life tables at once ---
#' 
#' LTs = LifeTable(x, mx = ahmd$mx)
#' LTs
#' # A warning is printed if the input contains missing values. 
#' # Some of the missing values can be handled by the function.
#' 
#' # Example 3 --- Abridged life table ------------
#' 
#' x  = c(0, 1, seq(5, 110, by = 5))
#' mx = c(.053, .005, .001, .0012, .0018, .002, .003, .004, 
#'        .004, .005, .006, .0093, .0129, .019, .031, .049, 
#'        .084, .129, .180, .2354, .3085, .390, .478, .551)
#' lt = LifeTable(x, mx = mx, sex = "female")
#' lt
#' 
#' @export
LifeTable <- function(x, Dx = NULL, Ex = NULL, mx = NULL, 
                      qx = NULL, lx = NULL, dx = NULL,
                      sex = NULL, lx0 = 1e5, ax = NULL){
  input <- c(as.list(environment()))
  X     <- LifeTable.check(input)
  LT    <- NULL
  
  if (X$iclass == "numeric") {
    LT <- with(X, LifeTable.core(x, Dx, Ex, mx, qx, lx, dx, sex, lx0, ax))
  } else {
    for (i in 1:X$nLT) {
      LTi <- with(X, LifeTable.core(x, Dx[,i], Ex[,i], mx[,i], 
                                    qx[,i], lx[,i], dx[,i], sex, lx0, ax))
      LTn <- if (is.na(X$LTnames[i])) i else  X$LTnames[i]
      LTi <- cbind(LT = LTn, LTi)
      LT  <- rbind(LT, LTi)
    }
  }
  
  out <- list(lt = LT, process_date = date())
  out <- structure(class = "LifeTable", out)
  out$call <- match.call()
  return(out)
}


#' LifeTable.core
#' @inheritParams LifeTable
#' @keywords internal
LifeTable.core <- function(x, Dx, Ex, mx, qx, lx, dx, sex, lx0, ax){
  my.case  <- find.my.case(Dx, Ex, mx, qx, lx, dx)$case
  gr_names <- paste0("[", x,",", c(x[-1], "+"), ")")
  N  <- length(x)
  df <- diff(x)
  nx <- c(df, df[N - 1]) 
  
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
  } else {
    ax <- rep(ax, N)
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
  
  out <- data.frame(x.int = gr_names, x = x, mx = mx, qx = qx, ax = ax,
                    lx = lx, dx = dx, Lx = Lx, Tx = Tx, ex = ex)
  return(out)
}


#' Function that identifies the case/problem we have to solve
#' @inheritParams LifeTable
#' @keywords internal
find.my.case <- function(Dx = NULL, Ex = NULL, mx = NULL, 
                         qx = NULL, lx = NULL, dx = NULL) {
  input   <- c(as.list(environment()))
  
  # Matrix of possible cases --------------------
  rn  <- c("C1_DxEx", "C2_mx", "C3_qx", "C4_lx", "C5_dx")
  cn  <- c("Dx", "Ex", "mx", "qx", "lx", "dx")
  mat <- matrix(ncol = 6, byrow = T, dimnames = list(rn, cn),
                data = c(T,T,F,F,F,F, 
                         F,F,T,F,F,F,
                         F,F,F,T,F,F,
                         F,F,F,F,T,F,
                         F,F,F,F,F,T))
  # ----------------------------------------------
  L1 <- !unlist(lapply(input, is.null))
  L2 <- apply(mat, 1, function(x) all(L1 == x))
  my_case <- rn[L2]
  
  if (sum(L1[c(1, 2)]) == 1) {
    stop("If you input 'Dx' you must input 'Ex' as well, and viceversa", call. = F)
  }
  if (!any(L2)) {
    stop("The input is not specified correctly. Check again the function ",
         "arguments and make sure the input data is added properly.", call. = F)
  }
  
  X   <- input[L1][[1]]
  nLT <- LTnames <- NA
  if (!is.vector(X)) {
    nLT     <- ncol(X)     # number of LTs to be created
    LTnames <- colnames(X) # the names to be assigned to LTs
  }
  
  out <- list(case = my_case, iclass = class(X), nLT = nLT, LTnames = LTnames)
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
#' @keywords internal
mx_qx <- function(x, nx, ux, out = c("qx", "mx")){
  out <- match.arg(out)
  
  if (out == "qx") {
    eta <- 1 - exp(-nx * ux)
  } else {
    eta <- suppressWarnings(-log(1 - ux)/nx)
    # If qx[last-age] = 1 then mx[last-age] = Inf. Not nice to have Inf's; they
    # distort the results in the subsequent processes. 
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
#' @keywords internal
uxAbove100 <- function(x, ux, omega = 100, verbose = FALSE) {
  
  if (is.vector(ux)) {
    L <- x >= 100 & (is.na(ux) | is.infinite(ux) | ux == 0)
    if (any(L)) {
      mux   <- max(ux[!L])
      ux[L] <- mux
      if (verbose) warning("The input data contains NA's, Inf or zero's over the age of 100. ", 
                           "These have been replaced with maximum observed value: ", 
                           round(mux, 4), call. = F)
    }
    
  } else {
    for (i in 1:ncol(ux)) ux[, i] = uxAbove100(x, ux[, i], omega, verbose)
  }
  return(ux)
}


#' dx to lx
#' 
#' Function to convert dx into lx and back
#' @param ux A vector of dx or lx data.
#' @param out Type of the output: dx or lx.
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
#' @return \code{ax} - the point in the age internal where 50% of the deaths 
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
#' Here we adjust the first two values of ax to account for infant mortality more accurately
#' 
#' @inheritParams LifeTable
#' @keywords internal
coale.demeny.ax <- function(x, mx, ax, sex) {
  if (mx[1] < 0) stop("'m[1]' must be greater than 0", call. = F)
  nx <- c(diff(x), Inf)
  m0 <- mx[1]
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
#' @keywords internal
LifeTable.check <- function(input) {
  
  with(input, {
    # ----------------------------------------------
    K <- find.my.case(Dx, Ex, mx, qx, lx, dx)
    C <- K$case
    valid_classes <- c("numeric", "matrix", "data.frame", NULL)
    if (!(K$iclass %in% valid_classes)) {
      stop(paste0("The class of the input should be: ", 
                  paste(valid_classes, collapse = ", ")), call. = F)
    }
    # ----------------------------------------------
    SMS <- "contains missing values. These have been replaced with "
    
    if (!is.null(sex)) {
      if (!(sex %in% c("male", "female", "total"))) 
        stop("'sex' should be: 'male', 'female', 'total' or 'NULL'.", call. = F)
    }
    if (C == "C1_DxEx") {
      Dx[is.na(Dx)] <- 0
      Ex[is.na(Ex) | Ex == 0] <- 0.01
      if (any(is.na(Dx))) warning("'Dx'", SMS, 0, call. = F)
      if (any(is.na(Ex))) warning("'Ex'", SMS, 0.01, call. = F)
    }
    if (C == "C2_mx") {
      mx <- uxAbove100(x, mx)
    }
    if (C == "C3_qx") {
      qx <- uxAbove100(x, qx)
    }
    if (C == "C4_lx") {
      lx[is.na(lx) & x >= 100] <- 0
      if (any(is.na(lx))) warning("'lx'", SMS, 0, call. = F)
    }
    if (C == "C5_dx") {
      dx[is.na(dx)] <- 0
      if (any(is.na(dx))) warning("'dx'", SMS, 0, call. = F)
    }
    if (!is.null(ax)) {
      if (!is.numeric(ax)) stop("'ax' must be a numeric scalar (or NULL)", call. = F)
      if (length(ax) != 1) stop("Provide a single values for 'ax'", call. = F)
    }
    
    out <- list(x = x, Dx = Dx, Ex = Ex, mx = mx, qx = qx, 
                lx = lx, dx = dx, sex = sex, lx0 = lx0, ax = ax,
                iclass = K$iclass, nLT = K$nLT, LTnames = K$LTnames)
    return(out)
  })
}


#' Print LifeTable
#' @param x An object of class \code{"LifeTable"}
#' @param ... Further arguments passed to or from other methods.
#' @keywords internal
#' @export
print.LifeTable <- function(x, ...){
  LT <- x$lt
  lt <- with(LT, data.frame(x.int = x.int, x = x, mx = round(mx, 6), 
                            qx = round(qx, 6), ax = round(ax, 2), lx = round(lx), 
                            dx = round(dx), Lx = round(Lx), Tx = round(Tx), 
                            ex = round(ex, 2)))
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


