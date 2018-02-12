#' Life Table Function
#' 
#' Construct either a full or abridge life table with various input choices like:
#' death counts and mid-interval population estimates \code{(Dx, Ex)} or 
#' age-specific death rates \code{(mx)} or death probabilities \code{(qx)}
#' or survivorship curve \code{(lx)} or a distribution of deaths \code{(dx)}.
#' If one of these options are specified, the other can be ignored.
#'  
#' @details The input data can be of an object of class: 
#' \code{numeric}, \code{matrix} or \code{data.frame}.
#' @usage 
#' LifeTable(x, Dx = NULL, Ex = NULL,
#'              mx = NULL,
#'              qx = NULL,
#'              lx = NULL,
#'              dx = NULL,
#'              sex = NULL,
#'              lx0 = 1e+05,
#'              ax  = NULL)
#' @param x Vector of ages at the beginning of the age interval.
#' @param Dx Object containing death counts. An element of the \code{Dx} object, 
#' represents the number of deaths during the year to persons aged x to x+n. 
#' @param Ex Exposure in the period. \code{Ex} can be approximated by the 
#' mid-year population aged x to x+n.
#' @param mx Death rate in age interval [x, x+n).
#' @param qx Probability of dying in age interval [x, x+n).
#' @param lx Probability to survive up until age x.
#' @param dx Deaths by life-table population in age interval [x, x+n).
#' @param sex Sex of the population considered here. Default: \code{NULL}. 
#' This argument affects the first two values in the life table ax column. 
#' If sex is specified the values are computed based on Coale-Demeny method 
#' and are slightly different for males than for females. 
#' Options: \code{NULL, male, female, total}.
#' @param lx0 Radix. Default: 100 000.
#' @param ax Numeric scalar. Subject-time alive in age-interval for those who 
#' die in the same interval. If \code{NULL} this will be estimated. A common 
#' assumption is \code{ax = 0.5}, i.e. the deaths occur in the middle of 
#' the interval. Default: \code{NULL}.
#' @return The output is of class \code{LifeTable} with the components:
#' @return \item{lt}{Computed life table;}
#' @return \item{call}{\code{Call} in which all of the specified arguments are 
#' specified by their full names;}
#' @return \item{process_date}{Time stamp.}
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
#' # Example 3 --- Abridge life table ------------
#' 
#' x  = c(0, 1, seq(5, 110, by = 5))
#' mx = c(.053, .005, .001, .0012, .0018, .002, .003, .004, 
#'        .004, .005, .006, .0093, .0129, .019, .031, .049, 
#'        .084, .129, .180, .2354, .3085, .390, .478, .551)
#' lt = LifeTable(x, mx = mx, sex = "female")
#' lt
#' 
#' @export
#'
LifeTable <- function(x, Dx = NULL, Ex = NULL, mx = NULL, 
                      qx = NULL, lx = NULL, dx = NULL,
                      sex = NULL, lx0 = 1e+05, ax = NULL){
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
#' 
#' @inheritParams LifeTable
#' @keywords internal
#' @export
LifeTable.core <- function(x, Dx, Ex, mx, qx, lx, dx, sex, lx0, ax){
  my.case  <- find.my.case(Dx, Ex, mx, qx, lx, dx)$case
  gr_names <- paste0("[", x,",", c(x[-1], "+"), ")")
  N        <- length(x)
  nx       <- c(diff(x), Inf)
  
  if (my.case == "C1_DxEx") {
    Dx <- as.numeric(Dx)
    Ex <- as.numeric(Ex)
    mx <- Dx/Ex 
    qx <- mx_qx(x, mx, out = "qx")
    lx <- lx0 * c(1, cumprod(1 - qx)[1:(N - 1)])
    dx <- lx * qx
  }
  if (my.case == "C2_mx") {
    mx <- as.numeric(mx)
    qx <- mx_qx(x, mx, out = "qx")
    lx <- lx0 * c(1, cumprod(1 - qx)[1:(N - 1)])
    dx <- lx * qx
  }
  if (my.case == "C3_qx") {
    qx <- as.numeric(qx)
    mx <- mx_qx(x, qx, out = "mx")
    lx <- lx0 * c(1, cumprod(1 - qx)[1:(N - 1)])
    dx <- lx * qx
  }
  if (my.case == "C4_lx") {
    lx <- as.numeric(lx)
    dx <- c(rev(diff(rev(lx))), 0)
    qx <- dx/lx
    qx[is.na(qx) & x >= 100] <- 1
    mx <- mx_qx(x, qx, out = "mx")
  }
  if (my.case == "C5_dx") {
    dx <- as.numeric(dx)
    lx <- rev(cumsum(rev(dx)))
    qx <- dx/lx
    qx[is.na(qx) & x >= 100] <- 1
    mx <- mx_qx(x, qx, out = "mx")
  }
  
  if (is.null(ax)) {
    ax <- compute.ax(x, mx, qx) 
    if (!is.null(sex)) ax <- coale.demeny.ax(x, mx, ax, sex)
  } else {
    ax <- rep(ax, N)
  }
  
  Lx    <- nx*lx - (nx - ax)*dx
  Lx[N] <- ax[N]*dx[N]
  Lx[is.na(Lx)] <- 0
  Tx    <- rev(cumsum(rev(Lx)))
  ex    <- Tx/lx  # life expectancy at the begining of the interval $e^{0}_x$
  # ex  <- Tx/(lx - dx*(ax/nx)) # life expectancy in the interval $e_x$
  ex[is.na(ex)] <- 0
  ex[N] <- if (ex[N - 1] == 0) 0 else ax[N]
  
  out <- data.frame(x.int = gr_names, x = x, mx = mx, qx = qx, ax = ax,
                    lx = lx, dx = dx, Lx = Lx, Tx = Tx, ex = ex)
  return(out)
}


#' Function that identifies the case/problem we have to solve
#' It also performs several checks
#' @inheritParams LifeTable
#' @keywords internal
#' 
find.my.case <- function(Dx = NULL, Ex = NULL, mx = NULL, 
                         qx = NULL, lx = NULL, dx = NULL) {
  input   <- c(as.list(environment()))
  my_case <- !unlist(lapply(input, is.null))
  if (sum(my_case[c(1, 2)]) == 1) {
    stop("If you input 'Dx' you must input 'Ex' as well, and viceversa", 
         call. = FALSE)
  }
  
  rn  <- c("C1_DxEx", "C2_mx", "C3_qx", "C4_lx", "C5_dx")
  cn  <- c("Dx", "Ex", "mx", "qx", "lx", "dx")
  mat <- matrix(ncol = 6, byrow = T, dimnames = list(rn,cn),
                data = c(T,T,F,F,F,F, 
                         F,F,T,F,F,F,
                         F,F,F,T,F,F,
                         F,F,F,F,T,F,
                         F,F,F,F,F,T))
  case = "C0"
  for (i in 1:nrow(mat)) if (all(my_case == mat[i, ])) case <- rn[i]
  if (case == "C0") {
    stop("Check again the input arguments. Too many inputs (Dx, Ex, mx, qx, lx, dx)", 
         call. = F)
  }
  
  X        <- input[my_case][[1]]
  my_class <- class(X)
  Aclasses <- c("numeric", "matrix", "data.frame", NULL)
  if (!(my_class %in% Aclasses)) {
    stop(paste0("The class of the input should be: ", 
                paste(Aclasses, collapse = ", ")), call. = F)
  }
  if (my_class %in% Aclasses[2:3]) {
    nLT     <- ncol(X)     # number of LTs to be created
    LTnames <- colnames(X) # the names to be assigned to LTs
  } else {
    nLT = LTnames <- NA
  }
  
  out <- list(case = case, iclass = my_class, nLT = nLT, LTnames = LTnames)
  return(out)
}



#' mx to qx
#'
#' Function to convert mx into qx and back, using the constant force of 
#' mortality assumption (CFM).
#' @inheritParams LifeTable
#' @param ux a vector of mx or qx
#' @param out type of the output: mx or qx
#' @keywords internal
mx_qx <- function(x, ux, out = "qx"){
  if (!(out %in% c("qx", "mx"))) stop("out must be: 'qx' or 'mx'", call. = FALSE)
  N     <- length(x)
  nx    <- c(diff(x), Inf)
  if (out == "qx") {
    eta <- 1 - exp(-nx*ux)
    eta[is.na(ux)] <- 1
    eta[x >= 100 & ux == 0]  <- 1
    if (max(x) > 100) eta[N] <- 1
    # eta[N] <- 1
  }
  if (out == "mx") {
    eta <- -log(1 - ux)/nx
    eta[is.infinite(eta)] <- max(eta[!is.infinite(eta)], na.rm = T)
    eta[is.na(eta)] <- max(eta, na.rm = T)
    # here if qx[N] = 1 then mx[N] = NaN therefore we apply a simple extrapolation method
    eta[N] <- eta[N - 1]^2 / eta[N - 2]
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
  a0M <- ifelse(m0 >= 0.107, 0.330, 0.045 + 2.684*m0)
  a1M <- ifelse(m0 >= 0.107, 0.330, 1.651 - 2.816*m0)
  a0F <- ifelse(m0 >= 0.107, 0.350, 0.053 + 2.800*m0)
  a1F <- ifelse(m0 >= 0.107, 0.350, 1.522 - 1.518*m0)
  a0T <- (a0M + a0F)/2
  a1T <- (a1M + a1F)/2
  
  f  <- nx[1:2] / c(1, 4)
  if (sex == "male")   ax[1:2] <- c(a0M, a1M) * f
  if (sex == "female") ax[1:2] <- c(a0F, a1F) * f
  if (sex == "total")  ax[1:2] <- c(a0T, a1T) * f
  
  return(ax) 
}

#' Check LifeTable input 
#' @param input a list containing the input arguments of the LifeTable functions
#' @keywords internal
LifeTable.check <- function(input) {
  with(input, {
    Y    <- find.my.case(Dx, Ex, mx, qx, lx, dx)
    C    <- Y$case
    SMS1 <- "contains missing values."
    SMS2 <- "NA's were replaced with"
    
    if (!is.null(sex)) {
      if (!(sex %in% c("male", "female", "total"))) 
        stop("'sex' should be: 'male', 'female', 'total' or 'NULL'.", call. = F)
    }
    if (C == "C1_DxEx") {
      if (any(is.na(Dx))) warning(paste("'Dx'", SMS1, SMS2, 0), call. = F)
      if (any(is.na(Ex))) warning(paste("'Ex'", SMS1, SMS2, 0.01), call. = F)
      Dx[is.na(Dx)] <- 0
      Ex[is.na(Ex) | Ex == 0] <- 0.01
    }
    if (C == "C2_mx") {
      if (any(is.na(mx))) {
        warning(paste("'mx'", SMS1, SMS2, "maximum observed mx:", 
                      max(mx, na.rm = T)), call. = F)
        mx[is.na(mx)] <- max(mx, na.rm = T)
      }
    }
    if (C == "C3_qx") {
      c1 <- is.na(qx[length(qx)])
      n  <- length(qx)
      c2 <- is.na(qx[-n][x[-n] >= 100])
      if (any(c2)) {
        warning(paste("'qx' contains several missing values over the age of 100.",
                      "NA's were replaced with", round(max(qx, na.rm = T)[1], 4)), 
                call. = F)
        qx[is.na(qx) & x >= 100] <- max(qx, na.rm = T)[1]
      }
      if (any(c1)) {
        warning(paste("'qx' ultimate is NA. It is replaces with 1."), call. = F)
        qx[length(qx)] <- 1
      }
    }
    if (C == "C4_lx") {
      if (any(is.na(lx))) warning(paste("'lx'", SMS1, SMS2, 0), call. = F)
      lx[is.na(lx) & x >= 100] <- 0
    }
    if (C == "C5_dx") {
      if (any(is.na(dx))) warning(paste("'dx'", SMS1, SMS2, 0), call. = F)
      dx[is.na(dx)] <- 0
    }
    
    if (!is.null(ax)) {
      if (!is.numeric(ax)) stop("'ax' must be a numeric scalar (or NULL)", call. = F)
      if (length(ax) != 1) stop("Provide a single values for 'ax'", call. = F)
    }
    
    out <- list(x = x, Dx = Dx, Ex = Ex, mx = mx, qx = qx, 
                lx = lx, dx = dx, sex = sex, lx0 = lx0, ax = ax,
                iclass = Y$iclass, nLT = Y$nLT, LTnames = Y$LTnames)
    return(out)
  })
}

#' Print LifeTable
#' @param x an object of class \code{"LifeTable"}
#' @param ... further arguments passed to or from other methods.
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
  type1 <- if (all(step == 1)) "Full" else "Abridge"
  type2 <- if (nlt == 1) "Life Table" else "Life Tables"
  
  cat("\n", type1, " ", type2, "\n\n", sep = "")
  cat("Number of life tables:", nlt, "\n")
  cat("Dimension:", nrow(LT), "x", ncol(LT), "\n")
  cat("Age intervals:", head_tail(lt$x.int, hlength = 3, tlength = 3), "\n\n")
  print(out, row.names = FALSE)
} 
