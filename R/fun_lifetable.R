#' Life table function
#' 
#' Function to create a life table with various choices of 2 input vectors: 
#' \code{(x, Dx, Ex)} or \code{(x, mx)} or \code{(x, qx)}.
#' @param x Vector of ages
#' @param Dx Vector containing death counts. An element of the vector, Dx, 
#' represents the number of deaths during the year to persons aged x to x+1 
#' @param Ex Vector containing the exposure in the period. 
#' Ex is the mid-year population aged x to x+1 
#' @param mx age-specific death rates
#' @param qx probability of dying between age x and x+n
#' @param lx probability to survive up until age x
#' @param dx life table death counts at age x
#' @param sex sex of the population considered here. This argument affects the 
#' first two values in the life table ax column. The values are computed based 
#' on Coale-Demeny method and are sligthly different for males than for females. 
#' @param lx0 Radix. Default: 100 000
#' @return The output is of class \code{lifetable} with the components:
#' @return \item{lt}{ computed life table with rounded values}
#' @return \item{lt.exact}{ computed life table}
#' @return \item{process_date}{ time stamp}
#' @examples 
#' # Example 1 --- Full life table --------------
#'  
#' y  <- 1900
#' x  <- as.numeric(rownames(ahmd$mx))
#' Dx <- ahmd$Dx[, paste(y)]
#' Ex <- ahmd$Ex[, paste(y)]
#' 
#' LT1 <- LifeTable(x, Dx = Dx, Ex = Ex)
#' LT2 <- LifeTable(x, mx = LT1$lt.exact$mx)
#' LT3 <- LifeTable(x, qx = LT1$lt.exact$qx)
#' LT4 <- LifeTable(x, lx = LT1$lt.exact$lx)
#' LT5 <- LifeTable(x, dx = LT1$lt.exact$dx)
#'
#' LT1
#' ls(LT5) 
#' 
#' # Example 2 --- Abridge life table ------------
#' x  = c(0, 1, seq(5, 110, by = 5))
#' mx = c(.053, .005, .001, .0012, .0018, .002, .003, .004, 
#'        .004, .005, .006, .0093, .0129, .019, .031, .049, 
#'        .084, .129, .180, .2354, .3085, .390, .478, .551)
#' lt = LifeTable(x, mx = mx, sex = "female")
#' lt
#' @export
#'
LifeTable <- function(x, Dx = NULL, Ex = NULL, mx = NULL, 
                      qx = NULL, lx = NULL, dx = NULL,
                      sex = "total", lx0 = 1e+05){
  input <- c(as.list(environment()))
  X     <- LifeTable.check(input)
  
  LT <- with(X, LifeTable.core(x, Dx, Ex, mx, qx, lx, dx, sex, lx0))
  lt <- data.frame(x = x, mx = round(LT$mx, 8), qx = round(LT$qx, 8), 
                   ax = round(LT$ax, 2), lx = round(LT$lx), dx = round(LT$dx), 
                   Lx = round(LT$Lx), Tx = round(LT$Tx), ex = round(LT$ex, 2))
  out <- list(lt = lt, lt.exact = LT, process_date = date())
  out <- structure(class = "LifeTable", out)
  
  return(out)
}

#' Check LifeTable input 
#' @keywords internal
#' 
LifeTable.check <- function(input) {
  with(input, {
    C    <- find.my.case(Dx, Ex, mx, qx, lx, dx)
    SMS1 <- "contains missing values"
    SMS2 <- "NA's were replaced with 0."
    
    if (C == "C1_DxEx") {
      if (any(is.na(Dx))) warning(paste("'Dx'", SMS1, SMS2), call. = F)
      if (any(is.na(Ex))) warning(paste("'Ex'", SMS1, SMS2), call. = F)
      Dx[is.na(Dx)] <- 0
      Ex[is.na(Ex) | Ex == 0] <- 0.01
    }
    if (C == "C2_mx") {
      c1 <- is.na(mx[x <  100])
      c2 <- is.na(mx[x >= 100])
      if (any(c1)) stop(paste("'mx'", SMS1), call. = F)
      if (any(c2)) {
        warning(paste("'mx'", SMS1, SMS2), call. = F)
        mx[c2] <- max(mx, na.rm = T)
      }
    }
    if (C == "C3_qx") {
      qx[is.na(qx) & x >= 100] <- max(qx, na.rm = T)
      qx[is.na(qx) & x <  100] <- 1
    }
    if (C == "C4_lx") {
      lx[is.na(lx) & x >= 100] <- 0
    }
    if (C == "C5_dx") {
      if (any(is.na(dx))) warning(paste("'dx'", SMS1, SMS2), call. = F)
      dx[is.na(dx)] <- 0
    }
    out <- list(x = x, Dx = Dx, Ex = Ex, mx = mx, qx = qx, 
                lx = lx, dx = dx, sex = sex, lx0 = lx0)
    return(out)
  })
}

#' LifeTable.core
#' 
#' @inheritParams LifeTable
#' @keywords internal
#' @export
LifeTable.core <- function(x, Dx = NULL, Ex = NULL, mx = NULL, 
                           qx = NULL, lx = NULL, dx = NULL,
                           sex, lx0 = 1e+05){
  my.case  <- find.my.case(Dx, Ex, mx, qx, lx, dx)
  gr_names <- paste0("[", x,",", c(x[-1], "+"), ")")
  N        <- length(x)
  nx       <- c(diff(x), Inf)
  
  if (my.case == "C1_DxEx") {
    mx <- Dx/Ex 
    qx <- mx_qx(x, mx, out = "qx")
    lx <- lx0 * c(1, cumprod(1 - qx)[1:(N - 1)])
    dx <- lx * qx
  }
  if (my.case == "C2_mx") {
    qx <- mx_qx(x, mx, out = "qx")
    lx <- lx0 * c(1, cumprod(1 - qx)[1:(N - 1)])
    dx <- lx * qx
  }
  if (my.case == "C3_qx") {
    mx <- mx_qx(x, qx, out = "mx")
    lx <- lx0 * c(1, cumprod(1 - qx)[1:(N - 1)])
    dx <- lx * qx
  }
  if (my.case == "C4_lx") {
    dx <- c(rev(diff(rev(lx))), 0)
    qx <- dx/lx
    mx <- mx_qx(x, qx, out = "mx")
  }
  if (my.case == "C5_dx") {
    lx <- rev(cumsum(rev(dx)))
    qx <- dx/lx
    mx <- mx_qx(x, qx, out = "mx")
  }
  
  ax       <- coale.demeny.ax(x, mx, qx, sex)
  Lx       <- nx*lx - (nx - ax)*dx
  Lx[N]    <- ax[N]*dx[N]
  Lx[is.na(Lx)] <- 0
  Tx       <- rev(cumsum(rev(Lx)))
  ex       <- Tx/(lx - dx*(ax/nx))
  ex[is.na(ex)] <- 0
  ex[N]   <- if (ex[N - 1] == 0) 0 else ax[N]
  
  out <- data.frame(x = x, mx = mx, qx = qx, ax = ax,
                    lx = lx, dx = dx, Lx = Lx, Tx = Tx, ex = ex)
  rownames(out) <- gr_names
  return(out)
}




#' Function that determintes the case/problem we have to solve
#' It also performes some checks
#' @keywords internal
#' 
find.my.case <- function(Dx, Ex, mx, qx, lx, dx) {
  my_case <- !unlist(lapply(list(Dx, Ex, mx, qx, lx, dx), is.null))
  
  if (sum(my_case[c(1, 2)]) == 1) stop("If you input 'Dx' you must input 'Ex' as well, and viceversa", call. = FALSE)
  
  case = "C0"
  if (all(my_case == c(T,T,F,F,F,F))) case = "C1_DxEx" 
  if (all(my_case == c(F,F,T,F,F,F))) case = "C2_mx"
  if (all(my_case == c(F,F,F,T,F,F))) case = "C3_qx" 
  if (all(my_case == c(F,F,F,F,T,F))) case = "C4_lx" 
  if (all(my_case == c(F,F,F,F,F,T))) case = "C5_dx" 
  if (case == "C0") stop("Check again the input arguments. Too many inputs (Dx, Ex, mx, qx, lx, dx)", call. = F)
  
  return(case)
}


#' mx to qx
#'
#' Function to convert mx into qx and back, using the constant force of 
#' mortality assumption (CFM).
#' @param ux a vector of mx or qx
#' @keywords internal
mx_qx <- function(x, ux, out = "qx"){
  if (!(out %in% c("qx", "mx"))) stop("out must be: 'qx' or 'mx'", call. = FALSE)
  N     <- length(x)
  nx    <- c(diff(x), Inf)
  if (out == "qx") {
    eta = 1 - exp(-nx*ux)
    eta[is.na(ux)] <- 1
    eta[x >= 100 & ux == 0] <- 1
    eta[N] <- 1
  }
  if (out == "mx") {
    eta = -log(1 - ux)/nx
    eta[is.infinite(eta)] <- max(eta[!is.infinite(eta)], na.rm = T)
    eta[is.na(eta)] <- max(eta, na.rm = T)
    # here if qx[N] = 1 then mx[N] = NaN therefore we apply a simple extrapolation method
    eta[N] = eta[N - 1]^2 / eta[N - 2]
  }
  return(eta)
}

#' Find ax indicator using the Coale-Demeny coefficients
#' 
#' ax - the point in the age internal where 50% of the deaths have already occurred
#' @keywords internal
#' 
coale.demeny.ax <- function(x, mx, qx, sex) {
  if (mx[1] < 0) stop("'m[1]' must be greater than 0", call. = F)
  
  nx <- c(diff(x), Inf)
  N  <- length(x)
  f  <- nx[1:2] / c(1, 4)
  m0 <- mx[1]
  ax <- nx + 1/mx - nx/qx
  for (i in 1:(N - 1)) { 
    if (is.infinite(ax[i + 1]) | is.na(ax[i + 1])) ax[i + 1] = ax[i]
  }
  
  # Here we adjust the first two values to account for infant mortality more accurately
  a0M <- ifelse(m0 >= 0.107, 0.330, 0.045 + 2.684*m0)
  a1M <- ifelse(m0 >= 0.107, 0.330, 1.651 - 2.816*m0)
  a0F <- ifelse(m0 >= 0.107, 0.350, 0.053 + 2.800*m0)
  a1F <- ifelse(m0 >= 0.107, 0.350, 1.522 - 1.518*m0)
  a0T <- (a0M + a0F)/2
  a1T <- (a1M + a1F)/2
  
  if (sex == "male")   ax[1:2] <- c(a0M, a1M) * f
  if (sex == "female") ax[1:2] <- c(a0F, a1F) * f
  if (sex == "total")  ax[1:2] <- c(a0T, a1T) * f
  
  return(ax) 
}

#' Print lifetable
#' @keywords internal
#' @export
print.LifeTable <- function(x, ...){
  cat("\nLife Table\n\n")
  tab = head_tail(x$lt, digits = 6, hlength = 5, tlength = 5, ...)
  print(tab)
} 
