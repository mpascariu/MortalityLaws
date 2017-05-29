
#' Life table function
#' 
#' Function to create a life table with input variables: (age, Dx, Ex) 
#' or (age, mx) or (age, qx)
#' @param x Vector of ages
#' @param Dx Vector containing death counts. An element of the vector, Dx, 
#' represents the number of deaths during the year to persons aged x to x+1 
#' @param Ex Vector containing the exposure in the period. 
#' Ex is the mid-year population aged x to x+1 
#' @param mx Age-specific death rates
#' @param qx 1-year pobability of dying between age x and x+1
#' @param lx0 Radix
#' @param ax0 average time spent between age 0 and age 1 of those how died 
#' in age 0. For x > 0  we assume an uniform distribution of deaths (UDD).
#' @return Life Tables
#' @examples 
#' library(MortalityLaws)
#' 
#' F_Dx <- ahmd$Dx
#' F_Nx <- ahmd$Nx
#' F_mx <- ahmd$mx
#' 
#' ages <- as.numeric(rownames(F_mx))
#' year <- 1900
#' 
#' mx <- F_mx[, paste(year)]
#' Dx <- F_Dx[, paste(year)]
#' Nx <- F_Nx[, paste(year)]
#' 
#' LifeTable(x = ages, mx = mx)$lt
#' LifeTable(x = ages, Dx = Dx, Ex = Nx)$lt
#' @export
#'
LifeTable <- function(x, Dx = NULL, Ex = NULL, mx = NULL, 
                      qx = NULL, lx0 = 1e+05, ax0 = 0.1){
  if (!is.null(mx)) mx[is.na(mx)] <- 0
  if (!is.null(qx)) qx[is.na(qx)] <- 0
  if (!is.null(Ex)) Ex[is.na(Ex) | Ex == 0] <- 0.01
  if (!is.null(Dx)) Dx[is.na(Dx)] <- 0
  
  nmax     <- length(x)
  n        <- rep(1,nmax)           # width of the intervals
  ax       <- n/2
  if (min(x) == 0) ax[1] <- ax0
  
  mx <- if (length(Dx) > 0) { Dx/Ex } else { 
          if (length(mx) > 0) { mx } else { 
            qx/(n - qx*(n - ax)) }
          }
  if (mx[nmax] < 0.5 | is.na(mx[nmax])) mx[nmax] = mx[nmax - 1]*1.1 
  # In small populations we could have problems 
  # in estimating a reliable mx at last age in the lifetable
  ax[nmax] <- if (mx[nmax] == 0) 0.5 else 1/mx[nmax]
  
  qx       <- if (length(qx) > 0) {qx} 
  else{n*mx / (1 + (n - ax)*mx)} 
  qx[x >= 100 & mx == 0] <- 1
  qx[nmax] <- 1
  
  lx       <- c(1,cumprod(1 - qx))*lx0 
  lx       <- lx[1:nmax]
  dx       <- lx*qx
  Lx       <- n*lx - ax*dx
  Lx[nmax] <- ax[nmax]*dx[nmax]
  Lx[is.na(Lx)] <- 0
  Tx       <- rev(cumsum(rev(Lx)))
  # Tx[nmax] <- max(dx[nmax,], Lx[nmax])
  ex       <- Tx/Lx
  ex[is.na(ex)] <- 0
  ex[nmax] <- if (ex[nmax - 1] == 0) 0 else ax[nmax]
  
  lt <- data.frame(age = x, mx = round(mx,6), qx = round(qx,6), ax = ax, 
                   lx = round(lx), dx = round(dx), Lx = round(Lx), 
                   Tx = round(Tx), ex = round(ex,2))
  lt.exact <- data.frame(age = x, mx = mx, qx = qx, ax = ax,
                         lx = lx, dx = dx, Lx = Lx, Tx = Tx, ex = ex)
  out <- list(lt = lt, lt.exact = lt.exact, process_date = date())
  return(out)
}



