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
#' @param sex sex of the population considered here. This argument affects the 
#' first two values in the life table ax column. The values are computed based 
#' on Coale-Demeny method and are sligthly different for males than for females. 
#' @param lx0 Radix. Default: 100 000
#' @return The output is of class \code{lifetable} with the components:
#' @return \item{lt}{ computed life table with rounded values}
#' @return \item{lt.exact}{ computed life table}
#' @return \item{process_date}{ time stamp}
#' @examples 
#' #' # Example 1 --- Full life table --------------
#' x <- as.numeric(rownames(ahmd$mx))
#' year <- 1900
#' 
#' mx <- ahmd$mx[, paste(year)]
#' Dx <- ahmd$Dx[, paste(year)]
#' Ex <- ahmd$Ex[, paste(year)]
#' 
#' LifeTable(x, mx = mx)
#' LifeTable(x, Dx = Dx, Ex = Ex)
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
                      qx = NULL, sex = "total", lx0 = 1e+05){
  if (!is.null(mx)) mx[is.na(mx) & x >= 100] <- max(mx, na.rm = T)
  if (!is.null(qx)) qx[is.na(qx) & x >= 100] <- max(qx, na.rm = T)
  if (!is.null(qx)) qx[is.na(qx) & x < 100] <- 1
  if (!is.null(Ex)) Ex[is.na(Ex) | Ex == 0] <- 0.01
  if (!is.null(Dx)) Dx[is.na(Dx)] <- 0
  
  if (is.null(mx) & !is.null(Dx)) { 
    mx = Dx/Ex 
  } else if (is.null(mx) & !is.null(qx)) {
    mx = mx_qx(x, qx, out = "mx")
  }
  
  if (is.null(qx) & !is.null(mx)) qx = mx_qx(x, mx, out = "qx")
  
  ax       <- coale.demeny.ax(x, mx, qx, sex)
  N        <- length(x)
  gr_names <- paste0("[", x,",", c(x[-1], "+"), ")")
  nx       <- c(diff(x), Inf)
  lx       <- lx0 * c(1, cumprod(1 - qx)[1:(N - 1)])
  dx       <- lx * qx
  Lx       <- nx*lx - (nx - ax)*dx
  Lx[N]    <- ax[N]*dx[N]
  Lx[is.na(Lx)] <- 0
  Tx       <- rev(cumsum(rev(Lx)))
  ex       <- Tx/(lx - dx*(ax/nx))
  ex[is.na(ex)] <- 0
  ex[N] <- if (ex[N - 1] == 0) 0 else ax[N]
  
  lt <- data.frame(x = x, mx = round(mx, 8), qx = round(qx, 8), 
                   ax = round(ax, 2), lx = round(lx), dx = round(dx), 
                   Lx = round(Lx), Tx = round(Tx), ex = round(ex, 2))
  lt.exact <- data.frame(x = x, mx = mx, qx = qx, ax = ax,
                         lx = lx, dx = dx, Lx = Lx, Tx = Tx, ex = ex)
  rownames(lt) = rownames(lt.exact) <- gr_names
  
  out <- list(lt = lt, lt.exact = lt.exact, process_date = date())
  out <- structure(class = "lifetable", out)
  return(out)
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
  
  a0M <- ifelse(m0 >= 0.107, 0.330, 0.045 + 2.684*m0)
  a1M <- ifelse(m0 >= 0.107, 0.330, 1.651 - 2.816*m0)
  a0F <- ifelse(m0 >= 0.107, 0.350, 0.053 + 2.800*m0)
  a1F <- ifelse(m0 >= 0.107, 0.350, 1.522 - 1.518*m0)
  a0T <- (a0M + a0F)/2
  a1T <- (a1M + a1F)/2
  
  if (sex == "male")   ax[1:2] <- c(a0M, a1M) * f
  if (sex == "female") ax[1:2] <- c(a0F, a1F) * f
  if (sex == "total")  ax[1:2] <- c(a0T, a1T) * f
  
  ax[N] <- if (mx[N] == 0) 0.5 else 1/mx[N]
  
  return(ax) 
}

#' Print lifetable
#' @keywords internal
#' @export
print.lifetable <- function(x, ...){
  cat("\nLife Table\n\n")
  tab = head_tail(x$lt, digits = 6, hlength = 5, tlength = 5, ...)
  print(tab)
} 
