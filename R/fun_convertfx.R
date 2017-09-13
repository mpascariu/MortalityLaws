
#' Convert the \code{"mx", "qx", "fx"} functions 
#' 
#' This function can be used to convert the \code{"mx", "qx", "fx"} functions between them.
#' For example to transform a vector of age-specific death rates (\code{mx}) into a 
#' empirical density function (\code{fx}) or 1-year probabilities of death (\code{qx}).
#' The \code{mx/qx} conversion follows the Chiang method (Shoen, 1978).
#' The \code{mx/fx} conversion is made using the \href{https://en.wikipedia.org/wiki/Gauss-Newton_algorithm}{Gauss-Newton algorithm}
#' 
#' @param data numerical vector 
#' @param x numerical vector (of ages)
#' @param type What type of data did you add as input? Accepted values: 
#' \code{"mx", "qx", "fx"}.
#' @param output What type of data do you want to obtain? Accepted values: 
#' \code{"mx", "qx", "fx"}. 
#' @source Schoen, R. Demography (1978) 15: 625. doi:10.2307/2061212
#' @return A vector or a data.frame with values for the specified functions. 
#' @examples 
#' library(MortalityLaws)
#'
#' # Example 1: ---------------------------------------
#' # Estimate hazard rates (mx) starting from a density (fx)
#' x   <- 0:105
#' fxT <- dlogis(x, location = 45, scale = 6) # true fx
#' hxE <- convertFx(data = fxT, x, type = 'fx', output = 'mx') # estimated hazard
#' 
#' 
#' # Example 2: ---------------------------------------
#' # Estimate death probabilities (qx) starting from hazard rates (mx). 
#' # Use a data.frame as input. 
#' mx <- ahmd$mx
#' x  <- as.numeric(rownames(mx))
#' qx <- convertFx(data = mx, x, type = 'mx', output = 'qx')
#' 
#' @export
#' 
convertFx <- function(data, x, type, output) {
  input <- c(as.list(environment()))
  check.convertFx(input)
  
  if (is.data.frame(data) | is.matrix(data)) {
    out <- data*0
    for (i in 1:ncol(data)) out[, i] = convertFx(data[, i], x, type, output)
  } else {
    # Step 1. - Convert everything to hazard rates (or age specific death rates)
    if (type == 'fx') hx = hx_from_fx(x, fx = data)
    if (type == 'qx') hx = mx_qx(ux = data, x, out = 'mx')
    if (type %in% c('mx', 'hx')) hx = data
    
    # Step 2. - Convert hazard rates into the specified output
    if (output == 'fx' & type != 'fx') out = fx_from_hx(x, hx = hx, delta = 0.25)$fx else out = data
    if (output == 'qx' & type != 'qx') out = mx_qx(ux = hx, x, out = 'qx') else out = data
    if (output %in% c('mx', 'hx')) out = hx
  }
  return(out)
}


#' mx to qx
#'
#' @keywords internal
mx_qx <- function(ux, x, out = 'qx'){
  n <- diff(x) # Width of age interval 
  n <- c(n, n[length(x) - 1])
  
  if (out == 'qx') {
    ax   = n + 1/ux - n/(1 - exp(-n*ux))
    vect = n*ux / (1 + (n - ax)*ux)
  }
  if (out == 'mx') {
    ax   = -n/ux - n/log(1 - ux) + n
    vect = ux/(n - ux*(n - ax))
  }
  return(vect)
}




# ----------------------------------------------
# FUNCTIONS for hx/fx conversions
# All credit goes to Ugofilippo Basellini & Giancarlo Camarda for the implementation 
# of the hx/fx conversion using the Gauss-Newton algorithm. Marius Pascariu brought 
# minor changes, mainly he just rewrote and commented the code more elegantly.
# Date: 24/03/2017

#' Functions for computing fx and the gradient from mx 
#' @keywords internal
#' 
fx_from_hx <- function(x, hx, delta = diff(x)[1]) {
  # Wed Jul  5 17:42:16 2017 ------------------------------
  # if you want to change the value of delta use 0.1 or 0.2. Higer values will not 
  # improve much the results, lower values will slow down the algorithm. 
  select_values <- x %in% x
  if (delta != diff(x)[1]) {
    long_x  = seq(x[1], x[length(x)], by = delta)
    select_values <- long_x %in% x
    lo = loess(hx ~ x, span = 0.1)
    long_hx = predict(lo, long_x)
    x  <- long_x
    hx <- long_hx 
  }
  # ----------------------------------------------
  
  n_    <- length(x)  # length of age vector
  I     <- diag(n_)   # identity matrix
  C     <- lower.tri(I, diag = TRUE) # matrix with logicals
  C[C == 1] <- -delta 
  
  fx  <- hx * exp(C %*% hx)  # compute fx
  
  ## compute gradient
  M1  <- C %*% diag(as.vector(hx)) + I
  V   <- diag(as.vector(fx))
  G   <- V %*% M1
  
  out <- list(fx = fx[select_values], gradient = G)
  return(out)
}



#' Function for obtaining hx from fx
#' @keywords internal
#' 
hx_from_fx <- function(x, fx, delta = diff(x)[1]){
  # First attempt
  start_eta = find_start_eta(fx) # eta = log(hx)
  eta_hat   = find_optim_eta(x, start_eta[[2]], fx, step = 0.1, iter = 15)
  
  # Second attempt
  check = is_eta_ok(eta_hat)
  if (!check) {
    eta_hat = find_optim_eta(x, start_eta[[1]], fx, step = 1, iter = 100)
  }
  
  out <- exp(eta_hat)
  return(out)
}



#' find some starting eta
#' @keywords internal
#' 
find_start_eta <- function(fx) {
  m    <- length(fx)
  Sx   <- pmax(0, 1 - cumsum(fx)) # numerical approximation of survival
  vsn  <- 10^-10                  # very small number
  eta1 <- log(fx/Sx + vsn)
  
  if (eta1[m] == "Inf" | eta1[m] == "NaN") eta1[m] = eta1[m - 1] 
  
  # A second eta function which is monotonically increasing
  eta2 <- eta1
  for (j in 2:m) { if (eta2[j] < eta2[j - 1])  eta2[j] = eta2[j - 1]} 
  
  out <- list(eta1, eta2)
  return(out)
}

#' @keywords internal
#' 
find_optim_eta <- function(x, eta, fx, step = 0.25, iter = 400) {
  good_eta = FALSE
  i = 1
  
  while (good_eta == FALSE) {
    lambda_i <- i*step
    eta_hat  <- iter.func(x, eta, fx, lambda_i)
    good_eta <- is_eta_ok(eta_hat)
    i <- i + 1
    if (i > iter) break
  }
  return(eta_hat)
}

#' @keywords internal
#' 
is_eta_ok <- function(data) {
  m     = length(data)
  cond1 = is.na(data)[m] == FALSE
  cond2 = data[m] >= data[m - 1] 
  cond3 = exp(data[m]) >= 0.001
  cond4 = max(data) != Inf
  out <- (cond1 & cond2 & cond3 & cond4)
  return(out)
}


#' iteration search to find hx
#' @keywords internal
#' 
iter.func <- function(x, eta, fx, lambda, iter = 100, tol = 10^-4){
  m <- length(x)
  ## define smoothness penalty
  D   <- diff(diag(m), diff = 2)
  tDD <- t(D) %*% D
  P   <- lambda * tDD
  
  for (it in 1:iter) {
    ## current f
    fn <- fx_from_hx(x, hx = exp(eta))
    f  <- fn$fx          # density
    G  <- fn$gradient    # gradient
    r  <- fx - f         # residuals
    f2 <- as.vector(f^2) # compute weigths 
    
    ## regression
    WG <- G / f2                                # weighted gradient
    Wr <- r / f2                                # weigthed residuals
    A_ <- t(G) %*% WG + P                       # matrix A
    B_ <- t(G) %*% Wr + (t(G) %*% WG ) %*% eta  # matrix B
    
    eta.old <- eta       # old eta
    eta <- solve(A_, B_) # new eta. This is ( A_ %*% eta = B_ ) solved for eta
    
    ## Conditions for stoping the algorithm: IF any TRUE then break.
    cond1 <- is.nan(eta[m])
    cond2 <- exp(eta[m]) <= 0.01 # last value of hx should be at least 0.01
    cond3 <- max(abs(eta - eta.old)) < tol # convergence
    if (cond1 | cond2 | cond3) break
  }
  
  out <- as.numeric(eta)
  return(out)
}
