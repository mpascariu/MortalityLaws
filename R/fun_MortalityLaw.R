
#' Integarte hazard funcion
#' @keywords internal
#' 
int_hazard <- function(x, par, ux){
  Hx <- NULL
  for(j in 1:length(x)){
    Hx[j] <- integrate(ux, x[1], x[j], par = par)$value
  }
  return(Hx)
}

int_hazard2 <- function(x, hx){
  mult   = 10
  lhx    = log(hx)
  hx_num = spline(x, lhx, n = mult*length(x))
  x2     = hx_num$x 
  hxfun  = splinefun(x2, exp(hx_num$y))
  Hx <- NULL
  for(j in 1:length(x2)){
    Hx[j] <- integrate(hxfun, x2[1], x2[j])$value
  }
  Hx = Hx[seq(1, length(x2), by = mult)+9]
  return(Hx)
}

# Compute distribution functions
# ux = function(x, par){ (1/par[1]) * exp( (x-par[2])/par[1] ) }
# hx <- ux(x, par)
# Hx <- int_hazard(x, par, ux)
# Hx <- int_hazard2(x, hx)

# ---- LAWS ---------------------------------------

#' Gompertz mortality law
#' @keywords internal
#' 
gompertz0 <- function(x, par = NULL){
  # default parameters
  if (is.null(par)) { par[1] = 0.0002; par[2] = 0.13 }
  names(par) <- letters[1:length(par)]
  # Compute distribution functions
  hx  <- with(as.list(par), a*exp(b*x) )
  Hx  <- with(as.list(par), a/b * (exp(b*x) - 1) )
  Sx  <- exp(-Hx)
  return(as.list(environment()))
}


#' Gompertz mortality law - informative parametrization
#' @keywords internal
#' 
gompertz <- function(x, par = NULL){
  # default parameters
  if (is.null(par)) { par[1] = 7.692308; par[2] = 49.82286 }
  names(par) <- c('sigma', 'm')
  # Compute distribution functions
  hx <- with(as.list(par), (1/sigma) * exp((x-m)/sigma) )
  Hx <- with(as.list(par), exp(-m/sigma) * (exp(x/sigma) - 1) )
  Sx <- exp(-Hx)
  return(as.list(environment()))
}

#' Inverse-Gompertz mortality law - informative parametrization
#' m - is a measure of location because it is the mode of the density, m > 0
#' sigma - represents the dispersion of the density about the mode, sigma > 0
#' @keywords internal
#' 
invgompertz <- function(x, par = NULL){
  # default parameters
  if (is.null(par)) { par[1] = 7.692308; par[2] = 49.82286 }
  names(par) <- c('sigma', 'm')
  # Compute distribution functions
  hx = with(as.list(par),  (1- exp(-(x-m)/sigma)) / (exp(-(x-m)/sigma) - 1) )
  Sx = with(as.list(par),  (1 - exp(-exp(-(x-m)/sigma))) / (1 - exp(-exp(m/sigma))) )
  Hx = -log(Sx)
  return(as.list(environment()))
}

#' Makeham mortality law
#' @keywords internal
#' 
makeham0 <- function(x, par = NULL){
  # default parameters
  if (is.null(par)) { par = c(.0002, .13, .001)}
  names(par) <- letters[1:length(par)]
  # Compute distribution functions
  hx <- with(as.list(par), a*exp(b*x) + c )
  Hx <- with(as.list(par), a/b * (exp(b*x) - 1) + x*c )
  Sx <- exp(-Hx)
  return(as.list(environment()))
}

#' Makeham mortality law - informative parametrization
#' @keywords internal
#' 
makeham <- function(x, par = NULL){
  # default parameters
  if (is.null(par)) { par[1] = 7.692308; par[2] = 49.82286; par[3] = 0.001 }
  names(par) <- c('sigma', 'm', 'c')
  # Compute hazard
  hx <- with(as.list(par), (1/sigma) * exp((x-m)/sigma) + c )
  Hx <- with(as.list(par), exp(-m/sigma) * (exp(x/sigma) - 1) + x*c )
  Sx <- exp(-Hx)
  return(as.list(environment()))
}

#' Weibull mortality law
#' Note that if sigma > m, then the mode of the density is 0 and hx is a 
#' non-increasing function of x, while if sigma < m, then the mode is 
#' greater than 0 and hx is an increasing function.
#' m > 0 is a measure of location
#' sigma > 0 is measure of dispersion
#' @keywords internal
#' 
weibull <- function(x, par = NULL){
  # default parameters
  if (is.null(par)) { par = c(2, 1) }
  names(par) <- c('sigma', 'm')
  # Compute hazard
  hx <- with(as.list(par), 1/sigma * (x/m)^(m/sigma - 1) )
  Hx <- with(as.list(par), (x/m)^(m/sigma) )
  Sx <- exp(-Hx)
  return(as.list(environment()))
}

#' Inverse-Weibull mortality law
#' The Inverse-Weibull proves useful for modeling the childood and teenage years, 
#' because the logarithm of h(x) is a very concave function.
#' m > 0 is a measure of location
#' sigma > 0 is measure of dispersion
#' @keywords internal
#' 
invweibull <- function(x, par = NULL){
  # default parameters
  if (is.null(par)) { par[1] = 10; par[2] = 25 }
  names(par) <- c('sigma', 'm')
  # Compute hazard
  hx = with(as.list(par), 1/sigma * (x/m)^(-m/sigma - 1) / 
                          (exp((x/m)^(-m/sigma)) - 1) )
  Hx <- with(as.list(par), -log(1 - exp(-(x/m)^(-m/sigma))) )
  Sx <- exp(-Hx)
  return(as.list(environment()))
}


#' Kannisto mortality law
#' @keywords internal
#' 
kannisto <- function(x, par = NULL){
  # default parameters
  if (is.null(par)) { par[1] = 0.5; par[2] = 0.13}
  names(par) <- letters[1:length(par)]
  # Compute distribution functions
  hx <- with(as.list(par), a*exp(b*x) / (1 + a*exp(b*x)) )
  Hx <- with(as.list(par), 1/a * log( (1 + a*exp(b*x)) / (1 + a) ) )
  return(as.list(environment()))
}


#' DeMoivre mortality law
#' @keywords internal
#' 
demoivre <- function(x, par = NULL){
  # default parameters
  if (is.null(par)) { par = 100 }
  names(par) = 'a'
  # Compute hazard
  vsmall = 1e-10 # very small number
  hx <- 1/(par-x) + vsmall
  Hx <- cumsum(hx)
  return(as.list(environment()))
}


#' Opperman mortality law
#' @keywords internal
#' 
opperman <- function(x, par = NULL){
  # default parameters
  if (is.null(par)) { par[1] = 0.004; par[2] = -0.0004; par[3] = 0.001 }
  names(par) <- letters[1:length(par)]
  # Compute distribution functions
  hx = with(as.list(par), a/sqrt(x) + b + c*(x^(1/3)) )
  Hx = cumsum(hx)
  return(as.list(environment()))
}

#' Heligman-Pollard mortality law
#' @keywords internal
#' 
heligman_pollard <- function(x, par = NULL){
  # default parameters
  if (is.null(par)) {par = c(.0005, .004, .08, .001, 10, 17, .00005, 1.1)}
  names(par) <- letters[1:length(par)]
  # Compute distribution functions
  mu1 = with(as.list(par), a^((x+b)^c) + g*h^x)
  mu2 = with(as.list(par), d*exp(-e*(log(x/f))^2) )
  hx = ifelse(x == 0, mu1, mu1 + mu2)
  Hx = cumsum(hx)
  return(as.list(environment()))
}


#' Thiele mortality law
#' @keywords internal
#' 
thiele <- function(x, par = NULL){
  # default parameters
  if (is.null(par)) {par = c(.02474, .3, .004, .5, 25, .0001, .13)}
  names(par) <- letters[1:length(par)]
  # Compute distribution functions
  mu1 = with(as.list(par), a*exp(-b*x) )
  mu2 = with(as.list(par), c*exp(-.5*d*(x-e)^2) )
  mu3 = with(as.list(par), f*exp(g*x) )
  hx = ifelse(x == 0, mu1 + mu3, mu1 + mu2 + mu3)
  Hx = cumsum(hx)
  return(as.list(environment()))
}


#' Wittstein mortality law
#' @keywords internal
#' 
wittstein <- function(x, par = NULL){
  # default parameters
  if (is.null(par)) {par = c(1.5, 1, .5, 100)}
  names(par) <- c('a', 'm', 'n', 'M')
  # Compute distribution functions
  hx = with(as.list(par), (1/m)*a^-((m*x)^n) + a^-((M-x)^n) )
  Hx = cumsum(hx)
  return(as.list(environment()))
}

#' Carriere mortality law
#' Carriere1 = weibull + invweibull + gompertz
#' @keywords internal
#' 
carriere1 <- function(x, par = NULL){
  if (is.null(par)) {par = c(0.01, 2, 1, 0.01, 10, 25, 7.692308, 49.82286)}
  names(par) <- c('p1', 'sigma1', 'm1',
                  'p2', 'sigma2', 'm2', 
                        'sigma3', 'm3')
  # Compute distribution functions
  S_wei  = weibull(x, par[c('sigma1', 'm1')])$Sx
  S_iwei = invweibull(x, par[c('sigma1', 'm1')])$Sx
  S_gom  = gompertz(x, par[c('sigma3', 'm3')])$Sx
  
  f1 <- max(0, min(par['p1'], 1))
  f2 <- max(0, min(par['p2'], 1))
  f3 <- 1 - f1 - f2
  
  Sx = f1*S_wei + f2*S_iwei + f3*S_gom
  Hx = -log(Sx)
  hx = c(Hx[1], diff(Hx)) # here we will need a numerical solution! 
  #This one is not quite correct.
  return(as.list(environment()))
}

#' Carriere mortality law
#' Carriere2 = weibull + invgompertz + gompertz
#' @keywords internal
#' 
carriere2 <- function(x, par = NULL){
  if (is.null(par)) {par = c(0.01, 2, 1, 0.01, 7.69, 49.82, 7.69, 49.82)}
  names(par) <- c('p1', 'sigma1', 'm1',
                  'p2', 'sigma2', 'm2', 
                        'sigma3', 'm3')
  # Compute distribution functions
  S_wei  = weibull(x, par[c('sigma1', 'm1')])$Sx
  S_igom = invgompertz(x, par[c('sigma2', 'm2')])$Sx
  S_gom  = gompertz(x, par[c('sigma3', 'm3')])$Sx
  
  f1 <- max(0, min(par['p1'], 1))
  f2 <- max(0, min(par['p2'], 1))
  f3 <- 1 - f1 - f2
  
  Sx = f1*S_wei + f2*S_igom + f3*S_gom
  Hx = -log(Sx)
  hx = c(Hx[1], diff(Hx)) # here we will need a numerical solution! 
  #This one is not quite correct.
  return(as.list(environment()))
}


#' Siler mortality law
#' @keywords internal
#' 
siler <- function(x, par = NULL){
  # default parameters
  if (is.null(par)) { par = c(.0002, .13, .001, .001, .013) }
  names(par) <- letters[1:length(par)]
  # compute hazard
  hx = with(as.list(par), a*exp(-b*x) + c + d*exp(e*x))
  Hx = cumsum(hx)
  return(as.list(environment()))
}









