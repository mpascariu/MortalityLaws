
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
gompertz <- function(x, par = NULL){
  model_info <- 'Gompertz (1825): h(x) = a*exp(b*x)'
  # default parameters
  if (is.null(par)) { par[1] = 0.0002; par[2] = 0.13 }
  names(par) <- letters[1:length(par)]
  # Compute distribution functions
  hx  <- par['a']*exp(par['b']*x)
  Hx  <- par['a']/par['b'] * (exp(par['b']*x) - 1)
  return(as.list(environment()))
}


#' Gompertz mortality law - informative parametrization
#' @keywords internal
#' 
gompertz0 <- function(x, par = NULL){
  model_info <- 'Gompertz (1825): h(x) = 1/sigma * exp[(x-m)/sigma)]'
  # default parameters
  if (is.null(par)) { par[1] = 7.692308; par[2] = 49.82286 }
  names(par) <- c('sigma', 'm')
  # Compute distribution functions
  hx <- (1/par['sigma']) * exp( (x-par['m'])/par['sigma'] )
  Hx <- exp(-par['m']/par['sigma']) * (exp(x/par['sigma']) - 1)
  return(as.list(environment()))
}

#' Inverse-Gompertz mortality law - informative parametrization
#' m - is a measure of location because it is the mode of the density, m > 0
#' sigma - represents the dispersion of the density about the mode, sigma > 0
#' @keywords internal
#' 
invgompertz <- function(x, par = NULL){
  model_info <- 'Inverse-Gompertz:
                h(x) = [1- exp(-(x-m)/sigma)] / [exp(-(x-m)/sigma) - 1]'
  # default parameters
  if (is.null(par)) { par[1] = 7.692308; par[2] = 49.82286 }
  names(par) <- c('sigma', 'm')
  # Compute distribution functions
  mu1 <- 1/par['sigma'] * exp(-(x-par['m'])/par['sigma'])
  mu2 <- exp(exp(-(x-par['m'])/par['sigma'])) - 1
  hx  <- mu1 / mu2
  
  mu3 <- 1 - exp(-exp(-(x-par['m'])/par['sigma']))
  mu4 <- 1 - exp(-exp(par['m']/par['sigma']))
  Hx  <- -log(mu3/mu4)
  return(as.list(environment()))
}

#' Makeham mortality law
#' @keywords internal
#' 
makeham <- function(x, par = NULL){
  model_info <- 'Makeham (1860):  h(x) = a*exp(b*x) + c'
  # default parameters
  if (is.null(par)) { par[1] = 0.0002; par[2] = 0.13; par[3] = 0.001 }
  names(par) <- letters[1:length(par)]
  # Compute distribution functions
  hx  <- par['a']*exp(par['b']*x) + par['c']
  Hx  <- par['a']/par['b'] * (exp(par['b']*x) - 1) + x*par['c']
  return(as.list(environment()))
}

#' Makeham mortality law - informative parametrization
#' @keywords internal
#' 
makeham0 <- function(x, par = NULL){
  model_info <- 'Makeham (1860):  h(x) = 1/sigma * exp[(x-m)/sigma)] + c'
  # default parameters
  if (is.null(par)) { par[1] = 7.692308; par[2] = 49.82286; par[3] = 0.001 }
  names(par) <- c('sigma', 'm', 'c')
  # Compute hazard
  hx <- (1/par['sigma']) * exp( (x-par['m'])/par['sigma'] ) + par['c']
  Hx <- exp(-par['m']/par['sigma']) * (exp(x/par['sigma']) - 1) + x*par['c']
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
  model_info <- 'Weibull (1939): h(x) = 1/sigma * (x/m)^(m/sigma - 1)'
  # default parameters
  if (is.null(par)) { par[1] = 2; par[2] = 1 }
  names(par) <- c('sigma', 'm')
  # Compute hazard
  hx <- 1/par['sigma'] * (x/par['m'])^(par['m']/par['sigma'] - 1)
  Hx <- (x/par['m'])^(par['m']/par['sigma'])
  return(as.list(environment()))
}

#' Inverse-Weibull mortality law
#' The Inverse-Weibull proves useful for modeling the teenage years, 
#' because the logarithm of h(x) is a very concave function.
#' m > 0 is a measure of location
#' sigma > 0 is measure of dispersion
#' @keywords internal
#' 
invweibull <- function(x, par = NULL){
  model_info <- 'Inverse-Weibull: 
  h(x) = 1/sigma * (x/m)^(-m/sigma - 1) / (exp((x/m)^(-m/sigma)) - 1)'
  # default parameters
  if (is.null(par)) { par[1] = 2; par[2] = 1 }
  names(par) <- c('sigma', 'm')
  # Compute hazard
  mu1 <- 1/par['sigma'] * (x/par['m'])^(-par['m']/par['sigma'] - 1)
  mu2 <- exp((x/par['m'])^(-par['m']/par['sigma'])) - 1
  hx <- mu1 / mu2
  Hx <- -log(1 - exp(-(x/par['m'])^(-par['m']/par['sigma'])))
  return(as.list(environment()))
}


#' Kannisto mortality law
#' @keywords internal
#' 
kannisto <- function(x, par = NULL){
  model_info <- 'Kannisto (1992): h(x) = a*exp(b*x) / [1 + a*exp(b*x)]'
  # default parameters
  if (is.null(par)) { par[1] = 0.5; par[2] = 0.13}
  names(par) <- letters[1:length(par)]
  # Compute distribution functions
  hx <- par['a']*exp(par['b']*x) / (1 + par['a']*exp(par['b']*x))
  Hx <- 1/par['a'] * log( (1 + par['a']*exp(par['b']*x)) / (1 + par['a']) )
  return(as.list(environment()))
}


#' DeMoivre mortality law
#' @keywords internal
#' 
demoivre <- function(x, par = NULL){
  model_info <- 'DeMoivre (1725): h(x) = 1/(a-x)'
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
  model_info <- 'Opperman (1870): h(x) = a*x^(-1/2) + b + c*x^(1/3)'
  # default parameters
  if (is.null(par)) { par[1] = 0.004; par[2] = -0.0004; par[3] = 0.001 }
  names(par) <- letters[1:length(par)]
  # Compute distribution functions
  hx = par['a']/sqrt(x) + par['b'] + par['c']*(x^(1/3))
  Hx = cumsum(hx)
  return(as.list(environment()))
}

#' Heligman-Pollard mortality law
#' @keywords internal
#' 
heligman_pollard <- function(x, par = NULL){
  model_info <- 'Heligman-Pollard (1980): q(x)/p(x) = a^((x+b)^c) + d*exp(-e*(log(x/f))^2) + g*h^x)'
  # default parameters
  if (is.null(par)) {par = c(.0005, .004, .08, .001, 10, 17, .00005, 1.1)}
  names(par) <- letters[1:length(par)]
  # Compute distribution functions
  mu1 = par['a']^((x+par['b'])^par['c'])
  mu2 = par['d']*exp(-par['e']*(log(x/par['f']))^2)
  mu3 = par['g']*par['h']^x
  hx = ifelse(x == 0, mu1 + mu3, mu1 + mu2 + mu3)
  Hx = cumsum(hx)
  return(as.list(environment()))
}


#' Thiele mortality law
#' @keywords internal
#' 
thiele <- function(x, par = NULL){
  model_info <- 'Thiele (1871): h(x) = a*exp(-b*x) + c*exp[-.5d*(x-e)^2] + f*exp(g*x)'
  # default parameters
  if (is.null(par)) {par = c(.02474, .3, .004, .5, 25, .0001, .13)}
  names(par) <- letters[1:length(par)]
  # Compute distribution functions
  mu1 = par['a']*exp(-par['b']*x)
  mu2 = par['c']*exp(-.5*par['d']*(x-par['e'])^2)
  mu3 = par['f']*exp(par['g']*x)
  hx = ifelse(x == 0, mu1 + mu3, mu1 + mu2 + mu3)
  Hx = cumsum(hx)
  return(as.list(environment()))
}


#' Wittstein mortality law
#' @keywords internal
#' 
wittstein <- function(x, par = NULL){
  model_info <- 'Wittstein (1883): q(x) = (1/m)*a^-[(m*x)^n] + a^-[(M-x)^n]'
  # default parameters
  if (is.null(par)) {par = c(1.5, 1, .5, 100)}
  names(par) <- c('a', 'm', 'n', 'M')
  # Compute distribution functions
  mu1 = (1/par['m'])*par['a']^-((par['m']*x)^par['n'])
  mu2 = par['a']^-((par['M']-x)^par['n'])
  hx = mu1 + mu2
  Hx = cumsum(hx)
  return(as.list(environment()))
}













