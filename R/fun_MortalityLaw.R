# ---- LAWS ---------------------------------------

#' Gompertz mortality law
#' @keywords internal
#' 
gompertz0 <- function(x, par = NULL){
  if (is.null(par)) { par = choose_Spar('gompertz0') }
  hx  <- with(as.list(par), a*exp(b*x) )
  Hx  <- with(as.list(par), a/b * (exp(b*x) - 1) )
  Sx  <- exp(-Hx)
  return(as.list(environment()))
}


#' Gompertz mortality law - informative parametrization
#' @keywords internal
#' 
gompertz <- function(x, par = NULL){
  if (is.null(par)) { par = choose_Spar('gompertz') }
  hx <- with(as.list(par), (1/sigma) * exp((x - m)/sigma) )
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
  if (is.null(par)) { par = choose_Spar('invgompertz') }
  hx = with(as.list(par), 
            1/sigma * exp(-(x - m)/sigma) / (exp(exp(-(x - m)/sigma)) - 1)  )
  Sx = with(as.list(par),  
            (1 - exp(-exp(-(x - m)/sigma))) / (1 - exp(-exp(m/sigma))) )
  Hx = -log(Sx)
  return(as.list(environment()))
}

#' Makeham mortality law
#' @keywords internal
#' 
makeham0 <- function(x, par = NULL){
  if (is.null(par)) { par = choose_Spar('makeham0') }
  hx <- with(as.list(par), a*exp(b*x) + c )
  Hx <- with(as.list(par), a/b * (exp(b*x) - 1) + x*c )
  Sx <- exp(-Hx)
  return(as.list(environment()))
}

#' Makeham mortality law - informative parametrization
#' @keywords internal
#' 
makeham <- function(x, par = NULL){
  if (is.null(par)) { par = choose_Spar('makeham') }
  hx <- with(as.list(par), (1/sigma) * exp((x - m)/sigma) + c )
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
  if (is.null(par)) { par = choose_Spar('weibull') }
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
  if (is.null(par)) { par = choose_Spar('invweibull') }
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
  if (is.null(par)) { par = choose_Spar('kannisto') }
  hx <- with(as.list(par), a*exp(b*x) / (1 + a*exp(b*x)) )
  Hx <- with(as.list(par), 1/a * log( (1 + a*exp(b*x)) / (1 + a) ) )
  Sx <- exp(-Hx)
  return(as.list(environment()))
}

#' DeMoivre mortality law
#' @keywords internal
#' 
demoivre <- function(x, par = NULL){
  if (is.null(par)) { par = choose_Spar('demoivre') }
  vsmall = 1e-10 # very small number
  hx <- pmax(1/(par - x) + vsmall, 0)
  Hx <- cumsum(hx)
  Sx <- pmax(1 - x/par, 0)
  return(as.list(environment()))
}


#' Opperman mortality law
#' @keywords internal
#' 
opperman <- function(x, par = NULL){
  if (is.null(par)) { par = choose_Spar('oppperman') }
  hx = with(as.list(par), a/sqrt(x) + b + c*(x^(1/3)) )
  Hx = cumsum(hx)
  Sx <- exp(-Hx)
  return(as.list(environment()))
}

#' Heligman-Pollard mortality law
#' @keywords internal
#' 
HP <- function(x, par = NULL){
  if (is.null(par)) { par = choose_Spar('HP') }
  mu1 = with(as.list(par), a^((x + b)^c) + g*h^x)
  mu2 = with(as.list(par), d*exp(-e*(log(x/f))^2) )
  hx = ifelse(x == 0, mu1, mu1 + mu2)
  Hx = cumsum(hx)
  Sx <- exp(-Hx)
  return(as.list(environment()))
}

#' Thiele mortality law
#' @keywords internal
#' 
thiele <- function(x, par = NULL){
  if (is.null(par)) { par = choose_Spar('thiele') }
  mu1 = with(as.list(par), a*exp(-b*x) )
  mu2 = with(as.list(par), c*exp(-.5*d*(x - e)^2) )
  mu3 = with(as.list(par), f*exp(g*x) )
  hx = ifelse(x == 0, mu1 + mu3, mu1 + mu2 + mu3)
  Hx = cumsum(hx)
  Sx <- exp(-Hx)
  return(as.list(environment()))
}

#' Wittstein mortality law
#' @keywords internal
#' 
wittstein <- function(x, par = NULL){
  if (is.null(par)) { par = choose_Spar('wittstein') }
  hx = with(as.list(par), (1/m)*a^-((m*x)^n) + a^-((M - x)^n) )
  Hx = cumsum(hx)
  Sx <- exp(-Hx)
  return(as.list(environment()))
}

#' Carriere mortality law
#' Carriere1 = weibull + invweibull + gompertz
#' @keywords internal
#' 
carriere1 <- function(x, par = NULL){
  if (is.null(par)) { par = choose_Spar('carriere1') }
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
  return(as.list(environment()))
}

#' Carriere mortality law
#' Carriere2 = weibull + invgompertz + gompertz
#' @keywords internal
#' 
carriere2 <- function(x, par = NULL){
  if (is.null(par)) { par = choose_Spar('carriere2') }
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
  return(as.list(environment()))
}

#' Siler mortality law
#' @keywords internal
#' 
siler <- function(x, par = NULL){
  if (is.null(par)) { par = choose_Spar('siler') }
  hx = with(as.list(par), a*exp(-b*x) + c + d*exp(e*x))
  Hx = cumsum(hx)
  return(as.list(environment()))
}

# Thu Mar  9 14:53:14 2017 ------------------------------

#' Integarte hazard function
#' @keywords internal
#' 
iHazard <- function(x, law, par, fun){
  x = x + 1e-15
  Hx <- NULL
  for (j in 1:length(x)) {
    Hx[j] <- integrate(f = fun, par = par, 
                       subdivisions = 2*length(x), law = law,
                       lower = x[1], upper = x[j])$value
  }
  return(Hx)
}


#' Select start parameters
#' @keywords internal
#' 
choose_Spar <-  function(law){
  switch(law,
         demoivre    = c(a = 105),
         gompertz0   = c(a = 0.0002, b = 0.13),
         gompertz    = c(sigma = 7.692308, m = 49.82286),
         invgompertz = c(sigma = 7.692308, m = 49.82286),
         makeham0    = c(a = .0002, b = .13, c = .001),
         makeham     = c(sigma = 7.692308, m = 49.82286, c = 0.001),
         opperman    = c(a = 0.004, b = -0.0004, c = 0.001),
         thiele      = c(a = .02474, b = .3, c = .004, d = .5, 
                         e = 25, f = .0001, g = .13),
         wittstein   = c(a = 1.5, m = 1, n = .5, M = 100),
         weibull     = c(sigma = 2, m = 1),
         invweibull  = c(sigma = 10, m = 25),
         HP          = c(a = .0005, b = .004, c = .08, d = .001, 
                         e = 10, f = 17, g = .00005, h = 1.1),
         siler       = c(a = .0002, b = .13, c = .001, 
                         d = .001, e = .013),
         kannisto    = c(a = 0.5, b = 0.13),
         carriere1   = c(p1 = 0.01, sigma1 = 2, m1 = 1, 
                         p2 = 0.01, sigma2 = 10, m2 = 25, 
                         sigma3 = 7.69, m3 = 49.82),
         carriere2   = c(p1 = 0.01, sigma1 = 2, m1 = 1, 
                         p2 = 0.01, sigma2 = 7.69, m2 = 49.82, 
                         sigma3 = 7.69, m3 = 49.82)
  )
}

#' Select info
#' @keywords internal
#' 
choose_law_info <-  function(law){
  info = switch(law,
                demoivre    = 'DeMoivre (1725): h(x) = 1/(a-x)',
                gompertz0   = 'Gompertz (1825): h(x) = a*exp(b*x)',
                gompertz    = 'Gompertz (1825): h(x) = 1/sigma * exp[(x-m)/sigma)]',
                invgompertz = 'Inverse-Gompertz: h(x) = [1- exp(-(x-m)/sigma)] / [exp(-(x-m)/sigma) - 1]',
                makeham0    = 'Makeham (1860): h(x) = a*exp(b*x) + c',
                makeham     = 'Makeham (1860): h(x) = 1/sigma * exp[(x-m)/sigma)] + c',
                opperman    = 'Opperman (1870): h(x) = a*x^(-1/2) + b + c*x^(1/3)',
                thiele      = 'Thiele (1871): h(x) = a*exp(-b*x) + c*exp[-.5d*(x-e)^2] + f*exp(g*x)',
                wittstein   = 'Wittstein (1883): h(x) = (1/m)*a^-[(m*x)^n] + a^-[(M-x)^n]',
                weibull     = 'Weibull (1939): h(x) = 1/sigma * (x/m)^(m/sigma - 1)',
                invweibull  = 'Inverse-Weibull: h(x) = 1/sigma * (x/m)^(-m/sigma - 1) / (exp((x/m)^(-m/sigma)) - 1)',
                HP          = 'Heligman-Pollard (1980): q(x)/p(x) = a^((x+b)^c) + d*exp(-e*(log(x/f))^2) + g*h^x)',
                siler       = 'Siler (1979): h(x) = a*exp(-b*x) + c + d*exp(e*x)',
                kannisto    = 'Kannisto (1992): h(x) = a*exp(b*x) / [1 + a*exp(b*x)]',
                carriere1   = 'Carriere1 (1992): Weibull + Inverse-Weibull + Gompertz',
                carriere2   = 'Carriere2 (1992): Weibull + Inverse-Gompertz + Gompertz'
  )
  return(info)
}

