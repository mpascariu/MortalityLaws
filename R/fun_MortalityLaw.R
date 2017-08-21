# ---- LAWS ---------------------------------------

#' Gompertz mortality law
#' @keywords internal
#' 
gompertz0 <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('gompertz0', par)
  hx  <- with(as.list(par), a*exp(b*x) )
  Hx  <- with(as.list(par), a/b * (exp(b*x) - 1) )
  Sx  <- exp(-Hx)
  return(as.list(environment()))
}


#' Gompertz mortality law - informative parametrization
#' @keywords internal
#' 
gompertz <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('gompertz', par)
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
  if (is.null(par)) par <- bring_parameters('invgompertz', par)
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
  if (is.null(par)) par <- bring_parameters('makeham0', par)
  hx <- with(as.list(par), a*exp(b*x) + c )
  Hx <- with(as.list(par), a/b * (exp(b*x) - 1) + x*c )
  Sx <- exp(-Hx)
  return(as.list(environment()))
}

#' Makeham mortality law - informative parametrization
#' @keywords internal
#' 
makeham <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('makeham', par)
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
  if (is.null(par)) par <- bring_parameters('weibull', par)
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
  if (is.null(par)) par <- bring_parameters('invweibull', par)
  hx <- with(as.list(par), 
            (1/sigma) * (x/m)^(-m/sigma - 1) / (exp((x/m)^(-m/sigma)) - 1) )
  Hx <- with(as.list(par), 
             -log(1 - exp(-(x/m)^(-m/sigma))) )
  Sx <- exp(-Hx)
  return(as.list(environment()))
}

#' Kannisto mortality law
#' @keywords internal
#' 
kannisto <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('kannisto', par)
  hx <- with(as.list(par), a*exp(b*x) / (1 + a*exp(b*x)) )
  Hx <- with(as.list(par), 1/a * log( (1 + a*exp(b*x)) / (1 + a) ) )
  Sx <- exp(-Hx)
  return(as.list(environment()))
}

#' DeMoivre mortality law
#' @keywords internal
#' 
demoivre <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('demoivre', par)
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
  if (is.null(par)) par <- bring_parameters('opperman', par)
  x = x + 1e-10
  hx = with(as.list(par), a/sqrt(x) - b + c*(x^(1/3)) )
  hx = pmax(0, hx)
  Hx = cumsum(hx)
  Sx = exp(-Hx)
  return(as.list(environment()))
}

#' Thiele mortality law
#' @keywords internal
#' 
thiele <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('thiele', par)
  mu1 = with(as.list(par), a*exp(-b*x) )
  mu2 = with(as.list(par), c*exp(-.5*d*(x - e)^2) )
  mu3 = with(as.list(par), f*exp(g*x) )
  hx = ifelse(x == 0, mu1 + mu3, mu1 + mu2 + mu3)
  Hx = cumsum(hx)
  Sx = exp(-Hx)
  return(as.list(environment()))
}

#' Wittstein mortality law
#' @keywords internal
#' 
wittstein <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('wittstein', par)
  hx = with(as.list(par), (1/m)*a^-((m*x)^n) + a^-((M - x)^n) )
  Hx = cumsum(hx)
  Sx = exp(-Hx)
  return(as.list(environment()))
}

#' Carriere mortality law
#' Carriere1 = weibull + invweibull + gompertz
#' @keywords internal
#' 
carriere1 <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('carriere1', par)
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
  if (is.null(par)) par <- bring_parameters('carriere2', par)
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
  if (is.null(par)) par <- bring_parameters('siler', par)
  hx = with(as.list(par), a*exp(-b*x) + c + d*exp(e*x))
  Hx = cumsum(hx)
  Sx = exp(-Hx)
  return(as.list(environment()))
}


#' Heligman-Pollard mortality law - 8 parameters
#' @keywords internal
#' 
HP <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('HP', par)
  mu1 = with(as.list(par), a^((x + b)^c) + g*h^x )
  mu2 = with(as.list(par), d*exp(-e*(log(x/f))^2) )
  eta = ifelse(x == 0, mu1, mu1 + mu2)
  hx = eta/(1 + eta) # here hx doese not represent the hazard but probabilities (qx's)
  return(as.list(environment()))
}

#' Heligman-Pollard 2 mortality law - 8 parameters
#' @keywords internal
#' 
HP2 <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('HP2', par)
  mu1 = with(as.list(par), a^((x + b)^c) + (g*h^x)/(1 + g*h^x) )
  mu2 = with(as.list(par), d*exp(-e*(log(x/f))^2) )
  eta = ifelse(x == 0, mu1, mu1 + mu2)
  hx = eta # here hx doese not represent the hazard but probabilities (qx's)
  return(as.list(environment()))
}

#' Heligman-Pollard 3 mortality law - 9 parameters
#' @keywords internal
#' 
HP3 <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('HP3', par)
  mu1 = with(as.list(par), a^((x + b)^c) + (g*h^x)/(1 + k*g*h^x) )
  mu2 = with(as.list(par), d*exp(-e*(log(x/f))^2) )
  eta = ifelse(x == 0, mu1, mu1 + mu2)
  hx = eta # here hx doese not represent the hazard but probabilities (qx's)
  return(as.list(environment()))
}

#' Heligman-Pollard 4 mortality law - 9 parameters
#' @keywords internal
#' 
HP4 <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('HP4', par)
  mu1 = with(as.list(par), a^((x + b)^c) + (g*h^(x^k)) / (1 + g*h^(x^k)) )
  mu2 = with(as.list(par), d*exp(-e*(log(x/f))^2) )
  eta = ifelse(x == 0, mu1, mu1 + mu2)
  hx = eta # here hx doese not represent the hazard but probabilities (qx's)
  return(as.list(environment()))
}

# Thu Mar  9 14:53:14 2017 ------------------------------


#' Select start parameters
#' @keywords internal
#' 
choose_Spar <- function(law){
  switch(law,
         demoivre    = c(a = 105),
         gompertz0   = c(a = 0.0002, b = 0.13),
         gompertz    = c(sigma = 7.692308, m = 49.82286),
         invgompertz = c(sigma = 7.692308, m = 49.82286),
         makeham0    = c(a = .0002, b = .13, c = .001),
         makeham     = c(sigma = 7.692308, m = 49.82286, c = 0.001),
         opperman    = c(a = 0.04, b = 0.0004, c = 0.001),
         thiele      = c(a = .02474, b = .3, c = .004, d = .5, 
                         e = 25, f = .0001, g = .13),
         wittstein   = c(a = 1.5, m = 1, n = .5, M = 100),
         weibull     = c(sigma = 2, m = 1),
         invweibull  = c(sigma = 10, m = 5),
         HP          = c(a = .0005, b = .004, c = .08, d = .001, 
                         e = 10, f = 17, g = .00005, h = 1.1),
         HP2         = c(a = .0005, b = .004, c = .08, d = .001, 
                         e = 10, f = 17, g = .00005, h = 1.1),
         HP3         = c(a = .0005, b = .004, c = .08, d = .001, 
                         e = 10, f = 17, g = .00005, h = 1.1, k = 1),
         HP4         = c(a = .0005, b = .004, c = .08, d = .001, 
                         e = 10, f = 17, g = .00005, h = 1.1, k = 1),
         siler       = c(a = .0002, b = .13, c = .001, 
                         d = .001, e = .013),
         kannisto    = c(a = 0.5, b = 0.13),
         carriere1   = c(p1 = 0.003, sigma1 = 15, m1 = 2.7, 
                         p2 = 0.007, sigma2 = 6, m2 = 3, 
                         sigma3 = 9.5, m3 = 88),
         carriere2   = c(p1 = 0.01, sigma1 = 2, m1 = 1, 
                         p2 = 0.01, sigma2 = 7.69, m2 = 49.82, 
                         sigma3 = 7.69, m3 = 49.82)
  )
}

#' Bring or rename start parameters in the law functions
#' @keywords internal
#' 
bring_parameters <- function(law, par = NULL) {
  Spar = choose_Spar(law)
  if (is.null(par)) { par = Spar}
  names(par) = names(Spar) # check par names
  return(par)
}




#' Check available mortality laws
#' 
#' The function returns information about the implemented parametric functions 
#' in \code{\link{MortalityLaw}} function.
#' @param law Default: \code{NULL}. One can substract details about a certain model
#' by specifing its code.
#' @return An \code{availableLaws} object.
#' @examples 
#' 
#' availableLaws()
#' 
#' @export
availableLaws <- function(law = NULL){
  
  if (is.null(law)) {
    table <- as.data.frame(matrix(ncol = 6, byrow = T,
              c(1825, 'Gompertz', 'mu[x] = a*exp(b*x)', 3, 'gompertz0', 'mu[x]',
                NA, 'Gompertz', 'mu[x] = 1/sigma * exp[(x-m)/sigma)]', 3, 'gompertz', 'mu[x]',
                NA, 'Inverse-Gompertz', 'mu[x] = [1- exp(-(x-m)/sigma)] / [exp(-(x-m)/sigma) - 1]', 2, 'invgompertz', 'mu[x]',
                1860, 'Makeham', 'mu[x] = a*exp(b*x) + c', 3, 'makeham0', 'mu[x]',
                NA, 'Makeham', 'mu[x] = 1/sigma * exp[(x-m)/sigma)] + c', 3, 'makeham', 'mu[x]',
                1870, 'Opperman', 'mu[x] = a*x^(-1/2) + b + c*x^(1/3)', 1, 'opperman', 'mu[x]',
                1871, 'Thiele', 'mu[x] = a*exp(-b*x) + c*exp[-.5d*(x-e)^2] + f*exp(g*x)', 5, 'thiele', 'mu[x]',
                1883, 'Wittstein', 'mu[x] = (1/m)*a^-[(m*x)^n] + a^-[(M-x)^n]', 5, 'wittstein', 'mu[x]',
                1939, 'Weibull', 'mu[x] = 1/sigma * (x/m)^(m/sigma - 1)', 1, 'weibull', 'mu[x]',
                NA, 'Inverse-Weibull', 'mu[x] = 1/sigma * (x/m)^[-m/sigma - 1] / [exp((x/m)^(-m/sigma)) - 1]', 2, 'invweibull', 'mu[x]', 
                1979, 'Siler', 'mu[x] = a*exp(-b*x) + c + d*exp(e*x)', 5, 'siler', 'mu[x]',
                1980, 'Heligman-Pollard', 'q[x]/p[x] = A^[(x+B)^C] + D*exp[-E*log(x/F)^2] + G*H^x', 5, 'HP', 'q[x]',
                1980, 'Heligman-Pollard', 'q[x] = A^[(x+B)^C] + D*exp[-E*log(x/F)^2] + G*H^x / [1+G*H^x]', 5, 'HP2', 'q[x]',
                1980, 'Heligman-Pollard', 'q[x] = A^[(x+B)^C] + D*exp[-E*log(x/F)^2] + G*H^x / [1+K*G*H^x]', 5, 'HP3', 'q[x]',
                1980, 'Heligman-Pollard', 'q[x] = A^[(x+B)^C] + D*exp[-E*log(x/F)^2] + G*H^(x^K) / [1+G*H^(x^K)]', 5, 'HP4', 'q[x]',
                1992, 'Kannisto', 'mu(x) = a*exp(b*x) / [1 + a*exp(b*x)]', 4, 'Kannisto', 'mu[x]',
                1992, 'Carriere', 'Weibull + Inverse-Weibull + Gompertz', 5, 'carriere1', '?',
                1992, 'Carriere', 'Weibull + Inverse-Gompertz + Gompertz', 5, 'carriere2', '?')))
    colnames(table) <- c('YEAR', 'NAME', 'MODEL', 'TYPE', 'CODE', 'FIT')
    
    legend <- as.data.frame(matrix(ncol = 2, byrow = T, 
                                   c(1, "Infant mortality",
                                     2, "Accidental hump",
                                     3, "Adult mortality",
                                     4, "Old-age mortality",
                                     5, "Full age range")))
    colnames(legend) <- c("TYPE", "Coverage" )
  }
  
  if (!is.null(law)) {
    A <- availableLaws() 
    table <- A$table[A$table$CODE %in% law, ]
    legend <- A$legend[A$legend$TYPE %in% unique(table$TYPE), ]
  }
  
  out <- structure(class = "availableLaws", 
                   list(table = table, legend = legend))
  return(out)
}





