# ---- LAWS ---------------------------------------

#' Gompertz mortality law
#' @keywords internal
#' 
gompertz <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('gompertz', par)
  hx  <- with(as.list(par), a*exp(b*x) )
  Hx  <- with(as.list(par), a/b * (exp(b*x) - 1) )
  Sx  <- exp(-Hx)
  return(as.list(environment()))
}


#' Gompertz mortality law - informative parametrization
#' @keywords internal
#' 
gompertz0 <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('gompertz0', par)
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
  hx = with(as.list(par), 1/sigma * exp(-(x - m)/sigma) / (exp(exp(-(x - m)/sigma)) - 1))
  Sx = with(as.list(par), (1 - exp(-exp(-(x - m)/sigma))) / (1 - exp(-exp(m/sigma))))
  Hx = -log(Sx)
  return(as.list(environment()))
}

#' Makeham mortality law
#' @keywords internal
#' 
makeham <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('makeham', par)
  hx <- with(as.list(par), a*exp(b*x) + c )
  Hx <- with(as.list(par), a/b * (exp(b*x) - 1) + x*c )
  Sx <- exp(-Hx)
  return(as.list(environment()))
}

#' Makeham mortality law - informative parametrization
#' @keywords internal
#' 
makeham0 <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('makeham0', par)
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
  Hx <- with(as.list(par), -log(1 - exp(-(x/m)^(-m/sigma))) )
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

#' Opperman mortality law
#' @keywords internal
#' 
opperman <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('opperman', par)
  x = x + 1
  hx = with(as.list(par), a/sqrt(x) - b + c*sqrt(x))
  hx = pmax(0, hx)
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
  return(as.list(environment()))
}

#' Wittstein mortality law
#' @keywords internal
#' 
wittstein <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('wittstein', par)
  hx = with(as.list(par), (1/m)*a^-((m*x)^n) + a^-((M - x)^n) )
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
  hx = with(as.list(par), A*exp(-B*x) + C + D*exp(E*x))
  return(as.list(environment()))
}


#' Heligman-Pollard mortality law - 8 parameters
#' @keywords internal
#' 
HP <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('HP', par)
  mu1 = with(as.list(par), A^((x + B)^C) + G*H^x )
  mu2 = with(as.list(par), D*exp(-E*(log(x/F_))^2) )
  eta = ifelse(x == 0, mu1, mu1 + mu2)
  hx = eta/(1 + eta)
  return(as.list(environment()))
}

#' Heligman-Pollard 2 mortality law - 8 parameters
#' @keywords internal
#' 
HP2 <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('HP2', par)
  mu1 = with(as.list(par), A^((x + B)^C) + (G*H^x)/(1 + G*H^x) )
  mu2 = with(as.list(par), D*exp(-E*(log(x/F_))^2) )
  eta = ifelse(x == 0, mu1, mu1 + mu2)
  hx = eta
  return(as.list(environment()))
}

#' Heligman-Pollard 3 mortality law - 9 parameters
#' @keywords internal
#' 
HP3 <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('HP3', par)
  mu1 = with(as.list(par), A^((x + B)^C) + (G*H^x)/(1 + K*G*H^x) )
  mu2 = with(as.list(par), D*exp(-E*(log(x/F_))^2) )
  eta = ifelse(x == 0, mu1, mu1 + mu2)
  hx = eta
  return(as.list(environment()))
}

#' Heligman-Pollard 4 mortality law - 9 parameters
#' @keywords internal
#' 
HP4 <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('HP4', par)
  mu1 = with(as.list(par), A^((x + B)^C) + (G*H^(x^K)) / (1 + G*H^(x^K)) )
  mu2 = with(as.list(par), D*exp(-E*(log(x/F_))^2) )
  eta = ifelse(x == 0, mu1, mu1 + mu2)
  hx = eta 
  return(as.list(environment()))
}

#' @keywords internal
#' 
perks <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('perks', par)
  hx  <- with(as.list(par), (A + B*C^x) / (B*(C^-x) + 1 + D*C^x) )
  return(as.list(environment()))
}


#' @keywords internal
#' 
beard <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('beard', par)
  hx  <- with(as.list(par), (A*exp(B*x)) / (1 + K*A*exp(B*x)) )
  return(as.list(environment()))
}

#' @keywords internal
#' 
makehambeard <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('makehambeard', par)
  hx  <- with(as.list(par), A*exp(B*x) / (1 + K*A*exp(B*x)) + C)
  return(as.list(environment()))
}

#' @keywords internal
#' 
vandermaen <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('vandermaen', par)
  hx  <- with(as.list(par), A + B*x + C*(x^2) + I/(N - x))
  return(as.list(environment()))
}

#' @keywords internal
#' 
vandermaen2 <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('vandermaen2', par)
  hx  <- with(as.list(par), A + B*x + I/(N - x) )
  return(as.list(environment()))
}

#' @keywords internal
#' 
quadratic <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('quadratic', par)
  hx  <- with(as.list(par), A + B*x + C*(x^2))
  return(as.list(environment()))
}

#' @keywords internal
#' 
martinelle <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('martinelle', par)
  hx <- with(as.list(par),  (A*exp(B*x) + C) / (1 + D*exp(B*x)) + K*exp(B*x) )
  return(as.list(environment()))
}

#' @keywords internal
#' 
rogersplanck <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('rogersplanck', par)
  hx <- with(as.list(par),   
             A0 + A1*exp(-a*x) + A2*exp(b*(x - u) - exp(-c*(x - u))) + A3*exp(d*x))
  return(as.list(environment()))
}

#' @keywords internal
#' 
kostaki <- function(x, par = NULL){
  if (is.null(par)) par <- bring_parameters('kostaki', par)
  pr = as.list(par)
  cond1 = with(pr, x <= F_)
  mu1 = with(pr, A^((x + B)^C) + G*H^x )
  mu2 = with(pr, D*exp(cond1*(-(E1*log(x/F_))^2 ) + (!cond1)*(-(E2*log(x/F_))^2 )))
  eta = ifelse(x == 0, mu1, mu1 + mu2)
  hx  = eta/(1 + eta)
  return(as.list(environment()))
}



# ------------------------------


#' Select start parameters
#' @keywords internal
#' 
choose_Spar <- function(law){
  switch(law,
         demoivre    = c(a = 105),
         gompertz    = c(a = 0.0002, b = 0.13),
         gompertz0   = c(sigma = 7.692308, m = 49.82286),
         invgompertz = c(sigma = 7.692308, m = 49.82286),
         makeham     = c(a = .0002, b = .13, c = .001),
         makeham0    = c(sigma = 7.692308, m = 49.82286, c = 0.001),
         opperman    = c(a = 0.04, b = 0.0004, c = 0.001),
         thiele      = c(a = .02474, b = .3, c = .004, d = .5, 
                         e = 25, f = .0001, g = .13),
         wittstein   = c(a = 1.5, m = 1, n = .5, M = 100),
         weibull     = c(sigma = 2, m = 1),
         invweibull  = c(sigma = 10, m = 5),
         HP          = c(A = .0005, B = .004, C = .08, D = .001, 
                         E = 10, F_ = 17, G = .00005, H = 1.1),
         HP2         = c(A = .0005, B = .004, C = .08, D = .001, 
                         E = 10, F_ = 17, G = .00005, H = 1.1),
         HP3         = c(A = .0005, B = .004, C = .08, D = .001, 
                         E = 10, F_ = 17, G = .00005, H = 1.1, K = 1),
         HP4         = c(A = .0005, B = .004, C = .08, D = .001, 
                         E = 10, F_ = 17, G = .00005, H = 1.1, K = 1),
         siler       = c(A = .0002, B = .13, C = .001, D = .001, E = .013),
         kannisto    = c(a = 0.5, b = 0.13),
         carriere1   = c(p1 = 0.003, sigma1 = 15, m1 = 2.7, 
                         p2 = 0.007, sigma2 = 6, m2 = 3, 
                         sigma3 = 9.5, m3 = 88),
         carriere2   = c(p1 = 0.01, sigma1 = 2, m1 = 1, 
                         p2 = 0.01, sigma2 = 7.69, m2 = 49.82, 
                         sigma3 = 7.69, m3 = 49.82),
         perks        = c(A = .002, B = .13, C = .01, D = .01),
         beard        = c(A = .002, B = .13, K = 1),
         makehambeard = c(A = .002, B = .13, C = .01, K = 1),
         vandermaen   = c(A = 0.01, B = 1, C = 0.01, I = 100, N = 200),
         vandermaen2  = c(A = 0.01, B = 1, I = 100, N = 200),
         quadratic    = c(A = 0.01, B = 1, C = 0.01),
         martinelle   = c(A = .001, B = 0.13, C = .001, D = 0.1, K = .001),
         rogersplanck = c(A0 = .0001, A1 = .02, A2 = .001, A3 = .0001, 
                          a = 2, b = .001, c = 100, d = .1, u = 0.33),
         kostaki      = c(A = .0005, B = .01, C = .10, D = .001, 
                          E1 = 3, E2 = 0.1, F_ = 25, G = .00005, H = 1.1)
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
              c(1825, 'Gompertz', 'mu[x] = a*exp(b*x)', 3, 'gompertz', 'mu[x]',
                NaN, 'Gompertz', 'mu[x] = 1/sigma * exp[(x-m)/sigma)]', 3, 'gompertz0', 'mu[x]',
                NaN, 'Inverse-Gompertz', 'mu[x] = [1- exp(-(x-m)/sigma)] / [exp(-(x-m)/sigma) - 1]', 2, 'invgompertz', 'mu[x]',
                1860, 'Makeham', 'mu[x] = a*exp(b*x) + c', 3, 'makeham', 'mu[x]',
                NaN, 'Makeham', 'mu[x] = 1/sigma * exp[(x-m)/sigma)] + c', 3, 'makeham0', 'mu[x]',
                1870, 'Opperman', 'mu[x] = a/sqrt(x) - b + c*sqrt(x)', 1, 'opperman', 'mu[x]',
                1871, 'Thiele', 'mu[x] = a*exp(-b*x) + c*exp[-.5d*(x-e)^2] + f*exp(g*x)', 6, 'thiele', 'mu[x]',
                1883, 'Wittstein', 'q[x] = (1/m)*a^-[(m*x)^n] + a^-[(M-x)^n]', 6, 'wittstein', 'q[x]',
                1932, 'Perks', 'mu[x] = [A + B*C^x] / [B*C^-x + 1 + D*C^x]', 3, 'perks', 'mu[x]',
                1939, 'Weibull', 'mu[x] = 1/sigma * (x/m)^(m/sigma - 1)', 1, 'weibull', 'mu[x]',
                NaN, 'Inverse-Weibull', 'mu[x] = 1/sigma * (x/m)^[-m/sigma - 1] / [exp((x/m)^(-m/sigma)) - 1]', 2, 'invweibull', 'mu[x]', 
                1943, 'Van der Maen', 'mu[x] = A + B*x + C*x^2 + I/[N - x]', 4, 'vandermaen', 'mu[x]',
                1943, 'Van der Maen', 'mu[x] = A + B*x + I/[N - x]', 5, 'vandermaen2', 'mu[x]',
                NaN, 'Quadratic', 'mu[x] = A + B*x + C*[x^2]', 5, 'quadratic', 'mu[x]',
                1961, 'Beard', 'mu[x] = [A*exp(B^x)] / [1 + K*A*exp(B^x)]', 4, 'beard', 'mu[x]',
                1961, 'Makeham-Beard', 'mu[x] = [A*exp(B^x)] / [1 + K*A*exp(B^x)] + C', 4, 'makehambeard', 'mu[x]',
                1979, 'Siler', 'mu[x] = A*exp(-B*x) + C + D*exp(E*x)', 6, 'siler', 'mu[x]',
                1980, 'Heligman-Pollard', 'q[x]/p[x] = A^[(x + B)^C] + D exp[-E log(x/F)^2] + G H^x', 6, 'HP', 'q[x]',
                1980, 'Heligman-Pollard', 'q[x] = A^[(x + B)^C] + D exp[-E log(x/F)^2] + G H^x / [1 + G H^x]', 6, 'HP2', 'q[x]',
                1980, 'Heligman-Pollard', 'q[x] = A^[(x + B)^C] + D exp[-E log(x/F)^2] + G H^x / [1 + K*G*H^x]', 6, 'HP3', 'q[x]',
                1980, 'Heligman-Pollard', 'q[x] = A^[(x + B)^C] + D exp[-E log(x/F)^2] + G H^(x^K) / [1 + G*H^(x^K)]', 6, 'HP4', 'q[x]',
                1983, 'Rogers-Planck', 'q[x] = A0 + A1*exp[-a*x] + A2*exp[b*(x - u) - exp(-c*(x - u))] + A3*exp[d*x]', 6, 'rogersplanck', 'q[x]',
                1987, 'Martinelle', 'mu[x] = [A*exp(B*x) + C] / [1 + D*exp(B*x)] + K*exp(B*x)', 6, 'martinelle', 'mu[x]',
                1992, 'Kannisto', 'mu[x] = A*exp(B*x) / [1 + A*exp(B*x)]', 5, 'kannisto', 'mu[x]',
                1992, 'Carriere', 'l[x] = p1*l[x](weibull) + p2*l[x](invweibull) + p3*l[x](gompertz)', 6, 'carriere1', 'q[x]',
                1992, 'Carriere', 'l[x] = p1*l[x](weibull) + p2*l[x](invgompertz) + p3*l[x](gompertz)', 6, 'carriere2', 'q[x]',
                1992, 'Kostaki', 'q[x]/p[x] = A^[(x+B)^C] + D*exp[-(Ei*log(x/F_))^2] + G*H^x', 6, 'kostaki', 'q[x]'
                )))
    colnames(table) <- c('YEAR', 'NAME', 'MODEL', 'TYPE', 'CODE', 'FIT')
    
    legend <- as.data.frame(matrix(ncol = 2, byrow = T, 
                                   c(1, "Infant mortality",
                                     2, "Accident hump",
                                     3, "Adult mortality",
                                     4, "Adult and/or old-age mortality",
                                     5, "Old-age mortality",
                                     6, "Full age range")))
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





