# ---- LAWS ---------------------------------------

#' Gompertz Mortality Law
#' 
#' @param x vector of age at the beginning of the age classes
#' @param par parameters of the selected model. If NULL the 
#' \code{\link{bring_parameters}} function will provide default values.
#' @examples gompertz(x = 45:90)
#' @keywords internal
#' @export
gompertz <- function(x, par = NULL){
  par <- bring_parameters('gompertz', par)
  hx  <- with(as.list(par), a*exp(b*x) )
  Hx  <- with(as.list(par), a/b * (exp(b*x) - 1) )
  Sx  <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}


#' Gompertz Mortality Law - informative parametrization
#' @inheritParams gompertz
#' @examples gompertz0(x = 45:90)
#' @keywords internal
#' @export
gompertz0 <- function(x, par = NULL){
  par <- bring_parameters('gompertz0', par)
  hx <- with(as.list(par), (1/sigma) * exp((x - m)/sigma) )
  Hx <- with(as.list(par), exp(-m/sigma) * (exp(x/sigma) - 1) )
  Sx <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}

#' Inverse-Gompertz Mortality Law - informative parametrization
#' m - is a measure of location because it is the mode of the density, m > 0
#' sigma - represents the dispersion of the density about the mode, sigma > 0
#' @inheritParams gompertz
#' @examples invgompertz(x = 15:25)
#' @keywords internal
#' @export
invgompertz <- function(x, par = NULL){
  par <- bring_parameters('invgompertz', par)
  hx = with(as.list(par), 1/sigma * exp(-(x - m)/sigma) / (exp(exp(-(x - m)/sigma)) - 1))
  Sx = with(as.list(par), (1 - exp(-exp(-(x - m)/sigma))) / (1 - exp(-exp(m/sigma))))
  Hx = -log(Sx)
  return(list(hx = hx, par = par, Sx = Sx))
}

#' Makeham Mortality Law
#' @inheritParams gompertz
#' @examples makeham(x = 45:90)
#' @keywords internal
#' @export
makeham <- function(x, par = NULL){
  par <- bring_parameters('makeham', par)
  hx <- with(as.list(par), a*exp(b*x) + c )
  Hx <- with(as.list(par), a/b * (exp(b*x) - 1) + x*c )
  Sx <- exp(-Hx)
  return(list(hx = hx, par = par))
}

#' Makeham Mortality Law - informative parametrization
#' @inheritParams gompertz
#' @examples makeham0(x = 45:90)
#' @keywords internal
#' @export
makeham0 <- function(x, par = NULL){
  par <- bring_parameters('makeham0', par)
  hx <- with(as.list(par), (1/sigma) * exp((x - m)/sigma) + c )
  Hx <- with(as.list(par), exp(-m/sigma) * (exp(x/sigma) - 1) + x*c)
  Sx <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}

#' Weibull Mortality Law
#' Note that if sigma > m, then the mode of the density is 0 and hx is a 
#' non-increasing function of x, while if sigma < m, then the mode is 
#' greater than 0 and hx is an increasing function.
#' m > 0 is a measure of location
#' sigma > 0 is measure of dispersion
#' @inheritParams gompertz
#' @examples weibull(x = 1:20)
#' @keywords internal
#' @export
weibull <- function(x, par = NULL){
  par <- bring_parameters('weibull', par)
  hx <- with(as.list(par), 1/sigma * (x/m)^(m/sigma - 1) )
  Hx <- with(as.list(par), (x/m)^(m/sigma) )
  Sx <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}

#' Inverse-Weibull Mortality Law
#' The Inverse-Weibull proves useful for modeling the childood and teenage years, 
#' because the logarithm of h(x) is a very concave function.
#' m > 0 is a measure of location
#' sigma > 0 is measure of dispersion
#' @inheritParams gompertz
#' @examples invweibull(x = 1:20)
#' @keywords internal
#' @export
invweibull <- function(x, par = NULL){
  par <- bring_parameters('invweibull', par)
  hx <- with(as.list(par), 
            (1/sigma) * (x/m)^(-m/sigma - 1) / (exp((x/m)^(-m/sigma)) - 1) )
  Hx <- with(as.list(par), -log(1 - exp(-(x/m)^(-m/sigma))) )
  Sx <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}

#' Kannisto Mortality Law
#' @inheritParams gompertz
#' @examples kannisto(x = 85:120)
#' @keywords internal
#' @export
kannisto <- function(x, par = NULL){
  par <- bring_parameters('kannisto', par)
  hx <- with(as.list(par), a*exp(b*x) / (1 + a*exp(b*x)) )
  Hx <- with(as.list(par), 1/a * log( (1 + a*exp(b*x)) / (1 + a) ) )
  Sx <- exp(-Hx)
  return(list(hx = hx, par = par))
}

#' Opperman Mortality Law
#' @inheritParams gompertz
#' @examples opperman(x = 1:25)
#' @keywords internal
#' @export
opperman <- function(x, par = NULL){
  par <- bring_parameters('opperman', par)
  x = x + 1
  hx = with(as.list(par), a/sqrt(x) - b + c*sqrt(x))
  hx = pmax(0, hx)
  return(list(hx = hx, par = par))
}

#' Thiele Mortality Law
#' @inheritParams gompertz
#' @examples thiele(x = 0:100)
#' @keywords internal
#' @export
thiele <- function(x, par = NULL){
  par <- bring_parameters('thiele', par)
  mu1 = with(as.list(par), a*exp(-b*x) )
  mu2 = with(as.list(par), c*exp(-.5*d*(x - e)^2) )
  mu3 = with(as.list(par), f*exp(g*x) )
  hx = ifelse(x == 0, mu1 + mu3, mu1 + mu2 + mu3)
  return(list(hx = hx, par = par))
}

#' Wittstein Mortality Law
#' @inheritParams gompertz
#' @examples wittstein(x = 0:100)
#' @keywords internal
#' @export
wittstein <- function(x, par = NULL){
  par <- bring_parameters('wittstein', par)
  hx = with(as.list(par), (1/m)*a^-((m*x)^n) + a^-((M - x)^n) )
  return(list(hx = hx, par = par))
}

#' Carriere Mortality Law
#' Carriere1 = weibull + invweibull + gompertz
#' @inheritParams gompertz
#' @examples carriere1(x = 0:100)
#' @keywords internal
#' @export
carriere1 <- function(x, par = NULL){
  par <- bring_parameters('carriere1', par)
  # Compute distribution functions
  S_wei  = weibull(x, par[c('sigma1', 'm1')])$Sx
  S_iwei = invweibull(x, par[c('sigma1', 'm1')])$Sx
  S_gom  = gompertz0(x, par[c('sigma3', 'm3')])$Sx
  
  f1 <- max(0, min(par['p1'], 1))
  f2 <- max(0, min(par['p2'], 1))
  f3 <- 1 - f1 - f2
  
  Sx = f1*S_wei + f2*S_iwei + f3*S_gom
  Hx = -log(Sx)
  hx = c(Hx[1], diff(Hx)) # here we will need a numerical solution! 
  return(list(hx = hx, par = par))
}

#' Carriere Mortality Law
#' Carriere2 = weibull + invgompertz + gompertz
#' @inheritParams gompertz
#' @examples carriere2(x = 0:100)
#' @keywords internal
#' @export
carriere2 <- function(x, par = NULL){
  par <- bring_parameters('carriere2', par)
  # Compute distribution functions
  S_wei  = weibull(x, par[c('sigma1', 'm1')])$Sx
  S_igom = invgompertz(x, par[c('sigma2', 'm2')])$Sx
  S_gom  = gompertz0(x, par[c('sigma3', 'm3')])$Sx
  
  f1 <- max(0, min(par['p1'], 1))
  f2 <- max(0, min(par['p2'], 1))
  f3 <- 1 - f1 - f2
  
  Sx = f1*S_wei + f2*S_igom + f3*S_gom
  Hx = -log(Sx)
  hx = c(Hx[1], diff(Hx)) # here we will need a numerical solution! 
  return(list(hx = hx, par = par))
}

#' Siler Mortality Law
#' @inheritParams gompertz
#' @examples siler(x = 0:100)
#' @keywords internal
#' @export
siler <- function(x, par = NULL){
  par <- bring_parameters('siler', par)
  hx = with(as.list(par), A*exp(-B*x) + C + D*exp(E*x))
  return(list(hx = hx, par = par))
}


#' Heligman-Pollard Mortality Law - 8 parameters
#' @inheritParams gompertz
#' @examples HP(x = 0:100)
#' @keywords internal
#' @export
HP <- function(x, par = NULL){
  par <- bring_parameters('HP', par)
  mu1 = with(as.list(par), A^((x + B)^C) + G*H^x )
  mu2 = with(as.list(par), D*exp(-E*(log(x/F_))^2) )
  eta = ifelse(x == 0, mu1, mu1 + mu2)
  hx = eta/(1 + eta)
  return(list(hx = hx, par = par))
}

#' Heligman-Pollard 2 Mortality Law - 8 parameters
#' @inheritParams gompertz
#' @examples HP2(x = 0:100)
#' @keywords internal
#' @export
HP2 <- function(x, par = NULL){
  par <- bring_parameters('HP2', par)
  mu1 = with(as.list(par), A^((x + B)^C) + (G*H^x)/(1 + G*H^x) )
  mu2 = with(as.list(par), D*exp(-E*(log(x/F_))^2) )
  eta = ifelse(x == 0, mu1, mu1 + mu2)
  hx = eta
  return(list(hx = hx, par = par))
}

#' Heligman-Pollard 3 Mortality Law - 9 parameters
#' @inheritParams gompertz
#' @examples HP3(x = 0:100)
#' @keywords internal
#' @export
HP3 <- function(x, par = NULL){
  par <- bring_parameters('HP3', par)
  mu1 = with(as.list(par), A^((x + B)^C) + (G*H^x)/(1 + K*G*H^x) )
  mu2 = with(as.list(par), D*exp(-E*(log(x/F_))^2) )
  eta = ifelse(x == 0, mu1, mu1 + mu2)
  hx = eta
  return(list(hx = hx, par = par))
}

#' Heligman-Pollard 4 Mortality Law - 9 parameters
#' @inheritParams gompertz
#' @examples HP4(x = 0:100)
#' @keywords internal
#' @export
HP4 <- function(x, par = NULL){
  par <- bring_parameters('HP4', par)
  mu1 = with(as.list(par), A^((x + B)^C) + (G*H^(x^K)) / (1 + G*H^(x^K)) )
  mu2 = with(as.list(par), D*exp(-E*(log(x/F_))^2) )
  eta = ifelse(x == 0, mu1, mu1 + mu2)
  hx = eta 
  return(list(hx = hx, par = par))
}

#' Perks Model
#' @inheritParams gompertz
#' @examples perks(x = 50:100)
#' @keywords internal
#' @export
perks <- function(x, par = NULL){
  par <- bring_parameters('perks', par)
  hx  <- with(as.list(par), (A + B*C^x) / (B*(C^-x) + 1 + D*C^x) )
  return(list(hx = hx, par = par))
}


#' Beard Model
#' @inheritParams gompertz
#' @examples beard(x = 50:100)
#' @keywords internal
#' @export
beard <- function(x, par = NULL){
  par <- bring_parameters('beard', par)
  hx  <- with(as.list(par), (A*exp(B*x)) / (1 + K*A*exp(B*x)) )
  return(list(hx = hx, par = par))
}


#' Makeham-Beard Model
#' @inheritParams gompertz
#' @examples makehambeard(x = 0:100)
#' @keywords internal
#' @export
makehambeard <- function(x, par = NULL){
  par <- bring_parameters('makehambeard', par)
  hx  <- with(as.list(par), A*exp(B*x) / (1 + K*A*exp(B*x)) + C)
  return(list(hx = hx, par = par))
}


#' Van der Maen Model
#' @inheritParams gompertz
#' @examples vandermaen(x = 0:100)
#' @keywords internal
#' @export
vandermaen <- function(x, par = NULL){
  par <- bring_parameters('vandermaen', par)
  hx  <- with(as.list(par), A + B*x + C*(x^2) + I/(N - x))
  return(list(hx = hx, par = par))
}


#' Van der Maen 2 Model
#' @inheritParams gompertz
#' @examples vandermaen(x = 0:100)
#' @keywords internal
#' @export
vandermaen2 <- function(x, par = NULL){
  par <- bring_parameters('vandermaen2', par)
  hx  <- with(as.list(par), A + B*x + I/(N - x) )
  return(list(hx = hx, par = par))
}

#' Quadratic Model
#' @inheritParams gompertz
#' @examples quadratic(x = 0:100)
#' @keywords internal
#' @export
quadratic <- function(x, par = NULL){
  par <- bring_parameters('quadratic', par)
  hx  <- with(as.list(par), A + B*x + C*(x^2))
  return(list(hx = hx, par = par))
}

#' Martinelle Model
#' @inheritParams gompertz
#' @examples martinelle(x = 0:100)
#' @keywords internal
#' @export
martinelle <- function(x, par = NULL){
  par <- bring_parameters('martinelle', par)
  hx <- with(as.list(par),  (A*exp(B*x) + C) / (1 + D*exp(B*x)) + K*exp(B*x) )
  return(list(hx = hx, par = par))
}


#' Rogers-Planck Model
#' @inheritParams gompertz
#' @examples rogersplanck(x = 0:100)
#' @keywords internal
#' @export
rogersplanck <- function(x, par = NULL){
  par <- bring_parameters('rogersplanck', par)
  hx <- with(as.list(par),   
             A0 + A1*exp(-a*x) + A2*exp(b*(x - u) - exp(-c*(x - u))) + A3*exp(d*x))
  return(list(hx = hx, par = par))
}

#' Kostaki Model
#' @inheritParams gompertz
#' @examples kostaki(x = 0:100)
#' @keywords internal
#' @export
kostaki <- function(x, par = NULL){
  par <- bring_parameters('kostaki', par)
  pr = as.list(par)
  cond1 = with(pr, x <= F_)
  mu1 = with(pr, A^((x + B)^C) + G*H^x )
  mu2 = with(pr, D*exp(cond1*(-(E1*log(x/F_))^2 ) + (!cond1)*(-(E2*log(x/F_))^2 )))
  eta = ifelse(x == 0, mu1, mu1 + mu2)
  hx  = eta/(1 + eta)
  return(list(hx = hx, par = par))
}


# ------------------------------
#' Select Start Parameters
#' @inheritParams MortalityLaw
#' @keywords internal
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

#' Bring or Rename Start Parameters in the Law Functions
#' @inheritParams MortalityLaw
#' @inheritParams gompertz
#' @keywords internal
bring_parameters <- function(law, par = NULL) {
  Spar <- choose_Spar(law)
  if (is.null(par)) par <- Spar
  # If 'par' is provided, just give them a name anyway
  names(par) <- names(Spar) 
  return(par)
}




