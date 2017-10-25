# ---- LAWS ---------------------------------------

#' Gompertz Mortality Law
#' 
#' @param x vector of age at the beginning of the age classes
#' @param par parameters of the selected model. If NULL the 
#' default values will be assigned automatically.
#' @examples gompertz(x = 45:90)
#' @keywords internal
#' @export
gompertz <- function(x, par = NULL){
  par <- bring_parameters('gompertz', par)
  hx  <- with(as.list(par), A*exp(B*x) )
  Hx  <- with(as.list(par), A/B * (exp(B*x) - 1) )
  Sx  <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}


#' Gompertz Mortality Law - informative parameterization
#' @inheritParams gompertz
#' @examples gompertz0(x = 45:90)
#' @keywords internal
#' @export
gompertz0 <- function(x, par = NULL){
  par <- bring_parameters('gompertz0', par)
  hx <- with(as.list(par), (1/sigma) * exp((x - M)/sigma) )
  Hx <- with(as.list(par), exp(-M/sigma) * (exp(x/sigma) - 1) )
  Sx <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}

#' Inverse-Gompertz Mortality Law - informative parameterization
#' 
#' m - is a measure of location because it is the mode of the density, m > 0
#' sigma - represents the dispersion of the density about the mode, sigma > 0
#' @inheritParams gompertz
#' @examples invgompertz(x = 15:25)
#' @keywords internal
#' @export
invgompertz <- function(x, par = NULL){
  par <- bring_parameters('invgompertz', par)
  hx = with(as.list(par), 1/sigma * exp(-(x - M)/sigma) / (exp(exp(-(x - M)/sigma)) - 1))
  Sx = with(as.list(par), (1 - exp(-exp(-(x - M)/sigma))) / (1 - exp(-exp(M/sigma))))
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
  hx <- with(as.list(par), A*exp(B*x) + C)
  Hx <- with(as.list(par), A/B * (exp(B*x) - 1) + x*C )
  Sx <- exp(-Hx)
  return(list(hx = hx, par = par))
}

#' Makeham Mortality Law - informative parameterization
#' @inheritParams gompertz
#' @examples makeham0(x = 45:90)
#' @keywords internal
#' @export
makeham0 <- function(x, par = NULL){
  par <- bring_parameters('makeham0', par)
  hx <- with(as.list(par), (1/sigma) * exp((x - M)/sigma) + C)
  Hx <- with(as.list(par), exp(-M/sigma) * (exp(x/sigma) - 1) + x*C)
  Sx <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}

#' Weibull Mortality Law
#' 
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
  hx <- with(as.list(par), 1/sigma * (x/M)^(M/sigma - 1) )
  hx[x == 0] <- 1
  Hx <- with(as.list(par), (x/M)^(M/sigma) )
  Sx <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}

#' Inverse-Weibull Mortality Law
#' 
#' The Inverse-Weibull proves useful for modelling the childhood and teenage years, 
#' because the logarithm of h(x) is a concave function.
#' m > 0 is a measure of location
#' sigma > 0 is measure of dispersion
#' @inheritParams gompertz
#' @examples invweibull(x = 1:20)
#' @keywords internal
#' @export
invweibull <- function(x, par = NULL){
  par <- bring_parameters('invweibull', par)
  hx <- with(as.list(par), 
             (1/sigma) * (x/M)^(-M/sigma - 1) / (exp((x/M)^(-M/sigma)) - 1) )
  Hx <- with(as.list(par), -log(1 - exp(-(x/M)^(-M/sigma))) )
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
  hx <- with(as.list(par), A*exp(B*x) / (1 + A*exp(B*x)) )
  Hx <- with(as.list(par), 1/A * log( (1 + B*exp(B*x)) / (1 + A) ) )
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
  hx = with(as.list(par), A/sqrt(x) - B + C*sqrt(x))
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
  mu1 = with(as.list(par), A*exp(-B*x) )
  mu2 = with(as.list(par), C*exp(-.5*D*(x - E)^2) )
  mu3 = with(as.list(par), F_*exp(G*x) )
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
  hx = with(as.list(par), (1/B)*A^-((B*x)^N) + A^-((M - x)^N) )
  return(list(hx = hx, par = par))
}

#' Carriere Mortality Law
#' 
#' Carriere1 = weibull + invweibull + gompertz
#' @inheritParams gompertz
#' @examples carriere1(x = 0:100)
#' @keywords internal
#' @export
carriere1 <- function(x, par = NULL){
  par <- bring_parameters('carriere1', par)
  # Compute distribution functions
  S_wei  = weibull(x, par[c('sigma1', 'M1')])$Sx
  S_iwei = invweibull(x, par[c('sigma1', 'M2')])$Sx
  S_gom  = gompertz0(x, par[c('sigma3', 'M3')])$Sx
  
  f1 <- par['P1'] <- max(0.0001, min(par['P1'], 1))
  f2 <- par['P2'] <- max(0.0001, min(par['P2'], 1))
  f3 <- 1 - f1 - f2
  
  Sx = f1*S_wei + f2*S_iwei + f3*S_gom
  Sx = pmax(0, pmin(1, Sx))
  Hx = -log(Sx)
  hx = c(Hx[1], diff(Hx)) # here we will need a numerical solution! 
  return(list(hx = hx, par = par))
}

#' Carriere Mortality Law
#' 
#' Carriere2 = weibull + invgompertz + gompertz
#' @inheritParams gompertz
#' @examples carriere2(x = 0:100)
#' @keywords internal
#' @export
carriere2 <- function(x, par = NULL){
  par <- bring_parameters('carriere2', par)
  # Compute distribution functions
  S_wei  = weibull(x, par[c('sigma1', 'M1')])$Sx
  S_igom = invgompertz(x, par[c('sigma2', 'M2')])$Sx
  S_gom  = gompertz0(x, par[c('sigma3', 'M3')])$Sx
  
  f1 <- par['P1'] <- max(0.0001, min(par['P1'], 1))
  f2 <- par['P2'] <- max(0.0001, min(par['P2'], 1))
  f3 <- 1 - f1 - f2
  
  Sx = f1*S_wei + f2*S_igom + f3*S_gom
  Sx = pmax(0, pmin(1, Sx))
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
             A0 + A1*exp(-A*x) + A2*exp(B*(x - U) - exp(-C*(x - U))) + A3*exp(D*x))
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
         demoivre    = c(A = 105),
         gompertz    = c(A = .0002, B = 0.13),
         gompertz0   = c(sigma = 7.7, M = 49),
         invgompertz = c(sigma = 7.7, M = 49),
         makeham     = c(A = .0002, B = .13, C = .001),
         makeham0    = c(sigma = 7.692308, M = 49, C = .001),
         opperman    = c(A = .04, B = .0004, C = .001),
         thiele      = c(A = .02474, B = .3, C = .004, D = .5, 
                         E = 25, F_ = .0001, G = .13),
         wittstein   = c(A = 1.5, B = 1, N = .5, M = 100),
         weibull     = c(sigma = 2, M = 1),
         invweibull  = c(sigma = 10, M = 5),
         HP          = c(A = .0005, B = .004, C = .08, D = .001, 
                         E = 10, F_ = 17, G = .00005, H = 1.1),
         HP2         = c(A = .0005, B = .004, C = .08, D = .001, 
                         E = 10, F_ = 17, G = .00005, H = 1.1),
         HP3         = c(A = .0005, B = .004, C = .08, D = .001, 
                         E = 10, F_ = 17, G = .00005, H = 1.1, K = 1),
         HP4         = c(A = .0005, B = .004, C = .08, D = .001, 
                         E = 10, F_ = 17, G = .00005, H = 1.1, K = 1),
         siler       = c(A = .0002, B = .13, C = .001, D = .001, E = .013),
         kannisto    = c(A = 0.5, B = 0.13),
         carriere1   = c(P1 = .003, sigma1 = 15, M1 = 2.7, 
                         P2 = .007, sigma2 = 6, M2 = 3, 
                         sigma3 = 9.5, M3 = 88),
         carriere2   = c(P1 = .01, sigma1 = 2, M1 = 1, 
                         P2 = .01, sigma2 = 7, M2 = 49, 
                                   sigma3 = 7, M3 = 49),
         perks        = c(A = .002, B = .13, C = .01, D = .01),
         beard        = c(A = .002, B = .13, K = 1),
         makehambeard = c(A = .002, B = .13, C = .01, K = 1),
         vandermaen   = c(A = .01, B = 1, C = .01, I = 100, N = 200),
         vandermaen2  = c(A = .01, B = 1, I = 100, N = 200),
         quadratic    = c(A = .01, B = 1, C = .01),
         martinelle   = c(A = .001, B = .13, C = .001, D = 0.1, K = .001),
         rogersplanck = c(A0 = .0001, A1 = .02, A2 = .001, A3 = .0001, 
                          A = 2, B = .001, C = 100, D = .1, U = .33),
         kostaki      = c(A = .0005, B = .01, C = .10, D = .001, 
                          E1 = 3, E2 = .1, F_ = 25, G = .00005, H = 1.1)
  )
}

#' Bring or Rename Starting Parameters in the Law Functions
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
