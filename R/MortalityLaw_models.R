# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-04 17:46:32
# --------------------------------------------

# ---- LAWS ---------------------------------------

#' De Moivre Mortality Law - 1725
#'
#' The oldest law in the catalogue: survivorship falls linearly to zero at a
#' limiting age, \eqn{l_x = N - x}, so the hazard rises steeply as age
#' approaches \eqn{N}, \eqn{\mu_x = 1/(N - x)}. A historical baseline rather
#' than a curve to graduate data with. USE WITH CARE: the hazard is defined
#' only below \eqn{N}, so a prediction past the fitted ages can be negative,
#' and \code{\link{MortalityLaw}} warns whenever it fits this law.
#' @noRd
demoivre <- function(x, par = NULL){
  par <- bring_parameters(law = 'demoivre', par = par)
  hx  <- 1/(par[['N']] - x)
  return(list(hx = hx, par = par))
}


#' Gompertz Mortality Law - 1825
#'
#' The exponential rise of mortality with age, the classic adult and old-age
#' law. The hazard is unbounded, so fit it over the adult ages.
#' @noRd
gompertz <- function(x, par = NULL){
  par <- bring_parameters(law = 'gompertz', par = par)
  hx  <- with(as.list(par), A*exp(B*x) )
  Hx  <- with(as.list(par), A/B * (exp(B*x) - 1) )
  Sx  <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}


#' Gompertz Mortality Law - informative parameterization
#'
#' The Gompertz hazard in terms of its mode \eqn{M} and dispersion
#' \eqn{\sigma}, \eqn{\mu_x = (1/\sigma) \exp((x - M)/\sigma)}; the same curve
#' as \code{gompertz} with coefficients that read off the plot.
#' @noRd
gompertz0 <- function(x, par = NULL){
  par <- bring_parameters(law = 'gompertz0', par = par)
  hx  <- with(as.list(par), (1/sigma) * exp((x - M)/sigma) )
  Hx  <- with(as.list(par), exp(-M/sigma) * (exp(x/sigma) - 1) )
  Sx  <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}

#' Inverse-Gompertz Mortality Law - informative parameterization
#'
#' The inverse-Gompertz hazard, which falls with age; it describes the decline
#' of mortality after the infant peak.
#' @noRd
invgompertz <- function(x, par = NULL){
  par <- bring_parameters(law = 'invgompertz', par = par)
  hx  <- with(as.list(par), 1/sigma * exp(-(x - M)/sigma) / (exp(exp(-(x - M)/sigma)) - 1))
  Sx  <- with(as.list(par), (1 - exp(-exp(-(x - M)/sigma))) / (1 - exp(-exp(M/sigma))))
  Hx  <- -log(Sx)
  return(list(hx = hx, par = par, Sx = Sx))
}

#' Makeham Mortality Law - 1860
#'
#' The Gompertz hazard plus a constant, \eqn{\mu_x = A \exp(Bx) + C}, so that
#' the age-independent component of mortality is represented too.
#' @noRd
makeham <- function(x, par = NULL){
  par <- bring_parameters(law = 'makeham', par = par)
  hx  <- with(as.list(par), A*exp(B*x) + C)
  Hx  <- with(as.list(par), A/B * (exp(B*x) - 1) + x*C )
  Sx  <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}


#' Makeham Mortality Law - informative parameterization
#'
#' The Makeham hazard with the exponential term in mode/dispersion form,
#' \eqn{\mu_x = (1/\sigma) \exp((x - M)/\sigma) + C}.
#' @noRd
makeham0 <- function(x, par = NULL){
  par <- bring_parameters(law = 'makeham0', par = par)
  hx <- with(as.list(par), (1/sigma) * exp((x - M)/sigma) + C)
  Hx <- with(as.list(par), exp(-M/sigma) * (exp(x/sigma) - 1) + x*C)
  Sx <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}


#' Opperman Mortality Law - 1870
#'
#' A three-term hazard across the whole lifespan, \eqn{\mu_x = A/\sqrt{x + 1} -
#' B + C\sqrt{x + 1}}, evaluated at ages shifted by one year so the term stays
#' finite at age 0. THIS SIGN IS A CHOICE: the published form writes the middle
#' term with a free sign (\eqn{+b}); the engine estimates on the log scale and
#' so requires positivity, hence the \code{- B} (\eqn{b = -B < 0}) branch,
#' which is the branch mortality data occupy. See the opperman entry of
#' \code{\link{availableLaws}}.
#' @noRd
opperman <- function(x, par = NULL){
  par <- bring_parameters(law = 'opperman', par = par)
  x  <- x + 1
  hx <- with(as.list(par), A/sqrt(x) - B + C*sqrt(x))
  hx <- pmax(0, hx)
  return(list(hx = hx, par = par))
}


#' Thiele Mortality Law - 1871
#'
#' A three-component hazard over the whole lifespan: a declining infancy term,
#' a Gaussian accident hump and a rising old-age term.
#' @noRd
thiele <- function(x, par = NULL){
  par <- bring_parameters(law = 'thiele', par = par)
  mu1 <- with(as.list(par), A*exp(-B*x) )
  mu2 <- with(as.list(par), C*exp(-.5*D*(x - E)^2) )
  mu3 <- with(as.list(par), F_*exp(G*x) )
  hx  <- mu1 + mu2 + mu3
  return(list(hx = hx, par = par))
}


#' Wittstein Mortality Law - 1883
#'
#' A two-term law giving a death probability rather than a hazard, \eqn{q_x =
#' (1/B) A^{-(Bx)^N} + A^{-(M - x)^N}}, so it covers both ends of the lifespan.
#' @noRd
wittstein <- function(x, par = NULL){
  par <- bring_parameters(law = 'wittstein', par = par)
  hx  <- with(as.list(par), (1/B)*A^-((B*x)^N) + A^-((M - x)^N) )
  return(list(hx = hx, par = par))
}


#' Weibull Mortality Law - 1939
#'
#' The Weibull hazard; increasing when \eqn{\sigma < M}, non-increasing
#' otherwise. NOT DEFINED AT BIRTH: the hazard is 0 when the shape is greater
#' than one and unbounded when it is smaller, so age 0 is reported as missing
#' and carries no weight in the fit; fit the law from age 1.
#' @noRd
weibull <- function(x, par = NULL){
  par <- bring_parameters(law = 'weibull', par = par)
  hx <- with(as.list(par), 1/sigma * (x/M)^(M/sigma - 1) )
  hx[x == 0] <- NA_real_
  Hx <- with(as.list(par), (x/M)^(M/sigma) )
  Sx <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}


#' Inverse-Weibull Mortality Law
#'
#' The inverse-Weibull hazard; useful for childhood and the teenage years,
#' where the logarithm of the hazard is concave.
#' @noRd
invweibull <- function(x, par = NULL){
  par <- bring_parameters(law = 'invweibull', par = par)
  hx <- with(as.list(par),
             (1/sigma) * (x/M)^(-M/sigma - 1) / (exp((x/M)^(-M/sigma)) - 1) )
  Hx <- with(as.list(par), -log(1 - exp(-(x/M)^(-M/sigma))) )
  Sx <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}


#' Perks Model - 1932
#'
#' The Perks logistic hazard, \eqn{\mu_x = (A + B C^x) / (1 + D C^x)}, which
#' flattens the Gompertz rise at the oldest ages.
#' @noRd
perks <- function(x, par = NULL){
  par <- bring_parameters(law = 'perks', par = par)
  hx  <- with(as.list(par), (A + B*C^x) / (1 + D*C^x))
  return(list(hx = hx, par = par))
}


#' Steffensen Model - 1930
#'
#' The Perks hazard with an extra \code{B*C^-x} denominator term that peaks at
#' birth and decays geometrically, so the hazard is dampened at young ages and
#' converges to Perks at old ages. ATTRIBUTION IS UNVERIFIED: the citation to
#' Steffensen (1930) is real, but whether the 1930 text contains this form is
#' not (the scan is paywalled), so it is documented as attributed.
#' @noRd
steffensen <- function(x, par = NULL){
  par <- bring_parameters(law = 'steffensen', par = par)
  hx  <- with(as.list(par), (A + B*C^x) / (B*(C^-x) + 1 + D*C^x))
  return(list(hx = hx, par = par))
}


#' Negative Gompertz Mortality Law - 1871
#'
#' The Gompertz hazard with a negative exponent, the hazard of a negative
#' Gompertz distribution; proposed by Thiele for the risk of death prior to
#' maturity, and reused by Siler.
#' @noRd
neggompertz <- function(x, par = NULL){
  par <- bring_parameters(law = 'neggompertz', par = par)
  hx  <- with(as.list(par), A*exp(-B*x) )
  return(list(hx = hx, par = par))
}


#' Pareto II Mortality Law - 1954
#'
#' The hazard of a Pareto type II (Lomax) distribution, \eqn{\mu_x = A / (x +
#' C)}; a shifted power hazard with the exponent fixed at one.
#' @noRd
pareto_2 <- function(x, par = NULL){
  par <- bring_parameters(law = 'pareto_2', par = par)
  hx  <- with(as.list(par), A/(x + C) )
  return(list(hx = hx, par = par))
}


#' Shifted Power Mortality Law - 2019
#'
#' The Scholey flexibly-shifted power hazard, \eqn{\mu_x = A (x + C)^{-B}}; a
#' shifted Weibull hazard, and the truncated-power law with the exponential
#' term switched off.
#' @noRd
scholey_shifted_power <- function(x, par = NULL){
  par <- bring_parameters(law = 'scholey_shifted_power', par = par)
  hx  <- with(as.list(par), A*(x + C)^(-B) )
  return(list(hx = hx, par = par))
}


#' Scholey Mortality Law - 2019
#'
#' The Scholey exponentially-truncated power hazard, \eqn{\mu_x = A (x +
#' C)^{-B} \exp(-Dx)}, the best-fitting parametric form on his day-level US
#' infant data; it nests the negative Gompertz, Pareto II, shifted power and
#' shifted Weibull hazards. AGE RESOLUTION MATTERS: \code{D} is identified only
#' on day- or week-level data over the first year; on single years of age it
#' collapses to the boundary and the fit reduces to
#' \code{scholey_shifted_power}, in which case the engine warns.
#' @noRd
scholey <- function(x, par = NULL){
  par <- bring_parameters(law = 'scholey', par = par)
  hx  <- with(as.list(par), A*(x + C)^(-B)*exp(-D*x) )
  return(list(hx = hx, par = par))
}


#' Van der Maen Model - 1943
#'
#' A quadratic hazard with a reciprocal closing term, \eqn{\mu_x = A + Bx +
#' Cx^2 + I/(N - x)}, so the table can close at a finite age \eqn{N}.
#' @noRd
vandermaen <- function(x, par = NULL){
  par <- bring_parameters(law = 'vandermaen', par = par)
  d   <- par[['N']] - x
  hx  <- with(as.list(par), A + B*x + C*(x^2) + ifelse(d > 0, I/d, NA_real_))
  return(list(hx = hx, par = par))
}


#' Van der Maen 2 Model - 1943
#'
#' The linear form of the Van der Maen hazard with the same reciprocal closing
#' term, \eqn{\mu_x = A + Bx + I/(N - x)}.
#' @noRd
vandermaen2 <- function(x, par = NULL){
  par <- bring_parameters(law = 'vandermaen2', par = par)
  d   <- par[['N']] - x
  hx  <- with(as.list(par), A + B*x + ifelse(d > 0, I/d, NA_real_))
  return(list(hx = hx, par = par))
}


#' Strehler-Mildvan Model - 1960
#'
#' The Strehler-Mildvan form, from a model of declining vitality with age; it
#' predicts a negative intercept-slope correlation across populations.
#' @noRd
strehler_mildvan <- function(x, par = NULL){
  par <- bring_parameters(law = 'strehler_mildvan', par = par)
  hx  <- with(as.list(par), A*exp(B*x)*exp(-(V/B)*(1 - exp(-B*x))) )
  return(list(hx = hx, par = par))
}


#' Beard Model - 1971
#'
#' The Beard logistic hazard, \eqn{\mu_x = A \exp(Bx) / (1 + K A \exp(Bx))},
#' which levels off at old age.
#' @noRd
beard <- function(x, par = NULL){
  par <- bring_parameters(law = 'beard', par = par)
  hx  <- with(as.list(par), (A*exp(B*x)) / (1 + K*A*exp(B*x)) )
  return(list(hx = hx, par = par))
}


#' Makeham-Beard Model - 1971
#'
#' The Beard logistic hazard plus a constant, covering the age-independent
#' component and the old-age levelling-off.
#' @noRd
beard_makeham <- function(x, par = NULL){
  par <- bring_parameters(law = 'beard_makeham', par = par)
  hx  <- with(as.list(par), A*exp(B*x) / (1 + K*A*exp(B*x)) + C)
  return(list(hx = hx, par = par))
}


#' Gamma-Gompertz Model - 1979
#'
#' The Gamma-Gompertz hazard, the marginal hazard of a Gompertz population with
#' Gamma-distributed frailty; the frailty produces the old-age levelling-off.
#' @noRd
ggompertz <- function(x, par = NULL){
  par <- bring_parameters(law = 'ggompertz', par = par)
  hx  <- with(as.list(par), (A*exp(B*x)) / (1 + (A*G/B)*(exp(B*x) - 1)) )
  return(list(hx = hx, par = par))
}


#' Quadratic Model
#'
#' A plain quadratic hazard, \eqn{\mu_x = A + Bx + Cx^2}; a smooth baseline
#' over the adult ages that cannot level off at the oldest ages.
#' @noRd
quadratic <- function(x, par = NULL){
  par <- bring_parameters(law = 'quadratic', par = par)
  hx  <- with(as.list(par), A + B*x + C*(x^2))
  return(list(hx = hx, par = par))
}


#' Siler Mortality Law - 1979
#'
#' A three-term competing-risks hazard: a declining infancy term, a constant
#' background term and a rising old-age term.
#' @noRd
siler <- function(x, par = NULL){
  par <- bring_parameters(law = 'siler', par = par)
  hx <- with(as.list(par), A*exp(-B*x) + C + D*exp(E*x))
  return(list(hx = hx, par = par))
}


#' Heligman-Pollard Mortality Law - 8 parameters - 1980
#'
#' The Heligman-Pollard eight-parameter law of the whole lifespan, fitted on
#' the odds of dying \eqn{q_x/p_x}; the fitted quantity is a death probability.
#' @noRd
HP <- function(x, par = NULL){
  par <- bring_parameters(law = 'HP', par = par)
  mu1 <- with(as.list(par), A^((x + B)^C) + G*H^x )
  mu2 <- with(as.list(par), D*exp(-E*(log(x/F_))^2) )
  eta <- ifelse(x == 0, mu1, mu1 + mu2)
  hx <- eta/(1 + eta)
  return(list(hx = hx, par = par))
}

#' Heligman-Pollard 2 Mortality Law - 8 parameters
#'
#' The Heligman-Pollard law with a logistic old-age term, which keeps the
#' hazard bounded.
#' @noRd
HP2 <- function(x, par = NULL){
  par <- bring_parameters(law = 'HP2', par = par)
  mu1 <- with(as.list(par), A^((x + B)^C) + (G*H^x)/(1 + G*H^x) )
  mu2 <- with(as.list(par), D*exp(-E*(log(x/F_))^2) )
  eta <- ifelse(x == 0, mu1, mu1 + mu2)
  hx <- eta
  return(list(hx = hx, par = par))
}

#' Heligman-Pollard 3 Mortality Law - 9 parameters
#'
#' The Heligman-Pollard law with an extra parameter in the old-age term, making
#' it a full logistic that bends at the oldest ages.
#' @noRd
HP3 <- function(x, par = NULL){
  par <- bring_parameters(law = 'HP3', par = par)
  mu1 <- with(as.list(par), A^((x + B)^C) + (G*H^x)/(1 + K*G*H^x) )
  mu2 <- with(as.list(par), D*exp(-E*(log(x/F_))^2) )
  eta <- ifelse(x == 0, mu1, mu1 + mu2)
  hx <- eta
  return(list(hx = hx, par = par))
}

#' Heligman-Pollard 4 Mortality Law - 9 parameters
#'
#' The Heligman-Pollard law with an extra exponent on the age in the old-age
#' term, so the rise in the hazard can accelerate.
#' @noRd
HP4 <- function(x, par = NULL){
  par <- bring_parameters(law = 'HP4', par = par)
  mu1 <- with(as.list(par), A^((x + B)^C) + (G*H^(x^K)) / (1 + G*H^(x^K)) )
  mu2 <- with(as.list(par), D*exp(-E*(log(x/F_))^2) )
  eta <- ifelse(x == 0, mu1, mu1 + mu2)
  hx <- eta
  return(list(hx = hx, par = par))
}


#' Martinelle Model - 1987
#'
#' A generalisation of the Perks formula for old age, with an extra linear term
#' that lets the hazard keep some exponential rise above the logistic plateau.
#' @noRd
martinelle <- function(x, par = NULL){
  par <- bring_parameters(law = 'martinelle', par = par)
  hx  <- with(as.list(par), (A*exp(B*x) + C) / (1 + D*exp(B*x)) + K*exp(B*x))
  return(list(hx = hx, par = par))
}


#' Rogers-Planck Model - 1983
#'
#' A parametric whole-lifespan schedule with infancy, middle-age hump and old-
#' age terms, developed for model life tables.
#' @noRd
rogersplanck <- function(x, par = NULL){
  par <- bring_parameters(law = 'rogersplanck', par = par)
  hx  <- with(as.list(par),
          A0 + A1*exp(-A*x) + A2*exp(B*(x - U) - exp(-C*(x - U))) + A3*exp(D*x))
  return(list(hx = hx, par = par))
}


#' Normalise Carriere Mixture Weights to the Simplex
#'
#' Clamps the first two weights into (0, 1) and rescales them proportionally
#' so that the third weight stays positive.
#' @param P1,P2 Weights of the first two mixture components (numeric).
#' @return Named numeric vector with the normalised weights P1, P2 and P3.
#' @noRd
carriere_weights <- function(P1, P2) {
  f1 <- min(max(P1, 1e-4), 1)
  f2 <- min(max(P2, 1e-4), 1)

  if (f1 + f2 > 1 - 1e-4) {
    scaling <- (1 - 1e-4) / (f1 + f2)
    f1 <- f1 * scaling
    f2 <- f2 * scaling
  }

  f3 <- 1 - f1 - f2
  return(c(P1 = f1, P2 = f2, P3 = f3))
}


#' Carriere Mortality Law - 1992
#'
#' A mixture law: Weibull + inverse-Weibull + Gompertz components combined on
#' the survivorship, with the mixture weights normalised to the simplex.
#' @noRd
carriere1 <- function(x, par = NULL){
  par <- bring_parameters(law = 'carriere1', par = par)
  # Compute distribution functions
  S_wei  <- weibull(x = x, par = unname(par[c('sigma1', 'M1')]))$Sx
  S_iwei <- invweibull(x = x, par = unname(par[c('sigma2', 'M2')]))$Sx
  S_gom  <- gompertz0(x = x, par = unname(par[c('sigma3', 'M3')]))$Sx

  w <- carriere_weights(P1 = par[['P1']], P2 = par[['P2']])
  par['P1'] <- w[['P1']]
  par['P2'] <- w[['P2']]

  Sx <- w[['P1']]*S_wei + w[['P2']]*S_iwei + w[['P3']]*S_gom
  Hx <- -log(Sx)
  hx <- c(Hx[1], diff(Hx)) # here we will need a numerical solution!
  return(list(hx = hx, par = par))
}


#' Carriere Mortality Law - 1992
#'
#' A mixture law: Weibull + inverse-Gompertz + Gompertz components combined on
#' the survivorship, with the mixture weights normalised to the simplex.
#' @noRd
carriere2 <- function(x, par = NULL){
  par <- bring_parameters(law = 'carriere2', par = par)
  # Compute distribution functions
  S_wei  <- weibull(x = x, par = unname(par[c('sigma1', 'M1')]))$Sx
  S_igom <- invgompertz(x = x, par = unname(par[c('sigma2', 'M2')]))$Sx
  S_gom  <- gompertz0(x = x, par = unname(par[c('sigma3', 'M3')]))$Sx

  w <- carriere_weights(P1 = par[['P1']], P2 = par[['P2']])
  par['P1'] <- w[['P1']]
  par['P2'] <- w[['P2']]

  Sx <- w[['P1']]*S_wei + w[['P2']]*S_igom + w[['P3']]*S_gom
  Hx <- -log(Sx)
  hx <- c(Hx[1], diff(Hx)) # here we will need a numerical solution!
  return(list(hx = hx, par = par))
}


#' Kostaki Model - 1992
#'
#' A nine-parameter Heligman-Pollard variant whose accident-hump term has two
#' dispersion parameters, one either side of a cut age, so the hump can be
#' asymmetric.
#' @noRd
kostaki <- function(x, par = NULL){
  par <- bring_parameters(law = 'kostaki', par = par)
  with(as.list(par), {
    # Sometimes the difference between estimated parameters E1 and E2 is
    # very large, in which case the resulting mortality curve will exhibit
    # a significant artificial jump in one age group. I am imposing a
    # restriction below to limit this behaviour.
    if (E1 >= 50*E2) E2 <- E1/50 # This hack seems to work.

    L   <- x <= F_    # Logical
    mu1 <- A^((x + B)^C) + G*H^x
    e1  <- -(E1*log(x/F_))^2
    e2  <- -(E2*log(x/F_))^2
    mu2 <- D*exp(L*e1 + (!L)*e2)
    eta <- ifelse(x == 0, mu1, mu1 + mu2)
    hx  <- eta/(1 + eta)
    return(list(hx = hx, par = par))
  })
}


#' Kannisto Mortality Law - 1998
#'
#' The Kannisto logistic hazard, which rises like Gompertz and levels off at a
#' ceiling of one; the field standard for closing a life table at old age (see
#' the \code{close} and \code{omega} arguments of \code{\link{LifeTable}}).
#' @noRd
kannisto <- function(x, par = NULL){
  par <- bring_parameters(law = 'kannisto', par = par)
  with(as.list(par), {
    hx  <- A*exp(B*x) / (1 + A*exp(B*x))
    Hx  <- (1/B) * log((1 + A*exp(B*x)) / (1 + A))
    Sx  <- exp(-Hx)
    return(list(hx = hx, par = par, Sx = Sx))
  })
}


#' Kannisto-Makeham Mortality Law - 1998
#'
#' The Kannisto logistic hazard plus a constant, for the age-independent
#' component above the logistic ceiling.
#' @noRd
kannisto_makeham <- function(x, par = NULL){
  par <- bring_parameters(law = 'kannisto_makeham', par = par)
  with(as.list(par), {
    hx  <- A*exp(B*x) / (1 + A*exp(B*x)) + C
    return(list(hx = hx, par = par))
  })
}


#' Makeham Log-Quadratic Mortality Law (GM(1,3)) - 1988
#'
#' The generalised Gompertz-Makeham graduation formula of Forfar, McCutcheon
#' and Wilkie: a Makeham constant plus a Gompertz whose log hazard carries a
#' quadratic term, \eqn{\mu_x = A_0 + K \exp(B_1 x - B_2 x^2)}. The quadratic
#' term bends the exponential rise downward, so the hazard decelerates at the
#' oldest ages. This is the family UK pensioner tables are graduated with (the
#' CMI S2 and 08 series) and the best-fitting law for the Canadian CPM2014
#' experience. SIGN IS A CHOICE: the published fits put a negative coefficient
#' on the square, so it is written here as \eqn{-B_2 x^2} with \eqn{B_2 > 0},
#' the branch the engine's positive parameters permit; the accelerating branch
#' is out of reach.
#' @noRd
makeham_logquad <- function(x, par = NULL){
  par <- bring_parameters(law = 'makeham_logquad', par = par)
  hx  <- with(as.list(par), A0 + K*exp(B1*x - B2*x^2) )
  return(list(hx = hx, par = par))
}


#' Gompertz Log-Quadratic Mortality Law (GM(0,3)) - 1988
#'
#' The generalised Gompertz-Makeham formula without the Makeham constant: a
#' Gompertz whose log hazard is quadratic,
#' \eqn{\mu_x = K \exp(B_1 x - B_2 x^2)}. It is the \eqn{GM(0, 3)} member of
#' the family and the log-quadratic law used to test for deceleration in old
#' age; prefer it when background mortality is negligible and the constant of
#' \code{makeham_logquad} is not wanted. SIGN IS A CHOICE, as in
#' \code{makeham_logquad}: only the decelerating branch is reachable.
#' @noRd
gompertz_logquad <- function(x, par = NULL){
  par <- bring_parameters(law = 'gompertz_logquad', par = par)
  hx  <- with(as.list(par), K*exp(B1*x - B2*x^2) )
  return(list(hx = hx, par = par))
}


#' Validate User-Supplied Parameters of a Mortality Law
#'
#' Checks that the supplied parameters form a numeric vector with the right
#' names and strictly positive values, ordered like the law's defaults.
#' @param law Name of the mortality law (character string).
#' @param par User-supplied parameter values.
#' @param Spar Default parameters of the law (named numeric vector).
#' @return A named numeric vector with the validated parameters.
#' @noRd
check_parameters <- function(law, par, Spar) {

  if (!is.numeric(par) || !is.null(dim(par))) {
    stop("'par' for law '", law, "' must be a numeric vector.", call. = FALSE)
  }

  if (!is.null(names(par))) {
    if (anyDuplicated(names(par)) > 0 || !setequal(names(par), names(Spar))) {
      stop(
        "Invalid parameter names for law '", law, "'. Expected: ",
        paste(names(Spar), collapse = ", "), "; got: ",
        paste(names(par), collapse = ", "), ".",
        call. = FALSE)
    }
    par <- par[names(Spar)]
  } else {
    if (length(par) != length(Spar)) {
      stop(
        "'par' for law '", law, "' must have ", length(Spar),
        " elements (", paste(names(Spar), collapse = ", "), "); got ",
        length(par), ".",
        call. = FALSE)
    }
    names(par) <- names(Spar)
  }

  if (anyNA(par) || any(par <= 0)) {
    stop(
      "All parameters in 'par' for law '", law,
      "' must be positive numeric values.",
      call. = FALSE)
  }

  return(par)
}


#' Bring or Rename Starting Parameters in the Law Functions
#'
#' Provides the defaults when \code{par} is \code{NULL}, otherwise matches a
#' named \code{par} by name (unnamed positionally) and validates it.
#' @inheritParams MortalityLaw
#' @param par A named numeric vector of parameter values, matched to the law's
#'   parameters by name, or unnamed and read positionally.
#' @return Vector or initial model parameters
#' @noRd
bring_parameters <- function(law, par = NULL) {
  Spar <- switch(law,
            demoivre    = c(N = 110),
            gompertz    = c(A = 0.0002, B = 0.13),
            gompertz0   = c(sigma = 7.7, M = 49),
            invgompertz = c(sigma = 7.7, M = 49),
            makeham     = c(A = .0002, B = .13, C = .001),
            makeham0    = c(sigma = 7.692308, M = 49, C = .001),
            opperman    = c(A = .04, B = .0004, C = .001),
            thiele      = c(A = .02474, B = .3, C = .004, D = .5,
                           E = 25, F_ = .0001, G = .13),
            wittstein   = c(A = 1.5, B = 1, N = .5, M = 100),
            perks       = c(A = .0005, B = .0002, C = 1.1, D = .01),
            steffensen  = c(A = .0005, B = .02, C = 1.05, D = .1),
            neggompertz = c(A = .02, B = .4),
            pareto_2    = c(A = .01, C = .001),
            scholey_shifted_power = c(A = .01, B = .7, C = .01),
            scholey     = c(A = .01, B = .7, C = .01, D = .1),
            weibull     = c(sigma = 2, M = 1),
            invweibull  = c(sigma = 10, M = 5),
            vandermaen  = c(A = .01, B = 1, C = .01, I = 100, N = 200),
            vandermaen2 = c(A = .01, B = 1, I = 100, N = 200),
            strehler_mildvan = c(A = 0.0001, B = 0.1, V = 1),
            quadratic   = c(A = .01, B = 1, C = .01),
            beard       = c(A = .002, B = .13, K = 1),
            beard_makeham = c(A = .002, B = .13, C = .01, K = 1),
            ggompertz = c(A = .002, B = .13, G = 1),
            siler     = c(A = .0002, B = .13, C = .001, D = .001, E = .013),
            HP        = c(A = .0005, B = .004, C = .08, D = .001,
                          E = 10, F_ = 17, G = .00005, H = 1.1),
            HP2       = c(A = .0005, B = .004, C = .08, D = .001,
                          E = 10, F_ = 17, G = .00005, H = 1.1),
            HP3       = c(A = .0005, B = .004, C = .08, D = .001,
                          E = 10, F_ = 17, G = .00005, H = 1.1, K = 1),
            HP4       = c(A = .0005, B = .004, C = .08, D = .001,
                          E = 10, F_ = 17, G = .00005, H = 1.1, K = 1),
            rogersplanck = c(A0 = .0001, A1 = .02, A2 = .001, A3 = .0001,
                             A = 2, B = .001, C = 100, D = .1, U = .33),
            martinelle = c(A = .001, B = .13, C = .001, D = 0.1, K = .001),
            kostaki    = c(A = .0005, B = .01, C = .10, D = .001,
                            E1 = 3, E2 = .1, F_ = 25, G = .00005, H = 1.1),
            carriere1  = c(P1 = .003, sigma1 = 15, M1 = 2.7,
                           P2 = .007, sigma2 = 6, M2 = 3,
                           sigma3 = 9.5, M3 = 88),
            carriere2  = c(P1 = .01, sigma1 = 2, M1 = 1,
                           P2 = .01, sigma2 = 7, M2 = 49,
                           sigma3 = 7, M3 = 49),
            kannisto   = c(A = 0.5, B = 0.13),
            kannisto_makeham = c(A = 0.5, B = 0.13, C = 0.001),
            makeham_logquad        = c(A0 = .001, K = .001, B1 = .1, B2 = .001),
            gompertz_logquad        = c(K = .001, B1 = .1, B2 = .001)
            )

  if (is.null(Spar)) {
    stop("Unknown mortality law '", law, "'.", call. = FALSE)
  }

  if (is.null(par)) {
    par <- Spar
  } else {
    par <- check_parameters(
      law = law,
      par = par,
      Spar = Spar
      )
  }

  return(par)
}
