# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-04 23:32:44
# --------------------------------------------

# ---- LAWS ---------------------------------------

#' Gompertz Mortality Law - 1825
#'
#' @param x vector of age at the beginning of the age classes
#' @param par parameters of the selected model. If NULL the
#' default values will be assigned automatically.
#' @examples gompertz(x = 45:90)
#' @return A list of rates and model parameters
#' @keywords internal
#' @export
gompertz <- function(x, par = NULL){
  par <- bring_parameters(law = 'gompertz', par = par)
  hx  <- with(as.list(par), A*exp(B*x) )
  Hx  <- with(as.list(par), A/B * (exp(B*x) - 1) )
  Sx  <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}


#' Gompertz Mortality Law - informative parameterization
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples gompertz0(x = 45:90)
#' @keywords internal
#' @export
gompertz0 <- function(x, par = NULL){
  par <- bring_parameters(law = 'gompertz0', par = par)
  hx  <- with(as.list(par), (1/sigma) * exp((x - M)/sigma) )
  Hx  <- with(as.list(par), exp(-M/sigma) * (exp(x/sigma) - 1) )
  Sx  <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}

#' Inverse-Gompertz Mortality Law - informative parameterization
#'
#' m - is a measure of location because it is the mode of the density, m > 0
#' sigma - represents the dispersion of the density about the mode, sigma > 0
#' @inheritParams gompertz 
#' @inherit gompertz return
#' @examples invgompertz(x = 15:25)
#' @keywords internal
#' @export
invgompertz <- function(x, par = NULL){
  par <- bring_parameters(law = 'invgompertz', par = par)
  hx  <- with(as.list(par), 1/sigma * exp(-(x - M)/sigma) / (exp(exp(-(x - M)/sigma)) - 1))
  Sx  <- with(as.list(par), (1 - exp(-exp(-(x - M)/sigma))) / (1 - exp(-exp(M/sigma))))
  Hx  <- -log(Sx)
  return(list(hx = hx, par = par, Sx = Sx))
}

#' Makeham Mortality Law - 1860
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples makeham(x = 45:90)
#' @keywords internal
#' @export
makeham <- function(x, par = NULL){
  par <- bring_parameters(law = 'makeham', par = par)
  hx  <- with(as.list(par), A*exp(B*x) + C)
  Hx  <- with(as.list(par), A/B * (exp(B*x) - 1) + x*C )
  Sx  <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}


#' Makeham Mortality Law - informative parameterization
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples makeham0(x = 45:90)
#' @keywords internal
#' @export
makeham0 <- function(x, par = NULL){
  par <- bring_parameters(law = 'makeham0', par = par)
  hx <- with(as.list(par), (1/sigma) * exp((x - M)/sigma) + C)
  Hx <- with(as.list(par), exp(-M/sigma) * (exp(x/sigma) - 1) + x*C)
  Sx <- exp(-Hx)
  return(list(hx = hx, par = par, Sx = Sx))
}


#' Opperman Mortality Law - 1870
#'
#' The model is evaluated at ages shifted by one year (\code{x + 1}), which
#' keeps the term \code{A/sqrt(x)} finite at age 0.
#'
#' The published form writes the middle term with a free sign,
#' \eqn{\mu_x = A/\sqrt{x} + b + C\sqrt{x}} (Oppermann 1870; catalogued with
#' \code{+b} in demofit (Li 2026) and Scholey (2019)). Here it is spelled
#' \code{- B} with \code{B > 0}, i.e. the \code{b = -B < 0} branch, because
#' the fitting engine estimates parameters on the log scale and so requires
#' positivity. The two are equivalent whenever the fitted \code{b} is
#' negative, which is the case for mortality data (the intercept is a
#' mortality floor in the infant and adult U-shape); the constraint only
#' binds when \code{b > 0}, a regime these data do not occupy.
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples opperman(x = 1:25)
#' @keywords internal
#' @export
opperman <- function(x, par = NULL){
  par <- bring_parameters(law = 'opperman', par = par)
  x  <- x + 1
  hx <- with(as.list(par), A/sqrt(x) - B + C*sqrt(x))
  hx <- pmax(0, hx)
  return(list(hx = hx, par = par))
}


#' Thiele Mortality Law - 1871
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples thiele(x = 0:100)
#' @keywords internal
#' @export
thiele <- function(x, par = NULL){
  par <- bring_parameters(law = 'thiele', par = par)
  mu1 <- with(as.list(par), A*exp(-B*x) )
  mu2 <- with(as.list(par), C*exp(-.5*D*(x - E)^2) )
  mu3 <- with(as.list(par), F_*exp(G*x) )
  hx  <- mu1 + mu2 + mu3
  return(list(hx = hx, par = par))
}


#' Wittstein Mortality Law - 1883
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples wittstein(x = 0:100)
#' @keywords internal
#' @export
wittstein <- function(x, par = NULL){
  par <- bring_parameters(law = 'wittstein', par = par)
  hx  <- with(as.list(par), (1/B)*A^-((B*x)^N) + A^-((M - x)^N) )
  return(list(hx = hx, par = par))
}


#' Weibull Mortality Law - 1939
#'
#' Note that if sigma > m, then the mode of the density is 0 and hx is a
#' non-increasing function of x, while if sigma < m, then the mode is
#' greater than 0 and hx is an increasing function.
#' m > 0 is a measure of location
#' sigma > 0 is measure of dispersion
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples weibull(x = 1:20)
#' @keywords internal
#' @export
weibull <- function(x, par = NULL){
  par <- bring_parameters(law = 'weibull', par = par)
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
#' @inherit gompertz return
#' @examples invweibull(x = 1:20)
#' @keywords internal
#' @export
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
#' Implements the published Perks (1932) form. A previous denominator
#' extension (the \code{B*C^-x} term, attributed to Steffensen (1930)) was a
#' misattribution; it now ships separately as \code{steffensen}.
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples perks(x = 50:100)
#' @keywords internal
#' @export
perks <- function(x, par = NULL){
  par <- bring_parameters(law = 'perks', par = par)
  hx  <- with(as.list(par), (A + B*C^x) / (1 + D*C^x))
  return(list(hx = hx, par = par))
}


#' Steffensen Model - 1930
#'
#' The Perks hazard with an additional \code{B*C^-x} term in the
#' denominator, attributed to Steffensen, J.F. (1930), "Infantile mortality
#' from an actuarial point of view", \emph{Skandinavisk Aktuarietidskrift}
#' 13(2), 272-286, \doi{10.1080/03461238.1930.10416902}. The citation is
#' verified; the formula's presence in the 1930 text itself is not (the scan
#' is paywalled). The term peaks at birth and
#' decays geometrically, so the hazard equals the Perks hazard divided by
#' \code{1 + B*C^-x/(1 + D*C^x)}: dampened at young ages and converging to
#' the Perks form at old ages. This is the formula the package shipped as
#' \code{perks} before it was separated out.
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples steffensen(x = 0:100)
#' @keywords internal
#' @export
steffensen <- function(x, par = NULL){
  par <- bring_parameters(law = 'steffensen', par = par)
  hx  <- with(as.list(par), (A + B*C^x) / (B*(C^-x) + 1 + D*C^x))
  return(list(hx = hx, par = par))
}


#' Negative Gompertz Mortality Law - 1871
#'
#' The Gompertz hazard with a negative exponent,
#' \eqn{\mu_x = A \exp(-Bx)}, i.e. the hazard of a negative Gompertz
#' distribution. Thiele (1871, p. 326) proposed it as the term describing the
#' risk of death prior to maturity, and Siler (1979) reused it in his
#' competing-risk model "to account for the hazard due to immaturity". On its
#' own it is a poor description of infancy as a whole (it decays at a constant
#' relative rate) but it is an excellent fit over the post-neonatal period;
#' see Scholey (2019, Fig. 3b).
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples neggompertz(x = 0:15)
#' @keywords internal
#' @export
neggompertz <- function(x, par = NULL){
  par <- bring_parameters(law = 'neggompertz', par = par)
  hx  <- with(as.list(par), A*exp(-B*x) )
  return(list(hx = hx, par = par))
}


#' Pareto II Mortality Law - 1954
#'
#' The hazard of a Pareto type II (Lomax) distribution,
#' \eqn{\mu_x = A / (x + C)}. de Beer and Janssen (2016) write the infancy and
#' childhood hazard in this form, and Vaupel and Yashin (1983) show it is
#' equivalent to a Gamma-exponential frailty model (a mixture of constant
#' individual hazards with Gamma-distributed rates). It is a shifted power
#' hazard with the exponent fixed at one.
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples pareto_2(x = 0:15)
#' @keywords internal
#' @export
pareto_2 <- function(x, par = NULL){
  par <- bring_parameters(law = 'pareto_2', par = par)
  hx  <- with(as.list(par), A/(x + C) )
  return(list(hx = hx, par = par))
}


#' Shifted Power Mortality Law - 2019
#'
#' The Scholey (2019) flexibly-shifted power hazard,
#' \eqn{\mu_x = A (x + C)^{-B}}. This is the hazard function of a shifted
#' Weibull distribution; it is the exponential-truncated power hazard with the
#' exponential term switched off (\eqn{D = 0}). The power term lets the hazard
#' decline faster than an exponential just after birth and the location
#' offset \code{C} keeps it finite at age 0.
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples scholey_shifted_power(x = 0:15)
#' @keywords internal
#' @export
scholey_shifted_power <- function(x, par = NULL){
  par <- bring_parameters(law = 'scholey_shifted_power', par = par)
  hx  <- with(as.list(par), A*(x + C)^(-B) )
  return(list(hx = hx, par = par))
}


#' Scholey Mortality Law - 2019
#'
#' The exponentially-truncated power hazard of Scholey (2019),
#' \eqn{\mu_x = A (x + C)^{-B} \exp(-Dx)}. Using individual-level US birth and
#' death register data, Scholey found that the age-trajectory of infant
#' mortality is initially dominated by a power-law regime and over the course
#' of infancy approaches a constant exponential decline; the product of the
#' two is the best-fitting parametric form he tested (99.9\% of the deviance
#' explained on the 2005-2009 US cohort). The family nests the negative
#' Gompertz, Pareto II, (shifted) power and (shifted) Weibull hazards as
#' special cases: \code{D = 0} gives \code{scholey_shifted_power}, \code{B = 1}
#' with \code{D = 0} gives \code{pareto_2}, and \code{B = 0} gives the negative
#' Gompertz form. The preprint is available from the author's page at SDU.
#'
#' \strong{Age resolution.} The truncation parameter \code{D} is identified
#' only when the fitted age range is wide enough in units of \code{1/D}: it was
#' estimated by Scholey on day-by-day data over the first year of life. On
#' coarse input (single years of age) or on a very short range \code{D}
#' collapses to the optimisation boundary, the exponential term contributes
#' nothing, and the fit reduces to \code{scholey_shifted_power}; in that case a
#' warning is issued and the four-parameter model should not be preferred over
#' the three-parameter one.
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples scholey(x = 0:15)
#' @keywords internal
#' @export
scholey <- function(x, par = NULL){
  par <- bring_parameters(law = 'scholey', par = par)
  hx  <- with(as.list(par), A*(x + C)^(-B)*exp(-D*x) )
  return(list(hx = hx, par = par))
}


#' Van der Maen Model - 1943
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples vandermaen(x = 0:100)
#' @keywords internal
#' @export
vandermaen <- function(x, par = NULL){
  par <- bring_parameters(law = 'vandermaen', par = par)
  d   <- par[['N']] - x
  hx  <- with(as.list(par), A + B*x + C*(x^2) + ifelse(d > 0, I/d, NA_real_))
  return(list(hx = hx, par = par))
}


#' Van der Maen 2 Model - 1943
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples vandermaen(x = 0:100)
#' @keywords internal
#' @export
vandermaen2 <- function(x, par = NULL){
  par <- bring_parameters(law = 'vandermaen2', par = par)
  d   <- par[['N']] - x
  hx  <- with(as.list(par), A + B*x + ifelse(d > 0, I/d, NA_real_))
  return(list(hx = hx, par = par))
}


#' Strehler-Mildvan Model - 1960
#'
#' Implements the published Strehler-Mildvan (1960) form
#' \code{hx = A*exp(B*x)*exp(-(V/B)*(1 - exp(-B*x)))} with parameters
#' \code{A}, \code{B} and \code{V}.
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples strehler_mildvan(x = 30:85)
#' @keywords internal
#' @export
strehler_mildvan <- function(x, par = NULL){
  par <- bring_parameters(law = 'strehler_mildvan', par = par)
  hx  <- with(as.list(par), A*exp(B*x)*exp(-(V/B)*(1 - exp(-B*x))) )
  return(list(hx = hx, par = par))
}


#' Beard Model - 1971
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples beard(x = 50:100)
#' @keywords internal
#' @export
beard <- function(x, par = NULL){
  par <- bring_parameters(law = 'beard', par = par)
  hx  <- with(as.list(par), (A*exp(B*x)) / (1 + K*A*exp(B*x)) )
  return(list(hx = hx, par = par))
}


#' Makeham-Beard Model - 1971
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples beard_makeham(x = 0:100)
#' @keywords internal
#' @export
beard_makeham <- function(x, par = NULL){
  par <- bring_parameters(law = 'beard_makeham', par = par)
  hx  <- with(as.list(par), A*exp(B*x) / (1 + K*A*exp(B*x)) + C)
  return(list(hx = hx, par = par))
}


#' Gamma-Gompertz Model as in Vaupel et al. (1979)
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples ggompertz(x = 50:120)
#' @keywords internal
#' @export
ggompertz <- function(x, par = NULL){
  par <- bring_parameters(law = 'ggompertz', par = par)
  hx  <- with(as.list(par), (A*exp(B*x)) / (1 + (A*G/B)*(exp(B*x) - 1)) )
  return(list(hx = hx, par = par))
}


#' Quadratic Model
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples quadratic(x = 0:100)
#' @keywords internal
#' @export
quadratic <- function(x, par = NULL){
  par <- bring_parameters(law = 'quadratic', par = par)
  hx  <- with(as.list(par), A + B*x + C*(x^2))
  return(list(hx = hx, par = par))
}


#' Siler Mortality Law - 1979
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples siler(x = 0:100)
#' @keywords internal
#' @export
siler <- function(x, par = NULL){
  par <- bring_parameters(law = 'siler', par = par)
  hx <- with(as.list(par), A*exp(-B*x) + C + D*exp(E*x))
  return(list(hx = hx, par = par))
}


#' Heligman-Pollard Mortality Law - 8 parameters - 1980
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples HP(x = 0:100)
#' @keywords internal
#' @export
HP <- function(x, par = NULL){
  par <- bring_parameters(law = 'HP', par = par)
  mu1 <- with(as.list(par), A^((x + B)^C) + G*H^x )
  mu2 <- with(as.list(par), D*exp(-E*(log(x/F_))^2) )
  eta <- ifelse(x == 0, mu1, mu1 + mu2)
  hx <- eta/(1 + eta)
  return(list(hx = hx, par = par))
}

#' Heligman-Pollard 2 Mortality Law - 8 parameters
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples HP2(x = 0:100)
#' @keywords internal
#' @export
HP2 <- function(x, par = NULL){
  par <- bring_parameters(law = 'HP2', par = par)
  mu1 <- with(as.list(par), A^((x + B)^C) + (G*H^x)/(1 + G*H^x) )
  mu2 <- with(as.list(par), D*exp(-E*(log(x/F_))^2) )
  eta <- ifelse(x == 0, mu1, mu1 + mu2)
  hx <- eta
  return(list(hx = hx, par = par))
}

#' Heligman-Pollard 3 Mortality Law - 9 parameters
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples HP3(x = 0:100)
#' @keywords internal
#' @export
HP3 <- function(x, par = NULL){
  par <- bring_parameters(law = 'HP3', par = par)
  mu1 <- with(as.list(par), A^((x + B)^C) + (G*H^x)/(1 + K*G*H^x) )
  mu2 <- with(as.list(par), D*exp(-E*(log(x/F_))^2) )
  eta <- ifelse(x == 0, mu1, mu1 + mu2)
  hx <- eta
  return(list(hx = hx, par = par))
}

#' Heligman-Pollard 4 Mortality Law - 9 parameters
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples HP4(x = 0:100)
#' @keywords internal
#' @export
HP4 <- function(x, par = NULL){
  par <- bring_parameters(law = 'HP4', par = par)
  mu1 <- with(as.list(par), A^((x + B)^C) + (G*H^(x^K)) / (1 + G*H^(x^K)) )
  mu2 <- with(as.list(par), D*exp(-E*(log(x/F_))^2) )
  eta <- ifelse(x == 0, mu1, mu1 + mu2)
  hx <- eta
  return(list(hx = hx, par = par))
}


#' Martinelle Model - 1987
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples martinelle(x = 0:100)
#' @keywords internal
#' @export
martinelle <- function(x, par = NULL){
  par <- bring_parameters(law = 'martinelle', par = par)
  hx  <- with(as.list(par), (A*exp(B*x) + C) / (1 + D*exp(B*x)) + K*exp(B*x))
  return(list(hx = hx, par = par))
}


#' Rogers-Planck Model - 1983
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples rogersplanck(x = 0:100)
#' @keywords internal
#' @export
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
#' Carriere1 = weibull + invweibull + gompertz. The mixture weights P1 and P2
#' are normalised to the simplex before the components are combined.
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples carriere1(x = 0:100)
#' @keywords internal
#' @export
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
#' Carriere2 = weibull + invgompertz + gompertz. The mixture weights P1 and
#' P2 are normalised to the simplex before the components are combined.
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples carriere2(x = 0:100)
#' @keywords internal
#' @export
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
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples kostaki(x = 0:100)
#' @keywords internal
#' @export
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
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples kannisto(x = 85:120)
#' @keywords internal
#' @export
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
#' @inheritParams gompertz
#' @inherit gompertz return
#' @examples kannisto_makeham(x = 85:120)
#' @keywords internal
#' @export
kannisto_makeham <- function(x, par = NULL){
  par <- bring_parameters(law = 'kannisto_makeham', par = par)
  with(as.list(par), {
    hx  <- A*exp(B*x) / (1 + A*exp(B*x)) + C
    return(list(hx = hx, par = par))
  })
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
#' @inheritParams gompertz
#' @return Vector or initial model parameters
#' @noRd
bring_parameters <- function(law, par = NULL) {
  Spar <- switch(law,
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
            kannisto_makeham = c(A = 0.5, B = 0.13, C = 0.001)
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
