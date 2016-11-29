#' Fit mortality law
#'
#' This is a description Fit mortality models
#' @param x Vector of ages
#' @param mx Vector of age-specific death rates
#' @param Dx Vector containing death counts
#' @param Ex Vector containing the exposed population
#' @param law The name of the mortality law/model to be fitted. eg. \code{gompertz}, 
#' \code{makeham}, ...
#' @param how How would you like to find the parameters? Specify the function 
#' to be optimize. Available options: the Poisson likelihood function 
#' \code{poissonL}; the Binomial likelihood function -\code{binomialL}; 
#' Least absolute errors \code{LAE};
#' and Least square errors \code{LSE}.
#' @param parS Starting parameters used in optimization process (optional)
#' @param ... Other argumnets
#' @return A \code{MortalityLaw} object
#' @examples 
#' library(MortalityLaws)
#' 
#' yr <- 2010
#' ages  <- 30:90
#' Dx <- HMD.test.data$Dx[paste(ages), paste(yr)]
#' Ex <- HMD.test.data$Nx[paste(ages), paste(yr)]
#' mx <- HMD.test.data$mx[paste(ages), paste(yr)]
#' 
#' model1 <- MortalityLaw(x = ages, Dx = Dx, Ex = Ex, law = 'makeham')
#' model2 <- MortalityLaw(x = ages, mx = mx, law = 'makeham', how = 'LSE')
#' model3 <- MortalityLaw(x = ages, Dx = Dx, Ex = Ex, law = 'gompertz', how = 'LAE')
#' model4 <- MortalityLaw(x = ages, mx = mx, law = 'gompertz', how = 'binomialL')
#' 
#' model1
#' model2
#' model3
#' model4
#' @export
#' 
MortalityLaw <- function(x, mx = NULL, Dx = NULL, Ex = NULL, 
                         law, how = 'poissonL', parS = NULL, ...){
  # Check input
  input <- c(as.list(environment()))
  check_input(input)
  if (is.null(parS)) { parS = choose_law(law, x)$par }
  # Find optim coefficients
  opt <- optim(par = log(parS), fn = objective_fun, law = law, 
                 fun = how, x = x, mx = mx, Dx = Dx, 
                 Ex = Ex, method = 'Nelder-Mead')
  coef <- exp(opt$par)
  # Fit mortality law
  mlaw <- choose_law(law, x, par = coef)
  fit <- mlaw$distribution$hx
  # Compute residuals
  if (!is.null(Dx)) { mx = Dx/Ex }
  resid <- mx - fit
  # Prepare, arrange, customize output
  output <- list(input = input, coefficients = coef, fitted.values = fit, 
                 residuals = resid, distribution = mlaw$distribution, 
                 model_info = mlaw$model_info, process_date = date())
  output$call <- match.call()
  out <- structure(class = "MortalityLaw", output)
  return(out)
}

#' Call a mortality law (model) - 3 options so far
#' @keywords internal
#' 
choose_law <- function(law, x, par = NULL){
  # Order and scale the x vector
  x <- unique(x[order(x)])
  x_ <- x - min(x)
  # Mortality law
  mlaw <- switch(law,
                gompertz = gompertz(x_, par),
                makeham  = makeham(x_, par),
                kannisto = kannisto(x_, par)
  )
  hx <- mlaw$hx
  Hx <- mlaw$Hx
  Sx <- exp(-Hx)
  fx <- hx * Sx
  Fx <- 1 - Sx
  # if (min(x) != 0) {Hx[1] = Fx[1] = Sx[1] = NA}
  foo <- data.frame(x, x_fit = x_, hx, Hx, fx, Fx, Sx)
  # Output
  out <- list(model_info = mlaw$model_info, 
              par = mlaw$par, distribution = foo)
  return(out)
}

#' Call a function to optimize - 4 options available
#' @keywords internal
#' 
choose_fun <- function(fun, mu, mx, Dx, Ex){
  out <- sum(
    switch(fun,
           poissonL = -(Dx * log(mu) - mu*Ex),
           binomialL = -(Dx * log(1 - exp(-mu)) - (Ex - Dx)*mu),
           LAE = abs(mx - mu),
           LSE = (mx - mu) ^ 2), 
    na.rm = TRUE)
  return(out)
}

#' Function to be optimize
#' @keywords internal
#' 
objective_fun <- function(par, x, Dx = NULL, Ex = NULL, mx = NULL, 
                          law, fun = 'poissonL'){
  par_ <- exp(par)
  mu <- choose_law(law, x, par_)$distribution$hx
  if (!is.null(mx)) { Dx = mx; Ex = 1 }
  if (!is.null(Dx)) { mx = Dx/Ex; Ex = Ex }
  out <- choose_fun(fun, mu, mx, Dx, Ex)
  return(out)
}

# =================================================================

#' Gompertz mortality law
#' @keywords internal
#' 
gompertz <- function(x, par = NULL){
  model_info <- 'Gompertz (1825): h(x) = a*exp(b*x)'
  # default parameters
  a = 0.0002; b = 0.13
  if (!is.null(par)) { a = par[1]; b = par[2] }
  # Compute distribution functions
  hx <- a*exp(b*x)
  Hx <- a/b * (exp(b*x) - 1)
  return(list(model_info = model_info, hx = hx, Hx = Hx,
         par = c(a = a, b = b)))
}

#' Makeham mortality law
#' @keywords internal
#' 
makeham <- function(x, par = NULL){
  model_info <- 'Makeham (1860):  h(x) = a*exp(b*x) + c'
  # default parameters
  a = 0.0002; b = 0.13; c = 0.001
  if (!is.null(par)) { a = par[1]; b = par[2]; c = par[3] }
  # Compute distribution functions
  hx <- a*exp(b*x) + c
  Hx <- a/b * (exp(b*x) - 1) + x*c
  return(list(model_info = model_info, hx = hx, Hx = Hx,
         par = c(a = a, b = b, c = c)))
}

#' Kannisto mortality law
#' @keywords internal
#' 
kannisto <- function(x, par = NULL){
  model_info <- 'Kannisto (1992): u(x) = a*exp(b*x) / [1 + a*exp(b*x)]'
  # default parameters
  a = 0.5; b = 0.13
  if (!is.null(par)) { a = par[1]; b = par[2] }
  # Compute distribution functions
  hx <- a*exp(b*x) / (1 + a*exp(b*x))
  Hx <- 1/a * log( (1 + a*exp(b*x)) / (1 + a) )
  return(list(model_info = model_info, hx = hx, Hx = Hx,
              par = c(a = a, b = b)))
}





