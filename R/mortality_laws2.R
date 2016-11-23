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
  check_input(law, how)
  input <- list(x = x, mx = mx, Dx = Dx, Ex = Ex, 
                law = law, how = how, parS = parS)

  if (is.null(parS)) { parS = choose_law(law, x)$par }
  
  opt <- optim(par = log(parS), fn = objective_fun, law = law, 
                 fun = how, x = x, mx = mx, Dx = Dx, 
                 Ex = Ex, method = 'Nelder-Mead')
  coef <- exp(opt$par)
  mlaw <- choose_law(law, x, par = coef)
  fit <- mlaw$distribution$hx
  
  if (!is.null(Dx)) { mx = Dx/Ex }
  resid <- mx - fit
    
  # Prepare, arrange, customize output
  output <- list(input = input, coefficients = coef, fitted = fit, 
                 residuals = resid, distribution = mlaw$distribution, 
                 model_info = mlaw$model_info, process_date = date())
  output$call <- match.call()
  out <- structure(class = "MortalityLaw", output)
  return(out)
}

#' Function to check input data in MortalityLaw
#' @keywords internal
#' 
check_input <- function(law, how){
  models <- c('gompertz', 'makeham')
  # models <- c('demoivre', 'gompertz', 'makeham', 'opperman', 'kannisto',
  #             'HP','thiele', 'wittstein')
  if ( !(law %in% models)) {
    cat('Error: mortality law not available.\n')
    cat('Check one of the following models: \n', models, sep = ' | ')
    stop()
  }
  
  function_to_optimize <- c('poissonL', 'binomialL', 'LAE', 'LSE')
  if (!(how %in% function_to_optimize)) {
    cat('Error: Choose a different function to optimize.\n')
    cat('Check one of the following options: \n', 
        function_to_optimize, sep = ' | ')
    stop()
  }
}

#' Call a mortality law (model) - 2 options so far
#' @keywords internal
#' 
choose_law <- function(law, x, par = NULL){
  out <- switch(law,
                gompertz = gompertz(x, par),
                makeham = makeham(x, par)
  )
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


# =========================

#' Gompertz mortality law
#' @keywords internal
#' 
gompertz <- function(x, par = NULL, u = NULL){
  model_info <- 'Gompertz (1825): h(x) = a*exp(b*x)'
  # default parameters
  a = 0.0002; b = 0.13
  if (!is.null(par)) { a = par[1]; b = par[2] }
  # Order and scale the x vector
  x <- unique(x[order(x)])
  x_ <- x - min(x)
  # Compute distribution functions
  hx <- a*exp(b*x_)
  Hx <- a/b * (exp(b*x_) - 1)
  Sx <- exp(a/b * (1 - exp(b*x_))) # exp(-Hx)
  fx <- a*exp(b*x_) * exp(a/b * (1 - exp(b*x_))) # hx * Sx
  Fx <- 1 - Sx
  foo <- data.frame(x, x_fit = x_, hx, Hx, fx, Fx, Sx)
  # Output
  out <- list(model_info = model_info, par = c(a = a, b = b), 
              distribution = foo)
  return(out)
}

#' Makeham mortality law
#' @keywords internal
#' 
makeham <- function(x, par = NULL, u = NULL){
  model_info <- 'Makeham (1860):  h(x) = a*exp(b*x) + c'
  # default parameters
  a = 0.0002; b = 0.13; c = 0.001
  if (!is.null(par)) { a = par[1]; b = par[2]; c = par[3] }
  # Order and scale the x vector
  x <- unique(x[order(x)])
  x_ <- x - min(x)
  # Compute distribution functions
  hx <- a*exp(b*x_) + c
  Hx <- a/b * (exp(b*x_) - 1) + x_*c
  Sx <- exp(-Hx)
  fx <- hx * Sx
  Fx <- 1 - Sx
  # if (min(x) != 0) {Hx[1] = Fx[1] = Sx[1] = NA}
  foo <- data.frame(x, x_fit = x_, hx, Hx, fx, Fx, Sx)
  # Output
  out <- list(model_info = model_info, par = c(a = a, b = b, c = c), 
              distribution = foo)
  return(out)
}

