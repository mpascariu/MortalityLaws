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
#' @param fit.this.x select the ages to be cosidered in model fitting. By default 
#' fit.this.x = x. One may want exculde from the fitting procedure say the 
#' advance ages were the data is sparse.
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
#' model2 <- MortalityLaw(x = ages, mx = mx, law = 'makeham', how = 'LF1')
#' model3 <- MortalityLaw(x = ages, Dx = Dx, Ex = Ex, law = 'gompertz', how = 'LF2')
#' model4 <- MortalityLaw(x = ages, mx = mx, law = 'gompertz', how = 'binomialL')
#'  
#' model1
#' ls(model1)
#' summary(model1)
#' plot(model1)
#' @export
#' 
MortalityLaw <- function(x, mx = NULL, Dx = NULL, Ex = NULL, 
                         law, how = 'poissonL', parS = NULL, 
                         fit.this.x = x, ...){
  # Check input
  if (is.null(parS)) { parS = choose_law(law, x)$par }
  input <- c(as.list(environment()))
  check_input(input)
  # Find optim coefficients
  coef <- choose_optim(input)
  # Fit mortality law
  mlaw <- choose_law(law, x, par = coef)
  fit  <- mlaw$distribution$hx
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

# -------------------------------------------------------------
#' Call a mortality law (model)
#' @keywords internal
#' 
choose_law <- function(law, x, par = NULL){
  # Order and scale the x vector
  x <- unique(x[order(x)])
  x_ <- x - min(x)
  # Mortality law
  mlaw <- switch(law,
                 demoivre  = demoivre(x_, par),
                 gompertz0 = gompertz0(x_, par),
                 gompertz  = gompertz(x_, par),
                 invgompertz = invgompertz(x_, par),
                 makeham0  = makeham0(x_, par),
                 makeham   = makeham(x_, par),
                 weibull = weibull(x_, par),
                 invweibull = invweibull(x_, par),
                 kannisto  = kannisto(x_, par),
                 opperman  = opperman(x_+1, par),
                 HP = heligman_pollard(x_, par),
                 thiele = thiele(x_, par),
                 wittstein = wittstein(x_, par)
  )
  hx <- mlaw$hx
  Hx <- mlaw$Hx
  Sx <- exp(-Hx)
  fx <- hx * Sx
  Fx <- 1 - Sx
  if (min(x) != 0) {Hx[1] = Fx[1] = Sx[1] = NA}
  foo <- data.frame(x, x_fit = x_, hx, Hx, fx, Fx, Sx)
  # Output
  out <- list(model_info = mlaw$model_info, 
              par = mlaw$par, distribution = foo)
  return(out)
}

#' Function to be optimize
#' @keywords internal
#' 
objective_fun <- function(par, x, Dx = NULL, Ex = NULL, mx = NULL, 
                          law, fun = 'poissonL'){
  #parameters
  par_ <- exp(par)
  if(law %in% c('demoivre', 'opperman')) par_ = par
  # compute input
  mu <- choose_law(law, x, par_)$distribution$hx
  if (!is.null(mx)) { Dx = mx; Ex = 1 }
  if (!is.null(Dx)) { mx = Dx/Ex; Ex = Ex }
  # compute likelihoods or loss functions
  output1 <- switch(fun,
           poissonL  = -(Dx * log(mu) - mu*Ex),
           binomialL = -(Dx * log(1 - exp(-mu)) - (Ex - Dx)*mu),
           LF1 = (1 - mu/mx)^2,
           LF2 = log(mu/mx)^2,
           LF3 = ((mx - mu)^2)/mx,
           LF4 = (mx - mu)^2,
           LF5 = (mx - mu) * log(mx/mu),
           LF6 = abs(mx - mu))
  out <- sum(output1, na.rm = TRUE)
  # because nls.lm function requires a vector we have to do the following:
  if (law %in% c('thiele', 'wittstein')) out = output1 
  return(out)
}


#' Select an optimizing method
#' @keywords internal
#' 
choose_optim <- function(input){
  with(as.list(input), {
    # Subset the data
    select.x <- x %in% fit.this.x
    x = x[select.x]
    mx = mx[select.x]
    Dx = Dx[select.x]
    Ex = Ex[select.x]
    # Optimize 
    if(law %in% c('gompertz', 'gompertz0', 'invgompertz',
                  'weibull', 'invweibull',
                  'makeham', 'makeham0', 'kannisto')){
      opt <- optim(par = log(parS), fn = objective_fun, 
                   law = law, fun = how,
                   x = x, mx = mx, Dx = Dx, Ex = Ex, 
                   method = 'Nelder-Mead')
      coef <- exp(opt$par)
    }
    if(law %in% c('opperman')){
      opt <- optim(par = parS, fn = objective_fun, 
                   law = law, fun = how,
                   x = x+1, mx = mx, Dx = Dx, Ex = Ex, 
                   method = 'Nelder-Mead', gr = NULL)
      coef <- opt$par
    }
    if(law %in% c('demoivre')){
      opt <- optimize(f = objective_fun, law = law, fun = how,
                      x = x, mx = mx, Dx = Dx, Ex = Ex,
                      lower = 20, upper = 150, maximum = FALSE)
      coef <- opt$minimum
    }
    if(law %in% c('HP')){
      opt <- nlminb(start = log(parS), objective = objective_fun,
                    law = law, fun = how,
                    x = x, mx = mx, Dx = Dx, Ex = Ex,
                    control = list(eval.max = 5000, iter.max = 5000))
      coef <- exp(opt$par)
    }
    if(law %in% c('thiele', 'wittstein')){
      opt <- nls.lm(par = log(parS), fn = objective_fun,
                    law = law, fun = how,
                    x = x, mx = mx, Dx = Dx, Ex = Ex,
                    control = nls.lm.control(nprint = 0,
                    maxfev = 10000, maxiter = 1024))
      coef <- exp(opt$par)
    }
    return(coef)
  })
}












