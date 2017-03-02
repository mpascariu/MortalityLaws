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
  opt_ <- choose_optim(input)
  coef <- opt_$coef
  AIC  <- opt_$AIC
  BIC  <- opt_$BIC
  logLikelihood <- opt_$logLikelihood
  # Fit mortality law
  mli  <- choose_law_info(law)
  mlaw <- choose_law(law, x, par = coef)
  fit  <- mlaw$distribution$hx
  # Compute residuals
  if (!is.null(Dx)) { mx = Dx/Ex }
  resid <- mx - fit
  # Prepare, arrange, customize output
  output <- list(input = input, coefficients = coef, AIC = AIC, BIC = BIC,
                 logLikelihood = logLikelihood, fitted.values = fit, 
                 residuals = resid, distribution = mlaw$distribution, 
                 model_info = mli, fn_opt = opt_$fn_opt, process_date = date())
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
  law1 = c('weibull', 'invweibull', 'opperman', 'carriere1', 'carriere2')
  if (law %in% law1 & min(x) == 0) x_ = x_ + 1
  # Mortality law
  mlaw <- switch(law,
                 demoivre  = demoivre(x_, par),
                 gompertz0 = gompertz0(x_, par),
                 gompertz  = gompertz(x_, par),
                 invgompertz = invgompertz(x_, par),
                 makeham0  = makeham0(x_, par),
                 makeham   = makeham(x_, par),
                 weibull   = weibull(x_, par),
                 invweibull = invweibull(x_, par),
                 kannisto   = kannisto(x_, par),
                 opperman   = opperman(x_, par),
                 HP = heligman_pollard(x_, par),
                 thiele    = thiele(x_, par),
                 wittstein = wittstein(x_, par),
                 siler     = siler(x_, par),
                 carriere1 = carriere1(x_, par),
                 carriere2 = carriere2(x_, par)
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
                  'weibull', 'invweibull', 'carriere1', 'carriere2',
                  'makeham', 'makeham0', 'kannisto', 'siler')){
      opt <- optim(par = log(parS), fn = objective_fun, 
                   law = law, fun = how,
                   x = x, mx = mx, Dx = Dx, Ex = Ex, 
                   method = 'Nelder-Mead')
      coef <- exp(opt$par)
      opt$fnvalue <- opt$value
    }
    if(law %in% c('opperman')){
      opt <- optim(par = parS, fn = objective_fun, 
                   law = law, fun = how,
                   x = x+1, mx = mx, Dx = Dx, Ex = Ex, 
                   method = 'Nelder-Mead', gr = NULL)
      coef <- opt$par
      opt$fnvalue <- opt$value
    }
    if(law %in% c('demoivre')){
      opt <- optimize(f = objective_fun, law = law, fun = how,
                      x = x, mx = mx, Dx = Dx, Ex = Ex,
                      lower = 20, upper = 150, maximum = FALSE)
      coef <- opt$minimum
      opt$fnvalue <- opt$objective
    }
    if(law %in% c('HP')){
      opt <- nlminb(start = log(parS), objective = objective_fun,
                    law = law, fun = how,
                    x = x, mx = mx, Dx = Dx, Ex = Ex,
                    control = list(eval.max = 5000, iter.max = 5000))
      coef <- exp(opt$par)
      opt$fnvalue <- opt$objective
    }
    if(law %in% c('thiele', 'wittstein')){
      opt <- nls.lm(par = log(parS), fn = objective_fun,
                    law = law, fun = how,
                    x = x, mx = mx, Dx = Dx, Ex = Ex,
                    control = nls.lm.control(nprint = 0,
                              maxfev = 10000, maxiter = 1024))
      coef <- exp(opt$par)
      opt$fnvalue <- sum(opt$fvec)
    }
    llik <- opt$fnvalue
    AIC  <- 2*length(parS) - 2*llik
    BIC  <- log(length(x)) * length(parS) - 2*llik
    
    if(!(how %in% c('poissonL', 'binomialL'))){ 
      llik = NA 
      AIC  = NA
      BIC  = NA
    }
    
    out <- list(coef = coef, logLikelihood = llik, AIC = AIC, 
                BIC = BIC, fn_opt = opt)
    return(out)
  })
}

#' Select info
#' @keywords internal
#' 
choose_law_info <-  function(law){
  info = switch (law,
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
