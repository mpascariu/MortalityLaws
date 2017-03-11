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
  # Check input & set clock
  if (is.null(parS)) { parS = choose_Spar(law) }
  input <- c(as.list(environment()))
  pb <- startpb(0, 4) # Start the clock!
  on.exit(closepb(pb)) # Stop clock on exit.
  check_input(input)
  setpb(pb, 1)
  
  # Find optim coefficients
  opt_ <- choose_optim(input)
  coef <- opt_$coef
  AIC  <- opt_$AIC
  BIC  <- opt_$BIC
  logLikelihood <- opt_$logLikelihood
  setpb(pb, 2)
  
  # Fit mortality law
  mli  <- choose_law_info(law)
  mlaw <- choose_law(x, law, par = coef)
  m.dist <- mort.distrib(x, law, par = coef)
  setpb(pb, 3)
  
  # Fitted values and residuals
  if (!is.null(Dx)) { mx = Dx/Ex }
  fit   = mlaw$hx
  resid = mx - fit
  # Prepare, arrange, customize output
  output <- list(input = input, coefficients = coef, AIC = AIC, BIC = BIC,
                 logLikelihood = logLikelihood, fitted.values = fit, 
                 residuals = resid, numerical.approx = m.dist,
                 model.info = mli, optimization.object = opt_$fn_opt, 
                 process.date = date())
  output$call <- match.call()
  out <- structure(class = "MortalityLaw", output)
  setpb(pb, 4)
  return(out)
}

# -------------------------------------------------------------
#' @keywords internal
#' 
mort.distrib <- function(x, law, par, ...) {
  x_ <- compute_x(x, law, ...)$x_
  x_full <- compute_x(x, law, ...)$x_full
  hxfun <- function(law, x, par) {
    hx <- eval(call(law, x, par))$hx
    hx[is.infinite(hx)] <- max(hx[!is.infinite(hx)]) # eliminare Inf
    hx[is.na(hx)] <- 0 # eliminate NaN to avoid error in intergation in iHazard
    return(hx)
  }
  
  hx_ = hxfun(law, x_full, par)
  Hx_ = iHazard(x = x_full, law, par, fun = hxfun)
  Sx_ = exp(-Hx_)
  fx_ = hx_ * Sx_
  if (sum(fx_ > 1)) fx_ = fx_ / sum(fx_)
  Fx_ = 1 - Sx_
  
  Hx_[Hx_ == 0] <- NaN # remove artificial zero to avoid weird plots
  fx_[fx_ == 0] <- NaN
  Sx_[Sx_ == 0] <- NaN
  Fx_[Fx_ == 0 & Sx_ != 1] <- NaN
  hx_[hx_ == 0] <- NaN
  out <- data.frame(x = 0:110, x_fit = x_full, 
                   hx = hx_, Hx = Hx_, fx = fx_, 
                   Fx = Fx_, Sx = Sx_)
  return(out)
}

#' Integarte hazard function
#' @keywords internal
#' 
iHazard <- function(x, law, par, fun){
  x = x + 1e-15
  Hx <- NULL
  for (j in 1:length(x)) {
    Hx[j] <- integrate(f = fun, par = par, subdivisions = 111L, 
                       law = law, lower = x[1], upper = x[j])$value
  }
  return(Hx)
}

#' @keywords internal
#' 
compute_x <- function(x, law, max_x = 110, ...){
  infant.mort.laws = c('weibull', 'invweibull', 'opperman')
  full.mort.laws = c('thiele', 'HP', 'wittstein', 'siler', 
                     'carriere1', 'carriere2')
  # Order and scale the x vector
  x <- unique(x[order(x)])
  x_ <- x - min(x) + 1
  if (law %in% infant.mort.laws) x_ = x + 1
  if (law %in% full.mort.laws) x_ = x + 1
  x_full <- (min(x_ - x)):(max(x_) + max_x - max(x))
  return(as.list(environment()))
}

# -------------------------------------------------------------
#' Call a mortality law (model)
#' @keywords internal
#' 
choose_law <- function(x, law, par = NULL){
  x_   <- compute_x(x, law)$x_
  eval(call(law, x_, par)) # Mortality law
}

#' Function to be optimize
#' @keywords internal
#' 
objective_fun <- function(par, x, Dx = NULL, Ex = NULL, mx = NULL, 
                          law, fun = 'poissonL'){
  par_ = exp(par)
  mu <- choose_law(x, law, par_)$hx
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
    if (law %in% c('gompertz', 'gompertz0', 'invgompertz', 'opperman',
                  'weibull', 'invweibull', 'carriere1', 'carriere2',
                  'makeham', 'makeham0', 'kannisto', 'siler')) {
      opt <- optim(par = log(parS), fn = objective_fun, 
                   law = law, fun = how,
                   x = x, mx = mx, Dx = Dx, Ex = Ex, 
                   method = 'Nelder-Mead')
      coef <- exp(opt$par)
      opt$fnvalue <- opt$value
    }
    if (law %in% c('HP')) {
      opt <- nlminb(start = log(parS), objective = objective_fun,
                    law = law, fun = how,
                    x = x, mx = mx, Dx = Dx, Ex = Ex,
                    control = list(eval.max = 5000, iter.max = 5000))
      coef <- exp(opt$par)
      opt$fnvalue <- opt$objective
    }
    if (law %in% c('thiele', 'wittstein')) {
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
    
    if (!(how %in% c('poissonL', 'binomialL'))) { 
      llik = NA 
      AIC  = NA
      BIC  = NA
    }
    
    out <- list(coef = coef, logLikelihood = llik, AIC = AIC, 
                BIC = BIC, fn_opt = opt)
    return(out)
  })
}


