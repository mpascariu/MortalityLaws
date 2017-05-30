#' Fit mortality law
#'
#' This function can be used to fit mortality models given a set of data. 
#' Using the argument \code{law} one can specify the model to be fitted. 
#' So far there are more than 20 parametric model implemented.
#' 
#' @param x Vector of ages
#' @param mx Vector of age-specific death rates
#' @param qx Vector of age-specific probabilities of death
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
#' @param custom.law This argument allows you to fit a model that is not defined 
#' in the package. Accepts as input a function.
#' @param ... Other argumnets
#' @return A \code{MortalityLaw} object
#' @examples 
#' library(MortalityLaws)
#' 
#' yr <- 1950
#' ages  <- 35:75
#' Dx <- ahmd$Dx[paste(ages), paste(yr)]
#' Ex <- ahmd$Nx[paste(ages), paste(yr)]
#' mx <- ahmd$mx[paste(ages), paste(yr)]
#' 
#' # Fit Makeham model
#' model1 <- MortalityLaw(x = ages, Dx = Dx, Ex = Ex, law = 'makeham')
#' 
#' model1
#' ls(model1)
#' summary(model1)
#' plot(model1)
#' 
#' # we can fit the same model using diffrent data and a different optimization procedure
#' model1.1 <- MortalityLaw(x = ages, mx = mx, law = 'makeham', how = 'LF1')
#' 
#' #---------------------------------------
#' # Now let's fit a mortality law that is not defined in the package, say a
#' # reparametrize Gompertz in terms of modal age at death
#' # hx = b*exp(b*(x-m))  (here b and m are the parameters to be estimated)
#' 
#' my_gompertz <- function(x, par = c(b = 0.13, m = 45)){
#'   hx  <- with(as.list(par), b*exp(b*(x - m)) )
#'   return(as.list(environment())) # return everything inside this function
#' }
#' 
#' model2 <- MortalityLaw(x = ages, Dx = Dx, Ex = Ex, custom.law = my_gompertz)
#' summary(model2)
#' plot(model2)
#' 
#' @export
#' 
MortalityLaw <- function(x, mx = NULL, qx = NULL, Dx = NULL, Ex = NULL, 
                         law, how = 'poissonL', parS = NULL, 
                         fit.this.x = x, custom.law = NULL, ...){
  # Check input & set clock
  if (!is.null(custom.law)) {
    law = 'custom.law'
    parS = custom.law(1)$par
    }
  if (is.null(parS)) { parS = choose_Spar(law) }
  input <- c(as.list(environment()))
  
  pb <- startpb(0, 4) # Start the clock!
  on.exit(closepb(pb)) # Stop clock on exit.
  check_input(input)
  setpb(pb, 1)
  
  # Find optim coefficients
  opt_ <- choose_optim(input)
  gof  <- list(AIC = opt_$AIC, BIC = opt_$BIC, 
               logLikelihood = opt_$logLikelihood)
  setpb(pb, 2)
  
  # Fit mortality law
  x_    <- compute_x(x, law)$x_
  mlaw  <- eval(call(law, x_, par = opt_$coef)) # Mortality law
  
  # Fitted values & residuals
  fit   <- mlaw$hx
  if (law %in% c('HP', 'HP2', 'HP3', 'HP4')) {
    if (!is.null(Dx)) qx = convertFx(Dx/Ex, x, type = 'mx', output = 'qx')
    resid <- qx - fit
  } else {
    if (!is.null(Dx)) { mx = Dx/Ex }
    resid <- mx - fit 
  }
  setpb(pb, 3)
  
  # Prepare, arrange, customize output
  info   <- list(model.info = choose_law_info(law), process.date = date())
  output <- list(input = input, info = info, coefficients = opt_$coef,
                 fitted.values = fit, residuals = resid,
                 optimization.object = opt_$fn_opt, goodness.of.fit = gof)
  output$info$call <- match.call()
  out <- structure(class = "MortalityLaw", output)
  setpb(pb, 4)
  return(out)
}


#' @keywords internal
#' 
compute_x <- function(x, law, max_x = 110, ...){
  x      <- unique(x[order(x)])
  x_     <- x
  x_full <- (min(x_ - x)):(max(x_) + max_x - max(x))
  return(as.list(environment()))
}

# -------------------------------------------------------------
#' Function to be optimize
#' @keywords internal
#' 
objective_fun <- function(par, x, Dx = NULL, Ex = NULL, mx = NULL, qx = NULL,
                          law, fun = 'poissonL', custom.law){
  par_ = exp(par)
  
  if (law == 'custom.law') { mu = custom.law(x, par = par_)$hx } 
    else {mu = eval(call(law, x, par_))$hx }
  
  if (!is.null(mx)) { 
    nu <- mx 
    Dx <- mx 
    Ex <- 1 
  }
  if (!is.null(qx)) { 
    nu <- qx 
    Dx <- convertFx(data = qx, x, type = 'qx', output = 'mx') 
    Ex <- 1
  }
  if (!is.null(Dx)) { 
    nu <- Dx/Ex 
    Ex <- Ex }
  
  # compute likelihoods or loss functions
  output1 <- switch(fun,
                    poissonL  = -(Dx * log(mu) - mu*Ex),
                    binomialL = -(Dx * log(1 - exp(-mu)) - (Ex - Dx)*mu),
                    LF1 = (1 - mu/nu)^2,
                    LF2 = log(mu/nu)^2,
                    LF3 = ((nu - mu)^2)/nu,
                    LF4 = (nu - mu)^2,
                    LF5 = (nu - mu) * log(nu/mu),
                    LF6 = abs(nu - mu))
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
    if (law %in% c('HP', 'HP2', 'HP3', 'HP4')) {
      opt <- nlminb(start = log(parS), objective = objective_fun, 
                    custom.law = custom.law,
                    law = law, fun = how,
                    x = x, mx = mx, qx = qx, Dx = Dx, Ex = Ex,
                    control = list(eval.max = 5000, iter.max = 5000))
      coef <- exp(opt$par)
      opt$fnvalue <- opt$objective
    } else {
        if (law %in% c('thiele', 'wittstein')) {
          opt <- nls.lm(par = log(parS), fn = objective_fun, 
                        custom.law = custom.law,
                        law = law, fun = how,
                        x = x, mx = mx, qx = qx, Dx = Dx, Ex = Ex,
                        control = nls.lm.control(nprint = 0,
                                  maxfev = 10000, maxiter = 1024))
          coef <- exp(opt$par)
          opt$fnvalue <- sum(opt$fvec)
        } else {
          opt <- optim(par = log(parS), fn = objective_fun, 
                       custom.law = custom.law,
                       law = law, fun = how,
                       x = x, mx = mx, qx = qx, Dx = Dx, Ex = Ex, 
                       method = 'Nelder-Mead')
          coef <- exp(opt$par)
          opt$fnvalue <- opt$value
        }
    }
    
    llik <- log(opt$fnvalue)
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


