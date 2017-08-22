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
#' @param law The name of the mortality law/model to be fitted. e.g. \code{gompertz}, 
#' \code{makeham}, ... To investigate all the possible options see \code{\link{availableLaws}}
#' function.
#' @param how How would you like to find the parameters? Specify the function 
#' to be optimize. Available options: the Poisson likelihood function 
#' \code{poissonL}; the Binomial likelihood function -\code{binomialL}; 
#' and other 6 loss functions. For more details check \code{\link{availableLF}} function.
#' @param parS Starting parameters used in optimization process (optional).
#' @param fit.this.x select the ages to be considered in model fitting. By default 
#' \code{fit.this.x = x}. One may want exclude from the fitting procedure say the 
#' advance ages were the data is sparse.
#' @param custom.law This argument allows you to fit a model that is not defined 
#' in the package. Accepts as input a function.
#' @param show_pb Choose whether to display a progress bar during the fitting process. 
#' Logical. Default value: \code{TRUE}.
#' @return A \code{MortalityLaw} object
#' @examples
#' library(MortalityLaws)
#' 
#' # Example 1: ---------------------------------------
#' # Fit Makeham model for year of 1950.
#' 
#' yr <- 1950
#' ages  <- 35:75
#' Dx <- ahmd$Dx[paste(ages), paste(yr)]
#' Ex <- ahmd$Ex[paste(ages), paste(yr)]
#' 
#' model1 <- MortalityLaw(x = ages, Dx = Dx, Ex = Ex, law = 'makeham')
#' 
#' model1
#' ls(model1)
#' summary(model1)
#' plot(model1)
#' 
#' # we can fit the same model using diffrent data and a different optimization procedure
#' mx <- ahmd$mx[paste(ages), paste(yr)]
#' model1.1 <- MortalityLaw(x = ages, mx = mx, law = 'makeham', how = 'LF1')
#' 
#' # Example 2: ---------------------------------------
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
#' # Example 3: ---------------------------------------
#' # Fit Heligman-Pollard model for every single year in the dataset between age 0 and 100.
#' 
#' ages  <- 0:100
#' mx <- ahmd$mx[paste(ages), ] # select data
#' qx <- convertFx(mx, x = ages, type = 'mx', output = 'qx') # transform mx into qx
#' 
#' model3 = MortalityLaw(x = ages, qx = qx, law = 'HP', how = 'LF2') # fit qx values
#' model3
#' 
#' 
#' @export
#' 
MortalityLaw <- function(x, mx = NULL, qx = NULL, Dx = NULL, Ex = NULL, 
                         law, how = 'poissonL', parS = NULL, 
                         fit.this.x = x, custom.law = NULL, show_pb = TRUE){
  
  if (!is.null(custom.law)) {law = 'custom.law'; parS = custom.law(1)$par}
  if (is.null(parS)) { parS = choose_Spar(law) }
  input <- c(as.list(environment()))
  
  if (!is.matrix.or.data.frame(mx, qx, Dx, Ex)) {
    check.MortalityLaw(input) # Check input
    if (show_pb) {pb <- startpb(0, 4); on.exit(closepb(pb)); setpb(pb, 1)} # Set progress bar
    
    opt_ <- choose_optim(input) # Find optim coefficients
    gof  <- c(log_Likelihood = opt_$logLikelihood, 
              AIC = opt_$AIC, BIC = opt_$BIC)
    if (show_pb) setpb(pb, 2)
    
    mlaw  <- eval(call(law, x, par = opt_$coef)) # Fit mortality law
    
    # Fitted values & residuals
    fit   <- mlaw$hx
    aLaws <- availableLaws()$table
    if (law %in% aLaws[aLaws$FIT == 'q[x]', 'CODE']) {
      if (!is.null(Dx)) qx = convertFx(Dx/Ex, x, type = 'mx', output = 'qx')
      resid <- qx - fit
    } else {
      if (!is.null(Dx)) { mx = Dx/Ex }
      resid <- mx - fit 
    }
    if (show_pb) setpb(pb, 3)
    
    # Prepare, arrange, customize output
    info   <- list(model.info = aLaws[aLaws$CODE == law, ], process.date = date())
    output <- list(input = input, info = info, coefficients = opt_$coef,
                   fitted.values = fit, residuals = resid,
                   optimization.object = opt_$fn_opt, goodness.of.fit = gof)
    output$info$call <- match.call()
    if (show_pb) setpb(pb, 4)
  }
  
  if (is.matrix.or.data.frame(mx, qx, Dx, Ex)) {
    n  <- max(unlist(lapply(list(mx, qx, Dx, Ex), FUN = ncol)))
    if (show_pb) {pb <- startpb(0, n + 1); on.exit(closepb(pb))} # Set progress bar
    
    cf = fit = gof = resid <- NULL
    for (i in 1:n) {
      if (show_pb) setpb(pb, i)
      mdl <- MortalityLaw(x, mx[, i], qx[, i], Dx[, i], Ex[, i], 
                          law, how, parS, fit.this.x, custom.law, 
                          show_pb = FALSE)
      cf    <- rbind(cf, coef(mdl))
      gof   <- rbind(gof, mdl$goodness.of.fit)
      fit   <- cbind(fit, fitted(mdl))
      resid <- cbind(resid, mdl$residuals)
    }
    
    c_names <- if (!is.null(Dx)) { colnames(Dx) } else { 
                  if (!is.null(mx)) colnames(mx) else colnames(qx) } 
    rownames(cf) = rownames(gof) = colnames(fit) = colnames(resid) <- c_names
    
    output <- list(input = input, info = mdl$info, coefficients = cf,
                   fitted.values = fit, residuals = resid,
                   goodness.of.fit = gof)
    if (show_pb) setpb(pb, n + 1)
  }
  out <- structure(class = "MortalityLaw", output)
  return(out)
}


#' @keywords internal
#' 
is.matrix.or.data.frame <- function(mx, qx, Dx, Ex) {
  c1 = is.matrix(mx) | is.data.frame(mx)
  c2 = is.matrix(qx) | is.data.frame(qx)
  c3 = is.matrix(Dx) | is.data.frame(Dx)
  c4 = is.matrix(Ex) | is.data.frame(Ex)
  return(any(c1, c2, c3, c4))
}

# -------------------------------------------------------------
#' Function to be optimize
#' @keywords internal
#' 
objective_fun <- function(par, x, Dx, Ex, mx, qx,
                          law, fun, custom.law){
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
    if (law %in% c('HP', 'HP2', 'HP3', 'HP4', 'kostaki')) {
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



#' Check available loss function 
#' 
#' The function returns information about the implemented loss function used by the 
#' optimization procedure in \code{\link{MortalityLaw}} function. 
#' @return An \code{availableLF} object.
#' @examples 
#' 
#' availableLF()
#' 
#' @export
availableLF <- function(){
  tab <- as.data.frame(
    matrix(c(" -[Dx * log(mu) - mu*Ex]", "poissonL",
             " -[Dx * log(1 - exp(-mu)) - (Ex - Dx)*mu]  ", "binomialL",
             "  [1 - mu/ov]^2", "LF1",
             "  log[mu/ov]^2", "LF2",
             "  [(ov - mu)^2]/ov", "LF3",
             "  [ov - mu]^2", "LF4",
             "  [ov - mu] * log[ov/mu]", "LF5",
             "  |ov - mu|", "LF6"), ncol = 2, byrow = T))
  colnames(tab) <- c("LOSS FUNCTION", "CODE")
  
  legend <- c("Dx - Death counts", "Ex - Exposure", 
              "mu - Estimated value", "ov - Observed value")
  
  hint <- c("Most of the functions work well with <poissonL>, however for complex",
            "mortality laws like Heligman-Pollard (HP) one can obtain a better fit using",
            "other loss function (e.g. LF2). You are strongly encouraged to test",
            "different option before deciding on the final version. The results will be",
            "slightly different.")
  
  out <- structure(class = "availableLF", 
                   list(table = tab, legend = legend, hint = hint))
  return(out)
} 
