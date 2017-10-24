#' Fit Mortality Laws
#'
#' This function can be used to fit parametric mortality models given a set of data. 
#' Using the argument \code{law} one can specify the model to be fitted. 
#' So far 27 parametric model have been implemented; check \code{\link{availableLaws}}
#' function to learn about the available options. The models can be fitted under 
#' the maximum likelihood methodology of by selecting a loss function to be 
#' optimised. See the implemented loss function by running 
#' \code{\link{availableLF}} function.
#' 
#' @details Depending on the complexity of the model, one of following optimization 
#' strategies are employed: 
#' \enumerate{
#' \item{Nelder-Mead method:}{ approximates a local optimum of a problem with n
#'  variables when the objective function varies smoothly and is unimodal. 
#'  For details see \code{\link{optim}}}
#' \item{PORT routines:}{ provides unconstrained optimization and optimization 
#' subject to box constraints for complicated functions. For details check 
#' \code{\link{nlminb}}}
#' \item{Levenberg-Marquardt algorithm:}{ damped least-squares method. 
#' For details check \code{\link{nls.lm}}}
#' }
#' @inheritParams LifeTable
#' @param law the name of the mortality law/model to be fitted. e.g. \code{gompertz}, 
#' \code{makeham}, ... To investigate all the possible options see \code{\link{availableLaws}}
#' function.
#' @param opt.method how would you like to find the parameters? Specify the function 
#' to be optimize. Available options: the Poisson likelihood function 
#' \code{poissonL}; the Binomial likelihood function -\code{binomialL}; 
#' and other 6 loss functions. For more details check \code{\link{availableLF}} function.
#' @param parS starting parameters used in optimization process (optional).
#' @param fit.this.x select the ages to be considered in model fitting. By default 
#' \code{fit.this.x = x}. One may want exclude from the fitting procedure say the 
#' advance ages were the data is sparse.
#' @param custom.law allows you to fit a model that is not defined 
#' in the package. Accepts as input a function.
#' @param show choose whether to display a progress bar during the fitting process. 
#' Logical. Default: \code{TRUE}.
#' @return The output is of \code{"MortalityLaw"} class with the components:
#' @return \item{input}{ list with arguments provided in input. Saved for convenience}
#' @return \item{info}{ short information about the model}
#' @return \item{coefficients}{ estimated coefficients}
#' @return \item{fitted.values}{ fitted values of the selected model}
#' @return \item{residuals}{ residuals} 
#' @return \item{goodness.of.fit}{ list containing goodness of fit measures like 
#' AIC, BIC and log-Likelihood} 
#' @examples
#' # Example 1: ---
#' # Fit Makeham Model for Year of 1950.
#' 
#' x  <- 45:75
#' Dx <- ahmd$Dx[paste(x), "1950"]
#' Ex <- ahmd$Ex[paste(x), "1950"]
#' 
#' M1 <- MortalityLaw(x = x, 
#'                    Dx = Dx, 
#'                    Ex = Ex, 
#'                    law = 'makeham')
#' M1
#' ls(M1)
#' summary(M1)
#' 
#' # Example 2: ---
#' # We can fit the same model using diffrent data 
#' # format and a different optimization method
#' mx <- ahmd$mx[paste(x), ]
#' M2 <- MortalityLaw(x = x, 
#'                    mx = mx, 
#'                    law = 'makeham', 
#'                    opt.method = 'LF1')
#' M2
#' 
#' # Example 3: ---
#' # Now let's fit a mortality law that is not defined 
#' # in the package, say a reparametrize Gompertz in 
#' # terms of modal age at death
#' # hx = b*exp(b*(x-m)) (here b and m are the parameters to be estimated)
#' 
#' my_gompertz <- function(x, par = c(b = 0.13, m = 45)){
#'   hx  <- with(as.list(par), b*exp(b*(x - m)) )
#'   return(as.list(environment()))
#' }
#' 
#' M3 <- MortalityLaw(x = x,
#'                    Dx = Dx, 
#'                    Ex = Ex, 
#'                    custom.law = my_gompertz) 
#' summary(M3)
#' 
#' # Example 4: ---
#' # Fit Heligman-Pollard model for a single 
#' # year in the dataset between age 0 and 100.
#' 
#' x  <- 0:100
#' mx <- ahmd$mx[paste(x), "1950"] # select data
#' M4 <- MortalityLaw(x = x, 
#'                    mx = mx, 
#'                    law = 'HP', 
#'                    opt.method = 'LF2')
#' M4
#' plot(M4)
#' 
#' @export
#'
MortalityLaw <- function(x, mx = NULL, qx = NULL, Dx = NULL, Ex = NULL, 
                         law = NULL, opt.method = 'poissonL', parS = NULL, 
                         fit.this.x = x, custom.law = NULL, show = TRUE){
  if (!is.null(custom.law)) {
    law  <- 'custom.law'
    parS <- custom.law(1)$par 
  }
  input <- c(as.list(environment()))
  
  if (!is.matrix.or.data.frame(mx, qx, Dx, Ex)) {
    check.MortalityLaw(input) # Check input
    if (show) {pb <- startpb(0, 4); on.exit(closepb(pb)); setpb(pb, 1)} # Set progress bar
    
    opt_ <- choose_optim(input) # Find optim coefficients
    gof  <- c(log_Likelihood = opt_$logLikelihood, AIC = opt_$AIC, BIC = opt_$BIC)
    cf   <- exp(opt_$fn_opt$par) 
    fit  <- opt_$hx
    if (show) setpb(pb, 2)
    
    # Fitted values & residuals
    if (!is.null(qx) & is.null(mx) & is.null(Dx) & is.null(Ex)) resid = qx - fit 
    if (!is.null(mx) & is.null(qx) & is.null(Dx) & is.null(Ex)) resid = mx - fit 
    if ( is.null(mx) & is.null(qx) & !is.null(Dx) & !is.null(Ex)) resid = Dx/Ex - fit 
    if (show) setpb(pb, 3)
    
    # Prepare, arrange, customize output
    aLaws  <- availableLaws()$table
    info   <- list(model.info = aLaws[aLaws$CODE == law, ], process.date = date())
    if (show) setpb(pb, 4)
  }
  
  if (is.matrix.or.data.frame(mx, qx, Dx, Ex)) {
    n  <- max(unlist(lapply(list(mx, qx, Dx, Ex), FUN = ncol)))
    if (show) {pb <- startpb(0, n + 1); on.exit(closepb(pb))} # Set progress bar
    
    cf = fit = gof = resid <- NULL
    for (i in 1:n) {
      if (show) setpb(pb, i)
      mdl <- MortalityLaw(x, mx[, i], qx[, i], Dx[, i], Ex[, i], 
                          law, opt.method, parS, fit.this.x, custom.law, show = FALSE)
      cf    <- rbind(cf, coef(mdl))
      gof   <- rbind(gof, mdl$goodness.of.fit)
      fit   <- cbind(fit, fitted(mdl))
      resid <- cbind(resid, mdl$residuals)
    }
    
    c_names <- if (!is.null(Dx)) { colnames(Dx) } else { 
      if (!is.null(mx)) colnames(mx) else colnames(qx) } 
    rownames(cf) = rownames(gof) = colnames(fit) = colnames(resid) <- c_names
    info <- mdl$info
    if (show) setpb(pb, n + 1)
  }
  output <- list(input = input, info = info, coefficients = cf,
                 fitted.values = fit, residuals = resid,
                 goodness.of.fit = gof)
  output$info$call <- match.call()
  out <- structure(class = "MortalityLaw", output)
  return(out)
}


#' is.matrix.or.data.frame?
#' @inheritParams MortalityLaw
#' @keywords internal
is.matrix.or.data.frame <- function(mx, qx, Dx, Ex) {
  c1 = is.matrix(mx) | is.data.frame(mx)
  c2 = is.matrix(qx) | is.data.frame(qx)
  c3 = is.matrix(Dx) | is.data.frame(Dx)
  c4 = is.matrix(Ex) | is.data.frame(Ex)
  return(any(c1, c2, c3, c4))
}



#' Function to be Optimize
#' @inheritParams MortalityLaw
#' @keywords internal
objective_fun <- function(par, x, mx, qx, Dx, Ex,
                          law, opt.method, custom.law){
  par_ <- exp(par)
  mu   <- eval(call(law, x, par_))$hx
  
  if ( is.null(Ex)) Ex = 1
  if (!is.null(mx) & is.null(qx) & is.null(Dx)) nu = Dx <- mx 
  if (!is.null(qx) & is.null(mx) & is.null(Dx)) nu = Dx <- qx 
  if (!is.null(Dx) & !is.null(Ex)) nu <- Dx/Ex 
  
  # compute likelihoods or loss functions
  loss <- switch(opt.method,
                 poissonL  = -(Dx * log(mu) - mu*Ex),
                 binomialL = -(Dx * log(1 - exp(-mu)) - (Ex - Dx)*mu),
                 LF1 = (1 - mu/nu)^2,
                 LF2 = log(mu/nu)^2,
                 LF3 = ((nu - mu)^2)/nu,
                 LF4 = (nu - mu)^2,
                 LF5 = (nu - mu) * log(nu/mu),
                 LF6 = abs(nu - mu))
  
  # Here I want to make sure that the optimisation algorimth is not returning 
  # NaN values when if it converges (because that is possible).
  if (sum(is.na(mu)) != 0) loss = loss + 10^7 
  out <- sum(loss, na.rm = TRUE)
  # because nls.lm function requires a vector we have to do the following:
  if (law %in% c('thiele', 'wittstein')) out = loss
  return(out)
}


#' Select an optimizing method
#' @param input list of all inputs collected from MortalityLaw function
#' @keywords internal
#' 
choose_optim <- function(input){
  with(as.list(input), {
    # Subset the data
    select.x <- x %in% fit.this.x
    qx = qx[select.x]
    mx = mx[select.x]
    Dx = Dx[select.x]
    Ex = Ex[select.x]
    if (is.null(parS)) parS <- bring_parameters(law, parS)
    # Optimize 
    foo <- function(k) objective_fun(par = k, x = fit.this.x, mx, qx, Dx, Ex, 
                                     law, opt.method, custom.law)
    
    if (law %in% c('HP', 'HP2', 'HP3', 'HP4', 'kostaki')) {
      opt <- nlminb(start = log(parS), objective = foo, 
                    control = list(eval.max = 5000, iter.max = 5000))
      opt$fnvalue <- opt$objective
    } else if (law %in% c('thiele', 'wittstein')) {
      opt <- nls.lm(par = log(parS), fn = foo, 
                    control = nls.lm.control(maxfev = 10000, maxiter = 1024))
      opt$fnvalue <- sum(opt$fvec)
    } else {
      opt <- optim(par = log(parS), fn = foo, method = 'Nelder-Mead')
      opt$fnvalue <- opt$value
    }
    
    cf   <- exp(opt$par)
    hx   <- do.call(law, list(x = x, par = cf))$hx
    llik <- log(opt$fnvalue)
    AIC  <- 2*length(parS) - 2*llik
    BIC  <- log(length(fit.this.x)) * length(parS) - 2*llik
    
    if (!(opt.method %in% c('poissonL', 'binomialL'))) { 
      llik = NA 
      AIC  = NA
      BIC  = NA
    }
    
    out <- list(logLikelihood = llik, AIC = AIC, 
                BIC = BIC, fn_opt = opt, hx = hx)
    return(out)
  })
}



#' Function to check input data in MortalityLaw
#' @inheritParams choose_optim
#' @keywords internal
#' 
check.MortalityLaw <- function(input){
  with(input, 
       {
         if (!is.logical(show)) stop("'show' should be TRUE or FALSE")
         
         if (!is.null(mx)) {
           if (length(x) != length(mx)) 
             stop('x and mx do not have the same length!', call. = FALSE)
         }
         
         if (!is.null(Dx)) {
           if (length(x) != length(Dx) | length(x) != length(Ex) ) 
             stop('x, Dx and Ex do not have the same length!', call. = FALSE)
         }
         
         models <- c(as.matrix(availableLaws()$table[, 5]), 'custom.law')
         if ( !(law %in% models)) {
           m1 <- 'Mortality law not available\n'
           m2 <- 'Check one of the following models:\n'
           err1 <- paste(m1, m2, paste(models, collapse = ', '))
           stop(err1, call. = FALSE)
         }
         
         function_to_optimize <- availableLF()$table[, 'CODE']
         if (!(opt.method %in% function_to_optimize)) {
           m1 <- 'Choose a different objective function to optimize\n'
           m2 <- 'Check one of the following options:\n'
           err2 <- paste(m1, m2, paste(function_to_optimize, collapse = ', '))
           stop(err2, call. = FALSE)
         }
         if (law %in% c('vandermaen', 'vandermaen2', 'quadratic') & min(x) > 1) {
           warning(paste('The x vector needs to be scaled down in order to obtain', 
                         'meaningful estimates and a good fit. [e.g.: x = x - min(x)]'), 
                   call. = FALSE) 
         }
       })
}

#' Print MortalityLaw
#' @param x an object of class \code{"MortalityLaw"}
#' @param ... further arguments passed to or from other methods.
#' @keywords internal
#' @export
print.MortalityLaw <- function(x, ...) {
  cat(paste(as.matrix(x$info$model.info[, c(2, 3)]), collapse = ':\n'))
  fv <- ifelse(!is.null(x$input$qx), 'qx', 'mx')
  cat('\n\nFitted values:', fv)
  cat('\nCoefficients :\n')
  digits <- if (all(coef(x) < 1e-3)) 6 else 4
  print(round(coef(x), digits))
}

#' Summary MortalityLaw
#' @param x an object of class \code{"MortalityLaw"}
#' @param ... additional arguments affecting the summary produced.
#' @keywords internal
#' @export
summary.MortalityLaw <- function(object, ...) {
  cat(paste(as.matrix(object$info$model.info[, c(2, 3)]), collapse = ':\n'))
  cat("\n\nCall: ")
  print(object$info$call)
  cat('\nDeviance Residuals:\n')
  print(round(summary(as.vector(as.matrix(object$residuals))), 5))
  
  fv <- ifelse(!is.null(object$input$qx), 'qx', 'mx')
  cat('\nFitted values:', fv)
  cat('\nCoefficients:\n')
  digits <- if (all(coef(object) < 1e-3)) 7 else 5
  print(round(coef(object), digits ))
  
  opt.method <- object$input$opt.method
  if (opt.method %in% c('poissonL, binomialL')) {
    cat('\nGoodness of fit:\n')
    print(round(object$goodness.of.fit, 2))
  }
}

