#' Fit Mortality Laws
#'
#' Fit parametric mortality models given a set of input data which can be 
#' represented by death counts and mid-interval population estimates \code{(Dx, Ex)}
#' or age-specific death rates \code{(mx)} or death probabilities \code{(qx)}. 
#' Using the argument \code{law} one can specify the model to be fitted. 
#' So far 27 parametric model have been implemented; check \code{\link{availableLaws}}
#' function to learn about the available options. The models can be fitted under 
#' the maximum likelihood methodology or by selecting a loss function to be 
#' optimised. See the implemented loss function by running 
#' \code{\link{availableLF}} function.
#' @usage 
#' MortalityLaw(x, Dx = NULL, Ex = NULL, mx = NULL, qx = NULL, 
#'                 law = NULL, 
#'                 opt.method = "poissonL", 
#'                 parS = NULL, 
#'                 fit.this.x = x,
#'                 custom.law = NULL, 
#'                 show = TRUE)
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
#' \code{makeham}, ... To investigate all the possible options, see \code{\link{availableLaws}}
#' function.
#' @param opt.method how would you like to find the parameters? Specify the function 
#' to be optimize. Available options: the Poisson likelihood function 
#' \code{poissonL}; the Binomial likelihood function -\code{binomialL}; 
#' and other 6 loss functions. For more details, check \code{\link{availableLF}} function.
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
#' @return \item{opt.diagnosis}{ the resulted optimization object useful for 
#' checking the convergence etc.} 
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
#' coef(M1)
#' summary(M1)
#' predict(M1, x = 25:95)
#' plot(M1)
#' 
#' # Example 2: ---
#' # We can fit the same model using a different data format 
#' # and a different optimization method. Also we can scale down 'x'.
#' mx <- ahmd$mx[paste(x), ]
#' x.scaled <- x - min(x) + 1
#' M2 <- MortalityLaw(x = x.scaled, 
#'                    mx = mx, 
#'                    law = 'makeham', 
#'                    opt.method = 'LF1')
#' M2
#' 
#' # Example 3: ---
#' # Now let's fit a mortality law that is not defined 
#' # in the package, say a reparameterized Gompertz in 
#' # terms of modal age at death
#' # hx = b*exp(b*(x-m)) (here b and m are the parameters to be estimated)
#' 
#' # A function with 'x' and 'par' as input has to be defined, which returns at least
#' # an object called 'hx' (hazard rate).
#' my_gompertz <- function(x, par = c(b = 0.13, M = 45)){
#'   hx  <- with(as.list(par), b*exp(b*(x - M)) )
#'   return(as.list(environment()))
#' }
#' 
#' M3 <- MortalityLaw(x = x,
#'                    Dx = Dx, 
#'                    Ex = Ex, 
#'                    custom.law = my_gompertz) 
#' summary(M3)
#' # predict M3 for different ages
#' predict(M3, x = 85:130)
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
#' @export
#'
MortalityLaw <- function(x, Dx = NULL, Ex = NULL, mx = NULL, qx = NULL, 
                         law = NULL, opt.method = 'poissonL', parS = NULL, 
                         fit.this.x = x, custom.law = NULL, show = TRUE){
  if (!is.null(custom.law)) {
    law  <- 'custom.law'
    parS <- custom.law(1)$par 
  }
  input <- c(as.list(environment()))
  FMC   <- find.my.case(Dx, Ex, mx, qx)
  C     <- FMC$case
  
  if (FMC$iclass == "numeric") {
    check.MortalityLaw(input) # Check input
    if (show) {pb <- startpb(0, 4); on.exit(closepb(pb)); setpb(pb, 1)} # Set progress bar
    # Find optim coefficients
    opt <- choose_optim(input) 
    fit <- opt$hx
    gof <- c(logLik = opt$logLik, AIC = opt$AIC, BIC = opt$BIC)
    diagnosis <- opt$fn_opt
    cf  <- exp(diagnosis$par) 
    if (show) setpb(pb, 2)
    # Fitted values & residuals
    if (C == "C1_DxEx") resid = Dx/Ex - fit 
    if (C == "C2_mx")   resid = mx - fit 
    if (C == "C3_qx")   resid = qx - fit 
    if (show) setpb(pb, 3)
    # Prepare, arrange, customize output
    if (law == "custom.law") {
      model.info <- "CUSTOM MORTALITY LAW"
    } else {
      availLaws  <- availableLaws()$table
      model.info <- data.frame(availLaws[availLaws$CODE == law, ], row.names = "")    
    }
    
    info   <- list(model.info = model.info, process.date = date())
    if (show) setpb(pb, 4)
    
  } else {
    N  <- FMC$nLT
    if (show) {pb <- startpb(0, N + 1); on.exit(closepb(pb))} # Set progress bar
    cf = fit = gof = resid = diagnosis <- NULL
    for (i in 1:N) {
      if (show) setpb(pb, i)
      M <- suppressMessages(MortalityLaw(x, Dx[, i], Ex[, i], mx[, i], qx[, i], 
                                         law, opt.method, parS, fit.this.x, custom.law, show = FALSE))
      fit   <- cbind(fit, fitted(M))
      gof   <- rbind(gof, M$goodness.of.fit)
      diagnosis[[i]] <- M$diagnosis
      cf    <- rbind(cf, coef(M))
      resid <- cbind(resid, M$residuals)
      info  <- M$info
    }
    rownames(cf) = rownames(gof) = colnames(fit) = colnames(resid) <- FMC$LTnames
    if (show) setpb(pb, N + 1)
  }
  
  output <- list(input = input, info = info, coefficients = cf,
                 fitted.values = fit, residuals = resid,
                 goodness.of.fit = gof, opt.diagnosis = diagnosis)
  output$info$call <- match.call()
  out <- structure(class = "MortalityLaw", output)
  messages.MortalityLaw(input)
  return(out)
}


#' Function to be Optimize
#' @inheritParams MortalityLaw
#' @keywords internal
objective_fun <- function(par, x, Dx, Ex, mx, qx,
                          law, opt.method, custom.law){
  C  <- find.my.case(Dx, Ex, mx, qx)$case
  mu <- eval(call(law, x, par = exp(par)))$hx
  mu[is.infinite(mu)] <- 1 
  
  if (is.null(Ex)) Ex = 1
  if (C == "C1_DxEx") nu = Dx/Ex 
  if (C == "C2_mx")   nu = Dx <- mx 
  if (C == "C3_qx")   nu = Dx <- qx 
  
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
  
  # Here I want to make sure that the optimisation algorithm is not returning 
  # NaN values when if it converges (because that is possible).
  loss[is.infinite(loss)] <- 10^5
  if (sum(is.na(mu)) != 0) loss = loss + 10^5
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
    foo <- function(k) objective_fun(par = k, x = fit.this.x, Dx, Ex, mx, qx,
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
      llik = AIC  = BIC  <- NaN
    }
    
    out <- list(logLik = llik, AIC = AIC, 
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
           m1 <- 'Mortality law not available. Check one of the following models:\n'
           err1 <- paste(m1, paste(models, collapse = ', '))
           stop(err1, call. = FALSE)
         }
         
         function_to_optimize <- availableLF()$table[, 'CODE']
         if (!(opt.method %in% function_to_optimize)) {
           m1 <- 'Choose a different objective function to optimize\n'
           m2 <- 'Check one of the following options:\n'
           err2 <- paste(m1, m2, paste(function_to_optimize, collapse = ', '))
           stop(err2, call. = FALSE)
         }
         if (length(fit.this.x) <= 5) {
           stop(paste("'More observations needed in order to start the fitting.",
                      " Increase the length of 'fit.this.x'"), call. = F)
         }
         if (!all(fit.this.x %in% x)) {
           stop("'fit.this.x' should be a subset of 'x'", call. = F)
         }
       })
}

# Print messages when exit MortalityLaw
#' @inheritParams choose_optim
#' @keywords internal
messages.MortalityLaw <- function(input) {
  with(input, {
    if (min(x) > 1) {
      message("\nIt would be recommended to scaled down the 'x' argument ",
              "so that is begins with a small value [e.g.  x = x - min(x) + 1].\n",
              "This is useful in order to obtain meaningful ", 
              "estimates and sometimes a better fit.")
    }
    if (law %in% c('HP', 'HP2', 'HP3', 'HP4', 'kostaki') & opt.method != "LF2") {
      message(paste("\nFor cases like", law, "the optimization method 'LF2'",
                    " has been observed to return reliable estimates."))
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
  law  <- object$input$law
  if (law == "custom.law") {
    mi <- "CUSTOM MORTALITY LAW"
  } else {
    mi   <- as.matrix(object$info$model.info[, c(2, 3)])
  }
  call <- object$info$call
  res  <- round(summary(as.vector(as.matrix(object$residuals))), 5)
  fv   <- ifelse(!is.null(object$input$qx), 'qx', 'mx')
  digits <- if (all(coef(object) < 1e-3)) 7 else 5
  cf   <- round(coef(object), digits )
  opt  <- object$input$opt.method
  gof  <- round(object$goodness.of.fit, 2)
  
  out  <- list(info = mi, call = call, opt.method = opt, goodness.of.fit = gof, 
               coefficients = cf, fv = fv, dev.resid = res)
  out  <- structure(class = "summary.MortalityLaw", out)
  return(out)
}


#' Print summary.MortalityLaw
#' @param x an object of class \code{"summary.MortalityLaw"}
#' @param ... additional arguments affecting the summary produced.
#' @keywords internal
#' @export
print.summary.MortalityLaw <- function(x, ...) {
  cat(paste(x$info, collapse = ':\n'))
  cat("\n\nCall: ")
  print(x$call)
  cat('\nDeviance Residuals:\n')
  print(x$dev.resid)
  cat('\nFitted values:', x$fv)
  cat('\nCoefficients:\n')
  print(x$coefficients)
  
  if (x$opt.method %in% c("poissonL", "binomialL")) {
    cat('\nGoodness of fit:\n')
    print(x$goodness.of.fit)
  }
}


#' logLik function for MortalityLaw
#' @inheritParams print.MortalityLaw
#' @keywords internal
#' @export
logLik.MortalityLaw <- function(object, ...) {
  c(object$goodness.of.fit["logLik"])
}

#' AIC function for MortalityLaw
#' @inheritParams print.MortalityLaw
#' @keywords internal
#' @export
AIC.MortalityLaw <- function(object, ...) {
  c(object$goodness.of.fit["AIC"])
}


#' Predict function for MortalityLaw
#' @param object an object of class \code{"MortalityLaw"}
#' @param x vector of ages to be considered in prediction
#' @param ... additional arguments affecting the predictions produced.
#' @keywords internal
#' @export
predict.MortalityLaw <- function(object, x, ...){
  law <- object$input$law
  
  if (law == "custom.law") {
    M <- object$input$custom.law
    hx <- M(x = x, coef(object))$hx
  } else {
    M   <- eval(call(law, x = x, par = coef(object)))
    hx  <- M$hx
  }
  return(hx)
}

