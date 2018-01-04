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
#'                 scale.x = FALSE,
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
#' @param law The name of the mortality law/model to be fitted. e.g. 
#' \code{gompertz}, \code{makeham}, ... To investigate all the possible options, 
#' see \code{\link{availableLaws}} function.
#' @param opt.method How would you like to find the parameters? Specify the 
#' function to be optimize. Available options: the Poisson likelihood function 
#' \code{poissonL}; the Binomial likelihood function -\code{binomialL}; and 
#' other 6 loss functions. For more details, check \code{\link{availableLF}} function.
#' @param parS Starting parameters used in optimization process (optional).
#' @param fit.this.x Select the ages to be considered in model fitting. By default 
#' \code{fit.this.x = x}. One may want exclude from the fitting procedure say the 
#' advance ages were the data is sparse.
#' @param scale.x Logical. Scale down \code{"x"} vector so that is begins with 
#' a small value. This is useful in order to obtain meaningful estimates and 
#' sometimes a better fit. Default: \code{FALSE}. Method: \code{new.x = x - min(x) + 1}.
#' @param custom.law Allows you to fit a model that is not defined 
#' in the package. Accepts as input a function.
#' @param show Choose whether to display a progress bar during the fitting process. 
#' Logical. Default: \code{TRUE}.
#' @return The output is of \code{"MortalityLaw"} class with the components:
#' @return \item{input}{List with arguments provided in input. Saved for convenience}
#' @return \item{info}{Short information about the model}
#' @return \item{coefficients}{Estimated coefficients}
#' @return \item{fitted.values}{Fitted values of the selected model}
#' @return \item{residuals}{Deviance residuals} 
#' @return \item{goodness.of.fit}{List containing goodness of fit measures like 
#' AIC, BIC and log-Likelihood} 
#' @return \item{opt.diagnosis}{Resulted optimization object useful for 
#' checking the convergence etc.} 
#' @return \item{stats}{List containing statistical measures like: 
#' parameter correlation, standard errors, degrees of freedom, deviance, 
#' gradient matrix, QR decomposition, covariance matrix etc.} 
#' @examples
#' # Example 1: ---
#' # Fit Makeham Model for Year of 1950.
#' 
#' x  <- 45:75
#' Dx <- ahmd$Dx[paste(x), "1950"]
#' Ex <- ahmd$Ex[paste(x), "1950"]
#' 
#' M1 <- MortalityLaw(x   = x, 
#'                    Dx  = Dx, 
#'                    Ex  = Ex, 
#'                    law = 'makeham',
#'                    scale.x = TRUE)
#' M1
#' ls(M1)
#' coef(M1)
#' summary(M1)
#' fitted(M1)
#' predict(M1, x = 45:95)
#' plot(M1)
#' 
#' 
#' # Example 2: ---
#' # We can fit the same model using a different data format 
#' # and a different optimization method.
#' x  <- 45:75
#' mx <- ahmd$mx[paste(x), ]
#' M2 <- MortalityLaw(x   = x, 
#'                    mx  = mx, 
#'                    law = 'makeham', 
#'                    opt.method = 'LF1', 
#'                    scale.x = TRUE)
#' M2
#' fitted(M2)
#' predict(M2, x = 55:90)
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
#' M3 <- MortalityLaw(x  = x,
#'                    Dx = Dx, 
#'                    Ex = Ex, 
#'                    custom.law = my_gompertz,
#'                    scale.x = TRUE) 
#' summary(M3)
#' # predict M3 for different ages
#' predict(M3, x = 85:130)
#' 
#' 
#' # Example 4: ---
#' # Fit Heligman-Pollard model for a single 
#' # year in the dataset between age 0 and 100.
#' 
#' x  <- 0:100
#' mx <- ahmd$mx[paste(x), "1950"] # select data
#' M4 <- MortalityLaw(x   = x, 
#'                    mx  = mx, 
#'                    law = 'HP', 
#'                    opt.method = 'LF2')
#' M4
#' plot(M4)
#' @export
MortalityLaw <- function(x, Dx = NULL, Ex = NULL, mx = NULL, qx = NULL, 
                         law = NULL, opt.method = 'poissonL', parS = NULL, 
                         fit.this.x = x, scale.x = FALSE, 
                         custom.law = NULL, show = TRUE){
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
    optim.model <- choose_optim(input) 
    if (show) setpb(pb, 2)
    
    fit    <- optim.model$hx
    dgn    <- optim.model$opt #diagnosis
    cf     <- exp(dgn$par)
    p      <- length(cf)
    pnames <- names(cf)
    resid  <- switch(C, C1_DxEx = Dx/Ex - fit,
                     C2_mx = mx - fit,
                     C3_qx = qx - fit)
    dev    <- sum(resid^2)
    rdf    <- length(x) - p
    df     <- c(p, rdf)
    resvar <- if (rdf <= 0) NaN else dev/rdf
    sigma  <- sqrt(resvar)
    fn     <- function(par) eval(call(law, x = x, par = par))$hx
    errfn  <- function(err) NULL 
    grad   <- tryCatch(rootSolve::gradient(fn, x = cf), error = errfn)
    QR     <- tryCatch(qr(grad, tol = 1e-10), error = errfn)
    XtXinv <- tryCatch(chol2inv(QR$qr), error = errfn)
    vcov = se = tval = corr = param <- NULL
    if (!is.null(XtXinv)) {
      vcov   <- dev * XtXinv
      se     <- sqrt(diag(XtXinv) * resvar)
      tval   <- cf/se
      corr   <- (XtXinv * resvar)/outer(se, se)
      param  <- cbind(cf, se, tval, 2 * pt(abs(tval), rdf, lower.tail = FALSE))
      dimnames(param) <- list(pnames, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
      dimnames(vcov) <- dimnames(XtXinv) <- dimnames(corr) <- list(pnames, pnames)
      rownames(grad) <- rownames(QR$qr) <- x
      names(se) <- pnames
    }
    gof    <- with(optim.model, c(logLik = logLik, AIC = AIC, BIC = BIC))
    stats  <- list(param = param, correlation = corr, se = se, df = df, 
                   deviance = dev, sigma = sigma, gradient = grad, QR = QR, 
                   vcov.unscaled = XtXinv, vcov = vcov)
    if (show) setpb(pb, 3)
    
    # Prepare, arrange, customize output
    if (law == "custom.law") {
      model.info <- "Custom Mortality Law"
    } else {
      availLaws  <- availableLaws()$table
      model.info <- data.frame(availLaws[availLaws$CODE == law, ], row.names = "")    
    }
    
    info <- list(model.info = model.info, process.date = date())
    names(fit) = names(resid) <- x
    if (show) setpb(pb, 4)
  } else {
    N  <- FMC$nLT
    if (show) {pb <- startpb(0, N + 1); on.exit(closepb(pb))} # Set progress bar
    cf = fit = gof = resid = dgn = stats <- NULL
    for (i in 1:N) {
      if (show) setpb(pb, i)
      M <- suppressMessages(MortalityLaw(x, Dx[, i], Ex[, i], mx[, i], qx[, i], 
                                         law, opt.method, parS, fit.this.x, scale.x, custom.law, show = FALSE))
      fit        <- cbind(fit, fitted(M))
      gof        <- rbind(gof, M$goodness.of.fit)
      dgn[[i]]   <- M$dgn
      cf         <- rbind(cf, coef(M))
      resid      <- cbind(resid, M$residuals)
      info       <- M$info
      stats[[i]] <- M$stats
    }
    rownames(cf)  = rownames(gof)   <- FMC$LTnames
    dimnames(fit) = dimnames(resid) <- list(x, FMC$LTnames)
    if (show) setpb(pb, N + 1)
  }
  
  output <- list(input = input, info = info, coefficients = cf,
                 fitted.values = fit, residuals = resid, stats = stats,
                 goodness.of.fit = gof, opt.diagnosis = dgn)
  output$info$call <- match.call()
  out <- structure(class = "MortalityLaw", output)
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
  
  if (is.null(Ex))    Ex = 1
  if (C == "C1_DxEx") nu = Dx/Ex 
  if (C == "C2_mx")   nu = Dx <- mx 
  if (C == "C3_qx")   nu = Dx <- qx 
  
  # compute likelihoods or loss functions
  loss <- switch(opt.method,
                 poissonL  = -(Dx * log(mu) - mu*Ex),
                 binomialL = -(Dx * log(1 - exp(-mu)) - (Ex - Dx)*mu),
                 LF1       =  (1 - mu/nu)^2,
                 LF2       =  log(mu/nu)^2,
                 LF3       =  ((nu - mu)^2)/nu,
                 LF4       =  (nu - mu)^2,
                 LF5       =  (nu - mu) * log(nu/mu),
                 LF6       =  abs(nu - mu))
  
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
choose_optim <- function(input){
  with(as.list(input), {
    # Subset the data
    if (scale.x) {
      x <- x - min(x) + 1 
      fit.this.x <- fit.this.x - min(fit.this.x) + 1
    }
    select.x <- x %in% fit.this.x
    qx       <- qx[select.x]
    mx       <- mx[select.x]
    Dx       <- Dx[select.x]
    Ex       <- Ex[select.x]
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
    
    cf     <- exp(opt$par)
    hx     <- do.call(law, list(x = x, par = cf))$hx
    logLik <- log(opt$fnvalue)
    AIC    <- 2*length(parS) - 2 * logLik
    BIC    <- log(length(fit.this.x)) * length(parS) - 2 * logLik
    
    if (!(opt.method %in% c('poissonL', 'binomialL'))) { 
      logLik = AIC  = BIC  <- NaN
    }
    out <- as.list(environment())
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
         # Errors ---
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
         # Warnings ---
         if (min(x) > 1 & (!scale.x)) {
           warning("\nScale down the 'x' argument so that is begins with a small value. ",
                   "Set 'scale.x = TRUE'. This is useful in order to obtain meaningful ", 
                   "estimates and in some cases a better fit (e.g. 'kannisto' law).", 
                   call. = F)
         }
         
         # Messages ---
         if (law %in% c('HP', 'HP2', 'HP3', 'HP4', 'kostaki') & opt.method != "LF2") {
           message(paste("\nFor cases like", law, "the optimization method 'LF2'",
                         "has been observed to return reliable estimates."))
         }
       })
}


#' Print MortalityLaw
#' @param x an object of class \code{"MortalityLaw"}
#' @param ... further arguments passed to or from other methods.
#' @keywords internal
#' @export
print.MortalityLaw <- function(x, ...) {
  L    <- x$input$law == "custom.law"
  info <- if (L) "Custom Mortality Law" else as.matrix(x$info$model.info[, c(2, 3)])
  cat(paste(info, collapse = " model: "))
  fv <- ifelse(!is.null(x$input$qx), 'qx', 'mx')
  cat("\nFitted values:", fv, "\n")
}


#' Summary MortalityLaw
#' @param x an object of class \code{"MortalityLaw"}
#' @param ... additional arguments affecting the summary produced.
#' @keywords internal
#' @export
summary.MortalityLaw <- function(object, ...) {
  x    <- object
  law  <- x$input$law
  L1   <- law == "custom.law"
  mi   <- if (L1) "Custom Mortality Law" else as.matrix(x$info$model.info[, c(2, 3)])
  call <- x$info$call
  res  <- summary(as.vector(as.matrix(x$residuals)))
  fv   <- ifelse(!is.null(x$input$qx), 'qx', 'mx')
  gof  <- x$goodness.of.fit
  rdf   <- x$stats$df[2]
  sigma <- x$stats$sigma
  
  nc <- nrow(coef(x))
  L2 <- is.null(nc)
  L3 <- x$input$opt.method %in% c("poissonL", "binomialL")
  if (L2) {
    param <- if (is.null(x$stats$vcov)) as.matrix(coef(x)) else x$stats$param
  } else {
    param <- coef(x)
    if (nc > 4) {
      param <- head_tail(param, hlength = 2, tlength = 2)
      gof   <- head_tail(gof, hlength = 2, tlength = 2)
    }
  }   
  out  <- list(info = mi, call = call, gof = gof, 
               fv = fv, dev.resid = res, param = param, sigma = sigma, rdf = rdf, 
               L1 = L1, L2 = L2, L3 = L3)
  out  <- structure(class = "summary.MortalityLaw", out)
  return(out)
}


#' Print summary.MortalityLaw
#' @param x an object of class \code{"summary.MortalityLaw"}
#' @param digits number of digits to display.
#' @param signif.stars logical. If TRUE show significance stars.
#' @param ... additional arguments affecting the summary produced.
#' @keywords internal
#' @export
print.summary.MortalityLaw <- function(x, digits = max(3L, getOption("digits") - 3L), 
                                       signif.stars = getOption("show.signif.stars"), ...) {
  with(x, {
    cat(paste(info, collapse = " model: "))
    cat("\nFitted values:", fv)
    cat("\n\nCall: ")
    print(call)
    cat("\nParameters:\n")
    printCoefmat(param, digits = digits, signif.stars = signif.stars, ...)
    if (L3) {
      cat("\nlogLik:", formatC(gof["logLik"], digits = digits))
      cat("\tAIC:", formatC(gof["AIC"], digits = digits))
      cat("\tBIC:", formatC(gof["BIC"], digits = digits), "\n")
    }
    cat('\nDeviance Residuals:\n')
    print(round(dev.resid, digits))
    if (L2) {
      cat("Residual standard error:", format(signif(x$sigma, digits)), 
          "on", rdf, "degrees of freedom")
    } 
  })
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

#' deviance function for MortalityLaw
#' @inheritParams print.MortalityLaw
#' @keywords internal
#' @export
deviance.MortalityLaw <- function(object, ...) {
  object$stats$deviance
}

#' df.residual function for MortalityLaw
#' @inheritParams print.MortalityLaw
#' @keywords internal
#' @export
df.residual.MortalityLaw <- function(object, ...) {
  object$stats$df[2]
}

#' Covariance function for MortalityLaw
#' @inheritParams print.MortalityLaw
#' @keywords internal
#' @export
vcov.MortalityLaw <- function(object, ...) {
  object$stats$vcov
}

#' Confidence Intervals for MortalityLaw parameter estimates
#' @inheritParams print.MortalityLaw
#' @inheritParams stats::confint
#' @keywords internal
#' @export
confint.MortalityLaw <- function(object, parm, level = 0.95, ...) {
  cf   <- coef(object)
  a    <- (1 - level)/2
  a    <- c(a, 1 - a)
  pct  <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3), "%")
  fac  <- qt(a, df.residual(object))
  ses  <- sqrt(diag(vcov(object)))
  ci   <- cf + ses %o% fac
  ci[ci < 0] <- 1e-9 # all parameters must be positive
  colnames(ci) <- pct
  
  pnames <- names(cf)
  if (missing(parm)) {
    parm <- pnames 
  } else {
    if (is.numeric(parm)) parm <- pnames[parm]
  }
  return(ci[parm, ])
}


#' Predict function for MortalityLaw
#' @param object An object of class \code{"MortalityLaw"}
#' @param x Vector of ages to be considered in prediction
#' @param ... Additional arguments affecting the predictions produced.
#' @seealso \code{\link{MortalityLaw}}
#' @examples 
#' # See complete example in MortalityLaw help page
#' @export
predict.MortalityLaw <- function(object, x, ...){
  sx <- object$input$scale.x
  xi <- object$input$x
  
  if (sx & min(x) < min(xi)) {
    stop(paste("When the mortality model is estimated using 'scale.x = TRUE'", 
               "the predicted 'x' must be higher that 'x' used in fitting.",
               "Provide values equal or greater than", min(xi)), call. = F)
  }
  if (min(x) < 0) stop("'x' must be greater or equal to zero.", call. = F)
  
  x_  <- if (sx) x - min(xi) + 1 else x
  law <- object$input$law
  Par <- coef(object)
  pn  <- names(Par)
  Par <- if (is.matrix(Par)) Par else matrix(Par, nrow = 1)
  
  if (law == "custom.law") {
    colnames(Par) <- pn
    M  <- object$input$custom.law
    hx <- apply(X = Par, 1, FUN = function(X) M(x = x_, par = X)$hx)
  } else {
    hx <- apply(X = Par, 1, FUN = function(X) eval(call(law, x = x_, par = X))$hx)
  }
  rownames(hx) <- x
  if (ncol(hx) == 1) {
    hx <- as.numeric(hx)
    names(hx) <- x
  }
  return(hx)
}


