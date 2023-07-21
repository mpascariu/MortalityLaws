# -------------------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last Update: Thu Jul 20 21:29:59 2023
# -------------------------------------------------------------- #

#' Fit Mortality Laws
#'
#' Fit parametric mortality models given a set of input data which can be
#' represented by death counts and mid-interval population estimates
#' \code{(Dx, Ex)} or age-specific death rates \code{(mx)} or death
#' probabilities \code{(qx)}. Using the argument \code{law} one can specify
#' the model to be fitted. So far more than 27 parametric models have been
#' implemented; check the \code{\link{availableLaws}} function to learn
#' about the available options. The models can be fitted under
#' the maximum likelihood methodology or by selecting a loss function to be
#' optimised. See the implemented loss function by running the
#' \code{\link{availableLF}} function.
#' @usage
#' MortalityLaw(x, Dx = NULL, Ex = NULL, mx = NULL, qx = NULL,
#'                 law = NULL,
#'                 opt.method = "LF2",
#'                 parS = NULL,
#'                 fit.this.x = x,
#'                 custom.law = NULL,
#'                 show = FALSE, ...)
#' @details Depending on the complexity of the model, one of following
#' optimization strategies is employed: \enumerate{
#'  \item{Nelder-Mead method:}{ approximates a local optimum of a problem with n
#'   variables when the objective function varies smoothly and is unimodal.
#'   For details see \code{\link{optim}}}
#'  \item{PORT routines:}{ provides unconstrained optimization and optimization
#'  subject to box constraints for complicated functions. For details check
#'  \code{\link{nlminb}}}
#'  \item{Levenberg-Marquardt algorithm:}{ damped least-squares method.
#'  For details check \code{\link{nls.lm}}}
#' }
#' @inheritParams LifeTable
#' @param law The name of the mortality law/model to be used. e.g.
#' \code{gompertz}, \code{makeham}, ... To investigate all the possible options,
#' see \code{\link{availableLaws}} function.
#' @param opt.method How would you like to find the parameters? Specify the
#' function to be optimize. Available options: the Poisson likelihood function
#' \code{poissonL}; the Binomial likelihood function -\code{binomialL}; and
#' 6 other loss functions. For more details, check the \code{\link{availableLF}}
#' function.
#' @param parS Starting parameters used in the optimization process (optional).
#' @param fit.this.x Select the ages to be considered in model fitting.
#' By default \code{fit.this.x = x}. One may want to exclude from the fitting
#' procedure, say, the advanced ages where the data is sparse.
#' @param custom.law Allows you to fit a model that is not defined
#' in the package. Accepts as input a function.
#' @param show Choose whether to display a progress bar during the fitting
#' process. Logical. Default: \code{FALSE}.
#' @param ... Arguments to be passed to or from other methods.
#' @return The output is of the \code{"MortalityLaw"} class with the components:
#'  \item{input}{List with arguments provided in input. Saved for convenience.}
#'  \item{info}{Brief information about the model.}
#'  \item{coefficients}{Estimated coefficients.}
#'  \item{fitted.values}{Fitted values of the selected model.}
#'  \item{residuals}{Deviance residuals.}
#'  \item{goodness.of.fit}{List containing goodness of fit measures like
#' AIC, BIC and log-Likelihood.}
#'  \item{opt.diagnosis}{Resultant optimization object useful for
#' checking the convergence etc.}
#' @seealso
#' \code{\link{availableLaws}}
#' \code{\link{availableLF}}
#' \code{\link{LifeTable}}
#' \code{\link{ReadHMD}}
#' @author Marius D. Pascariu
#' @examples
#' # Example 1: --------------------------
#' # Fit Makeham Model for Year of 1950.
#'
#' x  <- 45:75
#' Dx <- ahmd$Dx[paste(x), "1950"]
#' Ex <- ahmd$Ex[paste(x), "1950"]
#'
#' M1 <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, law = 'makeham')
#'
#' M1
#' ls(M1)
#' coef(M1)
#' summary(M1)
#' fitted(M1)
#' predict(M1, x = 45:95)
#' plot(M1)
#'
#'
#' # Example 2: --------------------------
#' # We can fit the same model using a different data format
#' # and a different optimization method.
#' x  <- 45:75
#' mx <- ahmd$mx[paste(x), ]
#' M2 <- MortalityLaw(x = x, mx = mx, law = 'makeham', opt.method = 'LF1')
#' M2
#' fitted(M2)
#' predict(M2, x = 55:90)
#'
#' # Example 3: --------------------------
#' # Now let's fit a mortality law that is not defined
#' # in the package, say a reparameterized Gompertz in
#' # terms of modal age at death
#' # hx = b*exp(b*(x-m)) (here b and m are the parameters to be estimated)
#'
#' # A function with 'x' and 'par' as input has to be defined, which returns
#' # at least an object called 'hx' (hazard rate).
#' my_gompertz <- function(x, par = c(b = 0.13, M = 45)){
#'   hx  <- with(as.list(par), b*exp(b*(x - M)) )
#'   return(as.list(environment()))
#' }
#'
#' M3 <- MortalityLaw(x = x, Dx = Dx, Ex = Ex, custom.law = my_gompertz)
#' summary(M3)
#' # predict M3 for different ages
#' predict(M3, x = 85:130)
#'
#'
#' # Example 4: --------------------------
#' # Fit Heligman-Pollard model for a single
#' # year in the dataset between age 0 and 100 and build a life table.
#'
#' x  <- 0:100
#' mx <- ahmd$mx[paste(x), "1950"] # select data
#' M4 <- MortalityLaw(x = x, mx = mx, law = 'HP', opt.method = 'LF2')
#' M4
#' plot(M4)
#'
#' LifeTable(x = x, qx = fitted(M4))
#' @export
MortalityLaw <- function(x,
                         Dx = NULL,
                         Ex = NULL,
                         mx = NULL,
                         qx = NULL,
                         law = NULL,
                         opt.method = 'LF2',
                         parS = NULL,
                         fit.this.x = x,
                         custom.law = NULL,
                         show = FALSE,
                         ...){

  info    <- addDetails(law, custom.law, parS)
  law     <- info$law
  scale.x <- info$scale.x
  parS    <- info$parS
  input   <- c(as.list(environment()))
  K       <- find.my.case(Dx, Ex, mx, qx)

  # TR: if inputs are matrix, then we have class matrix, array, and this
  # throws a warning. If we have a dim attribute then this won't work. Even
  # if it's a 1-column matrix (class =array) or a single-dim length-attribute vector
  # (also array!). Both of those cases are probably things we want to treat as
  # vectors!
  if (any(K$iclass == "numeric")) {

    check.MortalityLaw(input) # Check input

    # Set-up progress bar
    if (show) {pb <- startpb(0, 4); on.exit(closepb(pb)); setpb(pb, 1)}

    # Find optimal coefficients
    optim.model <- choose_optim(input)
    if (show) setpb(pb, 2)

    fit   <- optim.model$hx
    dgn   <- optim.model$opt #diagnosis
    cf    <- optim.model$C
    p     <- length(cf)
    resid <- switch(
      K$case,
      C1_DxEx = Dx/Ex - fit,
      C2_mx = mx - fit,
      C3_qx = qx - fit
      )
    dev  <- sum(resid^2)
    rdf  <- length(x) - p
    df   <- c(n.param = p, df.residual = rdf)
    gof  <- with(
      optim.model,
      c(logLik = logLik, AIC = AIC, BIC = BIC)
      )
    info <- list(model.info = info$model, process.date = date())
    names(fit) = names(resid) <- x

    if (show) setpb(pb, 4)

  } else {# if input is a matrix then iterate here

    N  <- K$nLT
    # Set-up progress bar
    if (show) {pb <- startpb(0, N + 1); on.exit(closepb(pb))}
    cf = fit = gof = resid = dgn = df = dev <- NULL

    for (i in 1:N) {
      if (show) setpb(pb, i)
      M <- suppressMessages(
        MortalityLaw(
          x = x,
          Dx = Dx[, i],
          Ex = Ex[, i],
          mx = mx[, i],
          qx = qx[, i],
          law = law,
          opt.method = opt.method,
          parS = parS,
          fit.this.x = fit.this.x,
          custom.law = custom.law,
          show = FALSE
          )
        )
      fit      <- cbind(fit, fitted(M))
      gof      <- rbind(gof, M$goodness.of.fit)
      dgn[[i]] <- M$dgn
      cf       <- rbind(cf, coef(M))
      resid    <- cbind(resid, resid(M))
      df       <- rbind(df, M$df)
      dev      <- c(dev, M$dev)
    }

    info <- M$info
    rownames(cf)  = rownames(gof) = rownames(df) = names(dev) <- K$LTnames
    dimnames(fit) = dimnames(resid) <- list(x, K$LTnames)
    if (show) setpb(pb, N + 1)
  }

  # Exit
  output <- list(
    input = input,
    info = info,
    coefficients = cf,
    fitted.values = fit,
    residuals = resid,
    goodness.of.fit = gof,
    opt.diagnosis = dgn,
    df = df,
    deviance = dev
    )
  output$info$call <- match.call()
  out <- structure(class = "MortalityLaw", output)
  return(out)
}


#' Depending on the chosen mortality law, additional details need to be
#' specified in order to be able to fit the models taking into account it's
#' particularities.
#' @inheritParams MortalityLaw
#' @return A list of model specifications 
#' @keywords internal
addDetails <- function(law,
                       custom.law = NULL,
                       parS = NULL) {

  if (is.null(law) & is.null(custom.law)) {
    stop("Which mortality law do you intend to fit?", call. = FALSE)
  }

  if (!is.null(custom.law)) {
    law  <- "custom.law"
    parS <- custom.law(1)$par
    MI   <- "Custom Mortality Law"
    sx   <- TRUE

  } else {
    law  <- law
    parS <- parS
    A    <- availableLaws(law)[["table"]]
    MI   <- data.frame(A[A$CODE == law, ], row.names = "")
    sx   <- as.logical(MI$SCALE_X)
  }

  out <- list(
    law = law,
    parS = parS,
    model = MI,
    scale.x = sx
    )
  return(out)
}



#' Function to be Optimize
#' @inheritParams MortalityLaw
#' @return The optimal value
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
  loss <- switch(
    EXPR = opt.method,
    poissonL  = -(Dx * log(mu) - mu*Ex),
    binomialL = -(Dx * log(1 - exp(-mu)) - (Ex - Dx)*mu),
    LF1       =  (1 - mu/nu)^2,
    LF2       =  log(mu/nu)^2,
    LF3       =  ((nu - mu)^2)/nu,
    LF4       =  (nu - mu)^2,
    LF5       =  (nu - mu) * log(nu/mu),
    LF6       =  abs(nu - mu)
    )

  # Here I want to make sure that the optimisation algorithm is not returning
  # NaN values when it converges (because that is possible).
  loss[is.infinite(loss)] <- 10^5
  if (sum(is.na(mu)) != 0) loss = loss + 10^5
  out <- sum(loss, na.rm = TRUE)
  # because nls.lm function requires a vector we have to do the following:
  if (any(law %in% c('thiele', 'wittstein'))) out = loss

  return(out)
}


#' Scaling method for x vector
#' @inheritParams MortalityLaw
#' @return scalar 
#' @keywords internal
scale_x <- function(x) {
  x - min(x) + 1
}


#' Select an optimizing method
#' @param input list of all inputs collected from MortalityLaw function
#' @return A list of model specification corresponding to the best fitted model
#' @keywords internal
choose_optim <- function(input){
  with(as.list(input), {
    # Subset the data
    select.x <- x %in% fit.this.x

    if (scale.x) {
      new.fit.this.x = scale_x(fit.this.x)
      d = fit.this.x[1] - new.fit.this.x[1]
      new.x = x - d

    } else {
      new.fit.this.x <- fit.this.x
      new.x <- x
    }

    if (is.null(parS)) parS <- bring_parameters(law, parS)
    # Optimize
    foo <- function(k) {
      objective_fun(
        par = k,
        x = new.fit.this.x,
        Dx = Dx[select.x],
        Ex = Ex[select.x],
        mx = mx[select.x],
        qx = qx[select.x],
        law,
        opt.method,
        custom.law)
    }

    if (any(law %in% c('HP', 'HP2', 'HP3', 'HP4', 'kostaki'))) {
      opt <- nlminb(
        start = log(parS),
        objective = foo,
        control = list(eval.max = 5000, iter.max = 5000)
        )
      opt$fnvalue <- opt$objective

    } else if (any(law %in% c('thiele', 'wittstein'))) {
      opt <- nls.lm(
        par = log(parS),
        fn = foo,
        control = nls.lm.control(maxfev = 10000, maxiter = 1024)
        )
      opt$fnvalue <- sum(opt$fvec)

    } else {
      opt <- optim(
        par = log(parS),
        fn = foo,
        method = 'Nelder-Mead'
        )
      opt$fnvalue <- opt$value
    }

    C <- exp(opt$par)
    if (law == 'kostaki') { #kostaki hack
      if (C[5] >= 50*C[6]) C[6] <- C[5]/50
    }

    hx     <- do.call(law, list(x = new.x, par = C))$hx
    logLik <- log(opt$fnvalue)
    AIC    <- 2 * length(parS) - 2 * logLik
    BIC    <- log(length(fit.this.x)) * length(parS) - 2 * logLik

    if (!any(opt.method %in% c('poissonL', 'binomialL'))) {
      logLik = AIC  = BIC  <- NaN
    }
    out <- as.list(environment())
    return(out)
  })
}


