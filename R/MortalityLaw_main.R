# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-04 23:33:16
# --------------------------------------------

#' Fit Mortality Laws
#'
#' Fit parametric mortality models given a set of input data. The data can be
#' supplied as death counts and mid-interval population estimates
#' \code{(Dx, Ex)}, age-specific death rates \code{(mx)}, or death
#' probabilities \code{(qx)}. Use the \code{law} argument to specify the
#' model to be fitted. Over 30 parametric models are currently implemented;
#' run \code{\link{availableLaws}} to see the full list. Models can be fitted
#' using maximum likelihood or by optimising a loss function. See the
#' \code{\link{availableLF}} function for the implemented options.
#'
#' @usage
#' MortalityLaw(x, Dx = NULL, Ex = NULL, mx = NULL, qx = NULL,
#'                 law = NULL,
#'                 opt.method = "LF2",
#'                 parS = NULL,
#'                 fit.this.x = x,
#'                 custom.law = NULL,
#'                 show = FALSE, ...)
#'
#' @details
#' \strong{Optimisation:} The PORT routines (via \code{\link{nlminb}}) are used
#' for unconstrained and box-constrained optimisation. Parameters are estimated
#' on the log scale to ensure positivity, and the routine is set to allow up to
#' 5000 iterations. When the optimisation method is \code{"poissonL"} or
#' \code{"binomialL"}, the AIC, BIC and log-likelihood are computed from the
#' likelihood. Otherwise these are set to \code{NaN}.
#'
#' \strong{Scaling of the age vector:} For models that cover only a portion
#' of the lifespan (e.g., adult or old-age mortality), the age vector \code{x}
#' is automatically re-scaled as \code{x = x - min(x) + 1} before fitting.
#' This transformation improves numerical stability and helps the optimisation
#' algorithm converge, especially when the starting age is far from zero.
#' Models that apply this scaling are flagged with \code{SCALE_X = TRUE} in
#' the table returned by \code{\link{availableLaws}}. When using
#' \code{\link{predict.MortalityLaw}} or \code{\link{LawTable}} with such
#' models, the same scaling is applied internally, so predictions remain
#' consistent with the fitted coefficients.
#'
#' \strong{Handling matrix input:} If \code{Dx}, \code{Ex}, \code{mx} or
#' \code{qx} are provided as matrices (with one column per population or
#' time period), the function iterates over the columns and fits a separate
#' model to each, returning a collection of results.
#' @inheritParams LifeTable
#' @param law The name of the mortality law to be used (e.g., \code{"gompertz"},
#' \code{"makeham"}). Run \code{\link{availableLaws}} to see all options.
#' @param opt.method The function to optimise. Available options:
#' \itemize{
#'   \item{\code{"poissonL"}: Poisson log-likelihood.}
#'   \item{\code{"binomialL"}: Binomial log-likelihood.}
#'   \item{\code{"LF1"}: Squared relative error \code{(1 - mu/nu)^2}.}
#'   \item{\code{"LF2"}: Squared log-ratio \code{log(mu/nu)^2}.}
#'   \item{\code{"LF3"}: Chi-squared-type \code{((nu - mu)^2)/nu}.}
#'   \item{\code{"LF4"}: Squared error \code{(nu - mu)^2}.}
#'   \item{\code{"LF5"}: Deviance-type \code{(nu - mu) * log(nu/mu)}.}
#'   \item{\code{"LF6"}: Absolute error \code{abs(nu - mu)}.}
#' }
#' See \code{\link{availableLF}} for details.
#' @param parS Optional starting parameter values for the optimisation. If
#' \code{NULL}, sensible defaults are automatically chosen via
#' \code{\link{bring_parameters}}.
#' @param fit.this.x A subset of \code{x} over which to fit the model. The
#' default is the entire \code{x} vector. Use this to exclude, for example,
#' advanced ages where data are sparse.
#' @param custom.law A user-defined function for fitting a model not included
#' in the package. The function must accept arguments \code{x} (age vector)
#' and \code{par} (named parameter vector) and return a list containing at
#' least an element named \code{hx} (the hazard or force of mortality). See
#' the examples below.
#' @param show Logical. If \code{TRUE}, a progress bar is displayed during
#' fitting. Default: \code{FALSE}.
#' @param ... Additional arguments passed to or from other methods.
#' @return
#' An object of class \code{"MortalityLaw"}, which is a list with the following
#' components:
#' \item{input}{List of input arguments, stored for reproducibility.}
#' \item{info}{Model information (name, formula, date of fitting).}
#' \item{coefficients}{Estimated parameters of the mortality law. A named
#' vector for a single fit, or a matrix for multiple fits.}
#' \item{fitted.values}{Fitted hazard rates (or death probabilities) evaluated
#' at the input ages \code{x}.}
#' \item{residuals}{Deviance residuals, computed as observed minus fitted
#' values.}
#' \item{goodness.of.fit}{List or matrix of goodness-of-fit measures: AIC,
#' BIC and log-likelihood (available only for likelihood-based methods).}
#' \item{opt.diagnosis}{Object returned by the optimisation routine, useful
#' for checking convergence.}
#' \item{df}{Number of parameters and residual degrees of freedom.}
#' \item{deviance}{Sum of squared log-residuals, used as a deviance measure.}
#' @seealso
#' \code{\link{availableLaws}} for a list of all implemented models;
#' \code{\link{availableLF}} for loss function details;
#' \code{\link{LifeTable}} for life table construction;
#' \code{\link{ReadHMD}} for downloading data from the Human Mortality Database.
#' @author Marius D. Pascariu
#' @examples
#' # Example 1: Fitting the Makeham model --------------------------
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

    resid <- switch(K$case,
      C1_DxEx = Dx/Ex - fit,
      C2_mx = mx - fit,
      C3_qx = qx - fit
      )
    # Deviance is computed as the sum of squared log-residuals
    dev <- switch(K$case,
      C1_DxEx = log(Dx/Ex) - log(fit),
      C2_mx   = log(mx) - log(fit),
      C3_qx   = log(qx) - log(fit)
      )
    dev  <- sum(dev^2)

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

#' Retrieve model-specific details for fitting
#'
#' Based on the chosen mortality law (or a custom law), this function retrieves
#' the default starting parameters, the model information table, and whether
#' the age vector should be scaled before fitting.
#' @inheritParams MortalityLaw
#' @return A list with components:
#' \item{law}{Internal law name (e.g., \code{"custom.law"} for user-supplied functions).}
#' \item{parS}{Starting parameter values for the optimisation.}
#' \item{model}{Model information data frame (from \code{\link{availableLaws}})
#' or \code{"Custom Mortality Law"} for user-defined models.}
#' \item{scale.x}{Logical; whether the age vector should be re-scaled
#' (\code{x = x - min(x) + 1}) before fitting.}
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

#' Objective function to minimise during optimisation
#'
#' Given a set of parameters (on the log scale), this function evaluates the
#' chosen loss function or negative log-likelihood by comparing observed
#' mortality values (Dx/Ex, mx, or qx) against the hazard rates predicted by
#' the specified mortality law.
#'
#' Parameters are transformed back to the original scale via \code{exp(par)}
#' when calling the mortality law function. Infinite hazard values are capped
#' to 1, and large penalties are applied for missing or out-of-range values
#' to guide the optimiser away from invalid regions.
#'
#' @inheritParams MortalityLaw
#' @param par Parameter vector on the log scale.
#' @return A scalar loss value to be minimised.
#' @keywords internal
objective_fun <- function(par, x, Dx, Ex, mx, qx,
                          law, opt.method, custom.law) {

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
  return(out)
}

#' Scale the age vector for stable optimisation
#'
#' For mortality laws that cover only a portion of the lifespan (e.g., adult
#' or old-age mortality), the age vector is rescaled so that the minimum age
#' becomes 1. This improves numerical stability by keeping the exponentiated
#' terms in the hazard function within a reasonable range.
#'
#' @param x A numeric vector of ages.
#' @return A numeric vector of scaled ages, where min(x) == 1.
#' @keywords internal
scale_x <- function(x) {
  x - min(x) + 1
}

#' Run the optimisation routine
#'
#' This is the core optimisation function for \code{\link{MortalityLaw}}. It:
#' \enumerate{
#'   \item Subsets the data to the fitting ages (\code{fit.this.x}).
#'   \item Scales the age vector if required by the chosen model.
#'   \item Obtains default starting parameters (if not provided).
#'   \item Minimises the objective function using \code{\link{nlminb}} (PORT
#'         routines) with the parameters on the log scale.
#'   \item Transforms parameters back to the original scale, computes the
#'         fitted hazard, and derives goodness-of-fit measures (AIC, BIC,
#'         log-likelihood) where applicable.
#' }
#'
#' @param input A list containing all input arguments to \code{\link{MortalityLaw}}.
#' @return A list with components:
#' \item{x}{Age vector (original).}
#' \item{new.x}{Age vector after optional scaling.}
#' \item{opt}{Object returned by \code{\link{nlminb}}.}
#' \item{C}{Estimated parameters on the original scale.}
#' \item{hx}{Fitted hazard values evaluated at \code{new.x}.}
#' \item{logLik, AIC, BIC}{Goodness-of-fit measures (\code{NaN} for
#' non-likelihood methods).}
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
    # Starting parameters
    if (is.null(parS)) parS <- bring_parameters(law, parS)
    
    # Objective function setup
    foo <- function(pars) {
      objective_fun(
        par = pars,
        x = new.fit.this.x,
        Dx = Dx[select.x],
        Ex = Ex[select.x],
        mx = mx[select.x],
        qx = qx[select.x],
        law,
        opt.method,
        custom.law)
    }

    # Optimization algorithm
      if (law == 'invweibull'){
        opt <- optim(par = log(parS), fn = foo, method = 'Nelder-Mead')
        opt$fnvalue <- opt$value

      } else {
        opt <- nlminb(start = log(parS), objective = foo, control = list(eval.max = 5000, iter.max = 5000))
        opt$fnvalue <- opt$objective
      }

    # Return the optimal parameters
    C <- exp(opt$par)

    if (law == 'kostaki') { #kostaki hack
      if (C[5] >= 50*C[6]) C[6] <- C[5]/50
    }

    # Hazard function of the fitted model
    hx     <- do.call(law, list(x = new.x, par = C))$hx

    # Compute goodness of fit measures  
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
