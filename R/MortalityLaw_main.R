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
#' \code{bring_parameters}.
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
#' \item{residuals}{Raw residuals, observed minus fitted values.}
#' \item{goodness.of.fit}{Named numeric vector (single fit) or matrix (one row
#' per fit) with log-likelihood, AIC and BIC (NaN for non-likelihood methods).}
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

  info    <- law_details(law = law, custom.law = custom.law, parS = parS)
  law     <- info$law
  scale.x <- info$scale.x
  parS    <- info$parS
  input   <- c(as.list(environment()))
  K       <- detect_case(Dx = Dx, Ex = Ex, mx = mx, qx = qx)

  # TR: if inputs are matrix, then we have class matrix, array, and this
  # throws a warning. If we have a dim attribute then this won't work. Even
  # if it's a 1-column matrix (class =array) or a single-dim length-attribute vector
  # (also array!). Both of those cases are probably things we want to treat as
  # vectors!
  if (any(K$iclass == "numeric")) {

    output <- fit_single(input = input, K = K)

  } else {

    # if input is a matrix then iterate here
    output <- fit_multiple(input = input, K = K)

  }

  # Exit
  output$info$call <- match.call()
  out <- structure(class = "MortalityLaw", output)
  return(out)
}

#' Assemble the components of a single-curve fit
#'
#' Runs the input checks and the optimisation and returns the components of a
#' \code{"MortalityLaw"} object for one mortality curve.
#' @param input A list of input arguments to \code{\link{MortalityLaw}}.
#' @param K Problem case details as returned by \code{detect_case}.
#' @return A list with the components of a \code{"MortalityLaw"} object.
#' @noRd
fit_single <- function(input, K) {

  with(as.list(input), {
    check_mortality_law_input(input = input)

    # Set-up progress bar
    if (show) {
      pb <- startpb(min = 0, max = 4)
      on.exit(closepb(pb = pb))
      setpb(pb = pb, value = 1)
    }

    # Find optimal coefficients
    optim.model <- choose_optim(input = input)

    if (show) setpb(pb = pb, value = 2)

    fit  <- optim.model$hx
    dgn  <- optim.model$opt # diagnosis
    cf   <- optim.model$C
    stat <- fit_statistics(fit = fit, optim.model = optim.model, input = input,
                           K = K)

    info         <- list(model.info = info$model, process.date = date())
    names(fit)   <- x
    names(stat$residuals) <- x

    if (show) setpb(pb = pb, value = 4)

    out <- list(
      input = input,
      info = info,
      coefficients = cf,
      fitted.values = fit,
      residuals = stat$residuals,
      goodness.of.fit = stat$goodness.of.fit,
      opt.diagnosis = dgn,
      df = stat$df,
      deviance = stat$deviance
      )
    return(out)
  })
}

#' Compute the residuals, deviance and goodness-of-fit of a single fit
#'
#' Compares the fitted hazard with the observed data of the problem case and
#' derives the goodness-of-fit measures of a \code{"MortalityLaw"} object.
#' @param fit Fitted hazard values, one per fitted age.
#' @param optim.model Result of \code{choose_optim}.
#' @param input A list of input arguments to \code{\link{MortalityLaw}}.
#' @param K Problem case details as returned by \code{detect_case}.
#' @return A list with the residuals, deviance, degrees of freedom and
#' goodness-of-fit measures.
#' @noRd
fit_statistics <- function(fit, optim.model, input, K) {

  with(as.list(input), {
    p <- length(optim.model$C)
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
    dev <- sum(dev^2)

    # Residual degrees of freedom: fitted observations minus parameters
    rdf <- sum(x %in% fit.this.x) - p
    df  <- c(n.param = p, df.residual = rdf)
    gof <- with(optim.model, c(logLik = logLik, AIC = AIC, BIC = BIC))
    out <- list(
      residuals = resid,
      deviance = dev,
      df = df,
      goodness.of.fit = gof
      )
    return(out)
  })
}

#' Assemble the components of a multi-curve fit
#'
#' Fits one mortality curve per column of a matrix input by calling
#' \code{\link{MortalityLaw}} on each column, then binds the per-column results
#' into the multi-fit components of a \code{"MortalityLaw"} object.
#' @param input A list of input arguments to \code{\link{MortalityLaw}}.
#' @param K Problem case details as returned by \code{detect_case}.
#' @return A list with the components of a \code{"MortalityLaw"} object.
#' @noRd
fit_multiple <- function(input, K) {

  with(as.list(input), {
    N <- K$nLT

    # Set-up progress bar
    if (show) {
      pb <- startpb(min = 0, max = N + 1)
      on.exit(closepb(pb = pb))
    }

    fits <- vector(mode = "list", length = N)

    for (i in seq_len(N)) {

      if (show) setpb(pb = pb, value = i)

      fits[[i]] <- suppressMessages(
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
    }

    if (show) setpb(pb = pb, value = N + 1)

    parts <- bind_fits(fits = fits, x = x, K = K)
    info  <- fits[[N]]$info

    out <- list(
      input = input,
      info = info,
      coefficients = parts$coefficients,
      fitted.values = parts$fitted.values,
      residuals = parts$residuals,
      goodness.of.fit = parts$goodness.of.fit,
      opt.diagnosis = parts$opt.diagnosis,
      df = parts$df,
      deviance = parts$deviance
      )
    return(out)
  })
}

#' Bind the per-column fits into the multi-fit components
#'
#' Collects the coefficients, fitted values, residuals, goodness-of-fit
#' measures, optimisation diagnoses, degrees of freedom and deviances of the
#' per-column fits and binds them once, instead of growing the objects inside
#' the fitting loop.
#' @param fits List of per-column \code{"MortalityLaw"} objects.
#' @param x Vector of ages at which the law was fitted.
#' @param K Problem case details as returned by \code{detect_case}.
#' @return A list with the multi-fit components of a \code{"MortalityLaw"}
#' object.
#' @noRd
bind_fits <- function(fits, x, K) {

  cf    <- do.call(rbind, lapply(fits, coef))
  fit   <- do.call(cbind, lapply(fits, fitted))
  resid <- do.call(cbind, lapply(fits, residuals))
  gof   <- do.call(rbind, lapply(fits, function(M) M$goodness.of.fit))
  df    <- do.call(rbind, lapply(fits, function(M) M$df))
  dgn   <- lapply(fits, function(M) M$opt.diagnosis)
  dev   <- unlist(lapply(fits, function(M) M$deviance))

  rownames(cf)  <- K$LTnames
  rownames(gof) <- K$LTnames
  rownames(df)  <- K$LTnames
  names(dev)    <- K$LTnames
  dimnames(fit)   <- list(x, K$LTnames)
  dimnames(resid) <- list(x, K$LTnames)

  out <- list(
    coefficients = cf,
    fitted.values = fit,
    residuals = resid,
    goodness.of.fit = gof,
    opt.diagnosis = dgn,
    df = df,
    deviance = dev
    )
  return(out)
}

#' Retrieve model-specific details for fitting
#'
#' Returns the internal law name, the starting parameters, the model
#' information table and the \code{SCALE_X} flag for the chosen law.
#' @param law The requested law, or \code{NULL} when a custom law is supplied.
#' @param custom.law Optional user-defined law function.
#' @param parS Optional starting parameter values.
#' @return A list with the law name, starting parameters, model information and
#' the scaling flag.
#' @noRd
law_details <- function(law,
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

#' Resolve the function that computes the hazard of a mortality law
#'
#' @param law Name of the mortality law, or \code{"custom.law"}.
#' @param custom.law Optional user-defined law function.
#' @return A function with arguments \code{x} and \code{par}.
#' @noRd
law_function <- function(law, custom.law = NULL) {

  if (identical(law, "custom.law")) {
    out <- custom.law

  } else {
    out <- get(law, mode = "function")

  }
  return(out)
}

#' Objective function to minimise during optimisation
#'
#' Given a set of parameters (on the log scale), this function evaluates the
#' chosen loss function or negative log-likelihood by comparing observed
#' mortality values (Dx/Ex, mx, or qx) against the hazard rates predicted by
#' the specified mortality law. The problem case and the law function are
#' resolved once per call; \code{\link{objective_loss}} does the work in the
#' optimiser hot loop.
#' @param par Parameter vector on the log scale.
#' @param x Vector of ages at which the law is evaluated.
#' @param Dx,Ex,mx,qx Observed data; the cases that do not apply are \code{NULL}.
#' @param law The mortality law to be fitted, see \code{\link{availableLaws}}.
#' @param opt.method The loss function, see \code{\link{availableLF}}.
#' @param custom.law Optional user-defined law function.
#' @return A scalar loss value to be minimised.
#' @noRd
objective_fun <- function(par, x, Dx, Ex, mx, qx,
                          law, opt.method, custom.law) {

  case <- detect_case(Dx = Dx, Ex = Ex, mx = mx, qx = qx)$case
  fn   <- law_function(law = law, custom.law = custom.law)
  out  <- objective_loss(par = par, x = x, Dx = Dx, Ex = Ex, mx = mx, qx = qx,
                         case = case, fn = fn, opt.method = opt.method)
  return(out)
}

#' Evaluate the loss function or the negative log-likelihood
#'
#' Core of \code{objective_fun} with the problem case and the law
#' function already resolved. Parameters are transformed back to the original
#' scale via \code{exp(par)}; probes with non-finite parameters or with
#' parameters that underflow to zero return a flat penalty without evaluating
#' the law. Hazard values that are non-positive or non-finite are set to
#' \code{NA}, so \code{log()} never warns while the optimiser probes invalid
#' regions, and every non-finite loss term is replaced by a penalty of
#' \code{1e5}.
#' @param par Parameter vector on the log scale.
#' @param x Vector of ages at which the law is evaluated.
#' @param Dx,Ex,mx,qx Observed data; the cases that do not apply are \code{NULL}.
#' @param case Problem case, one of \code{C1_DxEx}, \code{C2_mx}, \code{C3_qx}.
#' @param fn Function computing \code{hx} when called as \code{fn(x = , par = )}.
#' @param opt.method The loss function, see \code{\link{availableLF}}.
#' @return A scalar loss value to be minimised.
#' @noRd
objective_loss <- function(par, x, Dx, Ex, mx, qx, case, fn, opt.method) {

  # The optimiser probes the log parameter scale: exp() underflows to 0 below
  # about -745 and non-finite parameters would be rejected by the law
  # validation, so such probes get a flat penalty without evaluating the law
  ok.par <- all(is.finite(par)) && all(exp(par) > 0)

  if (ok.par) {
    mu <- fn(x = x, par = exp(par))$hx

  } else {
    mu <- rep(NA_real_, length(x))

  }
  # +Inf is capped to 1; the remaining non-finite and non-positive values are
  # set to NA so that log() stays silent and the loss becomes a penalty
  mu[is.infinite(mu)] <- 1
  mu[!is.finite(mu) | mu <= 0] <- NA

  if (is.null(Ex)) {
    Ex <- 1
  }

  if (case == "C1_DxEx") nu <- Dx/Ex

  # When rates are the input the observed rates stand in for the death counts
  # in the likelihood-based objectives, as in the original specification
  if (case == "C2_mx") {
    Dx <- mx
    nu <- mx
  }

  if (case == "C3_qx") {
    Dx <- qx
    nu <- qx
  }

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
  out <- sum(ifelse(is.finite(loss), loss, 1e5))
  return(out)
}

#' Scale the age vector for stable optimisation
#'
#' Rescales the ages so that the minimum age becomes 1, which keeps the
#' exponentiated terms of the laws within a reasonable range.
#' @param x A numeric vector of ages.
#' @return A numeric vector of scaled ages, where min(x) == 1.
#' @noRd
scale_x <- function(x) {
  x - min(x) + 1
}

#' Minimise the objective function and check convergence
#'
#' Runs \code{\link{nlminb}} (PORT routines) or, for the inverse Weibull law,
#' \code{\link{optim}} with the Nelder-Mead algorithm, and warns when the
#' optimiser does not converge.
#' @param foo Objective function of the parameter vector on the log scale.
#' @param start Starting values of the parameters on the log scale.
#' @param law The mortality law being fitted.
#' @return The optimisation object with an added \code{fnvalue} component.
#' @noRd
run_optimiser <- function(foo, start, law) {

  if (law == 'invweibull') {
    opt <- optim(par = start, fn = foo, method = 'Nelder-Mead')
    opt$fnvalue <- opt$value

  } else {
    opt <- nlminb(
      start = start,
      objective = foo,
      control = list(eval.max = 5000, iter.max = 5000)
      )
    opt$fnvalue <- opt$objective
  }

  # nlminb returns a message (e.g. "relative convergence (4)") also on success,
  # so only a non-zero convergence code is treated as a failure
  if (opt$convergence != 0) {
    msg <- paste0(
      "MortalityLaw: optimisation did not converge (code ",
      opt$convergence, ")"
      )

    if (!is.null(opt$message) && nzchar(opt$message)) {
      msg <- paste0(msg, ": ", opt$message)
    }

    warning(msg, call. = FALSE)
  }

  return(opt)
}

#' Run the optimisation routine
#'
#' Core of \code{\link{MortalityLaw}}. Normalises the fitting ages to the order
#' of \code{x}, scales the age vector if the law requires it, obtains the
#' starting parameters, minimises the objective function on the log parameter
#' scale and derives the fitted hazard and the goodness-of-fit measures.
#' @param input A list containing all input arguments to \code{\link{MortalityLaw}}.
#' @return A list with the fitted hazard values, the optimisation diagnosis,
#' the estimated coefficients and the log-likelihood, AIC and BIC.
#' @noRd
choose_optim <- function(input) {

  with(as.list(input), {
    # Normalise the fitting subset to the order of x and drop duplicates
    fit.this.x <- x[x %in% fit.this.x]
    select.x   <- x %in% fit.this.x
    case       <- detect_case(Dx = Dx, Ex = Ex, mx = mx, qx = qx)$case
    fn         <- law_function(law = law, custom.law = custom.law)

    if (scale.x) {
      new.fit.this.x <- scale_x(fit.this.x)
      d     <- fit.this.x[1] - new.fit.this.x[1]
      new.x <- x - d

    } else {
      new.fit.this.x <- fit.this.x
      new.x <- x

    }

    # Starting parameters: defaults, validation and matching by name
    if (law != "custom.law") {
      parS <- bring_parameters(law = law, par = parS)
    }

    # Objective function on the log parameter scale
    foo <- function(pars) {
      objective_loss(
        par = pars,
        x = new.fit.this.x,
        Dx = Dx[select.x],
        Ex = Ex[select.x],
        mx = mx[select.x],
        qx = qx[select.x],
        case = case,
        fn = fn,
        opt.method = opt.method
        )
    }
    opt <- run_optimiser(foo = foo, start = log(parS), law = law)

    # Return the optimal parameters; exp() can underflow to 0 at the boundary
    C <- exp(opt$par)
    C[C <= 0] <- .Machine$double.xmin

    if (law == 'kostaki') { # kostaki hack
      if (C[5] >= 50*C[6]) C[6] <- C[5]/50
    }

    hx <- fn(x = new.x, par = C)$hx

    # Compute goodness of fit measures
    logLik <- -opt$fnvalue
    AIC    <- 2 * length(parS) - 2 * logLik
    BIC    <- log(length(new.fit.this.x)) * length(parS) - 2 * logLik

    if (!any(opt.method %in% c('poissonL', 'binomialL'))) {
      logLik <- AIC <- BIC <- NaN
    }

    out <- as.list(environment())
    return(out)
  })
}
