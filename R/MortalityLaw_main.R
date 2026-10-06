# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-06 22:51:07
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
#' \item{deviance.residuals}{Deviance residuals. For the count cases
#' (\code{Dx}/\code{Ex}) they are the Poisson deviance residuals; for the rate
#' cases (\code{mx}, \code{qx}) they are the log-residuals.}
#' \item{pearson.residuals}{Pearson residuals. For the count cases they are the
#' Poisson Pearson residuals; for the rate cases they are the log-residuals.}
#' \item{goodness.of.fit}{Named numeric vector (single fit) or matrix (one row
#' per fit) with log-likelihood, AIC and BIC (NaN for non-likelihood methods).
#' For count fits the log-likelihood is the Poisson or binomial kernel: the
#' data-only additive constants are dropped, which leaves model comparison
#' (AIC/BIC) unaffected but makes the absolute value differ from \code{glm}.}
#' \item{opt.diagnosis}{Object returned by the optimisation routine, useful
#' for checking convergence.}
#' \item{df}{Number of parameters, residual degrees of freedom and the
#' dispersion.}
#' \item{dispersion}{Dispersion of the fit. For the count cases it is the
#' Pearson chi-square divided by the residual degrees of freedom (the GLM
#' dispersion, 1 for a correctly specified Poisson model); for the rate cases
#' it is the mean squared log-residual.}
#' \item{deviance}{The deviance of the fit. For the count cases this is the
#' Poisson deviance, the quantity minimised by \code{"poissonL"}; for the rate
#' cases it is the sum of squared log-residuals.}
#' @seealso
#' \code{\link{availableLaws}} for a list of all implemented models;
#' \code{\link{availableLF}} for loss function details;
#' \code{\link{LifeTable}} for life table construction;
#' \code{\link{ReadHMD}} for downloading data from the Human Mortality Database.
#' @author Marius D. Pascariu
#' @example inst/examples/MortalityLaw.R
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
      deviance.residuals = stat$deviance.residuals,
      pearson.residuals = stat$pearson.residuals,
      goodness.of.fit = stat$goodness.of.fit,
      opt.diagnosis = dgn,
      df = stat$df,
      dispersion = stat$dispersion,
      deviance = stat$deviance
      )
    return(out)
  })
}

#' Compute the residuals, deviance and goodness-of-fit of a single fit
#'
#' Compares the fitted hazard with the observed data of the problem case and
#' derives the residuals, the deviance and the statistical diagnostics of a
#' \code{"MortalityLaw"} object.
#'
#' For the count cases (\code{Dx}/\code{Ex}) the deviance, the Pearson
#' chi-square and the log-likelihood follow the Poisson definitions, so that a
#' fit obtained with \code{opt.method = "poissonL"} or \code{"binomialL"}
#' reports the same measures a GLM would, and the reported deviance is the
#' quantity the optimiser actually minimised. For the rate cases (\code{mx},
#' \code{qx}) there is no count likelihood, so the deviance is the sum of
#' squared log-residuals (a least-squares measure on the log scale) and the
#' dispersion is the mean squared log-residual, the analogue of the residual
#' variance on the log scale.
#' @param fit Fitted hazard values, one per fitted age.
#' @param optim.model Result of \code{choose_optim}.
#' @param input A list of input arguments to \code{\link{MortalityLaw}}.
#' @param K Problem case details as returned by \code{detect_case}.
#' @return A list with the residuals, deviance, degrees of freedom, dispersion
#' and goodness-of-fit measures.
#' @noRd
fit_statistics <- function(fit, optim.model, input, K) {

  with(as.list(input), {
    p   <- length(optim.model$C)
    obs <- switch(K$case,
      C1_DxEx = Dx / Ex,
      C2_mx   = mx,
      C3_qx   = qx
      )
    # Raw residuals: observed minus fitted
    resid <- obs - fit

    if (K$case == "C1_DxEx") {
      # Poisson diagnostics on the count scale. mu is the fitted hazard, so the
      # expected count is mu * Ex and the Poisson log-likelihood is
      # sum(Dx * log(mu) - mu * Ex). Ages where the law is not defined are
      # left out of the deviance and the log-likelihood.
      exp_count  <- fit * Ex
      pearson    <- (Dx - exp_count) / sqrt(exp_count)
      dev_resid  <- sign(Dx - exp_count) *
        sqrt(2 * (ifelse(Dx > 0, Dx * log(Dx / exp_count), 0) - (Dx - exp_count)))
      keep       <- is.finite(dev_resid)
      dev        <- sum(dev_resid[keep]^2)
      logLik     <- sum((Dx * log(fit) - exp_count)[keep])
      rdf        <- sum(x %in% optim.model$fit.this.x) - p
      disp       <- sum(pearson^2) / rdf

    } else {
      # Rate cases: no count likelihood. The deviance is the sum of squared
      # log-residuals; the dispersion is its mean, i.e. the residual variance
      # on the log scale. An age where the law is not defined (a missing
      # hazard) carries no information about the fit and is left out.
      log_resid  <- log(obs) - log(fit)
      keep       <- is.finite(log_resid)
      dev        <- sum(log_resid[keep]^2)
      dev_resid  <- log_resid
      pearson    <- log_resid
      logLik     <- NaN
      rdf        <- sum(x %in% optim.model$fit.this.x) - p
      disp       <- dev / rdf
    }

    # The likelihood-based information criteria are only defined when the
    # objective was a likelihood; otherwise the fit reports NaN (see
    # choose_optim).
    logLik_opt <- optim.model$logLik
    AIC_opt    <- optim.model$AIC
    BIC_opt    <- optim.model$BIC

    df  <- c(n.param = p, df.residual = rdf, dispersion = disp)
    gof <- c(logLik = logLik_opt, AIC = AIC_opt, BIC = BIC_opt)

    out <- list(
      residuals = resid,
      deviance = dev,
      deviance.residuals = dev_resid,
      pearson.residuals = pearson,
      dispersion = disp,
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
      deviance.residuals = parts$deviance.residuals,
      pearson.residuals = parts$pearson.residuals,
      goodness.of.fit = parts$goodness.of.fit,
      opt.diagnosis = parts$opt.diagnosis,
      df = parts$df,
      dispersion = parts$dispersion,
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
  dres  <- do.call(cbind, lapply(fits, function(M) M$deviance.residuals))
  pres  <- do.call(cbind, lapply(fits, function(M) M$pearson.residuals))
  gof   <- do.call(rbind, lapply(fits, function(M) M$goodness.of.fit))
  df    <- do.call(rbind, lapply(fits, function(M) M$df))
  dgn   <- lapply(fits, function(M) M$opt.diagnosis)
  dev   <- unlist(lapply(fits, function(M) M$deviance))
  disp  <- unlist(lapply(fits, function(M) M$dispersion))

  rownames(cf)  <- K$LTnames
  rownames(gof) <- K$LTnames
  rownames(df)  <- K$LTnames
  names(dev)    <- K$LTnames
  names(disp)   <- K$LTnames
  dimnames(fit)   <- list(x, K$LTnames)
  dimnames(resid) <- list(x, K$LTnames)
  dimnames(dres)  <- list(x, K$LTnames)
  dimnames(pres)  <- list(x, K$LTnames)

  out <- list(
    coefficients = cf,
    fitted.values = fit,
    residuals = resid,
    deviance.residuals = dres,
    pearson.residuals = pres,
    goodness.of.fit = gof,
    opt.diagnosis = dgn,
    df = df,
    dispersion = disp,
    deviance = dev
    )
  return(out)
}

#' Retrieve model-specific details for fitting
#'
#' Returns the internal law name, the starting parameters, the model
#' information table and the \code{SCALE_X} flag for the chosen law.
#' @inheritParams MortalityLaw
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
#' @inheritParams MortalityLaw
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
#' @inheritParams MortalityLaw
#' @param par Parameter vector on the log scale.
#' @param Dx,Ex,mx,qx Observed data; the cases that do not apply are \code{NULL}.
#' @param opt.method The loss function, see \code{\link{availableLF}}.
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
#' @inheritParams MortalityLaw
#' @param par Parameter vector on the log scale.
#' @param Dx,Ex,mx,qx Observed data; the cases that do not apply are \code{NULL}.
#' @param opt.method The loss function, see \code{\link{availableLF}}.
#' @param case Problem case, one of \code{C1_DxEx}, \code{C2_mx}, \code{C3_qx}.
#' @param fn Function computing \code{hx} when called as \code{fn(x = , par = )}.
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
#' @inheritParams MortalityLaw
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
#' @inheritParams MortalityLaw
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

    # The Weibull hazard is not defined at birth: it is 0 when the shape
    # exceeds one and unbounded when it is smaller. Leaving age 0 in the
    # objective adds only a large constant, which loosens the optimiser's
    # relative tolerance, so drop it from the fitting ages.
    if (law == 'weibull' && any(fit.this.x == 0)) {
      warning(paste0(
        "MortalityLaw: the Weibull hazard is not defined at age 0, so age 0 ",
        "is left out of the fit; fit the law from age 1."), call. = FALSE)
      fit.this.x <- fit.this.x[fit.this.x != 0]
      select.x   <- x %in% fit.this.x
    }

    # An age with zero exposure carries no information about the hazard: its
    # observed rate is 0/0 or Inf. Leaving it in the objective adds only a
    # large constant, which loosens the optimiser's relative tolerance, so
    # drop it from the fitting ages.
    if (!is.null(Ex)) {
      zero.ex <- Ex[match(fit.this.x, x)] == 0

      if (any(zero.ex)) {
        message(paste0(
          "MortalityLaw: ", sum(zero.ex), " age(s) with zero exposure are ",
          "left out of the fit."))
        fit.this.x <- fit.this.x[!zero.ex]
        select.x   <- x %in% fit.this.x
      }

      if (length(fit.this.x) < 2) {
        stop("MortalityLaw: fewer than two ages carry exposure, ",
             "nothing to fit.", call. = FALSE)
      }
    }

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

    # De Moivre's hazard is defined only below its limiting age N, and the fit
    # always puts N just above the top fitted age, so a prediction past the
    # fitted range can turn negative. Say so on every fit.
    if (law == 'demoivre') {
      warning(paste0(
        "MortalityLaw: 'demoivre' is defined only below its limiting age ",
        "(fitted N = ", format(C[["N"]], digits = 4), "). Do not extrapolate ",
        "past the fitted ages ", min(new.fit.this.x), "-",
        max(new.fit.this.x), ": the hazard turns negative above N."),
        call. = FALSE)
    }

    # The truncated power model degenerates onto the shifted power law when the
    # exponential term is not identified: D collapses to the optimisation
    # boundary and contributes nothing over the fitted age range. This happens
    # on coarse (year) or short infant age ranges, where the age span is too
    # narrow in units of 1/D to separate the power and exponential parts.
    if (law == 'scholey') {
      Dstar <- C[["D"]]
      if (Dstar * max(new.fit.this.x) < 1e-3) {
        warning(paste0(
          "MortalityLaw: the truncation parameter 'D' of 'scholey' fitted at ",
          "the boundary (D = ", format(Dstar, digits = 3), "), so the ",
          "exponential term is not identified over ages ",
          min(new.fit.this.x), "-", max(new.fit.this.x),
          " and the model reduces to 'scholey_shifted_power'. The truncated ",
          "power law needs finer age resolution (days or weeks over the first ",
          "year) for 'D' to be estimable."), call. = FALSE)
      }
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

# --------------------------------------------------------------------

#' Check Available Loss Functions
#'
#' Returns information about the loss functions implemented for use with the
#' optimisation procedure in the \code{\link{MortalityLaw}} function.
#'
#' The two likelihoods (\code{"poissonL"}, \code{"binomialL"}) are the only
#' objectives that yield a log-likelihood, an AIC and a BIC; the six loss
#' functions (\code{"LF1"} to \code{"LF6"}) leave those measures undefined
#' (\code{NaN}) and are compared on the deviance or the loss itself.
#' \code{"LF2"}, the squared log-ratio, is the default: it is scale-free and
#' robust, and it has been observed to return reliable estimates for the
#' high-parameter laws such as Heligman-Pollard. There is no universally best
#' choice, so it is worth trying more than one.
#' @return A list of class \code{availableLF} with the components:
#'  \item{table}{Table with loss functions and codes to be used in \code{\link{MortalityLaw}}.}
#'  \item{legend}{Table with details about the abbreviation used.}
#' @seealso \code{\link{MortalityLaw}}
#' @author Marius D. Pascariu
#' @examples availableLF()
#' @export
availableLF <- function(){
  tab <- as.data.frame(
    matrix(c("L = -[Dx * log(mu) - mu*Ex]", "poissonL",
             "L = -[Dx * log(1 - exp(-mu)) - (Ex - Dx)*mu]  ", "binomialL",
             "L =  [1 - mu/ov]^2", "LF1",
             "L =  log[mu/ov]^2", "LF2",
             "L =  [(ov - mu)^2]/ov", "LF3",
             "L =  [ov - mu]^2", "LF4",
             "L =  [ov - mu] * log[ov/mu]", "LF5",
             "L =  abs(ov - mu)", "LF6"), ncol = 2, byrow = T))
  colnames(tab) <- c("LOSS FUNCTION", "CODE")

  legend <- c("Dx: Death counts",
              "Ex: Population exposed to risk",
              "mu: Estimated value",
              "ov: Observed value")

  out <- structure(class = "availableLF", list(table = tab, legend = legend))
  return(out)
}


#' Print Available Loss Functions
#'
#' Prints the table of loss functions and their codes, with the legend that
#' explains the notation, followed by a short note on choosing between them.
#' @param x An object of class \code{"availableLF"}.
#' @param ... Further arguments passed to or from other methods.
#' @return The object \code{x}, invisibly. Called for its printed output.
#' @keywords internal
#' @export
print.availableLF <- function(x, ...) {
  cat("\nLoss functions available in the package:\n\n")
  print(x$table, right = FALSE, row.names = FALSE)
  cat("\nLEGEND:\n")
  cat(x$legend, sep = '\n')

  message("\nHINT: Most loss functions work well with 'poissonL'. However, for complex ",
          "mortality laws like Heligman-Pollard (HP), a better fit can be obtained using ",
          "other loss functions (e.g. 'LF2'). You are strongly encouraged to test ",
          "different options before deciding on the final version. The results might be ",
          "slightly different.\n")
}
