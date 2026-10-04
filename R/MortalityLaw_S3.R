# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-04 17:46:32
# --------------------------------------------

#' Print a Fitted Mortality Law
#'
#' Prints a compact one-line description of a \code{"MortalityLaw"} object:
#' the law that was fitted and whether the fitted values are hazards
#' (\code{mx}) or death probabilities (\code{qx}).
#' @param x An object of class \code{"MortalityLaw"}.
#' @param ... Further arguments passed to or from other methods.
#' @return The object \code{x}, invisibly. Called for its printed output.
#' @seealso \code{\link{MortalityLaw}} to fit a law;
#'   \code{\link{summary.MortalityLaw}} for the full diagnostic summary.
#' @keywords internal
#' @export
print.MortalityLaw <- function(x, ...) {
  L    <- x$input$law == "custom.law"
  info <- if (L) "Custom Mortality Law" else as.matrix(x$info$model.info[, c(2, 3)])
  cat(paste(info, collapse = " model: "))
  fv   <- if (!is.null(x$input$qx)) "qx" else "mx"
  cat("\nFitted values:", fv, "\n")
  return(invisible(x))
}


#' Summarise a Fitted Mortality Law
#'
#' Collects the fitted coefficients, the goodness-of-fit measures, the
#' dispersion and a five-number summary of the residuals into a compact
#' object for printing, and rounds them to \code{digits}. For models with
#' more than four parameters only the first and the last two coefficients
#' and fit measures are kept, so the output fits on one screen.
#' @param object An object of class \code{"MortalityLaw"}.
#' @param digits Number of significant digits to display.
#' @param ... Additional arguments affecting the summary produced.
#' @return An object of class \code{"summary.MortalityLaw"}, a list holding
#'   the model information, the matched call, the goodness-of-fit measures,
#'   the dispersion, the residual summary, the rounded coefficients and the
#'   degrees of freedom.
#' @seealso \code{\link{MortalityLaw}} to fit a law;
#'   \code{\link{coef}} and \code{\link{fitted}} for the extracted values.
#' @examples
#' x  <- 45:75
#' M1 <- MortalityLaw(x = x, Dx = ahmd$Dx[as.character(x), "1950"],
#'                    Ex = ahmd$Ex[as.character(x), "1950"], law = "makeham")
#' summary(M1)
#' @export
summary.MortalityLaw <- function(object, ...,
                                 digits = max(3L, getOption("digits") - 3L)) {
  x      <- object
  L1     <- x$input$law == "custom.law"
  mi     <- if (L1) "Custom Mortality Law" else as.matrix(x$info$model.info[, c(2, 3)])
  res    <- summary(as.vector(as.matrix(x$residuals)))
  fv     <- if (!is.null(x$input$qx)) "qx" else "mx"
  gof    <- round(x$goodness.of.fit, digits)
  param  <- round(coef(x), digits)
  # Dispersion: for count fits it is the Pearson chi-square over the residual
  # degrees of freedom (a GLM dispersion); for rate fits it is the mean squared
  # log-residual. Both are reported by fit_statistics.
  disp   <- x$dispersion
  sigma  <- if (is.matrix(disp)) rowMeans(disp) else mean(disp)
  nc     <- nrow(param)
  L2     <- is.null(nc)
  L3     <- x$input$opt.method %in% c("poissonL", "binomialL")

  if (!L2 && nc > 4) {
    param <- head_tail(
      x       = param,
      hlength = 2,
      tlength = 2,
      digits  = digits
      )
    gof   <- head_tail(
      x       = gof,
      hlength = 2,
      tlength = 2,
      digits  = digits
      )
  }

  out <- list(
    info   = mi,
    call   = x$info$call,
    gof    = gof,
    sigma  = sigma,
    fv     = fv,
    resid  = res,
    param  = param,
    df     = x$df,
    digits = digits,
    L1     = L1,
    L2     = L2,
    L3     = L3
    )
  out <- structure(class = "summary.MortalityLaw", out)
  return(out)
}


#' Print a MortalityLaw Summary
#'
#' Prints the contents of a \code{"summary.MortalityLaw"} object: the model
#' description, the matched call, the residual summary, the coefficients and,
#' for likelihood-based fits, the goodness-of-fit measures and the
#' degrees of freedom.
#' @param x An object of class \code{"summary.MortalityLaw"}.
#' @param ... Additional arguments affecting the summary produced.
#' @return The object \code{x}, invisibly. Called for its printed output.
#' @seealso \code{\link{summary.MortalityLaw}}.
#' @keywords internal
#' @export
print.summary.MortalityLaw <- function(x, ...) {
  with(x, {
    cat(paste(info, collapse = " model: "))
    cat("\nFitted values:", fv)
    cat("\n\nCall: ")
    print(call)
    cat("\nResiduals:\n")
    print(round(resid, digits))
    cat("\nParameters:\n")
    print(param)
    sg <- format(signif(sigma, digits))

    if (L3) {
      cat("\nGoodness of fit:\n")
      print(gof)
    }

    if (L2) {
      cat("\nDispersion:", sg, "on", df[2], "degrees of freedom")
    } else {
      cat("\nAverage dispersion:", sg, "on", df[1, 2], "degrees of freedom")
    }
  })
  return(invisible(x))
}


#' Extract the Log-Likelihood of a Fitted Mortality Law
#'
#' Returns the maximised log-likelihood of a \code{"MortalityLaw"} fit. It is
#' only defined when the objective was a likelihood, that is when the model
#' was fitted with \code{opt.method = "poissonL"} or \code{"binomialL"}; for
#' the loss-function objectives the value is \code{NaN}. For a multiple fit
#' the log-likelihoods are returned as a named vector.
#' @param object An object of class \code{"MortalityLaw"}.
#' @param ... Further arguments passed to or from other methods.
#' @return An object of class \code{"logLik"} for a single fit, or a named
#'   numeric vector of log-likelihoods for a multiple fit.
#' @seealso \code{\link{MortalityLaw}}; \code{\link{AIC.MortalityLaw}}.
#' @examples
#' x  <- 45:75
#' M1 <- MortalityLaw(x = x, Dx = ahmd$Dx[as.character(x), "1950"],
#'                    Ex = ahmd$Ex[as.character(x), "1950"],
#'                    law = "makeham", opt.method = "poissonL")
#' logLik(M1)
#' @export
logLik.MortalityLaw <- function(object, ...) {
  gof <- object$goodness.of.fit

  if (is.matrix(gof)) {
    out <- gof[, "logLik"]
  } else {
    df_fit <- object$df["n.param"]
    df_res <- object$df["df.residual"]
    value  <- unname(gof["logLik"])
    out    <- structure(
      value,
      class = "logLik",
      df    = unname(df_fit),
      nobs  = unname(df_fit) + unname(df_res)
      )
  }

  return(out)
}


#' Extract the AIC of a Fitted Mortality Law
#'
#' Returns the Akaike information criterion of a \code{"MortalityLaw"} fit,
#' \code{2 * k - 2 * logLik}, with \eqn{k} the number of fitted parameters.
#' Like the log-likelihood it is defined only for the likelihood-based
#' objectives and is \code{NaN} otherwise. Use it to compare candidate laws
#' fitted to the same data.
#' @param object An object of class \code{"MortalityLaw"}.
#' @param ... Further arguments passed to or from other methods.
#' @return The AIC value for a single fit, or a named vector of AIC values
#'   for a multiple fit.
#' @seealso \code{\link{MortalityLaw}}; \code{\link{logLik.MortalityLaw}}.
#' @examples
#' x  <- 45:75
#' M1 <- MortalityLaw(x = x, Dx = ahmd$Dx[as.character(x), "1950"],
#'                    Ex = ahmd$Ex[as.character(x), "1950"],
#'                    law = "makeham", opt.method = "poissonL")
#' AIC(M1)
#' @export
AIC.MortalityLaw <- function(object, ...) {
  gof <- object$goodness.of.fit
  out <- if (is.matrix(gof)) gof[, "AIC"] else gof["AIC"]

  return(out)
}


#' Extract the Deviance of a Fitted Mortality Law
#'
#' Returns the deviance of a \code{"MortalityLaw"} fit. For a fit entered
#' from death counts and exposures (\code{Dx, Ex}) it is the Poisson
#' deviance, the quantity \code{opt.method = "poissonL"} minimises. For a fit
#' entered from rates (\code{mx} or \code{qx}) there is no count likelihood,
#' so it is the sum of squared log-residuals.
#' @param object An object of class \code{"MortalityLaw"}.
#' @param ... Further arguments passed to or from other methods.
#' @return The deviance for a single fit, or a named vector of deviances for
#'   a multiple fit.
#' @seealso \code{\link{MortalityLaw}}; \code{\link{dispersion}}.
#' @examples
#' x  <- 45:75
#' M1 <- MortalityLaw(x = x, Dx = ahmd$Dx[as.character(x), "1950"],
#'                    Ex = ahmd$Ex[as.character(x), "1950"],
#'                    law = "makeham", opt.method = "poissonL")
#' deviance(M1)
#' @export
deviance.MortalityLaw <- function(object, ...) {
  out <- object$deviance

  return(out)
}


#' Extract the Residual Degrees of Freedom of a Fitted Mortality Law
#'
#' Returns the residual degrees of freedom of a \code{"MortalityLaw"} fit,
#' the number of fitted ages minus the number of estimated parameters.
#' @param object An object of class \code{"MortalityLaw"}.
#' @param ... Further arguments passed to or from other methods.
#' @return The residual degrees of freedom for a single fit, or a named
#'   vector for a multiple fit.
#' @seealso \code{\link{MortalityLaw}}; \code{\link{dispersion}}.
#' @examples
#' x  <- 45:75
#' M1 <- MortalityLaw(x = x, Dx = ahmd$Dx[as.character(x), "1950"],
#'                    Ex = ahmd$Ex[as.character(x), "1950"],
#'                    law = "makeham", opt.method = "poissonL")
#' df.residual(M1)
#' @export
df.residual.MortalityLaw <- function(object, ...) {
  df_all <- object$df

  out <- if (is.matrix(df_all)) df_all[, "df.residual"] else df_all["df.residual"]

  return(out)
}


#' Dispersion of a Fitted Mortality Law
#'
#' Returns the dispersion of the fit, a scalar measure of how far the fitted
#' values spread around the data. For the count cases it is the Pearson
#' chi-square divided by the residual degrees of freedom, the GLM dispersion
#' (about 1 for a correctly specified Poisson model); for the rate cases it
#' is the mean squared log-residual. The value is also reported by
#' \code{\link{summary.MortalityLaw}}.
#' @param object An object of class \code{"MortalityLaw"}.
#' @param ... Further arguments passed to or from other methods.
#' @return The dispersion for a single fit, or a named vector of dispersions
#'   for a multiple fit.
#' @seealso \code{\link{MortalityLaw}}; \code{\link{deviance.MortalityLaw}}.
#' @examples
#' x  <- 45:75
#' M1 <- MortalityLaw(x = x, Dx = ahmd$Dx[as.character(x), "1950"],
#'                    Ex = ahmd$Ex[as.character(x), "1950"],
#'                    law = "makeham", opt.method = "poissonL")
#' dispersion(M1)
#' @export
dispersion <- function(object, ...) {
  UseMethod("dispersion")
}

#' @rdname dispersion
#' @export
dispersion.MortalityLaw <- function(object, ...) {
  return(object$dispersion)
}


#' Predict from a Fitted Mortality Law
#'
#' Evaluates a fitted mortality law at new ages. The coefficients are reused
#' as they are, so the prediction is an extrapolation of the fitted curve:
#' it is meaningful over the ages that the law describes and becomes
#' unreliable far outside the fitted range. Models that scale the age vector
#' during fitting (the \code{SCALE_X} column of \code{\link{availableLaws}})
#' are rescaled internally, so the prediction stays consistent with the
#' coefficients.
#' @param object An object of class \code{"MortalityLaw"}.
#' @param x Vector of ages at which to evaluate the fitted law.
#' @param ... Additional arguments affecting the predictions produced.
#' @return A named vector of predicted mortality values for a single fit, or
#'   a matrix with one column per fit. The values are hazards
#'   (\code{mu[x]}) or death probabilities (\code{q[x]}), depending on the
#'   law; see the \code{FIT} column of \code{\link{availableLaws}}.
#' @seealso \code{\link{MortalityLaw}}; \code{\link{fitted}}.
#' @author Marius D. Pascariu
#' @examples
#' # Extrapolate old-age mortality with the Kannisto model
#' # Fit ages 80-94 and extrapolate up to 120.
#'
#' Mx <- ahmd$mx[paste(80:94), "1950"]
#' M1 <- MortalityLaw(x = 80:94, mx  = Mx, law = 'kannisto')
#' fitted(M1)
#' predict(M1, x = 80:120)
#'
#' # See more examples in MortalityLaw function help page.
#' @export
predict.MortalityLaw <- function(object, x, ...){
  if (min(x) < 0) {
    stop("'x' must be greater or equal to zero.", call. = FALSE)
  }

  law   <- object$input$law
  sx    <- object$input$scale.x
  new.x <- x

  if (sx) {
    fit.this.x <- object$input$fit.this.x
    d     <- fit.this.x[1] - scale_x(x = fit.this.x)[1]
    new.x <- x - d
  }

  Par    <- coef(object)
  single <- !is.matrix(Par)

  if (single) {
    Par <- matrix(Par, nrow = 1, dimnames = list("", names(Par)))
  }

  fn <- if (law == "custom.law") object$input$custom.law else get(law)

  hx <- apply(
    X      = Par,
    MARGIN = 1,
    FUN    = function(X) fn(x = new.x, par = X)$hx
    )
  hx <- matrix(hx, nrow = length(x))
  rownames(hx) <- x
  colnames(hx) <- rownames(Par)

  if (single) {
    value        <- as.numeric(hx)
    names(value) <- x
    hx           <- value
  }

  return(hx)
}
