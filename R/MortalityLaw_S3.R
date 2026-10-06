# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-06 00:32:38
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


#' Observed mortality series of a fit
#'
#' The series the fit was fitted to: the death probabilities if the model was
#' entered as \code{qx}, the death rates \code{Dx/Ex} for count input, or the
#' \code{mx} rates directly.
#' @param x An object of class \code{"MortalityLaw"}.
#' @return A numeric vector or matrix with the observed series.
#' @noRd
observed_values <- function(x) {
  if (!is.null(x$input$qx)) {
    x$input$qx
  } else {
    with(x$input, if (is.null(mx)) Dx / Ex else mx)
  }
}


#' Coefficient of determination and RMSE of a fit
#'
#' Computed on the observed scale over the fitted age range only. Ages the
#' model cannot be placed on (missing or non-positive rates) carry no fit
#' information and are left out.
#' @param x An object of class \code{"MortalityLaw"}.
#' @return A named vector \code{c(R.squared, RMSE)} for a single fit, or a
#'   curve-by-column matrix for a multiple fit.
#' @noRd
fit_quality <- function(x) {
  obs <- observed_values(x)
  fit <- x$fitted.values
  age <- x$input$x
  age2 <- x$input$fit.this.x

  one <- function(o, f) {
    keep <- age %in% age2 & is.finite(o) & is.finite(f) & o > 0 & f > 0
    den  <- sum((o[keep] - mean(o[keep]))^2)
    c(R.squared = if (den > 0) {
      1 - sum((o[keep] - f[keep])^2) / den
    } else {
      NA_real_
    },
    RMSE = sqrt(mean((o[keep] - f[keep])^2)))
  }

  if (is.matrix(fit)) {
    out <- t(vapply(seq_len(ncol(fit)),
                    function(j) one(obs[, j], fit[, j]),
                    numeric(2)))
    rownames(out) <- colnames(fit)
    out
  } else {
    one(obs, fit)
  }
}


#' Summarise a Fitted Mortality Law
#'
#' Collects the fitted coefficients, the fit measures and a summary of the
#' residuals into a compact object for printing, and rounds them to
#' \code{digits}. The fit measures cover the fit window (the ages fitted and
#' the ages reported), the optimisation (method used, optimiser outcome), the
#' deviance with its degrees of freedom and dispersion, and the
#' R-squared and RMSE of the fit. For a likelihood-based fit the maximised
#' log-likelihood with its information criteria is included. When more than
#' four curves were fitted only the first and the last two are kept in the
#' printed coefficients and fit measures, so the output fits on one screen.
#' @param object An object of class \code{"MortalityLaw"}.
#' @param digits Number of significant digits to display.
#' @param ... Additional arguments affecting the summary produced.
#' @return An object of class \code{"summary.MortalityLaw"}, a list holding
#'   the model information, the matched call, the rounded coefficients, the
#'   goodness-of-fit measures, the deviance and the degrees of freedom, the
#'   R-squared and RMSE of the fit, the optimisation outcome, the fit window
#'   and the residual summaries on the raw and the deviance scale.
#' @seealso \code{\link{MortalityLaw}} to fit a law;
#'   \code{\link{coef}} and \code{\link{fitted}} for the extracted values.
#' @example inst/examples/summary.MortalityLaw.R
#' @export
summary.MortalityLaw <- function(object, ...,
                                 digits = max(3L, getOption("digits") - 3L)) {
  x      <- object
  L1     <- x$input$law == "custom.law"
  mi     <- if (L1) "Custom Mortality Law" else as.matrix(x$info$model.info[, c(2, 3)])
  res    <- summary(as.vector(as.matrix(x$residuals)))
  dres   <- summary(as.vector(as.matrix(x$deviance.residuals)))
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

  dgn    <- x$opt.diagnosis
  conv   <- if (L2) dgn$convergence else
    vapply(dgn, function(d) d$convergence, numeric(1))
  iter   <- if (L2) dgn$iterations else
    vapply(dgn, function(d) d$iterations, numeric(1))
  msg    <- if (L2) dgn$message else
    vapply(dgn, function(d) d$message, character(1))

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
    dres   = dres,
    param  = param,
    deviance = signif(x$deviance, digits),
    rq     = signif(fit_quality(x), digits),
    method = x$input$opt.method,
    optim  = list(convergence = conv, iterations = iter, message = msg),
    n.curve = if (L2) 1L else nrow(coef(x)),
    age.range = range(x$input$x),
    fit.range = range(x$input$fit.this.x),
    n.fit  = sum(x$input$x %in% x$input$fit.this.x),
    n.age  = length(x$input$x),
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
#' description, the fit window, the matched call, the coefficients, the fit
#' measures (method and optimiser outcome, deviance, R-squared and RMSE, and
#' for likelihood-based fits the goodness-of-fit block) and the residuals on
#' the raw and the deviance scale.
#' @param x An object of class \code{"summary.MortalityLaw"}.
#' @param ... Additional arguments affecting the summary produced.
#' @return The object \code{x}, invisibly. Called for its printed output.
#' @seealso \code{\link{summary.MortalityLaw}}.
#' @keywords internal
#' @export
print.summary.MortalityLaw <- function(x, ...) {
  with(x, {
    cat(paste(info, collapse = " model: "), "\n", sep = "")
    cat("Fitted values: ", fv,
        "  |  ages ", age.range[1], "-", age.range[2],
        "  |  fitted on ", fit.range[1], "-", fit.range[2],
        " (", n.fit, " of ", n.age, " ages)",
        if (L2) "" else paste0("  |  ", n.curve, " curves"),
        "\n", sep = "")

    cat("\nCall:\n")
    print(call)

    cat("\nCoefficients", if (L2) "" else " by curve", ":\n", sep = "")
    if (L2) {
      print(cbind(estimate = param))
    } else {
      print(param)
    }

    # Optimiser outcome: a non-zero convergence code is reported as such,
    # with the optimiser's own message; the common case reads short.
    if (all(optim$convergence == 0)) {
      opt_lab <- if (L2) {
        sprintf("optimiser converged in %d iterations", max(optim$iterations))
      } else {
        sprintf("optimiser converged for all %d curves, up to %d iterations",
                n.curve, max(optim$iterations))
      }
    } else {
      opt_lab <- sprintf("optimiser NOT converged (code %s: %s), %d iterations",
                         paste(unique(optim$convergence), collapse = "/"),
                         paste(unique(optim$message), collapse = "; "),
                         max(optim$iterations))
    }

    cat("\nFit", if (L2) "" else " by curve", ":\n", sep = "")
    cat("  method ", method, "  |  ", opt_lab, "\n", sep = "")

    if (L2) {
      fmt <- function(v) format(v, scientific = FALSE, trim = TRUE)
      cat("  deviance ", fmt(deviance), " on ", df["df.residual"],
          " degrees of freedom  |  dispersion ", fmt(signif(sigma, digits)),
          "\n", sep = "")
      cat("  R-squared ", fmt(rq["R.squared"]),
          "  |  RMSE ", fmt(rq["RMSE"]), "\n", sep = "")
    } else {
      tab <- data.frame(
        deviance = deviance,
        df.residual = df[, "df.residual"],
        dispersion = round(df[, "dispersion"], digits),
        R.squared = rq[, "R.squared"],
        RMSE = rq[, "RMSE"]
        )
      if (n.curve > 4) {
        tab <- head_tail(x = tab, hlength = 2, tlength = 2, digits = digits)
      }
      print(tab)
      cat("  Average dispersion: ", format(signif(sigma, digits)), " on ",
          df[1, "df.residual"], " degrees of freedom\n", sep = "")
    }

    if (L3) {
      cat("\nGoodness of fit:\n")
      print(gof)
    }

    cat("\nResiduals", if (L2) "" else " (pooled over curves)", ":\n", sep = "")
    print(round(rbind(raw = resid, deviance = dres), digits))
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
#' @example inst/examples/logLik.MortalityLaw.R
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
#' @example inst/examples/AIC.MortalityLaw.R
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
#' @example inst/examples/deviance.MortalityLaw.R
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
#' @example inst/examples/df.residual.MortalityLaw.R
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
#' @name dispersion.MortalityLaw
#' @example inst/examples/dispersion.R
#' @export
dispersion <- function(object, ...) {
  UseMethod("dispersion")
}

#' @rdname dispersion.MortalityLaw
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
#' @example inst/examples/predict.MortalityLaw.R
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
