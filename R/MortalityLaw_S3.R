# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-04 23:32:19
# --------------------------------------------

#' Print MortalityLaw
#' @param x an object of class \code{"MortalityLaw"}
#' @param ... further arguments passed to or from other methods.
#' @return Print data on console
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


#' Summary MortalityLaw
#' @param object an object of class \code{"MortalityLaw"}
#' @param digits number of digits to display.
#' @param ... additional arguments affecting the summary produced.
#' @return A list of model diagnostics
#' @keywords internal
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


#' Print summary.MortalityLaw
#' @param x an object of class \code{"summary.MortalityLaw"}
#' @param ... additional arguments affecting the summary produced.
#' @return Print data on console
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


#' logLik function for MortalityLaw
#' @param object an object of class \code{"MortalityLaw"}
#' @param ... further arguments passed to or from other methods.
#' @return Model log-likelihood value
#' @keywords internal
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

#' AIC function for MortalityLaw
#' @param object an object of class \code{"MortalityLaw"}
#' @param ... further arguments passed to or from other methods.
#' @return model AIC value
#' @keywords internal
#' @export
AIC.MortalityLaw <- function(object, ...) {
  gof <- object$goodness.of.fit
  out <- if (is.matrix(gof)) gof[, "AIC"] else gof["AIC"]

  return(out)
}

#' deviance function for MortalityLaw
#' @param object an object of class \code{"MortalityLaw"}
#' @param ... further arguments passed to or from other methods.
#' @return model deviance value
#' @keywords internal
#' @export
deviance.MortalityLaw <- function(object, ...) {
  out <- object$deviance

  return(out)
}

#' df.residual function for MortalityLaw
#' @param object an object of class \code{"MortalityLaw"}
#' @param ... further arguments passed to or from other methods.
#' @return model residual value
#' @keywords internal
#' @export
df.residual.MortalityLaw <- function(object, ...) {
  df_all <- object$df

  out <- if (is.matrix(df_all)) df_all[, "df.residual"] else df_all["df.residual"]

  return(out)
}


#' dispersion function for MortalityLaw
#'
#' Returns the dispersion of the fit. For the count cases it is the Pearson
#' chi-square divided by the residual degrees of freedom (1 for a correctly
#' specified Poisson model); for the rate cases it is the mean squared
#' log-residual.
#' @param object an object of class \code{"MortalityLaw"}
#' @param ... further arguments passed to or from other methods.
#' @return The dispersion of the fit (a scalar for a single fit, a named
#' vector for multiple fits).
#' @keywords internal
#' @export
dispersion <- function(object, ...) {
  UseMethod("dispersion")
}

#' @rdname dispersion
#' @export
dispersion.MortalityLaw <- function(object, ...) {
  return(object$dispersion)
}


#' Predict function for MortalityLaw
#' @param object An object of class \code{"MortalityLaw"}
#' @param x Vector of ages to be considered in prediction
#' @param ... Additional arguments affecting the predictions produced.
#' @return A vector (single fit) or matrix (one column per fit) of predicted
#' mortality values: hazard rates \code{mu[x]} or death probabilities
#' \code{q[x]} depending on the law (see the \code{FIT} column of
#' \code{\link{availableLaws}}).
#' @seealso \code{\link{MortalityLaw}}
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
