
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
#' @param digits number of digits to display.
#' @param ... additional arguments affecting the summary produced.
#' @keywords internal
#' @export
summary.MortalityLaw <- function(object, ..., 
                                 digits = max(3L, getOption("digits") - 3L)) {
  x     <- object
  L1    <- x$input$law == "custom.law"
  mi    <- if (L1) "Custom Mortality Law" else as.matrix(x$info$model.info[, c(2, 3)])
  res   <- summary(as.vector(as.matrix(x$residuals)))
  fv    <- ifelse(!is.null(x$input$qx), 'qx', 'mx')
  gof   <- round(x$goodness.of.fit, digits)
  param <- round(coef(x), digits)
  sigma <- mean(sqrt(x$deviance))
  nc    <- nrow(param)
  L2    <- is.null(nc)
  L3    <- x$input$opt.method %in% c("poissonL", "binomialL")
  
  if (!L2) {
    if (nc > 4) {
      param <- head_tail(param, hlength = 2, tlength = 2, digits = digits)
      gof   <- head_tail(gof, hlength = 2, tlength = 2, digits = digits)
    }
  }
  out  <- list(info = mi, call = x$info$call, gof = gof, sigma = sigma,
               fv = fv, resid = res, param = param, df = x$df, digits = digits,
               L1 = L1, L2 = L2, L3 = L3)
  out  <- structure(class = "summary.MortalityLaw", out)
  return(out)
}


#' Print summary.MortalityLaw
#' @param x an object of class \code{"summary.MortalityLaw"}
#' @param ... additional arguments affecting the summary produced.
#' @keywords internal
#' @export
print.summary.MortalityLaw <- function(x, ...) {
  with(x, {
    cat(paste(info, collapse = " model: "))
    cat("\nFitted values:", fv)
    cat("\n\nCall: ")
    print(call)
    cat('\nDeviance Residuals:\n')
    print(round(resid, digits))
    cat("\nParameters:\n")
    print(param)
    sg <- format(signif(sigma, digits))
    if (L3) {
      cat("\nGoodness of fit:\n")
      print(gof)
    }
    if (L2) {
      cat("\nResidual standard error:", sg, "on", df[2], "degrees of freedom")
    } else {
      cat("\nAverage residual standard error:", sg, "on", df[1, 2], "degrees of freedom")
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
  object$deviance
}

#' df.residual function for MortalityLaw
#' @inheritParams print.MortalityLaw
#' @keywords internal
#' @export
df.residual.MortalityLaw <- function(object, ...) {
  object$df[2]
}


#' Predict function for MortalityLaw
#' @param object An object of class \code{"MortalityLaw"}
#' @param x Vector of ages to be considered in prediction
#' @param ... Additional arguments affecting the predictions produced.
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
  if (min(x) < 0) stop("'x' must be greater or equal to zero.", call. = F)
  law   <- object$input$law
  sx    <- object$input$scale.x
  new.x <- x
  
  if (sx) {
    fit.this.x <- object$input$fit.this.x
    d <- fit.this.x[1] - scale_x(fit.this.x)[1]
    new.x <- x - d
  }
  
  Par <- coef(object)
  
  if (!is.matrix(Par)) {
    Par <- matrix(Par, nrow = 1, dimnames = list("", names(Par)))
  }
  
  fn <- if (law == "custom.law") object$input$custom.law else get(law)
  hx <- apply(X = Par, 1, FUN = function(X) fn(x = new.x, par = X)$hx)
  rownames(hx) <- x
  
  if (ncol(hx) == 1) {
    hx <- as.numeric(hx)
    names(hx) <- x
  }
  return(hx)
}


