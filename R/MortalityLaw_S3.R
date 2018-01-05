
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
  x     <- object
  L1    <- x$input$law == "custom.law"
  mi    <- if (L1) "Custom Mortality Law" else as.matrix(x$info$model.info[, c(2, 3)])
  res   <- summary(as.vector(as.matrix(x$residuals)))
  fv    <- ifelse(!is.null(x$input$qx), 'qx', 'mx')
  gof   <- x$goodness.of.fit
  param <- coef(x)
  pnames<- names(param)
  sigma <- mean(sqrt(x$deviance))
  
  nc <- nrow(param)
  L2 <- is.null(nc)
  L3 <- x$input$opt.method %in% c("poissonL", "binomialL")
  
  if (!L2) {
    if (nc > 4) {
      param <- head_tail(param, hlength = 2, tlength = 2)
      gof   <- head_tail(gof, hlength = 2, tlength = 2)
    }
  }
  out  <- list(info = mi, call = x$info$call, gof = gof, sigma = sigma,
               fv = fv, resid = res, param = param, df = x$df, 
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
    print(round(param, digits))
    if (L3) {
      cat("\nlogLik:", formatC(gof["logLik"], digits))
      cat("\tAIC:", formatC(gof["AIC"], digits))
      cat("\tBIC:", formatC(gof["BIC"], digits), "\n")
    }
    cat('\nDeviance Residuals:\n')
    print(round(resid, digits))
    sg <- format(signif(sigma, digits))
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
  Par <- if (is.matrix(Par)) Par else matrix(Par, nrow = 1, dimnames = list("", pn))
  M   <- if (law == "custom.law") object$input$custom.law else get(law)
  hx  <- apply(X = Par, 1, FUN = function(X) M(x = x_, par = X)$hx)
  rownames(hx) <- x
  
  if (ncol(hx) == 1) {
    hx <- as.numeric(hx)
    names(hx) <- x
  }
  return(hx)
}


