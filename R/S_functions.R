# S Functions
# Classes and methods for 'MortalityLaw'

#' @keywords internal
#' @export
print.MortalityLaw <- function(x, ...) {
     cat('Model:\n')
     cat(x$model_name,'\n---')
     cat("\nCall:\n")
     print(x$call)
     cat("\nCoefficients:\n")
     print(x$coefficients)
}

#' @keywords internal
#' @export
summary.MortalityLaw <- function(object, ...) {
     cat('Model:\n')
     cat(object$model_name,'\n---')
     cat("\nCall:\n")
     print(object$call)
     cat("\nCoefficients:\n")
     print(object$coefficients)
}

#' @keywords internal
#' @export
predict.MortalityLaw <- function(object, newdata=NULL, ...) {
     if (is.null(newdata)) { pred.values <- fitted(object)
     }else{
          x <- newdata
          x_scaled <- x - min(x)
          pars <- coef(object)
          pred.values <- matrix(NA, nrow = length(x), ncol = nrow(pars))
          dimnames(pred.values) <- list(x, rownames(pars))
          fun_ux <- Fun_ux(object$model)
          for (i in 1:nrow(pars)) pred.values[,i] = fun_ux(pars[i,], x_scaled)
     }
     pred.values
}




