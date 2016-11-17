# S Functions
# Classes and methods for 'mortality'

#' Fit mortality models
#'
#' This is a description Fit mortality models
#' @param model The name of the mortality model to be fitted.
#' @param mx Matrix containing age-specific death rates (ages x years).
#' Can be also a vector or a data.frame with 1 column containing
#' rates in a single year
#' @param parS Starting parameters used in optimization process
#' @param ... Some more stuff
#' @param x Corresponding ages in the input matrix
#' @return Results
#' @export
Mlaw <- function(mx, x, model, parS, ...) UseMethod("Mlaw")

#' @keywords internal
#' @export
Mlaw.default <- function(mx, x, model, parS = NULL, ...){
     models <- c('demoivre', 'gompertz', 'makeham', 'opperman', 'kannisto',
                 'HP','thiele', 'wittstein')
     if ( !(model %in% models)) {
          cat('ERROR!!! The model name is incorrect. \n 
              Check one of the following models: \n',
              models, sep = ' ; ')
          stop()
     }
     ptm <- proc.time() # Start the clock!
     mdl <- switch(model,
                   demoivre  = demoivre(mx, x, parS, ...),
                   gompertz  = gompertz(mx, x, parS, ...),
                   # makeham   = makeham(mx, x, parS, ...),
                   # opperman  = opperman(mx, x, parS, ...),
                   # thiele    = thiele(mx, x, parS, ...),
                   # wittstein = wittstein(mx, x, parS, ...),
                   # HP        = heligman_pollard(mx, x, parS, ...),
                   kannisto  = kannisto(mx, x, parS, ...)
     )
     mdl$residuals  <- mdl$mx.input - mdl$fitted.values
     proc_speed     <- round((proc.time() - ptm)[3],2) %>% as.numeric()
     proc_speed_txt <- paste('Process completed in ', 
                             proc_speed, 'seconds!') # Stop the clock
     cat('\n', mdl$model_name, '\n', proc_speed_txt, '\n')
     mdl$process_date  <- date()
     mdl$process_speed <- proc_speed_txt
     mdl$model  <- model
     mdl$call   <- match.call()
     class(mdl) <- "Mlaw"
     mdl
}

#' @keywords internal
#' @export
print.Mlaw <- function(x, ...) {
     cat('Model:\n')
     cat(x$model_name,'\n---')
     cat("\nCall:\n")
     print(x$call)
     cat("\nCoefficients:\n")
     print(x$coefficients)
}

#' @keywords internal
#' @export
summary.Mlaw <- function(object, ...) {
     cat('Model:\n')
     cat(object$model_name,'\n---')
     cat("\nCall:\n")
     print(object$call)
     cat("\nCoefficients:\n")
     print(object$coefficients)
}

#' @keywords internal
#' @export
predict.Mlaw <- function(object, newdata=NULL, ...) {
     if(is.null(newdata)){ pred.values <- fitted(object)
     }else{
          x <- newdata
          x_scaled <- x - min(x)
          pars <- coef(object)
          pred.values <- matrix(NA, nrow = length(x), ncol = nrow(pars))
          dimnames(pred.values) <- list(x, rownames(pars))
          fun_ux <- Fun_ux(object$model)
          for(i in 1:nrow(pars)) pred.values[,i] = fun_ux(pars[i,], x_scaled)
     }
     pred.values
}




