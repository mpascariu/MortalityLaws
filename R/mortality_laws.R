#' Fit mortality models
#'
#' This is a description Fit mortality models
#' @param model The name of the mortality model to be fitted.
#' @param mx Matrix containing age-specific death rates (ages x years).
#' Can be also a vector or a data.frame with 1 column containing
#' rates in a single year
#' @param parS Starting parameters used in optimization process
#' @param ... Other argumnets
#' @param x Corresponding ages in the input matrix
#' @return Results
#' @export
#' 
MortalityLaw <- function(mx, x, model, parS = NULL, ...){
  models <- c('demoivre', 'gompertz', 'makeham', 'opperman', 'kannisto',
              'HP','thiele', 'wittstein')
  if ( !(model %in% models)) {
    cat('Error!!! The MODEL NAME is incorrect.\n')
    cat('Check one of the following models: \n',
        models, sep = ' | ')
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
  out <- structure(class = "MortalityLaw", mdl)
  return(out)
}



#---------------------------------------
#' Data preparation function
#' @keywords internal
fun_data_prep <- function(mx, x, n_parameters){
     # Format input data
     c_names <- if (ncol(data.frame(mx)) == 1) 'mx' else colnames(mx)
     mx <- as.matrix(mx)
     x <- as.numeric(x)
     dimnames(mx) <- list(x, c_names)
     c_no    <- ncol(mx)
     mx <- mx + (mx == 0)*1e-04 # If death rate is 0 we assign a very small value
     # Scale the age vectors in order to obtain meaningful parameter estimates
     x_scaled <- x - min(x)
     # Create storage objects for parametes and fitted mx's
     pars <- matrix(NA, c_no, n_parameters)
     dimnames(pars) <- list(c_names, letters[1:n_parameters] )
     fitted.values <- mx*0
     # Output
     return(list(mx = mx, x = x, x_scaled = x_scaled,
                 pars = pars, fitted.values = fitted.values,
                 n_parameters = n_parameters))
}

# --------------------------------------------
#' Select the mortality model
#' 
#' This function calls the mortality model that will be 
#' used in all the calculations
#' @keywords internal
Fun_ux <- function(model){
     switch(model,
        kannisto = function(par, x) with(as.list(par), a*exp(b*x) / (1 + a*exp(b*x)) ),
        demoivre = function(par, x) 1/(par - x) + 1e-10,
        gompertz = function(par, x) with(as.list(par), a*exp(b*x))
     )
}

# --------------------------------------------
#' Fit Kannisto model
#' @keywords internal

kannisto <- function(mx, x, parS, ...){
     all_data <- fun_data_prep(mx, x, n_parameters = 2)
     with(all_data,
          {
          model_name <- "Kannisto (1992): u(x) = a*exp(b*x) / [1 + a*exp(b*x)]"
          parS_default <- c(a = 0.5, b = 0.13)
          parS <- if (is.null(parS)) parS_default else parS 
          if (is.null(names(parS))) names(parS) <- letters[1:length(parS)]
          # Model ------------------------------------------
          fun_ux <- Fun_ux('kannisto')
          # Find parameters / Optimization -----------------
          fun_resid <- function(par, x, ux) {
               sum(ux*log(fun_ux(par, x)) - fun_ux(par, x), na.rm = TRUE)
          }
          for (i in 1:nrow(pars)) {
               opt_i <- optim(par = parS, fn = fun_resid, x = x_scaled,
                              ux = mx[, i], method = 'L-BFGS-B',
                              lower = 1e-15, control = list(fnscale = -1))
               pars[i, ] <- opt_i$par
          }
          # Compute death rates ---------------------------
          for (i in 1:nrow(pars)) fitted.values[, i] = fun_ux(pars[i, ], x_scaled)
          # Retun results ----------------------------------
          return(list(x = x, mx.input = mx, fitted.values = fitted.values,
                      model_name = model_name, coefficients = pars))
          })
     }

# --- DeMoivre model ----------------------------------------
#' @keywords internal
demoivre <- function(mx, x, parS, ...){
     all_data <- fun_data_prep(mx, x, n_parameters = 1)
     with(all_data, 
          {
          model_name = 'DeMoivre (1725): u(x) = 1/(a-x)'
          parS_default <- 100
          parS <- if (is.null(parS)) parS_default else parS 
          # if(is.null(names(parS))) names(parS) <- letters[1:length(parS)]
          # Model ------------------------------------------
          fun_ux <- Fun_ux('demoivre')
          # Find parameters / Optimization -----------------
          fun_resid <- function(par, x, ux){ 
               sum(abs(ux - fun_ux(par, x)), na.rm = TRUE)
          }
          for (i in 1:nrow(pars)) {
               opt_i <- optimize(fun_resid, x = x, ux = mx[, i],
                                 lower = parS - 20, upper = parS + 20, 
                                 maximum = FALSE)
               pars[i, ] <- opt_i$minimum
          }
          # Compute death rates ---------------------------
          for (i in 1:nrow(pars)) fitted.values[, i] = fun_ux(pars[i, ], x)
          # Retun results ----------------------------------
          return(list(x = x, mx.input = mx, fitted.values = fitted.values,
                      model_name = model_name, coefficients = pars))
          })
}

# --- Gompertz model ----------------------------------------
#' @keywords internal
gompertz <- function(mx, x, parS, ...){
     all_data <- fun_data_prep(mx, x, n_parameters = 2)
     with(all_data, 
          {
          model_name <- 'Gompertz (1825): u(x) = a*exp(b*x)'
          parS_default <- c(a = 0.01, b = 0.13)
          parS <- if (is.null(parS)) parS_default else parS 
          if (is.null(names(parS))) names(parS) <- letters[1:length(parS)]
          # Model -----------------------------------------------------
          fun_ux <- Fun_ux('gompertz')
          # Find parameters / Optimization -----------------------------
          fun_resid <- function(par,x,ux){
               sum(abs(ux - fun_ux(par,x)), na.rm = TRUE)
          }
          for (i in 1:nrow(pars)) {
               opt_i <- optim(par = parS, fn = fun_resid, 
                              x = x_scaled, ux = mx[, i], 
                              method = 'Nelder-Mead', gr = NULL)
               pars[i, ] <- opt_i$par
          }
          # Compute death rates ---------------------------
          for (i in 1:nrow(pars)) fitted.values[, i] = fun_ux(pars[i, ], x_scaled)
          # Retun results ----------------------------------
          return(list(x = x, mx.input = mx, fitted.values = fitted.values,
                      model_name = model_name, coefficients = pars))
          })
}

