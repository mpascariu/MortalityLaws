# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-04 17:46:32
# --------------------------------------------

#' Check one data vector for missing, infinite or negative values
#'
#' Validates a single user-supplied input (\code{Dx}, \code{Ex}, \code{mx} or
#' \code{qx}) before the fitting routine sees it.
#' @param value A numeric vector supplied to \code{\link{MortalityLaw}}.
#' @param name The name of the input, used in the error message.
#' @param positive Logical; if \code{TRUE} the values must be strictly positive.
#' @return No return value, called for side effects
#' @noRd
check_values <- function(value, name, positive = FALSE) {
  if (!is.numeric(value)) {
    stop(paste0("'", name, "' must be a numeric vector."), call. = FALSE)
  }

  if (any(!is.finite(value))) {
    stop(paste0("'", name, "' contains missing or infinite values."),
         call. = FALSE)
  }

  if (positive && any(value <= 0)) {
    stop(paste0("'", name, "' must contain strictly positive values."),
         call. = FALSE)
  }

  if (!positive && any(value < 0)) {
    stop(paste0("'", name, "' must not contain negative values."),
         call. = FALSE)
  }

  return(invisible(NULL))
}


#' Check the data vectors against the age vector
#'
#' Verifies that the supplied data have the same length as the age vector
#' \code{x} and that their values pass \code{check_values}. Every input must be
#' non-negative; ages with zero exposure are left out of the fit, not rejected.
#' @inheritParams MortalityLaw
#' @return No return value, called for side effects
#' @noRd
check_input_data <- function(x, Dx, Ex, mx, qx) {
  if (!is.null(mx)) {
    if (length(x) != length(mx)) {
      stop('x and mx do not have the same length!', call. = FALSE)
    }

    check_values(value = mx, name = "mx")
  }

  if (!is.null(qx)) {
    if (length(x) != length(qx)) {
      stop('x and qx do not have the same length!', call. = FALSE)
    }

    check_values(value = qx, name = "qx")
  }

  if (!is.null(Dx)) {
    if (length(x) != length(Dx) | length(x) != length(Ex)) {
      stop('x, Dx and Ex do not have the same length!', call. = FALSE)
    }

    check_values(value = Dx, name = "Dx")
    check_values(value = Ex, name = "Ex")
  }

  return(invisible(NULL))
}


#' Function to check input data in MortalityLaw
#'
#' Validates the user input before fitting: \code{x} must be numeric, finite,
#' non-negative and unique, the data vectors must match it in length, and no
#' value may be missing, infinite or out of range.
#' @param input A list of input arguments to \code{\link{MortalityLaw}}.
#' @return No return value, called for side effects
#' @noRd
check_mortality_law_input <- function(input){
  with(input,
       {
         # Errors ---
         if (!is.logical(show)) {
           stop("'show' should be TRUE or FALSE", call. = FALSE)
         }

         if (!is.numeric(x)) {
           stop("'x' must be a numeric vector.", call. = FALSE)
         }

         if (any(!is.finite(x))) {
           stop("'x' contains missing or infinite values.", call. = FALSE)
         }

         if (any(x < 0)) {
           stop("'x' must not contain negative values.", call. = FALSE)
         }

         if (anyDuplicated(x)) {
           stop("'x' must not contain duplicated ages.", call. = FALSE)
         }

         check_input_data(
           x  = x,
           Dx = Dx,
           Ex = Ex,
           mx = mx,
           qx = qx
           )

         function_to_optimize <- availableLF()$table[, 'CODE']

         if (!(opt.method %in% function_to_optimize)) {
           m1 <- 'Choose a different objective function to optimize\n'
           m2 <- 'Check one of the following options:\n'
           err2 <- paste(m1, m2, paste(function_to_optimize, collapse = ', '))
           stop(err2, call. = FALSE)
         }

         if (length(fit.this.x) < 2) {
           stop(paste("More observations needed in order to start the fitting.",
                      "Increase the length of 'fit.this.x'"), call. = FALSE)
         }

         if (!all(fit.this.x %in% x)) {
           stop("'fit.this.x' should be a subset of 'x'", call. = FALSE)
         }

         # Messages ---
         if (law %in% c('HP', 'HP2', 'HP3', 'HP4', 'kostaki') & opt.method != "LF2") {
           message("\nFor models like ", law, ", the optimisation method 'LF2'",
                   " has been observed to return reliable estimates.")
         }
       })

  return(invisible(NULL))
}
