
#' Function to check input data in MortalityLaw
#' @inheritParams choose_optim
#' @keywords internal
#' 
check.MortalityLaw <- function(input){
  with(input, 
       {
         # Errors ---
         if (!is.logical(show)) stop("'show' should be TRUE or FALSE")
         
         if (!is.null(mx)) {
           if (length(x) != length(mx)) 
             stop('x and mx do not have the same length!', call. = FALSE)
         }
         
         if (!is.null(Dx)) {
           if (length(x) != length(Dx) | length(x) != length(Ex) ) 
             stop('x, Dx and Ex do not have the same length!', call. = FALSE)
         }
         
         function_to_optimize <- availableLF()$table[, 'CODE']
         if (!(opt.method %in% function_to_optimize)) {
           m1 <- 'Choose a different objective function to optimize\n'
           m2 <- 'Check one of the following options:\n'
           err2 <- paste(m1, m2, paste(function_to_optimize, collapse = ', '))
           stop(err2, call. = FALSE)
         }
         if (length(fit.this.x) < 2) {
           stop(paste("More observations needed in order to start the fitting.",
                      "Increase the length of 'fit.this.x'"), call. = F)
         }
         if (!all(fit.this.x %in% x)) {
           stop("'fit.this.x' should be a subset of 'x'", call. = F)
         }
         
         # Messages ---
         if (law %in% c('HP', 'HP2', 'HP3', 'HP4', 'kostaki') & opt.method != "LF2") {
           message("\nFor cases like", law, "the optimization method 'LF2'",
                   "has been observed to return reliable estimates.")
         }
       })
}
