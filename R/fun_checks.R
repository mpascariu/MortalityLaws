
#' Function to check input data in MortalityLaw
#' @keywords internal
#' 
check_input <- function(input){
  with(input, 
       {
        if (!is.logical(show_pb)) stop('show_pb should be TRUE or FALSE')
           
        if (!is.null(mx)) {
          if (length(x) != length(mx)) 
            stop('x and mx do not have the same length!')
        }
        
        if (!is.null(Dx)) {
          if (length(x) != length(Dx) | length(x) != length(Ex) ) 
            stop('x, Dx and Ex do not have the same length!')
        }
        
        models <- c('gompertz', 'gompertz0', 'invgompertz',
                    'weibull', 'invweibull', 'carriere1', 'carriere2',
                    'makeham', 'makeham0', 'kannisto', #'demoivre', 
                    'opperman', 'HP', 'HP2', 'HP3', 'HP4',
                    'thiele', 'wittstein', 'siler', 'custom.law')
        if ( !(law %in% models)) {
          m1 <- 'Mortality law not available\n'
          m2 <- 'Check one of the following models:\n'
          err1 <- paste(m1, m2, paste(models, collapse = ', '))
          stop(err1, call. = FALSE)
        }
        
        function_to_optimize <- c('poissonL', 'binomialL', 
                                  'LF1', 'LF2', 'LF3', 'LF4', 'LF5', 'LF6')
        if (!(how %in% function_to_optimize)) {
          m1 <- 'Choose a different objective function to optimize\n'
          m2 <- 'Check one of the following options:\n'
          err2 <- paste(m1, m2, paste(function_to_optimize, collapse = ', '))
          stop(err2, call. = FALSE)
        }
  })
}


#' Check available loss function 
#' 
#' The function returns information about the implemented loss function used by the 
#' optimization procedure in \code{\link{MortalityLaw}} function. 
#' @return An \code{availableLF} object.
#' @examples 
#' 
#' availableLF()
#' 
#' @export
availableLF <- function(){
  tab <- as.data.frame(
  matrix(c("fx = -(Dx * log(mu) - mu*Ex)", "poissonL",
           "fx = -(Dx * log(1 - exp(-mu)) - (Ex - Dx)*mu)  ", "binomialL",
           "fx =  (1 - mu/ov)^2", "LF1",
           "fx =  log(mu/ov)^2", "LF2",
           "fx =  ((ov - mu)^2)/ov", "LF3",
           "fx =  (ov - mu)^2", "LF4",
           "fx =  (ov - mu) * log(ov/mu)", "LF5",
           "fx =  abs(ov - mu)", "LF6"), ncol = 2, byrow = T))
  colnames(tab) <- c("LOSS FUNCTION", "CODE")
  
  print(tab, right = FALSE)
  
  cat("\nLEGEND:\n")
  cat(" Dx - Death counts\n")
  cat(" Ex - Exposure\n")
  cat(" mu - Estimated value\n")
  cat(" ov - Observed value\n")
  
  cat("\nHINT:")
  cat("\n Most of the functions work well with <poissonL>, however for complex\n",
      "mortality laws like Heligman-Pollard (HP) one can obtain a better fit using\n",
      "other loss function (e.g. LF2). You are strongly encouraged to test\n",
      "different option before deciding on the final version. The results will be\n",
      "slightly different.\n")
} 




