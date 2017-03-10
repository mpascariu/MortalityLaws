
#' Function to check input data in MortalityLaw
#' @keywords internal
#' 
check_input <- function(input){
  with(input, 
       {
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
                    'opperman', 'HP', 'thiele', 'wittstein', 'siler')
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





