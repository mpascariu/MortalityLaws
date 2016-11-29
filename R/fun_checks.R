
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
        
        models <- c('gompertz', 'makeham', 'kannisto')
        # models <- c('demoivre', 'opperman', 'HP','thiele', 'wittstein')
        if ( !(law %in% models)) {
          cat('Error: mortality law not available.\n')
          cat('Check one of the following models: \n', models, sep = ' | ')
          stop()
        }
        
        function_to_optimize <- c('poissonL', 'binomialL', 'LAE', 'LSE')
        if (!(how %in% function_to_optimize)) {
          cat('Error: Choose a different function to optimize.\n')
          cat('Check one of the following options: \n', 
              function_to_optimize, sep = ' | ')
          stop()
        }
  })
}