
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

#' @keywords internal
#' 
check.convertFx <- function(input) {
  with(input, {
   if (type == output) warning("type == output. The output is the same as the input!")
   val <- c("mx", "qx", "fx")
   if (!(type %in% val)) stop("<type> argument accepts the following values: ", 
                              paste(val, collapse = ", "), call. = FALSE)
   if (!(output %in% val)) stop("<output> argument accepts the following values: ", 
                                paste(val, collapse = ", "), call. = FALSE)
   if (is.matrix(data) | is.data.frame(data)) {
     if (length(x) != nrow(data)) stop("The number of rows in <data> should be equal with the length of <x>", 
                                       call. = FALSE)
   } else {
     if (length(x) != length(data)) stop("The length of <x> and <data> should be equal.", call. = FALSE)
   }
  })
}

