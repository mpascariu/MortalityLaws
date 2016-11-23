# S Functions
# Classes and methods for 'MortalityLaw'

#' @keywords internal
#' @export
print.MortalityLaw <- function(x, ...) {
     cat('Model:\n')
     cat(x$model_info,'\n---')
     cat("\nCall:\n")
     print(x$call)
     cat("\nCoefficients:\n")
     print(x$coefficients)
}

#' @keywords internal
#' @export
summary.MortalityLaw <- function(object, ...) {
     cat('Model:\n')
     cat(object$model_info,'\n---')
     cat("\nCall:\n")
     print(object$call)
     cat("\nCoefficients:\n")
     print(round(object$coefficients, 6))
}

#' @keywords internal
#' @export
print.ReadHMD <- function(x, ...){
  cat('Human Mortality Database (www.mortality.org)\n')
  cat('Downloaded by:', x$input$username, '\n')
  cat('Download Date:', x$download.date, '\n')
  cat('Type of data:', x$input$what, '\n')
  cat('Countries included:', x$input$countries, '\n\nData:\n')
  print(headTail(x$data, hlength = 8, tlength = 8))
}


