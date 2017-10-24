
#' Check Available Loss Function 
#' 
#' The function returns information about the implemented loss function used by the 
#' optimization procedure in \code{\link{MortalityLaw}} function. 
#' @return A list of class \code{availableLF} with the components:
#' @return \item{table}{ table with loss functions and codes to be used in \code{\link{MortalityLaw}}}
#' @return \item{legend}{ table with details about the used abbreviation}
#' @examples 
#' 
#' availableLF()
#' 
#' @export
availableLF <- function(){
  tab <- as.data.frame(
    matrix(c("L = -[Dx * log(mu) - mu*Ex]", "poissonL",
             "L = -[Dx * log(1 - exp(-mu)) - (Ex - Dx)*mu]  ", "binomialL",
             "L =  [1 - mu/ov]^2", "LF1",
             "L =  log[mu/ov]^2", "LF2",
             "L =  [(ov - mu)^2]/ov", "LF3",
             "L =  [ov - mu]^2", "LF4",
             "L =  [ov - mu] * log[ov/mu]", "LF5",
             "L =  abs(ov - mu)", "LF6"), ncol = 2, byrow = T))
  colnames(tab) <- c("LOSS FUNCTION", "CODE")
  
  legend <- c("Dx: Death counts", 
              "Ex: Population exposed to risk", 
              "mu: Estimated value", 
              "ov: Observed value")
  
  out <- structure(class = "availableLF", list(table = tab, legend = legend))
  return(out)
} 

#' Print availableLF
#' @param x an object of class \code{"availableLF"}
#' @param ... further arguments passed to or from other methods.
#' @keywords internal
#' @export
print.availableLF <- function(x, ...) {
  cat("\nLoss functions available in the package:\n\n")
  print(x$table, right = FALSE, row.names = FALSE)
  cat("\nLEGEND:\n")
  cat(x$legend, sep = '\n')
  
  message("\nHINT: Most of the functions work well with <poissonL>, however for complex\n",
          "mortality laws like Heligman-Pollard (HP) one can obtain a better fit using\n",
          "other loss functions (e.g. LF2). You are strongly encouraged to test\n",
          "different option before deciding on the final version. The results will be\n",
          "slightly different.\n")
}


