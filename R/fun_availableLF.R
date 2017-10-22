
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
    matrix(c("L = -[Dx * log(mu) - mu*Ex]", "poissonL",
             "L = -[Dx * log(1 - exp(-mu)) - (Ex - Dx)*mu]  ", "binomialL",
             "L =  [1 - mu/ov]^2", "LF1",
             "L =  log[mu/ov]^2", "LF2",
             "L =  [(ov - mu)^2]/ov", "LF3",
             "L =  [ov - mu]^2", "LF4",
             "L =  [ov - mu] * log[ov/mu]", "LF5",
             "L =  |ov - mu|", "LF6"), ncol = 2, byrow = T))
  colnames(tab) <- c("LOSS FUNCTION", "CODE")
  
  legend <- c("Dx - Death counts", "Ex - Exposure", 
              "mu - Estimated value", "ov - Observed value")
  
  hint <- c("Most of the functions work well with <poissonL>, however for complex",
            "mortality laws like Heligman-Pollard (HP) one can obtain a better fit using",
            "other loss functions (e.g. LF2). You are strongly encouraged to test",
            "different option before deciding on the final version. The results will be",
            "slightly different.")
  
  out <- structure(class = "availableLF", 
                   list(table = tab, legend = legend, hint = hint))
  return(out)
} 



#' @keywords internal
#' @export
print.availableLF <- function(x, ...) {
  cat("\nLoss functions available in the package:\n\n")
  print(x$table, right = F)
  cat("\nLEGEND:\n")
  cat(x$legend, sep = '\n')
  cat("\nHINT:\n")
  cat(x$hint, sep = '\n')
}

