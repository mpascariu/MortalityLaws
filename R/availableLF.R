# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-04 17:46:32
# --------------------------------------------

#' Check Available Loss Functions
#'
#' Returns information about the loss functions implemented for use with the
#' optimisation procedure in the \code{\link{MortalityLaw}} function.
#'
#' The two likelihoods (\code{"poissonL"}, \code{"binomialL"}) are the only
#' objectives that yield a log-likelihood, an AIC and a BIC; the six loss
#' functions (\code{"LF1"} to \code{"LF6"}) leave those measures undefined
#' (\code{NaN}) and are compared on the deviance or the loss itself.
#' \code{"LF2"}, the squared log-ratio, is the default: it is scale-free and
#' robust, and it has been observed to return reliable estimates for the
#' high-parameter laws such as Heligman-Pollard. There is no universally best
#' choice, so it is worth trying more than one.
#' @return A list of class \code{availableLF} with the components:
#'  \item{table}{Table with loss functions and codes to be used in \code{\link{MortalityLaw}}.}
#'  \item{legend}{Table with details about the abbreviation used.}
#' @seealso \code{\link{MortalityLaw}}
#' @author Marius D. Pascariu
#' @examples
#' availableLF()
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


#' Print Available Loss Functions
#'
#' Prints the table of loss functions and their codes, with the legend that
#' explains the notation, followed by a short note on choosing between them.
#' @param x An object of class \code{"availableLF"}.
#' @param ... Further arguments passed to or from other methods.
#' @return The object \code{x}, invisibly. Called for its printed output.
#' @keywords internal
#' @export
print.availableLF <- function(x, ...) {
  cat("\nLoss functions available in the package:\n\n")
  print(x$table, right = FALSE, row.names = FALSE)
  cat("\nLEGEND:\n")
  cat(x$legend, sep = '\n')

  message("\nHINT: Most loss functions work well with 'poissonL'. However, for complex ",
          "mortality laws like Heligman-Pollard (HP), a better fit can be obtained using ",
          "other loss functions (e.g. 'LF2'). You are strongly encouraged to test ",
          "different options before deciding on the final version. The results might be ",
          "slightly different.\n")
}


