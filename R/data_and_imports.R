
#' onAttach
#' @param lib lib
#' @param pkg pkg
#' @name onAttach
#' @keywords internal
".onAttach" <- function(lib, pkg){
  packageStartupMessage("\nR Package  : MortalityLaws",
                        "\nName       : Parametric Mortality Models, Life Tables and HMD",
                        "\nAuthor     : Marius D. Pascariu",
                        "\nLast Update: January 11, 2018")
}


#' England and Wales Demographic Data
#'
#' Dataset containing altered death rates (mx), death counts (Dx) 
#' and exposures (Ex) for female population living in 
#' England & Wales in four different years: 1850, 1900, 1950 and 2010. 
#' The data-set is provided for testing purposes only.
#' Download the actual data free of charge from \url{http://www.mortality.org}.
#' Once a username and a password is created on the website the function
#' \code{\link{ReadHMD}} can be used for downloading.
#' 
#' @source \href{http://www.mortality.org}{Human Mortality Database}  
#' @seealso \code{\link{ReadHMD}}
#' @examples head(ahmd$mx) 
"ahmd"


# Imports -----

#' @import graphics 
#' @import minpack.lm
#' @importFrom RCurl getURL
#' @importFrom utils read.table head tail
#' @importFrom stats fitted coef optim predict quantile nlminb pt printCoefmat 
#' df.residual qt vcov
#' @importFrom pbapply startpb closepb setpb
NULL
