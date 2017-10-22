# Data in the package.

#' England and Wales demographic data
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
"ahmd"


# #' MortalityLaws: A package for fitting the most important mortality laws

#' @import graphics
#' @import minpack.lm
#' @import RCurl
#' @importFrom utils read.table flush.console head tail
#' @importFrom stats fitted coef optim quantile integrate spline splinefun
#' loess predict
#' @importFrom pbapply startpb closepb setpb
NULL
