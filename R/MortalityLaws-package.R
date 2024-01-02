# -------------------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last Update: Mon Aug  7 21:48:58 2023
# -------------------------------------------------------------- #

# MortalityLaws Package

#' @details
#' To learn more about the package, start with the vignettes:
#' \code{browseVignettes(package = "MortalityLaws")}
#'
#' @importFrom minpack.lm nls.lm nls.lm.control
#' @importFrom RCurl getURL
#' @importFrom tidyr spread
#' @importFrom graphics plot abline axis barplot box hist layout legend lines par rect
#' @importFrom utils read.table read.csv head tail
#' @importFrom stats fitted coef optim predict quantile nlminb pt printCoefmat
#' df.residual qt vcov
#' @importFrom pbapply startpb closepb setpb
#' @import rvest httr
#' @name MortalityLaws
#' @docType package
"_PACKAGE"
