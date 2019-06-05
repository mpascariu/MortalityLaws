# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: MIT
# Last update: Wed Jun 05 14:34:57 2019
# --------------------------------------------------- #

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
#' @name MortalityLaws
#' @aliases NULL
#' @docType package
"_PACKAGE"
