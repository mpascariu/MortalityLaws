
#' MortalityLaws: Parametric Mortality Models, Life Tables and HMD
#' 
#' @description Fit the most popular human mortality "laws", and construct 
#' full and abridge life tables given various input indices. A mortality
#' law is a parametric function that describes the dying-out process of 
#' individuals in a population during a significant portion of their 
#' life spans. For a comprehensive review of the most important mortality 
#' laws see Tabeau (\href{https://doi.org/10.1007/0-306-47562-6_1}{2001}).
#' An elegant function for downloading data from 
#' \href{https://www.mortality.org}{Human Mortality Database} is provided as well.
#' 
#' To learn more about the package, start with the vignettes: 
#' browseVignettes(package = "MortalityLaws")
#' 
#' @import graphics 
#' @import minpack.lm
#' @importFrom RCurl getURL
#' @importFrom utils read.table head tail
#' @importFrom stats fitted coef optim predict quantile nlminb pt printCoefmat 
#' df.residual qt vcov
#' @importFrom pbapply startpb closepb setpb
#' @importFrom tidyr spread
#' @author Marius D. Pascariu <rpascariu@@outlook.com>
#' @name MortalityLaws-package
#' @docType package
NULL