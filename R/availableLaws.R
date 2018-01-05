#' Check Available Mortality Laws
#' 
#' The function returns information about the parametric models that can be called and fitted
#' in \code{\link{MortalityLaw}} function. For a comprehensive review of the 
#' most important mortality laws Tabeau (2001) is a good starting point.
#' 
#' @param law Optional. Default: \code{NULL}. One can subtract details about 
#' a certain model by specifying its codename.
#' @return The output is of \code{"availableLaws"} class with the components:
#' @return \item{table}{Table with mortality models and codes to be used in \code{\link{MortalityLaw}}}
#' @return \item{legend}{Table with details about the section of the mortality curve }
#' @references 
#' \enumerate{
#' \item{Gompertz, B. (1825). \href{http://www.jstor.org/stable/107756}{On the 
#' Nature of the Function Expressive of the Law of Human Mortality, and on a 
#' New Mode of Determining the Value of Life Contingencies.} 
#' Philosophical Transactions of the Royal Society of London, 115, 513-583.}
#' \item{Makeham, W. (1860). \href{https://doi.org/10.1017/S204616580000126X}{
#' On the Law of Mortality and Construction of Annuity Tables.} 
#' The Assurance Magazine and Journal of the Institute of Actuaries, 8(6), 301-310.}
#' \item{Thiele, T. (1871). \href{https://doi.org/10.1017/S2046167400043688}{
#' On a Mathematical Formula to express the 
#' Rate of Mortality throughout the whole of Life, tested by a Series of 
#' Observations made use of by the Danish Life Insurance Company of 1871.} 
#' Journal of the Institute of Actuaries and Assurance Magazine, 16(5), 313-329.}
#' \item{Wittstein, T. (1883). \href{https://www.cambridge.org/core/journals/journal-of-the-institute-of-actuaries/article/the-mathematical-law-of-mortality/57A7403B578C84769A463EA2BC2F7ECD}{
#' The Mathematical Law of Mortality.} 
#' Journal of the Institute of Actuaries and Assurance Magazine, 24(3), 153-173.}
#' \item{Perks, W. (1932). \href{https://doi.org/10.1017/S0020268100046680}{
#' On Some Experiments in the Graduation of Mortality Statistics.} 
#' Journal of the Institute of Actuaries, 63(1), 12-57.}
#' \item{Siler, W. (1979), \href{https://doi.org/10.2307/1936612}{
#' A Competing-Risk Model for Animal Mortality.} Ecology, 60: 750-757.}
#' \item{Heligman, L., & Pollard, J. (1980). 
#' \href{https://doi.org/10.1017/S0020268100040257}{The age pattern of mortality.}
#' Journal of the Institute of Actuaries, 107(1), 49-80.}
#' \item{Rogers A & Planck F (1983). \href{http://pure.iiasa.ac.at/2210/}{
#' MODEL: A General Program for Estimating 
#' Parametrized Model Schedules of Fertility, Mortality, Migration, and Marital 
#' and Labor Force Status Transitions.} 
#' IIASA Working Paper. IIASA, Laxenburg, Austria: WP-83-102}
#' \item{Martinelle S. (1987). A generalized Perks formula for old-age mortality.
#' Stockholm, Sweden, Statistiska Centralbyran, 1987. 55 p. 
#' (R and D Report, Research-Methods-Development, U/STM No. 38)}
#' \item{Carriere J.F. (1992). Parametric models for life tables. 
#' Transactions of the Society of Actuaries. Vol.44}
#' \item{Kostaki A. (1992). \href{http://dx.doi.org/10.1080/08898489209525346}{
#' A nine-parameter version of the Heligman-Pollard formula}. 
#' Mathematical Population Studies. Vol. 3 277-288}
#' \item{Thatcher AR, Kannisto V and Vaupel JW (1998). 
#' The force of mortality at ages 80 to 120. Odense Monographs on Population Aging Vol. 5
#' Odense University Press, 1998. 104, 20 p. Odense, Denmark}
#' \item{Tabeau E. (2001) \href{https://doi.org/10.1007/0-306-47562-6_1}{
#' A Review of Demographic Forecasting Models for Mortality.}
#' In: Tabeau E., van den Berg Jeths A., Heathcote C. (eds) 
#' Forecasting Mortality in Developed Countries. 
#' European Studies of Population, vol 9. Springer, Dordrecht}
#' }
#' @examples 
#' 
#' availableLaws()
#' 
#' @export
availableLaws <- function(law = NULL){
  if (is.null(law)) {
    table <- as.data.frame(matrix(ncol = 6, byrow = T,
                                  c(1825, 'Gompertz', 'mu[x] = A exp[Bx]', 3, 'gompertz', 'mu[x]',
                                    NaN, 'Gompertz', 'mu[x] = 1/sigma * exp[(x-M)/sigma)]', 3, 'gompertz0', 'mu[x]',
                                    NaN, 'Inverse-Gompertz', 'mu[x] = [1- exp(-(x-M)/sigma)] / [exp(-(x-M)/sigma) - 1]', 2, 'invgompertz', 'mu[x]',
                                    1860, 'Makeham', 'mu[x] = A exp[Bx] + C', 3, 'makeham', 'mu[x]',
                                    NaN, 'Makeham', 'mu[x] = 1/sigma * exp[(x-M)/sigma)] + C', 3, 'makeham0', 'mu[x]',
                                    1870, 'Opperman', 'mu[x] = A/sqrt(x) - B + C*sqrt(x)', 1, 'opperman', 'mu[x]',
                                    1871, 'Thiele', 'mu[x] = A exp(-Bx) + C exp[-.5D (x-E)^2] + F exp(Gx)', 6, 'thiele', 'mu[x]',
                                    1883, 'Wittstein', 'q[x] = (1/B) A^-[(Bx)^N] + A^-[(M-x)^N]', 6, 'wittstein', 'q[x]',
                                    1932, 'Perks', 'mu[x] = [A + BC^x] / [BC^-x + 1 + DC^x]', 3, 'perks', 'mu[x]',
                                    1939, 'Weibull', 'mu[x] = 1/sigma * (x/M)^(M/sigma - 1)', 1, 'weibull', 'mu[x]',
                                    NaN, 'Inverse-Weibull', 'mu[x] = 1/sigma * (x/M)^[-M/sigma - 1] / [exp((x/M)^(-M/sigma)) - 1]', 2, 'invweibull', 'mu[x]', 
                                    1943, 'Van der Maen', 'mu[x] = A + Bx + Cx^2 + I/[N - x]', 4, 'vandermaen', 'mu[x]',
                                    1943, 'Van der Maen', 'mu[x] = A + Bx + I/[N - x]', 5, 'vandermaen2', 'mu[x]',
                                    NaN, 'Quadratic', 'mu[x] = A + Bx + Cx^2', 5, 'quadratic', 'mu[x]',
                                    1961, 'Beard', 'mu[x] = A exp(Bx) / [1 + KA exp(Bx)]', 4, 'beard', 'mu[x]',
                                    1961, 'Makeham-Beard', 'mu[x] = A exp(B^x) / [1 + KA exp(B^x)] + C', 4, 'makehambeard', 'mu[x]',
                                    1979, 'Siler', 'mu[x] = A exp(-Bx) + C + D exp(Ex)', 6, 'siler', 'mu[x]',
                                    1980, 'Heligman-Pollard', 'q[x]/p[x] = A^[(x + B)^C] + D exp[-E log(x/F)^2] + G H^x', 6, 'HP', 'q[x]',
                                    1980, 'Heligman-Pollard', 'q[x] = A^[(x + B)^C] + D exp[-E log(x/F)^2] + GH^x / [1 + GH^x]', 6, 'HP2', 'q[x]',
                                    1980, 'Heligman-Pollard', 'q[x] = A^[(x + B)^C] + D exp[-E log(x/F)^2] + GH^x / [1 + KGH^x]', 6, 'HP3', 'q[x]',
                                    1980, 'Heligman-Pollard', 'q[x] = A^[(x + B)^C] + D exp[-E log(x/F)^2] + GH^(x^K) / [1 + GH^(x^K)]', 6, 'HP4', 'q[x]',
                                    1983, 'Rogers-Planck', 'q[x] = A0 + A1 exp[-Ax] + A2 exp[B(x - u) - exp(-C(x - u))] + A3 exp[Dx]', 6, 'rogersplanck', 'q[x]',
                                    1987, 'Martinelle', 'mu[x] = [A exp(Bx) + C] / [1 + D exp(Bx)] + K exp(Bx)', 6, 'martinelle', 'mu[x]',
                                    1992, 'Carriere', 'l[x] = P1 l[x](weibull) + P2 l[x](invweibull) + P3 l[x](gompertz)', 6, 'carriere1', 'q[x]',
                                    1992, 'Carriere', 'l[x] = P1 l[x](weibull) + P2 l[x](invgompertz) + P3 l[x](gompertz)', 6, 'carriere2', 'q[x]',
                                    1992, 'Kostaki', 'q[x]/p[x] = A^[(x+B)^C] + D exp[-(E_i log(x/F_))^2] + G H^x', 6, 'kostaki', 'q[x]',
                                    1998, 'Kannisto', 'mu[x] = A exp(Bx) / [1 + A exp(Bx)]', 5, 'kannisto', 'mu[x]'
                                  )))
    colnames(table) <- c('YEAR', 'NAME', 'MODEL', 'TYPE', 'CODE', 'FIT')
    
    legend <- as.data.frame(matrix(ncol = 2, byrow = T, 
                                   c(1, "Infant mortality",
                                     2, "Accident hump",
                                     3, "Adult mortality",
                                     4, "Adult and/or old-age mortality",
                                     5, "Old-age mortality",
                                     6, "Full age range")))
    colnames(legend) <- c("TYPE", "Coverage")
  }
  
  if (!is.null(law)) {
    A <- availableLaws()
    if (!(law %in% A$table$CODE)) stop("The specified 'law' is not available.",
                                       "Run 'availableLaws()' to see the available models.", call. = F)
    table <- A$table[A$table$CODE %in% law, ]
    legend <- A$legend[A$legend$TYPE %in% unique(table$TYPE), ]
  }
  
  out <- structure(class = "availableLaws", list(table = table, legend = legend))
  return(out)
}


#' Print availableLaws
#' @param x An object of class \code{"availableLaws"}
#' @param ... Further arguments passed to or from other methods.
#' @keywords internal
#' @export
print.availableLaws <- function(x, ...) {
  cat("\nMortality laws available in the package:\n\n")
  print(x$table[, 1:5], right = FALSE, row.names = FALSE)
  cat("\nLEGEND:\n")
  print(x$legend, right = FALSE, row.names = FALSE)
}
