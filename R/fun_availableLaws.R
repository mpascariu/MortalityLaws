
#' Check available mortality laws
#' 
#' The function returns information about the implemented parametric functions 
#' in \code{\link{MortalityLaw}} function.
#' @param law Default: \code{NULL}. One can substract details about a certain model
#' by specifing its code.
#' @return An \code{availableLaws} object.
#' @references 
#' \enumerate{
#' \item{Gompertz, B. (1825). \href{http://www.jstor.org/stable/107756}{On the 
#' Nature of the Function Expressive of the Law of Human Mortality, and on a 
#' New Mode of Determining the Value of Life Contingencies.} 
#' Philosophical Transactions of the Royal Society of London, 115, 513-583.}
#' \item{Heligman, L., & Pollard, J. (1980). 
#' \href{https://doi.org/10.1017/S0020268100040257}{The age pattern of mortality.}
#' Journal of the Institute of Actuaries, 107(1), 49-80.}
#' \item{...}
#' \item{...}
#' }
#' @examples 
#' 
#' availableLaws()
#' 
#' @export
availableLaws <- function(law = NULL){
  if (is.null(law)) {
    table <- as.data.frame(matrix(ncol = 6, byrow = T,
                                  c(1825, 'Gompertz', 'mu[x] = a*exp[b*x]', 3, 'gompertz', 'mu[x]',
                                    NaN, 'Gompertz', 'mu[x] = 1/sigma * exp[(x-m)/sigma)]', 3, 'gompertz0', 'mu[x]',
                                    NaN, 'Inverse-Gompertz', 'mu[x] = [1- exp(-(x-m)/sigma)] / [exp(-(x-m)/sigma) - 1]', 2, 'invgompertz', 'mu[x]',
                                    1860, 'Makeham', 'mu[x] = a*exp[b*x] + c', 3, 'makeham', 'mu[x]',
                                    NaN, 'Makeham', 'mu[x] = 1/sigma * exp[(x-m)/sigma)] + c', 3, 'makeham0', 'mu[x]',
                                    1870, 'Opperman', 'mu[x] = a/sqrt(x) - b + c*sqrt(x)', 1, 'opperman', 'mu[x]',
                                    1871, 'Thiele', 'mu[x] = a*exp(-b*x) + c*exp[-.5d*(x-e)^2] + f*exp(g*x)', 6, 'thiele', 'mu[x]',
                                    1883, 'Wittstein', 'q[x] = (1/m)*a^-[(m*x)^n] + a^-[(M-x)^n]', 6, 'wittstein', 'q[x]',
                                    1932, 'Perks', 'mu[x] = [A + B*C^x] / [B*C^-x + 1 + D*C^x]', 3, 'perks', 'mu[x]',
                                    1939, 'Weibull', 'mu[x] = 1/sigma * (x/m)^(m/sigma - 1)', 1, 'weibull', 'mu[x]',
                                    NaN, 'Inverse-Weibull', 'mu[x] = 1/sigma * (x/m)^[-m/sigma - 1] / [exp((x/m)^(-m/sigma)) - 1]', 2, 'invweibull', 'mu[x]', 
                                    1943, 'Van der Maen', 'mu[x] = A + B*x + C*x^2 + I/[N - x]', 4, 'vandermaen', 'mu[x]',
                                    1943, 'Van der Maen', 'mu[x] = A + B*x + I/[N - x]', 5, 'vandermaen2', 'mu[x]',
                                    NaN, 'Quadratic', 'mu[x] = A + B*x + C*[x^2]', 5, 'quadratic', 'mu[x]',
                                    1961, 'Beard', 'mu[x] = [A*exp(B^x)] / [1 + K*A*exp(B^x)]', 4, 'beard', 'mu[x]',
                                    1961, 'Makeham-Beard', 'mu[x] = [A*exp(B^x)] / [1 + K*A*exp(B^x)] + C', 4, 'makehambeard', 'mu[x]',
                                    1979, 'Siler', 'mu[x] = A*exp(-B*x) + C + D*exp(E*x)', 6, 'siler', 'mu[x]',
                                    1980, 'Heligman-Pollard', 'q[x]/p[x] = A^[(x + B)^C] + D exp[-E log(x/F)^2] + G H^x', 6, 'HP', 'q[x]',
                                    1980, 'Heligman-Pollard', 'q[x] = A^[(x + B)^C] + D exp[-E log(x/F)^2] + G H^x / [1 + G H^x]', 6, 'HP2', 'q[x]',
                                    1980, 'Heligman-Pollard', 'q[x] = A^[(x + B)^C] + D exp[-E log(x/F)^2] + G H^x / [1 + K*G*H^x]', 6, 'HP3', 'q[x]',
                                    1980, 'Heligman-Pollard', 'q[x] = A^[(x + B)^C] + D exp[-E log(x/F)^2] + G H^(x^K) / [1 + G*H^(x^K)]', 6, 'HP4', 'q[x]',
                                    1983, 'Rogers-Planck', 'q[x] = A0 + A1*exp[-a*x] + A2*exp[b*(x - u) - exp(-c*(x - u))] + A3*exp[d*x]', 6, 'rogersplanck', 'q[x]',
                                    1987, 'Martinelle', 'mu[x] = [A*exp(B*x) + C] / [1 + D*exp(B*x)] + K*exp(B*x)', 6, 'martinelle', 'mu[x]',
                                    1992, 'Carriere', 'l[x] = p1*l[x](weibull) + p2*l[x](invweibull) + p3*l[x](gompertz)', 6, 'carriere1', 'q[x]',
                                    1992, 'Carriere', 'l[x] = p1*l[x](weibull) + p2*l[x](invgompertz) + p3*l[x](gompertz)', 6, 'carriere2', 'q[x]',
                                    1992, 'Kostaki', 'q[x]/p[x] = A^[(x+B)^C] + D*exp[-(Ei*log(x/F_))^2] + G*H^x', 6, 'kostaki', 'q[x]',
                                    1998, 'Kannisto', 'mu[x] = A*exp(B*x) / [1 + A*exp(B*x)]', 5, 'kannisto', 'mu[x]'
                                  )))
    colnames(table) <- c('YEAR', 'NAME', 'MODEL', 'TYPE', 'CODE', 'FIT')
    
    legend <- as.data.frame(matrix(ncol = 2, byrow = T, 
                                   c(1, "Infant mortality",
                                     2, "Accident hump",
                                     3, "Adult mortality",
                                     4, "Adult and/or old-age mortality",
                                     5, "Old-age mortality",
                                     6, "Full age range")))
    colnames(legend) <- c("TYPE", "Coverage" )
  }
  
  if (!is.null(law)) {
    A <- availableLaws()
    if (!(law %in% A$table$CODE)) stop("The specified 'law' is not available.",
                            "Run 'availableLaws()' to see the available models.", call. = F)
    table <- A$table[A$table$CODE %in% law, ]
    legend <- A$legend[A$legend$TYPE %in% unique(table$TYPE), ]
  }
  
  out <- structure(class = "availableLaws", 
                   list(table = table, legend = legend))
  return(out)
}

#' @keywords internal
#' @export
print.availableLaws <- function(x, ...) {
  cat("\nMortality laws available in the package:\n\n")
  print(x$table[, 1:5], right = F)
  cat("\nLEGEND:\n")
  print(x$legend, right = FALSE, row.names = FALSE)
}
