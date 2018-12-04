# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: GNU General Public License v3.0
# Last update: Tue Dec  4 15:55:43 2018
# --------------------------------------------------- #

#' Check Data Availability in HMD
#' 
#' The function returns information about available data in HMD (period life 
#' tables etc.), with the range of years covered by the life tables.
#' @param link Link to the HMD csv file summarising the available data. 
#' Change it only if the path to the file has been modified and the maintainer 
#' of the package is not quick enough to realised that.
#' Default: "https://www.mortality.org/countries.csv"
#' @return An object of class \code{availableHMD}.
#' @seealso \code{\link{ReadHMD}}
#' @author Marius D. Pascariu
#' @examples 
#' \dontrun{
#' availableHMD()
#' }
#' @export
availableHMD <- function(link = "https://www.mortality.org/countries.csv") {
  
  txt  <- RCurl::getURL(link)
  con  <- textConnection(txt)
  L    <- read.csv(con, header = TRUE, sep = ",", dec = ".", skip = 0)
  close(con)
  
  A <- L[L$fu22bar == 1, c("Country", 
                           "Subpop.Code", 
                           "ST_Per_LT_FY", 
                           "ST_Per_LT_EY")]
  colnames(A) <- c("country", "code", "BOP", "EOP")
  nc <- length(A$country)
  
  out <- list(avalable.data = A, 
              number.of.contries = nc, 
              hmd.csv = L,
              checked.date = date())
  
  out <- structure(class = "availableHMD", out)
  return(out)
}


#' Print for availableHMD
#' @param x an object of class \code{"availableHMD"}
#' @param ... further arguments passed to or from other methods.
#' @keywords internal
#' @export
print.availableHMD <- function(x, ...) {
  cat('Human Mortality Database (www.mortality.org)\n')
  cat('Checked Date:', x$checked.date, '\n')
  cat('Number of countries/populations:', nrow(x$avalable.data), '\n\n')
  cat('The list below contains the populations currently included in the HMD,\n')
  cat('with the range of years covered by the period life tables:\n\n')
  print(x$avalable.data)
}

