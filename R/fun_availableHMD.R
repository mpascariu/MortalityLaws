#' Check Data Availability in HMD
#' 
#' The function returns information about available data in HMD (period life tables etc.), 
#' with the range of years covered by the life tables.
#' @inheritParams ReadHMD
#' @param ... Other parameters to be passed in \code{ReadHMD} function.
#' @return An \code{availableHMD} object.
#' @seealso \code{\link{ReadHMD}}
#' @examples 
#' \dontrun{
#' # This will take few seconds...
#' datainfo <- availableHMD(username = "your_username", 
#'                          password = "your_password")
#' datainfo
#' }
#' @export
availableHMD <- function(username, password, ...) {
  HMD <- ReadHMD(what = 'e0', username = username, password = password, 
                 save = FALSE, ...)
  hmd <- HMD$data[, 1:2]
  cts <- as.character(unique(hmd$country))
  nc  <- length(cts)
  dta <- NULL
  for (i in 1:nc) {
    cntr = cts[i]
    int  = range(hmd[hmd$country == cntr, 2])
    dta  = rbind(dta, data.frame(country = cntr, BOP = int[1], EOP = int[2]))
  }
  
  cd  <- HMD$download.date
  out <- structure(class = "availableHMD",
                   list(avalable.data = dta, countries = cts, 
                        number.of.contries = nc, checked.date = cd))
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
  cat('Number of countries/populations:', length(x$countries), '\n\n')
  cat('The list below contains the populations currently included in the HMD,\n')
  cat('with the range of years covered by the period life tables:\n\n')
  print(x$avalable.data)
}
