# ---------------- Functions to download HMD data --------------------------- #

#' Download data from HMD
#'
#' Function for downloading data for several countries in a single object 
#' from the Human Mortality Database (\url{http://www.mortality.org}).
#' @param what What type of data are you looking for? We have death counts,
#' exposures, death-rates, life tables for femlaes, life table for males,
#' life table for total population, cohort death-rates and cohort exposures. 
#' The codes: \code{births}, \code{population}, \code{lexis},
#' \code{Dx}, \code{Ex}, \code{mx}, \code{LT_f}, 
#' \code{LT_m}, \code{LT_t}, \code{e0}, \code{mxc} and \code{Exc}.
#' @param countries Countries
#' @param interval Interval: \code{1x1}, \code{1x5}, \code{1x10}, 
#' \code{5x1}, \code{5x5}, \code{5x10}
#' @param username Your HMD Username. If you don't have one you can sign up
#' for free on Human Mortality Database website.
#' @param password Your HMD password
#' @param save Do you want to save a copy of the dataset on your local machine?
#' @return An \code{ReadHMD} object.
#' @examples
#' \dontrun{
#' library(MortalityLaws)
#'
#' # Let's download demographic data for 3 countries in 1x1 format 
#' age_int  <- 1  # age interval: 1,5
#' year_int <- 1  # year interval: 1,5,10
#' interval <- paste0(age_int, "x", year_int)  # --> 1x1
#' # And the 3 countries: Sweden Denmark and USA. We have to use the HMD codes
#' cntr  <- c('SWE', 'DNK', 'USA')  
#' 
#' # Download death counts. We don't want to export data outside R.
#' HMD_Dx <- ReadHMD(what = "Dx",
#'                   countries = cntr,
#'                   interval = interval,
#'                   username = "user@email.com",
#'                   password = "password",
#'                   save = FALSE)
#' ls(HMD_Dx)
#' HMD_Dx
#' 
#' # Download life tables for female population and export data.
#' HMD_LT_f <- ReadHMD(what = "LT_f",
#'                     countries = cntr,
#'                     interval = interval,
#'                     username = "user@email.com",
#'                     password = "password",
#'                     save = TRUE)
#' HMD_LT_f
#' } 
#' @export
ReadHMD <- function(what, countries = NULL, interval = '1x1',  
                    username, password, save = TRUE){
  # HMD country codes
  if (is.null(countries)) countries <- HMDcountries()
  
  input <- list(countries = countries, interval = interval, 
                what = what, username = username, save = save)
  check_input_ReadHMD(input)
  # Progress bar setup
  nr <- length(countries)
  pb <- startpb(0, nr + 1) # Start the clock!
  on.exit(closepb(pb)) # Stop clock on exit.
  setpb(pb, 0)
  
  data <- data.frame() 
  # Step 1 - Do the loop for the other countries
  for (i in 1:nr) {
      cntr_i <- countries[i] # country
      setpb(pb, i); cat(paste('      :Downloading', cntr_i, '    '))
      data_i <- read_hmd(what, country = cntr_i, 
                         interval, username, password)
      data <- rbind(data, data_i)
  }
  download_date <- date()
  
  # Step 2 - Write a file with the database in your working directory
  if (save == TRUE) { 
      save(input, download_date, data,
           file = paste('HMD_', what, '_', 
                        interval, '.Rdata', sep = ''))
  }
  out <- structure(class = 'ReadHMD',
                  list(input = input, data = data,
                       download.date = download_date))
  setpb(pb, nr + 1)
  cat(paste('\nHMD download completed!'))
  return(out)
}

#' Function to download data for a specified country
#' @keywords internal
read_hmd <- function(what, country, interval, username, password){
  if (what == 'e0' & interval == '1x1') {
    whichFile = 'E0per.txt' } else {
      whichFile <- switch(what, 
                          births = paste0("Births.txt"),
                          population = paste0("Population.txt"),
                          lexis = paste0("Deaths_lexis.txt"),
                          Dx   = paste0("Deaths_", interval, ".txt"),    # deaths
                          Ex   = paste0("Exposures_", interval, ".txt"), # exposure
                          mx   = paste0("Mx_", interval, ".txt"),        # death rates
                          LT_f = paste0("fltper_", interval, ".txt"),    # Life tables, Females
                          LT_m = paste0("mltper_", interval, ".txt"),    # Life tables, Males
                          LT_t = paste0("bltper_", interval, ".txt"),    # Life tables, Both sexes
                          e0   = paste0("E0per_", interval, ".txt"),     # Life expectancy
                          # Cohort data
                          mxc = paste0("cMx_", interval, ".txt"),    # deaths
                          Exc = paste0("cExposures_", interval, ".txt") # exposure
      )}
  path       <- paste0("http://www.mortality.org/hmd/", country, 
                       "/STATS/", whichFile)
  userpwd    <- paste0(username, ":", password)
  txt        <- getURL(path, userpwd = userpwd)
  con        <- textConnection(txt)
  errMessage <- paste0("\nThe server could not verify that you are authorized ",
                       "to access the requested data.\n", "Most probably you ",
                       "supplied the wrong credentials (e.g., bad password).")
  dat <- try(read.table(con, skip = 2, header = TRUE, na.strings = "."), 
             stop(errMessage))
  close(con)
  out <- cbind(country, dat)
  if (interval == "1x1" & !(what %in% c('births', 'lexis', 'e0')) ) out$Age = 0:110
  return(out)
}

#' Countries
#' @keywords internal
HMDcountries <- function() {
  HMDc <- c("AUS","AUT","BEL","BGR","BLR","CAN","CHL","CHE","CZE",
           "DEUTE","DEUTNP","DEUTW","DNK","ESP","EST","FIN","FRACNP",
           "FRATNP","GBR_NIR","GBR_NP","GBR_SCO","GBRCENW","GBRTENW","GRC",
           "HUN","HRV","IRL","ISL","ISR","ITA","JPN","LTU","LUX","LVA","NLD",
           "NOR","NZL_MA","NZL_NM","NZL_NP","POL","PRT","RUS","SVK",
           "SVN","SWE","TWN","USA","UKR")
  HMDc
}

#' Check data availability in HMD
#' 
#' The function returns information about available data in HMD (period life tables etc.), 
#' with the range of years covered by the life tables.
#' @inheritParams ReadHMD
#' @param ... Other parameters to be passed in \code{ReadHMD} function.
#' @return An \code{availableHMD} object.
#' @examples 
#' \dontrun{
#' library(MortalityLaws)
#' datainfo <- available.data(username = "your_username", 
#'                            password = "your_password")
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


#' Check input ReadHMD
#' @keywords internal
check_input_ReadHMD <- function(x) {
  int <- c('1x1', '1x5', '1x10', '5x1', '5x5','5x10')
  wht <- c('births', 'population', 'lexis', 'Dx', 'Ex', 'mx', 
            'LT_f', 'LT_m', 'LT_t', 'e0', 'mxc', 'Exc')
  
  if (!(x$interval %in% int)) {
    stop(paste('\nThe interval', x$interval,
               'does not exist in HMD\n',
               'Try one of these options:\n', 
                paste(int, collapse = ', ')) )}
  if (!(x$what %in% wht)) {
    stop(paste(x$what, 'does not exist in HMD\n',
               'Try one of these options:\n', 
                paste(wht, collapse = ', ')) )}
  if (all(!(x$countries %in% HMDcountries())) ) {
    stop(paste('\nSomething is wrong in the country/coutries',
               'added by you.\n',
               'Try one or more of these options:\n', 
               paste(HMDcountries(), collapse = ', ')) )}
}


#' Summary function - display head and tail in a single data.frame
#' The code for this function was first written for 'psych' R package
#' @keywords internal
head_tail <- function(x, hlength = 4, tlength = 4, digits = 2, ellipsis = TRUE) 
{
  if (is.data.frame(x) | is.matrix(x)) {
    if (is.matrix(x)) 
      x <- data.frame(unclass(x))
    nvar <- dim(x)[2]
    dots <- rep("...", nvar)
    h <- data.frame(head(x, hlength))
    t <- data.frame(tail(x, tlength))
    for (i in 1:nvar) {
      if (is.numeric(h[1, i])) {
        h[i] <- round(h[i], digits)
        t[i] <- round(t[i], digits)
      }
      else {
        dots[i] <- NA
      }
    }
    if (ellipsis) {
      head.tail <- rbind(h, ... = dots, t)
    }
    else {
      head.tail <- rbind(h, t)
    }
  }
  else {
    h <- head(x, hlength)
    t <- tail(x, tlength)
    if (ellipsis) {
      head.tail <- rbind(h, "...       ...", t)
    }
    else {
      head.tail <- rbind(h, t)
      head.tail <- as.matrix(head.tail)
    }
  }
  return(head.tail)
}

