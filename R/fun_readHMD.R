# ---------------- Functions to download HMD data --------------------------- #


#' Function for downloading data from HMD
#'
#' Function for downloading data for several countries in a single object 
#' from the Human Mortality Database (\url{http://www.mortality.org})
#' @param countries Countries
#' @param interval Interval: \code{1x1}, \code{1x5}, \code{1x10}, \code{5x1}
#' @param what What type of data are you looking for? We have death counts,
#' exposures, death-rates, life tables for femlaes, life table for males,
#' life table for total population, cohort death-rates and cohort exposures. 
#' The codes: \code{Dx}, \code{Nx}, \code{mx}, \code{LT_f}, 
#' \code{LT_m}, \code{LT_t}, \code{mxc} and \code{Nxc}.
#' @param username Your HMD Username. If you don't have one you can sign up
#' for free on Human Mortality Database website.
#' @param password Your HMD password
#' @param save Do you want to save a copy of the dataset on your local machine?
#' @return An \code{ReadHMD} object.
#' @examples
#' \dontrun{
#' library(MortalityLaws)
#'
#' # Let's download demographic data for 3 countries.
#' # First specify the username and password
#' username <- "user@email.com"
#' password <- "password"
#' # Let's say that we want a 1x1 format
#' age_int  <- 1  # age interval: 1,5
#' year_int <- 1  # year interval: 1,5,10
#' interval <- paste0(age_int, "x", year_int)  # --> 1x1
#' # And the 3 countries: Sweden Denmark and USA
#' cntr     <- c('SWE', 'DNK', 'USA')  
#' 
#' # Download death counts. We don't want to export data outside R.
#' HMD_Dx <- ReadHMD(cntr, interval, what = "Dx", 
#'                   username, password, save = FALSE)
#' HMD_Dx
#' ls(HMD_Dx)
#' 
#' # Download life tables for female population and export data.
#' HMD_LT_f <- ReadHMD(cntr, interval, what = "LT_f",
#'                     username, password, save = TRUE)
#' HMD_LT_f
#' } 
#' @export
#' 
ReadHMD <- function(countries, interval, what, 
                    username, password, save = TRUE){
  input <- list(countries = countries, interval = interval, 
                what = what, username = username, save = save)
  # Progress bar setup
  nr <- length(countries)
  pb <- startpb(0, nr) # Start the clock!
  on.exit(closepb(pb)) # Stop clock on exit.
  
  data <- data.frame() 
  # Step 1 - Do the loop for the other countries
  for (i in 1:nr) {
      cntr_i <- countries[i] # country
      data_i <- readHMD(country = cntr_i, interval, 
                         what, username, password)
      data <- rbind(data, data_i)
      setpb(pb, i)
  }
  download_date <- date()
  
  # Step 2 - Write a file with the database in your working directory
  if (save == TRUE) { 
      save(input, download_date, data,
           file = paste('HMD_', what, '_', interval, '.Rdata', sep = ''))
  }
  out <- structure(class = 'ReadHMD',
                  list(input = input, download.date = download_date, 
                       data = data))
  cat(paste('\nHMD download completed!'))
  return(out)
}

#--------------------
# Function to download data for a specified country
#' @keywords internal
readHMD <- function(country, interval, what, username, password){
     whichFile <- switch(what, 
                         Dx   = paste0("Deaths_", interval, ".txt"),    # deaths
                         Nx   = paste0("Exposures_", interval, ".txt"), # exposure
                         mx   = paste0("Mx_", interval, ".txt"),        # death rates
                         LT_f = paste0("fltper_", interval, ".txt"),    # Life tables, Females
                         LT_m = paste0("mltper_", interval, ".txt"),    # Life tables, Males
                         LT_t = paste0("bltper_", interval, ".txt"),    # Life tables, Both sexes
                         # Cohort data
                         mxc = paste0("cMx_", interval, ".txt"),    # deaths
                         Nxc = paste0("cExposures_", interval, ".txt") # exposure
     ) 
     cat(paste("        :Downloading", country,what))
     path       <- paste0("http://www.mortality.org/hmd/", country, 
                         "/STATS/", whichFile)
     userpwd    <- paste0(username, ":", password)
     txt        <- getURL(path, userpwd = userpwd)
     con        <- textConnection(txt)
     dat        <- read.table(con, skip = 2, header = TRUE, na.strings = ".")
     close(con)
     datCnt     <- cbind(country, dat)
     datCnt$Age <- if (interval == "1x1") {0:110} else datCnt$Age
     return(datCnt)
}




