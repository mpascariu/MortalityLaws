# ---------------- Functions to download HMD data --------------------------- #


#' Function to download data from HMD
#'
#' Function to download data for several countries in a single object 
#' from the Human Mortality Database (www.mortality.org)
#' @param countries Countries
#' @param interval Interval: 1x1, 1x5, 1x10, 5x1
#' @param what ...
#' @param username HMD Username
#' @param password HMD password
#' @param save Save data
#' @return ....
#' @examples
#' \dontrun{
#' username <- "user@email.com"
#' password <- "password"
#' age_int  <- 1  # age interval: 1,5
#' year_int <- 1  # year interval: 1,5,10
#' interval <- paste0(age_int, "x", year_int)  # --> 1x1
#' cntr     <- 'SWE'  # We want to download Swedish data
#'
#' # Download death counts. We don't want to export data outside R.
#' HMD_Dx <- ReadHMD(cntr, interval, what = "Dx", 
#'                   username, password, save = FALSE)
#' 
#' # Download life tables for female population and export data.
#' HMD_LT_f <- ReadHMD(cntr, interval, what = "LT_f",
#'                     username, password, save = TRUE)
#' } 
#' @export

ReadHMD <- function (countries, interval, what, 
                     username, password, save=TRUE){
     ptm <- proc.time() # Start the clock!
     data <- data.frame()
     # Step 1 - Do the loop for the other countries
     for(i in 1:(length(countries))){
          cntr_i <- countries[i] # country
          data_i <- readHMD(country=cntr_i, interval, 
                             what, username, password)
          data <- rbind(data, data_i)
     }
     download_date <- date()
     HMD_user <- username
     process_speed <- paste(round((proc.time() - ptm)[3],2),'seconds.') # Stop the clock
     
     # Step 2 - Write a file with the database in your working directory
     if(save == TRUE){ 
          save(download_date, data, countries, HMD_user, process_speed,
               file=paste('HMD_',what,'_',interval,'.Rdata',sep=''))
     }
     out <- list(download.date = download_date, data = data, 
                 process_speed = process_speed,
                 countries = countries, HMD_user = username)
     print(paste('DOWNLOAD FINISHED! ---- downloading time:',process_speed))
     return(invisible(out))
}

#--------------------
# Function to download data for a specified country
#' @keywords internal
readHMD <- function (country, interval, what, username, password) {
     whichFile <- switch(what, Dx   = paste0("Deaths_", interval, ".txt"),    # deaths
                         Nx   = paste0("Exposures_", interval, ".txt"), # exposure
                         mx   = paste0("Mx_", interval, ".txt"),        # death rates
                         LT_f = paste0("fltper_", interval, ".txt"),    # Life tables, Females
                         LT_m = paste0("mltper_", interval, ".txt"),    # Life tables, Males
                         LT_t = paste0("bltper_", interval, ".txt"),    # Life tables, Both sexes
                         # Cohort data
                         mxc = paste0("cMx_", interval, ".txt"),    # deaths
                         Nxc = paste0("cExposures_", interval, ".txt") # exposure
     ) 
     print(paste("Downloading", country,what)); flush.console()
     path       <- paste0("http://www.mortality.org/hmd/", country, 
                         "/STATS/", whichFile)
     userpwd    <- paste0(username, ":", password)
     txt        <- getURL(path, userpwd = userpwd)
     con        <- textConnection(txt)
     dat        <- read.table(con, skip = 2, header = TRUE, na.strings = ".")
     close(con)
     datCnt     <- cbind(country, dat)
     datCnt$Age <- if(interval=="1x1") {0:110} else datCnt$Age
     return(datCnt)
}




