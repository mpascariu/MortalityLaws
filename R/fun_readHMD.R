#' Download Mortality and Population Data (HMD)
#' 
#' Download detailed mortality and population data for different countries 
#' and regions in a single object from the \href{http://www.mortality.org}{
#' Human Mortality Database}.
#' 
#' @param what what type of data are you looking for? There are available:
#' birth records \code{"births"}, death counts \code{"Dx"}, 
#' deaths by Lexis triangles \code{"lexis"}, population size \code{"population"},
#' exposure-to-risk \code{"Ex"}, death-rates \code{"mx"}, 
#' life tables for females \code{"LT_f"}, life tables for males \code{"LT_m"},
#' life tables both sexes combined \code{"LT_t"}, 
#' life expectancy at birth \code{"e0"},
#' cohort death-rates \code{"mxc"} and cohort exposures \code{"Exc"}. 
#' @param countries HMD country codes.
#' @param interval HMD data format: (age interval x year interval).
#' Interval options: \code{1x1}, \code{1x5}, \code{1x10}, 
#' \code{5x1}, \code{5x5}, \code{5x10}.
#' @param username your HMD username. If you don't have one you can sign up
#' for free on Human Mortality Database website.
#' @param password your HMD password.
#' @param save do you want to save a copy of the dataset on your local machine?
#' @return An \code{ReadHMD} object that contains:
#' @return \item{input}{ list with the input data (except the password)}
#' @return \item{data}{ data downloaded from HMD}
#' @return \item{download.date}{ time stamp}
#' @export
#' @examples
#' \dontrun{
#' # Download demographic data for 3 countries in 1x1 format 
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
  input <- list(what = what, countries = countries, interval = interval, 
                username = username, save = save)
  check_input_ReadHMD(input)
  # Progress bar setup
  nr <- length(countries)
  pb <- startpb(0, nr + 1) # Start the clock!
  on.exit(closepb(pb)) # Stop clock on exit.
  setpb(pb, 0)
  
  data <- data.frame() 
  # Step 1 - Do the loop for the other countries
  for (i in 1:nr) {
    setpb(pb, i); cat(paste('      :Downloading', countries[i], '    '))
    data_i <- read_hmd(what, country = countries[i], interval, username, password)
    data   <- rbind(data, data_i)
  }
  download_date <- date()
  
  # Step 2 - Write a file with the database in your working directory
  if (save == TRUE) { 
    file_name <- paste0('HMD_', what, '_', interval, '.Rdata')
    save(input, download_date, data, file = file_name)
  }
  out <- list(input = input, data = data, download.date = download_date)
  out <- structure(class = 'ReadHMD', out)
  
  setpb(pb, nr + 1)
  message(paste('\nHMD download completed!'))
  if (save == TRUE) message(paste(file_name, 'is saved in your working directory:\n', getwd()))
  return(out)
}

#' Function to Download Data for a one Country
#' @inheritParams ReadHMD
#' @param country HMD country code for the selected country. Character.
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
             stop(errMessage, call. = FALSE))
  close(con)
  out <- cbind(country, dat)
  if (interval == "1x1" & !(what %in% c('births', 'lexis', 'e0')) ) out$Age = 0:110
  return(out)
}

#' Country codes
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


#' Check input ReadHMD
#' @param x a list containing the input arguments from ReadHMD function
#' @keywords internal
check_input_ReadHMD <- function(x) {
  int <- c('1x1', '1x5', '1x10', '5x1', '5x5','5x10')
  wht <- c('births', 'population', 'lexis', 'Dx', 'Ex', 'mx', 
           'LT_f', 'LT_m', 'LT_t', 'e0', 'mxc', 'Exc')
  
  if (!(x$interval %in% int)) {
    stop(paste('\nThe interval', x$interval,
               'does not exist in HMD\n',
               'Try one of these options:\n', 
               paste(int, collapse = ', ')), call. = F)}
  if (!(x$what %in% wht)) {
    stop(paste("\n", x$what, 'does not exist in HMD\n',
               'Try one of these options:\n', 
               paste(wht, collapse = ', ')), call. = F)}
  if (all(!(x$countries %in% HMDcountries())) ) {
    stop(paste('\nSomething is wrong in the country/countries',
               'added by you.\n',
               'Try one or more of these options:\n', 
               paste(HMDcountries(), collapse = ', ')), call. = F)}
}


#' Print ReadHMD
#' @param x an object of class \code{"ReadHMD"}
#' @param ... further arguments passed to or from other methods.
#' @keywords internal
#' @export
print.ReadHMD <- function(x, ...){
  cat('Human Mortality Database (www.mortality.org)\n')
  cat('Downloaded by:', x$input$username, '\n')
  cat('Download Date:', x$download.date, '\n')
  cat('Type of data:', x$input$what, '\n')
  cat('Countries included:', x$input$countries, '\n\nData:\n')
  print(head_tail(x$data, hlength = 5, tlength = 5))
}
