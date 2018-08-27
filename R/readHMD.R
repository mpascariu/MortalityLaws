#' Download Mortality and Population Data (HMD)
#' 
#' Download detailed mortality and population data for different countries 
#' and regions in a single object from the \href{https://www.mortality.org}{
#' Human Mortality Database}.
#' 
#' @param what What type of data are you looking for? There are available:
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
#' @param username Your HMD username. If you don't have one you can sign up
#' for free on the Human Mortality Database website.
#' @param password Your HMD password.
#' @param save Do you want to save a copy of the dataset on your local machine? 
#' Logical. Default: \code{FALSE}.
#' @param show Choose whether to display a progress bar. Logical. Default: \code{TRUE}.
#' @return A \code{ReadHMD} object that contains:
#'  \item{input}{List with the input values (except the password).}
#'  \item{data}{Data downloaded from HMD.}
#'  \item{download.date}{Time stamp.}
#'  \item{years}{Numerical vector with the years covered in the data.}
#'  \item{ages}{Numerical vector with ages covered in the data.}
#' @author Marius D. Pascariu
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
#'                   interval  = interval,
#'                   username  = "user@email.com",
#'                   password  = "password",
#'                   save = FALSE)
#' ls(HMD_Dx)
#' HMD_Dx
#' 
#' # Download life tables for female population and export data.
#' LTF <- ReadHMD(what = "LT_f",
#'                countries = cntr,
#'                interval  = interval,
#'                username  = "user@email.com",
#'                password  = "password",
#'                save = TRUE)
#' LTF
#' } 
#' @export
ReadHMD <- function(what, countries = NULL, interval = "1x1",  
                    username, password, save = TRUE, show = TRUE){
  # Step 1 - Validate input & Progress bar setup
  if (is.null(countries)) countries <- HMDcountries()
  input <- list(what = what, countries = countries, interval = interval, 
                username = username, save = save, show = show)
  check_input_ReadHMD(input)
  nr <- length(countries)
  if (show) {pb <- startpb(0, nr + 1); on.exit(closepb(pb)); setpb(pb, 0)}
  
  # Step 2 - Do the loop for the other countries
  D <- data.frame() 
  for (i in 1:nr) {
    if (show) {setpb(pb, i); cat(paste("      :Downloading", countries[i], "    "))}
    D <- rbind(D, ReadHMD.core(what, country = countries[i], 
                               interval, username, password))
  }
  Y   <- sort(unique(D$Year))
  A   <- sort(unique(D$Age))
  fn  <- paste0("HMD_", what) # file name
  out <- list(input = input, data = D, download.date = date(), years = Y, ages = A)
  out <- structure(class = "ReadHMD", out)
  
  # Step 3 - Write a file with the database in your working directory
  if (save) {
    assign(fn, value = out)
    save(list = fn, file = paste0(fn, ".Rdata"))
  }
  if (show) {
    setpb(pb, nr + 1)
    wd  <- getwd()
    n   <- nchar(wd)
    wd_ <- paste0("...", substring(wd, first = n - 45, last = n))
    cat("\n   ")
    if (save) {
      message(paste(fn, "is saved in your working directory:\n  ", wd_), appendLF = F)
      cat("\n   ")
    }
    message("Download completed!")
  }
  return(out)
}

#' Function to Download Data for a one Country
#' @inheritParams ReadHMD
#' @param country HMD country code for the selected country. Character.
#' @keywords internal
ReadHMD.core <- function(what, country, interval, username, password){
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
  path <- paste0("https://www.mortality.org/hmd/", country, "/STATS/", whichFile)
  txt  <- RCurl::getURL(path, userpwd = paste0(username, ":", password))
  con  <- try(textConnection(txt), 
              stop("ReadHMD() failed to connect to www.mortality.org. ",
                   "Maybe the website is down at this moment?", call. = FALSE))
  dat  <- try(read.table(con, skip = 2, header = TRUE, na.strings = "."), 
              stop("The server could not verify that you are authorized ",
                   "to access the requested data. Probably you ",
                   "supplied the wrong credentials (e.g., bad password)?", 
                   call. = FALSE))
  close(con)
  out <- cbind(country, dat)
  if (interval == "1x1" & !(what %in% c('births', 'lexis', 'e0')) ) out$Age = 0:110
  return(out)
}

#' Country codes
#' @keywords internal
HMDcountries <- function() {
  HMDc <- c("AUS","AUT","BEL","BGR","BLR","CAN","CHL","CHE","CZE",
            "DEUTE","DEUTNP","DEUTW","DNK","ESP","EST","FIN","FRACNP","FRATNP",
            "KOR","GBR_NIR","GBR_NP","GBR_SCO","GBRCENW","GBRTENW","GRC",
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
    stop('The interval ', x$interval, ' does not exist in HMD ', 
         'Try one of these options:\n', paste(int, collapse = ', '), 
         call. = F)}
  if (!(x$what %in% wht)) {
    stop(x$what, ' does not exist in HMD. Try one of these options:\n', 
         paste(wht, collapse = ', '), call. = F)}
  if (all(!(x$countries %in% HMDcountries())) ) {
    stop('Something is wrong in the country/countries added by you.\n',
         'Try one or more of these options:\n', 
          paste(HMDcountries(), collapse = ', '), call. = F)}
}


#' Print ReadHMD
#' @param x an object of class \code{"ReadHMD"}
#' @param ... further arguments passed to or from other methods.
#' @keywords internal
#' @export
print.ReadHMD <- function(x, ...){
  what <- x$input$what
  cat('Human Mortality Database (www.mortality.org)\n')
  cat('Downloaded by :', x$input$username, '\n')
  cat('Download Date :', x$download.date, '\n')
  cat('Type of data  :', what, '\n')
  cat(paste("Interval      :", x$input$interval, "\n"))

  ageMsg <- if (what == "e0") 0 else paste(min(x$ages), "-", max(x$ages))
  cat(paste("Years         :", min(x$years), "-", max(x$years), "\n"))
  cat(paste("Ages          :", ageMsg, "\n"))
  cat("Countries     :", x$input$countries, "\n")
  cat('\nData:\n')
  print(head_tail(x$data, hlength = 5, tlength = 5))
}
