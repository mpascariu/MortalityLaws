# -------------------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last Update: Mon Jan 15 18:23:03 2024
# -------------------------------------------------------------- #

#' Download The Human Mortality Database (HMD)
#'
#' Download detailed mortality and population data for different countries
#' and regions in a single object from the \href{https://www.mortality.org/}{
#' Human Mortality Database}.
#'
#' @details
#' The Human Mortality Database (HMD) was created to provide detailed mortality
#' and population data to researchers, students, journalists, policy analysts,
#' and others interested in the history of human longevity. The project began
#' as an outgrowth of earlier projects in the Department of Demography at the
#' University of California, Berkeley, USA, and at the Max Planck Institute for
#' Demographic Research in Rostock, Germany (see history). It is the work of two
#' teams of researchers in the USA and Germany (see research teams), with the
#' help of financial backers and scientific collaborators from around the world
#' (see acknowledgements). The Center on the Economics and Development of Aging
#' (CEDA) French Institute for Demographic Studies (INED) has also supported the
#' further development of the database in recent years.
#'
#' @param what What type of data are you looking for? The following options
#' might be available for some or all the countries and regions: \itemize{
#'   \item{\code{"births"}} -- birth records;
#'   \item{\code{"Dx_lexis"}} -- deaths by Lexis triangles;
#'   \item{\code{"Ex_lexis"}} -- exposure-to-risk by Lexis triangles;
#'   \item{\code{"population"}} -- population size;
#'   \item{\code{"Dx"}} -- death counts;
#'   \item{\code{"Ex"}} -- exposure-to-risk;
#'   \item{\code{"mx"}} -- central death-rates;
#'   \item{\code{"LT_f"}} -- period life tables for females;
#'   \item{\code{"LT_m"}} -- period life tables for males;
#'   \item{\code{"LT_t"}} -- period life tables both sexes combined;
#'   \item{\code{"e0"}} -- period life expectancy at birth;
#'   \item{\code{"Exc"}} -- cohort exposures;
#'   \item{\code{"mxc"}} -- cohort death-rates;
#'   \item{\code{"LT_fc"}} -- cohort life tables for females;
#'   \item{\code{"LT_mc"}} -- cohort life tables for males;
#'   \item{\code{"LT_tc"}} -- cohort life tables both sexes combined;
#'   \item{\code{"e0c"}} -- cohort life expectancy at birth;
#'   }
#' @param countries Specify the country data you want to download by adding the
#' HMD country code/s. Options:
#' \code{ 
#' "AUS"    "AUT",    "BEL",   "BGR", 
#' "BLR",   "CAN",    "CHL",   "HRV",
#' "HKG",   "CHE",    "CZE",   "DEUTNP", 
#' "DEUTE", "DEUTW",  "DNK",   "ESP", 
#' "EST",   "FIN",    "FRATNP","FRACNP", 
#' "GRC",   "HUN",    "IRL",   "ISL"    
#' "ISR",   "ITA",    "JPN",   "KOR", 
#' "LTU",   "LUX",    "LVA",   "NLD",   
#' "NOR",   "NZL_NP", "NZL_MA" "NZL_NM", 
#' "POL",   "PRT"     "RUS",   "SVK", 
#' "SVN",   "SWE",    "TWN",   "UKR",   
#' "GBR_NP","GBRTENW","GBRCENW","GBR_SCO",
#' "GBR_NIR","USA"}.
#'  If \code{NULL} data for all the countries are downloaded at once;
#' @param interval Datasets are given in various age and time formats based on
#' which the records are agregated. Interval options:
#' \itemize{
#'   \item{\code{"1x1"}} -- by age and year;
#'   \item{\code{"1x5"}} -- by age and 5-year time interval;
#'   \item{\code{"1x10"}} -- by age and 10-year time interval;
#'   \item{\code{"5x1"}} -- by 5-year age group and year;
#'   \item{\code{"5x5"}} -- by 5-year age group and 5-year time interval;
#'   \item{\code{"5x10"}} --by 5-year age group and 10-year time interval.
#'   }
#' @param username Your HMD username. If you don't have one you can sign up
#' for free on the Human Mortality Database website.
#' @param password Your HMD password.
#' @param save Do you want to save a copy of the dataset on your local machine?
#' Logical. Default: \code{FALSE}.
#' @param show Choose whether to display a progress bar. Logical.
#' Default: \code{TRUE}.
#' @return A \code{ReadHMD} object that contains:
#'  \item{input}{List with the input values (except the password).}
#'  \item{data}{Data downloaded from HMD.}
#'  \item{download.date}{Time stamp.}
#'  \item{years}{Numerical vector with the years covered in the data.}
#'  \item{ages}{Numerical vector with ages covered in the data.}
#' @author Marius D. Pascariu
#' @examples
#' \dontrun{
#'
#'
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
                    username, password, save = FALSE, show = TRUE){
  # Step 1 - Validate input & Progress bar setup
  if (is.null(countries)) {
    countries <- HMDcountries()
  }
  
  input <- list(what = what, countries = countries, interval = interval,
                username = username, save = save, show = show)
  check_input_ReadHMD(input)
  nr <- length(countries)
  
  if (show) {
    pb <- startpb(0, nr + 1)
    on.exit(closepb(pb))
    setpb(pb, 0)
  }
  
  # Step 2 - Do the loop for the other countries
  D <- data.frame()
  for (i in 1:nr) {
    if (show) {
      setpb(pb, i)
      cat(paste("      :Downloading", countries[i], "    "))
    }
    
    D <- rbind(D, ReadHMD.core(what, country = countries[i], interval,
                               username, password,
                               link = "https://www.mortality.org/File/GetDocument/hmd.v6/"))
  }
  
  if (length(D) != 0) {
    out <- list(input = input,
                data = D,
                download.date = date(),
                years = sort(unique(D$Year)),
                ages = unique(D$Age))
    out <- structure(class = "ReadHMD", out)
    
    # Step 3 - Write a file with the database in your working directory
    if (show) setpb(pb, nr + 1)
    if (save) saveOutput(out, show, prefix = "HMD")
    
  } else {
    out <- NULL
  }
  
  # Exit
  return(out)
}


#' Save Output in the working directory
#' @param out Output file
#' @inheritParams ReadHMD
#' @return No return value, called for side effects
#' @keywords internal
saveOutput <- function(out, show, prefix) {
  fn  <- paste0(prefix, "_", out$input$what) # file name
  assign(fn, value = out)
  save(list = fn, file = paste0(fn, ".Rdata"))
  if (show) saveMsg()
}


#' Print message when saving an object
#' @inherit saveOutput return
#' @keywords internal
saveMsg <- function() {
  wd  <- getwd()
  n   <- nchar(wd)
  wd_ <- paste0("...", substring(wd, first = n - 45, last = n))
  message(paste("\nThe dataset is saved in your working directory:\n  ", wd_),
          appendLF = FALSE)
  message("\nDownload completed!\n")
}


#' Function to Download Data for a one Country
#' @inheritParams ReadHMD
#' @param country HMD country code for the selected country. Character;
#' @param link the main link to the database.
#' @return A data.frame containing demographic data
#' @keywords internal
ReadHMD.core <- function(what, country, interval, username, password, link){
  
  if (what == "e0" & interval == "1x1") {
    whichFile <- "E0per"
    
  } else if (what == "e0c" & interval == "1x1"){
    whichFile <- "E0coh"
    
  } else {
    whichFile <- switch(what,
                        births = "Births",
                        population = "Population",
                        Dx_lexis = "Deaths_lexis",
                        Ex_lexis = "Exposures_lexis",
                        Dx   = paste0("Deaths_", interval),    # deaths
                        Ex   = paste0("Exposures_", interval), # exposure
                        mx   = paste0("Mx_", interval),        # death rates
                        LT_f = paste0("fltper_", interval),    # Life tables, Females
                        LT_m = paste0("mltper_", interval),    # Life tables, Males
                        LT_t = paste0("bltper_", interval),    # Life tables, Both sexes
                        e0   = paste0("E0per_", interval),     # Life expectancy
                        # Cohort data
                        Exc = paste0("cExposures_", interval),
                        mxc = paste0("cMx_", interval),
                        LT_fc = paste0("fltcoh_", interval),
                        LT_mc = paste0("mltcoh_", interval),
                        LT_tc = paste0("bltcoh_", interval),
                        e0c = paste0("E0coh_", interval)
    )}
  
  
  if (link %in% c("https://www.mortality.org/File/GetDocument/hmd.v6/",
                  "https://www.ipss.go.jp/p-toukei/JMD/")) {
    interlude <- "/STATS/"
    
  } else {
    interlude <- "/"
  }
  
  path     <- paste0(link, country, interlude, whichFile, ".txt")
  response <- ""
  
  if (is.null(username) | is.null(password)) {
    response <- try(silent = TRUE, GET(url = path))

  } else {
    # "We did not find the right method to login yet. Following the HMD website
    # change the line below no longer works."
    response <- try(silent = TRUE,
               RCurl::getURL(url = path, userpwd = paste0(username, ":", password))
    )
  }
  
  status_code_response <- if(is(response, "response")) status_code(response) else 404
  
  # Check if the request was successful (status code 200 means success)
  if (status_code_response == 200) {
    # Read the content of the text file
    txt <- content(response, "text", encoding = "UTF-8")
    
    con  <- try(textConnection(txt),
                stop("\nThe function failed to connect to ", link,
                     " Maybe the website is down at this moment?", call. = FALSE))
    
    JPNcodes <- substrRight(paste0(0, 0:47), 2)
    if (any(country %in% JPNcodes)) {
      country <- JPNregions()[as.numeric(country) + 1]
    }
    
    dat  <- try(read.table(con, skip = 2, header = TRUE, na.strings = "."),
                stop("\n", what, " data for ", country, " state in the ", interval,
                     " format was not to be found. We have been looking here:\n",
                     path, call. = FALSE))
    
    close(con)
    out <- cbind(country, dat)
    if (any(interval %in% c("1x1", "1x5", "1x10")) &
        !any(what %in% c("births", "Dx_lexis", "Ex_lexis", "e0", "e0c"))) {
      out$Age <- 0:110
    }
    
  } else {
    message("\nThe internet connection to ", link, " failed with status code:", 
            status_code_response, "!",
            "\nMaybe the website is down at this moment?")
    out <- NULL
  }
  
  return(out)
}


#' Country codes
#' @return a vector
#' @keywords internal
HMDcountries <- function() {
  c("AUS","AUT","BEL","BGR","BLR",
    "CAN","CHL","HRV","CHE","CZE",
    "DEUTNP","DEUTE", "DEUTW","DNK","ESP",
    "EST","FIN","FRATNP","FRACNP","GRC",
    "HUN", "HKG", "IRL","ISL", "ISR",
    "ITA","JPN","KOR","LTU","LUX",
    "LVA","NLD","NOR","NZL_NP","NZL_MA",
    "NZL_NM","POL","PRT","RUS","SVK",
    "SVN","SWE","TWN","UKR","GBR_NP",
    "GBRTENW", "GBRCENW","GBR_SCO","GBR_NIR","USA")
}

#' Data formats
#' @return a vector
#' @keywords internal
data_format <- function() c("1x1", "1x5", "1x10", "5x1", "5x5","5x10")


#' HMD Indices
#' @return a vector
#' @keywords internal
HMDindices <- function() c("births", "population", "Dx_lexis", "Ex_lexis", "Dx",
                           "mx", "Ex", "LT_f", "LT_m", "LT_t", "e0",
                           "mxc", "Exc", "LT_fc", "LT_mc", "LT_tc", "e0c")

#' Check input ReadHMD
#' @param x a list containing the input arguments from ReadHMD function
#' @return No return value, called for validating input data
#' @keywords internal
check_input_ReadHMD <- function(x) {
  coh_countries <- c("DNK", "FIN", "FRATNP", "FRACNP", "ISL", "ITA", "NLD",
                     "NOR", "SWE", "CHE", "GBRTENW", "GBRCENW", "GBR_SCO")
  
  if (!any(x$interval %in% data_format())) {
    stop("The interval ", x$interval, " does not exist in HMD ",
         "Try one of these options:\n", paste(data_format(), collapse = ", "),
         call. = FALSE)
  }
  
  if (!any(x$what %in% HMDindices())) {
    stop(x$what, " does not exist in HMD. Try one of these options:\n",
         paste(HMDindices(), collapse = ", "), call. = FALSE)
  }
  
  if (all(!(x$countries %in% HMDcountries()))) {
    stop("Something is wrong in the country/countries added by you.\n",
         "Try one or more of these options:\n",
         paste(HMDcountries(), collapse = ", "), call. = FALSE)
  }
  
  # Availability of Cohort Data
  if (any(x$what %in% c("LT_fc", "LT_mc", "LT_tc", "e0c")) &
      !(all(x$countries %in% coh_countries))) {
    stop("Data type ", x$what,
         " is not available for one or more countries specified in input.\n",
         "Check one of these countries:\n",
         paste(coh_countries, collapse = ", "), call. = FALSE)
  }
  
  # Availability of Life Expectancy Data
  if (any(x$what %in% c("e0", "e0c")) &
      !(x$interval %in% c("1x1", "1x5", "1x10"))) {
    stop("Data type ", x$what,
         " is available only in the following formats: '1x1', '1x5', '1x10'.",
         call. = FALSE)
  }
}


#' Print ReadHMD
#' @param x An object of class \code{"ReadHMD"}
#' @param ... Further arguments passed to or from other methods.
#' @return Print data on the console
#' @keywords internal
#' @export
print.ReadHMD <- function(x, ...){
  what <- x$input$what
  cat("Human Mortality Database (https://www.mortality.org)\n")
  cat("Downloaded by :", x$input$username, "\n")
  cat("Download Date :", x$download.date, "\n")
  cat("Type of data  :", what, "\n")
  cat(paste("Interval      :", x$input$interval, "\n"))
  cat(paste("Years         :", x$years[1], "--", rev(x$years)[1], "\n"))
  cat(paste("Ages          :", ageMsg(what, x), "\n"))
  cat("Countries     :", x$input$countries, "\n")
  cat("\nData:\n")
  print(head_tail(x$data, hlength = 5, tlength = 5))
}


#' What age(s) are we looking at?
#' @inheritParams print.ReadHMD
#' @return A scalar or character indicating age groups
#' @keywords internal
ageMsg <- function(what, x) {
  if (any(what %in% c("e0", "e0c"))) {
    0
    
  } else if (what %in% c("births")){
    "all ages"
    
  } else {
    paste(x$ages[1], "--", rev(x$ages)[1])
  }
}
