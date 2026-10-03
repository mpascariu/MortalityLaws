# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-04 23:31:33
# --------------------------------------------

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
#'   \item{\code{"births"}} -- birth records (the 1-year product only; HMD does
#'   not publish births by 5-year age group or by multi-year periods);
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
#' which the records are aggregated. Interval options:
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
  
  # Step 2 - Log in once, then download each country with the same session
  session <- tryCatch(
    hmd_session(username = username, password = password),
    error = function(e) e
  )

  if (inherits(session, "error")) {
    message(conditionMessage(session))
    out <- NULL

  } else {
    D <- data.frame()
    for (i in 1:nr) {
      if (show) {
        setpb(pb, i)
        cat(paste("      :Downloading", countries[i], "    "))
      }

      D <- rbind(D, tryCatch(
        ReadHMD.core(
          what     = what,
          country  = countries[i],
          interval = interval,
          session  = session,
          link     = "https://www.mortality.org/File/GetDocument/hmd.v6/"
        ),
        error = function(e) {
          message("\nThe ", what, " data for ", countries[i],
                  " could not be read. The download reported: ",
                  conditionMessage(e))
          NULL
        }
      ))
    }

    if (length(D) != 0) {
      out <- list(input         = input,
                  data          = D,
                  download.date = date(),
                  years         = sort(unique(D$Year)),
                  ages          = unique(D$Age))
      out <- structure(class = "ReadHMD", out)

      # Step 3 - Write a file with the database in your working directory
      if (show) setpb(pb, nr + 1)
      if (save) {
        saveOutput(
          out    = out,
          show   = show,
          prefix = "HMD"
        )
      }

    } else {
      out <- NULL
    }
  }

  # Exit
  return(out)
}


#' Save Output in the working directory
#' @param out Output file
#' @inheritParams ReadHMD
#' @param prefix File name prefix for the saved object, e.g. "HMD".
#' @return No return value, called for side effects
#' @noRd
saveOutput <- function(out, show, prefix) {
  fn  <- paste0(prefix, "_", out$input$what) # file name
  assign(fn, value = out)
  save(list = fn, file = paste0(fn, ".Rdata"))
  if (show) saveMsg()
}


#' Print message when saving an object
#' @return No return value, called for side effects
#' @noRd
saveMsg <- function() {
  wd  <- getwd()
  n   <- nchar(wd)
  wd_ <- paste0("...", substring(wd, first = n - 45, last = n))
  message(paste("\nThe dataset is saved in your working directory:\n  ", wd_),
          appendLF = FALSE)
  message("\nDownload completed!\n")
}


#' HMD file name for a data type and interval
#' @inheritParams ReadHMD
#' @return A character string with the file name stub, without the \code{.txt}
#'   extension. \code{NULL} if the data type is not one offered by HMD.
#' @noRd
hmd_file_name <- function(what, interval) {

  if (what == "e0" & interval == "1x1") {
    which_file <- "E0per"

  } else if (what == "e0c" & interval == "1x1") {
    which_file <- "E0coh"

  } else if (what == "population" & startsWith(interval, "5")) {
    # The 5-year product is a separate file: Population5.txt, 24 rows per year
    # with the age groups 0, 1-4, 5-9, ... (verified on SWE, ACT and CAN).
    which_file <- "Population5"

  } else {
    which_file <- switch(
      what,
      births     = "Births",
      population = "Population",
      Dx_lexis   = "Deaths_lexis",
      Ex_lexis   = "Exposures_lexis",
      Dx         = paste0("Deaths_", interval),
      Ex         = paste0("Exposures_", interval),
      mx         = paste0("Mx_", interval),
      LT_f       = paste0("fltper_", interval),
      LT_m       = paste0("mltper_", interval),
      LT_t       = paste0("bltper_", interval),
      e0         = paste0("E0per_", interval),
      Exc        = paste0("cExposures_", interval),
      mxc        = paste0("cMx_", interval),
      LT_fc      = paste0("fltcoh_", interval),
      LT_mc      = paste0("mltcoh_", interval),
      LT_tc      = paste0("bltcoh_", interval),
      e0c        = paste0("E0coh_", interval)
    )
  }

  return(which_file)
}


#' Function to Download Data for a one Country
#' @inheritParams ReadHMD
#' @param country HMD country code for the selected country. Character;
#' @param session A login cookie string returned by \code{hmd_session()}, or
#'   \code{NULL} for databases that need no authentication.
#' @param link the main link to the database.
#' @return A data.frame containing demographic data, or \code{NULL} when the
#'   download fails. Failures are reported with a message, never an error.
#' @noRd
ReadHMD.core <- function(what, country, interval, session = NULL, link){

  which_file <- hmd_file_name(what = what, interval = interval)

  if (is.null(which_file)) {
    message("\n", what, " is not a data type available in HMD.\n",
            "Try one of these options:\n",
            paste(HMDindices(), collapse = ", "))
    return(NULL)
  }

  if (link %in% c("https://www.mortality.org/File/GetDocument/hmd.v6/",
                  "https://www.ipss.go.jp/p-toukei/JMD/")) {
    interlude <- "/STATS/"

  } else {
    interlude <- "/"
  }

  path <- paste0(link, country, interlude, which_file, ".txt")
  res  <- fetch_text(url = path, session = session)

  if (!is.null(res$error)) {
    message(res$error)
    return(NULL)
  }

  if (isTRUE(res$html)) {
    message("\nThe response for ", path, " is an HTML page, not a data file.",
            "\nThe login to the database failed or the session has expired.")
    return(NULL)
  }

  if (is.null(res$text)) {
    message("\nThe server returned an empty response for ", path, ".")
    return(NULL)
  }

  con <- textConnection(res$text)
  on.exit(close(con))

  dat <- tryCatch(
    read.table(con, skip = 2, header = TRUE, na.strings = "."),
    error = function(e) {
      message("\nThe ", what, " data for ", country, " in the ", interval,
              " format could not be parsed. Looked here:\n", path,
              "\nThe parser reported: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(dat)) {
    return(NULL)
  }

  out <- cbind(country, dat)

  if (any(interval %in% c("1x1", "1x5", "1x10")) &
      !any(what %in% c("births", "Dx_lexis", "Ex_lexis", "e0", "e0c")) &
      !is.null(dat$Age)) {
    # One age per row, taken as-is from the file; the open interval "110+"
    # becomes the integer 110.
    out$Age <- as.integer(sub("\\+$", "", as.character(dat$Age)))
  }

  return(out)
}


#' Country codes
#' @return A character vector with the HMD country codes.
#' @noRd
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
#' @return A character vector with the available age and time intervals.
#' @noRd
data_format <- function() c("1x1", "1x5", "1x10", "5x1", "5x5","5x10")


#' HMD Indices
#' @return A character vector with the available data types.
#' @noRd
HMDindices <- function() c("births", "population", "Dx_lexis", "Ex_lexis", "Dx",
                           "mx", "Ex", "LT_f", "LT_m", "LT_t", "e0",
                           "mxc", "Exc", "LT_fc", "LT_mc", "LT_tc", "e0c")

#' Check input ReadHMD
#' @param x a list containing the input arguments from ReadHMD function
#' @return No return value, called for validating input data
#' @noRd
check_input_ReadHMD <- function(x) {
  coh_countries <- c("DNK", "FIN", "FRATNP", "FRACNP", "ISL", "ITA", "NLD",
                     "NOR", "SWE", "CHE", "GBRTENW", "GBRCENW", "GBR_SCO")

  if (length(x$what) != 1) {
    stop("Please specify exactly one data type in 'what'. You supplied ",
         length(x$what), " values: ", paste(x$what, collapse = ", "),
         call. = FALSE)
  }

  if (length(x$interval) != 1) {
    stop("Please specify exactly one interval. You supplied ",
         length(x$interval), " values: ", paste(x$interval, collapse = ", "),
         call. = FALSE)
  }

  if (!any(x$interval %in% data_format())) {
    stop("The interval ", x$interval, " does not exist in HMD ",
         "Try one of these options:\n", paste(data_format(), collapse = ", "),
         call. = FALSE)
  }

  if (!any(x$what %in% HMDindices())) {
    stop(x$what, " does not exist in HMD. Try one of these options:\n",
         paste(HMDindices(), collapse = ", "), call. = FALSE)
  }

  bad <- x$countries[!(x$countries %in% HMDcountries())]

  if (length(bad) > 0) {
    stop("Unknown country code(s) in 'countries': ", paste(bad, collapse = ", "),
         ".\nTry one or more of these options:\n",
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

  # Availability of Births Data. The file is the annual Births.txt whatever
  # the interval says, so only the true 1-year product is accepted; a 1x5 or
  # 1x10 request would silently return the same annual rows.
  if (any(x$what == "births") & x$interval != "1x1") {
    stop("Data type births is available only in the 1-year product ('1x1'). ",
         "HMD does not publish births by 5-year age group, nor by period ",
         "intervals other than one year.",
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
#' @inheritParams ReadHMD
#' @param x An object of class \code{"ReadHMD"}.
#' @return A scalar or character indicating age groups
#' @noRd
ageMsg <- function(what, x) {
  if (any(what %in% c("e0", "e0c"))) {
    0
    
  } else if (what %in% c("births")){
    "all ages"
    
  } else {
    paste(x$ages[1], "--", rev(x$ages)[1])
  }
}
