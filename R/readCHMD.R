# -------------------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last Update: Thu Jul 20 20:34:07 2023
# -------------------------------------------------------------- #

#' Download the Canadian Human Mortality Database (CHMD)
#'
#' Download detailed mortality and population data for different
#' provinces and territories in Canada, in a single object from the
#' Canadian Human Mortality Database.
#'
#' @details
#' (Description taken from the CHMD website).
#'
#' The Canadian Human Mortality Database (CHMD) was created to provide detailed
#' Canadian mortality and population data to researchers, students, journalists,
#' policy analysts, and others interested in the history of human longevity.
#' The project is an achievement of the Mortality and Longevity research team at
#' the Department of Demography, Universite de Montreal, under the supervision
#' of Professor Robert Bourbeau, in collaboration with demographers at the
#' Max Plank Institute for Demographic Research (Rostock, Germany) and the
#' Department of Demography, University of California at Berkeley.
#' Nadine Ouellette, researcher at the Institut national d'etudes demographiques
#' in Paris and member of the Mortality and Longevity research team at the
#' Universite de Montreal, is in charge of computing all CHMD life tables and
#' updating the CHMD web site.
#'
#' The CHMD is a "satellite" of the Human Mortality Database (HMD), an
#' international database which currently holds detailed data for multiple
#' countries or regions. Consequently, the CHMD's underlying methodology
#' corresponds to the one used for the HMD.
#'
#' The CHMD gathers all required data (deaths counts, births counts, population
#' size, exposure-to-risk, death rates) to compute life tables for Canada,
#' its provinces and its territories. One of the great advantages of the
#' database is to include data that is validated and corrected, when required,
#' and rendered comparable, if possible, for the period ranging from 1921
#' thru 2011. For comparison purposes, various life tables published by
#' governmental organizations are also available for download in PDF format.
#'
#' @inheritParams ReadHMD
#' @param what What type of data are you looking for? The following options are
#' available: \itemize{
#'   \item{\code{"births"}} -- birth records;
#'   \item{\code{"Dx_lexis"}} -- deaths by Lexis triangles;
#'   \item{\code{"population"}} -- population size;
#'   \item{\code{"Dx"}} -- death counts;
#'   \item{\code{"Ex"}} -- exposure-to-risk;
#'   \item{\code{"mx"}} -- central death-rates;
#'   \item{\code{"LT_f"}} -- period life tables for females;
#'   \item{\code{"LT_m"}} -- period life tables for males;
#'   \item{\code{"LT_t"}} -- period life tables both sexes combined;
#'   \item{\code{"e0"}} -- period life expectancy at birth;
#'   }
#' @param regions Specify the region specific data you want to download by
#' adding the CHMD region code/s. Options:
#' \itemize{
#'   \item{\code{"CAN"}} -- Canada - Sum of Canadian provinces and territories;
#'   \item{\code{"NFL"}} -- Newfoundland & Labrador;
#'   \item{\code{"PEI"}} -- Prince Edward Island;
#'   \item{\code{"NSC"}} -- Nova Scotia;
#'   \item{\code{"NBR"}} -- New Brunswick;
#'   \item{\code{"QUE"}} -- Quebec;
#'   \item{\code{"ONT"}} -- Ontario;
#'   \item{\code{"MAN"}} -- Manitoba;
#'   \item{\code{"SAS"}} -- Saskatchewan;
#'   \item{\code{"ALB"}} -- Alberta;
#'   \item{\code{"BCO"}} -- British Columbia;
#'   \item{\code{"NWT"}} -- Northwest Territories & Nunavut;
#'   \item{\code{"YUK"}} -- Yukon;
#'   \item{\code{NULL}} -- if \code{NULL} data for all the regions are downloaded.
#'   }
#' @return A \code{ReadCHMD} object that contains:
#'  \item{input}{List with the input values;}
#'  \item{data}{Data downloaded from CHMD;}
#'  \item{download.date}{Time stamp;}
#'  \item{years}{Numerical vector with the years covered in the data;}
#'  \item{ages}{Numerical vector with ages covered in the data.}
#' @author Marius D. Pascariu
#' @seealso
#' \code{\link{ReadHMD}}
#' \code{\link{ReadAHMD}}
#' @examples
#' \donttest{
#' # Download demographic data for Quebec and Saskatchewan regions in 1x1 format
#'
#' # Death counts. We don't want to export data outside R.
#' CHMD_Dx <- ReadCHMD(what = "Dx",
#'                     regions = c('QUE', 'SAS'),
#'                     interval  = "1x1",
#'                     save = FALSE)
#'
#' # Download life tables for female population. To export data use save = TRUE.
#' LTF <- ReadCHMD(what = "LT_f",
#'                 regions = c('QUE', 'SAS'),
#'                 interval  = "1x1",
#'                 save = FALSE)
#' }
#' @export
ReadCHMD <- function(what,
                     regions = NULL,
                     interval = "1x1",
                     save = FALSE,
                     show = TRUE){
  # Step 1 - Validate input & Progress bar setup
  if (is.null(regions)) {
    regions <- CANregions()
  }

  input <- as.list(environment())
  check_input_ReadCHMD(input)
  nr <- length(regions)

  if (show) {
    pb <- startpb(0, nr + 1)
    on.exit(closepb(pb))
    setpb(pb, 0)
  }

  # Step 2 - Do the loop for the other regions
  D <- data.frame()
  for (i in 1:nr) {
    if (show) {
      setpb(pb, i)
      cat(paste("      :Downloading", regions[i], "    "))
    }

    D <- rbind(D, ReadHMD.core(
      what = what,
      country = regions[i],
      interval = interval,
      username = NULL,
      password = NULL,
      link = "https://www.prdh.umontreal.ca/BDLC/data/"))
  }

  out <- list(input = input,
              data = D,
              download.date = date(),
              years = sort(unique(D$Year)),
              ages = unique(D$Age))
  out <- structure(class = "ReadCHMD", out)

  # Step 3 - Write a file with the database in your working directory
  if (show) setpb(pb, nr + 1)
  if (save) saveOutput(out, show, prefix = "CHMD")

  # Exit
  return(out)
}


#' Country codes
#' @return a vector
#' @keywords internal
CANregions <- function() {
  c("CAN",
    "NFL",
    "PEI",
    "NSC",
    "NBR",
    "QUE",
    "ONT",
    "MAN",
    "SAS",
    "ALB",
    "BCO",
    "NWT",
    "YUK")
}


#' Check input ReadHMD
#' @param x a list containing the input arguments from ReadHMD function
#' @return No return value, called for input validation 
#' @keywords internal
#' 
check_input_ReadCHMD <- function(x) {
  wht <- c("births", "population", "Dx_lexis", "Dx",
           "mx", "Ex", "LT_f", "LT_m", "LT_t", "e0")

  if (!(x$interval %in% data_format())) {
    stop("The interval ", x$interval, " does not exist in HMD ",
         "Try one of these options:\n", paste(data_format(), collapse = ", "),
         call. = FALSE)
  }

  if (!(x$what %in% wht)) {
    stop(x$what, " does not exist in CHMD. Try one of these options:\n",
         paste(wht, collapse = ", "), call. = FALSE)
  }

  if (all(!(x$regions %in% CANregions()))) {
    stop("Something is wrong in the region codes supplied.\n",
         "Try one or more of these options:\n",
         paste(CANregions(), collapse = ", "), call. = FALSE)
  }

  # Availability of Death and Exposures
  if ((x$what %in% c("Dx", "Ex")) & !(x$interval %in% c("1x1", "5x1"))) {
    stop("Data type ", x$what,
         " is available only in the following format: '1x1' and '5x1'.",
         call. = FALSE)
  }

  if (any(x$region %in% c("NWT", "YUK")) &
    (x$what %in% c("LT_m", "LT_f", "LT_t")) &
      (x$interval %in% c("1x1", "5x1"))) {
    stop("For the regions of Northwest Territories & Nunavut (NWT) and Yukon (YUK),",
         "\ndata type ", x$what, " is NOT available in the following format:",
         "'1x1' and '5x1'.",
         "\nTo download the life-tables for all the other regions use the argument:",
         "\nregions = c('CAN', 'NFL', 'PEI', 'NSC', 'NBR', 'QUE', 'ONT', 'MAN', 'SAS', 'ALB', 'BCO')",
         call. = FALSE)
  }
}



#' Print ReadCHMD
#' @param x An object of class \code{"ReadCHMD"}
#' @param ... Further arguments passed to or from other methods.
#' @return Print data on the console
#' @keywords internal
#' @export
print.ReadCHMD <- function(x, ...){
  what <- x$input$what
  cat("Canadian Human Mortality Database\n")
  cat("Web Address   : https://www.bdlc.umontreal.ca/chmd\n")
  cat("Download Date :", x$download.date, "\n")
  cat("Type of data  :", what, "\n")
  cat(paste("Interval      :", x$input$interval, "\n"))
  cat(paste("Years         :", x$years[1], "--", rev(x$years)[1], "\n"))
  cat(paste("Ages          :", ageMsg(what, x), "\n"))
  cat("Regions       :", x$input$regions, "\n")
  cat("\nData:\n")
  print(head_tail(x$data, hlength = 5, tlength = 5))
}








