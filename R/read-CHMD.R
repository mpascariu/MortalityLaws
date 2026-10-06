# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-05 23:38:09
# --------------------------------------------

#' Download the Canadian Human Mortality Database (CHMD)
#'
#' Download detailed mortality and population data for different
#' provinces and territories in Canada, in a single object from the
#' Canadian Human Mortality Database.
#'
#' @details
#' The Canadian Human Mortality Database is a "satellite" of the Human
#' Mortality Database, built with the same methodology, so the two are
#' directly comparable. It covers Canada, its provinces and its territories.
#' See the CHMD website for its history and research team; the data are
#' validated and corrected for the period it covers.
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
#' @example inst/examples/ReadCHMD.R
#' @export
ReadCHMD <- function(what,
                     regions = NULL,
                     interval = "1x1",
                     save = FALSE,
                     show = TRUE){
  # Step 1 - Validate input
  if (is.null(regions)) {
    regions <- can_regions()
  }

  input <- as.list(environment())
  check_input_read_chmd(input)

  # Step 2 - Download each region
  D <- download_regions(regions  = regions,
                        what     = what,
                        interval = interval,
                        session  = NULL,
                        link     = "https://www.prdh.umontreal.ca/BDLC/data/",
                        show     = show)

  # Step 3 - Assemble the object and write a copy when asked to
  out <- if (is.null(D)) {
    NULL
  } else {
    new_read_object(data   = D,
                    input  = input,
                    prefix = "CHMD",
                    class  = "ReadCHMD",
                    show   = show)
  }

  # Exit
  return(out)
}


#' Canadian region codes
#' @return A character vector with the region codes accepted by CHMD.
#' @noRd
can_regions <- function() {
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


#' Check input for ReadCHMD
#' @param x A list with the input values for ReadCHMD
#' @return No return value, called for input validation
#' @noRd
check_input_read_chmd <- function(x) {
  wht <- c("births", "population", "Dx_lexis", "Dx",
           "mx", "Ex", "LT_f", "LT_m", "LT_t", "e0")

  check_reader_input(x = x, database = "CHMD", what_set = wht)
  check_reader_regions(regions = x$regions,
                       known   = can_regions(),
                       label   = "region code")

  check_availability_read_chmd(what     = x$what,
                               regions  = x$regions,
                               interval = x$interval)
}


#' Check the CHMD data types against the requested region and interval
#' @param what A single CHMD data type, one of the options in
#'   \code{check_input_read_chmd()}.
#' @param regions A character vector with the requested CHMD region codes.
#' @param interval A single interval, one of \code{data_format()}.
#' @return No return value, called for input validation
#' @noRd
check_availability_read_chmd <- function(what, regions, interval) {

  # Availability of Death and Exposures
  if ((what %in% c("Dx", "Ex")) & !(interval %in% c("1x1", "5x1"))) {
    stop("Data type ", what,
         " is available only in the following format: '1x1' and '5x1'.",
         call. = FALSE)
  }

  # Population is served in every interval: read_hmd_file reads the single-age
  # 'Population' file for the 1-year formats and the 5-year age group
  # 'Population5' file for the 5-year formats. Both files exist on CHMD.
  # Births have no 5-year age product, so only '1x1' can be served.
  if ((what == "births") & (interval != "1x1")) {
    stop("Data type births is not available in CHMD in the ", interval,
         " format. Births data is published only in the 1-year product, '1x1'.",
         call. = FALSE)
  }

  if (any(regions %in% c("NWT", "YUK")) &
    (what %in% c("LT_m", "LT_f", "LT_t")) &
      (interval %in% c("1x1", "5x1"))) {
    stop("For the regions of Northwest Territories & Nunavut (NWT) and Yukon (YUK),",
         "\ndata type ", what, " is NOT available in the following format:",
         "'1x1' and '5x1'.",
         "\nTo download the life-tables for all the other regions use the argument:",
         "\nregions = c('CAN', 'NFL', 'PEI', 'NSC', 'NBR', 'QUE', 'ONT', 'MAN', 'SAS', 'ALB', 'BCO')",
         call. = FALSE)
  }
}



#' Print a ReadCHMD Object
#'
#' Prints the header of a CHMD download (web address, download date, data
#' type, interval, year and age coverage, regions) followed by the first and
#' the last rows of the data.
#' @param x An object of class \code{"ReadCHMD"}.
#' @param ... Further arguments passed to or from other methods.
#' @return The object \code{x}, invisibly. Called for its printed output.
#' @seealso \code{\link{ReadCHMD}}.
#' @keywords internal
#' @export
print.ReadCHMD <- function(x, ...){
  what <- x$input$what
  cat("Canadian Human Mortality Database\n")
  cat("Web Address   : https://www.prdh.umontreal.ca/BDLC/\n")
  cat("Download Date :", x$download.date, "\n")
  cat("Type of data  :", what, "\n")
  cat(paste("Interval      :", x$input$interval, "\n"))
  cat(paste("Years         :", x$years[1], "--", rev(x$years)[1], "\n"))
  cat(paste("Ages          :", age_message(what, x), "\n"))
  cat("Regions       :", x$input$regions, "\n")
  cat("\nData:\n")
  print(head_tail(x$data, hlength = 5, tlength = 5))
}








