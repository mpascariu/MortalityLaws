# ------------------------------------------------- #
# Author: Marius D. Pascariu
# Last update: Mon Oct  5 23:38:09 2026
# ------------------------------------------------- #

#' Download the Australian Human Mortality Database (AHMD)
#'
#' Download detailed mortality and population data for different
#' provinces and territories in Australia, in a single object from the
#' \href{https://aushd.org/}{Australian Human Mortality Database}.
#'
#' @details
#' The Australian Human Mortality Database is a "satellite" of the Human
#' Mortality Database, built with the same methodology, so the two are
#' directly comparable. It covers Australia, its states and its territories.
#' See the AHMD website for its history and research team. The database is
#' open, so no login is needed.
#'
#' @inheritParams ReadHMD
#' @param regions Specify the region specific data you want to download by
#' adding the AHMD region code/s. Options:
#' \itemize{
#'   \item{\code{"ACT"}} -- Australian Capital Territory;
#'   \item{\code{"NSW"}} -- New South Wales;
#'   \item{\code{"NT"}} -- Northern Territory;
#'   \item{\code{"QLD"}} -- Queensland;
#'   \item{\code{"SA"}} -- South Australia;
#'   \item{\code{"TAS"}} -- Tasmania;
#'   \item{\code{"VIC"}} -- Victoria;
#'   \item{\code{"WA"}} -- Western Australia;
#'   \item{\code{NULL}} -- if \code{NULL} data for all the regions are downloaded.
#'   }
#' @return A \code{ReadAHMD} object that contains:
#'  \item{input}{List with the input values;}
#'  \item{data}{Data downloaded from AHMD;}
#'  \item{download.date}{Time stamp;}
#'  \item{years}{Numerical vector with the years covered in the data;}
#'  \item{ages}{Numerical vector with ages covered in the data.}
#' @author Marius D. Pascariu
#' @seealso
#' \code{\link{ReadHMD}}
#' \code{\link{ReadCHMD}}
#' @example inst/examples/ReadAHMD.R
#' @export
ReadAHMD <- function(what,
                     regions = NULL,
                     interval = "1x1",
                     save = FALSE,
                     show = TRUE){
  # Step 1 - Validate input
  if (is.null(regions)) {
    regions <- aus_regions()
  }

  input <- as.list(environment())
  check_input_read_ahmd(input)

  # Step 2 - Download each region
  D <- download_regions(regions  = regions,
                        what     = what,
                        interval = interval,
                        session  = NULL,
                        link     = "https://aushd.org/assets/txtFiles/humanMortality/",
                        show     = show)

  # Step 3 - Assemble the object and write a copy when asked to
  out <- if (is.null(D)) {
    NULL
  } else {
    new_read_object(data   = D,
                    input  = input,
                    prefix = "AHMD",
                    class  = "ReadAHMD",
                    show   = show)
  }

  # Exit
  return(out)
}


#' Australian region codes
#' @return A character vector with the region codes accepted by AHMD.
#' @noRd
aus_regions <- function() {
  c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA")
}


#' Check input ReadAHMD
#' @param x a list containing the input arguments from ReadAHMD function
#' @return No return value, called for checking stuff
#' @noRd
check_input_read_ahmd <- function(x) {
  check_reader_input(x = x, database = "AHMD", what_set = hmd_indices())
  check_reader_regions(regions = x$regions,
                       known   = aus_regions(),
                       label   = "region code")

  # Population is served in every interval: read_hmd_file reads the single-age
  # 'Population' file for the 1-year formats and the 5-year age group
  # 'Population5' file for the 5-year formats. Both files exist on AHMD.
  # Births have no 5-year age product, so only '1x1' can be served.
  if ((x$what == "births") & (x$interval != "1x1")) {
    stop("Data type births is not available in AHMD in the ", x$interval,
         " format. Births data is published only in the 1-year product, '1x1'.",
         call. = FALSE)
  }

  # The 5-year e0 files are missing, so only the single-age formats work.
  if ((x$what == "e0") & !(x$interval %in% c("1x1", "1x5", "1x10"))) {
    stop("Data type 'e0' is not available in AHMD in the ", x$interval,
         " format. Try one of these formats: '1x1', '1x5', '1x10'.",
         call. = FALSE)
  }
}



#' Print a ReadAHMD Object
#'
#' Prints the header of an AHMD download (web address, download date, data
#' type, interval, year and age coverage, regions) followed by the first and
#' the last rows of the data.
#' @param x An object of class \code{"ReadAHMD"}.
#' @param ... Further arguments passed to or from other methods.
#' @return The object \code{x}, invisibly. Called for its printed output.
#' @seealso \code{\link{ReadAHMD}}.
#' @keywords internal
#' @export
print.ReadAHMD <- function(x, ...){
  what <- x$input$what
  cat("Australian Human Mortality Database\n")
  cat("Web Address   : https://aushd.org\n")
  cat("Download Date :", x$download.date, "\n")
  cat("Type of data  :", what, "\n")
  cat(paste("Interval      :", x$input$interval, "\n"))
  cat(paste("Years         :", x$years[1], "--", rev(x$years)[1], "\n"))
  cat(paste("Ages          :", age_message(what, x), "\n"))
  cat("Regions       :", x$input$regions, "\n")
  cat("\nData:\n")
  print(head_tail(x$data, hlength = 5, tlength = 5))
}








