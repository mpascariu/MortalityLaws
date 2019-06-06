# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: MIT
# Last update: Thu Jun 06 11:40:07 2019
# --------------------------------------------------- #


#' Download the Australian Human Mortality Database (AHMD)
#'
#' Download detailed mortality and population data for different
#' provinces and territories in Australia, in a single object from the
#' \href{http://demography.cass.anu.edu.au/research/australian-human-mortality-database}{
#' Australian Human Mortality Database}.
#'
#' @details
#' (Description taken from the AHMD website).
#'
#' The Australian Human Mortality Database (AHMD) was created to provide
#' detailed Australian mortality and population data to researchers, students,
#' journalists, policy analysts, and others interested in the history of
#' human longevity. The project is an achievement of the Mortality,
#' Ageing & Health research team in the ANU School of Demography under the
#' supervision of Associate Professor Vladimir Canudas-Romo, in collaboration
#' with demographers at the Max Plank Institute for Demographic Research
#' (Rostock, Germany) and the Department of Demography, University of
#' California at Berkeley.
#'
#' The AHMD is a "satellite" of the Human Mortality Database (HMD),
#' an international database which currently holds detailed data for multiple
#' countries or regions. Consequently, the AHMD's underlying methodology
#' corresponds to the one used for the HMD.
#'
#' The AHMD gathers all required data (deaths counts, births counts,
#' population size, exposure-to-risk, death rates) to compute life tables
#' for Australia, its states and its territories. One of the great advantages
#' of the database is to include data that is validated and corrected, when
#' required, and rendered comparable, if possible, for the period ranging
#' from 1971 thru 2016. For comparison purposes, various life tables published
#' by governmental organizations are also available for download in PDF format.
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
#' @examples
#' \dontrun{
#' # Download demographic data for Australian Capital Territory and
#' # Tasmania regions in 5x1 format
#'
#' # Death counts. We don't want to export data outside R.
#' AHMD_Dx <- ReadAHMD(what = "Dx",
#'                     regions = c('ACT', 'TAS'),
#'                     interval  = "5x1",
#'                     save = FALSE)
#' ls(AHMD_Dx)
#' AHMD_Dx
#'
#' # Download life tables for female population in all the states and export data.
#' LTF <- ReadAHMD(what = "LT_f", interval  = "5x1", save = TRUE)
#' LTF
#' }
#' @export
ReadAHMD <- function(what,
                     regions = NULL,
                     interval = "1x1",
                     save = FALSE,
                     show = TRUE){
  # Step 1 - Validate input & Progress bar setup
  if (is.null(regions)) {
    regions <- AUSregions()
  }

  input <- as.list(environment())
  check_input_ReadAHMD(input)
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

    D <- rbind(D, ReadHMD.core(what = what,
                               country = regions[i],
                               interval = interval,
                               username = NULL,
                               password = NULL,
                               link = "http://demography.cass.anu.edu.au/sites/default/ahmd/"))
  }

  fn  <- paste0("AHMD_", what) # file name
  out <- list(input = input,
              data = D,
              download.date = date(),
              years = sort(unique(D$Year)),
              ages = unique(D$Age))
  out <- structure(class = "ReadAHMD", out)

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
      message(paste(fn, "is saved in your working directory:\n  ", wd_),
              appendLF = FALSE)
      cat("\n   ")
    }

    message("Download completed!")
  }

  # Exit
  return(out)
}


#' region codes
#' @keywords internal
AUSregions <- function() {
  c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA")
}


#' Check input ReadAHMD
#' @param x a list containing the input arguments from ReadAHMD function
#' @keywords internal
check_input_ReadAHMD <- function(x) {
  int <- c("1x1", "1x5", "1x10", "5x1", "5x5","5x10")
  wht <- c("births", "population", "Dx_lexis", "Ex_lexis", "Dx",
           "mx", "Ex", "LT_f", "LT_m", "LT_t", "e0",
           "mxc", "Exc", "LT_fc", "LT_mc", "LT_tc", "e0c")
  all_regions <- AUSregions()

  if (!(x$interval %in% int)) {
    stop("The interval ", x$interval, " does not exist in AHMD ",
         "Try one of these options:\n", paste(int, collapse = ", "),
         call. = FALSE)
  }

  if (!(x$what %in% wht)) {
    stop(x$what, " does not exist in AHMD. Try one of these options:\n",
         paste(wht, collapse = ", "), call. = FALSE)
  }

  if (all(!(x$regions %in% all_regions))) {
    stop("Something is wrong in the region codes supplied.\n",
         "Try one or more of these options:\n",
         paste(all_regions, collapse = ", "), call. = FALSE)
  }
}



#' Print ReadCHMD
#' @param x An object of class \code{"ReadCHMD"}
#' @param ... Further arguments passed to or from other methods.
#' @keywords internal
#' @export
print.ReadAHMD <- function(x, ...){
  what <- x$input$what
  cat("Australian Human Mortality Database\n")
  cat("Web Address   : http://demography.cass.anu.edu.au/research/australian-human-mortality-database\n")
  cat("Download Date :", x$download.date, "\n")
  cat("Type of data  :", what, "\n")
  cat(paste("Interval      :", x$input$interval, "\n"))

  if (what %in% c("e0", "e0c")) {
    ageMsg <- 0

  } else {
    ageMsg <- paste(x$ages[1], "--", rev(x$ages)[1])
  }

  cat(paste("Years   :", x$years[1], "--", rev(x$years)[1], "\n"))
  cat(paste("Ages    :", ageMsg, "\n"))
  cat("Regions :", x$input$regions, "\n")
  cat("\nData:\n")
  print(head_tail(x$data, hlength = 5, tlength = 5))
}








