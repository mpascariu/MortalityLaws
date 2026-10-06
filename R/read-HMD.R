# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-05 23:38:09
# --------------------------------------------

# The Human Mortality Database reader and its input check. The HTTP fetch,
# the download loop and the shared catalogues live in `readers_shared.R`; this
# file keeps only what is specific to HMD: its URL shape, the cohort and
# births availability rules, and the print method.

#' Download The Human Mortality Database (HMD)
#'
#' Download detailed mortality and population data for different countries
#' and regions in a single object from the \href{https://www.mortality.org/}{
#' Human Mortality Database}.
#'
#' @details
#' The Human Mortality Database is the reference source of detailed national
#' mortality and population data; see the project's own pages for its history
#' and research teams. A free account (and acceptance of the user agreement)
#' is required, and a dataset is only as detailed as the country publishes:
#' not every \code{what} exists for every country.
#'
#' The login is performed once per call and reused for every country, so a
#' long \code{countries} vector costs one authentication. The password is
#' never stored in the returned object; \code{input} carries everything else,
#' for reproducibility.
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
#' "AUS",   "AUT",    "BEL",   "BGR",
#' "BLR",   "CAN",    "CHL",   "HRV",
#' "HKG",   "CHE",    "CZE",   "DEUTNP",
#' "DEUTE", "DEUTW",  "DNK",   "ESP",
#' "EST",   "FIN",    "FRATNP","FRACNP",
#' "GRC",   "HUN",    "IRL",   "ISL",
#' "ISR",   "ITA",    "JPN",   "KOR",
#' "LTU",   "LUX",    "LVA",   "NLD",
#' "NOR",   "NZL_NP", "NZL_MA","NZL_NM",
#' "POL",   "PRT",    "RUS",   "SVK",
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
#' @example inst/examples/ReadHMD.R
#' @export
ReadHMD <- function(what, countries = NULL, interval = "1x1",
                    username, password, save = FALSE, show = TRUE){
  # Step 1 - Validate input
  if (is.null(countries)) {
    countries <- hmd_countries()
  }

  input <- list(what = what, countries = countries, interval = interval,
                username = username, save = save, show = show)
  check_input_read_hmd(input)

  # Step 2 - Log in once, then download each country with the same session
  session <- tryCatch(
    hmd_session(username = username, password = password),
    error = function(e) e
  )

  if (inherits(session, "error")) {
    message(conditionMessage(session))
    out <- NULL

  } else {
    D <- download_regions(regions  = countries,
                          what     = what,
                          interval = interval,
                          session  = session,
                          link     = "https://www.mortality.org/File/GetDocument/hmd.v6/",
                          show     = show)

    # Step 3 - Assemble the object and write a copy when asked to
    out <- if (is.null(D)) {
      NULL
    } else {
      new_read_object(data   = D,
                      input  = input,
                      prefix = "HMD",
                      class  = "ReadHMD",
                      show   = show)
    }
  }

  # Exit
  return(out)
}


#' Check input ReadHMD
#' @param x a list containing the input arguments from ReadHMD function
#' @return No return value, called for validating input data
#' @noRd
check_input_read_hmd <- function(x) {
  coh_countries <- c("DNK", "FIN", "FRATNP", "FRACNP", "ISL", "ITA", "NLD",
                     "NOR", "SWE", "CHE", "GBRTENW", "GBRCENW", "GBR_SCO")

  check_reader_input(x = x, database = "HMD", what_set = hmd_indices())
  check_reader_regions(regions = x$countries,
                       known   = hmd_countries(),
                       label   = "country code")

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


#' Print a ReadHMD Object
#'
#' Prints the header of an HMD download (web address, account, download date,
#' data type, interval, year and age coverage, countries) followed by the
#' first and the last rows of the data.
#' @param x An object of class \code{"ReadHMD"}.
#' @param ... Further arguments passed to or from other methods.
#' @return The object \code{x}, invisibly. Called for its printed output.
#' @seealso \code{\link{ReadHMD}}.
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
  cat(paste("Ages          :", age_message(what, x), "\n"))
  cat("Countries     :", x$input$countries, "\n")
  cat("\nData:\n")
  print(head_tail(x$data, hlength = 5, tlength = 5))
}
