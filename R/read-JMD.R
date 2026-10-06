# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-05 23:38:09
# --------------------------------------------

#' Download the Japanese Mortality Database (JMD)
#'
#' Download detailed mortality and population data of the 47 prefectures in
#' Japan, in a single object. The source of data is the
#' \href{https://www.ipss.go.jp/p-toukei/JMD/index-en.asp}{
#' Japanese Mortality Database}.
#'
#' @details
#' The Japanese Mortality Database is a mortality database reorganised to be
#' consistent with the Human Mortality Database, for all Japan and by
#' prefecture; see the JMD website for its research project and methods. The
#' database is open, so no login is needed. Its life tables are built to be
#' internationally comparable, so they need not match the official Japanese
#' life tables, which use a different base population and estimation method.
#'
#' The region codes are prefecture names, not the numeric JIS codes used in
#' the server's folder names; the reader maps one to the other internally.
#'
#' @inheritParams ReadHMD
#' @param what What type of data are you looking for? The following options are
#' available for JMD: \itemize{
#'   \item{\code{"births"}} -- birth records;
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
#' adding the JMD region code/s. Options: \code{"Japan", "Hokkaido", "Aomori",
#' "Iwate", "Miyagi","Akita", "Yamagata", "Fukushima", "Ibaraki", "Tochigi",
#' "Gunma", "Saitama", "Chiba", "Tokyo", "Kanagawa", "Niigata", "Toyama",
#' "Ishikawa", "Fukui", "Yamanashi", "Nagano", "Gifu", "Shizuoka","Aichi",
#' "Mie", "Shiga", "Kyoto", "Osaka", "Hyogo", "Nara", "Wakayama", "Tottori",
#' "Shimane", "Okayama", "Hiroshima", "Yamaguchi", "Tokushima", "Kagawa",
#' "Ehime", "Kochi", "Fukuoka", "Saga", "Nagasaki", "Kumamoto", "Oita",
#' "Miyazaki", "Kagoshima", "Okinawa"}.
#' If \code{NULL} data for all the regions are downloaded at once.
#' @return A \code{ReadJMD} object that contains:
#'  \item{input}{List with the input values;}
#'  \item{data}{Data downloaded from JMD;}
#'  \item{download.date}{Time stamp;}
#'  \item{years}{Numerical vector with the years covered in the data;}
#'  \item{ages}{Numerical vector with ages covered in the data.}
#' @author Marius D. Pascariu
#' @seealso
#' \code{\link{ReadHMD}}
#' \code{\link{ReadCHMD}}
#' @example inst/examples/ReadJMD.R
#' @export
ReadJMD <- function(what,
                    regions = NULL,
                    interval = "1x1",
                    save = FALSE,
                    show = TRUE){
  # Step 1 - Validate input
  if (is.null(regions)) {
    regions <- jpn_regions()
  }

  input <- as.list(environment())
  check_input_read_jmd(input)

  # Step 2 - Download each region; the folder is its JIS prefecture code
  codes <- vapply(regions, FUN = jmd_region_code, FUN.VALUE = character(1))
  D <- download_regions(regions    = unname(codes),
                        what       = what,
                        interval   = interval,
                        session    = NULL,
                        link       = "https://www.ipss.go.jp/p-toukei/JMD/",
                        label      = "region",
                        row_labels = regions,
                        show       = show)

  # Step 3 - Assemble the object and write a copy when asked to
  out <- if (is.null(D)) {
    NULL
  } else {
    new_read_object(data   = D,
                    input  = input,
                    prefix = "JMD",
                    class  = "ReadJMD",
                    show   = show)
  }

  # Exit
  return(out)
}


#' JMD region names
#' @return A character vector with the region names accepted by \code{ReadJMD()}.
#' @noRd
jpn_regions <- function() {
  out <- names(jpn_region_codes())
  return(out)
}


#' JIS prefecture codes of the JMD regions
#'
#' The JMD server names each download folder after the JIS prefecture code,
#' not after the position in any alphabetical list.
#'
#' @return A named character vector: region names to 2-digit JIS codes.
#' @noRd
jpn_region_codes <- function() {
  c(
    "Japan"     = "00",
    "Aichi"     = "23",
    "Akita"     = "05",
    "Aomori"    = "02",
    "Chiba"     = "12",
    "Ehime"     = "38",
    "Fukushima" = "07",
    "Fukui"     = "18",
    "Fukuoka"   = "40",
    "Gifu"      = "21",
    "Gunma"     = "10",
    "Hyogo"     = "28",
    "Hokkaido"  = "01",
    "Hiroshima" = "34",
    "Iwate"     = "03",
    "Ibaraki"   = "08",
    "Ishikawa"  = "17",
    "Kagawa"    = "37",
    "Kanagawa"  = "14",
    "Kagoshima" = "46",
    "Kyoto"     = "26",
    "Kochi"     = "39",
    "Kumamoto"  = "43",
    "Miyazaki"  = "45",
    "Miyagi"    = "04",
    "Mie"       = "24",
    "Nara"      = "29",
    "Nagano"    = "20",
    "Nagasaki"  = "42",
    "Niigata"   = "15",
    "Oita"      = "44",
    "Okayama"   = "33",
    "Okinawa"   = "47",
    "Osaka"     = "27",
    "Saitama"   = "11",
    "Saga"      = "41",
    "Shizuoka"  = "22",
    "Shiga"     = "25",
    "Shimane"   = "32",
    "Tochigi"   = "09",
    "Tokyo"     = "13",
    "Tokushima" = "36",
    "Toyama"    = "16",
    "Tottori"   = "31",
    "Wakayama"  = "30",
    "Yamagata"  = "06",
    "Yamaguchi" = "35",
    "Yamanashi" = "19"
    )
}


#' Download folder code of one JMD region
#' @param region A single region name, one of \code{jpn_regions()}.
#' @return The 2-digit JIS code used in the JMD download URLs.
#' @noRd
jmd_region_code <- function(region) {
  out <- unname(jpn_region_codes()[region])

  if (is.na(out)) {
    stop(
      "Unknown JMD region: ", region, ".\n",
      "Try one or more of these options:\n",
      paste(jpn_regions(), collapse = ", "),
      call. = FALSE
      )
  }

  return(out)
}



#' Data types served by the JMD server
#'
#' JMD offers no cohort products and no Lexis exposures. Its Lexis-triangle
#' deaths file exists for the national aggregate only (folder \code{00}); every
#' prefecture folder returns 404 for it, so it is not advertised. Only the data
#' types below can be downloaded by \code{ReadJMD()}.
#'
#' @return A character vector with the JMD data types.
#' @noRd
jmd_indices <- function() {
  out <- c(
    "births",
    "population",
    "Dx",
    "Ex",
    "mx",
    "LT_f",
    "LT_m",
    "LT_t",
    "e0"
    )
  return(out)
}


#' Check input ReadJMD
#' @param x a list containing the input arguments from ReadJMD function
#' @return No return value, called for validating input data
#' @noRd
check_input_read_jmd <- function(x) {
  check_reader_input(x = x, database = "JMD", what_set = jmd_indices())
  check_reader_regions(regions = x$regions,
                       known   = jpn_regions(),
                       label   = "region name")

  check_interval_read_jmd(what = x$what, interval = x$interval)
}


#' Check the intervals served by the JMD server
#'
#' A data type is accepted only in the intervals that JMD actually serves, so
#' a dead download is stopped before any request is made.
#'
#' @param what A single JMD data type, one of \code{jmd_indices()}.
#' @param interval A single interval, one of \code{data_format()}.
#' @return No return value, called for validating input data
#' @noRd
check_interval_read_jmd <- function(what, interval) {

  # JMD serves births as one annual file, Births.txt. There is no Births5.txt,
  # so a 5-year request can only be an error.
  if ((what == "births") & (interval != "1x1")) {
    stop(
      "Data type ", what, " is available in JMD only in the ",
      "'1x1' format.",
      call. = FALSE
      )
  }

  # population needs no clause here: the server serves it as two files,
  # Population.txt (one-year age groups) and Population5.txt (five-year age
  # groups, ages 0, 1-4, 5-9, ...), and both exist in every region folder.
  # read_hmd_file() picks the file from the interval, so all the intervals in
  # data_format() download.

  # Verified 2026-10-03: E0per, E0per_1x5 and E0per_1x10 exist on the server.
  # The E0per_5x1, E0per_5x5 and E0per_5x10 requests return HTTP 404.
  if ((what == "e0") & !(interval %in% c("1x1", "1x5", "1x10"))) {
    stop(
      "Data type 'e0' is available in JMD only in the following formats: ",
      "'1x1', '1x5', '1x10'. You supplied: ",
      interval,
      ".",
      call. = FALSE
      )
  }
}



#' Print a ReadJMD Object
#'
#' Prints the header of a JMD download (web address, download date, data
#' type, interval, year and age coverage, regions) followed by the first and
#' the last rows of the data.
#' @param x An object of class \code{"ReadJMD"}.
#' @param ... Further arguments passed to or from other methods.
#' @return The object \code{x}, invisibly. Called for its printed output.
#' @seealso \code{\link{ReadJMD}}.
#' @keywords internal
#' @export
print.ReadJMD <- function(x, ...){
  what <- x$input$what
  cat("Japanese Mortality Database\n")
  cat("Web Address   : https://www.ipss.go.jp/p-toukei/JMD/index-en.asp\n")
  cat("Download Date :", x$download.date, "\n")
  cat("Type of data  :", what, "\n")
  cat(paste("Interval      :", x$input$interval, "\n"))
  cat(paste("Years         :", x$years[1], "--", rev(x$years)[1], "\n"))
  cat(paste("Ages          :", age_message(what, x), "\n"))
  cat("Regions       :", x$input$regions, "\n")
  cat("\nData:\n")
  print(head_tail(x$data, hlength = 5, tlength = 5))
}









