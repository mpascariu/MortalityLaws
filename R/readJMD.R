# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-04 23:31:48
# --------------------------------------------

#' Download the Japanese Mortality Database (JMD)
#'
#' Download detailed mortality and population data of the 47 prefectures in
#' Japan, in a single object. The source of data is the
#' \href{https://www.ipss.go.jp/p-toukei/JMD/index-en.asp}{
#' Japanese Mortality Database}.
#'
#' @details
#' (Description taken from the JMD website).
#'
#' The Japanese Mortality Database is a comprehensively-reorganized mortality
#' database that is optimized for mortality research and consistent with the
#' Human Mortality Database. This database is provided as a part of the research
#' project "Demographic research on the causes and the socio-economic
#' consequence of longetivity extension in Japan" (2011-2013), "Demographic
#' research on longevity extension, population aging, and their effects on the
#' social security and socio-economic structures in Japan" (2014-2016), and
#' "Comprehensive research from a demographic viewpoint on the longevity
#' revolution" (2017-2019) at the National Institute of Population and Social
#' Security Research.
#'
#' The Japanese Mortality Database is designed to provide the life tables to all
#' the people who are interested in Japanese mortality including domestic and
#' foreign mortality researchers for the purpose of mortality research.
#' Especially because we have structured it to conform with the HMD, our
#' database is suitable for international comparison, we put emphasis on the
#' compatibility with the HMD more than our country's particular
#' characteristics. Therefore, the life tables by JMD do not necessarily
#' exhibit the same values as ones by the official life tables prepared and
#' released by the Statistics and Information Department, Minister's
#' Secretariat, Ministry of Health, Labor and Welfare according to the different
#' base population or the methods for estimating the tables. When doing things
#' other than mortality research, if life table that statistically displays our
#' country's mortality situation is necessary, please use the official life
#' table that has been prepared by the Statistics and Information Department,
#' Minister's Secretariat, Ministry of Health, Labor and Welfare.
#'
#' At the present time, we offer the data for All Japan and by prefecture.
#' The project team is studying the methodology for estimating life tables
#' along with data preparation. Therefore, the data may be updated when a
#' new methodology is adopted. Please refer to "Methods" for further
#' information.
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
#' @examples
#' \dontrun{
#' # Download demographic data for Fukushima and Tokyo regions in 1x1 format
#'
#' # Death counts. We don't want to export data outside R.
#' JMD_Dx <- ReadJMD(what = "Dx",
#'                   regions = c('Fukushima', 'Tokyo'),
#'                   interval  = "1x1",
#'                   save = FALSE)
#' JMD_Dx
#'
#' # Download life tables for female population in all the states and export data.
#' LTF <- ReadJMD(what = "LT_f", interval  = "5x5", save = FALSE)
#' LTF
#' }
#' @export
ReadJMD <- function(what,
                    regions = NULL,
                    interval = "1x1",
                    save = FALSE,
                    show = TRUE){
  # Step 1 - Validate input & Progress bar setup
  if (is.null(regions)) {
    regions <- JPNregions()
  }

  input <- as.list(environment())
  check_input_ReadJMD(input)
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
    region_code <- JMDregion_code(regions[i])
    d <- ReadHMD.core(
      what     = what,
      country  = region_code,
      interval = interval,
      session  = NULL,
      link     = "https://www.ipss.go.jp/p-toukei/JMD/")

    if (!is.null(d)) {
      # The folder code alone cannot label the rows, so use the region name.
      d[["country"]] <- regions[i]
      colnames(d)[colnames(d) == "country"] <- "region"
      D <- rbind(D, d)
    }
  }

  if (length(D) != 0) {
    
    out <- list(
      input = input,
      data = D,
      download.date = date(),
      years = sort(unique(D$Year)),
      ages = unique(D$Age)
      )
    out <- structure(class = "ReadJMD", out)
  
    # Step 3 - Write a file with the database in your working directory
    if (show) setpb(pb, nr + 1)
    if (save) saveOutput(out, show, prefix = "JMD")
    
  } else {
    out <- NULL
  }

  # Exit
  return(out)
}


#' JMD region names
#' @return A character vector with the region names accepted by \code{ReadJMD()}.
#' @noRd
JPNregions <- function() {
  out <- names(JPNregion_codes())
  return(out)
}


#' JIS prefecture codes of the JMD regions
#'
#' The JMD server names each download folder after the JIS prefecture code,
#' not after the position in any alphabetical list.
#'
#' @return A named character vector: region names to 2-digit JIS codes.
#' @noRd
JPNregion_codes <- function() {
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
#' @param region A single region name, one of \code{JPNregions()}.
#' @return The 2-digit JIS code used in the JMD download URLs.
#' @noRd
JMDregion_code <- function(region) {
  out <- unname(JPNregion_codes()[region])

  if (is.na(out)) {
    stop(
      "Unknown JMD region: ", region, ".\n",
      "Try one or more of these options:\n",
      paste(JPNregions(), collapse = ", "),
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
JMDindices <- function() {
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
check_input_ReadJMD <- function(x) {

  if (length(x$what) != 1) {
    stop(
      "Please specify exactly one data type in 'what'. You supplied ",
      length(x$what), " values: ",
      paste(x$what, collapse = ", "),
      call. = FALSE
      )
  }

  if (length(x$interval) != 1) {
    stop(
      "Please specify exactly one interval. You supplied ",
      length(x$interval), " values: ",
      paste(x$interval, collapse = ", "),
      call. = FALSE
      )
  }

  if (length(x$regions) == 0) {
    stop(
      "Please specify at least one region in 'regions'.",
      call. = FALSE
      )
  }

  if (!(x$interval %in% data_format())) {
    stop(
      "The interval ",
      x$interval,
      " does not exist in JMD. ",
      "Try one of these options:\n",
      paste(data_format(), collapse = ", "),
      call. = FALSE
      )
  }

  bad <- x$regions[!(x$regions %in% JPNregions())]

  if (length(bad) > 0) {
    stop(
      "Unknown region name(s) in 'regions': ",
      paste(bad, collapse = ", "),
      ".\nTry one or more of these options:\n",
      paste(JPNregions(), collapse = ", "),
      call. = FALSE
      )
  }

  if (!(x$what %in% JMDindices())) {
    stop(
      x$what,
      " does not exist in JMD. Try one of these options:\n",
      paste(JMDindices(), collapse = ", "),
      call. = FALSE
      )
  }

  check_interval_ReadJMD(what = x$what, interval = x$interval)
}


#' Check the intervals served by the JMD server
#'
#' A data type is accepted only in the intervals that JMD actually serves, so
#' a dead download is stopped before any request is made.
#'
#' @param what A single JMD data type, one of \code{JMDindices()}.
#' @param interval A single interval, one of \code{data_format()}.
#' @return No return value, called for validating input data
#' @noRd
check_interval_ReadJMD <- function(what, interval) {

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
  # ReadHMD.core() picks the file from the interval, so all the intervals in
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



#' Print ReadJMD
#' @param x An object of class \code{"ReadJMD"}
#' @param ... Further arguments passed to or from other methods.
#' @return Print info on the console
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
  cat(paste("Ages          :", ageMsg(what, x), "\n"))
  cat("Regions       :", x$input$regions, "\n")
  cat("\nData:\n")
  print(head_tail(x$data, hlength = 5, tlength = 5))
}









