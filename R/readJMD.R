# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: MIT
# Last update: Thu Jun 06 16:43:49 2019
# --------------------------------------------------- #


#' Download the Japanese Mortality Database (JMD)
#'
#' Download detailed mortality and population data for different
#' provinces and territories in Australia, in a single object from the
#' \href{http://demography.cass.anu.edu.au/research/australian-human-mortality-database}{
#' Australian Human Mortality Database}.
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
#' characteristics. Therefore, the life tables by JMD do not necesarlily
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
#' @param regions Specify the region specific data you want to download by
#' adding the JMD region code/s. Options: \code{"Japan", "Hokkaido", "Aomori",
#' "Iwate", "Miyagi","Akita", "Yamagata", "Fukushima", "Ibaraki", "Tochigi",
#' "Gunma", "Saitama", "Chiba", "Tokyo", "Kanagawa", "Niigata", "Toyama",
#' "Ishikawa", "Fukui", "Yamanashi", "Nagano", "Gifu", "Shizuoka","Aichi",
#' "Mie", "Shiga", "Kyoto", "Osaka", "Hyogo", "Nara", "Wakayama", "Tottori",
#' "Shimane", "Okayama", "Hiroshima", "Yamaguchi", "Tokushima", "Kagawa",
#' "Ehime", "Kochi", "Fukuoka", "Saga", "Nagasaki", "Kumamoto", "Oita",
#' "Miyazaki", "Kagoshima", "Okinawa"}.
#' If \code{NULL} data for all the regions are downloaded
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
#' ls(JMD_Dx)
#' JMD_Dx
#'
#' # Download life tables for female population in all the states and export data.
#' LTF <- ReadJMD(what = "LT_f", interval  = "5x5", save = TRUE)
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
    region_code <- substrRight(paste0(0, match(regions[i], JPNregions()) - 1), 2)
    d <- ReadHMD.core(what = what,
                      country = region_code,
                      interval = interval,
                      username = NULL,
                      password = NULL,
                      link = "http://www.ipss.go.jp/p-toukei/JMD/")
    colnames(d)[colnames(d) == "country"] <- "region"
    d$region <- regions[i]

    D <- rbind(D, d)
  }

  fn  <- paste0("JMD_", what) # file name
  out <- list(input = input,
              data = D,
              download.date = date(),
              years = sort(unique(D$Year)),
              ages = unique(D$Age))
  out <- structure(class = "ReadJMD", out)

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
JPNregions <- function() {
  c("Japan", "Hokkaido", "Aomori", "Iwate",
    "Miyagi","Akita", "Yamagata", "Fukushima",
    "Ibaraki", "Tochigi", "Gunma", "Saitama",
    "Chiba", "Tokyo", "Kanagawa", "Niigata",
    "Toyama", "Ishikawa", "Fukui", "Yamanashi",
    "Nagano", "Gifu", "Shizuoka", "Aichi",
    "Mie", "Shiga", "Kyoto", "Osaka",
    "Hyogo", "Nara", "Wakayama", "Tottori",
    "Shimane", "Okayama", "Hiroshima", "Yamaguchi",
    "Tokushima", "Kagawa", "Ehime", "Kochi",
    "Fukuoka", "Saga", "Nagasaki", "Kumamoto",
    "Oita", "Miyazaki", "Kagoshima", "Okinawa")
}



#' Check input ReadAHMD
#' @param x a list containing the input arguments from ReadAHMD function
#' @keywords internal
check_input_ReadJMD <- function(x) {
  int <- c("1x1", "1x5", "1x10", "5x1", "5x5","5x10")
  wht <- c("births", "population", "Dx_lexis", "Ex_lexis", "Dx",
           "mx", "Ex", "LT_f", "LT_m", "LT_t", "e0",
           "mxc", "Exc", "LT_fc", "LT_mc", "LT_tc", "e0c")
  all_regions <- JPNregions()

  if (!(x$interval %in% int)) {
    stop("The interval ", x$interval, " does not exist in JMD ",
         "Try one of these options:\n", paste(int, collapse = ", "),
         call. = FALSE)
  }

  if (!(x$what %in% wht)) {
    stop(x$what, " does not exist in JMD. Try one of these options:\n",
         paste(wht, collapse = ", "), call. = FALSE)
  }

  if (all(!(x$regions %in% all_regions))) {
    stop("Something is wrong in the region codes supplied.\n",
         "Try one or more of these options:\n",
         paste(all_regions, collapse = ", "), call. = FALSE)
  }
}



#' Print ReadJMD
#' @param x An object of class \code{"ReadJMD"}
#' @param ... Further arguments passed to or from other methods.
#' @keywords internal
#' @export
print.ReadJMD <- function(x, ...){
  what <- x$input$what
  cat("Japanese Human Mortality Database\n")
  cat("Web Address   : http://www.ipss.go.jp/p-toukei/JMD/index-en.asp\n")
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









