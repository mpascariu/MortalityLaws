# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# License: MIT
# Last update: Tue Jun 04 17:13:45 2019
# --------------------------------------------------- #


#' Download Canadian Human Mortality Database (CHMD)
#'
#' Download detailed mortality and population data for different
#' provinces and territories in Canada, in a single object from the
#' \href{http://www.bdlc.umontreal.ca/chmd/index.htm}{
#' Canadian Human Mortality Database}.
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
#' @param regions Specify the region specific data you want to download by adding the
#' CHMD region code/s. Options:
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
#'   \item{\code{"YUK"}} -- Yukon.
#'   }
#' @return A \code{ReadCHMD} object that contains:
#'  \item{input}{List with the input values;}
#'  \item{data}{Data downloaded from CHMD;}
#'  \item{download.date}{Time stamp;}
#'  \item{years}{Numerical vector with the years covered in the data;}
#'  \item{ages}{Numerical vector with ages covered in the data.}
#' @author Marius D. Pascariu
#' @examples
#' \dontrun{
#' # Download demographic data for Quebec and Saskatchewan regions in 1x1 format
#'
#' # Death counts. We don't want to export data outside R.
#' CHMD_Dx <- ReadCHMD(what = "Dx",
#'                     regions = c('QUE', 'SAS'),
#'                     interval  = "1x1",
#'                     save = FALSE)
#' ls(CHMD_Dx)
#' CHMD_Dx
#'
#' # Download life tables for female population and export data.
#' LTF <- ReadCHMD(what = "LT_f",
#'                 regions = c('QUE', 'SAS'),
#'                 interval  = "1x1",
#'                 save = TRUE)
#' LTF
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

  input <- list(what = what,
                regions = regions,
                interval = interval,
                save = save,
                show = show)

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

    D <- rbind(D, ReadCHMD.core(what = what, region = regions[i], interval = interval))
  }

  fn  <- paste0("CHMD_", what) # file name
  out <- list(input = input,
              data = D,
              download.date = date(),
              years = sort(unique(D$Year)),
              ages = unique(D$Age))
  out <- structure(class = "ReadCHMD", out)

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


#' Function to Download Data for one region
#' @inheritParams ReadCHMD
#' @param region CHMD region code. Character.
#' @param username ignored;
#' @param password ignored;
#' @keywords internal
ReadCHMD.core <- function(what,
                          region,
                          interval,
                          username = NULL,
                          password = NULL){

  if (what == "e0") {
    whichFile <- "E0per.txt"  # Life expectancy at birth

  } else {
    whichFile <- switch(what,
                        births = "Births",
                        population = "Population",
                        Dx_lexis = "Deaths_lexis",
                        # Ex_lexis = "Exposures_lexis",
                        Dx   = paste0("Deaths_", interval),    # deaths
                        Ex   = paste0("Exposures_", interval), # exposure
                        mx   = paste0("Mx_", interval),        # death rates
                        LT_f = paste0("fltper_", interval),    # Life tables, Females
                        LT_m = paste0("mltper_", interval),    # Life tables, Males
                        LT_t = paste0("bltper_", interval))}   # Life tables, Both sexes
                        # Cohort data ...

  path <- paste0("www.prdh.umontreal.ca/BDLC/data/", region, "/", whichFile, ".txt")
  txt  <- RCurl::getURL(url = path)

  con  <- try(textConnection(txt),
              stop("ReadCHMD() failed to connect to <www.prdh.umontreal.ca/BDLC/> ",
                   "Maybe the website is down at this moment?", call. = FALSE))
  dat  <- try(read.table(con, skip = 2, header = TRUE, na.strings = "."),
              stop("Unknown error.", call. = FALSE))

  close(con)
  out <- cbind(region, dat)
  if (interval %in% c("1x1", "1x5", "1x10") &
      !(what %in% c("births", "Dx_lexis", "Ex_lexis", "e0", "e0c"))) {
    out$Age <- 0:110
  }
  return(out)
}


#' Country codes
#' @keywords internal
CANregions <- function() {
  c("CAN", "NFL", "PEI", "NSC", "NBR", "QUE", "ONT",
    "MAN", "SAS", "ALB", "BCO", "NWT", "YUK")
}


#' Check input ReadHMD
#' @param x a list containing the input arguments from ReadHMD function
#' @keywords internal
check_input_ReadCHMD <- function(x) {
  int <- c("1x1", "1x5", "1x10", "5x1", "5x5","5x10")
  wht <- c("births", "population", "Dx_lexis", "Dx",
           "mx", "Ex", "LT_f", "LT_m", "LT_t", "e0")
  all_regions <- CANregions()

  if (!(x$interval %in% int)) {
    stop("The interval ", x$interval, " does not exist in HMD ",
         "Try one of these options:\n", paste(int, collapse = ", "),
         call. = FALSE)
  }

  if (!(x$what %in% wht)) {
    stop(x$what, " does not exist in HMD. Try one of these options:\n",
         paste(wht, collapse = ", "), call. = FALSE)
  }

  if (all(!(x$regions %in% all_regions))) {
    stop("Something is wrong in the region codes supplied.\n",
         "Try one or more of these options:\n",
         paste(all_regions, collapse = ", "), call. = FALSE)
  }

  # Availability of Cohort Data

  # Availability of Life Expectancy Data
  if ((x$what %in% c("e0", "Births")) & (x$interval != "1x1")) {
    stop("Data type ", x$what,
         " is available only in the following format: '1x1'.",
         call. = FALSE)
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
    stop("For the regions of Newfoundland & Labrador and Yukon,",
         " due to small size population, data type ", x$what,
         " is NOT available in the following format: '1x1' and '5x1'.",
         call. = FALSE)
  }
}



#' Print ReadCHMD
#' @param x An object of class \code{"ReadCHMD"}
#' @param ... Further arguments passed to or from other methods.
#' @keywords internal
#' @export
print.ReadCHMD <- function(x, ...){
  what <- x$input$what
  cat("Canadian Human Mortality Database (http://www.bdlc.umontreal.ca/chmd)\n")
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








