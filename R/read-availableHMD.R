# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-05 23:38:09
# --------------------------------------------

#' Check Data Availability in HMD
#'
#' Returns information about the data available in the Human Mortality
#' Database (HMD), including the range of years covered by the life tables
#' for each country or region.
#'
#' The function scrapes the availability table published on the HMD site, so
#' it needs no account. It is a thin companion to \code{\link{ReadHMD}},
#' useful for checking what exists before a download; every failure (no
#' connection, a non-200 status, a body that is not a table) is reported with
#' a \code{message()} and returns \code{NULL} rather than raising an error.
#' @param link URL to the HMD available data.
#' Default: "https://www.mortality.org/Data/DataAvailability"
#' @return A data frame with one row per country or region, or \code{NULL}
#'   when the website cannot be reached or the response carries no table.
#' @seealso \code{\link{ReadHMD}}
#' @author Marius D. Pascariu
#' @example inst/examples/availableHMD.R
#' @export
availableHMD <- function(link = "https://www.mortality.org/Data/DataAvailability") {
  out <- NULL

  response <- tryCatch(
    httr::GET(url = link, config = httr::timeout(seconds = 300)),
    error = function(e) e
  )

  if (inherits(response, "condition")) {
    message("Could not connect to ", link, ": ", conditionMessage(response))
    return(out)
  }

  if (httr::status_code(response) != 200) {
    message("The website returned HTTP ", httr::status_code(response),
            ". Please check your internet connection or the URL.")
    return(out)
  }

  html <- httr::content(x = response, as = "text", encoding = "UTF-8")

  if (is.null(html) || !nzchar(html)) {
    message("The website returned an empty response.")
    return(out)
  }

  table_data <- parse_html_table(html = html)

  if (!is.null(table_data)) {
    out <- table_data

  } else {
    message("The response body could not be parsed as HTML. ")
  }

  return(out)
}


