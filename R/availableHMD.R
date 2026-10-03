# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-04 23:38:13
# --------------------------------------------

#' Check Data Availability in HMD
#'
#' Returns information about the data available in the Human Mortality 
#' Database (HMD), including the range of years covered by the life tables 
#' for each country or region.
#' @param link URL to the HMD available data.
#' Default: "https://www.mortality.org/Data/DataAvailability"
#' @return A data frame with one row per country or region. Returns
#'   \code{NULL} when the website cannot be reached, when the response
#'   status is not 200, or when the response body carries no HTML table.
#'   Every failure also emits a \code{message()}.
#' @seealso \code{\link{ReadHMD}}
#' @author Marius D. Pascariu
#' @examples
#' \dontrun{
#' availableHMD()
#' }
#' 
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


