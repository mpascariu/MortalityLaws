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
#' @return A tibble with one row per country or region. Returns \code{NULL}
#' when the website cannot be reached, when the response status is not 200,
#' or when the response body cannot be parsed as HTML. Every failure also
#' emits a \code{message()}.
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
  error_message <- "Website connection failed. Please check your internet connection or the URL."
  
  # Check website connectivity
  response <- make_http_request(link)
  
  # Check if the website is accessible
  if (!is.null(response)) {
    
    if (status_code(response) == 200) {
      # A 200 response can still carry a body that is not HTML.
      webpage <- tryCatch(
        read_html(x = response),
        error = function(e) NULL
      )
      
      if (!is.null(webpage)) {
        # Extract the table from the webpage
        table_data <- html_table(x = webpage, fill = TRUE)
        
        # from the list of tables extracted above the table of interest is the first one:
        if (length(table_data) > 0) {
          out <- table_data[[1]]
          
        } else {
          message("No tables found on the webpage.")
        }
        
      } else {
        content_type <- httr::headers(response)[["content-type"]]
        if (is.null(content_type) || is.na(content_type)) content_type <- "unknown"
        message(
          "The response body could not be parsed as HTML. ",
          "The response reported content-type: ", content_type, "."
        )
      }
      
    } else {
      message(error_message)
    }
    
  } else { 
    message(error_message)
  }
  
  return(out)
}



#' Make HTTP request
#'
#' Returns the \code{httr} response for \code{url}, or \code{NULL} when the
#' request raises an error.
#' @param url URL
#' @return An \code{httr} response, or \code{NULL} if the request fails.
#' @noRd
#' 
make_http_request <- function(url) {
  response <- NULL
  
  tryCatch({
    response <- GET(url = url)
    
  }, error = function(e) {
    # Error: Display the error message
    paste("Error:", conditionMessage(e))
  })
  
  return(response)
}


