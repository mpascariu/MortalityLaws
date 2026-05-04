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
#' @return A tibble.
#' @seealso \code{\link{ReadHMD}}
#' @author Marius D. Pascariu
#' @examples
#' availableHMD()
#' 
#' @export
availableHMD <- function(link = "https://www.mortality.org/Data/DataAvailability") {
  out <- NULL
  error_message <- "Website connection failed. Please check your internet connection or the URL."
  
  # Check website connectivity
  response <- make_http_request(link)
  
  # Check if the website is accessible
  if (!is.null(response)) {
    
    if (http_status(response)$message == "Success: (200) OK") {
      # Read the HTML content of the webpage
      webpage <- read_html(response)
      
      # Extract the table from the webpage
      table_data <- html_table(webpage, fill = TRUE)
      
      # from the list of tables extracted above the table of interest is the first one:
      if (length(table_data) > 0) {
        out <- table_data[[1]]
        
      } else {
        message("No tables found on the webpage.")
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
#' @param url URL
#' @return url response
#' @keywords internal
#' 
make_http_request <- function(url) {
  response <- NULL
  
  tryCatch({
    response <- GET(url)
    
  }, error = function(e) {
    # Error: Display the error message
    paste("Error:", conditionMessage(e))
  })
  
  return(response)
}


