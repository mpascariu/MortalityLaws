# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-04 17:46:32
# --------------------------------------------

# One HTTP layer for the database readers: the HMD form login and a text
# fetch that reports failures as data instead of raising.

#' Log in to the Human Mortality Database
#'
#' The HMD login is an ASP.NET cookie-session form login, so HTTP Basic
#' authentication is not accepted. The helper fetches the login page, reads
#' the antiforgery token from its HTML with a regular expression (no
#' third-party HTML parser), and posts the credentials together with the page
#' cookies. A successful login lands on `Home/Index` and sets the
#' `Authorization` cookie that every later HMD request must carry.
#'
#' @inheritParams ReadHMD
#' @return A character string with the session cookies as `name=value`
#'   pairs joined by `"; "`, ready to be passed to `fetch_text` as
#'   `session`. The function raises an error when the login page has no
#'   antiforgery token or when the login is rejected.
#' @noRd
hmd_session <- function(username, password) {
  login_url <- "https://www.mortality.org/Account/Login"

  # Step 1: open the login page and pick up the antiforgery cookie.
  login_page <- httr::GET(
    url    = login_url,
    config = httr::timeout(seconds = 300)
  )
  login_html <- httr::content(x = login_page, as = "text", encoding = "UTF-8")
  token <- extract_login_token(html = login_html)

  if (is.null(token)) {
    stop(
      "Could not find the HMD login token on the login page. The website ",
      "layout may have changed, or the website may be down. Try again later.",
      call. = FALSE
    )
  }

  page_cookies <- httr::cookies(x = login_page)
  step1_cookies <- stats::setNames(
    object = page_cookies$value,
    nm     = page_cookies$name
  )

  # Step 2: post the credentials, carrying the step-1 cookies.
  request_config <- c(
    httr::timeout(seconds = 300),
    httr::set_cookies(.cookies = step1_cookies)
  )
  response <- httr::POST(
    url    = login_url,
    config = request_config,
    body   = list(
      Email                        = username,
      Password                     = password,
      `__RequestVerificationToken` = token
    ),
    encode = "form"
  )

  # Step 3: a successful login redirects to Home/Index.
  if (!grepl("Home/Index", response$url, fixed = TRUE)) {
    stop(
      "The Human Mortality Database rejected the login for ", username,
      ". Check the username and password, and make sure the user ",
      "agreement at https://www.mortality.org/Account/UserAgreement has ",
      "been accepted.",
      call. = FALSE
    )
  }

  session <- httr::cookies(x = response)
  cookie_string <- paste0(session$name, "=", session$value, collapse = "; ")

  if (!nzchar(cookie_string)) {
    stop(
      "The HMD login reached Home/Index but no session cookie was set. ",
      "Try again later.",
      call. = FALSE
    )
  }

  return(cookie_string)
}


#' Read the antiforgery token from the HMD login page
#'
#' The login form carries a hidden input named
#' `__RequestVerificationToken`. A regular expression is enough to read its
#' `value` attribute, which keeps the login dependent on `httr` alone.
#'
#' @param html Login page HTML as a single character string.
#' @return The token as a character string, or `NULL` when the page has no
#'   such input or the input has no non-empty `value` attribute.
#' @noRd
extract_login_token <- function(html) {
  input_tag <- regmatches(
    x = html,
    m = regexpr(
      pattern     = "<input[^>]*__RequestVerificationToken[^>]*>",
      text        = html,
      ignore.case = TRUE
    )
  )
  token <- sub(
    pattern     = ".*value=\"([^\"]+)\".*",
    replacement = "\\1",
    x           = input_tag,
    ignore.case = TRUE
  )

  if (length(input_tag) == 0 || identical(token, input_tag)) {
    token <- NULL
  }

  return(token)
}


#' Fetch a text file and report failures instead of raising
#'
#' One HTTP GET shared by every database reader in the package. The
#' function never raises: failures are returned as data instead of being
#' signalled. A connection failure comes back as `status = NA_integer_`
#' with the condition message in `error`, and a non-200 response reports
#' its real status code. The `html` flag marks an HTML payload where a
#' data file was expected, which is how the HMD login page arrives when
#' the session is missing or expired.
#'
#' @param url Full URL of the file to fetch.
#' @param session Cookie string returned by `hmd_session` for databases
#'   that need a login, or `NULL` for open databases.
#' @return A list with four elements: `status`, `text`, `error` and `html`.
#'   `status` is the HTTP status, or `NA_integer_` when the connection
#'   failed; `text` is the body decoded as UTF-8, or `NULL` when there is
#'   no usable body; `error` is `NULL` on success, otherwise a one-line
#'   description of the real failure; `html` is `TRUE` when the body looks
#'   like an HTML document.
#' @noRd
fetch_text <- function(url, session = NULL) {
  request_config <- httr::timeout(seconds = 300)

  if (!is.null(session)) {
    cookie_config <- httr::set_cookies(.cookies = session_cookies(session))
    request_config <- c(request_config, cookie_config)
  }

  # A connection error must come back as a result, never as a condition.
  response <- tryCatch(
    httr::GET(url = url, config = request_config),
    error = function(condition) condition
  )

  if (inherits(response, "condition")) {
    out <- list(
      status = NA_integer_,
      text   = NULL,
      error  = paste0(
        "Could not connect to ", url, ": ",
        conditionMessage(response)
      ),
      html   = FALSE
    )
  } else {
    status <- httr::status_code(response)
    text <- httr::content(x = response, as = "text", encoding = "UTF-8")

    if (length(text) != 1 || is.na(text) || !nzchar(text)) {
      text <- NULL
    }

    html <- !is.null(text) && grepl("<!DOCTYPE|<html", text, ignore.case = TRUE)
    error <- NULL

    if (status != 200) {
      error <- paste0("The server returned HTTP ", status, " for ", url)
    }

    out <- list(status = status, text = text, error = error, html = html)
  }

  return(out)
}


#' Turn a session cookie string into named cookie values
#'
#' `hmd_session` returns the cookies as one `name=value; name2=value2`
#' string. `httr::set_cookies` needs the values as a named character
#' vector, because a bare unnamed string becomes a single empty-named
#' cookie.
#'
#' @param session Cookie string with `name=value` pairs separated by
#'   `"; "`.
#' @return Named character vector of cookie values, named by cookie name.
#' @noRd
session_cookies <- function(session) {
  pairs <- strsplit(x = session, split = "; ", fixed = TRUE)[[1]]
  values <- sub(pattern = "^[^=]+=", replacement = "", x = pairs)
  names(values) <- sub(pattern = "=.*$", replacement = "", x = pairs)

  return(values)
}
