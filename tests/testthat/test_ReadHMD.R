# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-03
# --------------------------------------------
# Rewritten against the real ReadHMD contract (review F2/F34/F36/F39/F48):
#   - input validation runs before any HTTP call and rejects the whole input
#     when one element is invalid;
#   - every download failure is reported with message() and a NULL return,
#     never with an error.
# The default suite is offline: hmd_session() and fetch_text() are replaced
# with testthat::local_mocked_bindings() and the captured files under
# fixtures/ stand in for the live downloads. The username/password below are
# inert placeholders: authentication is mocked on every path that gets past
# validation, so no credential is ever sent anywhere.
# --------------------------------------------


# Return a captured fixture as one string, the way fetch_text() returns it.
read_fixture <- function(name) {
  path <- test_path("fixtures", name)
  out  <- paste(readLines(path, warn = FALSE), collapse = "\n")
  return(out)
}


test_that("ReadHMD rejects an unknown indicator before any download", {
  expect_error(ReadHMD(what      = "DxDD",
                       countries = "AUS",
                       interval  = "1x1",
                       username  = "test-user",
                       password  = "test-password"),
               regexp = "DxDD does not exist in HMD")
})

test_that("ReadHMD rejects a country code that is not in HMD, naming it", {
  expect_error(ReadHMD(what      = "Dx",
                       countries = "AUSS",
                       interval  = "1x1",
                       username  = "test-user",
                       password  = "test-password"),
               regexp = "Unknown country code.*AUSS")
})

test_that("ReadHMD rejects the whole list and names the unknown country", {
  # F36 regression: the old all(!(...)) check let c("SWE", "XXX") through and
  # the loop then tried to download the invalid country.
  expect_error(ReadHMD(what      = "Dx",
                       countries = c("SWE", "XXX"),
                       interval  = "1x1",
                       username  = "test-user",
                       password  = "test-password"),
               regexp = "Unknown country code.*XXX")
})

test_that("ReadHMD rejects an interval HMD does not serve", {
  expect_error(ReadHMD(what      = "Dx",
                       countries = "SWE",
                       interval  = "1x50",
                       username  = "test-user",
                       password  = "test-password"),
               regexp = "The interval 1x50 does not exist in HMD")
})

test_that("ReadHMD requires a single 'what' and a single interval", {
  # The old validators compared vectors with if(), so length > 1 warned
  # "the condition has length > 1" instead of stopping with a clear message.
  err_what <- expect_no_warning(
    tryCatch(
      ReadHMD(what      = c("Dx", "mx"),
              countries = "SWE",
              interval  = "1x1",
              username  = "test-user",
              password  = "test-password"),
      error = function(e) conditionMessage(e)
    )
  )
  expect_match(err_what, "exactly one data type")
  expect_no_match(err_what, "condition has length")

  err_interval <- expect_no_warning(
    tryCatch(
      ReadHMD(what      = "Dx",
              countries = "SWE",
              interval  = c("1x1", "5x5"),
              username  = "test-user",
              password  = "test-password"),
      error = function(e) conditionMessage(e)
    )
  )
  expect_match(err_interval, "exactly one interval")
  expect_no_match(err_interval, "condition has length")
})

test_that("ReadHMD rejects cohort indicators for countries without cohort data", {
  expect_error(ReadHMD(what      = "LT_fc",
                       countries = "AUS",
                       interval  = "1x1",
                       username  = "test-user",
                       password  = "test-password"),
               regexp = "LT_fc is not available for one or more countries")
})

test_that("ReadHMD restricts e0 and e0c to the single-age time formats", {
  expect_error(ReadHMD(what      = "e0",
                       countries = "SWE",
                       interval  = "5x1",
                       username  = "test-user",
                       password  = "test-password"),
               regexp = "Data type e0 is available only in the following formats")

  expect_error(ReadHMD(what      = "e0c",
                       countries = "SWE",
                       interval  = "5x1",
                       username  = "test-user",
                       password  = "test-password"),
               regexp = "Data type e0c is available only in the following formats")
})

test_that("ReadHMD messages a rejected login and returns NULL", {
  local_mocked_bindings(
    hmd_session = function(username, password) {
      stop("The Human Mortality Database rejected the login for test-user.",
           call. = FALSE)
    },
    fetch_text  = function(url, session = NULL) {
      stop("fetch_text() must not run after a rejected login", call. = FALSE)
    },
    .package = "MortalityLaws"
  )

  out <- NULL
  expect_message(
    out <- ReadHMD(what      = "Dx",
                   countries = "SWE",
                   interval  = "1x1",
                   username  = "test-user",
                   password  = "test-password",
                   show      = FALSE),
    regexp = "rejected the login"
  )
  expect_null(out)
})

test_that("ReadHMD parses a captured 1x1 file into integer ages 0 to 110", {
  seen_urls <- character()
  session_calls <- 0
  local_mocked_bindings(
    hmd_session = function(username, password) {
      session_calls <<- session_calls + 1
      "Authorization=test"
    },
    fetch_text  = function(url, session = NULL) {
      seen_urls <<- c(seen_urls, url)
      list(status = 200L,
           text   = read_fixture("hmd_mx_1x1.txt"),
           error  = NULL,
           html   = FALSE)
    },
    .package = "MortalityLaws"
  )

  out <- ReadHMD(what      = "mx",
                 countries = c("GBRTENW", "SWE"),
                 interval  = "1x1",
                 username  = "test-user",
                 password  = "test-password",
                 show      = FALSE)

  # One login serves the whole download; the file names follow hmd_file_name()
  # and the reader's path layout.
  expect_equal(session_calls, 1)
  expect_equal(seen_urls, c(
    "https://www.mortality.org/File/GetDocument/hmd.v6/GBRTENW/STATS/Mx_1x1.txt",
    "https://www.mortality.org/File/GetDocument/hmd.v6/SWE/STATS/Mx_1x1.txt"
  ))
  expect_s3_class(out, "ReadHMD")
  # 2 countries x 3 years x 111 single ages in the captured file
  # (F35: the ages are derived from it).
  expect_equal(nrow(out$data), 666)
  expect_equal(unique(out$data$Age), 0:110)
  expect_equal(unique(out$data$Year), 1900:1902)
  expect_equal(unique(out$data$country), c("GBRTENW", "SWE"))
  expect_equal(out$data$Female[1], 0.151306, tolerance = 1e-12)
})

test_that("ReadHMD keeps the grouped age labels in a 5x5 file", {
  local_mocked_bindings(
    hmd_session = function(username, password) "Authorization=test",
    fetch_text  = function(url, session = NULL) {
      list(status = 200L,
           text   = read_fixture("hmd_mx_5x5.txt"),
           error  = NULL,
           html   = FALSE)
    },
    .package = "MortalityLaws"
  )

  out <- ReadHMD(what      = "mx",
                 countries = "SWE",
                 interval  = "5x5",
                 username  = "test-user",
                 password  = "test-password",
                 show      = FALSE)

  ages <- unique(out$data$Age)
  expect_equal(nrow(out$data), 48)          # 2 periods x 24 age groups
  expect_equal(length(ages), 24)
  expect_equal(ages[1:3], c("0", "1-4", "5-9"))
  expect_equal(ages[24], "110+")
  expect_equal(unique(out$data$Year), c("2000-2004", "2005-2009"))
})

test_that("ReadHMD messages a connection failure and returns NULL", {
  local_mocked_bindings(
    hmd_session = function(username, password) "Authorization=test",
    fetch_text  = function(url, session = NULL) {
      list(status = NA_integer_,
           text   = NULL,
           error  = paste0("Could not connect to ", url, ": simulated failure"),
           html   = FALSE)
    },
    .package = "MortalityLaws"
  )

  out <- NULL
  expect_message(
    out <- ReadHMD(what      = "Dx",
                   countries = "SWE",
                   interval  = "1x1",
                   username  = "test-user",
                   password  = "test-password",
                   show      = FALSE),
    regexp = "Could not connect to"
  )
  expect_null(out)
})

test_that("ReadHMD reports the real HTTP status and returns NULL", {
  local_mocked_bindings(
    hmd_session = function(username, password) "Authorization=test",
    fetch_text  = function(url, session = NULL) {
      list(status = 403L,
           text   = NULL,
           error  = paste0("The server returned HTTP 403 for ", url),
           html   = FALSE)
    },
    .package = "MortalityLaws"
  )

  out <- NULL
  expect_message(
    out <- ReadHMD(what      = "Dx",
                   countries = "SWE",
                   interval  = "1x1",
                   username  = "test-user",
                   password  = "test-password",
                   show      = FALSE),
    regexp = "HTTP 403"
  )
  expect_null(out)
})

test_that("ReadHMD rejects an HTML login page instead of a data file", {
  local_mocked_bindings(
    hmd_session = function(username, password) "Authorization=test",
    fetch_text  = function(url, session = NULL) {
      list(status = 200L,
           text   = "<!DOCTYPE html><html><body>Please log in</body></html>",
           error  = NULL,
           html   = TRUE)
    },
    .package = "MortalityLaws"
  )

  out <- NULL
  expect_message(
    out <- ReadHMD(what      = "Dx",
                   countries = "SWE",
                   interval  = "1x1",
                   username  = "test-user",
                   password  = "test-password",
                   show      = FALSE),
    regexp = "is an HTML page, not a data file"
  )
  expect_null(out)
})

test_that("ReadHMD messages an unparseable body and returns NULL", {
  local_mocked_bindings(
    hmd_session = function(username, password) "Authorization=test",
    fetch_text  = function(url, session = NULL) {
      list(status = 200L,
           text   = "this is not\na mortality file\n",
           error  = NULL,
           html   = FALSE)
    },
    .package = "MortalityLaws"
  )

  out <- NULL
  expect_message(
    out <- ReadHMD(what      = "Dx",
                   countries = "SWE",
                   interval  = "1x1",
                   username  = "test-user",
                   password  = "test-password",
                   show      = FALSE),
    regexp = "could not be parsed"
  )
  expect_null(out)
})

test_that("ReadHMD messages an empty response and returns NULL", {
  local_mocked_bindings(
    hmd_session = function(username, password) "Authorization=test",
    fetch_text  = function(url, session = NULL) {
      list(status = 200L,
           text   = NULL,
           error  = NULL,
           html   = FALSE)
    },
    .package = "MortalityLaws"
  )

  out <- NULL
  expect_message(
    out <- ReadHMD(what      = "Dx",
                   countries = "SWE",
                   interval  = "1x1",
                   username  = "test-user",
                   password  = "test-password",
                   show      = FALSE),
    regexp = "empty response"
  )
  expect_null(out)
})

test_that("the bundled HMD sample still prints", {
  expect_output(print(HMD_sample))
})
