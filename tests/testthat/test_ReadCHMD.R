# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-03
# --------------------------------------------
# Rewritten against the real ReadCHMD contract (review F34/F36/F48):
# input validation and the availability rules run before any HTTP call, and
# every download failure is reported with message() and a NULL return.
# The default suite is offline: fetch_text() is replaced with
# testthat::local_mocked_bindings() and the captured files under fixtures/
# stand in for the live downloads.
# --------------------------------------------


# Return a captured fixture as one string, the way fetch_text() returns it.
read_fixture <- function(name) {
  path <- test_path("fixtures", name)
  out  <- paste(readLines(path, warn = FALSE), collapse = "\n")
  return(out)
}


test_that("ReadCHMD rejects an unknown indicator before any download", {
  expect_error(ReadCHMD(what     = "DxDD",
                        regions  = "CAN",
                        interval = "1x1"),
               regexp = "DxDD does not exist in CHMD")
})

test_that("ReadCHMD rejects cohort life tables the CHMD does not serve", {
  expect_error(ReadCHMD(what     = "LT_fc",
                        regions  = "SAS",
                        interval = "1x1"),
               regexp = "LT_fc does not exist in CHMD")
})

test_that("ReadCHMD rejects a region code that is not Canadian", {
  expect_error(ReadCHMD(what     = "Dx",
                        regions  = "CANN",
                        interval = "1x1"),
               regexp = "CANN")
})

test_that("ReadCHMD rejects the whole list when one region code is unknown", {
  expect_error(ReadCHMD(what     = "Dx",
                        regions  = c("CAN", "ZZZ"),
                        interval = "1x1"),
               regexp = "ZZZ")
})

test_that("ReadCHMD rejects an interval CHMD does not serve", {
  expect_error(ReadCHMD(what     = "Dx",
                        regions  = "CAN",
                        interval = "1x50"),
               regexp = "The interval 1x50 does not exist in CHMD")
})

test_that("ReadCHMD requires a single 'what' and a single interval", {
  # The old validators compared vectors with if(), so length > 1 warned
  # "the condition has length > 1" instead of stopping with a clear message.
  err_what <- expect_no_warning(
    tryCatch(
      ReadCHMD(what     = c("Dx", "mx"),
               regions  = "CAN",
               interval = "1x1"),
      error = function(e) conditionMessage(e)
    )
  )
  expect_match(err_what, "exactly one data type")
  expect_no_match(err_what, "condition has length")

  err_interval <- expect_no_warning(
    tryCatch(
      ReadCHMD(what     = "Dx",
               regions  = "CAN",
               interval = c("1x1", "5x1")),
      error = function(e) conditionMessage(e)
    )
  )
  expect_match(err_interval, "exactly one interval")
  expect_no_match(err_interval, "condition has length")
})

test_that("ReadCHMD restricts Dx to the 1x1 and 5x1 formats", {
  expect_error(ReadCHMD(what     = "Dx",
                        regions  = "CAN",
                        interval = "1x5"),
               regexp = "Dx is available only in the following format")
})

test_that("ReadCHMD rejects life tables for Yukon in the 1x1 and 5x1 formats", {
  expect_error(ReadCHMD(what     = "LT_f",
                        regions  = "YUK",
                        interval = "5x1",
                        show     = FALSE),
               regexp = "LT_f is NOT available")
})

test_that("ReadCHMD restricts births to the 1x1 format", {
  expect_error(ReadCHMD(what     = "births",
                        regions  = "CAN",
                        interval = "5x5"),
               regexp = "births is not available in CHMD in the 5x5 format")
  expect_error(ReadCHMD(what     = "births",
                        regions  = "CAN",
                        interval = "1x5"),
               regexp = "published only in the 1-year product")
})

test_that("ReadCHMD reads the 5-year population file in the 5-year formats", {
  # read_hmd_file picks Population.txt for the single-age formats and
  # Population5.txt for the 5-year age formats; the 1x1 file must not be
  # served for a 5x* request under the label of the 1x1 product.
  seen_urls <- character()
  local_mocked_bindings(
    fetch_text = function(url, session = NULL) {
      seen_urls <<- c(seen_urls, url)
      list(status = 200L,
           text   = read_fixture("hmd_deaths_1x1.txt"),
           error  = NULL,
           html   = FALSE)
    },
    .package = "MortalityLaws"
  )

  ReadCHMD(what     = "population",
           regions  = "CAN",
           interval = "5x5",
           show     = FALSE)

  expect_equal(
    seen_urls,
    "https://www.prdh.umontreal.ca/BDLC/data/CAN/Population5.txt"
  )
})

test_that("ReadCHMD parses a captured 1x1 file into integer ages 0 to 110", {
  seen_urls <- character()
  local_mocked_bindings(
    fetch_text = function(url, session = NULL) {
      seen_urls <<- c(seen_urls, url)
      list(status = 200L,
           text   = read_fixture("hmd_deaths_1x1.txt"),
           error  = NULL,
           html   = FALSE)
    },
    .package = "MortalityLaws"
  )

  out <- ReadCHMD(what     = "Dx",
                  regions  = "CAN",
                  interval = "1x1",
                  show     = FALSE)

  expect_equal(
    seen_urls,
    "https://www.prdh.umontreal.ca/BDLC/data/CAN/Deaths_1x1.txt"
  )
  expect_s3_class(out, "ReadCHMD")
  # 1 year x 111 single ages in the captured file.
  expect_equal(nrow(out$data), 111)
  expect_equal(unique(out$data$Age), 0:110)
  expect_equal(unique(out$data$country), "CAN")
  expect_equal(out$data$Female[1], 123, tolerance = 1e-12)
})

test_that("ReadCHMD reports a missing file and returns NULL", {
  local_mocked_bindings(
    fetch_text = function(url, session = NULL) {
      list(status = 404L,
           text   = NULL,
           error  = paste0("The server returned HTTP 404 for ", url),
           html   = FALSE)
    },
    .package = "MortalityLaws"
  )

  out <- NULL
  expect_message(
    out <- ReadCHMD(what     = "Dx",
                    regions  = "CAN",
                    interval = "1x1",
                    show     = FALSE),
    regexp = "HTTP 404"
  )
  expect_null(out)
})

test_that("the bundled CHMD sample still prints", {
  expect_output(print(CHMD_sample))
})
