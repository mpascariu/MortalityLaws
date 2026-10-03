# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-03
# --------------------------------------------
# Rewritten against the real ReadAHMD contract (review F34/F36/F48):
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


test_that("ReadAHMD rejects an unknown indicator before any download", {
  expect_error(ReadAHMD(what     = "DxDD",
                        regions  = "ACT",
                        interval = "1x1"),
               regexp = "DxDD does not exist in AHMD")
})

test_that("ReadAHMD rejects a region code that is not Australian", {
  expect_error(ReadAHMD(what     = "Dx",
                        regions  = "ACTT",
                        interval = "1x1"),
               regexp = "ACTT")
})

test_that("ReadAHMD rejects the whole list when one region code is unknown", {
  expect_error(ReadAHMD(what     = "Dx",
                        regions  = c("ACT", "ZZZ"),
                        interval = "1x1"),
               regexp = "ZZZ")
})

test_that("ReadAHMD rejects a region with an indicator glued onto its name", {
  expect_error(ReadAHMD(what     = "LT_fc",
                        regions  = "TAS_LT_fc",
                        interval = "1x1",
                        show     = FALSE),
               regexp = "TAS_LT_fc")
})

test_that("ReadAHMD rejects an interval AHMD does not serve", {
  expect_error(ReadAHMD(what     = "Dx",
                        regions  = "ACT",
                        interval = "1x50"),
               regexp = "The interval 1x50 does not exist in AHMD")
})

test_that("ReadAHMD requires a single 'what' and a single interval", {
  # The old validators compared vectors with if(), so length > 1 warned
  # "the condition has length > 1" instead of stopping with a clear message.
  err_what <- expect_no_warning(
    tryCatch(
      ReadAHMD(what     = c("Dx", "mx"),
               regions  = "ACT",
               interval = "1x1"),
      error = function(e) conditionMessage(e)
    )
  )
  expect_match(err_what, "exactly one data type")
  expect_no_match(err_what, "condition has length")

  err_interval <- expect_no_warning(
    tryCatch(
      ReadAHMD(what     = "Dx",
               regions  = "ACT",
               interval = c("1x1", "5x1")),
      error = function(e) conditionMessage(e)
    )
  )
  expect_match(err_interval, "exactly one interval")
  expect_no_match(err_interval, "condition has length")
})

test_that("ReadAHMD restricts births to the 1x1 format", {
  expect_error(ReadAHMD(what     = "births",
                        regions  = "ACT",
                        interval = "5x5"),
               regexp = "births is not available in AHMD in the 5x5 format")
  expect_error(ReadAHMD(what     = "births",
                        regions  = "ACT",
                        interval = "1x5"),
               regexp = "published only in the 1-year product")
})

test_that("ReadAHMD reads the 5-year population file in the 5-year formats", {
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

  ReadAHMD(what     = "population",
           regions  = "ACT",
           interval = "5x5",
           show     = FALSE)

  expect_equal(
    seen_urls,
    "https://aushd.org/assets/txtFiles/humanMortality/ACT/Population5.txt"
  )
})

test_that("ReadAHMD rejects e0 in the five-year time formats", {
  expect_error(ReadAHMD(what     = "e0",
                        regions  = "TAS",
                        interval = "5x1",
                        show     = FALSE),
               regexp = "Data type 'e0' is not available in AHMD")
})

test_that("ReadAHMD parses a captured 1x1 file into integer ages 0 to 110", {
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

  out <- ReadAHMD(what     = "Dx",
                  regions  = "ACT",
                  interval = "1x1",
                  show     = FALSE)

  expect_equal(
    seen_urls,
    "https://aushd.org/assets/txtFiles/humanMortality/ACT/Deaths_1x1.txt"
  )
  expect_s3_class(out, "ReadAHMD")
  # 1 year x 111 single ages in the captured file.
  expect_equal(nrow(out$data), 111)
  expect_equal(unique(out$data$Age), 0:110)
  expect_equal(unique(out$data$country), "ACT")
  expect_equal(out$data$Female[1], 123, tolerance = 1e-12)
})

test_that("ReadAHMD reports a login page instead of a data file", {
  local_mocked_bindings(
    fetch_text = function(url, session = NULL) {
      list(status = 200L,
           text   = "<!DOCTYPE html><html><body>Not found</body></html>",
           error  = NULL,
           html   = TRUE)
    },
    .package = "MortalityLaws"
  )

  out <- NULL
  expect_message(
    out <- ReadAHMD(what     = "Dx",
                    regions  = "ACT",
                    interval = "1x1",
                    show     = FALSE),
    regexp = "is an HTML page, not a data file"
  )
  expect_null(out)
})

test_that("the bundled AHMD sample still prints", {
  expect_output(print(AHMD_sample))
})
