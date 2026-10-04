# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-04
# --------------------------------------------
# The machinery shared by the four database readers (R/readers_shared.R): the
# HMD file-name map, the object assembly and its optional file, the input
# checks, and the little HTML parser behind availableHMD.
#
# The default suite is offline. The branches that only run on the way to a
# live download are not tested here; the ones below are driven through
# validation, and where a network helper could still be reached it is replaced
# with a counting stub, so a stray request is caught as a test failure rather
# than a hung run.
# --------------------------------------------


# Run code in a private working directory, then restore the caller's and
# remove the directory. Base R only: saving writes to the working directory,
# and the suite must not grow a dependency just to test that.
in_temp_dir <- function(code) {
  tmp <- tempfile("readers_shared_")
  dir.create(tmp)
  old <- setwd(tmp)
  on.exit({
    setwd(old)
    unlink(tmp, recursive = TRUE)
  }, add = TRUE)

  force(code)
}


test_that("hmd_file_name names the e0 period and cohort files", {
  # The 1x1 life-expectancy products have their own file stubs; every other
  # interval falls through to the generic E0per_<interval> pattern.
  expect_identical(hmd_file_name(what = "e0",  interval = "1x1"), "E0per")
  expect_identical(hmd_file_name(what = "e0c", interval = "1x1"), "E0coh")
  expect_identical(hmd_file_name(what = "e0",  interval = "1x5"), "E0per_1x5")
  expect_identical(hmd_file_name(what = "e0c", interval = "1x5"), "E0coh_1x5")

  # A data type HMD does not publish has no file to name.
  expect_null(hmd_file_name(what = "DxDD", interval = "1x1"))
})


test_that("read_hmd_file reports an unpublished data type without downloading", {
  fetch_calls <- 0
  local_mocked_bindings(
    fetch_text = function(url, session = NULL) {
      fetch_calls <<- fetch_calls + 1
      list(status = 200L, text = "should never be reached", error = NULL,
           html = FALSE)
    },
    .package = "MortalityLaws"
  )

  msgs <- capture_messages(
    out <- read_hmd_file(what     = "DxDD",
                         country  = "SWE",
                         interval = "1x1",
                         link     = "https://www.mortality.org/File/GetDocument/hmd.v6/")
  )

  # The name check happens before the URL is even built, so nothing is fetched.
  expect_identical(fetch_calls, 0)
  expect_null(out)
  expect_match(msgs, "DxDD is not a data type available in HMD",
               all = FALSE, fixed = TRUE)
  # The message lists what HMD does serve, so the user can pick a valid type.
  expect_match(msgs, paste(hmd_indices(), collapse = ", "),
               all = FALSE, fixed = TRUE)
})


test_that("save_output writes the object under the HMD_<what> name and announces it", {
  in_temp_dir({
    out <- list(input = list(what = "Dx"), data = data.frame(Year = 1900L))

    msgs <- capture_messages(
      save_output(out = out, show = TRUE, prefix = "HMD")
    )

    # The file name is the documented prefix_what stub, with the .Rdata
    # extension, and plain load() restores the object under that name.
    expect_true(file.exists("HMD_Dx.Rdata"))
    env    <- new.env()
    loaded <- load("HMD_Dx.Rdata", envir = env)
    expect_identical(loaded, "HMD_Dx")
    expect_identical(env[["HMD_Dx"]], out)

    expect_match(msgs, "The dataset is saved in your working directory",
                 all = FALSE, fixed = TRUE)
    expect_match(msgs, "Download completed!", all = FALSE, fixed = TRUE)
  })
})


test_that("new_read_object writes a copy only when the input asks for save", {
  in_temp_dir({
    data <- data.frame(country = "SWE", Year = 1900L, Age = 0:2)

    out <- new_read_object(data   = data,
                           input  = list(what = "Dx", save = TRUE),
                           prefix = "HMD",
                           class  = "ReadHMD",
                           show   = FALSE)

    expect_s3_class(out, "ReadHMD")
    expect_equal(out$years, 1900L)
    expect_equal(out$ages, 0:2)
    expect_true(file.exists("HMD_Dx.Rdata"))

    env <- new.env()
    load("HMD_Dx.Rdata", envir = env)
    expect_identical(env[["HMD_Dx"]]$input$what, "Dx")

    # save = FALSE (the default) leaves the working directory alone.
    quiet <- new_read_object(data   = data,
                             input  = list(what = "mx", save = FALSE),
                             prefix = "HMD",
                             class  = "ReadHMD",
                             show   = FALSE)
    expect_s3_class(quiet, "ReadHMD")
    expect_false(file.exists("HMD_mx.Rdata"))
  })
})


test_that("age_message labels the life-expectancy and births products", {
  x <- structure(list(ages = c(0L, 1L, 110L)), class = "ReadHMD")

  # e0/e0c carry one value per period, not a range over ages.
  expect_identical(age_message("e0",  x), 0)
  expect_identical(age_message("e0c", x), 0)
  # Births are not broken down by age at all.
  expect_identical(age_message("births", x), "all ages")
  # Everything else reports the covered age range.
  expect_identical(age_message("Dx", x), "0 -- 110")
})


test_that("check_reader_regions demands at least one region", {
  expect_silent(check_reader_regions(regions = "SWE", known = hmd_countries()))

  expect_error(check_reader_regions(regions = character(0),
                                    known   = hmd_countries()),
               regexp = "Please specify at least one region in 'regions'")
})


test_that("parse_html_table returns NULL when the page carries no complete table", {
  # "<table" is present but never closed, so the table pattern matches nothing.
  expect_null(parse_html_table(
    html = "<html><table class=\"x\"><tr><th>Country</th></tr>"
  ))

  # A complete table with only a header row has no data to return.
  expect_null(parse_html_table(
    html = "<table><tr><th>Country</th><th>Code</th></tr></table>"
  ))
})


test_that("parse_html_row returns no cells for markup that carries none", {
  expect_identical(parse_html_row(row_html = "<tr></tr>"), character(0))
})


test_that("ReadHMD rejects births outside the 1-year product before any download", {
  session_calls <- 0
  fetch_calls   <- 0
  local_mocked_bindings(
    hmd_session = function(username, password) {
      session_calls <<- session_calls + 1
      "Authorization=test"
    },
    fetch_text  = function(url, session = NULL) {
      fetch_calls <<- fetch_calls + 1
      list(status = 200L, text = "should never be reached", error = NULL,
           html = FALSE)
    },
    .package = "MortalityLaws"
  )

  expect_error(
    ReadHMD(what      = "births",
            countries = "SWE",
            interval  = "5x5",
            username  = "test-user",
            password  = "test-password"),
    regexp = "births is available only in the 1-year product"
  )

  # The validation is step 1 of ReadHMD: no login is attempted, no file asked for.
  expect_identical(session_calls, 0)
  expect_identical(fetch_calls, 0)
})


# --- The default region list of the four readers ---------------------------
# Each entry point replaces a missing region argument with its hard-coded code
# vector before it validates the rest of the input. The default vector is not a
# download: the lines below run offline, and the validation that follows stops
# the call. The proof is in the error itself - if the default were not filled
# in, the empty region vector would fail the region check with "Please specify
# at least one region" instead of the data-type message asserted here.


test_that("ReadHMD fills in the HMD country list before validating", {
  # Covers R/readHMD.R:125, `countries <- hmd_countries()`.
  session_calls <- 0
  fetch_calls   <- 0
  local_mocked_bindings(
    hmd_session = function(username, password) {
      session_calls <<- session_calls + 1
      "Authorization=test"
    },
    fetch_text  = function(url, session = NULL) {
      fetch_calls <<- fetch_calls + 1
      list(status = 200L, text = "should never be reached", error = NULL,
           html = FALSE)
    },
    .package = "MortalityLaws"
  )

  expect_error(
    ReadHMD(what     = "births",
            interval = "5x5",
            username = "test-user",
            password = "test-password"),
    regexp = "births is available only in the 1-year product"
  )
  expect_identical(session_calls, 0)
  expect_identical(fetch_calls, 0)
})


test_that("ReadAHMD fills in the Australian region list before validating", {
  # Covers R/readAHMD.R:67, `regions <- aus_regions()`. AHMD needs no login,
  # so only the fetch stub is armed.
  fetch_calls <- 0
  local_mocked_bindings(
    fetch_text = function(url, session = NULL) {
      fetch_calls <<- fetch_calls + 1
      list(status = 200L, text = "should never be reached", error = NULL,
           html = FALSE)
    },
    .package = "MortalityLaws"
  )

  expect_error(
    ReadAHMD(what = "births", interval = "5x5"),
    regexp = "births is not available in AHMD in the 5x5 format"
  )
  expect_identical(fetch_calls, 0)
})


test_that("ReadCHMD fills in the Canadian region list before validating", {
  # Covers R/readCHMD.R:85, `regions <- can_regions()`.
  fetch_calls <- 0
  local_mocked_bindings(
    fetch_text = function(url, session = NULL) {
      fetch_calls <<- fetch_calls + 1
      list(status = 200L, text = "should never be reached", error = NULL,
           html = FALSE)
    },
    .package = "MortalityLaws"
  )

  expect_error(
    ReadCHMD(what = "births", interval = "5x5"),
    regexp = "births is not available in CHMD in the 5x5 format"
  )
  expect_identical(fetch_calls, 0)
})


test_that("ReadJMD fills in the Japanese region list before validating", {
  # Covers R/readJMD.R:80, `regions <- jpn_regions()`.
  fetch_calls <- 0
  local_mocked_bindings(
    fetch_text = function(url, session = NULL) {
      fetch_calls <<- fetch_calls + 1
      list(status = 200L, text = "should never be reached", error = NULL,
           html = FALSE)
    },
    .package = "MortalityLaws"
  )

  expect_error(
    ReadJMD(what = "births", interval = "5x5"),
    regexp = "births is available in JMD only in the '1x1' format"
  )
  expect_identical(fetch_calls, 0)
})
