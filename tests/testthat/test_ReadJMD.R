# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-03
# --------------------------------------------
# Rewritten against the real ReadJMD contract (review F3/F34/F36/F48).
# The F3 regression is pinned offline twice: JMDregion_code() must return the
# JIS prefecture codes (the old position-based mapping sent "Fukushima" to
# folder 06, which serves Yamagata), and the mocked download must hit the
# "/07/STATS/" folder. The only live test is opt-in and needs HMD_USER.
# Every default-suite expectation runs against mocks or captured fixtures.
# --------------------------------------------


# Return a captured fixture as one string, the way fetch_text() returns it.
read_fixture <- function(name) {
  path <- test_path("fixtures", name)
  out  <- paste(readLines(path, warn = FALSE), collapse = "\n")
  return(out)
}


test_that("ReadJMD rejects an unknown indicator before any download", {
  expect_error(ReadJMD(what     = "DxDD",
                       regions  = "Kyoto",
                       interval = "1x1"),
               regexp = "DxDD does not exist in JMD")
})

test_that("ReadJMD rejects a data type the JMD server does not serve", {
  # Ex_lexis is an HMD type; the JMD server offers no Lexis exposures.
  expect_error(ReadJMD(what     = "Ex_lexis",
                       regions  = "Kyoto",
                       interval = "1x1"),
               regexp = "Ex_lexis does not exist in JMD")
})

test_that("ReadJMD rejects a region name that is not a prefecture", {
  expect_error(ReadJMD(what     = "Dx",
                       regions  = "Kyotooooooo",
                       interval = "1x1"),
               regexp = "Kyotooooooo")
})

test_that("ReadJMD rejects the whole list when one region name is unknown", {
  expect_error(ReadJMD(what     = "Dx",
                       regions  = c("Kyoto", "ZZZ"),
                       interval = "1x1"),
               regexp = "ZZZ")
})

test_that("ReadJMD rejects an interval JMD does not serve", {
  expect_error(ReadJMD(what     = "Dx",
                       regions  = "Kyoto",
                       interval = "1x50"),
               regexp = "The interval 1x50 does not exist in JMD")
})

test_that("ReadJMD requires a single 'what' and a single interval", {
  # The old validators compared vectors with if(), so length > 1 warned
  # "the condition has length > 1" instead of stopping with a clear message.
  err_what <- expect_no_warning(
    tryCatch(
      ReadJMD(what     = c("Dx", "mx"),
              regions  = "Kyoto",
              interval = "1x1"),
      error = function(e) conditionMessage(e)
    )
  )
  expect_match(err_what, "exactly one data type")
  expect_no_match(err_what, "condition has length")

  err_interval <- expect_no_warning(
    tryCatch(
      ReadJMD(what     = "Dx",
              regions  = "Kyoto",
              interval = c("1x1", "5x5")),
      error = function(e) conditionMessage(e)
    )
  )
  expect_match(err_interval, "exactly one interval")
  expect_no_match(err_interval, "condition has length")
})

test_that("ReadJMD restricts births and e0 to the intervals JMD serves", {
  expect_error(ReadJMD(what     = "births",
                       regions  = "Kyoto",
                       interval = "5x5"),
               regexp = "births is available in JMD only in the '1x1' format")

  expect_error(ReadJMD(what     = "e0",
                       regions  = "Kyoto",
                       interval = "5x1"),
               regexp = "Data type 'e0' is available in JMD only")
})

test_that("JMDregion_code returns the JIS prefecture codes (F3)", {
  # The legacy position-based mapping returned "06" for Fukushima (which is
  # Yamagata on the server). These are the JIS codes of the same names.
  expect_equal(MortalityLaws:::JMDregion_code(region = "Fukushima"), "07")
  expect_equal(MortalityLaws:::JMDregion_code(region = "Tokyo"), "13")
  expect_equal(MortalityLaws:::JMDregion_code(region = "Kyoto"), "26")
  expect_equal(MortalityLaws:::JMDregion_code(region = "Hokkaido"), "01")
  expect_equal(MortalityLaws:::JMDregion_code(region = "Okinawa"), "47")
  expect_equal(MortalityLaws:::JMDregion_code(region = "Japan"), "00")
  expect_error(MortalityLaws:::JMDregion_code(region = "Atlantis"),
               regexp = "Unknown JMD region: Atlantis")
})

test_that("the JIS map covers every JPNregions() name exactly once (F3)", {
  codes   <- MortalityLaws:::JPNregion_codes()
  regions <- MortalityLaws:::JPNregions()

  expect_length(regions, 48)
  expect_setequal(names(codes), regions)
  expect_length(unique(unname(codes)), 48)
  expect_match(codes, "^[0-9]{2}$")
})

test_that("ReadJMD labels rows with the region name and uses the JIS folder", {
  seen_urls <- character()
  local_mocked_bindings(
    fetch_text = function(url, session = NULL) {
      seen_urls <<- c(seen_urls, url)
      fixture <- if (grepl("5x5", url, fixed = TRUE)) {
        "hmd_mx_5x5.txt"
      } else {
        "hmd_mx_1x1.txt"
      }
      list(status = 200L,
           text   = read_fixture(fixture),
           error  = NULL,
           html   = FALSE)
    },
    .package = "MortalityLaws"
  )

  out <- ReadJMD(what = "mx", regions = "Fukushima", interval = "1x1",
                 show = FALSE)

  expect_equal(
    seen_urls,
    "https://www.ipss.go.jp/p-toukei/JMD/07/STATS/Mx_1x1.txt"
  )
  expect_s3_class(out, "ReadJMD")
  # 3 years x 111 single ages in the captured file.
  expect_equal(nrow(out$data), 333)
  expect_equal(unique(out$data$Age), 0:110)
  expect_equal(unique(out$data$region), "Fukushima")
  expect_equal(out$data$Female[1], 0.151306, tolerance = 1e-12)

  out55 <- ReadJMD(what = "mx", regions = "Japan", interval = "5x5",
                   show = FALSE)

  expect_equal(seen_urls[2],
               "https://www.ipss.go.jp/p-toukei/JMD/00/STATS/Mx_5x5.txt")
  expect_equal(nrow(out55$data), 48)
  expect_equal(unique(out55$data$region), "Japan")
  expect_equal(tail(unique(out55$data$Age), 1), "110+")
})

test_that("ReadJMD messages a connection failure and returns NULL", {
  local_mocked_bindings(
    fetch_text = function(url, session = NULL) {
      list(status = NA_integer_,
           text   = NULL,
           error  = paste0("Could not connect to ", url, ": simulated failure"),
           html   = FALSE)
    },
    .package = "MortalityLaws"
  )

  out <- NULL
  expect_message(
    out <- ReadJMD(what = "mx", regions = "Fukushima", interval = "1x1",
                   show = FALSE),
    regexp = "Could not connect to https://www.ipss.go.jp/p-toukei/JMD/07/"
  )
  expect_null(out)
})

test_that("ReadJMD downloads Fukushima death rates from the JIS folder (live)", {
  # Live network test. It must never run during a CRAN check: the IPSS server
  # going down (or CRAN losing network) would fail the check through no fault
  # of the package. Opt in explicitly with an environment variable, so the
  # test runs locally and never on CRAN.
  skip_on_cran()
  skip_if(Sys.getenv("MORTALITYLAWS_LIVE_TESTS") != "true",
          message = "live internet tests are opt-in, set MORTALITYLAWS_LIVE_TESTS=true")
  skip_if(Sys.getenv("HMD_USER") == "", message = "HMD credentials are not set")

  out <- tryCatch(
    ReadJMD(what = "Dx", regions = "Fukushima", interval = "1x1", show = FALSE),
    error = function(e) NULL
  )
  # An upstream outage is a skip, not a failure.
  skip_if(is.null(out), message = "the JMD server did not answer")
  skip_if(!is.list(out) || is.null(out$data), message = "no JMD data returned")

  expect_s3_class(out, "ReadJMD")
  expect_equal(unique(out$data$region), "Fukushima")
  # Probed 2026-10-03: folder 07 serves Fukushima; folder 06 (the legacy
  # position mapping) serves Yamagata and returns 1818.25 here.
  expect_equal(out$data$Female[1], 2409.52, tolerance = 1e-6)
})

test_that("the bundled JMD sample still prints", {
  expect_output(print(JMD_sample))
})
