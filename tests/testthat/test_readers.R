# --------------------------------------------
# Author: Marius D PASCARIU; Date: 2026-10-04
# --------------------------------------------
# ReadHMD/AHMD/CHMD/JMD + R/readers_shared.R, one spec table; titles map in the doc.

in_temp_dir <- function(code) {
  tmp <- tempfile("test_readers_")
  dir.create(tmp)
  old <- setwd(tmp)
  on.exit(setwd(old), add = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  force(code)
}

rejected <- function(spec, ...) {
  out <- NULL
  expect_no_warning(out <- tryCatch(reader_call(spec, ...), error = function(e) conditionMessage(e)))
  expect_no_match(out, "condition has length")
  return(out)
}

# Call one reader its own way; a NULL region drops the argument and fills in the default.
reader_call <- function(spec, what = "Dx", interval = "1x1", regions = spec$region) {
  args <- list(what = what, interval = interval, show = FALSE)
  args[[spec$arg]] <- regions

  if (spec$login) args[c("username", "password")] <- list("test-user", "test-password")

  return(do.call(spec$fn, args))
}

# Arm the stubs: fetch_text() records $urls/$calls, hmd_session() counts $logins.
arm_reader <- function(text = "should never be reached", status = 200L, error = NULL,
                       html = FALSE, log = new_log(), .env = parent.frame()) {
  log$logins <- 0L
  local_mocked_bindings(
    hmd_session = function(username, password) {
      log$logins <- log$logins + 1L
      "Authorization=test"
    },
    fetch_text = stub_fetch(text = text, status = status, error = error, html = html, log = log),
    .package   = "MortalityLaws",
    .env       = .env
  )
  return(log)
}

# One row per database: `arg`/`login` say how the entry point differs, `failures` the failures it can reach.
readers <- list(
  HMD = list(
    fn = ReadHMD, arg = "countries", login = TRUE, region = "SWE",
    label = "country code", col = "country", bad_regions = "AUSS",
    unknown = "DxDD", what = "mx", fixture = "hmd_mx_1x1.txt",
    births = c("5x5" = "births is available only in the 1-year product"),
    regions = c("GBRTENW", "SWE"), rows = 666, value = 0.151306,
    urls = c("https://www.mortality.org/File/GetDocument/hmd.v6/GBRTENW/STATS/Mx_1x1.txt",
             "https://www.mortality.org/File/GetDocument/hmd.v6/SWE/STATS/Mx_1x1.txt"),
    population = "https://www.mortality.org/File/GetDocument/hmd.v6/SWE/STATS/Population5.txt",
    failures = list(
      "a connection failure" = list(regexp = "Could not connect to", error = "Could not connect to the server"),
      "the real HTTP status" = list(regexp = "HTTP 403", status = 403L, error = "The server returned HTTP 403"),
      "an HTML login page" = list(regexp = "is an HTML page, not a data file", html = TRUE,
                                  text = "<!DOCTYPE html><html><body>Please log in</body></html>"),
      "an unparseable body" = list(regexp = "could not be parsed", text = "this is not\na mortality file\n"),
      "an empty response" = list(regexp = "empty response", text = NULL)
    )
  ),
  AHMD = list(
    fn = ReadAHMD, arg = "regions", login = FALSE, region = "ACT",
    label = "region code", col = "country", bad_regions = c("ACTT", "TAS_LT_fc"),
    unknown = "DxDD", what = "Dx", fixture = "hmd_deaths_1x1.txt",
    births = c("5x5" = "births is not available in AHMD in the 5x5 format",
               "1x5" = "published only in the 1-year product"),
    regions = "ACT", rows = 111, value = 123,
    urls = "https://aushd.org/assets/txtFiles/humanMortality/ACT/Deaths_1x1.txt",
    population = "https://aushd.org/assets/txtFiles/humanMortality/ACT/Population5.txt",
    failures = list("a login page" = list(regexp = "is an HTML page, not a data file", html = TRUE,
                                          text = "<html><body>Not found</body></html>"))
  ),
  CHMD = list(
    fn = ReadCHMD, arg = "regions", login = FALSE, region = "CAN",
    label = "region code", col = "country", bad_regions = "CANN",
    unknown = c("DxDD", "LT_fc"), what = "Dx", fixture = "hmd_deaths_1x1.txt",
    births = c("5x5" = "births is not available in CHMD in the 5x5 format",
               "1x5" = "published only in the 1-year product"),
    regions = "CAN", rows = 111, value = 123,
    urls = "https://www.prdh.umontreal.ca/BDLC/data/CAN/Deaths_1x1.txt",
    population = "https://www.prdh.umontreal.ca/BDLC/data/CAN/Population5.txt",
    failures = list("a missing file" = list(regexp = "HTTP 404", status = 404L, error = "HTTP 404 from the server"))
  ),
  JMD = list(
    fn = ReadJMD, arg = "regions", login = FALSE, region = "Fukushima",
    label = "region name", col = "region", bad_regions = "Kyotooooooo",
    unknown = c("DxDD", "Ex_lexis"), what = "mx", fixture = "hmd_mx_1x1.txt",
    births = c("5x5" = "births is available in JMD only in the '1x1' format"),
    regions = "Fukushima", rows = 333, value = 0.151306,
    urls = "https://www.ipss.go.jp/p-toukei/JMD/07/STATS/Mx_1x1.txt",
    population = "https://www.ipss.go.jp/p-toukei/JMD/07/STATS/Population5.txt",
    failures = list("a connection failure in the JIS folder" = list(
      regexp = "Could not connect to https://www.ipss.go.jp/p-toukei/JMD/07/",
      error  = "Could not connect to https://www.ipss.go.jp/p-toukei/JMD/07/STATS/Mx_1x1.txt"))
  )
)

for (name in names(readers)) {
  spec <- readers[[name]]
  test_that(paste(name, "rejects an unknown indicator before any download"), {
    for (bad in spec$unknown) expect_match(rejected(spec, what = bad), paste0(bad, " does not exist in ", name))
  })
  test_that(paste0(name, " rejects an unknown ", spec$label, ", naming it"), {
    for (bad in spec$bad_regions) expect_match(rejected(spec, regions = bad), paste0("Unknown ", spec$label, ".*", bad))
  })
  test_that(paste(name, "rejects the whole list when one", spec$label, "is unknown"), {
    # F36: the old all(!(...)) check let c("SWE", "ZZZ") through to the download loop.
    expect_match(rejected(spec, regions = c(spec$region, "ZZZ")), paste0("Unknown ", spec$label, ".*ZZZ"))
  })
  test_that(paste(name, "rejects an interval it does not serve"), {
    expect_match(rejected(spec, interval = "1x50"), paste0("The interval 1x50 does not exist in ", name))
  })
  test_that(paste(name, "requires a single 'what' and a single interval"), {
    expect_match(rejected(spec, what = c("Dx", "mx")), "exactly one data type")
    expect_match(rejected(spec, interval = c("1x1", "5x5")), "exactly one interval")
  })
  test_that(paste(name, "restricts births to the 1x1 format"), {
    for (iv in names(spec$births)) expect_match(rejected(spec, what = "births", interval = iv), spec$births[[iv]])
  })
  test_that(paste(name, "reads the 5-year population file in the 5-year formats"), {
    # Population.txt serves the single-age formats and Population5.txt the 5-year ones.
    arm <- arm_reader(text = read_fixture(spec$fixture))
    reader_call(spec, what = "population", interval = "5x5")
    expect_equal(arm$urls, spec$population)
  })
  test_that(paste(name, "parses a captured 1x1 file into integer ages 0 to 110"), {
    arm <- arm_reader(text = read_fixture(spec$fixture))
    out <- reader_call(spec, what = spec$what, regions = spec$regions)
    # One login serves the download; F35: the ages and row count come from the file.
    expect_equal(arm$logins, if (spec$login) 1L else 0L)
    expect_equal(arm$urls, spec$urls)
    expect_s3_class(out, paste0("Read", name))
    expect_equal(nrow(out$data), spec$rows)
    expect_equal(unique(out$data$Age), 0:110)
    expect_equal(unique(out$data[[spec$col]]), spec$regions)
    expect_equal(out$data$Female[1], spec$value, tolerance = 1e-12)
  })
  test_that(paste(name, "fills in the default region list before validating"), {
    # The region argument is left out: the fill-in must run before the type check.
    arm <- arm_reader()
    expect_match(rejected(spec, what = "births", interval = "5x5", regions = NULL), spec$births[["5x5"]])
    expect_identical(arm$logins, 0L)
    expect_identical(arm$calls, 0L)
  })
  for (scenario in names(spec$failures)) {
    f <- spec$failures[[scenario]]
    test_that(paste(name, "messages", scenario, "and returns NULL"), {
      arm_reader(text = f$text, status = f$status %||% 200L, error = f$error, html = f$html %||% FALSE)
      out <- NULL
      expect_message(out <- reader_call(spec, what = spec$what), regexp = f$regexp)
      expect_null(out)
    })
  }
  test_that(paste("the bundled", name, "sample still prints"), {
    expect_output(print(get(paste0(name, "_sample"))))
  })
}

test_that("HMD rejects cohort indicators and restricts e0 and e0c", {
  expect_match(rejected(readers$HMD, what = "LT_fc", regions = "AUS"),
               "LT_fc is not available for one or more countries")
  for (what in c("e0", "e0c")) expect_match(
    rejected(readers$HMD, what = what, interval = "5x1"),
    paste0("Data type ", what, " is available only in the following formats"))
})
test_that("HMD messages a rejected login and returns NULL", {
  arm <- arm_reader(text = read_fixture("hmd_mx_1x1.txt"))
  local_mocked_bindings(hmd_session = stub_session(reject = TRUE), .package = "MortalityLaws")
  out <- NULL
  expect_message(out <- reader_call(readers$HMD, what = "Dx"), regexp = "rejected the login")
  expect_null(out)
  expect_identical(arm$calls, 0L)  # a rejected login downloads nothing
})
test_that("HMD keeps the grouped age labels in a 5x5 file", {
  arm_reader(text = read_fixture("hmd_mx_5x5.txt"))
  out  <- reader_call(readers$HMD, what = "mx", interval = "5x5")
  ages <- unique(out$data$Age)
  expect_equal(nrow(out$data), 48)  # 2 periods x 24 age groups
  expect_equal(length(ages), 24)
  expect_equal(c(ages[1:3], ages[24]), c("0", "1-4", "5-9", "110+"))
  expect_equal(unique(out$data$Year), c("2000-2004", "2005-2009"))
})
test_that("AHMD and JMD restrict e0 to the single-age formats", {
  expect_match(rejected(readers$AHMD, what = "e0", interval = "5x1"),
               "Data type 'e0' is not available in AHMD")
  expect_match(rejected(readers$JMD, what = "e0", interval = "5x5"),
               "Data type 'e0' is available in JMD only")
})
test_that("CHMD restricts Dx and rejects life tables for Yukon", {
  expect_match(rejected(readers$CHMD, what = "Dx", interval = "1x5"),
               "Dx is available only in the following format")
  expect_match(rejected(readers$CHMD, what = "LT_f", regions = "YUK", interval = "5x1"),
               "LT_f is NOT available")
})

test_that("jmd_region_code returns the JIS codes and map (F3)", {
  # F3: the legacy position mapping sent Fukushima to folder 06 (Yamagata on the server).
  jis <- c(Fukushima = "07", Tokyo = "13", Kyoto = "26", Hokkaido = "01",
           Okinawa = "47", Japan = "00")
  for (region in names(jis)) expect_equal(jmd_region_code(region = region), jis[[region]])
  expect_error(jmd_region_code(region = "Atlantis"), regexp = "Unknown JMD region: Atlantis")
  codes   <- jpn_region_codes()
  regions <- jpn_regions()
  expect_length(regions, 48)
  expect_setequal(names(codes), regions)
  expect_length(unique(unname(codes)), 48)
  expect_match(codes, "^[0-9]{2}$")
})
test_that("JMD reads the 5x5 national file from JIS folder 00", {
  arm <- arm_reader(text = read_fixture("hmd_mx_5x5.txt"))
  out <- ReadJMD(what = "mx", regions = "Japan", interval = "5x5", show = FALSE)
  expect_equal(arm$urls, "https://www.ipss.go.jp/p-toukei/JMD/00/STATS/Mx_5x5.txt")
  expect_equal(unique(out$data$region), "Japan")
  expect_equal(nrow(out$data), 48)
  expect_equal(tail(unique(out$data$Age), 1), "110+")
})
test_that("ReadJMD downloads Fukushima death rates from the JIS folder (live)", {
  # Live network test: opt-in only, so an IPSS outage can never fail a CRAN check.
  skip_on_cran()
  skip_if(Sys.getenv("MORTALITYLAWS_LIVE_TESTS") != "true",
          message = "live internet tests are opt-in, set MORTALITYLAWS_LIVE_TESTS=true")
  skip_if(Sys.getenv("HMD_USER") == "", message = "HMD credentials are not set")
  out <- tryCatch(ReadJMD(what = "Dx", regions = "Fukushima", interval = "1x1", show = FALSE),
                  error = function(e) NULL)
  skip_if(is.null(out), message = "the JMD server did not answer")
  skip_if(!is.list(out) || is.null(out$data), message = "no JMD data returned")
  expect_s3_class(out, "ReadJMD")
  expect_equal(unique(out$data$region), "Fukushima")
  # Probed 2026-10-03: folder 07 serves Fukushima; folder 06 serves Yamagata (1818.25).
  expect_equal(out$data$Female[1], 2409.52, tolerance = 1e-6)
})

test_that("the HMD file map and its unpublished-type message", {
  expect_identical(hmd_file_name(what = "e0", interval = "1x1"), "E0per")
  expect_identical(hmd_file_name(what = "e0c", interval = "1x1"), "E0coh")
  expect_identical(hmd_file_name(what = "e0", interval = "1x5"), "E0per_1x5")
  expect_identical(hmd_file_name(what = "e0c", interval = "1x5"), "E0coh_1x5")
  expect_null(hmd_file_name(what = "DxDD", interval = "1x1"))
  arm <- arm_reader()
  msgs <- capture_messages(out <- read_hmd_file(what = "DxDD", country = "SWE", interval = "1x1",
    link = "https://www.mortality.org/File/GetDocument/hmd.v6/"))
  expect_identical(arm$calls, 0L)
  expect_null(out)
  expect_match(msgs, "DxDD is not a data type available in HMD", all = FALSE, fixed = TRUE)
  expect_match(msgs, paste(hmd_indices(), collapse = ", "), all = FALSE, fixed = TRUE)
})
test_that("save_output and new_read_object write what the input asks for", {
  in_temp_dir({
    out  <- list(input = list(what = "Dx"), data = data.frame(Year = 1900L))
    msgs <- capture_messages(save_output(out = out, show = TRUE, prefix = "HMD"))
    expect_true(file.exists("HMD_Dx.Rdata"))
    env    <- new.env()
    loaded <- load("HMD_Dx.Rdata", envir = env)
    expect_identical(loaded, "HMD_Dx")
    expect_identical(env[["HMD_Dx"]], out)
    expect_match(msgs, "The dataset is saved in your working directory", all = FALSE, fixed = TRUE)
    expect_match(msgs, "Download completed!", all = FALSE, fixed = TRUE)
    data <- data.frame(country = "SWE", Year = 1900L, Age = 0:2)
    obj  <- new_read_object(data = data, input = list(what = "Dx", save = TRUE),
                            prefix = "HMD", class = "ReadHMD", show = FALSE)
    expect_s3_class(obj, "ReadHMD")
    expect_equal(obj$years, 1900L)
    expect_equal(obj$ages, 0:2)
    expect_true(file.exists("HMD_Dx.Rdata"))
    env <- new.env()
    load("HMD_Dx.Rdata", envir = env)
    expect_identical(env[["HMD_Dx"]]$input$what, "Dx")
    # save = FALSE (the default) leaves the working directory alone.
    quiet <- new_read_object(data = data, input = list(what = "mx", save = FALSE),
                             prefix = "HMD", class = "ReadHMD", show = FALSE)
    expect_s3_class(quiet, "ReadHMD")
    expect_false(file.exists("HMD_mx.Rdata"))
  })
})
test_that("age_message labels the products and check_reader_regions needs one", {
  x <- structure(list(ages = c(0L, 1L, 110L)), class = "ReadHMD")
  expect_identical(age_message(what = "e0", x = x), 0)  # one value per period
  expect_identical(age_message(what = "e0c", x = x), 0)
  expect_identical(age_message(what = "births", x = x), "all ages")
  expect_identical(age_message(what = "Dx", x = x), "0 -- 110")
  expect_silent(check_reader_regions(regions = "SWE", known = hmd_countries()))
  expect_error(check_reader_regions(regions = character(0), known = hmd_countries()),
               regexp = "Please specify at least one region in 'regions'")
})
test_that("parse_html_table reads a table, resolves entities and returns NULL", {
  html <- paste0(
    "<table><thead><tr><th>Country and data series</th><th>Code</th><th>Period Life Tables</th></tr></thead><tbody>",
    "<tr><td><a href=\"/x\">Australia</a></td><td>AUS</td><td>1921 - 2021</td></tr>",
    "<tr><td>Iceland</td><td>ISL</td></tr>",
    "<tr><td>Trinidad &amp; Tobago</td><td>1921 &#8211;   2021</td><td></td></tr></tbody></table>")
  tab <- parse_html_table(html = html)
  expect_s3_class(tab, "data.frame")
  expect_identical(dim(tab), c(3L, 3L))
  expect_identical(colnames(tab), c("Country and data series", "Code", "Period Life Tables"))
  expect_identical(tab[[1]], c("Australia", "Iceland", "Trinidad & Tobago"))
  expect_identical(tab[[2]], c("AUS", "ISL", "1921 \u2013 2021"))
  expect_identical(tab[[3]], c("1921 - 2021", NA_character_, ""))
  expect_null(parse_html_table(html = "<html><table class=\"x\"><tr><th>Country</th></tr>"))
  expect_null(parse_html_table(html = "<table><tr><th>Country</th></tr></table>"))
  expect_null(parse_html_table(html = "<html><body>no table here</body></html>"))
  expect_null(parse_html_table(html = NULL))
  expect_identical(parse_html_row(row_html = "<tr></tr>"), character(0))
})
