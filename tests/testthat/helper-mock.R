# --------------------------------------------
# Mocking helpers shared by the test suite.
# --------------------------------------------
# The default suite never touches the network: the readers are driven by
# replacing MortalityLaws::fetch_text() (and hmd_session() where the reader logs
# in), and R/http.R and R/availableHMD.R by replacing httr's request surface.
# A stray real request is then a test failure, not a hung run.
#
# local_mocked_bindings() is frame-scoped: installed inside a test_that() block,
# restored when it ends. mock_httr() is the one wrapper, because httr needs five
# stubs at once; it attaches them to the caller's frame.
# --------------------------------------------

# One captured fixture as one string, the way fetch_text() returns it.
read_fixture <- function(name) {
  path <- test_path("fixtures", name)
  out  <- paste(readLines(path, warn = FALSE), collapse = "\n")
  return(out)
}

# A counter for the stub below: which URLs were asked for, how many calls ran.
new_log <- function() {
  log <- new.env(parent = emptyenv())
  log$urls  <- character()
  log$calls <- 0L
  return(log)
}

# Stand-in for MortalityLaws::fetch_text(): one canned response, and every URL
# recorded in `log`, so a test can assert the file layout the reader built and
# prove that validation ran before any download.
stub_fetch <- function(text = NULL, status = 200L, error = NULL, html = FALSE,
                       log = NULL) {
  force(text)
  force(status)
  force(error)
  force(html)
  force(log)

  function(url, session = NULL) {
    if (is.environment(log)) {
      log$urls  <- c(log$urls, url)
      log$calls <- log$calls + 1L
    }
    list(status = status, text = text, error = error, html = html)
  }
}

# Stand-in for MortalityLaws::hmd_session(); `reject` reproduces a refused login.
stub_session <- function(reject = FALSE) {
  force(reject)

  function(username, password) {
    if (reject) {
      stop("The Human Mortality Database rejected the login for ",
           username, ".", call. = FALSE)
    }
    "Authorization=test"
  }
}

# Replace httr's request surface for the rest of the calling test: `text` is the
# body every response carries, `status` its code, `fail = TRUE` makes the request
# raise, which is how a broken connection arrives.
mock_httr <- function(status = 200L, text = "", fail = FALSE,
                      post_url = "https://www.mortality.org/Home/Index",
                      cookies = data.frame(name = "Authorization", value = "test"),
                      .env = parent.frame()) {
  get_stub <- function(url, config = NULL) {
    if (fail) {
      stop("simulated connection failure")
    }
    structure(list(url = url), class = "response")
  }

  post_stub <- function(url, config = NULL, body = NULL, encode = NULL) {
    structure(list(url = post_url), class = "response")
  }

  local_mocked_bindings(
    GET         = get_stub,
    POST        = post_stub,
    status_code = function(x) status,
    content     = function(x, as = NULL, encoding = NULL, ...) text,
    cookies     = function(x) cookies,
    .package    = "httr",
    .env        = .env
  )
}
