# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-10-04
# --------------------------------------------
# R/http.R and R/availableHMD.R, the two httr wrappers. mock_httr() replaces the
# request surface inside each block, so nothing here touches the network.
# --------------------------------------------

LOGIN_PAGE        <- '<input name="__RequestVerificationToken" value="T1">'
AVAILABILITY_PAGE <- paste0("<table><tr><th>Country</th><th>Years</th></tr>",
                            "<tr><td>Sweden</td><td>1751-2020</td></tr></table>")

test_that("fetch_text keeps a body only when there is one", {
  # The text survives only as one non-empty string; the error names the status.
  mock_httr(status = 200L, text = "Age Mx\n0 0.01")
  out <- MortalityLaws:::fetch_text("http://x/y.txt")
  expect_equal(out$status, 200L)
  expect_equal(out$text, "Age Mx\n0 0.01")
  expect_null(out$error)

  mock_httr(status = 403L, text = "forbidden")
  out <- MortalityLaws:::fetch_text("http://x/y.txt")
  expect_equal(out$status, 403L)
  expect_equal(out$text, "forbidden")
  expect_equal(out$error, "The server returned HTTP 403 for http://x/y.txt")

  mock_httr(status = 403L, text = "")
  out <- MortalityLaws:::fetch_text("http://x/y.txt")
  expect_null(out$text)
  expect_equal(out$error, "The server returned HTTP 403 for http://x/y.txt")

  mock_httr(status = 200L, text = "")
  out <- MortalityLaws:::fetch_text("http://x/y.txt")
  expect_null(out$text)
  expect_null(out$error)
  expect_false(out$html)
})

test_that("fetch_text flags HTML, sends a session, survives a dead connection", {
  mock_httr(status = 200L, text = "<!DOCTYPE html><html><body>x</body></html>")
  out <- MortalityLaws:::fetch_text("http://x/y.txt")
  expect_true(out$html)

  mock_httr(status = 200L, text = "ok")
  out <- MortalityLaws:::fetch_text("http://x/y.txt", session = "a=1; b=2")
  expect_equal(out$text, "ok")

  mock_httr(fail = TRUE)
  out <- MortalityLaws:::fetch_text("http://x/y.txt")
  expect_true(is.na(out$status))
  expect_null(out$text)
  expect_match(out$error, "^Could not connect to http://x/y\\.txt:")
  expect_false(out$html)
})

test_that("session_cookies and extract_login_token parse the login strings", {
  expect_equal(MortalityLaws:::session_cookies("a=1; b=2"),
               c(a = "1", b = "2"))

  expect_equal(MortalityLaws:::extract_login_token(html = LOGIN_PAGE), "T1")

  # No such input, or an input with an empty value, both mean "no token".
  expect_null(MortalityLaws:::extract_login_token(html = "<html>x</html>"))
  expect_null(MortalityLaws:::extract_login_token(
    html = '<input name="__RequestVerificationToken" value="">'
  ))
})

test_that("hmd_session returns the cookie, or reports why the login failed", {
  mock_httr(text = LOGIN_PAGE)
  expect_identical(MortalityLaws:::hmd_session("u", "p"), "Authorization=test")

  mock_httr(text = "<html><body>login</body></html>")
  expect_error(MortalityLaws:::hmd_session("u", "p"),
               "Could not find the HMD login token")

  mock_httr(text     = LOGIN_PAGE,
            post_url = "https://www.mortality.org/Account/Login")
  expect_error(MortalityLaws:::hmd_session("u", "p"), "rejected the login")

  # Cookie-less landing: the guard tests nrow(session), not paste0()'s "=".
  mock_httr(text    = LOGIN_PAGE,
            cookies = data.frame(name = character(), value = character()))
  expect_error(MortalityLaws:::hmd_session("u", "p"),
               "no session cookie was set")
})

test_that("availableHMD parses the table, or messages why it has none", {
  mock_httr(status = 200L, text = AVAILABILITY_PAGE)
  out <- availableHMD(link = "http://x/Data")
  expect_true(is.data.frame(out))
  expect_equal(colnames(out), c("Country", "Years"))
  expect_equal(out$Country, "Sweden")
  expect_equal(out$Years, "1751-2020")

  mock_httr(fail = TRUE)
  expect_message(out <- availableHMD(link = "http://x/Data"),
                 "Could not connect to http://x/Data:")
  expect_null(out)

  mock_httr(status = 404L, text = "gone")
  expect_message(out <- availableHMD(link = "http://x/Data"), "returned HTTP")
  expect_null(out)

  mock_httr(status = 200L, text = "")
  expect_message(out <- availableHMD(link = "http://x/Data"), "empty response")
  expect_null(out)

  mock_httr(status = 200L, text = "<html><body>no table</body></html>")
  expect_message(out <- availableHMD(link = "http://x/Data"),
                 "could not be parsed as HTML")
  expect_null(out)
})
