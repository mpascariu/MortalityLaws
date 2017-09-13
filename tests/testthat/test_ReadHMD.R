library(testthat)
library(MortalityLaws)


test_that("ReadHMD", {
  expect_error(ReadHMD(what = "Dx", username = "fake_user", password = "fake_password"))
})


