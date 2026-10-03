# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-05 18:50:54
# --------------------------------------------

test_that("availableLaws() returns a printable catalogue", {
  AL <- availableLaws()
  expect_s3_class(AL, "availableLaws")
  expect_false(is.null(AL$table))
  expect_false(is.null(AL$legend))
  expect_output(print(AL))
})

test_that("availableLaws() filters by law code", {
  AL2 <- availableLaws(law = "rogersplanck")
  expect_s3_class(AL2, "availableLaws")
  expect_identical(unique(AL2$table$CODE), "rogersplanck")
  expect_output(print(AL2))
})

test_that("availableLaws() rejects unknown law codes", {
  expect_error(availableLaws(law = "notavailable"), regexp = "not available")
})

test_that("availableLF() returns a printable catalogue", {
  AF <- availableLF()
  expect_s3_class(AF, "availableLF")
  expect_false(is.null(AF$table))
  expect_false(is.null(AF$legend))
  expect_output(print(AF))
})
