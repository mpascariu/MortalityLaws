rm(list = ls())

AL = availableLaws()
expect_true(class(AL) == "availableLaws")
expect_false(is.null(AL$table))
expect_false(is.null(AL$legend))
expect_output(print(AL))

AL2 = availableLaws(law = 'rogersplanck')
expect_output(print(AL2))


A <- availableLaws()
law <- 'rogersplanck'


expect_error(availableLaws(law = "notavailable"))


AF = availableLF()
expect_true(class(AF) == "availableLF")
expect_false(is.null(AF$table))
expect_false(is.null(AF$legend))
expect_output(print(AF))
