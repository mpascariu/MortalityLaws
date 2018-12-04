# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: GNU General Public License v3.0
# Last update: Tue Dec  4 15:58:12 2018
# --------------------------------------------------- #
remove(list = ls())

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



A_HMD <- availableHMD()
expect_output(print(A_HMD))
expect_true(class(A_HMD) == "availableHMD")
