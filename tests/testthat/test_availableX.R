# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-05 18:50:54
# --------------------------------------------
remove(list = ls())

# Test that availableLaws() returns an object of class "availableLaws"
# and that its components (table and legend) are non-null and printable.
AL = availableLaws()
expect_true(class(AL) == "availableLaws")
expect_false(is.null(AL$table))
expect_false(is.null(AL$legend))
expect_output(print(AL))

# Test filtering by a specific law name ('rogersplanck')
AL2 = availableLaws(law = 'rogersplanck')
expect_output(print(AL2))

# Test that requesting a non‑existent law name triggers an error.
A <- availableLaws()
law <- 'rogersplanck'

expect_error(availableLaws(law = "notavailable"))

# Test that availableLF() works similarly for loss functions.
AF = availableLF()
expect_true(class(AF) == "availableLF")
expect_false(is.null(AF$table))
expect_false(is.null(AF$legend))
expect_output(print(AF))

