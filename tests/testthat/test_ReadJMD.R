# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-05 18:52:50
# --------------------------------------------
remove(list = ls())

# Test 1: Invalid index argument
# Logic: The function should error when given a non-existent indicator name ("DxDD").
expect_error(ReadJMD(what = "DxDD"))

# Test 2: Invalid region name
# Logic: "Kyotooooooo" is not a valid Japanese prefecture name, so the function should error.
expect_error(ReadJMD(what = "Dx",
                     regions = "Kyotooooooo"))

# Test 3: Invalid interval format
# Logic: "1x50" is not a recognized interval format (should be like "1x1", "5x5", etc.).
expect_error(ReadJMD(what = "Dx",
                     regions = "Kyoto",
                     interval = "1x50"))

# Test 4: Interval mismatch for a lexis‑type index
# Logic: For "Ex_lexis" data the interval must be "1x1"; using "1x1" is correct so no error,
# but a message is expected regarding the interval format for this index type.
expect_message(ReadJMD(what = "Ex_lexis",
                     regions = "Japan",
                     interval = "1x1"))

# Test 5: Interval mismatch for life‑expectancy index
# Logic: "e0" (life expectancy at birth) is a period measure and cannot be retrieved with a "5x1" interval;
# the function should issue a message about the expected interval format.
expect_message(ReadJMD(what = "e0",
                     regions = "Japan",
                     interval = "5x1"))

# (Commented out) Test that was previously expected to fail but is now valid.
# expect_error(ReadJMD(what = "LT_f",
#                      regions = "Kyoto",
#                      interval = "1x1"))

# Test that the built-in sample dataset JMD_sample prints without error.
expect_output(
  print(JMD_sample)
)

# The tests below have been removed because in case the internet source is
# temporary not working the CRAN will consider it as a software failure and will
# demand correction. Unfortunately, we can test only the error messages i.e. the
# automated checks put in place.

# expect_output(ReadJMD(what = "e0",
#                       regions = "Japan",
#                       interval = "1x5"))
#
# expect_output(ReadJMD(what = "LT_f",
#                       regions = "Kyoto",
#                       interval = "5x5"))
#
# expect_silent(D <- ReadJMD(what = "LT_f",
#                            regions = "Kyoto",
#                            interval = "5x5",
#                            show = FALSE))
# expect_output(print(D))

