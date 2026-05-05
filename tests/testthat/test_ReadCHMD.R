# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-05 18:52:25
# --------------------------------------------
remove(list = ls())

# Test 1: Invalid index (indicator) name
# Logic: "DxDD" is not a valid CHMD indicator; the function should raise
# an error during input validation before attempting any data retrieval.
expect_error(ReadCHMD(what = "DxDD"))

# Test 2: Invalid region name
# Logic: "CANN" is not a valid Canadian region code; the function's internal
# validation should detect this and throw an error.
expect_error(ReadCHMD(what = "Dx",
                      regions = "CANN"))

# Test 3: Invalid interval format
# Logic: "1x50" is not a recognized interval pattern; the function should error.
expect_error(ReadCHMD(what = "Dx",
                      regions = "CAN",
                      interval = "1x50"))

# Test 4: Wrong region for a cohort-type index
# Logic: "LT_fc" (cohort life table - females) is not available for region
# "SAS" (Saskatchewan) in CHMD; the function should error.
expect_error(ReadCHMD(what = "LT_fc",
                      regions = "SAS",
                      interval = "1x1"))

# Test 5: Wrong interval for period life expectancy
# Logic: "e0" (period life expectancy at birth) requires a "1x1" interval;
# using "5x1" should trigger a message informing the user about the correct
# interval requirement (since the function may auto-correct it).
expect_message(
  ReadCHMD(what = "e0",
           regions = "CAN",
           interval = "5x1",
           show = F))

# Test 6: Wrong interval for period life table by sex
# Logic: "LT_f" (female period life table) requires a "1x1" interval;
# using "5x1" is invalid and should error.
expect_error(ReadCHMD(what = "LT_f",
                      regions = "YUK",
                      interval = "5x1",
                      show = F))

# Test that the built-in sample dataset CHMD_sample prints without error.
# This verifies the print method for CHMD sample data.
expect_output(
  print(CHMD_sample)
)

# The tests below have been removed because internet-dependent tests can
# cause false CRAN failures when the remote server is temporarily down.
# We only test the input validation checks (which require no internet).
# # Test the show arg and print function
# expect_silent(D <- ReadCHMD(what = "LT_f",
#                             regions = "CAN",
#                             interval = "5x10",
#                             show = F))
# expect_output(print(D))

