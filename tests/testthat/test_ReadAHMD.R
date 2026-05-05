# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-05 18:52:11
# --------------------------------------------
remove(list = ls())

# Test 1: Invalid index (indicator) name
# Logic: "DxDD" is not a valid AHMD indicator; the function's input validation
# should detect this and throw an error before attempting any data retrieval.
expect_error(ReadAHMD(what = "DxDD"))

# Test 2: Invalid region name
# Logic: "ACTT" is not a valid Australian state/territory code (correct code is "ACT");
# the function should reject it with an error.
expect_error(ReadAHMD(what    = "Dx",
                      regions = "ACTT"))

# Test 3: Invalid interval format
# Logic: "1x50" is not a recognized interval pattern (expected formats like "1x1", "1x5", etc.); expect an error.
expect_error(ReadAHMD(what     = "Dx",
                      regions  = "ACT",
                      interval = "1x50"))

# Test 4: Wrong region for a cohort-type index
# Logic: "LT_fc" (cohort female life table) requires a standard region code; "TAS_LT_fc" is
# not a valid region identifier — the region should be just "TAS". Expect error.
expect_error(
  ReadAHMD(what     = "LT_fc",
           regions  = "TAS_LT_fc",
           interval = "1x1",
           show     = FALSE))

# Test 5: Wrong interval for period life expectancy
# Logic: "e0" (life expectancy at birth) is a period measure that requires "1x1" interval;
# using "5x1" is invalid and the function should error.
expect_error(
  ReadAHMD(what     = "e0",
           regions  = "TAS",
           interval = "5x1",
           show     = FALSE))

# Test that the built-in sample dataset AHMD_sample prints without error.
expect_output(
  print(AHMD_sample)
)

# (Commented out - was previously expected to fail, but now the data is available)
# expect_error(ReadAHMD(what = "LT_f", regions = "TAS", interval = "1x1"))

# The tests below have been removed because internet-dependent tests can
# cause false CRAN failures when the remote server is temporarily down.
# We only test the input validation checks (which require no internet access).
# # Test the show arg and print function
# expect_silent(D <- ReadAHMD(what     = "LT_f",
#                             regions  = "ACT",
#                             interval = "5x10",
#                             show     = F))
# expect_output(print(D))

