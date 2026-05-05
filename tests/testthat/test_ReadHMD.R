# --------------------------------------------
# Author: Marius D PASCARIU
# Date: 2026-05-05 18:52:40
# --------------------------------------------
remove(list = ls())

# Test 1: Invalid credentials
# Logic: When wrong username/password are provided, the function should send
# a message (not an error) indicating authentication failure, because the
# function attempts to connect and then reports failure gracefully.
expect_message(ReadHMD(what     = "Dx",
                       countries = "AUS",
                       username = "fake_user",
                       password = "fake_password"))


# Test 2: Invalid index (indicator) name
# Logic: "DxDD" is not a valid HMD indicator; the function's input validation
# should detect this and throw an error before attempting any download.
expect_error(ReadHMD(what     = "DxDD",
                     countries = "AUS",
                     username = "username",
                     password = "password"))


# Test 3: Invalid country code
# Logic: "AUSS" is not a valid 3-letter ISO country code in HMD; the function's
# validation should reject it and throw an error.
expect_error(ReadHMD(what      = "Dx",
                     countries = "AUSS",
                     username  = "username",
                     password  = "password"))


# Test 4: Invalid interval format
# Logic: "1x50" is not a recognized interval string (expected patterns like
# "1x1", "1x5", "5x5", etc.); the function should error.
expect_error(ReadHMD(what      = "Dx",
                     countries = "AUS",
                     interval  = "1x50",
                     username  = "username",
                     password  = "password"))


# Test 5: Wrong country for a cohort-type index
# Logic: "LT_fc" (cohort life table, females) may not be available for all
# countries in HMD. Trying to fetch this indicator for "AUS" should trigger
# an error because the raw data does not contain that specific table for that country.
expect_error(ReadHMD(what      = "LT_fc",
                     countries = "AUS",
                     interval  = "1x1",
                     username  = "username",
                     password  = "password"))


# Test 6: Wrong interval for period life expectancy
# Logic: "e0" (period life expectancy at birth) is a period measure that requires
# a "1x1" interval; using "5x1" is invalid and the function should error.
expect_error(ReadHMD(what      = "e0",
                     countries = "SWE",
                     interval  = "5x1",
                     username  = "username",
                     password  = "password"))

# Test 7: Wrong interval for cohort life expectancy
# Logic: "e0c" (cohort life expectancy at birth) also requires a "1x1" interval;
# any other interval like "5x1" should be rejected.
expect_error(ReadHMD(what      = "e0c",
                     countries = "SWE",
                     interval  = "5x1",
                     username  = "username",
                     password  = "password"))

# Test that the built-in sample dataset HMD_sample prints without error.
# This verifies the print method works on the HMD sample data object.
expect_output(
  print(HMD_sample)
)

# ----------------------------------------------------------------------------
# (Commented-out section) Manual testing block used during development to
# verify each indicator works with "1x10" interval for Sweden.

