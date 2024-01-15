# -------------------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last Update: Mon Jan 15 16:53:07 2024
# -------------------------------------------------------------- #
remove(list = ls())

# Wrong index
expect_error(ReadCHMD(what = "DxDD"))

# Wrong country
expect_error(ReadCHMD(what = "Dx",
                      regions = "CANN"))

# Wrong interval
expect_error(ReadCHMD(what = "Dx",
                      regions = "CAN",
                      interval = "1x50"))

# Wrong region for the index
expect_error(ReadCHMD(what = "LT_fc",
                      regions = "SAS",
                      interval = "1x1"))

# Wrong interval for the index
expect_message(
  ReadCHMD(what = "e0",
           regions = "CAN",
           interval = "5x1",
           show = F))

expect_error(ReadCHMD(what = "LT_f",
                      regions = "YUK",
                      interval = "5x1",
                      show = F))

expect_output(
  print(CHMD_sample)
)

# The tests below have been removed because in case the internet source is
# temporary not working the CRAN will consider it as a software failure and will
# demand correction. Unfortunately, we can test only the error messages i.e. the
# automated checks put in place.

# # Test the show arg and print function
# expect_silent(D <- ReadCHMD(what = "LT_f",
#                             regions = "CAN",
#                             interval = "5x10",
#                             show = F))
# expect_output(print(D))
