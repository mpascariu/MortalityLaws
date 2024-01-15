# -------------------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last Update: Mon Jan 15 16:54:54 2024
# -------------------------------------------------------------- #
remove(list = ls())

# Wrong index
expect_error(ReadJMD(what = "DxDD"))

# Wrong region
expect_error(ReadJMD(what = "Dx",
                     regions = "Kyotooooooo"))

# Wrong interval
expect_error(ReadJMD(what = "Dx",
                     regions = "Kyoto",
                     interval = "1x50"))

# Wrong interval for the index
expect_message(ReadJMD(what = "Ex_lexis",
                     regions = "Japan",
                     interval = "1x1"))

expect_message(ReadJMD(what = "e0",
                     regions = "Japan",
                     interval = "5x1"))

# remove test since the case is now valid and available in the database.
# expect_error(ReadJMD(what = "LT_f",
#                      regions = "Kyoto",
#                      interval = "1x1"))

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
