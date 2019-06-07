# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: MIT
# Last update: Fri Jun 07 13:50:43 2019
# --------------------------------------------------- #
remove(list = ls())


# Wrong index
expect_error(ReadCHMD(what = "DxDD"))
# Wrong country
expect_error(ReadCHMD(what = "Dx", regions = "CANN"))

# Wrong interval
expect_error(ReadCHMD(what = "Dx", regions = "CAN", interval = "1x50"))

# Wrong country for the index
expect_error(ReadCHMD(what = "LT_fc", regions = "SAS", interval = "1x1"))

# Wrong interval for the index
expect_error(ReadCHMD(what = "e0", regions = "CAN", interval = "5x1", show = F))
expect_error(ReadCHMD(what = "LT_f", regions = "YUK", interval = "5x1", show = F))

# Test the show arg and print function
expect_silent(D <- ReadCHMD(what = "LT_f",
                            regions = "CAN",
                            interval = "5x10",
                            show = F))
expect_output(print(D))
