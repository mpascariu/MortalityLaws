# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: MIT
# Last update: Fri Jun 07 13:53:02 2019
# --------------------------------------------------- #
remove(list = ls())


# Wrong index
expect_error(ReadAHMD(what = "DxDD"))
# Wrong country
expect_error(ReadAHMD(what = "Dx", regions = "ACTT"))

# Wrong interval
expect_error(ReadAHMD(what = "Dx", regions = "ACT", interval = "1x50"))

# Wrong country for the index
expect_error(ReadAHMD(what = "LT_fc", regions = "TAS", interval = "1x1", show = FALSE))

# Wrong interval for the index
expect_error(ReadAHMD(what = "e0", regions = "TAS", interval = "5x1", show = FALSE))
# expect_error(ReadAHMD(what = "LT_f", regions = "TAS", interval = "1x1"))

# Test the show arg and print function
expect_silent(D <- ReadAHMD(what = "LT_f",
                            regions = "ACT",
                            interval = "5x10",
                            show = F))
expect_output(print(D))
