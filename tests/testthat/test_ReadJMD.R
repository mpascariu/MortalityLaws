# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: MIT
# Last update: Fri Jun 07 10:29:11 2019
# --------------------------------------------------- #
remove(list = ls())


# Wrong index
expect_error(ReadJMD(what = "DxDD"))
# Wrong region
expect_error(ReadJMD(what = "Dx", regions = "Kyotooooooo"))

# Wrong interval
expect_error(ReadJMD(what = "Dx", regions = "Kyoto", interval = "1x50"))

# Wrong interval for the index
expect_error(ReadJMD(what = "Ex_lexis", regions = "Japan", interval = "1x1"))
expect_error(ReadJMD(what = "e0", regions = "Japan", interval = "5x1"))
expect_output(ReadJMD(what = "e0", regions = "Japan", interval = "1x5"))
expect_error(ReadJMD(what = "LT_f", regions = "Kyoto", interval = "1x1"))
expect_output(ReadJMD(what = "LT_f", regions = "Kyoto", interval = "5x5"))

expect_silent(D <- ReadJMD(what = "LT_f",
                           regions = "Kyoto",
                           interval = "5x5",
                           show = FALSE))
expect_output(print(D))
