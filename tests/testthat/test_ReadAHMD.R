# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: MIT
# Last update: Thu Jun 06 13:16:25 2019
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

# ReadAHMD(what = "LT_m", regions = "TAS", interval = "1x5")
# ReadAHMD(what = "LT_f", regions = "TAS", interval = "1x1")
# ReadAHMD(what = "LT_t", regions = "TAS", interval = "1x1")
