# -------------------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last Update: Mon Jan 15 18:12:12 2024
# -------------------------------------------------------------- #
remove(list = ls())

# Wrong user & password
expect_message(ReadHMD(what     = "Dx",
                       countries = "AUS",
                       username = "fake_user",
                       password = "fake_password"))

# Wrong index
expect_error(ReadHMD(what     = "DxDD",
                     countries = "AUS",
                     username = "username",
                     password = "password"))
# Wrong country
expect_error(ReadHMD(what      = "Dx",
                     countries = "AUSS",
                     username  = "username",
                     password  = "password"))

# Wrong interval
expect_error(ReadHMD(what      = "Dx",
                     countries = "AUS",
                     interval  = "1x50",
                     username  = "username",
                     password  = "password"))

# Wrong country for the index
expect_error(ReadHMD(what      = "LT_fc",
                     countries = "AUS",
                     interval  = "1x1",
                     username  = "username",
                     password  = "password"))

# Wrong interval for the index
expect_error(ReadHMD(what      = "e0",
                     countries = "SWE",
                     interval  = "5x1",
                     username  = "username",
                     password  = "password"))

expect_error(ReadHMD(what      = "e0c",
                     countries = "SWE",
                     interval  = "5x1",
                     username  = "username",
                     password  = "password"))

expect_output(
  print(HMD_sample)
)

# ----------------------------------------------------------------------------


# Marius D. Pascariu --- Sun Oct  7 14:37:24 2018 ------------------------------

# cntr <- c('SWE')
# int <- c("1x10")
# wht <- c("births", "population", "Dx_lexis", "Ex_lexis", "Dx",
#          "mx", "Ex", "LT_f", "LT_m", "LT_t", "e0",
#          "mxc", "Exc", "LT_fc", "LT_mc", "LT_tc", "e0c")
# u = "..."
# p = "..."
# expand.grid(wht, int)
#
#
# for (w in wht) {
#   cat(w, "\n")
#   assign(w, ReadHMD(what = w, countries = cntr, interval  = int,
#                     username  = u, password  = p))
# }
#
#
# births
# population
# Dx_lexis
# Ex_lexis
# Dx
# mx
# Ex
# LT_f
# LT_m
# LT_t
# e0
# mxc
# Exc
# LT_fc
# LT_mc
# LT_tc
# e0c
