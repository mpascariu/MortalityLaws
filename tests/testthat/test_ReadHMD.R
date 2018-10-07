rm(list = ls())

# Wrong user & password
expect_error(availableHMD(username = "fake_user", password = "fake_password"))
expect_error(ReadHMD(what = "Dx", username = "fake_user", password = "fake_password"))

# Wrong index
expect_error(ReadHMD(what = "DxDD", 
                     username = "username", password = "password"))
# Wrong country
expect_error(ReadHMD(what = "Dx", countries = "AUSS", 
                     username = "username", password = "password"))

# Wrong interval
expect_error(ReadHMD(what = "Dx", countries = "AUS", interval = "1x50", 
                     username = "username", password = "password"))

# Wrong country for the index
expect_error(ReadHMD(what = "LT_fc", countries = "AUS", interval = "1x1", 
                     username = "username", password = "password"))

# Wrong interval for the index
expect_error(ReadHMD(what = "e0", countries = "SWE", interval = "5x1", 
                     username = "username", password = "password"))
expect_error(ReadHMD(what = "e0c", countries = "SWE", interval = "5x1", 
                     username = "username", password = "password"))


