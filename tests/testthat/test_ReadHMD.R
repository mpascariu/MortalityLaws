rm(list = ls())

expect_error(ReadHMD(what = "Dx", username = "fake_user", password = "fake_password"))
expect_error(ReadHMD(what = "DxDD", username = "username", password = "password"))
expect_error(ReadHMD(what = "Dx", countries = "AUSS", 
                     username = "username", password = "password"))
expect_error(ReadHMD(what = "Dx", countries = "AUS", interval = "1x50", 
                     username = "username", password = "password"))

expect_error(availableHMD(username = "fake_user", password = "fake_password"))

