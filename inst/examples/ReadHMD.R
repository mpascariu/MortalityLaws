\dontrun{


# Download demographic data for 3 countries in 1x1 format
age_int  <- 1  # age interval: 1,5
year_int <- 1  # year interval: 1,5,10
interval <- paste0(age_int, "x", year_int)  # --> 1x1
# And the 3 countries: Sweden Denmark and USA. We have to use the HMD codes
cntr  <- c('SWE', 'DNK', 'USA')

# Download death counts. We don't want to export data outside R.
HMD_Dx <- ReadHMD(what = "Dx",
                  countries = cntr,
                  interval  = interval,
                  username  = "user@email.com",
                  password  = "password",
                  save = FALSE)
HMD_Dx

# Download life tables for female population and export data.
LTF <- ReadHMD(what = "LT_f",
               countries = cntr,
               interval  = interval,
               username  = "user@email.com",
               password  = "password",
               save = TRUE)
LTF
}
