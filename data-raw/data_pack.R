# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Sun May 02 17:02:37 2021
# --------------------------------------------------- #
remove(list = ls())
library(tidyverse)
library(MortalityLaws)

# Code used to add test data in to the package


cntr = 'GBRTENW'
years <- c(1850, 1900, 1950, 2010)
user = "your@email.com"
pass = "your_password"

Dx <- ReadHMD(what = "Dx",
              countries = cntr,
              username = user,
              password = pass,
              save = FALSE)

Ex <- ReadHMD(what = "Ex",
              countries = cntr,
              username = user,
              password = pass,
              save = FALSE)

mx <- ReadHMD(what = "mx",
              countries = cntr,
              username = user,
              password = pass,
              save = FALSE)

my_Dxf <- Dx$data %>%
  filter(country == cntr, Year %in% years) %>%
  select(Year:Female) %>%
  spread(key = Year, value = Female) %>%
  select(-Age)
rownames(my_Dxf) <- 0:110
head(my_Dxf)

my_Exf <- Ex$data %>%
  filter(country == cntr, Year %in% years) %>%
  select(Year:Female) %>%
  spread(key = Year, value = Female) %>%
  select(-Age)
rownames(my_Exf) <- 0:110
head(my_Exf)

my_mxf <- mx$data %>%
  filter(country == cntr, Year %in% years) %>%
  select(Year:Female) %>%
  spread(key = Year, value = Female) %>%
  select(-Age)
rownames(my_mxf) <- 0:110
head(my_mxf)

ahmd <- list(mx = my_mxf, Ex = my_Exf, Dx = my_Dxf)

usethis::use_data(ahmd, overwrite = TRUE)

# ----------------------------------------------------------------------------
# Save some samples for testing purposes

HMD_sample <- mx

HMD_sample$data <- mx$data %>%
  filter(Year %in% range(.$Year))
usethis::use_data(HMD_sample, overwrite = TRUE)

# ----------------------------------------------------------------------------
JMD_sample <- ReadJMD(what = "mx",
                      regions = 'Tokyo',
                      interval  = "5x1",
                      save = FALSE)

# keep only the begining and the end of the period
JMD_sample$data  <- JMD_sample$data %>%
  filter(Year %in% range(.$Year))

usethis::use_data(JMD_sample, overwrite = TRUE)

# ----------------------------------------------------------------------------
AHMD_sample <- ReadAHMD(what = "mx",
                        regions = 'ACT',
                        interval  = "5x1",
                        save = FALSE)

AHMD_sample$data  <- AHMD_sample$data %>%
  filter(Year %in% range(.$Year))
usethis::use_data(AHMD_sample, overwrite = TRUE)

# ----------------------------------------------------------------------------
CHMD_sample <- ReadCHMD(what = "mx",
                        regions = 'CAN',
                        interval  = "5x1",
                        save = FALSE)

CHMD_sample$data  <- CHMD_sample$data %>%
  filter(Year %in% range(.$Year))
usethis::use_data(CHMD_sample, overwrite = TRUE)

















