# Mon Aug 21 12:35:32 2017 ------------------------------
# Code used to add test data in to the package

rm(list = ls())
library(MortalityLaws)
library(dplyr)
library(tidyr)

cntr = 'GBRTENW'
years <- c(1850, 1900, 1950, 2010)
user = "your@email.com"
pass = "your_password"

Dx <- ReadHMD("Dx", countries = cntr, 
              username = user, password = pass, 
              save = FALSE)
Ex <- ReadHMD("Ex", countries = cntr, 
              username = user, password = pass, 
              save = FALSE)
mx <- ReadHMD("mx", countries = cntr, 
              username = user, password = pass, 
              save = FALSE)

my_Dxf <- Dx$data %>% filter(country == cntr, Year %in% years) %>% 
  select(Year:Female) %>% spread(key = Year, value = Female) %>% 
  select(-Age)
rownames(my_Dxf) <- 0:110
head(my_Dxf)

my_Exf <- Ex$data %>% filter(country == cntr, Year %in% years) %>% 
  select(Year:Female) %>% spread(key = Year, value = Female) %>% 
  select(-Age)
rownames(my_Exf) <- 0:110
head(my_Exf)

my_mxf <- mx$data %>% filter(country == cntr, Year %in% years) %>% 
  select(Year:Female) %>% spread(key = Year, value = Female) %>% 
  select(-Age)
rownames(my_mxf) <- 0:110
head(my_mxf)

ahmd <- list(mx = my_mxf, Ex = my_Exf, Dx = my_Dxf)

devtools::use_data(ahmd, overwrite = TRUE)
