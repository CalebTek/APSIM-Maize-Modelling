library(apsimx)
library(tidyverse)

lat <- 7.250395934	
lon <- 5.199982054

#get_gsod_apsim_met(lonlat = c(lon,lat), dates = c("1990-01-01", "2020-12-31"), fillin.radn = TRUE)

ames_power <- get_power_apsim_met(lonlat = c(lon,lat), dates = c("1990-01-01", "2020-12-31"))
