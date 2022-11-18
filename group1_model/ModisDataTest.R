library(MODIS)
library(MODISTools)

#Coordinates for the site, will need to find a way to code these to be pulled from the site metadata
latitude <- c(46.23391)
longitude <- c(-89.537254)
period <- data.frame(lat=lat,long=long, start.date=2017,end.date=2022,id=1)

#Pulling MODIS LAI for the 500m pixel our site is in. 2 different MODIS LAI products to test
MODISsubset <- mt_subset(product = "MOD15A2H", band = c("Lai_500m","FparLai_QC"), lat=latitude, lon=longitude, start="2017-01-01", end=format(Sys.time(),"%Y-%m-%d")) #8 day LAI from Terra satellite
MODISsubset2 <- mt_subset(product = "MCD15A3H", band = c("Lai_500m","FparLai_QC"), lat=latitude, lon=longitude, start="2017-01-01", end=format(Sys.time(),"%Y-%m-%d")) #4 day LAI from Terra + Aqua satellites

#Need to filter out bad data using the QC flags in the FparLAI_QC column