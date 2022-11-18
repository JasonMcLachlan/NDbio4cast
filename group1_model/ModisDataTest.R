library(MODIS)
library(MODISTools)

latitude <- c(46.23391)
longitude <- c(-89.537254)
period <- data.frame(lat=lat,long=long, start.date=2017,end.date=2022,id=1)
MODISSubsets(LoadDat = period, Products = "MOD15A2",
             Bands = c("Lai_1km", "Fpar_1km", "FparLai_QC"),
             Size = c(0,0))

MODISsubset <- mt_subset(product = "MOD15A2H", band = c("Lai_500m","FparLai_QC"), lat=latitude, lon=longitude, start="2017-01-01", end=format(Sys.time(),"%Y-%m-%d"))

MODISsubset2 <- mt_subset(product = "MCD15A3H", band = c("Lai_500m","FparLai_QC"), lat=latitude, lon=longitude, start="2017-01-01", end=format(Sys.time(),"%Y-%m-%d"))
