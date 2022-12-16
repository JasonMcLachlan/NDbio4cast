##load libraries
#install.packages("neonUtilities")
library(neonUtilities)
library(lubridate)


#Load Target Data
download_targets <- function(){
  readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz", guess_max = 1e6)
}

target<-download_targets()

##subset target data
target<-subset(target, target$site_id == "PRPO")
target<-subset(target, target$variable == "chla")

##plot target data over time
plot(target$datetime,target$observation,type="p",xlab="Date",ylab = "Chl-A")

##Start and end date of data availability (2018-2022)
summary(target$datetime)

##resolution of data = DAILY
table(target$datetime)
target.date<-format(as.POSIXct( target$datetime,format='%m/%d/%Y'),format='%m/%d/%Y')

target.data<-cbind(target.date,target$observation)
colnames(target.data)<-c("date","target")



##% NAs
na.perc<-sum(is.na(target$observation))/nrow(target)

## pull predictor variables 
## dissolved o2 is available monthly (2016-2022)

  dissolved.gasses <- loadByProduct(dpID="DP1.20097.001", 
                                    site=c("PRPO"),
                                    check.size = F)
  
  ## extract date and dissolved 02 data
  time.gas<-dissolved.gasses$sdg_fieldSuperParent$collectDate
  time.gas<-lubridate::as_datetime(time.gas)
  
  ##format date to merge with target data
  time.gas.format<-format(as.POSIXct( time.gas,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')
  dissolved.02<-dissolved.gasses$sdg_fieldSuperParent$dissolvedOxygen
  dissolved.02.time<-cbind(time.gas.format,dissolved.02)
  
  dissolved.O2.data<-cbind(time.gas.format,dissolved.02)
  colnames(dissolved.O2.data)<-c("date","observation")
  
  
  ##plot time series
  plot(time.gas,dissolved.02, type="p",xlab = "Dissolved O2", ylab= "time")
  
##data summaries 
summary(time.gas)
table(time.gas)

  



##Biomass 2014-2021, every few months

  biomass <- loadByProduct(dpID="DP1.20163.001", 
                           site=c("PRPO"),
                           check.size = F)
  
  ## extract date and biomass data
  time.biomass<-biomass$alg_fieldData$collectDate
  time.biomass<-lubridate::as_datetime(time.biomass)
  
  ##format date to merge with target data
  time.biomass.format<-format(as.POSIXct( time.biomass,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')
 

  
  phyto<-biomass$alg_fieldData$phytoDepth1
  phyto.time<-cbind(time.biomass,phyto)
  phyto.time.format<-cbind(time.biomass.format,  phyto)
  colnames(phyto.time.format)<-c("date","observation")
  
  ##plot time series
  plot(time.biomass, phyto,type="p", xlab = "Time", ylab = "Biomass")
  
  #data summaries 
  print(summary(time.biomass))
  print(table(time.biomass))
  
  


##water temperature available from 2018-22
##data resolution every 30 minutes


  watertemp <- loadByProduct(dpID="DP1.20264.001", 
                             site=c("PRPO"),
                             check.size = F)
  
  ## extract date and water temp data 
  time.watertemp<-watertemp$TSD_30_min$startDateTime
  time.watertemp<-lubridate::as_datetime(time.watertemp)
  
  ##format date to merge with target data
  time.watertemp.format<-format(as.POSIXct( time.watertemp,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')
  
temp_depth<-watertemp$TSD_30_min$tsdWaterTempMean
  
watertemp.time.format<-cbind(  time.watertemp.format, temp_depth)
watertemp.time<-cbind(time.watertemp,temp_depth)
colnames(watertemp.time.format)<-c("date","observation")
  
  ##plot time series
  plot(time.watertemp, temp_depth,type="p", xlab = "Time", ylab = "Water temp")
  
  #data summaries 
summary(time.watertemp)
table(time.watertemp)


##predictor variables vs target variable
##plot dissolved oxy vs target data
disO2.target<-merge(target.data, dissolved.O2.data, by="date", all=TRUE)
plot(disO2.target$observation,disO2.target$target,type="p",xlab = "Dissolved O2", ylab= "Chl-A")

##biomass vs chlorophyl
biomass.target<-merge(target.data, phyto.time.format, by="date", all=TRUE)
plot(biomass.target$observation,biomass.target$target,type="p",xlab ="Biomass",ylab ="Chl-A")

#water temp vs Chla
watertemp.target<-merge(target.data, watertemp.time.format, by="date", all=TRUE)
plot(watertemp.target$observation,watertemp.target$target,type="p",xlab ="Water temp",ylab ="Chl-A")


