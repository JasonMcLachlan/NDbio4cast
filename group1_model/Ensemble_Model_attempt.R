library(tidyverse)
library(neon4cast)
library(lubridate)
#install.packages("rMR")
library(rMR)
library(dplyr)
library(rjags)
##' Download Targets
##' @return data.frame in long format with days as rows, and time, site_id, variable, and observed as columns
target <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/terrestrial_daily/terrestrial_daily-targets.csv.gz", guess_max = 1e6) |> 
  na.omit()
target <- target %>% filter(variable == "nee")

##' Download Site metadata
##' @return metadata dataframe
site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") 
site_data %>% filter(as.integer(terrestrial) == 1)

#Filter for UNDERC NEE
underc <- target %>% filter(site_id == "UNDE")

#Plot UNDERC NEE over time
plot(underc$datetime,underc$observation, type="p", pch=1, cex=0.5, main="NEE at UNDERC", xlab="Date", ylab="NEE (gC/m2/day)")
#Winter NEE values are in the range from 0 to slightly positive (emitting CO2 to atmosphere)
#Summer NEE vallues drop in to the negatives (absorbing C02)

forecast_date <- Sys.Date()#assign the nee forecast date as today (the date on the computer)
noaa_date <- Sys.Date() - lubridate::days(1)#assign weather forecast date as yesterday (todays is not available yet)  

#Download past weather estimates from the NOAA using the neon4cast package
df_past <- neon4cast::noaa_stage3()

#Download future weather forecasts from the NOAA
df_future <- neon4cast::noaa_stage2()

sites <- unique(target$site_id) #unique site ids

#Add column to target with Julian day
underc$julianday <- yday(underc$datetime)

#Average NEE observations by julian day
DOYaverage <- underc %>% group_by(julianday) %>% summarise(NEEav = mean(observation))

#plot NEE vs DAY of Year
plot(DOYaverage$julianday,DOYaverage$NEEav, type="p", pch=1, cex=0.5, main="NEE at UNDERC", xlab="Day of year", ylab="NEE (gC/m2/day)")

undercdata <- left_join(underc, DOYaverage, by = c("julianday" = "julianday"))

###SUNLIGHT
#past weather estimates of shortwave flux (solar radiation) from NOAA
noaa_past_sw <- df_past |> 
  dplyr::filter(site_id %in% sites,
                variable == "surface_downwelling_shortwave_flux_in_air", 
  ) |> 
  dplyr::collect()

#Filter for UNDERC
noaa_past_sw_underc <- noaa_past_sw |> 
  dplyr::filter(site_id == "UNDE" ) |> 
  dplyr::collect()

#Filter for 12:00pm daily
noaa_past_sw_underc_daily=noaa_past_sw_underc[which(substr(noaa_past_sw_underc$datetime,12,17)=="12:00:"),]
#averaging by day
noaa_past_sw_dailyavg=data.frame(matrix(ncol=2, nrow=length(unique(substr(noaa_past_sw_underc$datetime,1,11)))))
noaa_past_sw_dailyavg[,1]=unique(substr(noaa_past_sw_underc$datetime,1,11))
for(day in 1:nrow(noaa_past_sw_dailyavg)){
  days=which(noaa_past_sw_dailyavg[day,1]==substr(noaa_past_sw_underc_daily$datetime, 1, 11))
  value=(sum(noaa_past_sw_underc_daily[days,9]))/length(days)
  noaa_past_sw_dailyavg[day,2]=value
}
colnames(noaa_past_sw_dailyavg)=c("Date", "Average_sw_at_noon")

noaa_past_sw_dailyavg$julianday <- yday(noaa_past_sw_dailyavg$Date)

av_sw_julian <- noaa_past_sw_dailyavg %>% group_by(julianday) %>%
  summarise(mean_sw_noon = mean(Average_sw_at_noon))

av_sw_NEE_julian <- left_join(DOYaverage, av_sw_julian, by = c('julianday'))

julian_day = yday(noaa_date)

linear_model <- lm(NEEav~mean_sw_noon, data = av_sw_NEE_julian) # estimating relationship between NEEav and mean_sw for JAGS model
int <- as.double(linear_model$coefficients[1]) 
slope <- as.double(linear_model$coefficients[2])

days_into_future = 14

#JAGS code for model
NEEmodel = "
model{
  
  #### Data Model : NEEav
  for(t in 1:n){
    z[t] ~ dnorm(NEEav[day+t],tau_NEE)
  }
  
  #### Data Model : sw_noon
    for(t in 1:n){
    y[t] ~ dnorm(sw[day+t],tau_sw)
    predict_sw[t] <- y[t]*int + slope
  }
  
  #### Process Model
  for(t in 1:n){
    NEE_new[t] <- (z[t] + predict_sw[t])/2
    NEE[t]~dnorm(NEE_new[t],tau_add)
  }
  
  #### Priors
  tau_add ~ dnorm(a_add,r_add)
  tau_sw ~ dnorm(a_sw, r_sw)
  tau_NEE ~ dnorm(a_NEE, r_NEE)
}
"

NEEav <- av_sw_NEE_julian$NEEav
sw <- av_sw_NEE_julian$mean_sw_noon
#time <- as.Date(undercdata$datetime)

data <- list(NEEav = NEEav, sw = sw,n= days_into_future, day = julian_day, slope = slope, int = int,      ## data
             a_add=0,r_add=1,            ## process error prior
             a_sw =0, r_sw =1,
             a_NEE=0, r_NEE=1
)


#define initial state of model parameters for MCMC chains
nchain = 3
init <- list()
for(i in 1:nchain){
  NEEav.samp = sample(NEEav,length(NEEav),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff((NEEav))),  ## initial guess on process precision
                    tau_sw =1,
                    tau_NEE = 1)        ## initial guess on obs precision
}

#run model
j.model   <- jags.model (file = textConnection(NEEmodel),
                         data = data,
                         inits = init,
                         n.chains = 3)

#take sample from MCMC output
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("tau_add","tau_sw", 'tau_NEE'),
                            n.iter = 10000)
plot(jags.out)


#plot
time.rng = c(1,length(time))       ## adjust to zoom in and out
out <- as.matrix(jags.out)         ## convert from coda to matrix  
x.cols <- grep("^t",colnames(out)) ## grab all columns that start with the letter x
ci <- apply((out[,x.cols]),2,quantile,c(0.025,0.5,0.975)) 

ModelMedian<- ci[2,]

plot(time,ci[2,],type='n',ylim=range(y,na.rm=TRUE),ylab="NEE",xlim=time[time.rng])
## adjust x-axis label to be monthly if zoomed
if(diff(time.rng) < 100){ 
  axis.Date(1, at=seq(time[time.rng[1]],time[time.rng[2]],by='month'), format = "%Y-%m")
}
ecoforecastR::ciEnvelope(time,ci[1,],ci[3,],col=ecoforecastR::col.alpha("lightBlue",0.75))
points(time,y,pch="+",cex=0.5)
points(time,ci[2,],pch="+",cex=0.5,col="red")
