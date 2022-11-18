#fair warning that this takes super long to run when filtering noaa data
#need to update filtering lines to be more efficient
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
site_metadata <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") 
site_metadata %>% filter(as.integer(terrestrial) == 1)

#Filter for one sites NEE
Site <- target %>% filter(site_id == "UNDE")

forecast_date <- Sys.Date()#assign the nee forecast date as today (the date on the computer)
noaa_date <- Sys.Date() - lubridate::days(1)#assign weather forecast date as yesterday (todays is not available yet)  

#Download past weather estimates from the NOAA using the neon4cast package
df_past <- neon4cast::noaa_stage3()

#Download future weather forecasts from the NOAA
df_future <- neon4cast::noaa_stage2(cycle = 0)

#Ideally do all driver filtering for df_past and df_future here 
#and keep the filtered versions but remove the original df_past 
#and df_future because they are too big

sites <- unique(target$site_id) #unique site ids

##Julian Day stuff to possibly add day of year (DOY) as a stand-in for LAI if we can't get LAI to work
##unused so far
##Add column to target with Julian day
#Site$julianday <- yday(Site$datetime)
##Average NEE observations by julian day
#DOYaverage <- Site %>% group_by(julianday) %>% summarise(NEEav = mean(observation))
#Sitedata <- left_join(Site, DOYaverage, by = c("julianday" = "julianday"))

#Plot NEE over time
#plot(Site$datetime,Site$observation, type="p", pch=1, cex=0.5, main="NEE", xlab="Date", ylab="NEE (gC/m2/day)")
#plot NEE vs DAY of Year
#plot(DOYaverage$julianday,DOYaverage$NEEav, type="p", pch=1, cex=0.5, main="NEE", xlab="Day of year", ylab="NEE (gC/m2/day)")


##DRIVERS

###PAST SUNLIGHT
#past weather estimates of shortwave flux (solar radiation) from NOAA
noaa_past_sw <- df_past |> 
  dplyr::filter(site_id %in% sites,
                variable == "surface_downwelling_shortwave_flux_in_air", 
  ) |> 
  dplyr::collect()

#Filter for one site
noaa_past_sw_Site <- noaa_past_sw |> 
  dplyr::filter(site_id == "UNDE" ) |> 
  dplyr::collect()

#Filter + aggregate for daily averages
noaa_past_sw_mean <- noaa_past_sw_Site %>% 
  mutate(date = as_date(datetime)) %>% 
  group_by(date) %>% 
  summarize(downwelling_sw = mean(prediction, na.rm = TRUE), .groups = "drop") %>% 
  rename(datetime = date)

#Calculate mean sw over all past sw data
swBar = mean(noaa_past_sw_mean$downwelling_sw,na.rm=TRUE)

#Convert downwelling sw to anomalies
noaa_past_sw_mean$downwelling_sw = noaa_past_sw_mean$downwelling_sw - swBar

#Merge in past sw data into the targets file, matching by date.
site_target_past <- Site |>
  select(datetime, site_id, variable, observation) |>
  dplyr::filter(variable %in% c("nee")) |>
  pivot_wider(names_from = "variable", values_from = "observation") |>
  left_join(noaa_past_sw_mean, by = c("datetime"))

###FUTURE SUNLIGHT
noaa_future_sw <- df_future |>
  dplyr::filter(site_id == "UNDE",
                reference_datetime == as.Date(noaa_date),
                datetime >= lubridate::as_datetime(forecast_date),
                variable == "surface_downwelling_shortwave_flux_in_air") |>
  dplyr::rename(ensemble = parameter) %>%
  dplyr::select(datetime, prediction, ensemble) |>
  dplyr::collect()

#Filter + aggregate for daily averages
noaa_future_sw_mean <- noaa_future_sw %>% 
  mutate(date = as_date(datetime)) %>% 
  group_by(date) %>% 
  summarize(downwelling_sw = mean(prediction, na.rm = TRUE), .groups = "drop") %>% 
  rename(datetime = date)

#Convert future downwelling sw to anomalies
noaa_future_sw_mean$downwelling_sw = noaa_future_sw_mean$downwelling_sw - swBar


###PAST TEMPERATURE
#past weather estimates of air temperature from NOAA
noaa_past_temp <- df_past |> 
  dplyr::filter(site_id %in% sites,
                variable == "air_temperature", 
  ) |> 
  dplyr::collect()

#Filter for one site
noaa_past_temp_Site <- noaa_past_temp |> 
  dplyr::filter(site_id == "UNDE" ) |> 
  dplyr::collect()

#Filter + aggregate for daily averages
noaa_past_temp_mean <- noaa_past_temp_Site %>% 
  mutate(date = as_date(datetime)) %>% 
  group_by(date) %>% 
  summarize(air_temp = mean(prediction, na.rm = TRUE), .groups = "drop") %>% 
  rename(datetime = date)

#Calculate mean temp over all past sw data
tempBar = mean(noaa_past_temp_mean$air_temp,na.rm=TRUE)

#Convert temp to anomalies
noaa_past_temp_mean$air_temp = noaa_past_temp_mean$air_temp - tempBar

#Merge in past temp data into the targets file, matching by date.
site_target_past <- site_target_past |>
  left_join(noaa_past_temp_mean, by = c("datetime"))

###FUTURE TEMPERATURE
noaa_future_temp <- df_future |>
  dplyr::filter(site_id == "UNDE",
                reference_datetime == as.Date(noaa_date),
                datetime >= lubridate::as_datetime(forecast_date),
                variable == "air_temperature") |>
  dplyr::rename(ensemble = parameter) %>%
  dplyr::select(datetime, prediction, ensemble) |>
  dplyr::collect()

#Filter + aggregate for daily averages
noaa_future_temp_mean <- noaa_future_temp %>% 
  mutate(date = as_date(datetime)) %>% 
  group_by(date) %>% 
  summarize(air_temp = mean(prediction, na.rm = TRUE), .groups = "drop") %>% 
  rename(datetime = date)

#Convert future temp to anomalies
noaa_future_temp_mean$air_temp = noaa_future_temp_mean$air_temp - tempBar



###PREPPING FUTURE DATA
#confusing code that needs to be commented:)
#adding all future projections to one df
past_length<-length(noaa_past_sw_mean$datetime)
past_length7<-past_length-6
future_data <- data.frame()
future_data[1:7,1]<-noaa_past_sw_mean[past_length7:past_length,1]
future_data[,2:3]<-NA
future_data[1:7,4]<-noaa_past_sw_mean[past_length7:past_length,2]
future_data[1:7,5]<-noaa_past_temp_mean[past_length7:past_length,2]
future_data[8:42,1]<-noaa_future_sw_mean[1:35,1]
future_data[8:42,4]<-noaa_future_sw_mean[1:35,2]
future_data[8:42,5]<-noaa_future_temp_mean[1:35,2]
future_data <- future_data %>% mutate_all(~ifelse(is.nan(.), NA, .))
colnames(future_data)<-c("datetime", "site_id", "nee", "downwelling_sw", "air_temp")
future_data[,1]<- as.Date(future_data[,1], origin = "1970-01-01")

site_target<- rbind(site_target_past,future_data)


###MODEL

y <- site_target$nee
time <- as.Date(site_target$datetime)

#JAGS code for model
NEEmodel = "
model{
  
  #### Data Model
  for(t in 1:n){
    y[t] ~ dnorm(x[t],tau_obs)
  }
  
  #### Process Model
  for(t in 2:n){
    x[t]~dnorm(x[t-1],tau_add)
  }
  
  #### Priors
  x[1] ~ dnorm(x_ic,tau_ic)
  tau_obs ~ dnorm(a_obs,r_obs)
  tau_add ~ dnorm(a_add,r_add)
}
"

data <- list(y=(y),n=length(y),      ## data
             x_ic=(0),tau_ic=1, ## initial condition prior
             a_obs=0,r_obs=1,           ## obs error prior
             a_add=0,r_add=1            ## process error prior
)

#define initial state of model parameters for MCMC chains
nchain = 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(y,length(y),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff((y.samp)), na.rm = TRUE),  ## initial guess on process precision
                    tau_obs=5/var((y.samp), na.rm = TRUE))       ## initial guess on obs precision
}

#run model
j.model   <- jags.model (file = textConnection(NEEmodel),
                         data = data,
                         inits = init,
                         n.chains = 3)

#take sample from MCMC output
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("x","tau_add","tau_obs"),
                            n.iter = 10000)


##DYNAMIC LINEAR MODEL

#add drivers to data
data$sw = site_target$downwelling_sw[match(time,site_target$datetime)]
data$temp = site_target$air_temp[match(time,site_target$datetime)]

#add drivers to DLM
ef.out <- ecoforecastR::fit_dlm(model=list(obs="y",fixed="~ 1 + X + sw + temp"),data)


#Plot DLM forecast
## confidence interval
time.rng = c(1,length(time))
out <- as.matrix(ef.out$predict)
ci2 <- apply((out),2,quantile,c(0.025,0.5,0.975))
plot(time,ci2[2,],type='n',ylim=range(y,na.rm=TRUE),ylab="NEE",xlim=time[time.rng])
## adjust x-axis label to be monthly if zoomed
if(diff(time.rng) < 100){ 
  axis.Date(1, at=seq(time[time.rng[1]],time[time.rng[2]],by='month'), format = "%Y-%m")
}
ecoforecastR::ciEnvelope(time,ci2[1,],ci2[3,],col=ecoforecastR::col.alpha("lightBlue",0.75))
points(time,y,pch="+",cex=0.5)
points(time,ci2[2,],pch="+",cex=0.5,col="red")

