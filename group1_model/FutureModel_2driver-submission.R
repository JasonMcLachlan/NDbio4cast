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

###FILTER WEATHER DATA FOR SITES AND DRIVERS

#Download past weather estimates from the NOAA using the neon4cast package
df_past <- neon4cast::noaa_stage3()

#filter for past weather estimates for our site from NOAA
past_drivers <- df_past |> 
  dplyr::filter(site_id == "UNDE",
                variable %in%  c("surface_downwelling_shortwave_flux_in_air", "air_temperature")
  ) |> 
  dplyr::collect()

#Filter + aggregate for daily averages
past_drivers <- past_drivers %>% 
  mutate(date = as_date(datetime)) %>% 
  group_by(date,variable) %>% 
  summarize(driver_value = mean(prediction, na.rm = TRUE), .groups = "drop") %>% 
  rename(datetime = date)

rm(df_past)

#Download future weather forecasts from the NOAA
df_future <- neon4cast::noaa_stage2(cycle = 0)

#filter for future weather estimates for our site from NOAA
future_drivers <- df_future |> 
  dplyr::filter(site_id == "UNDE",
                reference_datetime == as.Date(noaa_date),
                datetime >= lubridate::as_datetime(forecast_date),
                variable %in%  c("surface_downwelling_shortwave_flux_in_air", "air_temperature") 
  ) |>
  dplyr::rename(ensemble = parameter) %>%
  dplyr::select(datetime, prediction, ensemble, variable) |>
  dplyr::collect()

#Filter + aggregate for daily averages
future_drivers <- future_drivers %>% 
  mutate(date = as_date(datetime)) %>% 
  group_by(date,variable) %>% 
  summarize(driver_value = mean(prediction, na.rm = TRUE), .groups = "drop") %>% 
  rename(datetime = date)

rm(df_future)
gc()

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
noaa_past_sw <- past_drivers |> 
  dplyr::filter( variable == "surface_downwelling_shortwave_flux_in_air") |> 
  dplyr::select(datetime, driver_value) |>
  dplyr::collect()

#Calculate mean sw over all past sw data
swBar = mean(noaa_past_sw$driver_value,na.rm=TRUE)

#Convert downwelling sw to anomalies
noaa_past_sw$driver_value = noaa_past_sw$driver_value - swBar
noaa_past_sw = rename(noaa_past_sw, downwelling_sw = driver_value)

#Merge in past sw data into the targets file, matching by date.
site_target_past <- Site |>
  select(datetime, site_id, variable, observation) |>
  dplyr::filter(variable %in% c("nee")) |>
  pivot_wider(names_from= "variable", values_from = "observation")
site_target_past<-left_join(site_target_past, noaa_past_sw, by = c("datetime"))

###FUTURE SUNLIGHT
noaa_future_sw <- future_drivers |>
  dplyr::filter(variable == "surface_downwelling_shortwave_flux_in_air") |>
  dplyr::select(datetime, driver_value) |>
  dplyr::collect()

#Convert future downwelling sw to anomalies
noaa_future_sw$driver_value = noaa_future_sw$driver_value - swBar
noaa_future_sw = rename(noaa_future_sw, downwelling_sw = driver_value)

###PAST TEMPERATURE
#past weather estimates of air temperature from NOAA
noaa_past_temp <-past_drivers |> 
  dplyr::filter( variable == "air_temperature") |> 
  dplyr::select(datetime, driver_value) |>
  dplyr::collect()

#Calculate mean temp over all past temp data
tempBar = mean(noaa_past_temp$driver_value,na.rm=TRUE)

#Convert temp to anomalies
noaa_past_temp$driver_value = noaa_past_temp$driver_value - tempBar
noaa_past_temp = rename(noaa_past_temp, air_temp = driver_value)

#Merge in past temp data into the targets file, matching by date.
site_target_past <- site_target_past |>
  left_join(noaa_past_temp, by = c("datetime"))

###FUTURE TEMPERATURE
noaa_future_temp <- future_drivers |>
  dplyr::filter(variable == "air_temperature") |>
  dplyr::select(datetime, driver_value) |>
  dplyr::collect()

#Convert future temp to anomalies
noaa_future_temp$driver_value = noaa_future_temp$driver_value - tempBar
noaa_future_temp = rename(noaa_future_temp, air_temp = driver_value)

###PHENOLOGY
#Ideally we would use LAI but since we do not have future LAI predictions we are using phenology gcc climatology predictions
phenfile <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/phenology/phenology-targets.csv.gz", guess_max = 1e6) |> 
  na.omit()
phenSite <- phenfile %>% filter(site_id == "UNDE", variable == "gcc_90")

##Phenology climatology model for future predictions of phenology
#Add column to phenology data with Julian day
phenSite$julianday <- yday(phenSite$datetime)
##Average phenology observations by julian day
phenDailyAv <- phenSite %>% group_by(julianday) %>% summarise(PhenAv = mean(observation, na.rm=TRUE))


###PREPPING FUTURE DATA
#adding all past nee/noaa data and future projections to one df with NAs if data is missing
past_length<-length(noaa_past_sw$datetime) # number of days we have drivers for (start day to yesterday, we have a wwek more data for drivers than NEE)
past_length7<-past_length-6 #number of days we also have NEE for (NEE availability has about one week lag)
future_data <- data.frame() #empty df to hold everything
future_data[1:7,1]<-noaa_past_sw[past_length7:past_length,1] #add datetime column dates from the last week
future_data[,2:3]<-NA #columns  2 and 3 empty, to match the other df we merge with
future_data[1:7,4]<-noaa_past_sw[past_length7:past_length,2] #add the last week of sw data to column 4
future_data[1:7,5]<-noaa_past_temp[past_length7:past_length,2] #add the last week of temp data to column 5
future_data[8:42,1]<-noaa_future_sw[1:35,1] #add 35 days of datetime dates into the future to col 1
future_data[8:42,4]<-noaa_future_sw[1:35,2] #add 35 days of sw predictions into the future to col 4
future_data[8:42,5]<-noaa_future_temp[1:35,2] #add 35 days of temp predictions into the future to col 5
future_data <- future_data %>% mutate_all(~ifelse(is.nan(.), NA, .)) #NA if data is missing
colnames(future_data)<-c("datetime", "site_id", "nee", "downwelling_sw", "air_temp") #rename columns to be able to merge with past data
future_data[,1]<- as.Date(future_data[,1], origin = "1970-01-01")

target_data<- rbind(site_target_past,future_data) #merge past nee/noaa data and future data

#add NA values to all data gaps
#make a complete sequence of dates with missing dates from our first day of past NEE to our last prediction day in the future
dateseqstart<- target_data$datetime[1]
dateseqend<-target_data$datetime[length(target_data$datetime)]
dateseq<-seq(from=dateseqstart, to=dateseqend, by="day")

#join our full dataset to the complete sequence of dates, with NAs for dates when we are missing data
site_target<- as.data.frame(dateseq)
site_target<-rename(site_target, datetime = dateseq)
site_target<-left_join(site_target, target_data, by = c("datetime"))

#add phenology driver
site_target$julianday <- yday(site_target$datetime) #add column of julian day to full dataset of nee + other drivers
site_target <- left_join(site_target, phenDailyAv, by = c("julianday" = "julianday")) #add climatology derived phenology to every date

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
data$phen = site_target$PhenAv[match(time,site_target$datetime)]

#add drivers to DLM
ef.out <- ecoforecastR::fit_dlm(model=list(obs="y",fixed="~ 1 + X + phen + sw"),data)#sw and phen as drivers, not using temp for now


#Plot DLM forecast
## confidence interval
time.rng = c(1,length(time))
out <- as.matrix(ef.out$predict)
ci <- apply((out),2,quantile,c(0.025,0.5,0.975))
plot(time,ci[2,],type='n',ylim=range(y,na.rm=TRUE),ylab="NEE",xlim=time[time.rng])
## adjust x-axis label to be monthly if zoomed
if(diff(time.rng) < 100){ 
  axis.Date(1, at=seq(time[time.rng[1]],time[time.rng[2]],by='month'), format = "%Y-%m")
}
ecoforecastR::ciEnvelope(time,ci[1,],ci[3,],col=ecoforecastR::col.alpha("lightBlue",0.75))
points(time,y,pch="+",cex=0.5)
points(time,ci[2,],pch="+",cex=0.5,col="red")




##SUBMIT FORECAST
model_id <- "ND_PioNEErs" #our model name

#subset last 35 days to submit
submission_end<- length(out[1,])
submission_start<-submission_end-34 #today
final_out<-out[,submission_start:submission_end]

#standard deviations
sigma<-as.data.frame(apply(final_out,2,sd))
#means
mu<-as.data.frame(apply(final_out,2,mean))
#dates
final_dates<- as.data.frame(site_target[submission_start:submission_end,1])

#Format using EFI standards
#column names
columns<-c("datetime","variable","parameter","family","model_id","reference_datetime","site_id","predicted")
forecast<-data.frame(matrix(nrow = 70,ncol = length(columns)))
colnames(forecast)<-columns
#add dates x2 (one set of dates for mu, one for sigma)
forecast[1:35,1]<-final_dates
forecast[36:70,1]<-final_dates
forecast$datetime<-as.Date(forecast$datetime, origin = "1970-01-01")
#add variable
forecast$variable<- "nee"
#add parameters x2 (mu with first set of dates, sigma with second)
forecast[1:35,3]<-"mu"
forecast[36:70,3]<-"sigma"
#add family
forecast$family<-"normal"
#add model id
forecast$model_id<-model_id
#add reference datetime
forecast$reference_datetime<-as.Date(forecast_date)
#add site id
forecast$site_id<-"UNDE"
#add mu and sigma predictions
forecast[1:35,8]<-mu
forecast[36:70,8]<-sigma
forecast<-as.tibble(forecast)

#EFI file name standards
# csv.gz means that it will be compressed
file_date <- Sys.Date() #forecast$reference_datetime[1]
forecast_file <- paste0("terrestrial_daily","-",file_date,"-",model_id,".csv.gz")

#Write csv to disk
write_csv(forecast, forecast_file)

# Generate metadata
team_name <- "NDPioNEErs"
team_list <- list(list(individualName = list(givenName = c("Rachel","Eva","Nate","Hayden") ,
                                             surName = c("Badzioch","Deegan","Kroeze","Gallo"),
                                             organizationName = "University of Notre Dame",
                                             electronicMailAddress = "hgallo@nd.edu")))

model_metadata = list(
  forecast = list(
    model_description = list(
      forecast_model_id =  "ND_PioNEErs", 
      name = "NEE at UNDERC with SW and Phenology", 
      type = "empirical",  
      repository = "https://github.com/edeegan2/NDbio4cast/tree/main/group1_model"   ## put your REPO here *******************
    ),
    initial_conditions = list(
      status = "present"
    ),
    drivers = list(
      status = "present",
      complexity = 2
    ),
    parameters = list(
      status = "data_driven"
    ),
    random_effects = list(
      status = "absent"
    ),
    process_error = list(
      status = "present"
    ),
    obs_error = list(
      status = "present"
    )
  )
)

#metadata_file <- neon4cast::generate_metadata(forecast_file, team_list, model_metadata)
##some group members having trouble with metadata, some not, we also submitted metadata using the online submission form so metadata should still be linked to our model id

#Submit forecast!!!!!
neon4cast::submit(forecast_file = forecast_file, metadata = NULL, ask = FALSE)

#check submission
neon4cast::check_submission(forecast_file)


