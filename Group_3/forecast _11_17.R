##Group 3
##Ensemble Forecast

##############SET-UP STEPS

##Load in libraries
library(neonUtilities) 
library(lubridate)
library(ggplot2)
library(rjags)
library(daymetr)
library(neon4cast)
library(tidyverse)
library(rnoaa)

##Set forecast date
forecast_date <- Sys.Date()
noaa_date <- Sys.Date() - days(1)  #Need to use yesterday's NOAA forecast because today's is not available yet

##Set team info
team_name <- "chlorofun"
team_list <- list(list(individualName = list(givenName = c("Kayla","Nick","Stacy","Zhuoran") ,
                                             surName = c("A","A","M","Y"),
                       organizationName = "University of Notre Dame",
                       electronicMailAddress = "smowry@nd.edu")))


##Download site-data
site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |> 
  filter(aquatics == 1, field_site_id == "BARC")


##############TARGET DATA

##Download target data 
download_targets <- function(){ readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz", guess_max = 1e6) }
target<-download_targets()

target$year <- year(target$datetime)

#seperate out chla, oxygen, and temperature, we used years after 2020 
#because when we tried to download NOAA data from the past, the data was not available until Sept, 2020

oxygen <- subset(target,site_id=="BARC"& variable=="oxygen"&year>"2020")
temperature <- subset(target,site_id=="BARC"& variable=="temperature"&year>"2020")
chla <- subset(target,site_id=="BARC"& variable=="chla"&year>"2020")
class(target$datetime)

##look at plots of each
oxygen.plot <- ggplot(oxygen, aes(x= datetime, y = observation)) + 
  geom_point()+ 
  theme_bw()+ theme(text = element_text(size=10)) + theme(legend.position = "none")+
  ylab("oxgen")+xlab("Year")+ labs(subtitle = "Oxygen")
oxygen.plot

temperature.plot <- ggplot(temperature, aes(x= datetime, y = observation)) + 
  geom_point()+ 
  theme_bw()+ theme(text = element_text(size=10)) + theme(legend.position = "none")+
  ylab("temperature")+xlab("Year")+ labs(subtitle = "Temperature")
temperature.plot

chla.plot <- ggplot(chla, aes(x= datetime, y = observation)) + 
  geom_point()+ 
  theme_bw()+ theme(text = element_text(size=10)) + theme(legend.position = "none")+
  ylab("chla")+xlab("Year")+ labs(subtitle = "chlorophyll a")
chla.plot

##merge temperature and chla by data
data<-data.frame(merge(chla,temperature,"datetime"))
colnames(data)<-c("date","site","target","chla","year","site","predictor","temperature","year")
dat <- data[!duplicated(as.list(data))]


##############RANDOM WALK

##Add 35 NAs to end of dataset for predictions
time <- as.Date(dat$date)
new_dates<-c(forecast_date,forecast_date + 1,forecast_date + 2,forecast_date + 3,forecast_date + 4,
         forecast_date + 5,forecast_date + 6,forecast_date + 7,forecast_date + 8,forecast_date + 9,
         forecast_date + 10,forecast_date + 11,forecast_date + 12,forecast_date + 13,forecast_date + 14,
         forecast_date + 15,forecast_date + 16,forecast_date + 17,forecast_date + 18,forecast_date + 19,
         forecast_date + 20,forecast_date + 21,forecast_date + 22,forecast_date + 23, forecast_date + 24,
         forecast_date + 25,forecast_date + 26,forecast_date + 27,forecast_date + 28,forecast_date + 29,
         forecast_date + 30,forecast_date + 31,forecast_date + 32,forecast_date + 33,forecast_date + 34)

time <- append(time, new_dates)
y <- as.vector(dat$chla)
y <- append(y, rep(NA,35))

#Random Walk JAGS
RandomWalk = "
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
  tau_obs ~ dgamma(a_obs,r_obs)
  tau_add ~ dgamma(a_add,r_add)
}
"

#Define  data
data <- list(y=log(y),n=length(y),      ## data
             x_ic=log(1000),tau_ic=100, ## initial condition prior
             a_obs=1,r_obs=1,           ## obs error prior
             a_add=1,r_add=1  )

#Define MCMC settings
init <- list()
nchain=3
for(i in 1:nchain){
  y.samp = sample(y,length(y),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(log(y.samp))),  
                    tau_obs=5/var(log(y.samp)))}        

##Define JAGS models
j.model   <- jags.model (file = textConnection(RandomWalk),
                         data = data,
                         inits = init,
                         n.chains = 3)

##Sample JAGS model 
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("x", "tau_add","tau_obs"),
                            n.iter = 1000)
##Look at trace plots
#plot(jags.out) #too big

##Plot forecast
time.rng = c(1,length(time))       
out_chla <- as.matrix(jags.out)         
x.cols <- as.data.frame(out_chla[,1:length(y)]) ## grab all columns that contain data for a time point
ci_chla <- apply(x.cols,2,quantile,c(0.025,0.5,0.975)) ## model was NOT fit on log scale

plot(time,ci_chla[2,],type='l',ylim=c(0,123),ylab="Chla-A",xlim=time[time.rng],main= "Random Walk Model")

ecoforecastR::ciEnvelope(time,ci_chla[1,],ci_chla[3,],col=ecoforecastR::col.alpha("lightBlue",0.75))
points(time,y,pch="+",cex=0.5)

##Download past air-temperature data 
df_past <- neon4cast::noaa_stage3()

noaa_past <- df_past |> 
  dplyr::filter(site_id =="BARC",
                variable == "air_temperature") |> 
  dplyr::rename(ensemble = parameter) |> 
  dplyr::collect()

head(noaa_past)

##Download future air temperature data 
df_future <- neon4cast::noaa_stage2()

## Subset variable/site of interest
noaa_future <- df_future |>
  dplyr::filter(site_id == "BARC",
                reference_datetime == as.Date(noaa_date),
                datetime >= lubridate::as_datetime(forecast_date),
                variable == "air_temperature") |>
  dplyr::rename(ensemble = parameter) %>%
  dplyr::select(datetime, prediction, ensemble) |>
  dplyr::collect()


# Aggregate  noaa_future at each site (to the day) and convert units of drivers

noaa_past_temp<- noaa_past %>% 
  mutate(date = as_date(datetime)) %>% 
  group_by(date) %>% 
  summarize(air_temp = mean(prediction- 273.15, na.rm = TRUE), .groups = "drop") %>% 
  rename(datetime = date)

noaa_future_temp<- noaa_future %>% 
  mutate(date = as_date(datetime)) %>% 
  group_by(date) %>% 
  summarize(air_temp = mean(prediction- 273.15, na.rm = TRUE), .groups = "drop") %>% 
  rename(datetime = date)

noaa_past_temp$year <- year(noaa_past_temp$datetime)

#Only get tempature after 2020 since historical date only started in late 2020
noaa_past_temp <- subset(noaa_past_temp,year>"2020")


#temperature from Jan.1 2020 to 35 days in the future
Temp<-c(noaa_past_temp$air_temp,noaa_future_temp$air_temp)


##JAGS MODEL 
Temp_Covar = "
model{
  #### Data Model
  for(t in 1:n)
  {
    y[t] ~ dnorm(x[t],tau_obs)
  }
  #### Process Model
  for(t in 2:n)
  {
    mu[t] <- x[t-1]  + betaX*x[t-1] + betaTemp*Temp[t]
    x[t]~dnorm(mu[t],tau_add)
  }
  #### Priors
  x[1] ~ dnorm(x_ic,tau_ic)
  tau_obs ~ dgamma(a_obs,r_obs)
  tau_add ~ dgamma(a_add,r_add)
  betaX ~ dnorm(0,0.1)
  betaTemp ~ dnorm(0,0.1)
}
"

#Define  data
data_2 <- list(y=log(y),n=length(y),Temp=Temp,      ## data
             x_ic=log(1000),tau_ic=100, ## initial condition prior
             a_obs=1,r_obs=1,           ## obs error prior
             a_add=1,r_add=1  )

#Set initial conditions
init <- list()
for(i in 1:3){
  y.samp = sample(y,length(y),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(log(y.samp))),  
                    tau_obs=5/var(log(y.samp)))}        

##Define JAGS models
j.model_2   <- jags.model (file = textConnection(Temp_Covar),
                         data = data_2,
                         inits = init,
                         n.chains = 3)

##Sample JAGS model 
jags.out_2   <- coda.samples (model = j.model_2,
                            variable.names = c("x", "tau_add","tau_obs"),
                            n.iter = 1000)
##Look at trace plots
#plot(jags.out_2) #too big

##Plot forecast
time.rng = c(1,length(time))       
out_chla <- as.matrix(jags.out_2)         
x.cols <- as.data.frame(out_chla[,1:length(y)]) ## grab all columns that contain data for a time point
ci_chla <- apply(x.cols,2,quantile,c(0.025,0.5,0.975)) ## model was NOT fit on log scale

plot(time,ci_chla[2,],type='l',ylim=c(0,123),ylab="Chla-A",xlim=time[time.rng],main= "Model with Temperature")

ecoforecastR::ciEnvelope(time,ci_chla[1,],ci_chla[3,],col=ecoforecastR::col.alpha("lightBlue",0.75))
points(time,y,pch="+",cex=0.5)

##############FORMAT FORCAST FOR SUBMISSION

## Create a data_frame of chl-a forecast 
    
  chla<- tibble(time = time,
                     site_id = "BARC",
                    # ensemble = noaa_future_site$ensemble,  ##have to take this line out bc code doesn't work
                     predicted = ci_chla[2,],
                     variable = "chla")
    
  

#Forecast output file name in standards requires for Challenge.  
forecast_file <- paste0("aquatics","-",min(time),"-",team_name,".csv.gz")

#Write csv to disk
write_csv(chla, forecast_file)

#Confirm that output file meets standard for Challenge
neon4cast::forecast_output_validator(forecast_file)

###############################NEED TO COMPLETE SECTION BELOW 

# Generate metadata for forecast. 

model_metadata = list(
  forecast = list(
    model_description = list(
      forecast_model_id =  "chlorofun",  #What goes here
      name = "Chla-A at Barc w/ water temp", 
      type = "empirical",  
      repository = "https://github.com/rqthomas/neon4cast-example"  ##add in repo
    ),
    initial_conditions = list(
      status = "absent"
    ),
    drivers = list(
      status = "propagates",
      complexity = 1, #Just temperature
      propagation = list( 
        type = "ensemble", 
        size = 1000) 
    ),
    parameters = list(
      status = "absent"
    ),
    random_effects = list(
      status = "absent"
    ),
    process_error = list(
      status = "absent"
    ),
    obs_error = list(
      status = "absent"
    )
  )
)

##generate_metadata is a specific function in the neon4cast package which takes the list of metadata, the forecast the metadata are associated with, and the team name as inputs. This function will creat a standardized  metadata file.

metadata_file <- neon4cast::generate_metadata(forecast_file, team_list, model_metadata)


##Submit forecast 
  

neon4cast::submit(forecast_file = forecast_file, metadata = metadata_file, ask = FALSE)

## Partition uncertainty 

