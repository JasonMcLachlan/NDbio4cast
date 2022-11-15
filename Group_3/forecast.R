
##Group 3
##Ensemble Forecast

##Load in libraries
library(neonUtilities) 
library(lubridate)
library(ggplot2)
library(rjags)
library(daymetr)
library(neon4cast)
library(tidyverse)


##Set forecast date
forecast_date <- Sys.Date()
noaa_date <- Sys.Date() - days(1)  #Need to use yesterday's NOAA forecast because today's is not available yet
team_name <- "chlorofun"

## Team list is a list of lists, which requires a specific, standard format and information including first and last name, organization, and email address. 

team_list <- list(list(individualName = list(givenName = c("Kayla","Nick","Stacy","Zhuoran") ,
                                             surName = c("A","A","M","Y"),
                       organizationName = "University of Notre Dame",
                       electronicMailAddress = "smowry@nd.edu")))

##Download target data 
##Load Target Data 
download_targets <- function(){ readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz", guess_max = 1e6) }
target<-download_targets()
target$year <- year(target$datetime)

#seperate out chla, oxygen, and temperature
oxygen <- subset(target,site_id=="BARC"& variable=="oxygen"&year>"2019")
temperature <- subset(target,site_id=="BARC"& variable=="temperature"&year>"2019")
chla <- subset(target,site_id=="BARC"& variable=="chla"&year>"2019")
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


##Download site-data
site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |> 
  filter(aquatics == 1)


########################################### DO NOT RUN! --  WILL CRASH R


##This part of the code uses the noaa_stage3() and noaa_stage2() functions in the neon4cast packages which gets NOAA global ensemble forecasts
##Download future data 
df_future <- neon4cast::noaa_stage2()

## Subset variable/site of interest
noaa_future <- df_future |> 
  dplyr::filter(cycle == 0,
                start_date == as.character(noaa_date),
                time >= lubridate::as_datetime(forecast_date), 
                variable == "air_temperature") |> 
  dplyr::collect()


# Aggregate  noaa_future at each site (to the day) and convert units of drivers

noaa_future <- noaa_future %>% 
  mutate(time = as_date(time)) %>% 
  group_by(time, site_id, ensemble) |> 
  summarize(air_temperature = mean(predicted), .groups = "drop") |> 
  mutate(air_temperature = air_temperature - 273.15) |> 
  select(time, site_id, air_temperature, ensemble)


###################################################### CONTINUE RUNNING HERE


y = dat$chla

##Fit historic data
##create a data list
data <- list(y=log(y),n=length(y),      ## data
             x_ic=log(1000),tau_ic=100, ## initial condition prior
             a_obs=1,r_obs=1,           ## obs error prior
             a_add=1,r_add=1  )          ## process error prior


time.rng = c(1,length(dat$date))  
time = as.Date(dat$date)

plot(time,y,type='l',ylab="Chla",lwd=2,log='y')
Temp = dat$temperature
ef.out <- ecoforecastR::fit_dlm(model=list(obs="y",fixed="~ 1 + X + Temp"),data)
names(ef.out)

## confidence interval
out <- as.matrix(ef.out$predict)
ci <- apply(exp(out),2,quantile,c(0.025,0.5,0.975))
plot(time,ci[2,],type='n',ylim=range(y,na.rm=TRUE),ylab="Chla",log='y',xlim=dat$date[time.rng],main="historical data and water temperature")
## adjust x-axis label to be monthly if zoomed
if(diff(time.rng) < 100){ 
  axis.Date(1, at=seq(dat$date[time.rng[1]],dat$date[time.rng[2]],by='month'), format = "%Y-%m")
}
ecoforecastR::ciEnvelope(time,ci[1,],ci[3,],col=ecoforecastR::col.alpha("lightBlue",0.75))
points(dat$date,y,pch="+",cex=0.5)


############################################# DO NOT RUN -- WILL CRASH R

##download past air-temperature data to predict water temperature
df_past <- neon4cast::noaa_stage3()


noaa_past <- df_past |> 
  dplyr::filter(site_id == "BARC",
                variable == "air_temperature") |> 
  ## dplr::collect saves data in a local simple dataframe (tibble)
  dplyr::collect()

############################################### CONTINUE RUNNING HERE

##Generate a forecast for 35 time-steps

##Create a matrix to hold forecasts
n= 35
N<-matrix(nrow=5000, ncol=n)


##Make up Temp data and initial conditions bc we cannot download
Temp<-rnorm(35,5,2)
IC<-5.2


##Forecast 5000 ensembles of 35 time steps 
    Nprev <- matrix(IC, nrow=5000,ncol=35)
    
    for (t in 1:35)
    {
      for (i in 1:5000)
      {
        
      N[i,t]<-(Nprev[i,t] + as.numeric(ef.out$params[i,"betaX"][1])*Nprev[i,t] 
                      + as.numeric(ef.out$params[i,"betaIntercept"][1]) + as.numeric(ef.out$params[i,"betaTemp"][1])*Temp[t])
    
      }
        Nprev[i,t] <- N[i,t] 
    }
    

##Save median prediction
predicted<-vector(length=n)
for (t in 1:n)
{
 predicted[t]<-   median(N[,t])
}


dates<-c(forecast_date,forecast_date + 1,forecast_date + 2,forecast_date + 3,forecast_date + 4,
         forecast_date + 5,forecast_date + 6,forecast_date + 7,forecast_date + 8,forecast_date + 9,
         forecast_date + 10,forecast_date + 11,forecast_date + 12,forecast_date + 13,forecast_date + 14,
         forecast_date + 15,forecast_date + 16,forecast_date + 17,forecast_date + 18,forecast_date + 19,
         forecast_date + 20,forecast_date + 21,forecast_date + 22,forecast_date + 23, forecast_date + 24,
         forecast_date + 25,forecast_date + 26,forecast_date + 27,forecast_date + 28,forecast_date + 29,
         forecast_date + 30,forecast_date + 31,forecast_date + 32,forecast_date + 33,forecast_date + 34)

## tibble creates a new data frame. We are creating data frame based on our temperature forecast
    
  chla<- tibble(time = dates,
                     site_id = "BARC",
                    # ensemble = noaa_future_site$ensemble,  ##have to take this line out bc code doesn't work
                     predicted = predicted,
                     variable = "chla")
    
  
#Visualize forecast.  Is it reasonable?
chla %>% 
  ggplot(aes(x = time, y = predicted)) +
  geom_line() +
  facet_grid(variable~site_id, scale ="free")



############################### DID NOT COMPLETE REST BC NEED TO FIX ERRORS

#Forecast output file name in standards requires for Challenge.  
# csv.gz means that it will be compressed
forecast_file <- paste0("aquatics","-",min(forecast$time),"-",team_name,".csv.gz")

#Write csv to disk
write_csv(forecast, forecast_file)

#Confirm that output file meets standard for Challenge
#neon4cast::forecast_output_validator(forecast_file)
```

# Step 9: Generate metadata for forecast. This part of the code illustrates an important concept we've discussed:including complete meta-data, including meta-data on uncertainty. Including meta-data allows others to understand your forecast, uncertainty in your forecast, and how your forecast was generated. Specifially including metadata in a standarized format (as below) facilitates understanding of metadata.

```{r}

##our metadata is in the form of nested lists. Meta data includes the forecast id, name, type, repository, inital conditions, information on the drivers of the forecast, and information on how uncertainty is propogated. 

model_metadata = list(
  forecast = list(
    model_description = list(
      forecast_model_id =  "air2waterSat",  #What goes here
      name = "Air temperatuer to water temperature linear regression plus assume saturated oxygen", 
      type = "empirical",  
      repository = "https://github.com/rqthomas/neon4cast-example" 
    ),
    initial_conditions = list(
      status = "absent"
    ),
    drivers = list(
      status = "propagates",
      complexity = 1, #Just temperature
      propagation = list( 
        type = "ensemble", 
        size = 31) 
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

```
Step 10: Submit forecast!
  
  ```{r}
neon4cast::submit(forecast_file = forecast_file, metadata = metadata_file, ask = FALSE)
```
