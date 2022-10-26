library(tidyverse)
library(neon4cast)
library(lubridate)
#install.packages("rMR")
library(rMR)
library(dplyr)
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

#Looking at variables that might impact NEE

###TEMPERATURE
#get past weather estimates of air temp from NOAA
noaa_past_temp <- df_past |> 
  dplyr::filter(site_id %in% sites,
                variable == "air_temperature") |> 
  dplyr::collect()

#Filter for UNDERC
noaa_past_temp_underc <- noaa_past_temp |> 
  dplyr::filter(site_id == "UNDE" ) |> 
  dplyr::collect()

#Plot air temp over time
ggplot(data = noaa_past_temp_underc, aes(x=datetime,y=prediction))+
  geom_line()+
  ggtitle("Air Temperature at UNDERC")

#Join UNDERC past NEE and past temp data
underc_nee_temp <- left_join(underc, noaa_past_temp_underc, by = c("datetime" = "datetime"))

#Plot NEE and Temp over time
#Not exactly sure how to plot both on the same plot with a y-axis scale that makes sense
#ggplot(data = underc_nee_temp, aes(x=datetime))+
  #geom_line(aes(y=observation))
#ggplot(data = underc_nee_temp, aes(x=datetime))+
  #geom_line(aes(y=prediction))

#Plot NEE  and temp correlation
plot(underc_nee_temp$observation,underc_nee_temp$prediction,xlab="NEE",ylab = "Air Temperature")
#ggplot(data = underc_nee_temp, aes(x = observation, y = prediction))+
  #geom_point() #ggplot version
#air temps of <275 K (1.85 degrees C) have no negative NEE values and all values are close to 0
#Temp needs to be above ~275 K for ecosystem to photosynthesize?


###SUNLIGHT
#past weather estimates of shortwave flux (solar radiation) from NOAA
noaa_past_sw <- df_past |> 
  dplyr::filter(site_id %in% sites,
                variable == "surface_downwelling_shortwave_flux_in_air") |> 
  dplyr::collect()

#Filter for UNDERC
noaa_past_sw_underc <- noaa_past_sw |> 
  dplyr::filter(site_id == "UNDE" ) |> 
  dplyr::collect()

#Plot downwelling sw flux over time
plot(noaa_past_sw_underc$datetime,noaa_past_sw_underc$prediction, main="Surface downwelling shortwave radiation at UNDERC")
#way too many data points (multiple forecasts of measurements per day for 31 ensembles) to see anything

#filter for one ensemble to hopefully visualize downwelling shortwave flux better
noaa_past_sw_1 <- noaa_past_sw_underc |> 
  dplyr::filter(parameter == 1 ) |> 
  dplyr::collect()

plot(noaa_past_sw_1$datetime,noaa_past_sw_1$prediction, main="Surface downwelling shortwave radiation at UNDERC")
#multiple "curves", need to look more at what is causing them
#points along y=0 from nighttime predictions with no incoming solar radiation


#WATER AVAILABILITY
#not sure precipitation is the best variable to represent this but it's most readily available
#get past weather estimates of precipitation from NOAA
noaa_past_precip <- df_past |> 
  dplyr::filter(site_id %in% sites,
                variable == "precipitation_flux") |> 
  dplyr::collect()

#Filter for UNDERC
noaa_past_precip_underc <- noaa_past_precip |> 
  dplyr::filter(site_id == "UNDE" ) |> 
  dplyr::collect()

#Plot precipitation over time
ggplot(data = noaa_past_precip_underc, aes(x=datetime,y=prediction))+
  geom_line()+
  ggtitle("Precipitation at UNDERC")
#There are some random negative precip values????




                         
#####################

###ADDING FUTURE WEATHER DATA

#future weather estimates of air temp
noaa_future <- df_future |> 
  dplyr::filter(cycle == 0,
                start_date == as.character(noaa_date),
                time >= lubridate::as_datetime(forecast_date), 
                variable == "air_temperature") |> 
  dplyr::collect()







##' append historical meteorological data into target file
##' @param target targets dataframe
##' @return updated targets dataframe with added weather data
merge_met_past <- function(target){
  
  ## connect to data
  df_past <- neon4cast::noaa_stage3()
  
  ## filter for site and variable
  sites <- unique(target$site_id)
  noaa_past <- df_past |> 
    dplyr::filter(site_id %in% sites,
                  variable == "nee") |> 
    dplyr::collect()
  
  ## Aggregate (to day) and convert units of drivers
  target <- target %>% 
    group_by(datetime, site_id,variable) %>%
    summarize(obs2 = mean(observed, na.rm = TRUE), .groups = "drop") %>%
    mutate(obs3 = ifelse(is.nan(obs2),NA,obs2)) %>%
    select(datetime, site_id, variable, obs3) %>%
    rename(observed = obs3) %>%
    filter(variable %in% c("nee", "le")) %>% 
    tidyr::pivot_wider(names_from = "variable", values_from = "observed")
  
  ## Merge in past NOAA data into the targets file, matching by date.
  target <- target |> 
    select(datetime, site_id, variable, observed) |> 
    filter(variable %in% c("nee", "le")) |> 
    tidyr::pivot_wider(names_from = "variable", values_from = "observed")
  
  target <- left_join(target, noaa_past_mean, by = c("datetime","site_id"))
  
}

##' Download NOAA GEFS weather forecast
##' @param forecast_date start date of forecast
##' @return dataframe
download_met_forecast <- function(forecast_date){
  ## connect to data
  df_future <- neon4cast::noaa_stage2()
  
  noaa_date <- forecast_date - lubridate::days(1)  #Need to use yesterday's NOAA forecast because today's is not available yet
  
  ## filter available forecasts by date and variable
  met_future <- df_future |> 
    dplyr::filter(cycle == 0,
                  start_date == as.character(noaa_date),
                  time >= lubridate::as_datetime(forecast_date), 
                  variable == "nee") |> 
    dplyr::collect()
  
  ## aggregate to daily
  met_future <- met_future %>% 
    mutate(time = lubridate::as_date(time)) %>% 
    group_by(time, site_id, ensemble) |> 
    summarize(nee = mean(predicted), .groups = "drop") |> 
    select(time, site_id, nee, ensemble)
  
  return(met_future)
}

### Step 1: Download Required Data
target     <- download_targets()       ## Y variables
site_data  <- download_site_meta()
target     <- merge_met_past(target)   ## append met data (X) into target file
met_future <- download_met_forecast(forecast_date) ## Weather forecast (future X)

## visual check of data
ggplot(target, aes(x = temperature, y = air_temperature)) +
  geom_point() +
  labs(x = "NEON water temperature (C)", y = "NOAA air temperature (C)") +
  facet_wrap(~site_id)