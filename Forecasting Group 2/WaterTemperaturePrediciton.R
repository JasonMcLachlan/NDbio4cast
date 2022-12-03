library(tidyverse)
library(neon4cast)
library(lubridate)
library(rMR)
library(arrow)
library(rjags)
library(rnoaa)
library(daymetr)
library(padr)
#devtools::install_github("EcoForecast/ecoforecastR",force=TRUE)
#load target data
target <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz") |>
  na.omit()

#subset to BARC (Barco Lake in Florida)
target_barc <- subset(target, site_id == "BARC")

#past NOAA data
sites <- unique(target_barc$site_id)
df_past <- neon4cast::noaa_stage3()
noaa_past <- df_past |> 
  dplyr::filter(site_id %in% sites,
                variable == "air_temperature") |> 
  dplyr::rename(ensemble = parameter) |> 
  dplyr::collect()

noaa_past_mean <- noaa_past |> 
  mutate(date = as_date(datetime)) |> 
  group_by(date, site_id) |> 
  summarize(air_temperature = mean(prediction, na.rm = TRUE), .groups = "drop") |> 
  rename(datetime = date) |> 
  mutate(air_temperature = air_temperature - 273.15)


forecast_date <- Sys.Date()#assign the nee forecast date as today (the date on the computer)
noaa_date <- Sys.Date() - lubridate::days(1)#assign weather forecast date as yesterday (todays is not available yet)  


df_future <- neon4cast::noaa_stage2()
noaa_future <- df_future |> 
  dplyr::filter(start_date == as.character(noaa_date),
                variable == "air_temperature") |> 
  dplyr::rename(ensemble = parameter) |> 
  dplyr::collect()

target <- target_barc |> 
  select(datetime, site_id, variable, observation) |> 
  filter(variable %in% c("temperature", "oxygen")) |> 
  pivot_wider(names_from = "variable", values_from = "observation")

target <- left_join(target, noaa_past_mean, by = c("datetime","site_id"))

sites <- unique(target$site_id)
#subset_site_data <- site_target[917:nrow(site_target),]


temp_forecast <- NULL

for(i in 1:length(sites)){ 
  i = 1
  
  site_target <- target |> 
    filter(site_id == sites[i])
  
  site_target <- site_target[!is.na(site_target$air_temperature),]
  
  noaa_future_site <- noaa_future |> 
    filter(site_id == sites[i])
  
  #if(length(which(!is.na(site_target$air_temperature) & !is.na(site_target$temperature))) > 0){ #just an optional check if you're running it in a workflow
  
  #Fit linear model based on past data: water temperature = m * air temperature + b
  
  fit <- lm(temperature ~ air_temperature, data = site_target)
  
  noaa_subset <- subset(noaa_future_site, variable = "air_temperature")
  colnames(noaa_subset) <- c("site_id", "air_temperature", "variable", "height", "horizon", "ensemble", "reference_datetime", "forecast_valid", "datetime", "longitude", "latitude", "family", "start_date")
  noaa_subset[,2] = noaa_subset[,2] - 273
  
  #use linear regression to forecast water temperature for each ensemble member
  forecasted_water_temperature <- fit$coefficients[1] + fit$coefficients[2] * noaa_subset$air_temperature
  
  
  
  
  #Build site level dataframe.  Note we are not forecasting chla
  temp_forecast <- cbind(noaa_subset, forecasted_water_temperature)
  #}
}
temp_forecast <- subset(temp_forecast, select = -c(site_id, variable, height, horizon, ensemble, reference_datetime, forecast_valid, longitude, latitude, family, start_date))
temp_forecast <- separate(temp_forecast, datetime, into = c("date", "time"), sep = " (?=[^ ]+$)")
temp_forecast$date <- as.Date(temp_forecast$date)

temp_forecast <- aggregate(forecasted_water_temperature ~ date, temp_forecast, mean)
colnames(temp_forecast) <- c("datetime", "observation")



