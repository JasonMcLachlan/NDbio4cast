library(tidyverse)
library(neon4cast)
library(lubridate)
library(rMR)
library(arrow)

#load target data
target <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz") |>
  na.omit()

#subset to BARC (Barco Lake in Florida)
target_barc <- subset(target, site_id == "BARC")

#subset data frames based on data type
target_barc_do <- subset(target_barc, variable == "oxygen")
target_barc_temp <- subset(target_barc, variable == "temperature")
target_barc_chla <- subset(target_barc, variable == "chla")

#plot time-series for each data type
ggplot(data = target_barc_do, aes(x=datetime, y=observation)) + geom_point() + ggtitle("Dissolved oxygen")
ggplot(data = target_barc_temp, aes(x=datetime, y=observation)) + geom_point() + ggtitle("Temperature")
ggplot(data = target_barc_chla, aes(x=datetime, y=observation)) + geom_point() + ggtitle("Chlorophyll a")

#past NOAA data
sites <- unique(target_barc$site_id)
df_past <- neon4cast::noaa_stage3()
noaa_past <- df_past |> 
  dplyr::filter(site_id %in% sites,
                variable == "air_temperature") |> 
  dplyr::rename(ensemble = parameter) |> 
  dplyr::collect()

#plot relationships


forecast_date <- Sys.Date()#assign the nee forecast date as today (the date on the computer)
noaa_date <- Sys.Date() - lubridate::days(1)#assign weather forecast date as yesterday (todays is not available yet)  

#future NOAA data
#set forecast date as yesterday
#error here!
<<<<<<< HEAD
forecast_date <- Sys.Date() - lubridate::days(1)
df_future <- neon4cast::noaa_stage2()
=======
df_future <- neon4cast::noaa_stage2()
noaa_future <- df_future |> 
  dplyr::filter(start_date == as.character(noaa_date),
                variable == "air_temperature") |> 
  dplyr::rename(ensemble = parameter) |> 
  dplyr::collect()

##



df_future <- neon4cast::noaa_stage2(cycle = 0)
>>>>>>> 1dbcad3452e826786e509479b7678bd818bd99ba
noaa_future <- df_future |> 
  dplyr::filter(start_date == as.character(forecast_date),
                variable == "air_temperature") |> 
  dplyr::rename(ensemble = parameter) |> 
  dplyr::collect()

noaa_past_mean <- noaa_past |> 
  mutate(date = as_date(time)) |> 
  group_by(date, site_id) |> 
  summarize(air_temperature = mean(predicted, na.rm = TRUE), .groups = "drop") |> 
  rename(datetime = date) |> 
  mutate(air_temperature = air_temperature - 273.15)

