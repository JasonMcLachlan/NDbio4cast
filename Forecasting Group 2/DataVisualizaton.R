library(tidyverse)
library(neon4cast)
library(lubridate)
library(rMR)
library(arrow)

#load target data
target <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz") |>
  na.omit()

#subset to CRAM (Crampton Lake at UNDERC)
target_cram <- subset(target, site_id == "CRAM")

#subset data frames based on data type
target_cram_do <- subset(target_cram, variable == "oxygen")
target_cram_temp <- subset(target_cram, variable == "temperature")
target_cram_chla <- subset(target_cram, variable == "chla")

#plot time-series for each data type
ggplot(data = target_cram_do, aes(x=datetime, y=observation)) + geom_point() + ggtitle("Dissolved oxygen")
ggplot(data = target_cram_temp, aes(x=datetime, y=observation)) + geom_point() + ggtitle("Temperature")
ggplot(data = target_cram_chla, aes(x=datetime, y=observation)) + geom_point() + ggtitle("Chlorophyll a")

#past NOAA data
sites <- unique(target_cram$site_id)
df_past <- neon4cast::noaa_stage3()
noaa_past <- df_past |> 
  dplyr::filter(site_id %in% sites,
                variable == "air_temperature") |> 
  dplyr::rename(ensemble = parameter) |> 
  dplyr::collect()

#plot relationships

#future NOAA data
#set forecast date as yesterday
#error here!
forecast_date <- Sys.Date() - lubridate::days(1)
df_future <- neon4cast::noaa_stage2(cycle = 0)
noaa_future <- df_future |> 
  #start date filter error
  dplyr::filter(variable == "air_temperature",
                start_date == forecast_date) |> 
  dplyr::rename(ensemble = parameter) |> 
  dplyr::collect()

noaa_past_mean <- noaa_past |> 
  mutate(date = as_date(time)) |> 
  group_by(date, site_id) |> 
  summarize(air_temperature = mean(predicted, na.rm = TRUE), .groups = "drop") |> 
  rename(datetime = date) |> 
  mutate(air_temperature = air_temperature - 273.15)

