#From original linear model
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
  dplyr::summarize(air_temperature = mean(prediction, na.rm = TRUE), .groups = "drop") |> 
  rename(datetime = date) |> 
  mutate(air_temperature = air_temperature - 273.15)


noaa_date <- Sys.Date() - lubridate::days(1)


#New Way of Accessing Forecasted Temperature Data
noaa_mean_forecast <- function(site, var, reference_date) {
  endpoint = "data.ecoforecast.org"
  bucket <- glue::glue("neon4cast-drivers/noaa/gefs-v12/stage1/0/{reference_date}")
  s3 <- arrow::s3_bucket(bucket, endpoint_override = endpoint, anonymous = TRUE)
  
  # stage1 air temp is Celsius
  arrow::open_dataset(s3) |>
    dplyr::filter(site_id == site,
                  datetime >= lubridate::as_datetime(noaa_date),
                  variable == var) |>
    dplyr::select(datetime, prediction, parameter) |>
    dplyr::mutate(datetime = as_date(datetime)) |>
    dplyr::group_by(datetime, parameter) |>
    dplyr::summarize(air_temperature = mean(prediction), .groups = "drop") |>
    dplyr::select(datetime, air_temperature, parameter) |>
    dplyr::rename(ensemble = parameter) |>
    dplyr::collect()
  
}

noaa_future <- noaa_mean_forecast("BARC", "TMP", noaa_date)

target_barc <- target_barc |> 
  select(datetime, site_id, variable, observation) |> 
  filter(variable %in% c("temperature", "oxygen")) |> 
  pivot_wider(names_from = "variable", values_from = "observation")

target <- left_join(target_barc, noaa_past_mean, by = c("datetime","site_id"))

sites <- unique(target$site_id)
#subset_site_data <- site_target[917:nrow(site_target),]



temp_forecast <- NULL
for(i in 1:length(sites)){ 
  i = 1
  
  site_target <- target |> 
    filter(site_id == sites[i])
  
  site_target <- site_target[!is.na(site_target$air_temperature),]
  
  
  #if(length(which(!is.na(site_target$air_temperature) & !is.na(site_target$temperature))) > 0){ #just an optional check if you're running it in a workflow
  
  #Fit linear model based on past data: water temperature = m * air temperature + b
 
  fit <- lm(temperature ~ air_temperature, data = site_target)
  
  noaa_subset <- subset(noaa_future, variable = "air_temperature")
  colnames(noaa_subset) <- c("site_id", "air_temperature", "variable", "height", "horizon", "ensemble", "reference_datetime", "forecast_valid", "datetime", "longitude", "latitude", "family", "start_date")
  noaa_subset[,2] = noaa_subset[,2] 
  
  #use linear regression to forecast water temperature for each ensemble member
  init_forecast <- fit$coefficients[1] + fit$coefficients[2] * noaa_subset$air_temperature
  
  
  
  #Build site level dataframe.  Note we are not forecasting chla
  temp_forecast <- cbind(noaa_subset, init_forecast)
  #}
}

non_ensemble_forecast <- temp_forecast[temp_forecast$variable == 1,]
temp_forecast <- subset(non_ensemble_forecast, select = -c(air_temperature, variable))
colnames(temp_forecast) <- c("datetime", "observation")

