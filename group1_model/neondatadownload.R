library(tidyverse)
library(neon4cast)
library(lubridate)
#install.packages("rMR")
library(rMR)
##' Download Targets
##' @return data.frame in long format with days as rows, and time, site_id, variable, and observed as columns
download_targets <- function(){
  readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/terrestrial_30min/terrestrial_30min-targets.csv.gz", guess_max = 1e6)
}

##' Download Site metadata
##' @return metadata dataframe
download_site_meta <- function(){
  site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") 
  site_data %>% filter(as.integer(terrestrial) == 1)
}


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