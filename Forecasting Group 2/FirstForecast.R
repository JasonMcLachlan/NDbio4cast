
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

df_future <- neon4cast::noaa_stage2(cycle = 0)
noaa_future <- df_future |> 
  dplyr::filter(start_date == as.character(forecast_date),
                variable == "air_temperature") |> 
  dplyr::rename(ensemble = parameter) |> 
  dplyr::collect()

target <- target_barc |> 
  select(datetime, site_id, variable, observation) |> 
  filter(variable %in% c("temperature", "oxygen")) |> 
  pivot_wider(names_from = "variable", values_from = "observation")

target <- left_join(target, noaa_past_mean, by = c("datetime","site_id"))

sites <- unique(target$site_id)

temp_forecast <- NULL

for(i in 1:length(sites)){
  
  
  
  site_target <- target |> 
    filter(site_id == sites[i])
  
  noaa_future_site <- noaa_future |> 
    filter(site_id == sites[i])
  
  if(length(which(!is.na(site_target$air_temperature) & !is.na(site_target$temperature))) > 0){
    
    #Fit linear model based on past data: water temperature = m * air temperature + b
    fit <- lm(site_target$temperature~site_target$air_temperature)
    
    #use linear regression to forecast water temperature for each ensemble member
    forecasted_temperature <- fit$coefficients[1] + fit$coefficients[2] * noaa_future_site$air_temperature
    
    
    
    #Build site level dataframe.  Note we are not forecasting chla
    temp_forecast <- dplyr::bind_rows(forecasted_temperature)
  }
}