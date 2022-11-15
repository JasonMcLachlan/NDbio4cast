#We think the first step is to get variables and make a simple forecast to submit
#so we took the mid term exercise and started to make changes to it

library(tidyverse)
library(neon4cast)
library(lubridate)
#install.packages("rMR")
library(rMR)

dir.create("drivers", showWarnings = FALSE)

forecast_date <- Sys.Date()
noaa_date <- Sys.Date() - days(1)  #Need to use yesterday's NOAA forecast because today's is not available yet

#Step 0: Define team name and team members 

team_name <- "chlorofun"

team_list <- list(list(individualName = list(givenName = c("Kayla","Nick","Stacy","Zhuoran"), 
                                             surName = c("Anderson","Andrysiak","Mowry","Yu")),
                       organizationName = "University of Notre Dame",
                       electronicMailAddress = "zyu3@nd.edu")))

#Step 1: Download latest target data and site description data

target <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz", guess_max = 1e6)

site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") |> 
  filter(aquatics == 1)

#Step 2: Get drivers

df_past <- neon4cast::noaa_stage3()

df_future <- neon4cast::noaa_stage2()

sites <- unique(target$site_id)

noaa_past <- df_past |> 
  dplyr::filter(site_id %in% sites,
                variable == "air_temperature") |> 
  dplyr::collect()

#can't get future data "Error: Filter expression not supported for 
#Arrow Datasets: time >= lubridate::as_datetime(forecast_date)" and we have different 
#error messages from different group members
noaa_future <- df_future |> 
  dplyr::filter(cycle == 0,
                start_date == as.character(noaa_date),
                time >= lubridate::as_datetime(forecast_date), 
                variable == "air_temperature") |> 
  dplyr::collect()

# Step 2.4 Aggregate (to day) and convert units of drivers

noaa_past_mean <- noaa_past %>% 
  mutate(date = as_date(time)) %>% 
  group_by(date, site_id) %>% 
  summarize(air_temperature = mean(predicted, na.rm = TRUE), .groups = "drop") %>% 
  rename(time = date) %>% 
  mutate(air_temperature = air_temperature - 273.15)


noaa_future <- noaa_future %>% 
  mutate(time = as_date(time)) %>% 
  group_by(time, site_id, ensemble) |> 
  summarize(air_temperature = mean(predicted), .groups = "drop") |> 
  mutate(air_temperature = air_temperature - 273.15) |> 
  select(time, site_id, air_temperature, ensemble)

#Step 2.5: Merge in past NOAA data into the targets file, matching by date.
target <- target |> 
  select(time, site_id, variable, observed) |> 
  filter(variable %in% c("temperature", "chla")) |> 
  pivot_wider(names_from = "variable", values_from = "observed")

target <- left_join(target, noaa_past_mean, by = c("time","site_id"))
target$year <- year(target$datetime)
#only choose our study site BARC
target <- subset(target,site_id=="BARC")

ggplot(target, aes(x = chla, y = air_temperature)) +
  geom_point() +
  labs(x = "cholophyll a", y = "air temperature") +
  facet_wrap(~site_id)


#Step 3.0: Generate forecasts for each site

forecast <- NULL

for(i in 1:length(sites)){
  
  # Get site information for elevation
  site_info <- site_data %>% filter(field_site_id == sites[i]) 
  
  site_target <- target |> 
    filter(site_id == sites[i])
  
  noaa_future_site <- noaa_future |> 
    filter(site_id == sites[i])
  
  if(length(which(!is.na(site_target$air_temperature) & !is.na(site_target$chla))) > 0){
    
    #Fit linear model based on past data: chla = m * air temperature + b
    fit <- lm(site_target$chla~site_target$air_temperature)
    
    #use linear regression to forecast water temperature for each ensemble member
    #use a simple model here for first step, we have some questions we want to ask in class
    forecasted_chla <- fit$coefficients[1] + fit$coefficients[2] * noaa_future_site$air_temperature
    
    chla <- tibble(time = noaa_future_site$time,
                     site_id = sites[i],
                     ensemble = noaa_future_site$ensemble,
                     predicted = forecasted_chla,
                     variable = "chla")
    
    
    #Build site level dataframe.  Note we ARE forecasting chla
    forecast <- dplyr::bind_rows(forecast, chla)
  }
}

forecast <- forecast |> 
  mutate(start_time = forecast_date) |> #start_time is today
  select(time, start_time, site_id, variable, ensemble, predicted)

#Visualize forecast. We can't yet since we can't get future temperature from NOAA
forecast %>% 
  ggplot(aes(x = time, y = predicted, group = ensemble)) +
  geom_line() +
  facet_grid(variable~site_id, scale ="free")

#Forecast output file name in standards requires for Challenge.  
# csv.gz means that it will be compressed
forecast_file <- paste0("aquatics","-",min(forecast$time),"-",team_name,".csv.gz")

#Write csv to disk
write_csv(forecast, forecast_file)

#Confirm that output file meets standard for Challenge
#neon4cast::forecast_output_validator(forecast_file)

# Step 4: Generate metadata

model_metadata = list(
  forecast = list(
    model_description = list(
      forecast_model_id =  "chlorofun",  #What goes here
      name = "Air temperature to chla linear regression", 
      type = "empirical",  
      repository = "https://github.com/zyu3/NDbio4cast.git" 
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

metadata_file <- neon4cast::generate_metadata(forecast_file, team_list, model_metadata)

# Step 5: Submit forecast!


#neon4cast::submit(forecast_file = forecast_file, metadata = metadata_file, ask = FALSE)
