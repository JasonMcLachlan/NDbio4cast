model_id <- "NDWaterTempDO"

##### RUN THIS THIRD TO SUBMIT FORECAST #########

#source("~/FirstForecast.R")

file_date <- Sys.Date() #forecast$reference_datetime[1]
forecast_file <- paste0("aquatics","-",file_date,"-",model_id,".csv.gz")

x.cols <- as.data.frame(x.cols)

# created vector with 5 characters
#columns= c("model_id", "datetime", "reference_datetime", "site_id", "family", "parameter", "variable", "prediction")

forecast <- stack(x.cols)
names(forecast)[names(forecast) == "ind"] <- "datetime"
names(forecast)[names(forecast) == "values"] <- "prediction"
forecast$parameter <- c(1:3000)
forecast$model_id <- rep(model_id, times = nrow(forecast))
forecast$reference_datetime <- rep(Sys.Date(), times = nrow(forecast))
forecast$family <- rep("ensemble", times = nrow(forecast))
forecast$site_id <- rep("BARC", times = nrow(forecast))
forecast$variable <- rep("oxygen", times=nrow(forecast))
forecast$datetime <- as.Date(forecast$datetime)

write_csv(forecast, forecast_file)

neon4cast::forecast_output_validator(forecast_file)

neon4cast::submit(forecast_file = forecast_file, metadata = NULL, ask = FALSE)





