
model_id <- "NDWaterTempDO"

source("~/Downloads/NDbio4cast/Forecasting Group 2/FirstForecast.R")

file_date <- Sys.Date() #forecast$reference_datetime[1]
forecast_file <- paste0("aquatics","-",file_date,"-",model_id,".csv.gz")

x.cols <- as.data.frame(x.cols)
colnames(x.cols) <- time
x.cols2 <- cbind(ensemble = 1:nrow(x.cols), x.cols)
ensemble = 1:nrow(x.cols)

# created vector with 5 characters
columns= c("model_id", "datetime", "reference_date", "site_id", "family", "parameter", "variable", "prediction")

# pass this vector length to ncol parameter
# and nrow with 0
forecast = data.frame(matrix(nrow = 2, ncol = length(columns)))

# assign column names
colnames(forecast) = columns

# display
print(forecast)

forecast$model_id <- rep(model_id, times = nrow(forecast))





# Format results to EFI standard
forecast <- forecast |>
  mutate(reference_datetime = forecast_date,
         family = "ensemble",
         model_id = model_id) |>
  rename(parameter = ensemble) |>
  select(model_id, datetime, reference_datetime,
         site_id, family, parameter, variable, prediction)

