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
#install.packages("Hmisc")
library(Hmisc)

source("~/Downloads/NDbio4cast/Forecasting Group 2/NewLM.R")


#load target data
target <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz") |>
  na.omit()

#subset to BARC (Barco Lake in Florida)
target_barc <- subset(target, site_id == "BARC")
target_barc <- pad(target_barc)

#subset data frames based on data type
target_barc_do <- subset(target_barc, variable == "oxygen")
target_barc_do <- pad(target_barc_do)
target_barc_temp <- subset(target_barc, variable == "temperature")
target_barc_temp <- subset(target_barc_temp, select = -c(site_id, variable))
target_barc_temp <- pad(target_barc_temp)
target_barc_chla <- subset(target_barc, variable == "chla")
target_barc_chla <- pad(target_barc_chla)

#appending forecasted water temp
target_barc_temp <- rbind(target_barc_temp, temp_forecast)
#tail(target_barc_temp)
jags_df <- merge(x = target_barc_temp, y = target_barc_do, by = "datetime", all.x = TRUE)
colnames(jags_df) <- c("datetime", "watertemp", "site_id", "variable", "DO")

jags_df <- jags_df[!duplicated(jags_df$datetime),]

#tail(target_barc_temp)
y <- as.vector(jags_df$DO)
#y <- append(y, rep(NA,30))

time <- as.Date(jags_df$datetime)
#time <- append(time, tail(time, 1) + c(1:30))

Temp <- jags_df$watertemp
Temp <- with(jags_df, impute(watertemp, mean))

#set up model variables
# time <- as.Date(target_barc_do$datetime)
# time <- append(time, tail(time, 1) + c(1:30))
# y <- as.vector(target_barc_do$observation)
# y <- append(y, rep(NA,30))

data <- list(Temp = Temp, y = y, n = length(y), ##data
             x_ic=7,tau_ic=3, ## initial condition error prior
             a_obs=1, r_obs=1, ## observation error prior
             a_add=1, r_add=1, ## random walk error prior
             a_driv=1, r_driv=1) ## driver uncertainty


WaterTempDO = "
model{
  #### Data Model
  for(t in 1:n){
    y[t] ~ dnorm(x[t],tau_obs) ##Observation uncertainty
  }
  #### Process Model
  for(t in 2:n){
    mu[t] <- x[t-1] + betaX*x[t-1] + betaTemp*Temp_a[t]
    x[t]~dnorm(mu[t],tau_add) ##Process uncertainty 
    Temp_a[t] ~ dnorm(Temp[t], tau_driv) ##Driver uncertainty 
  }
  #### Priors
  x[1] ~ dnorm(x_ic,tau_ic) ##Initial condition uncertainty 
  tau_obs ~ dgamma(a_obs,r_obs)
  tau_add ~ dgamma(a_add,r_add)
  tau_driv ~ dgamma(a_driv,r_driv)
  betaX ~ dnorm(0,0.1) ##Parameter uncertainty 
  betaTemp ~ dnorm(0,0.1) ##Parameter uncertainty 
}
"

#Define MCMC Chains -- 
nchain = 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(y,length(y),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(log(y.samp))),  ## initial guess on process precision
                    tau_obs=5/var(log(y.samp)),
                    tau_driv=1/var(diff(log(y.samp))))
}

j.model   <- jags.model (file = textConnection(WaterTempDO),
                         data = data,
                         inits = init,
                         n.chains = 3)

jags.out   <- coda.samples (model = j.model,
                            variable.names = c("x", "tau_add", "tau_obs", "tau_driv"),
                            n.iter = 10000)
#plot(jags.out)
time.rng = c(1,length(time))
#time.rng = c(1,length(time))       ## adjust to zoom in and out
out0 <- as.matrix(jags.out)         ## convert from coda to matrix  
x.cols <- as.data.frame(out0[,4:(length(y)+3)]) ## grab all columns that contain data for a time point
ci0 <- apply(x.cols,2,quantile,c(0.025,0.5,0.975)) ## model was NOT fit on log scale
#ci0 <- subset(ci0, select = -c(r_add, r_driv, r_obs))
#length(ci0[2,])
#length(time)
plot(time,ci0[2,],type='l',ylim=range(y,na.rm=TRUE),ylab="DO",log='y',xlim=time[time.rng])
## adjust x-axis label to be monthly if zoomed
if(diff(time.rng) < 100){ 
  axis.Date(1, at=seq(time[time.rng[1]],time[time.rng[2]],by='month'), format = "%Y-%m")
}
ecoforecastR::ciEnvelope(time,ci0[1,],ci0[3,],col=ecoforecastR::col.alpha("lightBlue",0.75))
points(time,y,pch="+",cex=0.5)

#plot(time, Temp, type="l", ylim=c(0,40))
#lines(time, y)
