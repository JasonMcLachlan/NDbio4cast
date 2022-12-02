library(tidyverse)
library(neon4cast)
library(lubridate)
library(rMR)
library(arrow)
library(rjags)
library(rnoaa)
library(daymetr)
library(padr)
devtools::install_github("EcoForecast/ecoforecastR",force=TRUE)

source("~/WaterTemperaturePrediction.R")

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

#set up model variables
time <- as.Date(target_barc_do$datetime)
time <- append(time, tail(time, 1) + c(1:30))
y <- as.vector(target_barc_do$observation)
y <- append(y, rep(NA,30))

#appending forecasted water temp
target_barc_temp <- rbind(target_barc_temp, temp_forecast)

jags_df <- merge(x = target_barc_temp, y = target_barc_do, by = "datetime", all.x = TRUE)
colnames(jags_df) <- c("datetime", "watertemp", "site_id", "variable", "DO")

c <- jags_df$watertemp
y <- jags_df$DO

#Regression Model (Water Temp ~ DO)
WaterTempDO = "
model{

  b ~ dmnorm(b0,Vb)     ## multivariate Normal prior on vector of regression params
  S ~ dgamma(s1,s2)    ## prior precision
  tau_obs ~ dgamma(a_obs,r_obs)

  #### Data Model
  for(t in 1:n){
    y[t] ~ dnorm(x[t],tau_obs)
  }

  #### Process Model
  for(i in 1:n){
      mu[i] <- b[1] + b[2]*c[i]                ## process model - c1 = temp
      x[i] ~ dnorm(mu[i],S)                		 ## data model - y = DO
  }
}
"
#Define the data
# data <- list(y=y,n=length(y),      ## data (what do I put??)
#              x_ic=7,tau_ic=3,      ## initial condition prior (what do I put??)
#              a_obs=1,r_obs=1,      ## obs error prior (what do I put??)
#              a_add=1,r_add=1       ## process error prior (what do I put??)
# )

data <- list(c = c, y = y, a_obs=1, r_obs=1, n = length(y))

## specify priors
data$b0 <- as.vector(c(0,0))      ## regression b means
data$Vb <- solve(diag(10000,2))   ## regression b precisions
data$s1 <- 0.1                    ## error prior n/2
data$s2 <- 0.1                    ## error prior SS/2

#Define MCMC Chains -- 
nchain = 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(y,length(y),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(log(y.samp))),  ## initial guess on process precision
                    tau_obs=5/var(log(y.samp)))        ## initial guess on obs precision
}

j.model   <- jags.model (file = textConnection(WaterTempDO),
                         data = data,
                         inits = init,
                         n.chains = 3)

jags.out   <- coda.samples (model = j.model,
                            variable.names = c("x", "tau_add","tau_obs"),
                            n.iter = 1000)
#plot(jags.out)


time.rng = c(1,length(time))       ## adjust to zoom in and out
out0 <- as.matrix(jags.out)         ## convert from coda to matrix  
x.cols <- as.data.frame(out0[,1:length(y)]) ## grab all columns that contain data for a time point
ci0 <- apply(x.cols,2,quantile,c(0.025,0.5,0.975)) ## model was NOT fit on log scale

plot(time,ci0[2,],type='l',ylim=range(y,na.rm=TRUE),ylab="DO",log='y',xlim=time[time.rng])
## adjust x-axis label to be monthly if zoomed
if(diff(time.rng) < 100){ 
  axis.Date(1, at=seq(time[time.rng[1]],time[time.rng[2]],by='month'), format = "%Y-%m")
}
ecoforecastR::ciEnvelope(time,ci0[1,],ci0[3,],col=ecoforecastR::col.alpha("lightBlue",0.75))
points(time,y,pch="+",cex=0.5)