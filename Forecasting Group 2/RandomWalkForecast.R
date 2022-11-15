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
target_barc_temp <- pad(target_barc_temp)
target_barc_chla <- subset(target_barc, variable == "chla")
target_barc_chla <- pad(target_barc_chla)

#set up model variables
time <- as.Date(target_barc_do$datetime)
time <- append(time, tail(time, 1) + c(1:30))
y <- as.vector(target_barc_do$observation)
y <- append(y, rep(NA,30))

#Random Walk Timeseries Model
RandomWalk = "
model{
  
  #### Data Model
  for(t in 1:n){
    y[t] ~ dnorm(x[t],tau_obs)
  }
  
  #### Process Model
  for(t in 2:n){
    x[t]~dnorm(x[t-1],tau_add)
  }
  
  #### Priors
  x[1] ~ dnorm(x_ic,tau_ic)
  tau_obs ~ dgamma(a_obs,r_obs)
  tau_add ~ dgamma(a_add,r_add)
}
"
#Define the data
data <- list(y=y,n=length(y),      ## data (what do I put??)
             x_ic=7,tau_ic=3,      ## initial condition prior (what do I put??)
             a_obs=1,r_obs=1,      ## obs error prior (what do I put??)
             a_add=1,r_add=1       ## process error prior (what do I put??)
)

#Define MCMC Chains -- 
nchain = 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(y,length(y),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(log(y.samp))),  ## initial guess on process precision
                    tau_obs=5/var(log(y.samp)))        ## initial guess on obs precision
}

j.model   <- jags.model (file = textConnection(RandomWalk),
                         data = data,
                         inits = init,
                         n.chains = 3)

jags.out   <- coda.samples (model = j.model,
                            variable.names = c("x", "tau_add","tau_obs"),
                            n.iter = 1000)
plot(jags.out)


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
