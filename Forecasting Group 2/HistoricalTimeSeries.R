library(tidyverse)
library(neon4cast)
library(lubridate)
library(rMR)
library(arrow)

#load target data
target <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/aquatics/aquatics-targets.csv.gz") |>
  na.omit()

#subset to BARC (Barco Lake in Florida)
target_barc <- subset(target, site_id == "BARC")

#subset data frames based on data type
target_barc_do <- subset(target_barc, variable == "oxygen")
target_barc_temp <- subset(target_barc, variable == "temperature")
target_barc_chla <- subset(target_barc, variable == "chla")

#set up model variables
time <- target_barc$datetime
y <- target_barc_do


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
             x_ic= 7,tau_ic= 3, ## initial condition prior (what do I put??)
             a_obs=1,r_obs=1,           ## obs error prior (what do I put??)
             a_add=1,r_add=1            ## process error prior (what do I put??)
)

#Define MCMC Chains -- THIS DOESN'T WORK BECAUSE THERE ARE STILL NAs IN THE DO DATA I THINK
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
                            variable.names = c("tau_add","tau_obs"),
                            n.iter = 1000)
plot(jags.out)

