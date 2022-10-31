library(tidyverse)
library(neon4cast)
library(lubridate)
#install.packages("rMR")
library(rMR)
library(dplyr)
library(rjags)
##' Download Targets
##' @return data.frame in long format with days as rows, and time, site_id, variable, and observed as columns
target <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/terrestrial_daily/terrestrial_daily-targets.csv.gz", guess_max = 1e6) |> 
  na.omit()
target <- target %>% filter(variable == "nee")

##' Download Site metadata
##' @return metadata dataframe
site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") 
site_data %>% filter(as.integer(terrestrial) == 1)

#Filter for UNDERC NEE
underc <- target %>% filter(site_id == "UNDE")

#Plot UNDERC NEE over time
plot(underc$datetime,underc$observation, type="p", pch=1, cex=0.5, main="NEE at UNDERC", xlab="Date", ylab="NEE (gC/m2/day)")
#Winter NEE values are in the range from 0 to slightly positive (emitting CO2 to atmosphere)
#Summer NEE vallues drop in to the negatives (absorbing C02)

forecast_date <- Sys.Date()#assign the nee forecast date as today (the date on the computer)
noaa_date <- Sys.Date() - lubridate::days(1)#assign weather forecast date as yesterday (todays is not available yet)  

#Download past weather estimates from the NOAA using the neon4cast package
df_past <- neon4cast::noaa_stage3()

#Download future weather forecasts from the NOAA
df_future <- neon4cast::noaa_stage2()

sites <- unique(target$site_id) #unique site ids

#Add column to target with Julian day
underc$julianday <- yday(underc$datetime)

#Average NEE observations by julian day
DOYaverage <- underc %>% group_by(julianday) %>% summarise(NEEav = mean(observation))

#plot NEE vs DAY of Year
plot(DOYaverage$julianday,DOYaverage$NEE, type="p", pch=1, cex=0.5, main="NEE at UNDERC", xlab="Day of year", ylab="NEE (gC/m2/day)")

undercdata <- left_join(underc, DOYaverage, by = c("julianday" = "julianday"))

#testing with random walk model, something is wrong from here down because the model only outputs positive NEE values when we need negative NEE in the summer
DOYmodel = "
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
  tau_obs ~ dnorm(a_obs,r_obs)
  tau_add ~ dnorm(a_add,r_add)
}
"

y <- undercdata$observation
time <- as.Date(undercdata$datetime)


data <- list(y=(y),n=length(y),      ## data
             x_ic=(0),tau_ic=1, ## initial condition prior
             a_obs=0,r_obs=1,           ## obs error prior
             a_add=0,r_add=1            ## process error prior
)

#define initial state of model parameters for MCMC chains
nchain = 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(y,length(y),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff((y.samp))),  ## initial guess on process precision
                    tau_obs=5/var((y.samp)))        ## initial guess on obs precision
}

#run model
j.model   <- jags.model (file = textConnection(DOYmodel),
                         data = data,
                         inits = init,
                         n.chains = 3)

#take sample from MCMC output
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("x","tau_add","tau_obs"),
                            n.iter = 10000)
#plot(jags.out)


#plot
time.rng = c(1,length(time))       ## adjust to zoom in and out
out <- as.matrix(jags.out)         ## convert from coda to matrix  
x.cols <- grep("^x",colnames(out)) ## grab all columns that start with the letter x
ci <- apply(exp(out[,x.cols]),2,quantile,c(0.025,0.5,0.975)) 

ModelMedian<- ci[2,]

plot(time,ci[2,],type='n',ylim=range(y,na.rm=TRUE),ylab="NEE",xlim=time[time.rng])
## adjust x-axis label to be monthly if zoomed
if(diff(time.rng) < 100){ 
  axis.Date(1, at=seq(time[time.rng[1]],time[time.rng[2]],by='month'), format = "%Y-%m")
}
ecoforecastR::ciEnvelope(time,ci[1,],ci[3,],col=ecoforecastR::col.alpha("lightBlue",0.75))
points(time,y,pch="+",cex=0.5)
points(time,ci[2,],pch="+",cex=0.5,col="red")

#only outputting positive values?
