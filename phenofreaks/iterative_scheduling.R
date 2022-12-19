##For Windows Users:
#install.packages("taskscheduleR")
#install.packages("miniUI")
#install.packages("shiny")

library(taskscheduleR)
library(miniUI)
library(shiny)

## Run script once in 5 seconds (for testing purposes only)
taskscheduler_create(taskname = "singlerun_TEST", rscript = "phenofreaks/test_for_iterative_scheduling.R", 
                     schedule = "ONCE", starttime = format(Sys.time() + 5, "%H:%M"))
## Run every day at noon (12:00)
taskscheduler_create(taskname = "dailyforecast_TEST", rscript = "phenofreaks/test_for_iterative_scheduling.R", 
                     schedule = "DAILY", starttime = "12:00", startdate = format(Sys.Date()+1, "%d/%m/%Y"))

## get a data.frame of all tasks
tasks <- taskscheduler_ls()
str(tasks)

##Non-Windows Users:
#install.packages("cronR")

#library(cronR)
#cmd <- cron_rscript("phenofreaks/test_for_iterative_scheduling.R")
## Run every day at noon (12:00)
#cron_add(command = cmd, frequency = 'daily', at='12pm', id = 'dailyforecast_TEST')
