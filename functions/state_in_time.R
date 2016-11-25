library(xts)
library(readxl)
library(dplyr)
state_in_time <- read.csv2("./data/Temps_du_film_XP_&_Etats_Pour_Godefroy.csv", 
                           col_names = c("time", "state"), c = rep("text",2))

state_in_time <- read.csv2(file = "./data/Temps_du_film_XP_&_Etats_Pour_Godefroy.csv", 
                           header = F, sep = ",", col.names = c("time", "state"), 
                           colClasses = rep("character",2), na.strings = "")
dplyr::glimpse(state_in_time)

head(state_in_time)
any(is.na(state_in_time$time))
any(is.na(state_in_time$state))

#modif na
time_na <- which(is.na(state_in_time$time))
state_in_time$time[time_na] <- "00:01 à 03:05"


state_na <- which(is.na(state_in_time$state))
state_in_time$state[state_na] <- "inconnu"

any(is.na(state_in_time$time))
any(is.na(state_in_time$state))

#separate interval in begining and end
state_in_time$from <- gsub(" à.*", "", state_in_time$time)
#put m:ss to format 0m:ss format
state_in_time$from <- gsub("^([0-9]:[0-9]{2})","0\\1", state_in_time$from)
state_in_time$to <- gsub(".*à ", "", state_in_time$time)
#put m:ss to format 0m:ss format
state_in_time$to <- gsub("^([0-9]:[0-9]{2})","0\\1", state_in_time$to)

xts(x = "rien", minute(df_all$tps_ecoule), origin = "00:00")

beg <- state_in_time$from[0]
end <- max(state_in_time$to[nrow(state_in_time)])
