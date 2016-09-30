#######################
######TEMPERATURE######
#######################

if (!exists("DEBUG")) DEBUG <- FALSE
if (!exists("PRINT")) PRINT <- FALSE

library(knitr)
library(grid)
library(ggplot2)
library(gtable)
library(reshape2)
library(grDevices)
library(signal)
library(dplyr)
theme_set(theme_bw(24))

############################################################
#######################LOAD & PREP #########################
############################################################


source("../RGeneralFunctions/ggplot-functions.R")
source("./functions/load_df-all.R")
source("../RGeneralFunctions/clean-filters-functions.R")
source("../RGeneralFunctions/cleaning-functions.R")
#for plot_ho
source(file = "./functions/kohonen_graph.R")

df_all <- import_df_all()
df_half_1 <- df_all[df_all$nom.experience %in% list_half_1,]
df_half_2 <- df_all[df_all$nom.experience %in% list_half_2,]

###########################################################
###################FIN LOAD & PREPARE######################
###########################################################


#plot.evol.par.expe(df_half_1, mesure = "temperature")
#plot.evol.par.expe(df_half_2, mesure = "temperature")


if (PRINT) {              
  plot.evol.par.expe(df_half_1, mesure = "temperature", titre = "température 1ere partie des expé")
  plot.evol.par.expe(df_half_2, mesure = "temperature", titre = "température 2nde moitié des expé")

  plot.evol.par.expe(df_half_1, mesure = "temperature", lim.bas=20, lim.haut = 35, titre = "température 1ere partie des expé")
  plot.evol.par.expe(df_half_2, mesure = "temperature", lim.bas=20, lim.haut = 35, titre = "température 2nde moitié des expé")
}

#######################
### VISUALIZATION  ####
#######################

if (PRINT) {
  df_DA <- df_all %>% filter(nom.experience == "DA", tps.ecoule < 50)
  plot.evol.par.expe(df_DA,titre="expérience DA, valeur proche de 0", mesure = "temperature")
  df_DA_clean <- df_all %>% filter(nom.experience == "DA", tps.ecoule < 50, temperature >8)
  plot.evol.par.expe(df_DA_clean,titre="expérience DA, valeurs inf à 9 nettoyées", mesure = "temperature")

  plot.evol.par.expe(df_all[df_all$tps.ecoule<10,], mesure = "temperature", titre="premiers instants")
  plot.evol.par.expe(df_half_1[df_half_1$tps.ecoule<5,], mesure = "temperature", titre="premiers instants")
  plot.evol.par.expe(df_half_2[df_half_2$tps.ecoule<5,], mesure = "temperature", titre="premiers instants")

  df.deb <- df.all %>% filter(tps.ecoule < 100)
  ggplot(df.deb,aes(x = as.numeric(tps.ecoule), y = temperature, col=nom.experience)) + geom_path(size=.5)
  ggplot(df.deb,aes(x = as.numeric(tps.ecoule), y = temperature, col=nom.experience)) + geom_path(size=.5) + ylim(c(20,35))
  
  df.deb.AB <- df.deb %>% filter(nom.experience == "AB")
  plot.evol.par.expe(df.deb.AB,titre="expérience AB", mesure = "temperature")
  df.deb <- df.all %>% filter(nom.experience == "AW", tps.ecoule < 75)
  plot.evol.par.expe(df.deb,titre="expérience AW", mesure = "temperature")
}

###################
#### EXPE VIZ #####
###################

df_temp <- df_all %>% select(date, nom.experience, temperature, tps.ecoule, quart.temps)

show_temp <- function(nom.exp) {
  df_temp_exp <- df_temp %>% filter(nom.experience == nom.exp)
  gg <- ggplot(df_temp_exp, aes(x = date, y = temperature)) + 
    geom_path() +
    ggtitle(paste("temperature", nom.exp))
  print(gg)
}

if (PRINT){
  
  #AB:29-33.5 : 4 deg ??? 
  show_temp("AB")

  #DA: !! signal off in the beginging
  show_temp("DA")
  
  #LM:22.9 to 23.05 : .15 deg
  show_temp("LM")
  
  #FS1:21.5 to 22.5 : 1 deg
  show_temp("FS1")
  
  # PCo:22.2 to 23 : 0.8 deg
  show_temp("PCo")
  
  # CW:32.5 to 35 : 2.5 deg ?
  show_temp("CW")
  
  # CLP: 26 to 27.4 weird downslope : 1.4 deg
  show_temp("CLP")
  
  # DE: 26 to 32 : 6 deg ???
  show_temp("DE")
  
  # AW: 30.4 to 32 : 1.6 deg 
  show_temp("AW")
  
  # EZ1: 23.4 to 24.6 : 1.2 deg  
  show_temp("EZ1")

  # IA: 23.5 to 26.2 : 2.7 deg ? 
  show_temp("IA")

  # DA2: 28.8 to 29.3 : 0.5 deg
  show_temp("DA2")

  # DA3: 27 to 28.5 : 1.5 deg  
  show_temp("DA3")

  # PCo2: 21.7 to 22.4 : 0.7 deg 
  show_temp("PCo2")

  # PCo3 : 22.15 to 22.4 : 0.25 deg:
  show_temp("PCo3")

  #ST: 26.2 to 27.2 : 1 deg
  show_temp("ST")

  # HL: 22.5 to 23 : 0.5 deg
  show_temp("HL")

  ##NETTOYER
  # GC1: 23.5 to 24.7 : 1.2 deg
  show_temp("GC1")

}


###############
### CLEAN  ####
###############

### ELIMINATE : AB, IA, CW, DE
df_all <- df_all %>%
  mutate(temp_clean = replace(temperature, nom.experience %in% c("AB", "IA", "CW", "DE"), NA))
#test
test <- df_all %>% filter(nom.experience %in% c("AB", "IA", "CW", "DE"))
expect_true(any(is.na(test$temp_clean)))

### remove signal off from DA
df_all <- df_all %>% mutate(temp_clean = replace(temp_clean, temperature < 10, NA))
#test
expect_equal(which(df_all$temperature < 10), 98153)
expect_equal(length(which(df_all$temperature < 10)), 1)
expect_length(which(df_all$temp_clean < 10),0)

#######################
### ADD VARIABLES  ####
#######################

### ADD lowpass_filter ### 
##use of a low pass filter
## clean_low_freq can't deal with NA's...
df_all <- df_all %>% group_by(nom.experience) %>%
  mutate(., temp_low_f = 
           ifelse(!is.na(temp_clean), 
                  clean_high_freq(temp_clean[!is.na(temp_clean)] - mean(temp_clean, na.rm = T), 1/100) + 
                    mean(temp_clean, na.rm = T), 
                  temp_clean))

#df_all$temp_low <- NA
#m_tmp <- mean(df_all$temp_clean, na.rm = T)
#df_all$temp_low[!is.na(df_all$temp_clean)] <- clean_high_freq(df_all$temp_clean[!is.na(df_all$temp_clean)] - m_tmp, 1/100) + m_tmp
   

### ADD diff ####
df_all <- df_all %>% group_by(nom.experience) %>% mutate(temp_diff = temp_clean - lag(temp_clean))
df_all <- df_all %>% group_by(nom.experience) %>% mutate(temp_low_diff = temp_low_f - lag(temp_low_f))
 

if(PRINT){
  df_half_1 <- df_all[df_all$nom.experience %in% list_half_1,]
  df_half_2 <- df_all[df_all$nom.experience %in% list_half_2,]
  
  plot.evol.par.expe(df_half_1[df_half_1$nom.experience == "DA",], mesure = "temp_clean")
  plot.evol.par.expe(df_half_1[df_half_1$nom.experience == "DA",], mesure = "temp_low_f")
  plot.evol.par.expe(df_half_1[df_half_1$nom.experience == "DA",], mesure = "temp_low")
   
  plot.evol.par.expe(df_half_1, mesure = "temperature")
  plot.evol.par.expe(df_half_2, mesure = "temperature")
  
  #missing values
  suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "temp_clean"))
  suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "temp_clean"))
  
  suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "temp_low_f"))
  suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "temp_low_f"))
  
  suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "temp_diff"))
  suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "temp_diff"))
  
  suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "temp_low_diff"))
  suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "temp_low_diff"))
}

save(df_all, file = "./data/df_temp.RDa")

df_add <- df_all %>% select(temp_clean, temp_low_f, temp_diff, temp_low_diff) %>% as.data.frame()
save(df_add, file = "./data/df_temp_add.RDa")
