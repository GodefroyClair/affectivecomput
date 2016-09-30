#############################################################
################# FREQUENCE CARDIAQUE #######################
#############################################################

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


plot.evol.par.expe(df_half_1, mesure = "frequence.cardiaque")
plot.evol.par.expe(df_half_2, mesure = "frequence.cardiaque")


#AB:ok, ST:ok,DA:ok-debut?, LM:ok, FS1:ok, PCo:ok, PCo2:ok, PCo3:ok, CW:ok,HL:ok,CLP:ok, DE:ok-début?, AW:ok-zeros, DA2:ok, DA3:ok, EZ1:ok,GC1:ok-zero,IA:ok

####EXPE OK#####
df_freq_card <- df_all %>% select(date, nom.experience, frequence.cardiaque, tps.ecoule, quart.temps)

show_freq_card <- function(nom.exp) {
  df_freq_card_exp <- df_freq_card %>% filter(nom.experience == nom.exp)
  gg <- ggplot(df_freq_card_exp, aes(x = date, y = frequence.cardiaque)) + 
    geom_path() +
    ggtitle(paste("frequence card", nom.exp))
  print(gg)
}
if (PRINT){
  #AB:(highpeak-end),
  show_freq_card("AB")

  #DA:ok, 
  show_freq_card("DA")

  #LM:ok-upsacle, 
  show_freq_card("LM")
  
  #FS1:ok-upscale,
  show_freq_card("FS1")
  
  # PCo:ok-upslope, 
  show_freq_card("PCo")
  
  # CW:ok-debut,downslope?,
  show_freq_card("CW")
  
  # CLP:ok-peaks à 110, 
  show_freq_card("CLP")
  
  # DE:ok-debut?, 
  show_freq_card("DE")
  
  # EZ1:ok-peak?,
  show_freq_card("EZ1")
  
  # IA: peak a 110?
  show_freq_card("IA")
  
  # DA2:ok?-downslope, 
show_freq_card("DA2")
  
  # DA3:ok, 
  show_freq_card("DA3")
  
  # PCo2:ok-peaks, 
  show_freq_card("PCo2")
  
  # PCo3:ok-peaks, 
  show_freq_card("PCo3")
  
  ##NO###
  #ST:no:flat, 
  show_freq_card("ST")
  
  # HL:no-flat,
  show_freq_card("HL")

  ##NETTOYER
  # GC1:ok?,
  show_freq_card("GC1")
  
  # AW: signal off 
  show_freq_card("AW")
}

###############
### CLEAN  ####
###############


### remove signal off from DA
df_all <- df_all %>% mutate(freqcard_clean = replace(frequence.cardiaque, frequence.cardiaque == 0, NA))
#test
expect_equal(length(which(df_all$frequence.cardiaque == 0)), 476)
expect_length(which(df_all$freqcard_clean == 0),0)

#######################
### ADD VARIABLES  ####
#######################

### ADD lowpass_filter ### 
##use of a low pass filter
## clean_low_freq can't deal with NA's...
df_all <- df_all %>% group_by(nom.experience) %>%
  mutate(., freqcard_low_f = 
           ifelse(!is.na(freqcard_clean), 
                  clean_high_freq(freqcard_clean[!is.na(freqcard_clean)] - mean(freqcard_clean, na.rm = T), 1/100) + 
                    mean(freqcard_clean, na.rm = T), 
                  freqcard_clean))

#df_all$freqcard_low <- NA
#m_tmp <- mean(df_all$freqcard_clean, na.rm = T)
#df_all$freqcard_low[!is.na(df_all$freqcard_clean)] <- clean_high_freq(df_all$freqcard_clean[!is.na(df_all$freqcard_clean)] - m_tmp, 1/100) + m_tmp
   

### ADD diff ####
df_all <- df_all %>% group_by(nom.experience) %>% mutate(freqcard_diff = freqcard_clean - lag(freqcard_clean))
df_all <- df_all %>% group_by(nom.experience) %>% mutate(freqcard_low_diff = freqcard_low_f - lag(freqcard_low_f))
 

if(PRINT){
  df_half_1 <- df_all[df_all$nom.experience %in% list_half_1,]
  df_half_2 <- df_all[df_all$nom.experience %in% list_half_2,]
  
  plot.evol.par.expe(df_half_1[df_half_1$nom.experience == "DA",], mesure = "freqcard_clean")
  plot.evol.par.expe(df_half_1[df_half_1$nom.experience == "DA",], mesure = "freqcard_low_f")
   
  plot.evol.par.expe(df_half_1, mesure = "frequence.cardiaque")
  plot.evol.par.expe(df_half_2, mesure = "frequence.cardiaque")
  
  #missing values
  suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "freqcard_clean"))
  suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "freqcard_clean"))
  
  suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "freqcard_low_f"))
  suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "freqcard_low_f"))
  
  suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "freqcard_diff"))
  suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "freqcard_diff"))
  
  suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "freqcard_low_diff"))
  suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "freqcard_low_diff"))
}

save(df_all, file = "./data/df_freqcard.RDa")

df_add <- df_all %>% select(freqcard_clean, freqcard_low_f, freqcard_diff, freqcard_low_diff) %>% as.data.frame()
save(df_add, file = "./data/df_freqcard_add.RDa")
