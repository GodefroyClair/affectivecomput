#############################################################
################# Activite.electrodermale ###################
#############################################################

if(!exists("DEBUG"))DEBUG <- FALSE
if(!exists("PRINT"))DEBUG <- FALSE

library(knitr)
library(grid)
library(gtable)
library(reshape2)
library(grDevices)
library(signal)
library(dplyr)
library(testthat)
theme_set(theme_bw(24))

############################################################
#######################LOAD & PREP #########################
############################################################


source("../RGeneralFunctions/ggplot-functions.R")
#filter high freq
source("../RGeneralFunctions/clean-filters-functions.R")
source("../RGeneralFunctions/cleaning-functions.R")
source("./functions/load_df-all.R")
#for plot_ho
source(file = "./functions/kohonen_graph.R")


df_all <- import_df_all()
df_half_1 <- df_all[df_all$nom.experience %in% list_half_1,]
df_half_2 <- df_all[df_all$nom.experience %in% list_half_2,]

###########################################################
###################FIN LOAD & PREPARE######################
###########################################################

if(PRINT) {
  plot.evol.par.expe(df_half_1, mesure = "activite.electrodermale")
  plot.evol.par.expe(df_half_2, mesure = "activite.electrodermale")
}

#voir :
#http://theses.univ-lyon2.fr/documents/getpart.php?id=lyon2.2010.baltenneck_n&part=275513
#http://theses.univ-lyon2.fr/documents/getpart.php?id=lyon2.2010.baltenneck_n&part=275552

#+++++++++++++++++++++#
####EXPE VIZ CHECK#####
#+++++++++++++++++++++#

if(PRINT){
  df_transp <- df_all %>% select(date, nom.experience, activite.electrodermale, tps.ecoule, quart.temps)
  
  #create the function for plotting
  plot_transp <- plot_var_exp_ho(df_all, "activite.electrodermale")
  
  #AB:(highpeak-end),
  #jump 4/5
  plot_transp("AB")
  
  #LM:ok-upsacle, 
  #jump 2/3
  plot_transp("LM")
  
  # EZ1:ok-peak?,
  #jump 2/3
  plot_transp("EZ1")
  
  #FS1:ok-upscale,
  #jump 2/3
  plot_transp("FS1")
  
  # CLP:ok-peaks, 
  #learning ?
  #jump 2/3
  plot_transp("CLP")
  
  #DA:
  plot_transp("DA")
  
  # PCo:ok-upslope, 
  plot_transp("PCo")
  
  # CW:ok-debut,downslope?,
  #learning ?
  plot_transp("CW")
  
  # DE:ok-debut?, 
  #learning ?
  plot_transp("DE")
  
  # AW:ok, 
  #jump 2/3
  plot_transp("AW")
  
  # IA:?
  #jump 2/3
  plot_transp("IA")
  
  # DA2:ok?-downslope, 
  #jump 4/5
  plot_transp("DA2")
  
  # DA3:ok, 
  plot_transp("DA3")
  
  # PCo2:ok-peaks, 
  plot_transp("PCo2")
  
  # PCo3:ok-peaks, 
  #jump 4/5
  plot_transp("PCo3")
  
  
  ##NO###
  #ST:no:flat, 
  plot_transp("ST")
  
  # HL:no-flat,
  plot_transp("HL")
  
  ## A NETTOYER !!!
  # GC1:ok?,
  plot_transp("GC1")
  
}

####################
######TRANSFO#######
####################

################
###NETTOYAGE####
################

#ST & HL:
df_all <- df_all %>%
  mutate(elecderm_clean = replace(activite.electrodermale, nom.experience %in% c("ST", "HL"), NA))

##test
test_df <- df_all %>% 
  filter(nom.experience %in% c("ST","HL")) %>% select(elecderm_clean)
expect_true(all(is.na(test_df)))
rm(test_df)

##GC1##
#-----#
gc1_transpi <- df_all %>% filter(nom.experience == "GC1")
m_gc1 <- round(mean(gc1_transpi$activite.electrodermale, na.rm = T),4)
gc1_transpi <- gc1_transpi %>% mutate( elecderm_clean = 
    clean_high_freq(activite.electrodermale - m_gc1, 1/150) + m_gc1)
df_all[df_all$nom.experience == "GC1",]$elecderm_clean <- gc1_transpi$elecderm_clean


##FAIL TRY

#filter GC1 data with a low pass filter
#df_all[df_all$nom.experience == "GC1",] <- 
#  df_all %>% filter(nom.experience == "GC1") %>% 
#  mutate(elecderm_clean = clean_high_freq(activite.electrodermale, 1/150))


if (PRINT) {
  gg <- plot_var_exp_ho(gc1_transpi, "activite.electrodermale")("GC1", print = F)
  gg + ylim(c(-48, -39))
  gg <- plot_var_exp_ho(gc1_transpi, "elecderm_clean")("GC1", print = F)
  gg + ylim(c(-48, -39))
}


##################
## ADD COLUMNS ###
##################

### ADD var with high frequencies filtered ###
##use of a low pass filter
## clean_low_freq can't deal with NA's...
df_all <- df_all %>% group_by(nom.experience) %>%
  mutate(.,elecderm_low_f = 
           ifelse(!is.na(elecderm_clean), 
                  clean_high_freq(elecderm_clean - mean(elecderm_clean, na.rm = T), 1/100) + 
                    mean(elecderm_clean, na.rm = T), 
                  elecderm_clean))
#any(df_all$elecderm_low_f != df_all$elecderm_low_f2, na.rm = T)

### ADD variation variable ###
df_all <- df_all %>% group_by(nom.experience) %>% mutate(elecderm_diff = elecderm_clean - lag(elecderm_clean))
df_all <- df_all %>% group_by(nom.experience) %>% mutate(elecderm_low_diff = elecderm_low_f - lag(elecderm_low_f))
#df_all[is.na(df_all$elecderm_diff),"elecderm_diff"] <- 0.0

if(PRINT){
  df_half_1 <- df_all[df_all$nom.experience %in% list_half_1,]
  df_half_2 <- df_all[df_all$nom.experience %in% list_half_2,]
  
  plot.evol.par.expe(df_half_1[df_half_1$nom.experience == "AB",], mesure = "elecderm_clean")
  plot.evol.par.expe(df_half_1[df_half_1$nom.experience == "AB",], mesure = "elecderm_low_f")
   
  plot.evol.par.expe(df_half_1, mesure = "activite.electrodermale")
  plot.evol.par.expe(df_half_2, mesure = "activite.electrodermale")
  
  #missing values
  suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "elecderm_clean"))
  suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "elecderm_clean"))
  
  suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "elecderm_low_f"))
  suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "elecderm_low_f"))
  
  suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "elecderm_diff"))
  suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "elecderm_diff"))
  plot.evol.par.expe(df_half_1[df_half_2$nom.experience == "IA",], mesure = "elecderm_diff")
  
  suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "elecderm_low_diff"))
  suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "elecderm_low_diff"))
}

save(df_all, file = "./data/df_transpi.RDa")

df_add <- df_all %>% select(elecderm_clean, elecderm_low_f, elecderm_diff, elecderm_low_diff) %>% as.data.frame()
save(df_add, file = "./data/df_transpi_add.RDa")
