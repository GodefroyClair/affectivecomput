############################################
############### RESPIRATION ################
############################################


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
library(testthat)
library(lubridate)
library(stringr)
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

#insert df_all in high order function
#exemple to pick up AB pick_exp(df_all)("AB")
get_expe_base <- function(df) {
  function(expe) {
    df %>% filter(nom.experience == expe)
  }
}

#high order function to create a signal based on a stat and a loess
add_stat_per_period <- function(stat) {
  
  function(df) {
    #extract fonction name from stat
    fun_name <- str_extract(deparse(stat), "(?<=\").*(?=\")")
    fun_name <- fun_name[!is.na(fun_name)]
    col_name <- paste(fun_name[1], "per_period", sep = "_")
    
    if(any(!is.na(df$respi_clean_hl))) {
      na <- is.na(df$respi_clean_hl)
      df$loess <- rep(NA, nrow(df)) 
      df[!na,]$loess <- loess(df$respi_clean_hl[!na] ~ as.numeric(df$date[!na]), degree=1,span=.1)$fitted
      above <- df$respi_clean_hl >= df$loess # Is it ok ???
      period <- rep(NA, length(above))
      period[!na] <- cumsum(c(0, diff(above[!na]) == 1)) #difference between vector and following
      df[!na, col_name] <- stat_per_period(df$respi_clean_hl, period)(stat)
    } else {
      df[,col_name] <- rep(NA, nrow(df))
    }
   
    
    df
  }
  
}

df_all <- import_df_all()
get_expe <- get_expe_base(df_all)


list_exp <- levels(df_all$nom.experience) #redondant

df_half_1 <- df_all[df_all$nom.experience %in% list_half_1,]
df_half_2 <- df_all[df_all$nom.experience %in% list_half_2,]

###########################################################
###################FIN LOAD & PREPARE######################
###########################################################

###############################
########## DATA VIZ ###########
###############################

if (PRINT) {
  
  plot.evol.par.expe(df_half_1, mesure = "respiration")
  plot.evol.par.expe(df_half_2, mesure = "respiration")
  
  
  df_apprenti <- df_all[df_all$nom.experience %in% c("AB","ST","DA","FS1","CW","AW","EZ1","GC1","IA"),]
  df_apprenti_fort <- df_all[df_all$nom.experience %in% c("AB","AW","GC1","IA"),]
  
  plot.evol.par.expe(df_apprenti, titre="diminution de tension de la ceinture")
  plot.evol.par.expe(df_apprenti_fort, titre="diminution tension ceinture fort")
  plot.evol.par.expe(df_all[df_all$tps.ecoule<100,],titre="premiers instants")
  
  df_comp_ampl <- df_all[df_all$nom.experience %in% c("AB","CW"),]
  plot_evol_par_expe(df_comp_ampl,titre="comparaison de l'amplitude")
  
  df_comp_ampl2 <- df_all[df_all$nom.experience %in% c("DE","CLP"),]
  plot.evol.par.expe(df_comp_ampl2,titre="amplitude, autre exemple")
  
  df_seuil <- df_all[df_all$nom.experience %in% c("AB","LM","CLP","DA","DA2","DA3"),]
  plot.evol.par.expe(df_seuil,titre="Seuil bas")
  
  df_seuil2 <- df_all[df_all$nom.experience %in% c("AB","CLP","DA","DA3"),]
  plot.evol.par.expe(df_seuil2,titre="Seuil bas, exemples")
  
  df_seuil3 <- df_all[df_all$nom.experience %in% c("DA3"),]
  plot.evol.par.expe(df_seuil3[1:5000,],titre="Seuil bas en détail")
  
  df_var_ampl <- df_all[df_all$nom.experience %in% c("AB","DE","HL"),]
  plot.evol.par.expe(df_var_ampl,titre="Changement de l'amplitude ?")
  
  df_var_aber <- df_all[df_all$nom.experience %in% c("FS1"),]
  plot.evol.par.expe(df_var_aber,titre="Données aberrantes")
  
  df_rupt_AB <- df_all[df_all$nom.experience %in% c("AB"),]
  plot.evol.par.expe(df_rupt_AB[20000:30000,],titre="rupture, AB")
  plot.mesure.direct(data_AB[20000:30000,])
}
################################
##### Patterns par individu ####
################################

get_data_ho <- function(df) {
  function(nom.expe) {
    df %>% filter(nom.experience == nom.expe)
  }
}

get_expe <- get_data_ho(df_all)
data_AB <- get_expe("AB")
data_CW <- get_expe("CW")
data_CLP <- get_expe("CLP")
data_LM <- get_expe("LM")
data_AW <- get_expe("AW")
data_DA <- get_expe("DA")
data_DA2 <- get_expe("DA2")
data_DA3 <- get_expe("DA3")
data_DE <- get_expe("DE")
data_EZ1 <- get_expe("EZ1")
data_FS1 <- get_expe("FS1")
data_GC1 <- get_expe("GC1")
data_HL <- get_expe("HL")
data_IA <- get_expe("IA")
data_LM <- get_expe("LM")
data_PCo <- get_expe("PCo")
data_PCo2 <- get_expe("PCo2")
data_PCo3 <- get_expe("PCo3")
data_ST <- get_expe("ST")

if (PRINT) {
  ##AB>
  # hyperventilation ?
  
  chrono.plot.direct(data_AB, "respiration", taille=500, offset=200)
  # hyperventilation ?
  chrono.plot.direct(data_AB, "respiration", taille=1000, offset=3600)
  #zoom
  chrono.plot.direct(data_AB, "respiration", taille=500, offset=3800)
  #hyperventilation ou mauvaise mesure ?
  chrono.plot.direct(data_AB, "respiration", taille=500, offset=5400)
  chrono.plot.direct(data_AB, "respiration", taille=300, offset=5500)
  #hypoventilation ?
  chrono.plot.ellipse(data_AB[5000:13000,], "respiration", taille=2000, offset=5000)
  chrono.plot.direct(data_AB, "respiration", taille=4000, offset=10000)
  chrono.plot.direct(data_AB, "respiration", taille=1000, offset=10000)
  chrono.plot.direct(data_AB, "respiration", taille=800, offset=10100)
  
  #Conclusion pour AB : doit-on prendre en compte les "pics" ?
  
  ##AW
  
  chrono.plot.direct(data_AW, "respiration", taille=5000, offset=0000)
  chrono.plot.direct(data_AW, "respiration", taille=5000, offset=10000)
  
  chrono.plot.direct(data_AW, "respiration", taille=5000, offset=20000)
  chrono.plot.direct(data_AW, "respiration", taille=5000, offset=25000)
  chrono.plot.direct(data_AW, "respiration", taille=5000, offset=30000)
  chrono.plot.direct(data_AW, "respiration", taille=5000, offset=35000)
  chrono.plot.direct(data_AW, "respiration", taille=5000, offset=40000)
  chrono.plot.direct(data_AW, "respiration", taille=5000, offset=45000)
  
  ##CW
  #réajustement ceinture ?
  chrono.plot.direct(data_CW, "respiration", taille=800, offset=22200)
  #parle ou bouge ?
  chrono.plot.direct(data_CW, "respiration", taille=500, offset=25600)
  #idem ?
  chrono.plot.direct(data_CW, "respiration", taille=700, offset=30000)
  chrono.plot.direct(data_CW, "respiration", taille=5000, offset=44000)
  
  
  
  
  chrono.plot.direct(data_CW,"respiration",.01,taille=10000,offset=0)
  chrono.plot.direct(data_CW,"respiration",.01,taille=10000,offset=10000)
  chrono.plot.direct(data_CW,"respiration",.01,taille=10000,offset=20000)
  chrono.plot.direct(data_CW,"respiration",.01,taille=10000,offset=30000)
  chrono.plot.direct(data_CW,"respiration",.01,taille=10000,offset=40000)
  chrono.plot.direct(data_CW,"respiration",.01,taille=10000,offset=50000)
  
  chrono.plot.direct(data_CW,"respiration",.01,taille=400,offset=25700)
  chrono.plot.direct(data_CW,"respiration",.01,taille=10000,offset=20000)
}

########################################
############## fin patterns ############
########################################

if(DEBUG) {
  #filter
  #problème à envisager : perte d'amplitude
  #distinguer les 2 pbs : amplitude & périodicité
  #pour la périodicité, il faut mesurer la fréquence en comparant un filtre passe-bas élevé (env 1/500) et moins élevé (1/50)
  #pour l'amplitude, il faut un filtre
  #par ailleurs, pour mettre la courbe à plat, il faut utiliser un filtre passe-haut assez élevé (1/000?)
  aw.hf <- butter(1, 1/1000, type="high")
  #retire les données à zeros
  # clean.data <- function(df, var.to.clean){data_AW[!(var.to.clean == 0),]  }
  data_AW_clean <- data_AW[!(data_AW$respiration == 0),] 
  
  #filtre passe bas pour éliminer les fréquences basses (ie périodes longues) todo : justifier 1/20
  aw.lf <- butter(1, 1/75, type="low")
  
  #eliminates noise
  data_AW_clean$respiration.lf <- signal::filter(aw.lf,data_AW_clean$respiration)
  #elminates trends
  data_AW_clean$respiration.hf <- signal::filter(aw.hf,data_AW_clean$respiration)
  
  data_AW_clean$respiration.hlf <- signal::filter(aw.hf,as.numeric(data_AW_clean$respiration.lf))
  
  library(pracma)
  #recheche de max
  
  #travaille sur un échantillon
  offset <- 10000
  sample.AW.clean <- data_AW_clean[offset + 0: (offset + 2001),]
  max.peaks.samp <- as.data.frame(findpeaks(sample.AW.clean$respiration.hlf ))
  max.peaks.samp$x <- sample.AW.clean$date[max.peaks.samp[,2] ]
  #with noise
  max.peaks.hf.samp <- as.data.frame(findpeaks(sample.AW.clean$respiration.hf ))
  max.peaks.hf.samp$x <- sample.AW.clean$date[max.peaks.hf.samp[,2] ]
  
  ggplot(sample.AW.clean, aes(y = respiration.hlf, date)) + geom_point( color = "lightblue", alpha = .7) +
    geom_point(data = max.peaks.samp, aes(x = x, y = V1), color = "blue", alpha = 1) + 
    geom_point(aes(y = respiration.hf), color = "pink", alpha = .5) +
    geom_point(data = max.peaks.hf.samp, aes(x = x, y = V1), color = "magenta", alpha = 1, size = .9)
  
  
  #travaille sur l'ens des données
  offset <- 10000
  max.peaks <- as.data.frame(findpeaks(as.numeric(data_AW_clean$respiration.hlf )))
  max.peaks$x <- data_AW_clean$date[max.peaks[,2] ]
  str(max.peaks)
  
  ggplot(data_AW_clean, aes(y = respiration.lf, date)) + geom_line( color = "blue") +
    geom_line(aes(y = respiration.hlf), color = "red", alpha = .75) +
    geom_line(aes(y = respiration), color = "green", alpha = .75) +
    # geom_smooth(method="loess", formula = y ~ x,se=TRUE, size= 1, span=.1) +
    geom_smooth(aes(y = respiration.hlf), se=TRUE, formula= y ~ poly(x,5), colour="purple") +
    geom_smooth(se=TRUE, formula= y ~ poly(x,2), colour="orange", span=.1) +
    geom_point(data = max.peaks, aes(x = x, y = V1))
  
  
  
  data_AW_clean$loess <- loess(data_AW_clean$respiration ~ as.numeric(data_AW_clean$date), degree=1,span=.1)$fitted
  data_AW_clean$s.max.min <- signal.max.min(data_AW_clean$respiration, data_AW_clean$loess)
  ggplot(data = data_AW_clean[0:10000,]) + geom_line(aes(x= date, y = respiration), color = "blue") +
    #geom_smooth(method="loess", formula = y ~ x, se=TRUE, size= 1, span=.2, color = "green")  +
    geom_line(aes(x = date, y = loess ), color = "green")  +
    geom_line(aes(x = date, y = s.max.min), color = "red") 
  
} 

################FIN AW###################

if (DEBUG) {
  data.CLP.m <- data.CLP[1:17000,]
  clp.bf <- butter(1, 1/20, type="low")
  data.CW.m <- data.CW
  data.DA.m <- data.DA[!(data.DA$respiration > -5),] 
  data.DA2.m <- NULL
  data.DA3.m <- NULL
  data.DE.m <- data.DE
  de.bf <- butter(1, 1/50, type="low")
  data.EZ1.m <- data.EZ1
  bf <- butter(1, 1/80, type="low")
  data.FS1.m <- data.FS1[!(data.FS1$respiration == 0 | abs(data.FS1$respiration) >25),] 
  #bizarre : ggplot(data = data.FS1.m, aes(x = as.numeric(date),y = respiration)) + geom_line(size=1/10, col = "blue") 
  data.FS1.m <- NULL #??
  data.GC1.m <- data.GC1
  #comment interpéter les piques ??
  #passer un loess en plus ?
  chrono.plot.direct(data.GC1.m, mesure = "resp.low.f")
  data.GC1.m$resp.lf.lo <- loess(data.GC1.m$resp.low.f ~ as.numeric(data.GC1.m$date),degree=1,span=.02)$fitted
  
  data.LM.m <- NULL
  
  data.HL.m <- data.HL
  chrono.plot.direct(data.HL.m, "respiration")
  bf <- butter(1, 1/100, type="low")
  data.HL.m$resp.high.f <- signal::filter(hf,data.HL.m$respiration)
  data.HL.m$resp.low.f <- signal::filter(bf,data.HL.m$respiration)
  ggplot(data = data.HL.m, aes(x = as.numeric(date),y = resp.high.f)) + geom_line(col = "red") + 
    geom_line(data = data.HL.m, aes(x = as.numeric(date),y = resp.low.f), col = "blue") 
  df <-data.frame(data.CW$date[1:lg],b,data.CW$respiration[1:lg])
  colnames(df) <- c("date","filter.low","respi")
  
  ggplot(df[1:10000,],aes(date,filter.low)) + geom_line(size=1) + 
    geom_point(aes(y=respi),col="orange",size=1) 
  
  
  lowpass.spline <- smooth.spline(df$date,df$respi, spar = 0.0001)
  df$lowpass.spline <-lowpass.spline$y
  
  ggplot(df[10000:20000,],aes(date,filter.low)) + geom_line(size=2) + 
    geom_point(aes(y=respi),col="orange",size=1) +
    geom_point(aes(y=lowpass.spline),col="green") 
  
  
  #changepoint
  library(changepoint)
  seg=cpt.var(data.CW$respiration,method="PELT")
  ggplot()
  
  #test
  lapply(c("data.CW"),function(exp){chrono.plot(exp,"respiration")})
  lapply(list.exp.df,function(exp){chrono.plot(exp,"respiration")})
}

#+++++++++++++++++++++#
####EXPE VIZ CHECK#####
#+++++++++++++++++++++#

if(PRINT){
  df_transp <- df_all %>% select(date, nom.experience, respiration, tps.ecoule, quart.temps)
  
  #create the function for plotting
  plot_respi <- plot_var_exp_ho(df_all, "respiration")
  
  # EZ1:
  plot_respi("EZ1")
  
  # PCo:
  plot_respi("PCo")
  
  # CW:
  plot_respi("CW")
  
  # DE:
  plot_respi("DE")
  
  # IA:?
  plot_respi("IA")
  
  # PCo2:
  plot_respi("PCo2")
  
  # PCo3:ok-peaks, 
  plot_respi("PCo3")
  
  #ST:
  plot_respi("ST")
  
  # HL:
  plot_respi("HL")
  
  # GC1
  plot_respi("GC1")
  
  #### NO #### 
  #LM, DA2, DA3,
  
  #LM:
  #NON
  plot_respi("LM")
  expect_equal(min(data_LM$respiration), -18.67)
  
  # DA2:
  #seuil bas
  #NON
  plot_respi("DA2")
  expect_equal(min(data_DA2$respiration), -18.67)
  
  # DA3:NO, 
  plot_respi("DA3")
  expect_equal(min(data_DA3$respiration), -18.67)
  
  #### A NETTOYER !!!###
  #AW, FS1, DA, AB, CLP
  
  # AW:
  #seuil haut 0
  plot_respi("AW")
  
  #FS1:
  #seuil haut 0
  plot_respi("FS1")
  
  #DA:
  #seuil haut 0
  #seuil bas à la fin
  plot_respi("DA")
  expect_equal(min(data_DA$respiration), -18.66)
  
  #AB:
  #seuil bas à la fin
  plot_respi("AB")
  expect_equal(min(data_AB$respiration), -18.67)
  
  # CLP:
  #seuil bas à la fin
  plot_respi("CLP")
  expect_equal(min(data_CLP$respiration), -18.67)
  
}

#####################
###### CLEAN ########
#####################
#new var
df_all$respi_clean <- df_all$respiration

#données manquantes
lg_na <- which(is.na(df_all$respiration))
df_all$respi_clean[lg_na] <- df_all$respiration[lg_na-1]
#test
if(any(is.na(df_all$respi_clean)))stop("still na data in respiration measurement...")

#### NO #### 
#LM, DA2, DA3,

df_all <- df_all %>% mutate( respi_clean = replace(respi_clean, nom.experience %in% c("LM","DA2","DA3"), NA))
expect_true(any(is.na( df_all %>% filter(nom.experience %in% c("LM","DA2","DA3")) %>% select(respi_clean))))
expect_false(any(is.na( df_all %>% filter(!nom.experience %in% c("LM","DA2","DA3")) %>% select(respi_clean))))

if (PRINT) {
  df_half_1 <- df_all[df_all$nom.experience %in% list_half_1,]
  plot.evol.par.expe(df_half_1, mesure = "respi_clean")
}

#### A NETTOYER !!!###
#AW, FS1, DA, AB, CLP

#AW
df_all <- df_all %>% mutate( respi_clean = 
                               replace(respi_clean, nom.experience == "AW" & respiration >= 0,
                                       NA))
if (PRINT) {
  plot_var_exp_ho(df_all, "respiration")("AW")
  plot_var_exp_ho(df_all, "respi_clean")("AW")
}

#FS1
df_all <- df_all %>% mutate( respi_clean = 
                               replace(respi_clean, nom.experience == "FS1" & 
                                         (respiration >= 0 | respiration < -25),
                                       NA))
if (PRINT) {
  plot_var_exp_ho(df_all, "respiration")("FS1")
  plot_var_exp_ho(df_all, "respi_clean")("FS1")
}

#DA 
df_all <- df_all %>% mutate( respi_clean = 
                               replace(respi_clean, nom.experience == "DA" & respiration >= -1 | respiration == -18.6,
                                       NA))
if (PRINT) {
  plot_var_exp_ho(df_all, "respiration")("DA")
  plot_var_exp_ho(df_all, "respi_clean")("DA")
}

#AB
library(lubridate)
date_seuil <- df_all %>% 
  filter(nom.experience == "AB" & respiration <= -18.66 & lubridate::minute(date) >= 43) %>%
  select(date) %>%
  summarise(min(date)) %>% `[[`(.,1)

if(PRINT)ggplot(date_seuil, aes(lubridate::minute(date))) + geom_histogram()
df_all <- df_all %>% mutate(respi_clean = 
                              replace(respi_clean, nom.experience == "AB" & minute(date) >= date_seuil,
                                      NA)
)
if (PRINT) {
  plot_var_exp_ho(df_all, "respiration")("AB")
  plot_var_exp_ho(df_all, "respi_clean")("AB")
}

#CLP
min_date <- df_all %>% filter(nom.experience == "CLP" & respiration < -18.65) %>% select(date) %>% summarise(min(date)) %>% `[[`(.,1)
df_all <- df_all %>% mutate( respi_clean = 
                               replace(respi_clean, nom.experience == "CLP" & date >= min_date ,
                                       NA))
if (PRINT) {
  plot_var_exp_ho(df_all, "respiration")("CLP")
  plot_var_exp_ho(df_all, "respi_clean")("CLP")
}


#### SPECTRAL ANALYSIS #####

if (DEBUG) {
  
  #get the spectrum of each exp
  spectre <- function(df) {
    #df <- df %>% filter(nom.experience == nom.exp)
    spectre <- spectrum_analysis(df$respiration,df$date)
    print(plot_spectrum(spectre, x_lim = c(0,4), rm_zero = T, df$nom.experience[1]))
  }
  if(PRINT) purrr::walk(list_exp, ~ spectre(get_expe(.)))
  if (DEBUG && PRINT) purrr::walk("AB", ~ spectre(get_expe(.)))
  
  
  plot_3_low_filter <- function(df, sample = 1:nrow(df)){
    df$respi_clean_noise1 <- clean_high_freq(df$respiration,1/50)
    df$respi_clean_noise2 <- clean_high_freq(df$respiration,1/5)
    df$respi_clean_noise3 <- clean_high_freq(df$respiration,9/10)
    
    print(ggplot(df[sample,], aes(date, respiration)) +
            geom_line(col = "red") + 
            geom_line(aes(y = respi_clean_noise1 - 3),col = "green") +
            geom_line(aes(y = respi_clean_noise2 - 6),col = "blue") +
            geom_line(aes(y = respi_clean_noise3 - 9),col = "black")
    )
  }
  

  if(PRINT && DEBUG) plot_3_low_filter(get_expe("AB"), sample = 15000:35000)
  if(PRINT) purrr::walk(list_exp, ~ plot_3_low_filter(get_expe(.), sample = 15000:35000))
} 


#========== CLEAN HIGH FREQUENCIES ==============#
#we can do it all at one
df_all <- df_all %>% group_by(nom.experience) %>% 
  mutate(respi_clean_h = clean_high_freq(respi_clean, 1/5))

#be carefull with the beginning and the ending
#Show all graphs
if(PRINT){
  plot_comp <- function(nom.exp) {
    gg <- ggplot(df_all %>% filter(nom.experience == nom.exp), aes(date, respiration)) + 
      geom_path() + geom_point(mapping = aes(y = respi_clean_h_group), alpha = .25, size = .15, col = "blue")
    print(gg)
  }
  walk(list_exp, ~ plot_comp(.))
}

#========== SEPARATE LOW FREQUENCIES (trend) ==============#

#getplot_var_exp_ho(df_all, "respiration")("CLP")
#get_expe <- get_expe_base(df_all)
#df_all$respi_clean_hl <- list_exp %>% map( ~ clean_low_freq(get_expe(.)$respi_clean_h, .01)) %>% 
#flatten_dbl()
df_all <- df_all %>% group_by(nom.experience) %>% mutate(respi_clean_hl = clean_low_freq(respi_clean_h, .01))

#be carefull with the beginning and the ending :
df_all <- df_all %>% group_by(nom.experience) %>% mutate(respi_trend = clean_high_freq(respi_clean_h, .01))
#  list_exp %>% map_df( ~ clean_high_freq(get_expe(.)$respi_clean_h,.01)) %>% flatten_dbl()

if(DEBUG && PRINT) {
  df_all %>% filter(nom.experience == "AB") %>% ggplot(aes(x = date, y = respiration)) + geom_line(col = "purple")
  df_all %>% filter(nom.experience == "AB") %>% ggplot(aes(x = date, y = respi_clean_h)) + geom_line(col = "green")
  df_all %>% filter(nom.experience == "AB") %>% ggplot(aes(x = date, y = respi_clean_hl)) + geom_line(col = "red")
  df_all %>% filter(nom.experience == "AB") %>% ggplot(aes(x = date, y = trend)) + geom_line(col = "blue")
}

#========== ADD FREQUENCY STATS (max, min, mean...) ==============#
#test
if(DEBUG) {
  get_expe <- get_expe_base(df_all)
  add_mean_per_period <- add_stat_per_period(mean)
  add_max_per_period <- add_stat_per_period(max)
  add_min_per_period <- add_stat_per_period(min)
  test <- add_mean_per_period(get_expe("AB"))
  test <- rbind(test, add_mean_per_period(get_expe("AW")))
  test2 <- purrr::map_df(list("AB","AW"), ~ add_mean_per_period(get_expe(.)))
  expect_true(all(test$mean_per_period == test2$mean_per_period,na.rm = T))
  expect_false(all(test$mean_per_period != test2$mean_per_period))
  
  df <- data.frame(date = rep(1:4000), respi_clean_hl = rep(c(2, 3, 2, 1, 0, -1, 0, 1),500))
  #lm <- loess(df$respi_clean_hl ~ as.numeric(df$date))
  #plot(df$date[1:100],df$respi_clean_hl[1:100], type = "l") + abline(0.003008, -0.0000015)
  expect_equal(mean(add_mean_per_period(df)$mean_per_period), 1)
  expect_equal(abs(mean(add_max_per_period(df)$max_per_period) -3 ), 5e-04)
  expect_equal(abs(mean(add_min_per_period(df)$min_per_period) +1) , 5e-04)
}

##TODO travailler par colonnes au lieu du df ?
get_expe <- get_expe_base(df_all)
add_mean_per_period <- add_stat_per_period(mean)
df_all <- purrr::map_df(list_exp, ~ add_mean_per_period(get_expe(.)))
get_expe <- get_expe_base(df_all)
add_min_per_period <- add_stat_per_period(min)
df_all <- purrr::map_df(list_exp, ~ add_min_per_period(get_expe(.)))
get_expe <- get_expe_base(df_all)
add_max_per_period <- add_stat_per_period(max)
df_all <- purrr::map_df(list_exp, ~ add_max_per_period(get_expe(.)))
get_expe <- get_expe_base(df_all)

###### frequency
df_all$period_duration  <- period_id2duration(df_all$max_per_period)
if(DEBUG){
  test  <- period_id2duration(df_all$mean_per_period)
  expect_equal(df_all$period_duration, test)
}

plot_loess <- function(df) {
  print(ggplot(df, aes(x = date, y = respi_clean_hl)) + geom_line(col = "red") +
          geom_line(aes(y  = loess), col = "blue") +
          ggtitle(paste("data", df$nom.experience[1])))
}

get_expe <- get_expe_base(df_all)
#data_AB <- add_max_min_freq(data_AB)
#plot_loess(get_expe("AB"))
if(PRINT) purrr::walk(list_exp, ~ plot_loess(get_expe(.)))

plot_signal <- function(df) {
  ggplot(data = df) + geom_line(aes(x= date, y = respi_clean_hl), color = "blue") +
    geom_smooth(method="loess", formula = y ~ x, se=TRUE, size= 1, span=.2, color = "green")  +
    geom_line(aes(x = date, y = loess ), color = "green")  +
    geom_line(aes(x = date, y = max_per_period), color = "red") +
    geom_line(aes(x = date, y = min_per_period), color = "purple") +
    geom_text(aes(x = date, y = max_per_period, label= period_duration), check_overlap = TRUE) +
    ggtitle(df$nom.experience)
}

if(PRINT ) {
  get_expe <- get_expe_base(df_all)
  
  plot_signal(get_expe("AB")[1000:10000,])
  plot_signal(get_expe("CLP")[1000:10000,])
  plot_signal(get_expe("CW")[1000:10000,])
  plot_signal(get_expe("DA")[1000:10000,])
  plot_signal(get_expe("DA2")[1000:10000,])
  plot_signal(get_expe("DA3")[1000:10000,])
  plot_signal(get_expe("DE")[1000:10000,])
  plot_signal(get_expe("PCo")[1000:10000,])
  plot_signal(get_expe("PCo2")[1000:10000,])
  plot_signal(get_expe("PCo3")[1000:10000,])
}

############################
########## SAVE ############
############################

save(df_all, file = "./data/df_respi.RDa")

df_add <- df_all %>% select(respi_clean, respi_low_f, respi_diff, respi_low_diff) %>% as.data.frame()
save(df_add, file = "./data/df_respi_add.RDa")
