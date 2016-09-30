DEBUG <- FALSE

library(knitr)
library(plyr)
library(grid)
library(ggplot2)
library(gtable)
library(ggvis)
library(reshape2)
library(grDevices)
library(kohonen)
library(zoo)
library(tidyr)
library(signal)
library(dplyr)
theme_set(theme_bw(24))

############################################################
#######################LOAD & PREP #########################
############################################################


source("../RGeneralFunctions/dplyr-functions.R")
source("../RGeneralFunctions/load-functions.R")
source("../RGeneralFunctions/cleaning-functions.R")
source("../RGeneralFunctions/ggplot-functions.R")
source("../RGeneralFunctions/clean-filters-functions.R")

mac_gd_path <- "/Users/godot/githubRepos/"
hp_gd_path <- "C:/Users/Godefroy/githubRepos/"


if(Sys.info()['nodename']=="THIM")gd_path <- hp_gd_path else
  gd_path <- mac_gd_path
data_path <- paste(gd_path,"affectiveComputing/data/",sep = "/")


#en argument : la liste des noms des fichiers csv à importer
load_data <- function(exp){
  #exp <- c("AB", "ST")
  filename <- paste(data_path,exp, "_SurEchantillonHF_header.csv", sep="")
  #print(filename)
  #df_name <- paste("data_", exp, sep="")
  #permet de lancer la fonction load.file sur la liste passée en arguments
  #assign(df_name, load.file(filename), envir = .GlobalEnv)
  df <- purrr::map_df(filename, load_file)
  df$nom.experience <- factor(df$nom.experience)
  df <- fun.calc.duree(df)
  df
}

list_exp <- c("AB", "ST", "DA", "LM", "FS1", "PCo", "PCo2", "PCo3", "CW", 
              "HL", "CLP", "DE", "AW", "DA2", "DA3", "EZ1", "GC1", "IA")

#charge les datasets en mémoire
df_all <- load_data(list_exp)
testthat::expect_identical(sort(list_exp), levels(df_all$nom.experience))

list_half_1 <- list_exp[1:floor(length(list_exp)/2)]
list_half_2 <- list_exp[(floor(length(list_exp)/2)+1):length(list_exp)]

###########################################################
###################FIN LOAD & PREPARE######################
###########################################################

###########################################################
###################RESPIRATION  ##########################
###########################################################

df_half_1 <- df_all[df_all$nom.experience %in% list_half_1,]
df_half_2 <- df_all[df_all$nom.experience %in% list_half_2,]

plot.evol.par.expe(df_half_1)
plot.evol.par.expe(df_half_2)

##A nettoyer à partir d'ici
df.all <- df_all
df.half.1 <- df_half_1
df.half.2 <- df_half_2

df.apprenti <- df.all[df.all$nom.experience %in% c("AB","ST","DA","FS1","CW","AW","EZ1","GC1","IA"),]
df.apprenti.fort <- df.all[df.all$nom.experience %in% c("AB","AW","GC1","IA"),]

plot.evol.par.expe(df.apprenti,titre="diminution de tension de la ceinture")
plot.evol.par.expe(df.apprenti.fort,titre="diminution tension ceinture fort")
plot.evol.par.expe(df.all[df.all$tps.ecoule<100,],titre="premiers instants")

df.comp.ampl <- df.all[df.all$nom.experience %in% c("AB","CW"),]
plot.evol.par.expe(df.comp.ampl,titre="comparaison de l'amplitude")

df.comp.ampl2 <- df.all[df.all$nom.experience %in% c("DE","CLP"),]
plot.evol.par.expe(df.comp.ampl2,titre="amplitude, autre exemple")

df.seuil <- df.all[df.all$nom.experience %in% c("AB","LM","CLP","DA","DA2","DA3"),]
plot.evol.par.expe(df.seuil,titre="Seuil bas")

df.seuil2 <- df.all[df.all$nom.experience %in% c("AB","CLP","DA","DA3"),]
plot.evol.par.expe(df.seuil2,titre="Seuil bas, exemples")

df.seuil3 <- df.all[df.all$nom.experience %in% c("DA3"),]
plot.evol.par.expe(df.seuil3[1:5000,],titre="Seuil bas en détail")

df.var.ampl <- df.all[df.all$nom.experience %in% c("AB","DE","HL"),]
plot.evol.par.expe(df.var.ampl,titre="Changement de l'amplitude ?")

df.var.aber <- df.all[df.all$nom.experience %in% c("FS1"),]
plot.evol.par.expe(df.var.aber,titre="Données aberrantes")

df.rupt.AB <- df.all[df.all$nom.experience %in% c("AB"),]
plot.evol.par.expe(df.rupt.AB[20000:30000,],titre="rupture, AB")
plot.mesure.direct(data.AB[20000:30000,])

################################
##### Patterns par individu ####
################################

##AB
# hyperventilation ?
chrono.plot.direct(data.AB, "respiration", taille=500, offset=200)
# hyperventilation ?
chrono.plot.direct(data.AB, "respiration", taille=1000, offset=3600)
#zoom
chrono.plot.direct(data.AB, "respiration", taille=500, offset=3800)
#hyperventilation ou mauvaise mesure ?
chrono.plot.direct(data.AB, "respiration", taille=500, offset=5400)
chrono.plot.direct(data.AB, "respiration", taille=300, offset=5500)
#hypoventilation ?
chrono.plot.ellipse(data.AB[5000:13000,], "respiration", taille=2000, offset=5000)
chrono.plot.direct(data.AB, "respiration", taille=4000, offset=10000)
chrono.plot.direct(data.AB, "respiration", taille=1000, offset=10000)
chrono.plot.direct(data.AB, "respiration", taille=800, offset=10100)

#Conclusion pour AB : doit-on prendre en compte les "pics" ?

##AW

chrono.plot.direct(data.AW, "respiration", taille=5000, offset=0000)
chrono.plot.direct(data.AW, "respiration", taille=5000, offset=10000)
chrono.plot.direct(data.AW, "respiration", taille=5000, offset=20000)
chrono.plot.direct(data.AW, "respiration", taille=5000, offset=25000)
chrono.plot.direct(data.AW, "respiration", taille=5000, offset=30000)
chrono.plot.direct(data.AW, "respiration", taille=5000, offset=35000)
chrono.plot.direct(data.AW, "respiration", taille=5000, offset=40000)
chrono.plot.direct(data.AW, "respiration", taille=5000, offset=45000)

##CW
#réajustement ceinture ?
chrono.plot.direct(data.CW, "respiration", taille=800, offset=22200)
#parle ou bouge ?
chrono.plot.direct(data.CW, "respiration", taille=500, offset=25600)
#idem ?
chrono.plot.direct(data.CW, "respiration", taille=700, offset=30000)
chrono.plot.direct(data.CW, "respiration", taille=5000, offset=44000)




chrono.plot.direct(data.CW,"respiration",.01,taille=10000,offset=0)
chrono.plot.direct(data.CW,"respiration",.01,taille=10000,offset=10000)
chrono.plot.direct(data.CW,"respiration",.01,taille=10000,offset=20000)
chrono.plot.direct(data.CW,"respiration",.01,taille=10000,offset=30000)
chrono.plot.direct(data.CW,"respiration",.01,taille=10000,offset=40000)
chrono.plot.direct(data.CW,"respiration",.01,taille=10000,offset=50000)

chrono.plot.direct(data.CW,"respiration",.01,taille=400,names="IA",offset=25700)
chrono.plot.direct(data.CW,"respiration",.01,taille=10000,names="IA",offset=20000)

########################################
############## fin patterns ############
########################################

#filter
#problème à envisager : perte d'amplitude
#distinguer les 2 pbs : amplitude & périodicité
#pour la périodicité, il faut mesurer la fréquence en comparant un filtre passe-bas élevé (env 1/500) et moins élevé (1/50)
#pour l'amplitude, il faut un filtre
#par ailleurs, pour mettre la courbe à plat, il faut utiliser un filtre passe-haut assez élevé (1/000?)
aw.hf <- butter(1, 1/1000, type="high")
#retire les données à zeros
# clean.data <- function(df, var.to.clean){data.AW[!(var.to.clean == 0),]  }
data.AW.clean <- data.AW[!(data.AW$respiration == 0),] 

#filtre passe bas pour éliminer les fréquences basses (ie périodes longues) todo : justifier 1/20
aw.lf <- butter(1, 1/75, type="low")

#eleminates noise
data.AW.clean$respiration.lf <- signal::filter(aw.lf,data.AW.clean$respiration)
#elminates trends
data.AW.clean$respiration.hf <- signal::filter(aw.hf,data.AW.clean$respiration)

data.AW.clean$respiration.hlf <- signal::filter(aw.hf,as.numeric(data.AW.clean$respiration.lf))

library(pracma)
#recheche de max

#travaille sur un échantillon
offset <- 10000
sample.AW.clean <- data.AW.clean[offset + 0: (offset + 2001),]
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
max.peaks <- as.data.frame(findpeaks(as.numeric(data.AW.clean$respiration.hlf )))
max.peaks$x <- data.AW.clean$date[max.peaks[,2] ]
str(max.peaks)

ggplot(data.AW.clean, aes(y = respiration.lf, date)) + geom_line( color = "blue") +
 geom_line(aes(y = respiration.hlf), color = "red", alpha = .75) +
 geom_line(aes(y = respiration), color = "green", alpha = .75) +
 # geom_smooth(method="loess", formula = y ~ x,se=TRUE, size= 1, span=.1) +
 geom_smooth(aes(y = respiration.hlf), se=TRUE, formula= y ~ poly(x,5), colour="purple") +
 geom_smooth(se=TRUE, formula= y ~ poly(x,2), colour="orange", span=.1) +
 geom_point(data = max.peaks, aes(x = x, y = V1))



data.AW.clean$loess <- loess(data.AW.clean$respiration ~ as.numeric(data.AW.clean$date), degree=1,span=.1)$fitted
data.AW.clean$s.max.min <- signal.max.min(data.AW.clean$respiration, data.AW.clean$loess)
ggplot(data = data.AW.clean[0:10000,]) + geom_line(aes(x= date, y = respiration), color = "blue") +
  #geom_smooth(method="loess", formula = y ~ x, se=TRUE, size= 1, span=.2, color = "green")  +
  geom_line(aes(x = date, y = loess ), color = "green")  +
  geom_line(aes(x = date, y = s.max.min), color = "red") 


################FIN AW###################


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


############################################################
###################FIN RESPIRATION #########################
############################################################

#########A METTRE AILLEURS !!

######################
######BROUILLON#######
######################

df.apprenti <- df.all[df.all$nom.experience %in% c("AB","ST","DA","FS1","CW","AW","EZ1","GC1","IA"),]
df.apprenti.fort <- df.all[df.all$nom.experience %in% c("AB","AW","GC1","IA"),]


plot.evol.par.expe(df.apprenti,titre="effet d'\"apprentissage\"")
plot.evol.par.expe(df.apprenti.fort,titre="effet d'\"apprentissage\" fort")

df.comp.ampl <- df.all[df.all$nom.experience %in% c("AB","CW"),]
plot.evol.par.expe(df.comp.ampl,titre="comparaison de l'amplitude")

df.comp.ampl2 <- df.all[df.all$nom.experience %in% c("DE","CLP"),]
plot.evol.par.expe(df.comp.ampl2,titre="amplitude, autre exemple")

df.seuil <- df.all[df.all$nom.experience %in% c("AB","LM","CLP","DA","DA2","DA3"),]
plot.evol.par.expe(df.seuil,titre="Seuil bas")

df.seuil2 <- df.all[df.all$nom.experience %in% c("AB","CLP","DA","DA3"),]
plot.evol.par.expe(df.seuil2,titre="Seuil bas, exemples")

df.seuil3 <- df.all[df.all$nom.experience %in% c("DA3"),]
plot.evol.par.expe(df.seuil3[1:5000,],titre="Seuil bas en détail")

df.var.ampl <- df.all[df.all$nom.experience %in% c("AB","DE","HL"),]
plot.evol.par.expe(df.var.ampl,titre="Changement de l'amplitude ?")

df.var.aber <- df.all[df.all$nom.experience %in% c("FS1"),]
plot.evol.par.expe(df.var.aber,titre="Données aberrantes")

df.rupt.AB <- df.all[df.all$nom.experience %in% c("AB"),]
plot.evol.par.expe(df.rupt.AB[20000:30000,],titre="rupture, AB")
plot.mesure.direct(data.AB[20000:30000,])

plot.echantillon(data.AB)
plot.echantillon(data.AB[1:1000,])
chrono.plot.direct(data.AB,"respiration",taille=1000,names="AB")
chrono.plot.direct(data.CW,"respiration",taille=2500,names="CW")
chrono.plot.direct(data.CW,"respiration",taille=10000,names="IA",offset=20000)


lapply(list.exp.df,function(exp){chrono.plot(exp,"temperature")})

