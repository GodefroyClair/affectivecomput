
require(knitr)
require(plyr)
require(ggplot2)
require(ggvis)
require(reshape2)
require(grDevices)
require(kohonen)
require(zoo)
require(tidyr)
require(signal)
require(dplyr)

theme_set(theme_bw(24))

mac.gd.path <- "/Users/godot/githubRepos/"
hp.gd.path <- "C:/Users/Godefroy/githubRepos/"


if(Sys.info()['nodename']=="THIM")gd.path <- hp.gd.path else
  gd.path <- mac.gd.path
data.path <- paste(gd.path,"affectiveComputing/data/",sep = "/")

load.file <- function(filename){
  
  #chargement d'un fichier csv dans un data frame, le CSV a une ligne pour les noms de variable (header = TRUE), les variables des colonnes sont respectivement de type chaîne de caractère et numérique (x3) au chargement
  df <-read.csv2(file = filename, sep=";",header = TRUE,dec=",",colClasses=c("character", rep("numeric",4)),na=NA)
  
  #modification des noms de variables
  names(df)<-c("date","respiration", "activite.electrodermale", "temperature", "frequence.cardiaque")
  
  #mise au format data/time de la 1ère variable de mesure de l'écoulement du temps
  df$date<-strptime(df$date,format="%d/%m/%Y %H:%M:%OS")
  
  
  #ajout d'un variable catégorielles qui divise la durée de l'expérience en quatre quart temps #utile pour la vérification des données par la suite.
  df$quart.temps <- cut(1:nrow(df), breaks = 4, labels = c("1er","2eme","3eme","4eme"))
  
  #ajout d'une variable pour l'origine de l'expérience
  #en deux temps : 1) retirer ce qui est à droite du dernier / 2) on retire l'extension (.csv)
  filename <- gsub(".*/","",filename)
  df$nom.experience <- gsub("_.*", "",filename)
  
  return(df)
}

#en argument : la liste des noms des fichiers csv à importer
load.data <- function(exp){
  filename <- paste(data.path,exp, "_SurEchantillonHF_header.csv", sep="")
  # print(filename)
  
  #permet de lancer la fonction load.file sur la liste passée en arguments
  assign(paste("data.", exp, sep=""), load.file(filename), envir = .GlobalEnv)
  
}

list.exp <- c("AB", "ST", "DA", "LM", "FS1", "PCo", "PCo2", "PCo3", "CW", "HL", 
              "CLP", "DE", "AW", "DA2", "DA3", "EZ1", "GC1", "IA")


list.df <- lapply(list.exp,load.data)

df.all <- do.call(rbind, list.df)
df.all$nom.experience <- factor(df.all$nom.experience)

nom.expe <- paste('data',list.exp[2],sep='.')
list.exp.df <- sapply(list.exp,function(exp){paste('data',exp,sep='.')})

chrono.plot <- function(nom.expe,mesure){
  df <- eval(as.name(nom.expe))
  reg1 <- loess(df$respiration ~ as.numeric(df$date))
  df$resp.corr <- df$respiration - reg1$fitted 
  ggplot(data = df[,], aes(x = date, y= resp.corr)) +
    geom_line(size=.2) +
    xlab("temps (m:s)") + 
    ylab(mesure) + 
    ggtitle(paste("Expérience",nom.expe,sep = " ")) +
    theme(legend.title = element_text( size=4),axis.text.x = element_text(angle=90, vjust=1, size = 5))+
    #scale_y_continuous(limits=c(-17,-3)) +
    scale_x_datetime(date_labels = '%M:%S' , date_breaks= "15 sec") +
#    scale_x_datetime(date_labels = '%M:%S' , date_breaks= "30 sec", limit= c(debut, fin))
    geom_smooth(method="loess", formula = y ~ x,se=TRUE, size= 1) +
    stat_smooth(method="lm", se=TRUE, fill=NA,
                formula= y ~ poly(x, 10),colour="red")
}

chrono.plot.direct <- function(df,mesure,my.span=.01,poly=10){
  reg1 <- loess(df$respiration ~ as.numeric(df$date))
  reg1 <- lm(df$respiration ~ poly(as.numeric(df$date),9))
  df$resp.corr <- df$respiration - reg1$fitted 
  ggplot(data = df[,], aes(x = date, y= respiration)) +
    geom_line(size=.2) +
    xlab("temps (m:s)") + 
    ylab(mesure) + 
    ggtitle(paste("Expérience",deparse(substitute(data.AB)),sep = " ")) +
    theme(legend.title = element_text( size=4),axis.text.x = element_text(angle=90, vjust=1, size = 5))+
    #scale_y_continuous(limits=c(-17,-3)) +
    scale_x_datetime(date_labels = '%M:%S' , date_breaks= "15 sec") +
#    scale_x_datetime(date_labels = '%M:%S' , date_breaks= "30 sec", limit= c(debut, fin))
    geom_smooth(method="loess", formula = y ~ x,se=TRUE, size= 1, span=my.span) +
    geom_smooth(method="lm", se=TRUE, fill=NA,
                formula= y ~ poly(x,poly , raw=F ),colour="red")
}

#filter
lg <- 30000
bf <- butter(1,1/25, type="low")
b <- filter(bf,data.CW$respiration[1:lg])
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
require(changepoint)
seg=cpt.var(data.CW$respiration,method="PELT")
ggplot()
#test
lapply(c("data.CW"),function(exp){chrono.plot(exp,"respiration")})
lapply(list.exp.df,function(exp){chrono.plot(exp,"respiration")})



#respiration :

#IL faudrait pouvoir faire des comparaisons sur l'amplitude et la période

#AB:
#jusqu'à la moitié ajustement
#conjecture : plus la ceinture est serrée plus l'amplitude est grande ?
#a remis en place sa ceinture au milieu et à la fin
#fin : on atteint la limite (-18.67)

#ST:
#petit ajustement pendant 1/3 de l'expe
#amplitude semble comparable pdt toute l'expe

#DA:
#légère ajustement pendant toute l'expe
#atteint la limite à la fin (-18.6)

#LM:
#presque toujours en dessous des limites
#illisible

#FS1:
#valeurs abérantes
#plusieurs périodes à 0
#ajustement
#amplitude très variantes

#PCo:
#valeurs positives

#PCo2:
#valeurs autour de zéro

#PCo3: 
#valeurs autour de zéro

#CW:
#fort ajustement pendant le 1er tiers de l'expérience
#réajustement au milieu
#faible ajustement ensuite
#l'amplitude diminue légèrement à mesure que la ceinture se relâche

#HL: 
#valeurs + > 6
#grande variation
#conjecture : influence de niveau de serrement sur la variation ? 
#conjecture : ininterprétable si > à un seuil ?

#CLP: 
#ajustement pendant 1/3 de l'expe
#min en deça du seuil de reconnaissance (-18.67) ensuite

#DE: 
#valeurs positives entre 8 et 16
#légère variation de la moyenne
#amplitude légèrement décroissante

#AW: 
#valeur négatives entre -5 et -17
#périodes d'ajustement de 1/3
#quelques valeurs négatives (1358)
#9 périodes de 1 ou 2 secondes

#DA2:
#valeurs négatives entre -15 et -19
#min en deça du seuil (-18.6) pdt toute l'expé

#DA3:
#valeurs négatives entre -15 et -19
#min en deça du seuil (-18.6) pdt toute l'expé

#EZ1:
#valeurs entre 5 et -5
#ajustement pendant 2/3 de l'expe

#GC1:
#valeurs entre 12.5 et -1
#ajustement pendant 2/3 de l'expe
#réajustement brusque à la fin ?

#IA:
#valeurs entre 3 et -11
#ajustement pendant tout le 
#plusieurs réajustements brusques ?
#amplitude de plus en plus faible

#Conclusion :
#
#https://www.biopac.com/knowledge-base/respiration-recording/

#regression :
regTemp <- lm(data.CW$temperature ~ poly(1:length(data.CW$temperature)))

              #######################
              ######TEMPERATURE######
              #######################

lapply(list.exp.df,function(exp){chrono.plot(exp,"temperature")})
#AB:
#valeurs entre 19 et 33 !
#downscale:ok-upscale,

#ST:
#valeurs entre 26.2 et 27.2
#1/2d. descente progressive
#1/2f. montée assez brutale

#DA:
#valeurs entre 26.5 et 29
#montée forte puis progressive

#LM
#valeurs entre 22.9 et 23.05
#1/3f montée prog
#effet "code barre"

#FS1
#valeurs entre 21.5 et 22.5 
#1/3d : montée forte
#2/3f : baisse prog
#effet "code barre"

#PCo
#valeurs entre 22.2 et 23
#d : montée brutale
#descente prog
#moitié: chute
#PCo2:no-upscale;downscale, PCo3:ok-debut, CW:no-2deg, HL:ok, CLP:no-downslope, DE:no-scaleup;6deg, AW:ok-up, DA2:ok, DA3:ok?-downslope, EZ1:no-weird, GC1:ok, IA:no-range,peak?

#PCo2:
#valeurs entre  et 
#

#Pco3:
#valeurs entre  et 
#

#CW:
#valeurs entre  et 
#

#HL:
#valeurs entre  et 
#


#CLP:
#valeurs entre  et 
#


#conclusion :
#necessiter d'arrondir ?

lapply(list.exp.df,function(exp){chrono.plot(exp,"activite.electrodermale")})
#AB:ok-highpeak!!, ST:no:flat, DA:ok, LM:ok-upsacle, FS1:ok-upscale, PCo:ok-upslope, PCo2:ok-peaks, PCo3:ok-peaks, CW:ok-debut,downslope?,HL:no-flat,CLP:ok-peaks, DE:ok-debut?, AW:ok, DA2:ok?-downslope, DA3:ok, EZ1:ok-peak?,GC1:ok?,IA:?
lapply(list.exp.df,function(exp){chrono.plot(exp,"frequence.cardiaque")})
#AB:ok, ST:ok,DA:ok-debut?, LM:ok, FS1:ok, PCo:ok, PCo2:ok, PCo3:ok, CW:ok,HL:ok,CLP:ok, DE:ok-début?, AW:ok-zeros, DA2:ok, DA3:ok, EZ1:ok,GC1:ok-zero,IA:ok



#moyenne mobile

filter(data.IA, c(1/3, 1/3, 1/3))

mtcars %>% 
  ggvis(~mpg) %>% 
  input_numeric(value=1,label = "Choose a binwidth:") %>% 
  layer_histograms(width = 2)

library(babynames)
hadley <- dplyr::filter(babynames, name == "Hadley")
ggplot(hadley, aes(year, n,group =year)) +
  geom_line()


ggplot(mpg, aes(drv)) +
  geom_bar(colour="white")

ggplot(mpg, aes(drv, fill = hwy, group = hwy)) +
  geom_bar(colour="white")

mpg2 <- mpg %>% arrange(hwy) %>% mutate(id = seq_along(hwy))
ggplot(mpg2, aes(drv, fill = hwy, group = id)) +
  geom_bar(colour="white")

ggplot(mpg, aes(cyl, hwy,group=cyl)) +
  geom_boxplot()

ggplot(mpg, aes(as.factor(round(displ)), cty,group= as.factor(round(displ)))) +
  geom_boxplot()

df <- data.frame(x = 1:4, y = c(2,0,4,7), colour = c(1,3,5,6))
ggplot(df, aes(x, y, colour = factor(colour))) +
  geom_line(aes(group =c(2,0,0,2)), size = 2) +
  geom_point(size = 5)
ggplot(df, aes(x, y, colour = colour)) +
  geom_line(aes(group =161), size = 4) +
  geom_point(size = 5)
print(BankFractal(4))
