
require(knitr)
require(plyr)
require(ggplot2)
require(reshape2)
require(grDevices)
require(kohonen)


mac.gd.path <- "/Users/godot/githubRepos/"
hp.gd.path <- "C:/Users/Godefroy/githubRepos/"


if(Sys.info()['nodename']=="THIM")gd.path <- hp.gd.path else
  gd.path <- mac.gd.path
data.path <- paste(gd.path,"affectiveComputing/data-brute/",sep = "/")

load.file <- function(filename){
  
  #chargement d'un fichier csv dans un data frame, le CSV a une ligne pour les noms de variable (header = TRUE), les variables des colonnes sont respectivement de type chaîne de caractère et numérique (x3) au chargement
  df <-read.csv2(file = filename, sep=";",header = TRUE,dec=",",colClasses=c("character", rep("numeric",4)),na=NA)
  
  #modification des noms de variables
  names(df)<-c("date","respiration", "activite.electrodermale", "temperature", "frequence.cardiaque")
  
  #mise au format data/time de la 1ère variable de mesure de l'écoulement du temps
  df$date<-strptime(df$date,format="%d/%m/%Y %H:%M:%OS")
  
  
  #ajout d'un variable catégorielles qui divise la durée de l'expérience en quatre quart temps
  #utile pour la vérification des données par la suite.
  df$quart.temps <- cut(1:nrow(df), breaks = 4, labels = c("1er","2eme","3eme","4eme"))
  
  #ajout d'une variable pour l'origine de l'expérience
  #en deux temps : 1) retirer ce qui est à droite du dernier "/" 2) on retire l'extension (".csv")
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

list.exp <- c("AB", "ST","DA", "LM", "FS1", "PCo", "PCo2", "PCo3", "CW","HL","CLP", "DE")


list.df <- lapply(list.exp,load.data)

df.all <- do.call("rbind", list.df)
df.all$nom.experience <- factor(df.all$nom.experience)


