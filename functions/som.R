###########################################################
######################## CREATE SOM MAP ###################
###########################################################

library(plyr)
library(ggplot2)
library(gtable)
library(kohonen)
library(tidyr)
library(signal)
library(dplyr)
theme_set(theme_bw(24))


############################################################
#######################LOAD & PREP #########################
############################################################


source("../general-functions/dplyr-functions.R")
source("../general-functions/load-functions.R")
source("../general-functions/clean-filters-functions.R")

mac.gd.path <- "/Users/godot/githubRepos/"
hp.gd.path <- "C:/Users/Godefroy/githubRepos/"


if(Sys.info()['nodename']=="THIM")gd.path <- hp.gd.path else
  gd.path <- mac.gd.path
data.path <- paste(gd.path,"affectiveComputing/data/",sep = "/")


#en argument : la liste des noms des fichiers csv à importer
load.data <- function(exp){
  filename <- paste(data.path,exp, "_SurEchantillonHF_header.csv", sep="")
  # print(filename)
  df.name <- paste("data_", exp, sep="")
  #permet de lancer la fonction load.file sur la liste passée en arguments
  assign(df.name, load.file(filename), envir = .GlobalEnv)
}

list.exp <- c("AB", "ST", "DA", "LM", "FS1", "PCo", "PCo2", "PCo3", "CW", 
              "HL", "CLP", "DE", "AW", "DA2", "DA3", "EZ1", "GC1", "IA")

#charge les datasets en mémoire
list.df <- lapply(list.exp,load.data)
df_all <- do.call(rbind, list.df)
df_all$nom.experience <- factor(df_all$nom.experience)

df_all <- fun.calc.duree(df_all)

nom.expe <- paste('data',list.exp[2],sep='.')
list.exp.df <- sapply(list.exp,function(exp){paste('data',exp,sep='.')})

###########################################################
###################FIN LOAD & PREPARE######################
###########################################################

###########################################################
######################## CLEAN ############################
###########################################################

#données manquante
l.na <- which(is.na(data_PCo),arr.ind = T)
ncol.1 <- l.na[1,2]
ncol.2 <- l.na[2,2]
data_PCo[l.na[1],ncol.1] <- data_PCo[l.na[1]-1,ncol.1]
data_PCo[l.na[2],ncol.2] <- data_PCo[l.na[2]-1,ncol.2]
if(any(is.na(data_PCo)))stop("still na data...")

#idem pour df_all
l.na <- which(is.na(df_all), arr.ind =  T)
ncol.1 <- l.na[1,2]
ncol.2 <- l.na[2,2]
df_all[l.na[1],ncol.1] <- df_all[l.na[1]-1,ncol.1]
df_all[l.na[2],ncol.2] <- df_all[l.na[2]-1,ncol.2]
#test
if(any(is.na(df_all)))stop("still na data...")

num.row<- which(df_all$temperature< 20)
#remplacer cette valeur par la valeur de la ligne au-dessus
df_all[num.row,"temperature"] <- df_all[num.row-1,"temperature"]

num.row<- which(data_DA$temperature <20)
data_DA[num.row,"temperature"] <-data_DA[num.row-1,"temperature"]

num.row<- which(df_all$nom.experience == 'FS1' & df_all$respiration > 0 )
#remplacer cette valeur par la valeur de la ligne au-dessus
df_all[num.row,"respiration"] <- df_all[num.row-1,"respiration"]

#obtenir l'index de la ligne de la donnée aberrant
num.row<- which(df_all$nom.experience == 'FS1' & df_all$respiration < -18)
#remplacer cette valeur par la valeur de la ligne au-dessus
df_all[num.row,"respiration"] <- df_all[num.row-1,"respiration"]

###################FIN CLEANING###################

df.selec <- df_all[!(df_all$nom.experience %in% c("FS1","LM","HL","ST")),]
list.expe.selec <- c("AB", "CLP", "CW", "DA", "PCo", "PCo2", "PCo3","DE")


###################2eme PARTIE CLEANING###################

#respiration
source("../general-functions/cleaning-functions.R")
spectrum.analysis(data_AB$respiration,data_AB$date)
spectrum.analysis(data_CLP$respiration,data_CLP$date)
spectrum.analysis(data_CW$respiration,data_CW$date)
spectrum.analysis(data_DA$respiration,data_DA$date)
#pb with PCo
spectrum.analysis(data_PCo$respiration,data_PCo$date)
spectrum.analysis(data_PCo2$respiration,data_PCo2$date)
spectrum.analysis(data_PCo3$respiration,data_PCo3$date)
spectrum.analysis(data_DE$respiration,data_DE$date)


data_AB$respi.clean.noise <- cleaning.high.freq(data_AB$respiration,1/10)
data_CLP$respi.clean.noise <- cleaning.high.freq(data_CLP$respiration,1/10)
data_CW$respi.clean.noise <- cleaning.high.freq(data_CW$respiration,1/10)
data_DA$respi.clean.noise <- cleaning.high.freq(data_DA$respiration,1/10)
data_PCo$respi.clean.noise <- cleaning.high.freq(data_PCo$respiration,1/10)
data_PCo2$respi.clean.noise <- cleaning.high.freq(data_PCo2$respiration,1/10)
data_PCo3$respi.clean.noise <- cleaning.high.freq(data_PCo3$respiration,1/10)
data_DE$respi.clean.noise <- cleaning.high.freq(data_DE$respiration,1/10)

ggplot(data_AB, aes(x= date)) + 
  geom_line(aes(y = respiration), col = "red") +
  geom_line(aes(y = respi.clean.noise), col = "green") 

#minmax signal
add_max_min_freq <- function(df) {
  df$loess <- loess(df$respi.clean.noise ~ as.numeric(df$date), degree=1,span=.1)$fitted
  df$min_max_sig <- min_max_signal(df$respi.clean.noise, df$loess)
  df$freq_sig <- min_max_sig2freq(df$min_max_sig)
  df
}
data_AB <- add_max_min_freq(data_AB)
data_CLP <- add_max_min_freq(data_CLP)
data_DA <- add_max_min_freq(data_DA)
data_CW <- add_max_min_freq(data_CW)
data_PCo <- add_max_min_freq(data_PCo)
data_PCo2 <- add_max_min_freq(data_PCo2)
data_PCo3 <- add_max_min_freq(data_PCo3)
data_DE <- add_max_min_freq(data_DE)

plot_signal <- function(df) {
  ggplot(data = df) + geom_line(aes(x= date, y = respi.clean.noise), color = "blue") +
    geom_smooth(method="loess", formula = y ~ x, se=TRUE, size= 1, span=.2, color = "green")  +
    geom_line(aes(x = date, y = loess ), color = "green")  +
    geom_line(aes(x = date, y = min_max_sig), color = "red") +
    geom_text(aes(x = date, y = min_max_sig, label= freq_sig), check_overlap = TRUE)
}
plot_signal(data_AB[1000:10000,])
plot_signal(data_CW[1000:10000,])
plot_signal(data_DE[1000:10000,])
plot_signal(data_DA[1000:10000,])
plot_signal(data_CLP[20000:30000,])
plot_signal(data_PCo[10000:20000,])
plot_signal(data_PCo2[1000:10000,])
plot_signal(data_PCo3[1000:10000,])

df <- rbind(data_AB,data_CLP,data_DA,data_CW,data_PCo,data_PCo2,data_PCo3,data_DE) 
df_4_som <- df[,c(3,4,5,7,8,10,11)]
#on retire nom experience
df_4_som_scaled <- scale(df_4_som[,-4])
#########SOM##############

set.seed(77)
#calcul de l'a cart'algorithme d'attiribution des données aux neurones
#rlen permet de préciser le nombre d'itérations
som1 <- som(data = df_4_som_scaled, grid = somgrid(30, 30, "hexagonal"), rlen=75)
par(xpd=F)
par(mai=  c(0, 0, 0, 0))
plot(som1, main = "", type="codes",labels = NULL) 

plot(som1, type="changes", main="carte du progrès d'apprentissage")

plot(som1, type="count", main= "carte de comptages des données captées")

plot(som1, type="quality", palette.name = coolBlueHotRed, main = "carte de \"qualité\" : distance moyenne des données aux neurones")

#TODO : heatmap par variable
plot(som1, type = "property", property = som1$codes[,1], main="carte heatmap de l'activité électrodermale", palette.name=coolBlueHotRed)
plot(som1, type = "property", property = som1$codes[,2], main="carte heatmap de temperature", palette.name=coolBlueHotRed)
plot(som1, type = "property", property = som1$codes[,3], main="carte heatmap de freq card", palette.name=coolBlueHotRed)
plot(som1, type = "property", property = som1$codes[,4], main="carte heatmap de respi nettoyée", palette.name=coolBlueHotRed)
plot(som1, type = "property", property = som1$codes[,5], main="carte heatmap de min max", palette.name=coolBlueHotRed)
plot(som1, type = "property", property = som1$codes[,6], main="carte heatmap de frequence", palette.name=coolBlueHotRed)



#créons une nvelle variable donnant le neurone associé à chaque données
#attention : certaines neurones n'ont pas de données associées dc ils n'apparaissent pas
df_4_som_scaled$nom.neurone <- som1$unit.classif
df_4_som_scaled$nom.experience <- df_4_som$nom.experience
#data frame du nombre d'individus par expe pour chaque neurone **non vide**
expe.par.neurone <- as.data.frame(with(df_4_som_scaled, 
                                       tapply(row.names(df_4_som),
                                              list(neurone=nom.neurone,experience=nom.experience), 
                                              length)))


#ajoutons les neurones manquants
#simplifier ??
list.neuro.vid <- which(! seq(1:900) %in% as.numeric(rownames(expe.par.neurone)))
nb.neuro.vid <- length(list.neuro.vid)
#df des neurones manquants
df.neuro.vid <- as.data.frame(matrix(nrow=nb.neuro.vid,ncol = ncol(expe.par.neurone)))
#prépa du rbind avec autre fdf
rownames(df.neuro.vid) <- list.neuro.vid
colnames(df.neuro.vid) <- colnames(expe.par.neurone)
#rbind & mis en ordre
expe.par.neurone <- rbind(expe.par.neurone,df.neuro.vid)
expe.par.neurone <- expe.par.neurone[order(as.numeric(row.names(expe.par.neurone))),]

                    
#expe.par.neurone

#supprimons les facteurs inutiles
expe.par.neurone <- expe.par.neurone[,list.expe.selec]
expe.par.neurone[is.na(expe.par.neurone)]<-0

#nom de l'experience la plus représentée par neurones
expe.major.par.neurone <- as.factor(list.expe.selec[max.col(expe.par.neurone)])
#put NA in line of neurone empty
expe.major.par.neurone[rowSums(expe.par.neurone)==0]<-NA
#test <- as.factor(rep(c("AB","CW","DA"),300))
#plot(som1, type = "property", property = as.numeric(test), main="expérience majoritaire par neurone")


#représentons cela graphiquement :
coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
plot(som1, type = "property", property = as.numeric(expe.major.par.neurone), main="",  heatkey =F,palette.name=coolBlueHotRed)
text(x=som1$grid$pts[,1],y=som1$grid$pts[,2],labels=expe.major.par.neurone,cex = .5,font=2)
