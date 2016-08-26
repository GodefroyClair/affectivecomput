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
library(purrr)
library(testthat)
library(stringr)
library(ggradar)
library(purrr)
library(reshape2)
#instal ggradar : devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
# configured to work on a Mac, change directory to Unix or Windows
#download.file("https://dl.dropboxusercontent.com/u/2364714/airbnb_ttf_fonts/Circular Air-Light 3.46.45 PM.ttf", "/Library/Fonts/Circular Air-Light 3.46.45 PM.ttf", method="curl")
#extrafont::font_import(pattern = 'Circular', prompt=FALSE)
suppressPackageStartupMessages(library(dplyr))
library(scales)
theme_set(theme_bw(24))

################################# MACRO ###################
################################# #########################

DEBUG <- FALSE
PRINT <- FALSE
############################################################
#######################LOAD & PREP #########################
############################################################


source("../general-functions/dplyr-functions.R")
source("../general-functions/load-functions.R")
source("../general-functions/cleaning-functions.R")
source("../general-functions/ggplot-functions.R")
source("../general-functions/clean-filters-functions.R")

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

###########################################################
###################FIN LOAD & PREPARE######################
###########################################################

###########################################################
######################## CLEAN ############################
###########################################################

#données manquante
#l.na <- which(is.na(data_PCo),arr.ind = T)
#ncol.1 <- l.na[1,2]
#ncol.2 <- l.na[2,2]
#data_PCo[l.na[1],ncol.1] <- data_PCo[l.na[1]-1,ncol.1]
#data_PCo[l.na[2],ncol.2] <- data_PCo[l.na[2]-1,ncol.2]
#if(any(is.na(data_PCo)))stop("still na data...")

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

#num.row<- which(data_DA$temperature <20)
#data_DA[num.row,"temperature"] <-data_DA[num.row-1,"temperature"]

num.row<- which(df_all$nom.experience == 'FS1' & df_all$respiration > 0 )
#remplacer cette valeur par la valeur de la ligne au-dessus
df_all[num.row,"respiration"] <- df_all[num.row-1,"respiration"]

#obtenir l'index de la ligne de la donnée aberrant
num.row<- which(df_all$nom.experience == 'FS1' & df_all$respiration < -18)
#remplacer cette valeur par la valeur de la ligne au-dessus
df_all[num.row,"respiration"] <- df_all[num.row-1,"respiration"]


###################FIN CLEANING###################


###################2eme PARTIE CLEANING###################

#getters of experience in a data frame
get_expe_base <- function(df){
  get_expe <- function(nom.exp){
    df %>% filter(nom.experience == nom.exp)
  }
}
get_expe <- get_expe_base(df_all)
list_exp <- levels(df_all$nom.experience) #redondant

#####################################
########### respiration #############
#####################################

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
      geom_line(col="red") + 
      geom_line(aes(y = respi_clean_noise1 - 3),col="green") +
      geom_line(aes(y = respi_clean_noise2 - 6),col="blue") +
      geom_line(aes(y = respi_clean_noise3 - 9),col="black")
    )
}

#insert df_all in high order function
#exemple to pick up AB pick_exp(df_all)("AB")
get_expe <- get_expe_base(df_all)
if(PRINT && DEBUG) plot_3_low_filter(get_expe("AB"), sample = 15000:35000)
if(PRINT) purrr::walk(list_exp, ~ plot_3_low_filter(get_expe(.), sample = 15000:35000))

#TODO
add_var_per_expe <- function(df, fun) {
}

#========== CLEAN HIGH FREQUENCIES ==============#
#we can do it all at one
df_all <- df_all %>% mutate(respi_clean_h = clean_high_freq(respiration,1/5))
#be carefull with the beginning and the ending
#Show all graphs
if(PRINT)walk(list_exp, ~ plot_clean_low_freq(get_expe(.)))

#========== SEPARATE LOW FREQUENCIES (trend) ==============#
get_expe <- get_expe_base(df_all)
df_all$respi_clean_hl <- 
  list_exp %>% map( ~ clean_low_freq(get_expe(.)$respi_clean_h, .01)) %>% flatten_dbl()
#be carefull with the beginning and the ending :
df_all$trend <- 
  list_exp %>% map( ~ clean_high_freq(get_expe(.)$respi_clean_h,.01)) %>% flatten_dbl()

if(DEBUG && PRINT) {
  df_all %>% filter(nom.experience == "AB") %>% ggplot(aes(x = date, y = respiration)) + geom_line(col = "purple")
  df_all %>% filter(nom.experience == "AB") %>% ggplot(aes(x = date, y = respi_clean_h)) + geom_line(col = "green")
  df_all %>% filter(nom.experience == "AB") %>% ggplot(aes(x = date, y = respi_clean_hl)) + geom_line(col = "red")
  df_all %>% filter(nom.experience == "AB") %>% ggplot(aes(x = date, y = trend)) + geom_line(col = "blue")
}

#=================== GARBAGE =====================#

#OLD 
#add_max_min_freq <- function(df) {
#  ##DEBUG df <- data_AB
#  df$loess <- loess(df$respi_clean_noise ~ as.numeric(df$date), degree=1,span=.1)$fitted
#  df$min_max_sig <- min_max_signal(df$respi_clean, df$loess)
#  df$freq_sig <- min_max_sig2freq(df$min_max_sig)
#  df
#}
#add_max_min_per_period <- function(df) {
#  #DEBUG 
#  #df <- get_expe("AB")
#  df$loess <- loess(df$respi_clean ~ as.numeric(df$date), degree=1,span=.1)$fitted
#  above <- df$respi_clean >= df$loess
#  period <- cumsum(c(0, diff(above) == 1))
#  df$max_per_period <- stat_per_period(df$respi_clean, period)(max)
#  df$min_per_period <- stat_per_period(df$respi_clean, period)(min)
#  df
#}
#get_expe <- get_expe_base(df_all)

##test
#df_test <- add_max_min_per_period(get_expe("AB"))
#df_test <- rbind(df_test, add_max_min_per_period(get_expe("AW")))
#df_test2 <- purrr::map_df(list("AB","AW"), ~ add_max_min_per_period(get_expe(.)))
#expect_true(all(df_test$max_per_period == df_test2$max_per_period))
#
#df_all <- purrr::map_df(list_exp, ~ add_max_min_per_period(get_expe(.)))
#

#========== ADD FREQUENCY STATS (max, min, mean...) ==============#
#high order function to create a signal based on a stat and a loess
add_stat_per_period <- function(stat) {
  function(df) {
    df$loess <- loess(df$respi_clean_hl ~ as.numeric(df$date), degree=1,span=.1)$fitted
    above <- df$respi_clean_hl >= df$loess # Is it ok ???
    period <- cumsum(c(0, diff(above) == 1))
    #extract fonction name from stat
    fun_name <- str_extract(deparse(stat), "(?<=\").*(?=\")")
    fun_name <- fun_name[!is.na(fun_name)]
    col_name <- paste(fun_name[1], "per_period", sep = "_")
 
    df[,col_name] <- stat_per_period(df$respi_clean_hl, period)(stat)
    df
  }
}

#test
if(DEBUG) {
  get_expe <- get_expe_base(df_all)
  add_mean_per_period <- add_stat_per_period(mean)
  add_max_per_period <- add_stat_per_period(max)
  add_min_per_period <- add_stat_per_period(min)
  test <- add_mean_per_period(get_expe("AB"))
  test <- rbind(test, add_mean_per_period(get_expe("AW")))
  test2 <- purrr::map_df(list("AB","AW"), ~ add_mean_per_period(get_expe(.)))
  expect_true(all(test$mean_per_period == test2$mean_per_period))
  
  df <- data.frame(date = rep(1:4000), respi_clean_hl = rep(c(2, 3, 2, 1, 0, -1, 0, 1),500))
  #lm <- loess(df$respi_clean_hl ~ as.numeric(df$date))
  #plot(df$date[1:100],df$respi_clean_hl[1:100], type = "l") + abline(0.003008, -0.0000015)
  expect_equal(mean(add_mean_per_period(df)$mean_per_period), 1)
  expect_lt(abs(mean(add_max_per_period(df)$max_per_period) -3 ), 5e-04)
  expect_lt(abs(mean(add_min_per_period(df)$min_per_period) +1) , 5e-04)
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

############## PREP SOM ###############

df_selec <- df_all[!(df_all$nom.experience %in% c("AW","DA2","DA3","EZ1","FS1","GS1","HL","IA","LM","ST")),]
df_selec2 <- df_all[!(df_all$nom.experience %in% c("FS1","LM","HL","ST")),]
list_expe_selec <- c("AB", "CLP", "CW", "DA", "DE", "PCo", "PCo2", "PCo3")
list_expe_selec2 <- c("AB", "CLP", "CW", "DA", "DE", "PCo", "PCo2", "PCo3")

#nom des variables sur lesquels nous allons travailler
#on retire nom experience
noms_var_4_som <- c("activite.electrodermale","temperature","frequence.cardiaque",
                    "nom.experience", "respi_clean_hl","trend", 
                    "loess","max_per_period","min_per_period","period_duration")


df_4_SOM <- as.data.frame(df_selec[,noms_var_4_som])
#2 scale : global or per individuals

#df_4_SOM_scaled_pr_exp <- as.data.frame(df_4_SOM) %>% group_by(nom.experience) %>% mutate_each(funs(scale))
df_4_SOM_scaled_per_exp <- df_4_SOM %>% group_by(nom.experience) %>% 
  do(mutate(.,
            nom.experience = nom.experience,
            activite.electrodermale = scale(activite.electrodermale),
            temperature = scale(temperature), 
            frequence.cardiaque = scale(frequence.cardiaque),
            respi_clean_hl = scale(respi_clean_hl),
            trend = scale(trend),
            loess = scale(loess),
            max_per_period = scale(max_per_period),
            min_per_period = scale(min_per_period),
            period_duration = scale(period_duration)))

df_4_SOM_scaled <- df_4_SOM %>% mutate_each(funs(scale), -c(nom.experience, loess)) 
df_4_SOM_scaled_per_exp <- as.data.frame(df_4_SOM_scaled_per_exp)

df_4_SOM_scale_scale <- df_4_SOM_scaled_per_exp %>% mutate_each(funs(round(scale(.),digits= 4)), -nom.experience) 
df_4_SOM_scale_scale <- as.data.frame(df_4_SOM_scale_scale)

#df_4_SOM_scale2_round <- df_4_SOM_scale_scale %>% mutate_each(funs(round(.,digits = 4)), -nom.experience)
#df_4_SOM_scale2_round <- as.data.frame(df_4_SOM_scale2_round)

if(DEBUG) {
  set.seed(7)
  df <- data.frame(x=10*runif(9), y=20*rnorm(9), group=rep(c("a","b","c"),3))
  df
  df_2 <- df %>% dplyr::group_by(group) %>% dplyr::mutate_each(funs(scale))
  df_2
  str(df_2)
  View(df_2)
}
#df_test <- data.frame(A = c(8, 8, 10, 6, 12), B =  c(10, 10, 14, 7, 8), C = c("one","two","two","one","two"))
#t1 <- df_test %>% group_by(C) %>% mutate_each(funs(scale)) 
#t2_1 <- df_test[df_test$C == "one",1:2] %>% scale 
#t2_2 <- df_test[df_test$C == "two",1:2] %>% scale
#testthat::expect_true(any(as.matrix(t2_1) == as.matrix(t1[df_test$C == "one",1:2])))
#testthat::expect_true(any(as.matrix(t2_2) == as.matrix(t1[df_test$C == "two",1:2])))


###TODO
########ACP ###########

############### K-MEANS #################

set.seed(77)
df_4_km_scaled <- df_4_SOM_scaled %>% select(-nom.experience, -loess)
km <- kmeans(df_4_km_scaled, 50, nstart = 100, iter.max = 150, algorithm="Lloyd")
df_4_km_scaled_per_exp <- df_4_SOM_scaled_per_exp %>% select(-nom.experience)
km_sc_pr_exp <- kmeans(df_4_km_scaled_per_exp, 50, nstart = 100, iter.max = 150, algorithm="Lloyd")
df_4_km_scale_scale <- df_4_SOM_scale_scale %>% select(-nom.experience)
km <- kmeans(df_4_km_scaled, 50, nstart = 100, iter.max = 150, algorithm="Lloyd")

set.seed(77)
# Initialise ratio_ss 
ratio_ss <- rep(0,10)
for (k in 20:30) {
  
# Apply k-means to school_result: school_km
km <- kmeans(df_4_SOM, nstart = 40, centers = k)
  
  # Save the ratio between of WSS to TSS in kth element of ratio_ss
  ratio_ss[k] <- km$tot.withinss /  km$totss
  
}

# Make a scree plot with type "b" and xlab "k"
plot( ratio_ss, type = "b", xlab = "k")

################ SOM ##################


#calcul de l'algorithme d'attiribution des données aux neurones
#rlen permet de préciser le nombre d'itérations
#temp à mettre au plus haut
set.seed(77)
df_SOM_scaled_exp_scaled <-  df_4_SOM_scaled_per_exp %>% dplyr::select(-nom.experience) %>% scale()
#round(df_SOM_scaled_p_exp_norm, digits = 4)

i <- 1:60
x <- 1:120
y <- c(rep(1,60),1/i)
som1 <- kohonen::som(data = df_SOM_scaled_exp_scaled, grid = somgrid(30, 30, "hexagonal"), rlen=180, alpha =c(2,0.0001))


  ##graph
str(som1)
ref.df <- data.frame(som1$codes)
df_SOM_scaled_p_exp$ref <- som1$unit.classif
data_frame(df_SOM_scaled_p_exp_norm)

ggplot(df_SOM_scaled_p_exp_norm, aes(x = ref, y = nom.experience )) %>% geom_bar()


set.seed(77)
som2 <- som(data = df_4_SOM_scaled_per_exp, grid = somgrid(30, 30, "hexagonal"), rlen=75)

set.seed(77)
som3 <- som(data = df_4_SOM_scale_scale, grid = somgrid(30, 30, "hexagonal"), rlen=75)

par(xpd=F)
par(mai=  c(0, 0, 0, 0))
#ajout d'itération et une descente de temp plus smooth
plot(som1, main = "", type="codes",labels = NULL) 

plot(som1, type="changes", main="carte du progrès d'apprentissage")

plot(som1, type="count", main= "carte de comptages des données captées")

#obtenir un dégradé de couleurs de bleu à rouge
coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
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
df_4_SOM_scaled <- df_SOM_scaled_p_exp_norm
df_4_SOM_scaled$nom.neurone <- som1$unit.classif
df_4_SOM_scaled$nom.experience <- df_selec$nom.experience
#data frame du nombre d'individus par expe pour chaque neurone **non vide**
expe.par.neurone <- as.data.frame(with(df_4_SOM_scaled, 
                                       tapply(row.names(df_selec),
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
expe.par.neurone <- expe.par.neurone[,list_expe_selec]
expe.par.neurone[is.na(expe.par.neurone)]<-0

#nom de l'experience la plus représentée par neurones
expe.major.par.neurone <- as.factor(list_expe_selec[max.col(expe.par.neurone)])
#put NA in line of neurone empty
expe.major.par.neurone[rowSums(expe.par.neurone)==0]<-NA
#test <- as.factor(rep(c("AB","CW","DA"),300))
#plot(som1, type = "property", property = as.numeric(test), main="expérience majoritaire par neurone")


#représentons cela graphiquement :
plot(som1, type = "property", property = as.numeric(expe.major.par.neurone), main="",  heatkey =F,palette.name=coolBlueHotRed)
text(x=som1$grid$pts[,1],y=som1$grid$pts[,2],labels=expe.major.par.neurone,cex = .5,font=2)

#moyenne des données captées par chaque référent
df_codes <- data.frame(som1$codes)
#gg <- ggplot(df.codes,) + geom_histogram() + coord_polar()

##Radar graph (extension ggplot) pour les codes de chaque référent

nb_group <- 20
test_radar <- df_codes %>% add_rownames(var = "group")  %>% 
  mutate_each(funs(rescale), -group)  %>% head(nb_group)
test_radar$group <- as.numeric(test_radar$group)

m_radar <- melt(test_radar, id = "group")
length(m_radar$group)

#draft
#ggplot(m_radar, aes(x = group, y = value, fill = variable)) %+% geom_bar(stat="identity",position="dodge")
my_theme <-  theme(panel.margin.x = unit(0, "lines"), panel.margin.y = unit(0, "lines"), panel.border = element_rect(colour = rgb(1.0, 0, 0, 0.5), fill=NA, size=1),
        axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        strip.background = element_blank(), strip.text.x = element_blank(),
        legend.title = element_text(colour="black", size=6,  face="bold"),
        legend.text = element_text(colour="black", size=5),
        legend.key = element_rect(size = 5))

base_rep <- 1:20
rep <- c("","","","","","","","")
lab_rep <- purrr::map(base_rep, function(ele) { c(ele, rep)}) %>% unlist()
lab_rep <- rep(c(i,"","","","","","","",""),20)
length(lab_rep)
ann_text <- data.frame(x=1.25, y=5, lab = as.character(lab_rep))

ggplot(m_radar, aes(x = variable, y = value, fill = variable)) %+% 
  facet_wrap( ~ group, ncol = 20)  %+% 
  geom_bar(stat="identity",position="dodge") %+%
  coord_polar(start = 0)  %+%
  geom_text(aes(x = 1.15, y = 2, label = lab_rep), colour="black",  inherit.aes=FALSE) %+%
  my_theme

ggplot(m_radar, aes(x = variable, y = value, fill = variable)) %+% 
  facet_wrap( ~ group, ncol = 30)  %+% 
  geom_bar(width = 1 ,stat="identity",position="dodge") %+%
  coord_polar()  %+% my_theme

df.codes[,1] - min(df.codes[,1])
rescale(df.codes[,1])
range(df_all$period_duration)
mtcars %>%
  add_rownames(var = "group" ) %>%
  mutate_each(funs(rescale), -group) %>%
  tail(4) %>% select(1:10) -> mtcars_radar

ggradar(mtcars_radar)

