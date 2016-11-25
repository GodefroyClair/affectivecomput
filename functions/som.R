###########################################################
######################## CAP & SOM MAP ###################
###########################################################
# to customize the color scheme, one can use the scale_fill_gradientn() function 
# in combination with brewer.pal() from the RColorBrewer package. 
# (RColorBrewer provides some nice color schemes that would be time-consuming to  reproduce manually. 
# You will use mostly red colors here, but you can type display.brewer.all() in the R console to see the available color palattes.)


library(devtools)
library(knitr)
#stat
library(kohonen)
library(signal)
#Whickham
library(stringr)
library(purrr)
library(tidyr)
library(dplyr)
library(tibble)
library(tibble)
suppressPackageStartupMessages(library(dplyr))
library(lubridate)
library(testthat)
#GGplot & data viz
library(ggplot2)
#library(ggbiplot)
library(ggradar)
library(ggbagplot)
library(grid)
#other struc
library(data.table)
library(gtable)
#?
library(grDevices)
library(reshape2)
library(scales)
#instal ggradar : devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
# configured to work on a Mac, change directory to Unix or Windows
#download.file("https://dl.dropboxusercontent.com/u/2364714/airbnb_ttf_fonts/Circular Air-Light 3.46.45 PM.ttf", "/Library/Fonts/Circular Air-Light 3.46.45 PM.ttf", method="curl")
#extrafont::font_import(pattern = 'Circular', prompt=FALSE)
theme_set(theme_bw(24))

################################# MACRO ###################
################################# #########################

if (!exists("DEBUG")) DEBUG <- FALSE
if (!exists("PRINT")) PRINT <- FALSE


############################################################
#######################LOAD & PREP #########################
############################################################

#general helpers
source("../RGeneralFunctions/dplyr-functions.R")
source("../RGeneralFunctions/load-functions.R")
source("../RGeneralFunctions/cleaning-functions.R")
source("../RGeneralFunctions/ggplot-functions.R")
source("../RGeneralFunctions/clean-filters-functions.R")
#affect comput related
source("./functions/load_df-all.R")
source(file = "./functions/kohonen_graph.R") #for plot_ho

df_all <- import_df_all()
df_all <- as_data_frame(df_all)

###########################################################
###################FIN LOAD & PREPARE######################
###########################################################


###########################################################
################ ASSEMBLE CLEAN DATA ######################
###########################################################


source("./functions/add_variables.R")
df_all <- transpi_add_variables(df_all)
df_all <- respi_add_variables(df_all)
df_all <- freqcard_add_variables(df_all)
df_all <- temp_add_variables(df_all)
df_all <- as_data_frame(df_all)

#df_transpi <- load("./data/df_transpi.RDa")
#df_temp <- load("./data/df_temp.RDa")
#df_freq_card <- load("./data/df_freqcard.RDa")
#df_resp <- load("./data/df_resp.RDa")

df_half_1 <- df_all[df_all$nom.experience %in% list_half_1,]
df_half_2 <- df_all[df_all$nom.experience %in% list_half_2,]

################### EXOGENEOUS VARIABLES ############

## AR 
#df_all$state <- NA
#df_all %>% filter()
# #table(round(df_all$tps.ecoule))
# df_all %>% dplyr::group_by(nom.experience) %>% dplyr::select(tps.ecoule) %>% dplyr::summarise(max(tps.ecoule))
# expe_type1 <- df_all %>% group_by(nom.experience) %>% filter(max(tps.ecoule) > 1500 & max(tps.ecoule) < 1600)
# nom_expe_type1 <- unique(as.character(expe_type1$nom.experience))
# 
# library(xts)
# library(timeSeries)
# library(difftime)
# x <- timeSeries(1:10, 1:10)
# df_all$evol <- NA
# 
# df_all %>% filter(nom.experience %in% nom_expe_type1) <- "neutre"
# data_chg_etat <- c("3:06-3:06", "3:33-3:33", "4:16")
# #df_all %>% filter(nom.experience %in% nom_expe_type1 & tps.ecoule ) 

############## PREP FOR KOHONEN MAP ###############
list_expe <- levels(df_all$nom.experience)
list_expe_sml <- list_expe[(!list_expe %in% c("AW","DA2","DA3","EZ1","FS1","GC1","HL","IA","LM","ST"))]
list_expe_lrg <- list_expe[(!list_expe %in% c("FS1","LM","HL","ST"))]

#choose between the 2 list of experiences : 
#list_expe_selec <- list_expe_sml
list_expe_selec <- list_expe_lrg
df_selec <- df_all[df_all$nom.experience %in% list_expe_selec,]

#nom des variables sur lesquelles seront appliquées les algo statistique
#on retire nom experience
noms_var_stat <- c("activite.electrodermale","temperature","frequence.cardiaque",
                   "nom.experience", "respi_clean_hl","respi_trend", 
                   "loess","max_per_period","min_per_period","period_duration")
#larger list
noms_var_stat <- c("nom.experience",
                   "elecderm_clean", "elecderm_diff", 
                   "temp_clean", "temp_diff",
                   "freqcard_clean", "freqcard_diff",
                   "respi_clean", "respi_clean_hl", "respi_trend", "loess", 
                   "max_per_period","min_per_period","period_duration")
df_stat <- as_data_frame(df_selec[,noms_var_stat])

#2 scale : global or per individuals

#scale the var by experiences 
#!!bug with dplyr::group_by() + dplyr::mutate_each() + scale() : https://github.com/hadley/dplyr/issues/2049
#need to create my own scale function
my_scale <- function(x, scale = T) c(scale(x, scale = scale))

df_stat_scaled_per_exp <- df_stat %>% group_by(nom.experience) %>% mutate_each(funs(my_scale)) %>% ungroup 

df_stat_centered_per_exp <- df_stat %>% group_by(nom.experience) %>% mutate_each(funs(my_scale(., scale = F))) %>% ungroup

#simple (general) scale
df_stat_scaled <- df_stat %>% mutate_each(funs(scale), -c(nom.experience, loess)) 

df_stat_scale_scale <- df_stat_scaled_per_exp %>% mutate_each(funs(my_scale(.)), -nom.experience) 
df_stat_scale_scale2 <- df_stat_scaled_per_exp2 %>% mutate_each(funs(my_scale(.)), -nom.experience) 
#df_stat_scale_scale <- data.frame(df_stat_scale_scale)

df_stat_centered_scale <- df_stat_centered_per_exp %>% mutate_each(funs(my_scale(., scale = F)), -nom.experience) 
df_stat_centered_scale2 <- df_stat_centered_per_exp2 %>% mutate_each(funs(my_scale(., scale = F)), -nom.experience) 

df_stat_scaled <- df_stat_scaled %>% bind_cols(df_selec %>% dplyr::select(tps.ecoule))
df_stat_scaled2 <- df_stat_scaled2 %>% bind_cols(df_selec2 %>% dplyr::select(tps.ecoule))

if(DEBUG) {
  ## test solution 4 bug
  set.seed(7)
  df <- data.frame(x=10*runif(9), y=20*rnorm(9), group=rep(c("a","b","c"),3))
  
  #show the bug :
  df_2 <- df %>% dplyr::group_by(group) %>% dplyr::mutate_each(funs(scale))
  expect_that(dim(df2[[2]]), equals(c(ncol(df),1))) #dim should be NULL !!
  
  #solution : use a fonction that coerse the result of scale to vector
  my_scale_test <- function(x, ...) c(scale(x, ...))
  df_3 <- df %>% dplyr::group_by(group) %>% dplyr::mutate_each(funs(my_scale_test))
  expect_that(dim(df3[[2]]), equals(NULL)) #should be 
  # old solution :
  # df_2 <- data.frame(df_2[1:nrow(df_2),])
  
  df_4 <- df %>% dplyr::group_by(group) %>% dplyr::mutate_each(funs(my_scale_test(.,scale = F)))
  expect_equal(df_4[df$group == "a",]$x, df$x[df$group == "a"] - mean(df$x[df$group == "a"]))
}
#df_test <- data.frame(A = c(8, 8, 10, 6, 12), B =  c(10, 10, 14, 7, 8), C = c("one","two","two","one","two"))
#t1 <- df_test %>% group_by(C) %>% mutate_each(funs(scale)) 
#t2_1 <- df_test[df_test$C == "one",1:2] %>% scale 
#t2_2 <- df_test[df_test$C == "two",1:2] %>% scale
#testthat::expect_true(any(as.matrix(t2_1) == as.matrix(t1[df_test$C == "one",1:2])))
#testthat::expect_true(any(as.matrix(t2_2) == as.matrix(t1[df_test$C == "two",1:2])))

########################
######## PCA ###########
########################
# PCA for respiration

PCAbiplot <- function(PC, x="PC1", y="PC2", sample = NA, seed = 7) {
  #theme that are used for the multiradar plot
  pca_theme <-  theme( axis.title.x=element_text(size = 6), axis.text.x=element_text(size = 5),
                       axis.title.y=element_text(size = 6), axis.text.y=element_text(size = 5))
  
  # PC being a prcomp objecto
  #PC <- pca_respi ##DEBUG##
  df <- data.frame(row.names = 1:nrow(PC$x), PC$x)
  #plot_data <- compute.bagplot(x = PC$x, y = PC$y)
  gg <- ggplot(df, aes(x=PC1, y=PC2)) + stat_bag(alpha=.2, col = "blue", fill = "orange")
  #gg <- ggplot(df, aes(x=PC1, y=PC2)) + geom_text(alpha=.4, size=3, aes(label=obsnames))
  #gg <- ggplot(df, aes(x=PC1, y=PC2)) + geom_point(alpha=.2, size=.5, col= "blue")
  gg <- gg + geom_hline(aes(yintercept = 0), size=.2) + geom_vline(aes(xintercept = 0), size=.2)
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min(
    (max(df[,"PC2"]) - min(df[,"PC2"]) / (max(datapc[,"PC2"]) - min(datapc[,"PC2"]))),
    (max(df[,"PC1"]) - min(df[,"PC1"]) / (max(datapc[,"PC1"]) - min(datapc[,"PC1"])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get("PC1")),
                      v2 = .7 * mult * (get("PC2"))
  )
  
  gg <- gg +
    geom_text(data = datapc, aes(x = datapc$v1, y = datapc$v2, label = varnames), 
              size = 2, vjust = 1, color = "black")
  gg <- gg + geom_segment(data=datapc, aes(x = 0, y = 0, xend = v1, yend = v2), 
                          arrow = arrow(length = unit(0.2,"cm")), alpha=0.75, color="Magenta")
  gg + pca_theme
  
}

# PCA for all variables
df_pca <- df_stat %>% ungroup %>% dplyr::select(-nom.experience, -loess)
df_pca2 <- df_stat2 %>% ungroup %>% dplyr::select(-nom.experience, -loess)
pca <- prcomp( ~ ., data = df_pca, center = T, scale = T, na.action = na.omit)
pca2 <- prcomp( ~ ., data = df_pca2, center = T, scale = T, na.action = na.omit)
pca_rot <- as_data_frame(pca$rotation)
pca_rot2 <- as_data_frame(pca2$rotation)
dim(pca$x)
dim(pca2$x)
#library(ade4)
#pca2 <- nipals(df_pca)
str(df_pca)
str(pca)

# like plot(pca, type = "l") but better ;)
pca_plot_var <- function(pca){
  var_by_axis <- data_frame(share_var_total = pca$sdev^2/sum(pca$sdev^2), pca_axis = as.factor(1:length(pca$sdev)))
  ggplot(var_by_axis, aes(x = pca_axis, y = share_var_total, group = 1)) + 
    geom_line(col =" grey") + geom_point(col = "blue") +
    xlab("axis rank") +
    ylab("share of total variance") +
    ggtitle("Principal component analyis : variance per axis") +
    scale_y_continuous(labels = percent) +
    theme_classic()
}
pca_plot_var(pca)

PCAbiplot(pca)

pca_respi <- prcomp( ~ respi_clean + respi_clean_hl + respi_trend + max_per_period + min_per_period +
                       period_duration, data = df_pca, center = T, scale = T, na.action = na.omit)
# plot(pca_respi, type = "l")
pca_plot_var(pca_respi)
#PCAbiplot(pca_respi)

pca_transpi <- prcomp( ~ elecderm_clean + elecderm_diff, data = df_pca, center = T, scale = T, na.action = na.omit)
plot(pca_transpi, type = "l")
PCAbiplot(pca_transpi)

pca_temp <- prcomp( ~ temp_clean + temp_diff, data = df_pca, center = T, scale = T, na.action = na.omit)
plot(pca_transpi, type = "l")
PCAbiplot(pca_temp)

pca_freqcard <- prcomp( ~ freqcard_clean + freqcard_diff, data = df_pca, center = T, scale = T, na.action = na.omit)
plot(pca_freqcard, type = "l")
PCAbiplot(pca_freqcard)

#get the percent of variance of eigen value 1 compared to eigen value 2
share_eigenv1 = pca$sdev[1]^2 / sum(pca$sdev[1:2]^2)
share_eigenv2 = pca$sdev[2]^2 / sum(pca$sdev[1:2]^2)




# Predict PCs
#predict(pca,  newdata=tail(df_scale_scale, 2))

#require(graphics)

#install_github("ggbiplot", "vqv")

#ggscreplot(pca, type = c("pev", "cev"))
# 
# ggbiplot(pca, obs.scale = 1, var.scale = 1, 
#          ellipse = TRUE, 
#          circle = TRUE) +
#   scale_color_discrete(name = '') + 
#   theme(legend.direction = 'horizontal', 
#         legend.position = 'top')
# ggbiplot(pca, obs.scale = 1, var.scale = 1,
#          groups = df_stat_scale_scale$nom.experience, ellipse = TRUE, circle = TRUE) +
#   scale_color_discrete(name = '') +
#   theme(legend.direction = 'horizontal', legend.position = 'top')

#-------------------------------------------#
#--- Eliminate too extreme observations  ---#
#-------------------------------------------#

df_rm_var <- df_stat_scaled %>% ungroup %>% dplyr::select(-nom.experience, -loess)
stat_df <- df_rm_var %>% purrr::map(~ c(mean(., na.rm = T), sd(., na.rm = T)))

dist_deal_na <- function(vec) {
  sqrt(sum(vec^2, na.rm = T))
}
df_rm_var <- df_rm_var %>% purrr::by_row(..f = dist_deal_na, 
                                         .collate = "cols", .to = "dist")

df_stat_scaled <- dplyr::bind_cols(df_stat_scaled, dplyr::select(df_rm_var, dist))

View(df_stat_scaled)


if(DEBUG) {
  test <- df_stat_scaled %>% select(c(2:10,12:14))
  expect_equal(dist_deal_na(test[1,]), df_stat_scaled$dist[1])
  expect_equal(dist_deal_na(test[2,]), df_stat_scaled$dist[2])
  expect_equal(dist_deal_na(test[2000,]), df_stat_scaled$dist[2000])
  set.seed(77)
  nb <- round(runif(n =1, min = 1, max = nrow(test)))
  expect_equal(dist_deal_na(test[nb,]), df_stat_scaled$dist[nb])
}

#ggplot(df_stat_scaled) %+% geom_jitter(aes(x = 1, y = dist, col = nom.experience), alpha = .5)
ggplot(df_stat_scaled) %+% geom_boxplot(aes(x = 1, y = dist, col = nom.experience), alpha = .5)
ggplot(df_stat_scaled) %+% geom_boxplot(aes(x = 1, y = dist, col = tps.ecoule), alpha = .5)

summary(df_rm_var$dist > 6)
df_rm_var$dist_sup_6 <- df_rm_var$dist > 6

#=======================================#
############### K-MEANS #################
#=======================================#

set.seed(77)
df_km_scaled <- df_stat_scaled %>% select(-nom.experience, -loess)
km <- kmeans(df_km_scaled, 50, nstart = 100, iter.max = 150, algorithm="Lloyd")
df_km_scaled_per_exp <- df_stat_scaled_per_exp %>% select(-nom.experience)
km_sc_pr_exp <- kmeans(df_km_scaled_per_exp, 50, nstart = 100, iter.max = 150, algorithm="Lloyd")
df_km_scale_scale <- df_stat_scale_scale %>% select(-nom.experience)
km <- kmeans(df_km_scaled, 50, nstart = 100, iter.max = 150, algorithm="Lloyd")

set.seed(77)
# Initialise ratio_ss 
ratio_ss <- rep(0,10)
for (k in 20:30) {
  
  # Apply k-means to school_result: school_km
  km <- kmeans(df_stat, nstart = 40, centers = k)
  
  # Save the ratio between of WSS to TSS in kth element of ratio_ss
  ratio_ss[k] <- km$tot.withinss /  km$totss
  
}

# Make a scree plot with type "b" and xlab "k"
plot( ratio_ss, type = "b", xlab = "k")


#=====================================#
################ SOM ##################
#=====================================#


#calcul de l'algorithme d'attiribution des données aux neurones
#rlen permet de préciser le nombre d'itérations
#temp à mettre au plus haut
set.seed(77)
df_SOM_scale_scale <-  df_stat_scaled_per_exp %>% ungroup %>% dplyr::select(-nom.experience, -loess)
df_SOM_scale_scale_new <-  df_stat_scaled_per_exp_new %>% ungroup %>%  dplyr::select(-nom.experience, -loess)
df_SOM_scaled <-  df_stat_scaled %>% dplyr::select(-nom.experience, -loess)
df_SOM_scaled_new <-  df_stat_scaled_new %>% dplyr::select(-nom.experience, -loess)
df_SOM_center_scale <-  df_stat_centered_scale %>% ungroup %>% dplyr::select(-nom.experience, -loess)
df_SOM_center_scale_new <-  df_stat_centered_scale_new %>% ungroup %>% dplyr::select(-nom.experience, -loess)

i <- 1:60
x <- 1:120
y <- c(rep(1,60),1/i)
nhbrdist <- unit.distances(som1$grid, som1$toroidal) #

som1 <- kohonen::som(data = as.matrix(df_SOM_scale_scale), grid = somgrid(30, 30, "hexagonal"),
                     rlen=300, alpha =c(2,0.0001), radius = quantile(nhbrdist, 0.9) * c(1, -1))
#som2 <- kohonen::som(data = as.matrix(df_SOM_scaled), grid = somgrid(30, 30, "hexagonal"), rlen=180, alpha =c(2,0.0001))
som3 <- kohonen::som(data = as.matrix(df_SOM_center_scale), grid = somgrid(30, 30, "hexagonal"), rlen=180, alpha =c(2,0.0001))


som_10_sc_sc <- kohonen::supersom(data = list(mat = as.matrix(df_SOM_scale_scale_new)),
                                  grid = somgrid(10, 10, "hexagonal"),
                                  rlen = 200, alpha = c(2,0.0001),
                                  maxNA.fraction = .5,
                                  keep.data = F,
                                  contin = T,
                                  whatmap = 1)

som_10_avg_sc <- kohonen::supersom(data = list(mat = as.matrix(df_SOM_center_scale_new)),
                                   grid = somgrid(10, 10, "hexagonal"),
                                   rlen = 200, alpha = c(2,0.0001),
                                   maxNA.fraction = .5,
                                   keep.data = F,
                                   contin = T,
                                   whatmap = 1)

som_short_new <- kohonen::supersom(data = list(mat = as.matrix(df_SOM_scale_scale_new)),
                                   grid = somgrid(10, 10, "hexagonal"),
                                   rlen = 200, alpha = c(2,0.0001),
                                   maxNA.fraction = .5,
                                   keep.data = F,
                                   contin = T,
                                   whatmap = 1)

som_mid <- kohonen::supersom(data = list(mat = as.matrix(df_SOM_scale_scale)),
                             grid = somgrid(20, 20, "hexagonal"),
                             rlen = 200, alpha = c(1,0.0001),
                             maxNA.fraction = .5,
                             keep.data = F,
                             contin = T,
                             whatmap = 1)

som_mid_new <- kohonen::supersom(data = list(mat = as.matrix(df_SOM_scale_scale_new)),
                                 grid = somgrid(20, 20, "hexagonal"),
                                 rlen = 200, alpha = c(1,0.0001),
                                 maxNA.fraction = .5,
                                 keep.data = F,
                                 contin = T,
                                 whatmap = 1)
##Save main data for graphs (for markdown report)
save(som1,file = "./data/som1.RDa")
#save(som2,file = "./data/som2.RDa")
save(som3,file = "./data/som3.RDa")
save(df_stat_scale_scale, file = "./data/df_stat_scale_scale.RDa")
save(df_stat_scale_scale, file = "./data/df_stat_center_scale.RDa")
save(df_stat_scaled, file = "./data/df_stat_scaled.RDa")
save(df_selec, file = "./data/df_selec.RDa")


##GRAPHICS

str(som)
ref.df <- data.frame(som$codes)
df_SOM_scale_scale$ref <- som$unit.classif
df_SOM_scale_scale$nom.experience <- df_selec$nom.experience



par(xpd=F)
par(mai=  c(0, 0, 0, 0))
#ajout d'itération et une descente de temp plus smooth
plot(som, main = "", type="codes",labels = NULL) 

plot(som, type="changes", main="carte du progrès d'apprentissage")

plot(som, type="count", main= "carte de comptages des données captées")

#obtenir un dégradé de couleurs de bleu à rouge
coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
plot(som, type="quality", palette.name = coolBlueHotRed, main = "carte de \"qualité\" : distance moyenne des données aux neurones")

#TODO : heatmap par variable
plot(som, type = "property", property = som$codes[,1], main="carte heatmap de l'activité électrodermale", palette.name=coolBlueHotRed)
plot(som, type = "property", property = som$codes[,2], main="carte heatmap de temperature", palette.name=coolBlueHotRed)
plot(som, type = "property", property = som$codes[,3], main="carte heatmap de freq card", palette.name=coolBlueHotRed)
plot(som, type = "property", property = som$codes[,4], main="carte heatmap de respi nettoyée", palette.name=coolBlueHotRed)
plot(som, type = "property", property = som$codes[,5], main="carte heatmap du trend de respi", palette.name=coolBlueHotRed)
plot(som, type = "property", property = som$codes[,6], main="carte heatmap de max par periode", palette.name=coolBlueHotRed)
plot(som, type = "property", property = som$codes[,7], main="carte heatmap de min par periode", palette.name=coolBlueHotRed)
plot(som, type = "property", property = som$codes[,8], main="carte heatmap de frequence", palette.name=coolBlueHotRed)


####################### NEW GRAPH WITH GGPLOT #########################


#==========================================================================#
## Multi-radar graph (extension ggplot) pour les codes de chaque référent ##
#==========================================================================#

## reference :
## ggradar
#mtcars %>%
#  add_rownames(var = "group" ) %>%
#  mutate_each(funs(rescale), -group) %>%
#  tail(4) %>% select(1:10) -> mtcars_radar
#ggradar(mtcars_radar)

#limit the nb of neurones we are working on to fasten the graph making process
#nb_group has to be 300 in the end
nb_group <- 100
nb_var <- 8
#make a df with the value of the variables associated with each neurone and the id of the neurone (group)
#it is easier to rescale everythng to ratio ([0-1]) with rescale
df_codes <- data.frame(som$codes) %>% add_rownames(var = "group")  %>% 
  mutate_each(funs(rescale), -group)  %>% head(nb_group)
#group needs to be numeric
df_codes$group <- as.numeric(df_codes$group)
# to be able to use geom_bar we need to untidy the data
melted_radar <- melt(df_codes, id = "group")

#we build the label such that each facetted graph has a number associated to it
##!! this is a patch, there must be a way to do this BETTER !!
base_rep <- 1:nb_group
rep <- rep("", nb_var - 1)
lab_rep <- purrr::map(base_rep, function(ele) { c(ele, rep)}) %>% unlist()
#ann_text <- data.frame(x=1.25, y=5, lab = as.character(lab_rep))

#titre des graphiques
base_titre <- "Carte d'identité des référents"

#theme that are used for the multiradar plot
radar_theme <-  theme(panel.margin.x = unit(0, "lines"), panel.margin.y = unit(0, "lines"), panel.border = element_rect(colour = rgb(1.0, 0, 0, 0.5), fill=NA, size=1),
                      axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
                      axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                      strip.background = element_blank(), strip.text.x = element_blank(),
                      legend.title = element_text(colour="black", size=5,  face="bold"),
                      legend.text = element_text(colour="black", size=5),
                      legend.key = element_rect(size = 1),
                      title = element_text(color = "blue", size =6, face = "bold"))

#length
ggplot(melted_radar, aes(x = variable, y = value, fill = variable)) %+% 
  facet_wrap( ~ group, ncol = 30)  %+% 
  geom_bar(stat="identity",position="dodge") %+%
  ylim(0,1) %+%
  geom_text(aes(x = 1.5, y = 1, label = lab_rep), colour="black", size = 1, inherit.aes=F) %+%
  coord_polar(start = 0)  %+%
  radar_theme %+%
  ggtitle(paste(base_titre, "(longueur)"))

#area
ggplot(melted_radar, aes(x = variable, y = value, fill = variable)) %+% 
  facet_wrap( ~ group, ncol = 30)  %+% 
  ylim(0,1) %+%
  geom_bar(width = 1 ,stat="identity",position="dodge") %+%
  geom_text(aes(x = 1.5, y = 1, label = lab_rep), colour="black",  size = 1, inherit.aes=F) %+%
  coord_polar()  %+% 
  radar_theme %+%
  ggtitle(paste(base_titre, "(aire)"))

#radar
ggplot(melted_radar, aes(x = variable, y = value, fill = variable)) %+% 
  facet_wrap( ~ group, ncol = 30)  %+% 
  geom_bar(stat="identity",position="dodge",width=.2) %+%
  ylim(0,1) %+%
  coord_polar(start = 0)  %+%
  geom_text(aes(x = 1.5, y = 1, label = lab_rep), colour="black", size = 1, inherit.aes=F) %+%
  radar_theme %+%
  ggtitle(paste(base_titre, "(radar)"))

#bar
ggplot(melted_radar, aes(x = variable, y = value, fill = variable)) %+% 
  facet_wrap( ~ group, ncol = 30)  %+% 
  geom_bar(stat="identity",position="dodge",width=.5) %+%
  ylim(0,1) %+%
  geom_text(aes(x = 2.5, y = 1.1, label = lab_rep), colour="black", size = 2, inherit.aes=F) %+%
  radar_theme %+%
  ggtitle(paste(base_titre, "(ligne)"))

#==========================================#
## Carte des données captées par neurones ##
#==========================================#

#création d'un nouveau df montrant le nombre de données associées à chaque référent
#attention : certaines neurones peuvent ne pas avoir de données associées...

#on prend le df qui a servi au som
df_ref <- df_stat_scale_scale #inutile ??
#on ajoute experience
df_ref$nom_experience <- df_selec$nom.experience
#et id_neurone
df_ref$id_neurone <- som$unit.classif
#on retire le superflu, on regroupe par pair id_neurone associé - nom_experience...
#...et on compte le nombre de données associé à chaque pair
df_count_neuro_expe <- df_ref %>% select(id_neurone, nom_experience) %>% 
  group_by(id_neurone, nom_experience) %>% summarise(count = n())

#on utilise tidyr::spread pour que chaque expe est sa colonne
expe_par_neurone <- df_count_neuro_expe %>% spread(nom_experience, count, fill = 0)

#reste algo précédent
#expe_par_neurone <- as.data.frame(with(df_ref, 
#                                       tapply(row.names(df_selec),
#                                              list(neurone=id_neurone,experience=nom.experience), 
#                                              length)))


#ajoutons les neurones manquants

#liste des neurones non présents
list_neuro_vid <- which(!seq(1:900) %in% as.numeric(expe_par_neurone$id_neurone))
nb_neuro_vid <- length(list_neuro_vid)
#df des neurones manquants
df_neuro_vid <- as.data.frame(matrix(nrow=nb_neuro_vid,ncol = ncol(expe_par_neurone)))
#prépa du rbind avec autre fdf
rownames(df_neuro_vid) <- list_neuro_vid
colnames(df_neuro_vid) <- colnames(expe_par_neurone)
df_neuro_vid$id_neurone <- list_neuro_vid
df_neuro_vid[is.na(df_neuro_vid)] <- 0
#bind_rows (not rbind !) & mis en ordre
expe_par_neurone <- bind_rows(expe_par_neurone,df_neuro_vid)
expe_par_neurone <- expe_par_neurone %>% arrange(id_neurone)



#nom de l'experience la plus représentée par neurones
expe_major_par_neurone <- as.factor(colnames(expe_par_neurone[-1])[max.col(expe_par_neurone[-1])])
#put NA in line of neurone empty
expe_major_par_neurone[rowSums(expe_par_neurone)==0]<-NA
#test <- as.factor(rep(c("AB","CW","DA"),300))
#plot(som, type = "property", property = as.numeric(test), main="expérience majoritaire par neurone")

#representation graphique de l'expérience majoritaire par neurones :
plot(som, type = "property", property = as.numeric(expe_major_par_neurone), main="",  heatkey =F,palette.name=coolBlueHotRed)
text(x=som$grid$pts[,1],y=som$grid$pts[,2],labels=expe_major_par_neurone,cex = .5,font=2)


## Graph des répartitions entre expériences par neurones
nb_group <- 300
nb_expe <- 8
base_rep <- 1:nb_group
rep <- rep("", nb_expe - 1)
lab_rep <- purrr::map(base_rep, function(ele) { c(ele, rep)}) %>% unlist()

# reprendre code pour recup expe_par_neurone
expe_par_neurone <- expe_par_neurone %>% head(nb_group) 
#expe_par_neurone$groups <- as.numeric(expe_par_neurone$groups)
#expe_par_neurone %>% filter(groups == 15)

#melted_expe_par_n <- melt(expe_par_neurone, id = "id_neurone") %>% 
#  group_by(id_neurone) %>% mutate(cumsum = cumsum(value)) %>% filter(variable != "id_neurone")

gathered_expe_par_n <- expe_par_neurone %>% gather(variable, value, -id_neurone) %>% 
  group_by(id_neurone) %>% mutate(cumsum = cumsum(value))

gathered_expe_par_n %>% filter(id_neurone == 15)

expe_neuro_theme <-  theme(panel.margin.x = unit(0, "lines"), panel.margin.y = unit(0, "lines"), panel.border = element_rect(colour = rgb(1.0, 0, 0, 0.5), fill=NA, size=1),
                           axis.text.x=element_text(size = 6), 
                           axis.title.y=element_blank(), axis.text.y=element_text(size = 8), axis.ticks.y=element_blank(),
                           strip.background = element_blank(), strip.text.x = element_blank(),
                           legend.title = element_text(colour="black", size = 5,  face="bold"),
                           legend.text = element_text(colour="black", size = 5),
                           legend.key = element_rect(size = 1),
                           title = element_text(color = "blue", size = 6, face = "bold"))

expe_neuro_theme2 <- theme(axis.text.y = element_text(size = 3, margin = margin(0, 0, 0, 0)))

ggplot(gathered_expe_par_n, aes(x = as.factor(id_neurone), y = value, fill = factor(variable))) %+% 
  geom_bar(stat = "identity") %+%
  #geom_text(aes(x = group + .2, y = cumsum - 15, label = value), size = 3, colour = "black", check_overlap = TRUE) %+%
  geom_text(aes(x = id_neurone - 0, y = (cumsum + cumsum - value)/2, label = ifelse(value != 0, as.character(variable), "")), size = 3, colour = "black", check_overlap = FALSE) %+%
  expe_neuro_theme %+%
  ggtitle("expériences captées")

ggplot(gathered_expe_par_n, aes(x = id_neurone, y = value, fill = factor(variable))) %+% 
  geom_bar(stat = "identity", position = "fill") %+%
  #geom_text(aes(y = cumsum - 15, label = value), colour = "black") %+%
  expe_neuro_theme %+%
  ggtitle("expériences captées")

ggplot(gathered_expe_par_n, aes(x = variable, y = value, fill = variable)) %+% 
  facet_wrap( ~ id_neurone, ncol = 30, scales = "free")  %+% 
  geom_bar(stat="identity") %+%
  #geom_text(aes(x = 2.5, y = 1, label = lab_rep), colour="black", size = 2, inherit.aes=F) %+%
  expe_neuro_theme %+%
  ggtitle("expériences captées")


### Carte des temporalités captées par neurones
#traiter à part les données pour les expériences "2" (ie PCo2, PCo3)


#on prend le df général
df_sec <- as.data.table(df_selec)
#on crée une var de facteur par secondes (arbitraire ??)
df_sec$one_sec_elapsed <- df_selec$tps.ecoule %>% round(digits = 0) %>% factor()
df_sec$five_sec_elapsed <- df_selec$tps.ecoule %>% `/`(5)  %>% round(digits = 0) %>% factor()
#on ajoute id_neurone
df_sec$id_neurone <- som$unit.classif


## Table of the time per neurones

#on retire les expe qui finissent pas 2 ou 3
#df_sec <- df_sec %>% filter(!grepl("*[2|3]",nom.experience))
#on retire le superflu, on regroupe par pair id_neurone - sec_elapsed...
#...et on compte le nombre de données associé à chaque pair
df_neuro_sec <- df_sec %>% select(id_neurone, one_sec_elapsed, nom.experience) 
df_neuro_5sec <- df_sec %>% select(id_neurone, five_sec_elapsed, nom.experience) 
df_count_neuro_1sec <- df_neuro_sec %>% 
  group_by(id_neurone, one_sec_elapsed, nom.experience) %>% summarise(count = n())
df_count_neuro_5sec <- df_neuro_5sec %>% 
  group_by(id_neurone, five_sec_elapsed, nom.experience) %>% summarise(count = n())

#on utilise tidyr::spread pour que chaque expe est sa colonne
expe_par_sec <- df_count_neuro_1sec %>% spread(nom.experience, count, fill = 0)
expe_par_5sec <- df_count_neuro_5sec %>% spread(nom.experience, count, fill = 0)

## table of the neurones graphs according to time
dt_neur_coord <- data.table(id_neurones = 1:900, x = som$grid$pts[,1], y = som$grid$pts[,2])

#create a data table that for each experience shows the neurone path in the data and the time spent in each neurone
chg_neur <- df_sec$id_neurone != lag(df_sec$id_neurone) | df_sec$nom.experience != lag(df_sec$nom.experience) 
chg_neur[1] <- T
path_neur <- df_sec$id_neurone[chg_neur]
nom_expe <- df_sec$nom.experience[chg_neur]
#no change : 1 as long as the referent is not changing
no_chg <- as.numeric(!chg_neur)
#reduce the no change vec into a vector that counts the length of each series of no change  
nb_data <- no_chg %>% purrr::reduce( function(prev, cur) { if(cur == 1) {prev[length(prev)] <- prev[length(prev)] + 1 ; prev } else{ prev[length(prev)+1] <- 1; prev }}, 1)
#creata a dt
dt_path_neur <- data.table(path_neur, nom_expe, nb_data)
dt_path_neur <- bind_cols(dt_path_neur,as.data.frame(dt_neur_coord[dt_path_neur$path_neur,.(x,y)]))
#next neurone coordinates
#let first of each experience at na ??
new_expe <- dt_path_neur$nom_expe != lag(dt_path_neur$nom_expe)
dt_path_neur$xend <- lag(dt_path_neur$x, default = dt_path_neur$x[1])
dt_path_neur$yend <- lag(dt_path_neur$y, default = dt_path_neur$y[1])

#new graph of the neurones
ggplot(dt_neur_coord, aes(x = x, y = y)) + geom_point( col = "blue", shape = 21)
ggplot(dt_path_neur, aes(x = x, y = y)) %+% geom_point(aes(col = nom_expe, size = nb_data), alpha = 0.5) 

#graph with neurones color given according to the experience attached to it (add a little jitter to help with overlap)
ggplot(dt_path_neur, aes(x = x, y = y)) %+% geom_jitter(aes(col = nom_expe), width = 0.5, height = 0.5, alpha = 0.5) 

test_AB <- dt_path_neur %>% filter(nom_expe == "AB") %>% dplyr::mutate(elapsed_time = cumsum(nb_data)) %>% data.table

ggplot(test_AB, aes(x = x, y = y)) %+% 
  geom_point(col = "darkgrey")  %+%
  geom_segment(aes(xend = xend, yend = yend, col = elapsed_time), alpha = .5, arrow = arrow(length = unit(0.02, "npc"))) %+%
  scale_color_gradient2(mid = "blue", high = "red")

dt_path_neur <- dt_path_neur %>% group_by(nom_expe) %>% mutate(elapsed_time = cumsum(nb_data)) %>% data.table() 

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(dt_path_neur, aes(x = x, y = y)) %+%
  ggplot2::facet_wrap(facets =  ~ nom_expe) %+% 
  scale_color_manual(values = cbPalette) %+%
  geom_point(aes(col = nom_expe)) 


graph_path_neurons <- function(df) {
  #colors & theme
  cbPalette2 <- c("#F00000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  simple_theme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  gg <-  ggplot(df, aes(x = x, y = y)) %+%
    facet_wrap(facets =  ~ nom_expe) %+% 
    geom_segment(aes(xend = xend, yend = yend, col = elapsed_time), alpha = .5, arrow = arrow(length = unit(0.05, "npc"))) %+% 
    scale_color_gradient2(mid = "grey87", high = "grey27") %+%
    geom_point(aes(fill = nom_expe, size = nb_data), shape = 21) %+% 
    scale_fill_manual(values = cbPalette2) %+%
    simple_theme %+%
    ggtitle("déplacement des données entre les référents")
  print(gg)
}

dt_path_short <- dt_path_neur %>% filter(elapsed_time < 10000)
graph_path_neurons(dt_path_short)

dt_path_short2 <- dt_path_neur %>% filter(elapsed_time >= 10000 & elapsed_time < 20000)
graph_path_neurons(dt_path_short2)

dt_path_short3 <- dt_path_neur %>% filter(elapsed_time >= 20000 & elapsed_time < 30000)
graph_path_neurons(dt_path_short3)

dt_path_short4 <- dt_path_neur %>% filter(elapsed_time >= 30000 & elapsed_time < 40000)
graph_path_neurons(dt_path_short4)
