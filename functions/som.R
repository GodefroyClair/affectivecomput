# !diagnostics off
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
library(assertthat)
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
library(readr)
library(stringr)
#instal ggradar : devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
# configured to work on a Mac, change directory to Unix or Windows
#download.file("https://dl.dropboxusercontent.com/u/2364714/airbnb_ttf_fonts/Circular Air-Light 3.46.45 PM.ttf", "/Library/Fonts/Circular Air-Light 3.46.45 PM.ttf", method="curl")
#extrafont::font_import(pattern = 'Circular', prompt=FALSE)
theme_set(theme_bw(24))

####################################################################################
################################# MACRO & HYPER-PARAMETRES##########################
####################################################################################

if (!exists("DEBUG")) DEBUG <- FALSE
if (!exists("PRINT")) PRINT <- FALSE
LARGE_SET_EXPE <- TRUE
LARGE_SET_VAR <- TRUE
PCA_REPLACE_NA <- FALSE

############################################################
#######################LOAD & PREP #########################
############################################################

#general helpers
source("../RGeneralFunctions/dplyr-functions.R")
source("../RGeneralFunctions/load-functions.R")
source("../RGeneralFunctions/cleaning-functions.R")
source("../RGeneralFunctions/ggplot-functions.R")
source("../RGeneralFunctions/clean-filters-functions.R")
source("../RGeneralFunctions/toolbox_functions.R")
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

################### EXOGENEOUS VARIABLES ############

df_all$tps_ecoule <- as.difftime(df_all$tps_ecoule, units = "secs")
df_all$tps.ecoule <- NULL

### STATE
states <- read_delim("./data/decomposition_etats_XP.csv", 
                     delim = ";", col_names = c("debut","fin", "etat"), 
                     col_types = "ttc", skip = 1)
state_levels <- c("neutre", "etat1", "etat2", "etat3", "etat4", "etat5", "etat6",
                  "etat7", "etat8",  "etat9",  "etat10", "etat11", "etat12", "etat13",
                  "etat14", "etat15", "etat16", "etat17", "etat18", "etat19", "etat20",
                  "etat21", "etat22", "etat23", "etat24", "etat25", "etat26", "fin", "unknown")
states$etat <- factor(x = states$etat, levels = state_levels)

#http://stackoverflow.com/questions/29373160/using-dplyrmutate-between-two-dataframes-to-create-column-based-on-date-range
df_all <- df_all %>% rowwise() %>% mutate(state = states$etat[between(tps_ecoule, states$debut, states$fin )][1])
df_all <- df_all %>% ungroup
expect_true(is.factor(df_all$state))

#some states are missing for the end (longer XPs)
df_all$state <- df_all$state
df_all[is.na(df_all$state),"state"] <- "fin"
expect_length(which(is.na(df_all$state)), 0)

#change state for experience 2 and 3
df_all <- df_all %>% mutate(state = if_else(grepl( "[2|3]",as.character(nom.experience)),
                                             factor("unknown",levels = state_levels), state))


############## PREP FOR KOHONEN MAP ###############

list_expe <- levels(df_all$nom.experience)
#two kinds of expe
list_expe_small <- list_expe[(!list_expe %in% c("AW","DA2","DA3","EZ1","FS1","GC1","HL","IA","LM","ST"))]
list_expe_large <- list_expe[(!list_expe %in% c("FS1","LM","HL","ST"))]

#hyperp to choose large or small set
if(LARGE_SET_EXPE) {
  list_expe_selec <- list_expe_large
} else {
  list_expe_selec <- list_expe_small
}

#choose between the 2 list of experiences : 
#list_expe_selec <- list_expe_sml
df_selec <- df_all[df_all$nom.experience %in% list_expe_selec,]

#nom des variables sur lesquelles seront appliquées les algo statistique
#on retire nom experience
noms_var_stat_small <- c("activite.electrodermale","temperature","frequence.cardiaque",
                         "nom.experience", "respi_clean_hl","respi_trend", 
                         "loess","max_per_period","min_per_period","period_duration", "state", "tps_ecoule")
#larger list
noms_var_stat_large <- c("nom.experience",
                         "elecderm_clean", "elecderm_diff", 
                         "temp_clean", "temp_diff",
                         "freqcard_clean", "freqcard_diff",
                         "respi_clean", "respi_clean_hl", "respi_trend", "loess", 
                         "max_per_period","min_per_period","period_duration","state","tps_ecoule")

#hyperp to choose minimal or maximal set of variables
if(LARGE_SET_VAR) { 
  noms_var_stat <- noms_var_stat_large 
} else{ 
  noms_var_stat <- noms_var_stat_small 
}

df_stat <- as_data_frame(df_selec[, noms_var_stat])

#2 scale : global or per individuals

#scale the var by experiences 
#!!bug with dplyr::group_by() + dplyr::mutate_each() + scale() : https://github.com/hadley/dplyr/issues/2049
#need to create my own scale function
my_scale <- function(x, scale = T) c(scale(x, scale = scale))

#simple (general) scale
df_stat_scaled <- df_stat %>% mutate_each(funs(scale), -c(nom.experience, loess, state, tps_ecoule))


#add statistical transformations per experiences
#NB : ungroup after groop_by to avoid bugs
#!!TODO add c(nom.experience, loess, state) to mutate_each!!
df_stat_scaled_per_exp <- df_stat %>% group_by(nom.experience) %>% mutate_each(funs(my_scale), -c(nom.experience, state, tps_ecoule)) %>% ungroup 
df_stat_centered_per_exp <- df_stat %>% group_by(nom.experience) %>% mutate_each(funs(my_scale(., scale = F)),-c(nom.experience, state, tps_ecoule)) %>% ungroup
df_stat_scale_scale <- df_stat_scaled_per_exp %>% mutate_each(funs(my_scale(.)), -c(state, nom.experience)) 
df_stat_centered_scale <- df_stat_centered_per_exp %>% mutate_each(funs(my_scale(., scale = F)), -c(nom.experience, state, tps_ecoule)) 

if(DEBUG) {
  ## test solution 4 bug
  set.seed(7)
  df <- data.frame(x=10*runif(9), y=20*rnorm(9), group=rep(c("a","b","c"),3))
  
  #show the bug :
  df_2 <- df %>% dplyr::group_by(group) %>% dplyr::mutate_each(funs(scale))
  expect_that(dim(df_2[[2]]), equals(c(ncol(df),1))) #dim should be NULL !!
  
  #solution : use a fonction that coerse the result of scale to vector
  my_scale_test <- function(x, ...) c(scale(x, ...))
  df_3 <- df %>% dplyr::group_by(group) %>% dplyr::mutate_each(funs(my_scale_test))
  expect_that(dim(df_3[[2]]), equals(NULL)) #should be 
  # old solution :
  # df_2 <- data.frame(df_2[1:nrow(df_2),])
  
  df_4 <- df %>% dplyr::group_by(group) %>% dplyr::mutate_each(funs(my_scale_test(.,scale = F)))
  expect_equal(df_4[df$group == "a",]$x, df$x[df$group == "a"] - mean(df$x[df$group == "a"]))
}

#################################
############# PCA ###############
#################################

source(file = "~/githubRepos/RGeneralFunctions/ggplot_et_co/ggpca/PCA.R")
# PCA for all numerical variable
df4pca <- df_stat %>% ungroup %>% dplyr::select(-loess)

#true by default
if(PCA_REPLACE_NA){
  #replace NA by a stat calculated on the column of NA
  #inspired by https://stat.ethz.ch/pipermail/r-help/2007-November/146598.html
  
  # na2stat <- function (df, fun) {
  #   #replace missing values :
  #   replace_na <- function(vec, fun) {
  #     m <- fun(vec, na.rm = TRUE)
  #     vec[is.na(vec)] <- m
  #     return(vec)
  #   }
  #   map_df(df,replace_na, fun)
  # }
  na2stat_ho <- function(fun) {
    function(vec) {
      m <- fun(vec, na.rm = TRUE)
      vec[is.na(vec)] <- m
      return(vec)
    }
  }
  #df_pca_no_na <- df_pca %>% select(-c(nom.experience, state)) %>% na2stat(fun = mean)
  df4pca_no_na <- df4pca %>% mutate_if(is.numeric, na2stat_ho(fun = mean))
  if(DEBUG){ 
    print("dim of df pca no-na :");print(dim(df4pca_no_na))
  }
} else {
  df4pca_no_na <- df4pca %>% na.omit
  if(DEBUG) print(dim(df4pca_no_na))
}

#you can do pca on the df4pca and omit the lines with nas
# or do pca on the df4pca_no_na, we took 2nd option
filt <- function(vec) {
  is.factor(vec) | (class(vec) == "difftime")
}
pca_no_na <- prcomp( ~ ., data = select_if(df4pca_no_na,not(filt)), 
                     center = T, scale = T, na.action = "na.pass")
#get the individuals in new base from PCA

df_in_pca <- as_data_frame(pca_no_na$x)
df_in_pca$nom.experience <- df4pca_no_na$nom.experience
df_in_pca$state <- df4pca_no_na$state
df_in_pca$tps_ecoule<- df4pca_no_na$tps_ecoule

#test if the multi of rotation matrix and original data are equat to the result ("x")
if(DEBUG) {
  df_pca_scaled <- df_pca_no_na[,!names(df_pca) %in% c("nom.experience", "state", "tps_ecoule")] %>% mutate_each(funs(scale))
  head(df_pca_scaled)
  test <- as.matrix(df_pca_scaled) %*% pca_no_na$rotation
  head(test)
  head(as.matrix(pca_no_na$x))
  assert_that(identical(as.matrix(pca_no_na$x[2,]), as.matrix(test[2,])))
  assert_that(all(as.matrix(pca_no_na$x) == as.matrix(test)) && dim(pca_no_na$x == dim(test)))
  rm(test)
}

if(PRINT) {
  gg_PCA_biplot(pca)
  gg_PCA_biplot(pca_no_na)
}


#get the percent of variance of eigen value 1 compared to eigen value 2
share_eigenv1 = pca_no_na$sdev[1]^2 / sum(pca_no_na$sdev[1:2]^2)
share_eigenv2 = pca_no_na$sdev[2]^2 / sum(pca_no_na$sdev[1:2]^2)

#install_github("ggbiplot", "vqv")

write_csv(df_in_pca,'./data/pca_no_na_omit.csv')
write(share_eigenv1, './data/share_eigen1')
write(share_eigenv2, './data/share_eigen2')

#-------------------------------------------#
#--- Eliminate too extreme observations  ---#
#-------------------------------------------#
df_in_pca <- read_csv('./data/pca_no_na_omit.csv')
share_eigenv1 <- scan('./data/share_eigen1')
share_eigenv2 <- scan('./data/share_eigen2')

#--- FOR PCA ---#

dist_deal_na <- function(vec) {
  sqrt(sum(vec^2, na.rm = T)) #somehow na.rm remove non numerical column...
}
#### ICI UN BUG ??? ###
df_pca_dist <- df_in_pca %>% select(-c(nom.experience, state, tps_ecoule)) %>% 
  purrr::by_row(..f = dist_deal_na, .collate = "cols", .to = "dist")
names(df_pca_dist)[names(df_pca_dist)==".out"] <- "dist"
df_pca_dist_test <- df_in_pca[1:1000,] %>% select(-c(nom.experience, state)) %>% purrr::by_row(..f = dist_deal_na, .collate = "cols", .to ="dist")

## ATTENTION : this is with PCA_REPLACE_NA == FALSE
if(! PCA_REPLACE_NA) {
  mean(df_pca_dist$dist)
  sd(df_pca_dist$dist)
  ggplot(df_pca_dist, aes(dist)) + geom_histogram()
  ggplot(df_pca_dist, aes(dist)) + geom_histogram() + xlim(NA, 20)
  ggplot(df_pca_dist, aes(log(dist))) + geom_histogram()
  #the histogram shows a 2nd (small) distribution around log(dist) = 2.1 i.e. dist ~= 8, we want to cut it
  df_pca_filt <- df_pca_dist %>% filter(dist < 6)
  ggplot(df_pca_filt, aes(dist)) + geom_histogram()
}else {
  #filter too extreme data
  df_pca_filt <- df_pca_dist %>% filter(dist < 6)
  #update nom.experience and state
  rows2keep <- which(df_pca_dist$dist < 6)
  df_pca_filt$nom.experience <- df_in_pca$nom.experience[rows2keep]
  df_pca_filt$state <- df_in_pca$state[rows2keep]
  df_pca_filt$tps_ecoule <- df_in_pca$tps_ecoule[rows2keep]
  df_pca_filt <- df_pca_filt %>% select(-dist)
}
nb_neurons_per_dim <- sqrt(400)
grid_x <- round(share_eigenv1/share_eigenv2 * nb_neurons_per_dim)
grid_y <- round(share_eigenv2/share_eigenv1 * nb_neurons_per_dim)


#=====================================#
################ SOM ##################
#=====================================#

###########################
####### with PCA ##########
###########################

#package som
som_pca_test <- som::som(df_pca_dist_test, xdim = 10, ydim = 10,
                     topol="hexa", neigh="gaussian",
                     rlen = 300)
plot(som_pca_test)
som_pca0 <- som::som(df_pca_filt, xdim = 10, ydim = 10,
                     topol="hexa", neigh="gaussian",
                     rlen = 300)
plot(som_pca0)

#package kohonen
som_pca <- kohonen::som(data = as.matrix(df_pca_filt), grid = somgrid(10, 10, "hexagonal"),
                        rlen = 300, alpha = c(2,0.0001))
plot(som_pca)
som_pca2 <- kohonen::som(data = as.matrix(df_in_pca[,names(df_in_pca) != "nom.experience"]), grid = somgrid(grid_x, grid_y, "hexagonal"), rlen=180, alpha =c(2,0.0001))
som_pca3 <- kohonen::som(data = as.matrix(df_in_pca[,names(df_in_pca) != "nom.experience"]), grid = somgrid(10, 10, "hexagonal"), rlen = 250, alpha =c(2,0.0001))
som <- som_pca3


som_pca <- kohonen::supersom(data = list(mat = as.matrix(df_in_pca)),
                             grid = somgrid(20, 20, "hexagonal"),
                             rlen = 200, alpha = c(2,0.0001),
                             maxNA.fraction = .6,
                             keep.data = F,
                             contin = T,
                             whatmap = 1)

plot(som_pca)
plot(som_pca, type = "counts")


#----- with original df ----#

df_SOM <- df_stat_scaled %>% select(-c(nom.experience, dist, tps.ecoule, loess))
share_NA_maj <- which(rowSums(is.na(df_SOM)) / ncol(df_SOM) > .60)
df_SOM[share_NA_maj,]

#calcul de l'algorithme d'attiribution des données aux neurones
#rlen permet de préciser le nombre d'itérations
#temp à mettre au plus haut
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

#som1 <- kohonen::som(data = as.matrix(df_SOM_scale_scale), grid = somgrid(30, 30, "hexagonal"),
#                     rlen=300, alpha =c(2,0.0001), radius = quantile(nhbrdist, 0.9) * c(1, -1))
#som2 <- kohonen::som(data = as.matrix(df_SOM_scaled), grid = somgrid(30, 30, "hexagonal"), rlen=180, alpha =c(2,0.0001))
#som3 <- kohonen::som(data = as.matrix(df_SOM_center_scale), grid = somgrid(30, 30, "hexagonal"), rlen=180, alpha =c(2,0.0001))


som_scaled <- kohonen::supersom(data = list(mat = as.matrix(df_SOM)),
                                grid = somgrid(20, 20, "hexagonal"),
                                rlen = 200, alpha = c(2,0.0001),
                                maxNA.fraction = .6,
                                keep.data = F,
                                contin = T,
                                whatmap = 1)

#som_10_avg_sc <- kohonen::supersom(data = list(mat = as.matrix(df_SOM_center_scale_new)),
# grid = somgrid(10, 10, "hexagonal"),
# rlen = 200, alpha = c(2,0.0001),
# maxNA.fraction = .5,
# keep.data = F,
# contin = T,
# whatmap = 1)

# som_short_new <- kohonen::supersom(data = list(mat = as.matrix(df_SOM_scale_scale_new)),
#                                    grid = somgrid(10, 10, "hexagonal"),
#                                    rlen = 200, alpha = c(2,0.0001),
#                                    maxNA.fraction = .5,
#                                    keep.data = F,
#                                    contin = T,
#                                    whatmap = 1)

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
save(som_scaled,file = "./data/som_scaled.RDa")

save(som1,file = "./data/som1.RDa")
#save(som2,file = "./data/som2.RDa")
save(som3,file = "./data/som3.RDa")
save(df_stat_scale_scale, file = "./data/df_stat_scale_scale.RDa")
save(df_stat_scale_scale, file = "./data/df_stat_center_scale.RDa")
save(df_stat_scaled, file = "./data/df_stat_scaled.RDa")
save(df_selec, file = "./data/df_selec.RDa")


##GRAPHICS
som <- som_pca0
str(som)
df_SOM$ref <- som$unit.classif
df_SOM$nom.experience <- df_selec$nom.experience


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
