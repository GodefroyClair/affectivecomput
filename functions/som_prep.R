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
PCA_REPLACE_NA <- TRUE
PREDICT_NA <- FALSE

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
df_all <- dplyr::as_data_frame(df_all)



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
df_all <- dplyr::as_data_frame(df_all)

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

df_stat <- dplyr::as_data_frame(df_selec[, noms_var_stat])

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
  ##TWO methods to find NA's value
  if(PREDICT_NA){
    
  }else {
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

df_in_pca <- dplyr::as_data_frame(pca_no_na$x)
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

