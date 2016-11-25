source("../RGeneralFunctions/load-functions.R")
source("../RGeneralFunctions/dplyr-functions.R")

#########IMPORT FUNCTION FOR AFFEC COMPUT ##############

list_exp <- c("AB", "ST", "DA", "LM", "FS1", "PCo", "PCo2", "PCo3", "CW",
                "HL", "CLP", "DE", "AW", "DA2", "DA3", "EZ1", "GC1", "IA")

list_half_1 <- list_exp[1:floor(length(list_exp)/2)]
list_half_2 <- list_exp[(floor(length(list_exp)/2)+1):length(list_exp)]

#IMPORT the basic df
import_df_all <- function(){
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
  }


  #charge les datasets en mémoire
  df_all <- load_data(list_exp)
  testthat::expect_identical(sort(list_exp), levels(df_all$nom.experience))

  as.data.frame(df_all)
}