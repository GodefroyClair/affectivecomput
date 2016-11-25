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

#-- HELPER FUNCTIONS --#

##FOR RESPI
#insert df in high order function
#exemple to pick up AB pick_exp(DF_ALL)("AB")
get_expe_base <- function(df) {
  function(expe) {
    df %>% dplyr::filter(nom.experience == expe)
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

#RESPIRATION
respi_add_variables <- function(df) {
  #base
  df$respi_clean <- df$respiration
  
  #données manquantes
  lg_na <- which(is.na(df$respiration))
  df$respi_clean[lg_na] <- df$respiration[lg_na-1]
  #test
  if(any(is.na(df$respi_clean)))stop("still na data in respiration measurement...")
  
  #### NO #### 
  #LM, DA2, DA3,
  
  df <- df %>% dplyr::mutate( respi_clean = replace(respi_clean, nom.experience %in% c("LM","DA2","DA3"), NA))
  expect_true(any(is.na( df %>% dplyr::filter(nom.experience %in% c("LM","DA2","DA3")) %>%  ungroup() %>% dplyr::select(respi_clean))))
  expect_false(any(is.na( df %>% dplyr::filter(!nom.experience %in% c("LM","DA2","DA3")) %>% ungroup %>% dplyr::select(respi_clean))))
  
  #### NETTOYER ###
  #AW, FS1, DA, AB, CLP
  
  #AW
  df <- df %>% dplyr::mutate( respi_clean = 
                         replace(respi_clean, nom.experience == "AW" & respiration >= 0,
                                 NA))
  
  #FS1
  df <- df %>% dplyr::mutate( respi_clean = 
                         replace(respi_clean, nom.experience == "FS1" & 
                                   (respiration >= 0 | respiration < -25),
                                 NA))
  #DA 
  df <- df %>% dplyr::mutate( respi_clean = 
                         replace(respi_clean, nom.experience == "DA" & respiration >= -1 | respiration == -18.6,
                                 NA))
  
  #AB
  date_seuil <- df %>% 
    dplyr::filter(nom.experience == "AB" & respiration <= -18.66 & lubridate::minute(date) >= 43) %>% ungroup %>%
    dplyr::select(date) %>% dplyr::summarise(min(date)) %>% `[[`(.,1)
  
  df <- df %>% dplyr::mutate(respi_clean = 
                        replace(respi_clean, nom.experience == "AB" & minute(date) >= date_seuil,
                                NA))
  
  #CLP
  min_date <- df %>% dplyr::filter(nom.experience == "CLP" & respiration < -18.65) %>% ungroup %>% dplyr::select(date) %>% summarise(min(date)) %>% `[[`(.,1)
  df <- df %>% dplyr::mutate( respi_clean = 
                         replace(respi_clean, nom.experience == "CLP" & date >= min_date ,
                                 NA))
  
  
  #========== CLEAN HIGH FREQUENCIES ==============#
  df <- df %>% group_by(nom.experience) %>% 
    dplyr::mutate(respi_clean_h = clean_high_freq(respi_clean, 1/5))
  
  
  #========== SEPARATE LOW FREQUENCIES (trend) ==============#
  df <- df %>% group_by(nom.experience) %>% dplyr::mutate(respi_clean_hl = clean_low_freq(respi_clean_h, .01))
  
  df <- df %>% group_by(nom.experience) %>% dplyr::mutate(respi_trend = clean_high_freq(respi_clean_h, .01))
  
  
  ### A REVOIR !!!!! ###
  if (DEBUG) {
    maybeLoess <- function(df) {
      ls <- loess(data = df, formula = y ~ x, degree = 1, span = .3)
      print(ls)
      ls$fitted
    }
    df_test <- data.frame(x = runif(100, 0, 1), y = rnorm(100, 0, 2), z = as.factor(rep(c("b","r"),50)))
    df_test %>% group_by(z) %>% dplyr::mutate(m_loess = maybeLoess()) 
    #magritr
  }
  #loess <- df %>% group_by(nom.experience) %>% loess(data = ., formula = respi_clean ~ as.numeric(date), degree = 1, span = .3) %>% `[`(.,"fitted")  
  #df$loess <- loess 

  get_expe <- get_expe_base(df)
  add_mean_per_period <- add_stat_per_period(mean)
  df <- purrr::map_df(list_exp, ~ add_mean_per_period(get_expe(.)))
  get_expe <- get_expe_base(df)
  add_min_per_period <- add_stat_per_period(min)
  df <- purrr::map_df(list_exp, ~ add_min_per_period(get_expe(.)))
  get_expe <- get_expe_base(df)
  add_max_per_period <- add_stat_per_period(max)
  df <- purrr::map_df(list_exp, ~ add_max_per_period(get_expe(.)))
  
  ###### frequency
  df$period_duration  <- period_id2duration(df$max_per_period)
  
  df
}

##################################################### 
################# FREQ CARD #########################
##################################################### 

freqcard_add_variables <- function(df) {
  
  ##remove signal off from DA
  df <- df %>% dplyr::mutate(freqcard_clean = replace(frequence.cardiaque, frequence.cardiaque == 0, NA))
  #test
  expect_equal(length(which(df$frequence.cardiaque == 0)), 476)
  expect_length(which(df$freqcard_clean == 0),0)
  
  #######################
  ### ADD VARIABLES  ####
  #######################
  
  ### ADD lowpass_filter ### 
  ##use of a low pass filter
  ## clean_low_freq can't deal with NA's...
  df <- df %>% group_by(nom.experience) %>%
    dplyr::mutate(., freqcard_low_f = 
             ifelse(!is.na(freqcard_clean), 
                    clean_high_freq(freqcard_clean[!is.na(freqcard_clean)] - mean(freqcard_clean, na.rm = T), 1/100) + 
                      mean(freqcard_clean, na.rm = T), 
                    freqcard_clean))
  
  
  ### ADD diff ####
  df <- df %>% group_by(nom.experience) %>% dplyr::mutate(freqcard_diff = freqcard_clean - lag(freqcard_clean))
  df <- df %>% group_by(nom.experience) %>% dplyr::mutate(freqcard_low_diff = freqcard_low_f - lag(freqcard_low_f))
   
  if(PRINT){
    df_half_1 <- df[df$nom.experience %in% list_half_1,]
    df_half_2 <- df[df$nom.experience %in% list_half_2,]
    
    plot.evol.par.expe(df_half_1[df_half_1$nom.experience == "DA",], mesure = "freqcard_clean")
    plot.evol.par.expe(df_half_1[df_half_1$nom.experience == "DA",], mesure = "freqcard_low_f")
     
    plot.evol.par.expe(df_half_1, mesure = "frequence.cardiaque")
    plot.evol.par.expe(df_half_2, mesure = "frequence.cardiaque")
    
    #missing values
    suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "freqcard_clean"))
    suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "freqcard_clean"))
    
    suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "freqcard_low_f"))
    suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "freqcard_low_f"))
    
    suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "freqcard_diff"))
    suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "freqcard_diff"))
    
    suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "freqcard_low_diff"))
    suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "freqcard_low_diff"))
  }
  df
}

temp_add_variables <- function(df) {
  
  ### ELIMINATE : AB, IA, CW, DE
  df <- df %>%
    dplyr::mutate(temp_clean = replace(temperature, nom.experience %in% c("AB", "IA", "CW", "DE"), NA))
  #test
  test <- df %>% dplyr::filter(nom.experience %in% c("AB", "IA", "CW", "DE"))
  expect_true(any(is.na(test$temp_clean)))
  
  ### remove signal off from DA
  df <- df %>% dplyr::mutate(temp_clean = replace(temp_clean, temperature < 10, NA))
  #test
  expect_equal(which(df$temperature < 10), 98153)
  expect_equal(length(which(df$temperature < 10)), 1)
  expect_length(which(df$temp_clean < 10),0)
  
  #######################
  ### ADD VARIABLES  ####
  #######################
  
  ### ADD lowpass_filter ### 
  ##use of a low pass filter
  ## clean_low_freq can't deal with NA's...
  df <- df %>% group_by(nom.experience) %>%
    dplyr::mutate(., temp_low_f = 
             ifelse(!is.na(temp_clean), 
                    clean_high_freq(temp_clean[!is.na(temp_clean)] - mean(temp_clean, na.rm = T), 1/100) + 
                      mean(temp_clean, na.rm = T), 
                    temp_clean))
  
  #df$temp_low <- NA
  #m_tmp <- mean(df$temp_clean, na.rm = T)
  #df$temp_low[!is.na(df$temp_clean)] <- clean_high_freq(df$temp_clean[!is.na(df$temp_clean)] - m_tmp, 1/100) + m_tmp
     
  
  ### ADD diff ####
  df <- df %>% group_by(nom.experience) %>% dplyr::mutate(temp_diff = temp_clean - lag(temp_clean))
  df <- df %>% group_by(nom.experience) %>% dplyr::mutate(temp_low_diff = temp_low_f - lag(temp_low_f))
   
  
  if(PRINT){
    df_half_1 <- df[df$nom.experience %in% list_half_1,]
    df_half_2 <- df[df$nom.experience %in% list_half_2,]
    
    plot.evol.par.expe(df_half_1[df_half_1$nom.experience == "DA",], mesure = "temp_clean")
    plot.evol.par.expe(df_half_1[df_half_1$nom.experience == "DA",], mesure = "temp_low_f")
    plot.evol.par.expe(df_half_1[df_half_1$nom.experience == "DA",], mesure = "temp_low")
     
    plot.evol.par.expe(df_half_1, mesure = "temperature")
    plot.evol.par.expe(df_half_2, mesure = "temperature")
    
    #missing values
    suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "temp_clean"))
    suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "temp_clean"))
    
    suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "temp_low_f"))
    suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "temp_low_f"))
    
    suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "temp_diff"))
    suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "temp_diff"))
    
    suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "temp_low_diff"))
    suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "temp_low_diff"))
  }

  df
}

transpi_add_variables <- function(df) {
  
  ################
  ###NETTOYAGE####
  ################
  #ST & HL:
  df <- df %>%
    dplyr::mutate(elecderm_clean = replace(activite.electrodermale, nom.experience %in% c("ST", "HL"), NA))
  
  ##test
  test_df <- df %>% 
    dplyr::filter(nom.experience %in% c("ST","HL")) %>% ungroup %>%  dplyr::select(elecderm_clean)
  expect_true(all(is.na(test_df)))
  rm(test_df)
  
  ##GC1##
  #-----#
  gc1_transpi <- df %>% dplyr::filter(nom.experience == "GC1")
  m_gc1 <- round(mean(gc1_transpi$activite.electrodermale, na.rm = T),4)
  gc1_transpi <- gc1_transpi %>% dplyr::mutate( elecderm_clean = 
      clean_high_freq(activite.electrodermale - m_gc1, 1/150) + m_gc1)
  df[df$nom.experience == "GC1",]$elecderm_clean <- gc1_transpi$elecderm_clean
  
  
  ##FAIL TRY
  
  #filter GC1 data with a low pass filter
  #df[df$nom.experience == "GC1",] <- 
  #  df %>% filter(nom.experience == "GC1") %>% 
  #  dplyr::mutate(elecderm_clean = clean_high_freq(activite.electrodermale, 1/150))
  
  
  if (PRINT) {
    gg <- plot_var_exp_ho(gc1_transpi, "activite.electrodermale")("GC1", print = F)
    gg + ylim(c(-48, -39))
    gg <- plot_var_exp_ho(gc1_transpi, "elecderm_clean")("GC1", print = F)
    gg + ylim(c(-48, -39))
  }
  
  
  ##################
  ## ADD COLUMNS ###
  ##################
  
  ### ADD var with high frequencies filtered ###
  ##use of a low pass filter
  ## clean_low_freq can't deal with NA's...
  df <- df %>% group_by(nom.experience) %>%
    dplyr::mutate(.,elecderm_low_f = 
             ifelse(!is.na(elecderm_clean), 
                    clean_high_freq(elecderm_clean - mean(elecderm_clean, na.rm = T), 1/100) + 
                      mean(elecderm_clean, na.rm = T), 
                    elecderm_clean))
  #any(df$elecderm_low_f != df$elecderm_low_f2, na.rm = T)
  
  ### ADD variation variable ###
  df <- df %>% group_by(nom.experience) %>% dplyr::mutate(elecderm_diff = elecderm_clean - lag(elecderm_clean))
  df <- df %>% group_by(nom.experience) %>% dplyr::mutate(elecderm_low_diff = elecderm_low_f - lag(elecderm_low_f))
  #df[is.na(df$elecderm_diff),"elecderm_diff"] <- 0.0
  
  if(PRINT){
    df_half_1 <- df[df$nom.experience %in% list_half_1,]
    df_half_2 <- df[df$nom.experience %in% list_half_2,]
    
    plot.evol.par.expe(df_half_1[df_half_1$nom.experience == "AB",], mesure = "elecderm_clean")
    plot.evol.par.expe(df_half_1[df_half_1$nom.experience == "AB",], mesure = "elecderm_low_f")
     
    plot.evol.par.expe(df_half_1, mesure = "activite.electrodermale")
    plot.evol.par.expe(df_half_2, mesure = "activite.electrodermale")
    
    #missing values
    suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "elecderm_clean"))
    suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "elecderm_clean"))
    
    suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "elecderm_low_f"))
    suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "elecderm_low_f"))
    
    suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "elecderm_diff"))
    suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "elecderm_diff"))
    plot.evol.par.expe(df_half_1[df_half_2$nom.experience == "IA",], mesure = "elecderm_diff")
    
    suppressWarnings(plot.evol.par.expe(df_half_1, mesure = "elecderm_low_diff"))
    suppressWarnings(plot.evol.par.expe(df_half_2, mesure = "elecderm_low_diff"))
  }
  df
}