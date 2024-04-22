###R library set up
library(dplyr)
library(data.table)
library(lubridate)
library(janitor)
library(haven)
library(cli)
library(arrow)
library(foreach)
library(ggplot2)
library(hash)
library(RColorBrewer)
library(cowplot)
library(patchwork)
library(ggalluvial)




###Setting up the standard file paths used in this project
project <- 
codelists <- 
Rscripts <- 
graphfiles <- 
datafiles <- 
results <- 
raw_data_zip <- 
raw_data <- 
parquet <- paste(c(raw_data, "/parquet_files/"), collapse = "") 
# 

###recurrent functions
#make sure that path for list.files is set correctly
#extract medcodes
extractor_med <- function(list.files, codelist){
  
  df <- as.data.table(read_parquet(list.files[[1]]))
  df[, medcodeid := as.character(medcodeid)]
  print(paste0(length(unique(df$patid)), " in original"))
  df <- df[medcodeid %chin% codelist[, medcodeid]]
  print(paste0(length(unique(df$patid)), " subjects with code extracted"))
  df <- merge(df, codelist, by ="medcodeid")
  
  for(i in 2:length(list.files)){
    df_temp <- as.data.table(read_parquet(list.files[[i]]))
    df_temp[, medcodeid := as.character(medcodeid)]
    print(paste0(length(unique(df_temp$patid)), " in original"))
    df_temp <- df_temp[medcodeid %chin% codelist[, medcodeid]]
    print(paste0(length(unique(df_temp$patid)), " subjects with code extracted"))
    df_temp <- merge(df_temp, codelist, by = "medcodeid")
    
    df <- rbind(df, df_temp)
  }
  
  return(df)
}

#extract prodcodes
extractor_prod <- function(list.files, codelist){
  
  df <- as.data.table(read_parquet(list.files[[1]]))
  df[, prodcodeid := as.character(prodcodeid)]
  print(paste0(length(unique(df$patid)), " in original"))
  df <- df[prodcodeid %chin% codelist[, prodcodeid]]
  print(paste0(length(unique(df$patid)), " subjects with code extracted"))
  df <- merge(df, codelist, by ="prodcodeid")
  
  for(i in 2:length(list.files)){
    df_temp <- as.data.table(read_parquet(list.files[[i]]))
    df_temp[, prodcodeid := as.character(prodcodeid)]
    print(paste0(length(unique(df_temp$patid)), " in original"))
    df_temp <- df_temp[prodcodeid %chin% codelist[, prodcodeid]]
    print(paste0(length(unique(df_temp$patid)), " subjects with code extracted"))
    df_temp <- merge(df_temp, codelist, by = "prodcodeid")
    
    df <- rbind(df, df_temp)
  }
  
  return(df)
}


#rounding the results
rounding <- function(X){
  n <- round(X, digits = 2)
  return(n)
}



#---vaccination coverage at ages 1, 2, and 5 plus 95%-CI
vacc_age <- function(data, vac, age_vac, start, end, vac_dose){
  
  n <- nrow(data[vaccine == vac &
                   age <= age_vac &
                   age_startfu <= start &
                   age_endfu >= end &
                   n_dose == vac_dose])
  return(n)
}

pop_vacc <- function(data_p, start, end){
  
  n <- nrow(data_p[age_startfu <= start & age_endfu >= end])
  return(n)
}


coverage_1y <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 365, start = 40, end = 365, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 365)
  
  return((a/b))
}
coverage_1y_lb <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 365, start = 40, end = 365, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 365)
  p_hat <- a/b
  res <- p_hat -1.96*(sqrt(p_hat*(1-p_hat)/b))
  return(res)
}
coverage_1y_ub <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 365, start = 40, end = 365, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 365)
  p_hat <- a/b
  res <- p_hat +1.96*(sqrt(p_hat*(1-p_hat)/b))
  return(res)
}




coverage_2y <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 731, start = 40, end = 731, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 731)
  
  return((a/b))
}
coverage_2y_lb <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 731, start = 40, end = 731, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 731)
  p_hat <- a/b
  res <- p_hat -1.96*(sqrt(p_hat*(1-p_hat)/b))
  return(res)
}
coverage_2y_ub <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 731, start = 40, end = 731, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 731)
  p_hat <- a/b
  res <- p_hat +1.96*(sqrt(p_hat*(1-p_hat)/b))
  return(res)
}

coverage_3y <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 1096, start = 40, end = 1096, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 1096)
  
  return((a/b))
}

coverage_4y <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 1461, start = 40, end = 1461, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 1461)
  
  return((a/b))
}




coverage_5y <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 1826, start = 40, end = 1826, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 1826)
  
  return((a/b))
}
coverage_5y_lb <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 1826, start = 40, end = 1826, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 1826)
  p_hat <- a/b
  res <- p_hat -1.96*(sqrt(p_hat*(1-p_hat)/b))
  return(res)
}
coverage_5y_ub <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 1826, start = 40, end = 1826, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 1826)
  p_hat <- a/b
  res <- p_hat +1.96*(sqrt(p_hat*(1-p_hat)/b))
  return(res)
}


coverage_6y <- function(vacc, dose, df, df_p){
  
  a <- vacc_age(data = df, vac = vacc, age = 2191, start = 40, end = 2191, vac_dose = dose)
  b <-  pop_vacc(data_p = df_p, start = 40, end = 2191)
  
  return((a/b))
}

max_NA <- function(x){
  return(sum(x, na.rm=T))
}

