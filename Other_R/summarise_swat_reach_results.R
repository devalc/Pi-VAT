## --------------------------------------------------------------------------------------##
##
## 
##
## Purpose of the script: summarize swat Reach annual results.This example script processes 
## multiple scenarios for one watershed. Can be adapted for multi-watersheds under
## which multiple scenarios might be nested.
##
## Author: Chinmay Deval
##
## Created On: 2021-03-24
##
## Copyright (c) Chinmay Deval, 2021
## Email: chinmay.deval91@gmail.com
##
## --------------------------------------------------------------------------------------##
##  Notes:
##   
##
## --------------------------------------------------------------------------------------##

## --------------------------clear environment and console-------------------------------##
rm(list = ls())
cat("\014")

## ----------------------------------Load packages---------------------------------------##
library(sf, quietly = TRUE)
library(tidyverse,quietly = TRUE)
library(qs,quietly = TRUE)


## --------------------------------------------------------------------------------------##
#out_path: directory to save the final processed file.
out_path <- "D:/SWAT_Runs/cd_test/cd_processed_for_viz" #e.g.


## --------------------------Functions---------------------------------------##


process_rchout <- function(rchout_path){
  
  
  
  ## --------------------------Import & process subbasun outputs-------------------------------##
  
  header_names <- c("RCH","GIS","MON","AREAkm2","FLOW_INcms","FLOW_OUTcms","EVAPcms",
                    "TLOSScms",'SED_INtons', "SED_OUTtons", "SEDCONCmg/L","ORGN_INkg",  
                    "ORGN_OUTkg","ORGP_INkg", "ORGP_OUTkg", "NO3_INkg","NO3_OUTkg","NH4_INkg",
                    "NH4_OUTkg","NO2_INkg","NO2_OUTkg","MINP_INkg","MINP_OUTkg",
                    "CHLA_INkg",  "CHLA_OUTkg","CBOD_INkg","CBOD_OUTkg","DISOX_INkg", 
                    "DISOX_OUTkg", "SOLPST_INmg","SOLPST_OUTmg", "SORPST_INmg", "SORPST_OUTmg",
                    "REACTPSTmg","VOLPSTmg","SETTLPSTmg", "RESUSP_PSTmg", "DIFFUSEPSTmg",
                    "REACBEDPSTmg", "BURYPSTmg", "BED_PSTmg", "BACTP_OUTct","BACTLP_OUTct",
                    "CMETAL#1kg", "CMETAL#2kg", "CMETAL#3kg","TOTNkg",  "TOTPkg", "NO3ConcMg/l",
                    "WTMPdegc")
  
  mutate_vars <- c("FLOW_INcms","FLOW_OUTcms","EVAPcms",
                   "TLOSScms",'SED_INtons', "SED_OUTtons", "SEDCONCmg/L","ORGN_INkg",  
                   "ORGN_OUTkg","ORGP_INkg", "ORGP_OUTkg", "NO3_INkg","NO3_OUTkg","NH4_INkg",
                   "NH4_OUTkg","NO2_INkg","NO2_OUTkg","MINP_INkg","MINP_OUTkg",
                   "CHLA_INkg",  "CHLA_OUTkg","CBOD_INkg","CBOD_OUTkg","DISOX_INkg", 
                   "DISOX_OUTkg", "SOLPST_INmg","SOLPST_OUTmg", "SORPST_INmg", "SORPST_OUTmg",
                   "REACTPSTmg","VOLPSTmg","SETTLPSTmg", "RESUSP_PSTmg", "DIFFUSEPSTmg",
                   "REACBEDPSTmg", "BURYPSTmg", "BED_PSTmg", "BACTP_OUTct","BACTLP_OUTct",
                   "CMETAL#1kg", "CMETAL#2kg", "CMETAL#3kg","TOTNkg",  "TOTPkg", "NO3ConcMg/l",
                   "WTMPdegc")
  
  rch_df <- data.table::fread(rchout_path,
                              skip = 9,fill = T)
  
  rch_df <- rch_df %>% dplyr::select(c(V2:V51))
  
  colnames(rch_df) <- header_names
  
  rch_df <-rch_df %>% dplyr::select(RCH,all_of(mutate_vars))  %>% 
    group_by(RCH) %>% summarise_all(mean) %>%
    dplyr::mutate(Watershed = sapply(str_split(rchout_path, "/"), tail, 5)[1],
                  Scenario = sapply(str_split(rchout_path, "/"), tail, 5)[3])
  
  print(paste0("processing:", rchout_path))
  
  return(rch_df)
  
  
}


bind_rchout_dfs <- function(filelst){
  #filelst: is the list of all out files to be processed
  df<- dplyr::bind_rows(lapply(filelst, process_rchout))
  
  return(df)
  
}



## --------------------------Processing---------------------------------------##

# list of output.sub files to be
# Please provide project directory/ watershed directory under which 
#Scenarios and Watershed folder are nested 
# (important that this path isn't passed ending with "/")


dirpath<- "D:/SWAT_Runs/cd_test/WE38" # e.g. path

filenames <- list.files(dirpath, 
                        pattern="output.rch",
                        recursive = T,
                        full.names = T)


rchout_dfs <- bind_rchout_dfs(filenames)
# rchout_sf <- rch_shp(dirpath)




## --------------------------------------WRITE OUTPUTS------------------------------------------------##

saveRDS(rchout_dfs, file = paste0(out_path, "/summary_output_rch.RDS"))
qs::qsave(rchout_dfs, file = paste0(out_path, "/summary_output_rch.qs"))
write.csv(rchout_dfs, file = paste0(out_path, "/summary_output_rch.csv"),row.names = FALSE)
