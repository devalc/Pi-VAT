## --------------------------------------------------------------------------------------##
##
## Script name:
##
## Purpose of the script: summarize swat subbasin annual results.This example script processes 
## multiple scenarios for one watershed. Can be adapted for multi-watersheds under
## which multiple scenarios might be nested.
##
## Author: Chinmay Deval
##
## Created On: 2021-03-22
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
library(qs)

## --------------------------------------------------------------------------------------##
#out_path: directory to save the final processed file.
out_path <- "D:/SWAT_Runs/cd_test/cd_processed_for_viz" #e.g.


## --------------------------Functions---------------------------------------##


process_subout <- function(subout_path){
  
  
  
  ## --------------------------Import & process subbasun outputs-------------------------------##
  
  header_names <- c("SUB","GIS","MON","AREAkm2","PRECIPmm", "SNOMELTmm",  "PETmm",
                    "ETmm", "SWmm","PERCmm","SURQmm","GW_Qmm","WYLDmm","SYLDt_ha", "ORGNkg_ha",
                    "ORGPkg_ha", "NSURQkg_ha", "SOLPkg_ha", "SEDPkg_ha", "LATQ_mm", "LATNO3kg_ha",
                    "GWNO3kg_ha", "CHOLAmic_L", "CBODUmg_L", "DOXQmg_L", "TNO3kg_ha",   "QTILEmm", "TVAPkg_ha")
  
  mutate_vars <- c("PRECIPmm", "SNOMELTmm",  "PETmm",
                   "ETmm", "SWmm","PERCmm","SURQmm","GW_Qmm","WYLDmm","SYLDt_ha", "ORGNkg_ha",
                   "ORGPkg_ha", "NSURQkg_ha", "SOLPkg_ha", "SEDPkg_ha", "LATQ_mm", "LATNO3kg_ha",
                   "GWNO3kg_ha", "CHOLAmic_L", "CBODUmg_L", "DOXQmg_L", "TNO3kg_ha",   "QTILEmm", "TVAPkg_ha")
  
  sub_df <- data.table::fread(subout_path,
                              skip = 9,fill = T)
  
  sub_df <- sub_df %>%mutate(V4_new = sapply(str_split(V4,pattern = "\\."), `[`, 1),
                             V5_new = sapply(str_split(V4,pattern = "\\."), `[`, 2)
  )%>% dplyr::select(-V4) %>% dplyr::select(c(V2:V3),V4_new, V5_new, c(V5:V28))
  
  colnames(sub_df) <- header_names
  
  sub_df <-sub_df %>% dplyr::select(SUB,all_of(mutate_vars))  %>% 
    group_by(SUB) %>% summarise_all(mean) %>%
    dplyr::mutate(Watershed = sapply(str_split(subout_path, "/"), tail, 5)[1],
                  Scenario = sapply(str_split(subout_path, "/"), tail, 5)[3])
  
  print(paste0("processing:", subout_path))
  
  return(sub_df)
  
  
}


bind_subout_dfs <- function(filelst){
  #filelst: is the list of all out files to be processed
   df<- dplyr::bind_rows(lapply(filelst, process_subout))
   
   return(df)
  
  }

sub_shp <- function(path){
  #path: This is the project directory/ watershed directory under which 
  #Scenarios and Watershed folder are nested (important that this path isn't passed ending with "/")
  #e.g. D:/SWAT_Runs/cd_test/WE38
  
  sub_sf <- sf::st_read(paste0(path, "/Watershed/Shapes/Sub.shp"))
  sub_sf<- sub_sf %>% st_transform(4326)
  return(sub_sf)
  
}

## --------------------------Processing---------------------------------------##

# list of output.sub files to be
# Please provide project directory/ watershed directory under which 
#Scenarios and Watershed folder are nested 
# (important that this path isn't passed ending with "/")


dirpath<- "D:/SWAT_Runs/cd_test/WE38" # e.g. path

filenames <- list.files(dirpath, 
                        pattern="output.sub",
                        recursive = T,
                        full.names = T)


subout_dfs <- bind_subout_dfs(filenames)
subout_sf <- sub_shp(dirpath)


## --------------------------custom steps since I am not sure if these are standard input files---------------------------------------##

subout_sf <- subout_sf %>% dplyr::select(Subbasin) %>%
  dplyr::rename("SUB" = "Subbasin")

subout_df_with_sf <- dplyr::full_join(subout_dfs,subout_sf, by=c("SUB"))

# convert to a sf object
subout_df_with_sf <- subout_df_with_sf %>% sf::st_as_sf()

## --------------------------------------WRITE OUTPUTS------------------------------------------------##

saveRDS(subout_df_with_sf, file = paste0(out_path, "/summary_output_sub.RDS"))
qs::qsave(subout_df_with_sf, file = paste0(out_path, "/summary_output_sub.qs"))
# sf::st_write(subout_df_with_sf, paste0(out_path, "/summary_output_sub.csv"),append=FALSE)
