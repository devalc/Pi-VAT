## --------------------------------------------------------------------------------------##
##
## 
##
## Purpose of the script: summarize swat HRU results. This example script processes 
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


## --------------------------clear environment and console-------------------------------##
rm(list = ls())
cat("\014")

## ----------------------------------Load packages---------------------------------------##
library(sf, quietly = TRUE)
library(tidyverse,quietly = TRUE)
library(qs)

## --------------------------------------------------------------------------------------##
#out_path: directory to save the final processed file.
out_path <- "D:/SWAT_Runs/cd_test/cd_processed_for_viz"


## --------------------------Functions---------------------------------------##


process_hruout <- function(hruout_path){
  
  
  
  ## --------------------------Import & process subbasun outputs-------------------------------##
  
  header_names <- c("LULC", "HRU", "GIS","SUB", " MGT",  "MON",   "AREAkm2",  "PRECIPmm",
                    "SNOFALLmm", "SNOMELTmm", "IRRmm","PETmm","ETmm", "SW_INITmm", "SW_ENDmm",
                    "PERCmm", "GW_RCHGmm", "DA_RCHGmm", "REVAPmm",  "SA_IRRmm", "DA_IRRmm",
                    "SA_STmm",   "DA_STmm", "SURQ_GENmm","SURQ_CNTmm", "TLOSSmm", "LATQGENmm",
                    "GW_Qmm", "WYLDmm", "DAILYCN", "TMP_AVdgC", "TMP_MXdgC", "TMP_MNdgC", "SOL_TMPdgC",
                    "SOLARMJ_m2",  "SYLDt_ha",  "USLEt_ha", "N_APPkg_ha", "P_APPkg_ha",
                    "NAUTOkg_ha", "PAUTOkg_ha", "NGRZkg_ha", "PGRZkg_ha", "NCFRTkg_ha", "PCFRTkg_ha",
                    "NRAINkg_ha", "NFIXkg_ha", "F_MNkg_ha", "A_MNkg_ha", "A_SNkg_ha", "F_MPkg_ha",
                    "AO_LPkg_ha", "L_APkg_ha", "A_SPkg_ha", "DNITkg_ha",  "NUPkg_ha",  "PUPkg_ha",
                    "ORGNkg_ha", "ORGPkg_ha", "SEDPkg_ha", "NSURQkg_ha", "NLATQkg_ha",
                    "NO3Lkg_ha", "NO3GWkg_ha", "SOLPkg_ha", "P_GWkg_ha",  "W_STRS", "TMP_STRS",  "N_STRS",
                    "P_STRS",  "BIOMt_ha",       "LAI",   "YLDt_ha",  "BACTPct",   "BACTLPct", "unknown1", "unknown2",
                    "SNOmm", "CMUPkg_ha", "CMTOTkg_ha",   "QTILEmm", "TNO3kg_ha", "LNO3kg_ha",
                    "GW_Q_Dmm", "LATQCNTmm", "TVAPkg_ha")
  
  mutate_vars <- c("GIS","PRECIPmm",
                   "SNOFALLmm", "SNOMELTmm", "IRRmm","PETmm","ETmm", "SW_INITmm", "SW_ENDmm",
                   "PERCmm", "GW_RCHGmm", "DA_RCHGmm", "REVAPmm",  "SA_IRRmm", "DA_IRRmm",
                   "SA_STmm",   "DA_STmm", "SURQ_GENmm","SURQ_CNTmm", "TLOSSmm", "LATQGENmm",
                   "GW_Qmm", "WYLDmm", "DAILYCN", "TMP_AVdgC", "TMP_MXdgC", "TMP_MNdgC", "SOL_TMPdgC",
                   "SOLARMJ_m2",  "SYLDt_ha",  "USLEt_ha", "N_APPkg_ha", "P_APPkg_ha",
                   "NAUTOkg_ha", "PAUTOkg_ha", "NGRZkg_ha", "PGRZkg_ha", "NCFRTkg_ha", "PCFRTkg_ha",
                   "NRAINkg_ha", "NFIXkg_ha", "F_MNkg_ha", "A_MNkg_ha", "A_SNkg_ha", "F_MPkg_ha",
                   "AO_LPkg_ha", "L_APkg_ha", "A_SPkg_ha", "DNITkg_ha",  "NUPkg_ha",  "PUPkg_ha",
                   "ORGNkg_ha", "ORGPkg_ha", "SEDPkg_ha", "NSURQkg_ha", "NLATQkg_ha",
                   "NO3Lkg_ha", "NO3GWkg_ha", "SOLPkg_ha", "P_GWkg_ha",  "W_STRS", "TMP_STRS",  "N_STRS",
                   "P_STRS",  "BIOMt_ha",       "LAI",   "YLDt_ha",  "BACTPct",   "BACTLPct", "unknown1", "unknown2",
                   "SNOmm", "CMUPkg_ha", "CMTOTkg_ha",   "QTILEmm", "TNO3kg_ha", "LNO3kg_ha",
                   "GW_Q_Dmm", "LATQCNTmm", "TVAPkg_ha")
  
  hru_df <- data.table::fread(hruout_path,
                              skip = 9,fill = T)
  
  hru_df <- hru_df %>%mutate(V75_new = as.numeric(sapply(str_split(V75,pattern = "\\-"), `[`, 1)),
                                V76_b =as.numeric(sapply(str_split(V75,pattern = "\\-"), `[`, 2)),
                                V76_new = case_when(V85 == "0" ~ V76, TRUE ~ V76_b),
                                V77_new = case_when(V85 == "0" ~ V77, TRUE ~ V76),
                                V78_new = case_when(V85 == "0" ~ V78, TRUE ~ V77),
                                V79_new = case_when(V85 == "0" ~ V79, TRUE ~ V78),
                                V80_new = case_when(V85 == "0" ~ V80, TRUE ~ V79),
                                V81_new = case_when(V85 == "0" ~ V81, TRUE ~ V80),
                                V82_new = case_when(V85 == "0" ~ V82, TRUE ~ V81),
                                V83_new = case_when(V85 == "0" ~ V83, TRUE ~ V82),
                                V84_new = case_when(V85 == "0" ~ V84, TRUE ~ V83),
                                V85_new = case_when(V85 == "0" ~ V85, TRUE ~ V84),
                                V6_new = sapply(str_split(V6,pattern = "\\."), `[`, 1),
                                V7_new = sapply(str_split(V6,pattern = "\\."), `[`, 2)
  ) %>% dplyr::select(-c(V75:V85),-V76_b, -V6) %>% dplyr::select( c(V1:V5, V6_new,V7_new, V7:V74, V75_new,
                                                                    V76_new,V77_new,V78_new,V79_new,
                                                                    V80_new,V81_new,V82_new,V83_new,V84_new,V85_new))%>% dplyr::rename("V6"= "V6_new",
                                                                                                                                       "V75" = "V75_new",
                                                                                                                                       "V76" = "V76_new",
                                                                                                                                       "V77" = "V77_new",
                                                                                                                                       "V78" = "V78_new",
                                                                                                                                       "V79" = "V79_new",
                                                                                                                                       "V80" = "V80_new",
                                                                                                                                       "V81" = "V81_new",
                                                                                                                                       "V82" = "V82_new",
                                                                                                                                       "V83" = "V83_new",
                                                                                                                                       "V84" = "V84_new",
                                                                                                                                       "V85" = "V85_new"
                                                                    )
  colnames(hru_df) <- header_names
  
  print(paste0("processing:", hruout_path))
  firstMON <- hru_df %>% dplyr::distinct(MON)%>% dplyr::pull()%>% dplyr::first()
  luinfo <- hru_df %>% dplyr::filter(MON==firstMON)%>%dplyr::select(GIS, LULC)
  
  hru_df <-hru_df %>% dplyr::select(GIS,all_of(mutate_vars))  %>% 
    group_by(GIS) %>% summarise_all(mean) %>%
    dplyr::mutate(Watershed = sapply(str_split(hruout_path, "/"), tail, 5)[1],
                  Scenario = sapply(str_split(hruout_path, "/"), tail, 5)[3])
  
  hru_df <- dplyr::left_join(hru_df,luinfo,by= "GIS")
  
  return(hru_df)
  
  
}


bind_hruout_dfs <- function(filelst){
  #filelst: is the list of all out files to be processed
  df<- dplyr::bind_rows(lapply(filelst, process_hruout))
  
  return(df)
  
}

hru_shp <- function(path){
  #path: This is the project directory/ watershed directory under which 
  #Scenarios and Watershed folder are nested (important that this path isn't passed ending with "/")
  #e.g. D:/SWAT_Runs_From_ZachEaston/cd_test/WE38
  
  hru_sf <- sf::st_read(paste0(path, "/Watershed/Shapes/HRU.shp"))
  hru_sf<- hru_sf %>% st_transform(4326)
  return(hru_sf)
  
}

## --------------------------Processing---------------------------------------##

# list of output.sub files to be
# Please provide project directory/ watershed directory under which 
#Scenarios and Watershed folder are nested 
# (important that this path isn't passed ending with "/")
# 


dirpath<- "D:/SWAT_Runs/cd_test/WE38"  #example path

filenames <- list.files(dirpath, 
                        pattern="output.hru",
                        recursive = T,
                        full.names = T)


hruout_dfs <- bind_hruout_dfs(filenames)
hruout_sf <- hru_shp(dirpath)


## -------------------------------------------------------------------------------------------##

hruout_sf <- hruout_sf %>% dplyr::select(HRUGIS,MEAN_SLOPE, AREA, SUBBASIN, LU_CODE, SOIL_CODE) %>%
  dplyr::rename("GIS" = "HRUGIS") 
hruout_sf$GIS <- as.numeric(hruout_sf$GIS)

hruout_df_with_sf <- dplyr::left_join(hruout_dfs,hruout_sf, by=c("GIS"))

# convert to a sf object
hruout_df_with_sf <- hruout_df_with_sf %>% sf::st_as_sf()
 

## --------------------------------------WRITE OUTPUTS------------------------------------------------##

saveRDS(hruout_df_with_sf, file = paste0(out_path, "/summary_output_hru.RDS"))
qs::qsave(hruout_df_with_sf, file = paste0(out_path, "/summary_output_hru.qs"))