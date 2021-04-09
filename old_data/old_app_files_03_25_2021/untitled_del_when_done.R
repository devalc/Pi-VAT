library(tidyverse)
a <-  qs::qread("data/Palouse202103_shps_subcatchments_wgs84_split_wshed_and_scen.qs")
colnames(a)

b <- readRDS("data/portland202009_shps_subcatchments_wgs84_split_wshed_and_scen.RDS")

a <- a %>% dplyr::select(colnames(b))

spsub_a <- a %>%
    dplyr::filter(Watershed == "Kamiache_Creek") 

spsub_lt <- b %>%
    dplyr::filter(Watershed == "BlazedAlder") 


Spatial_subset_base <- spsub_a %>%
        dplyr::filter(Watershed  == "Kamiache_Creek" &
                          Scenario == "Conventional_Till")%>% 
        arrange_at(.vars = "SoLs_kg_ha", desc) %>%
        mutate(cumPercArea = cumsum(area_ha_) / sum(area_ha_) *
                   100) %>% dplyr::mutate_if(is.numeric, round, 2) 

Spatial_subset_base_lt <- spsub_lt %>%
    dplyr::filter(Watershed  == "BlazedAlder" &
                      Scenario == "CurCond_202009_cl532")%>% 
    arrange_at(.vars = "SoLs_kg_ha", desc) %>%
    mutate(cumPercArea = cumsum(area_ha_) / sum(area_ha_) *
               100) %>% dplyr::mutate_if(is.numeric, round, 2) 



tm_shape(Spatial_subset_base, name = "Baseline Scenario") +
    # tm_borders(lwd = 0, alpha=0.0) +
    tmap::tm_polygons(
        "SoLs_kg_ha",
        id = "watershed",
        palette = "-inferno",
        # style = "log10_pretty"
        style = "fixed",
        breaks = c(0, 1, 10, 100, 1000,
                   5000, 10000, 15000,20000,Inf),
        title = " "
    )

    # req(Spatial_data())
    # req(Spatial_subset_comp())
    # req(input$S_scen_comp)
    # req(input$S_variable)
    # req(Spatial_subset_base())
    # req(input$S_scen_base)
    tm2 <-tm_shape(Spatial_subset_comp(), name = "Difference between comparison & baseline scenario") +
        tmap::tm_polygons(
            paste0("AbsChange_", input$S_variable),
            id = "watershed",
            palette = "-viridis",
            # style = "log10_pretty",
            style = "fixed",
            breaks = c(0, 1, 10, 100, 1000,
                       5000, 10000, 15000,20000,Inf),
            title = "Comparison minus Baseline"
        )+
        tm_shape(Spatial_subset_base(), name = "Baseline Scenario") +
        # tm_borders(lwd = 0, alpha=0.0) +
        tmap::tm_polygons(
            input$S_variable,
            id = "watershed",
            palette = "-inferno",
            # style = "log10_pretty"
            style = "fixed",
            breaks = c(0, 1, 10, 100, 1000,
                       5000, 10000, 15000,20000,Inf),
            title = " "
        )+
        tm_shape(Spatial_subset_comp(), name = "Comparison Scenario") +
        tmap::tm_polygons(
            input$S_variable,
            id = "watershed",
            palette = "-inferno",
            legend.hist = TRUE,
            # style = "log10_pretty",
            style = "fixed",
            breaks = c(0, 1, 10, 100, 1000,
                       5000, 10000, 15000,20000,Inf),
            legend.show = FALSE,
        ) +
        tmap::tm_layout(scale = 0.1,
                        title = "")
    
    tmap_leaflet(tm2,in.shiny = TRUE)  %>%
        addMiniMap(tiles = providers$Esri.WorldStreetMap,
                   toggleDisplay = TRUE,
                   zoomAnimation = TRUE,position = "bottomleft",height = 100)












Spatial_data_rel_lt<- spsub_lt%>%
    dplyr::filter(Scenario != "CurCond_202009_cl532") %>%
    dplyr::mutate(AbsChange_SoLs_kg_ha = SoLs_kg_ha- spsub_lt$SoLs_kg_ha[spsub_lt$Scenario=="CurCond_202009_cl532"],
                  AbsChange_SdDp_kg_ha = SdDp_kg_ha- spsub_lt$SdDp_kg_ha[spsub_lt$Scenario=="CurCond_202009_cl532"],
                  AbsChange_SdYd_kg_ha = SdYd_kg_ha- spsub_lt$SdYd_kg_ha[spsub_lt$Scenario=="CurCond_202009_cl532"],
                  AbsChange_SRP_kg_ha_ = SRP_kg_ha_- spsub_lt$SRP_kg_ha_[spsub_lt$Scenario=="CurCond_202009_cl532"],
                  AbsChange_PP_kg_ha_ = PP_kg_ha_- spsub_lt$PP_kg_ha_[spsub_lt$Scenario=="CurCond_202009_cl532"],
                  AbsChange_TP_kg_ha_ = TP_kg_ha_- spsub_lt$TP_kg_ha_[spsub_lt$Scenario=="CurCond_202009_cl532"],
                  AbsChange_Runoff_mm_ = Runoff_mm_- spsub_lt$Runoff_mm_[spsub_lt$Scenario=="CurCond_202009_cl532"],
                  AbsChange_DepLos_kg_ = DepLos_kg_- spsub_lt$DepLos_kg_[spsub_lt$Scenario=="CurCond_202009_cl532"],
    )


Spatial_data_rel_a<- spsub_a%>%
    dplyr::filter(Scenario != "No_Till") %>%
    dplyr::mutate(AbsChange_SoLs_kg_ha = SoLs_kg_ha- spsub_a$SoLs_kg_ha[spsub_a$Scenario=="No_Till"]
                  # AbsChange_SdDp_kg_ha = SdDp_kg_ha- spsub_a$SdDp_kg_ha[spsub_a$Scenario=="No_Till"],
                  # AbsChange_SdYd_kg_ha = SdYd_kg_ha- spsub_a$SdYd_kg_ha[spsub_a$Scenario=="No_Till"],
                  # AbsChange_SRP_kg_ha_ = SRP_kg_ha_- spsub_a$SRP_kg_ha_[spsub_a$Scenario=="No_Till"],
                  # AbsChange_PP_kg_ha_ = PP_kg_ha_- spsub_a$PP_kg_ha_[spsub_a$Scenario=="No_Till"],
                  # AbsChange_TP_kg_ha_ = TP_kg_ha_- spsub_a$TP_kg_ha_[spsub_a$Scenario=="No_Till"],
                  # AbsChange_Runoff_mm_ = Runoff_mm_- spsub_a$Runoff_mm_[spsub_a$Scenario=="No_Till"],
                  # AbsChange_DepLos_kg_ = DepLos_kg_- spsub_a$DepLos_kg_[spsub_a$Scenario=="No_Till"],
    )



Spatial_data_rel_a<- spsub_a%>% group_by(TopazID) %>%
    dplyr::filter(Scenario != "No_Till") %>% 
    mutate(deltac = SoLs_kg_ha -SoLs_kg_ha[spsub_a$Scenario=="No_Till"])%>%
    ungroup()



test<- spsub_a %>% group_by(TopazID) %>% mutate(deltaC = SdYd_kg_ha - SdYd_kg_ha[Scenario == 'Conventional_Till'])


hill_subset_rel <- reactive({
    hill_subset() %>%
        dplyr::filter(Scenario != input$Hill_scen_base) %>%
        dplyr::mutate(
            AbsChange_Runoff..mm. = Runoff..mm. - hill_subset()$Runoff..mm.[hill_subset()$Scenario ==
                                                                                input$Hill_scen_base],
            AbsChange_Lateral.Flow..mm. = Lateral.Flow..mm. - hill_subset()$Lateral.Flow..mm.[hill_subset()$Scenario ==
                                                                                                  input$Hill_scen_base],
            AbsChange_Baseflow..mm.  = Baseflow..mm. - hill_subset()$Baseflow..mm.[hill_subset()$Scenario ==
                                                                                       input$Hill_scen_base],
            AbsChange_Soil_Loss_kg = Soil_Loss_kg - hill_subset()$Soil_Loss_kg[hill_subset()$Scenario ==
                                                                                   input$Hill_scen_base],
            AbsChange_Sediment_Deposition_kg = Sediment_Deposition_kg - hill_subset()$Sediment_Deposition_kg[hill_subset()$Scenario ==
                                                                                                                 input$Hill_scen_base],
            AbsChange_Sediment_Yield_kg = Sediment_Yield_kg - hill_subset()$Sediment_Yield_kg[hill_subset()$Scenario ==
                                                                                                  input$Hill_scen_base],
            AbsChange_Soluble_Reactive_P_kg = Soluble_Reactive_P_kg - hill_subset()$Soluble_Reactive_P_kg[hill_subset()$Scenario ==
                                                                                                              input$Hill_scen_base],
            AbsChange_Particulate_P_kg = Particulate_P_kg - hill_subset()$Particulate_P_kg[hill_subset()$Scenario ==
                                                                                               input$Hill_scen_base],
            AbsChange_Total_P_kg = Total_P_kg - hill_subset()$Total_P_kg[hill_subset()$Scenario ==
                                                                             input$Hill_scen_base],
            AbsChange_Particle_Class_1_Fraction_kg = Particle_Class_1_Fraction_kg - hill_subset()$Particle_Class_1_Fraction_kg[hill_subset()$Scenario ==
                                                                                                                                   input$Hill_scen_base],
            AbsChange_Particle_Class_2_Fraction_kg = Particle_Class_2_Fraction_kg - hill_subset()$Particle_Class_2_Fraction_kg[hill_subset()$Scenario ==
                                                                                                                                   input$Hill_scen_base],
            AbsChange_Particle_Class_3_Fraction_kg = Particle_Class_3_Fraction_kg - hill_subset()$Particle_Class_3_Fraction_kg[hill_subset()$Scenario ==
                                                                                                                                   input$Hill_scen_base],
            AbsChange_Particle_Class_4_Fraction_kg = Particle_Class_4_Fraction_kg - hill_subset()$Particle_Class_4_Fraction_kg[hill_subset()$Scenario ==
                                                                                                                                   input$Hill_scen_base],
            AbsChange_Particle_Class_5_Fraction_kg = Particle_Class_5_Fraction_kg - hill_subset()$Particle_Class_5_Fraction_kg[hill_subset()$Scenario ==
                                                                                                                                   input$Hill_scen_base],
            AbsChange_Particle_Fraction_Under_0.016_mm_kg = Particle_Fraction_Under_0.016_mm_kg - hill_subset()$Particle_Fraction_Under_0.016_mm_kg[hill_subset()$Scenario ==
                                                                                                                                                        input$Hill_scen_base],
            AbsChange_Sediment_Yield_of_Particles_Under_0.016_mm_kg = Sediment_Yield_of_Particles_Under_0.016_mm_kg - hill_subset()$Sediment_Yield_of_Particles_Under_0.016_mm_kg[hill_subset()$Scenario ==
                                                                                                                                                                                      input$Hill_scen_base]
        )
})





# a <- a %>%sf::st_set_crs(value = 4326)
# 
# 
# scens = unique(a$Scenario)
# wsheds = unique(a$Watershed)
# vars  <- colnames(a)
# 
# 
# 
# Spatial_subset <- a %>% dplyr::filter(Watershed == "Kamiache_Creek" & Scenario == "Conventional_Till")%>% 
#     arrange_at(.vars = "SdYd_kg_ha", desc) %>%
#     mutate(cumPercArea = cumsum(area_ha_) / sum(area_ha_) *100) %>% 
#     dplyr::mutate_if(is.numeric, round, 2)
# 
# Spatial_subset_base <- a %>%
#         dplyr::filter(Watershed == "Kamiache_Creek" & Scenario == "Mulch_Till")%>% 
#         arrange_at(.vars = "SdYd_kg_ha", desc) %>%
#         mutate(cumPercArea = cumsum(area_ha_) / sum(area_ha_) *
#                    100) %>% dplyr::mutate_if(is.numeric, round, 2) 
# 
# spsub <- a %>%
#         dplyr::filter(Watershed == "Kamiache_Creek") 
# 
# Spatial_data_rel<- spsub %>%
#         dplyr::filter(Scenario != "Conventional_Till") %>%
#         dplyr::mutate(AbsChange_SoLs_kg_ha = SoLs_kg_ha- spsub$SoLs_kg_ha[spsub$Scenario=="Conventional_Till"],
#                       AbsChange_SdDp_kg_ha = SdDp_kg_ha- spsub$SdDp_kg_ha[spsub$Scenario=="Conventional_Till"],
#                       AbsChange_SdYd_kg_ha = SdYd_kg_ha- spsub$SdYd_kg_ha[spsub$Scenario=="Conventional_Till"],
#                       AbsChange_SRP_kg_ha_ = SRP_kg_ha_- spsub$SRP_kg_ha_[spsub$Scenario=="Conventional_Till"],
#                       AbsChange_PP_kg_ha_ = PP_kg_ha_- spsub$PP_kg_ha_[spsub$Scenario=="Conventional_Till"],
#                       AbsChange_TP_kg_ha_ = TP_kg_ha_- spsub$TP_kg_ha_[spsub$Scenario=="Conventional_Till"],
#                       AbsChange_Runoff_mm_ = Runoff_mm_- spsub$Runoff_mm_[spsub$Scenario=="Conventional_Till"],
#                       AbsChange_DepLos_kg_ = DepLos_kg_- spsub$DepLos_kg_[spsub$Scenario=="Conventional_Till"],
#         )
# 
# 
# spsub%>%
#     dplyr::filter(Watershed == "Kamiache_Creek") %>%
#     dplyr::filter(Scenario != "Conventional_Till")%>%
#     dplyr::mutate(AbsChange_SoLs_kg_ha = SoLs_kg_ha - 
#                       dplyr::filter(., Scenario=="Conventional_Till")%>% 
#                       pull(SoLs_kg_ha))
# 
#     # dplyr::mutate(AbsChange_SoLs_kg_ha= SoLs_kg_ha- spsub$SoLs_kg_ha[spsub$Scenario=="Conventional_Till"])
# 
# spsub_mt <- spsub%>%
#     dplyr::filter(Watershed == "Kamiache_Creek") %>%
#     dplyr::filter(Scenario == "Mulch_Till")
# 
# 
# b <- qs::qread("data/lt_202010_shps_subcatchments_wgs84_split_wshed_and_scen.qs")
# # colnames(b)
# # 
# # 
# # setdiff(colnames(a), colnames(b))
# "56_General_Creek"
# "CurCond" 
# "PrescFire"

spsub_lt <- b %>%
    dplyr::filter(Watershed == "56_General_Creek") 


Spatial_data_rel_lt<- spsub_lt%>%
    dplyr::filter(Scenario != "CurCond_202009_cl532") %>%
    dplyr::mutate(AbsChange_SoLs_kg_ha = SoLs_kg_ha- spsub_lt$SoLs_kg_ha[spsub_lt$Scenario=="CurCond"],
                  AbsChange_SdDp_kg_ha = SdDp_kg_ha- spsub_lt$SdDp_kg_ha[spsub_lt$Scenario=="CurCond"],
                  AbsChange_SdYd_kg_ha = SdYd_kg_ha- spsub_lt$SdYd_kg_ha[spsub_lt$Scenario=="CurCond"],
                  AbsChange_SRP_kg_ha_ = SRP_kg_ha_- spsub_lt$SRP_kg_ha_[spsub_lt$Scenario=="CurCond"],
                  AbsChange_PP_kg_ha_ = PP_kg_ha_- spsub_lt$PP_kg_ha_[spsub_lt$Scenario=="CurCond"],
                  AbsChange_TP_kg_ha_ = TP_kg_ha_- spsub_lt$TP_kg_ha_[spsub_lt$Scenario=="CurCond"],
                  AbsChange_Runoff_mm_ = Runoff_mm_- spsub_lt$Runoff_mm_[spsub_lt$Scenario=="CurCond"],
                  AbsChange_DepLos_kg_ = DepLos_kg_- spsub_lt$DepLos_kg_[spsub_lt$Scenario=="CurCond"],
    )
