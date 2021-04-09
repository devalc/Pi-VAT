## --------------------------------------------------------------------------------------##
##
## Script name: app.R
##
## Purpose of the script: Script to ingest and visualize WEPPcloud simulations so as to 
##  support targeted management
##
## @author: Chinmay Deval
##
## Created on Fri Jan 17 19:35:48 2020
##
## Copyright (c) Chinmay Deval, 2020
## Email: chinmay.deval91@gmail.com
##
## --------------------------------------------------------------------------------------##
##    Notes:
##
##
## --------------------------------------------------------------------------------------##
## ----------------------------------Load packages---------------------------------------##



library(shiny,quietly = TRUE)
library(qs,quietly = TRUE)
library(tools,quietly = TRUE)
library(sf,quietly = TRUE)
library(echarts4r, quietly = TRUE)
library(tidyverse,quietly = TRUE)
library(shinythemes,quietly = TRUE)
library(shinycssloaders,quietly = TRUE)
library(shinyWidgets,quietly = TRUE)
library(plotly,quietly = TRUE)
library(stringr,quietly = TRUE)
library(leaflet,quietly = TRUE)
library(tmap,quietly = TRUE)
library(ggthemes,quietly = TRUE)
library(DT,quietly = TRUE)
library(shinyhelper,quietly = TRUE)
library(shinyalert,quietly = TRUE)
library(crosstalk,quietly = TRUE)
library(sever,quietly = TRUE)


source("global.R")


## ----------------------------------Init Options---------------------------------------##
options(shiny.maxRequestSize = 200 * 1024 ^ 2)




## ----------------------------------define UI------------------------------------------##
# 

ui <-navbarPage(title = div("Viz-WEPPcloud",
                            div(tags$a(href="https://forest.moscowfsl.wsu.edu/fswepp/", tags$img(src='FS.png',style="position:fixed;right: 105px;top: 5px;padding-bottom:10px;", height = 50)),
                                tags$a(href="https://www.uidaho.edu/", tags$img(src='UI.jpg',style="position:fixed;right: 150px;top: 5px;padding-bottom:10px;", height = 50)),
                                tags$a(href="https://nifa.usda.gov/", tags$img(src='nifa.jpg',style="position:fixed;right: 60px;top: 5px;padding-bottom:10px;", height = 50)),
                                tags$a(href= "https://github.com/devalc/Viz-WEPPCloud", tags$img(src="GitHub-Mark.png",style="position:fixed;right: 10px;top: 5px;padding-bottom:10px;", height = 50))
                            )),
                
                windowTitle = "Viz-WEPPcloud",
                position = "fixed-top",
                # fluid = TRUE,
                collapsible = TRUE,
                id = 'tabs',
                
                
                ## ----------------------------------Set Theme------------------------------------------##
                ## set the theme
                
                theme = "mytheme.css",
                
                
                
                ## ----------------------------------Start defining Tabs------------------------------------------##
                
                
                ## -----------------------------------------Landing Page---------------------------------------------##
                
                tabPanel(
                    "Home",
                    icon = icon("home"),
                    
                    tags$head(includeHTML((
                        "google-analytics.html"
                    ))),
                    
                    
                    setBackgroundColor(
                        color = c("#ffffff", "#46344E"),
                        gradient = "linear",
                        direction = "bottom"
                    ),
                    # setBackgroundImage(src = "bg_pixabay.jpg", shinydashboard = FALSE),
                    
                    use_sever(),
                    #h1("sever" )
                    
                    mainPanel(
                        fluidPage(#HTML("<br style = “line-height:10;”><br>"),
                            HTML("<br>"),
                            fluidRow(
                                column(
                                    12,
                                    offset = 3,
                                    align  = "center",
                                    
                                    HTML(
                                        '<div class="jumbotron" style="background-color:	#17141D;">
                                                      <h1  style="color:white;">Viz-WEPPcloud </h1>
                                                      <h4  style="color:white;">A post-processing tool for identifying and examining impacts of management on pollutant 
                                                      source areas in large spatially explicit watershed output files.</h4>
                                                      </div>'
                                    ) #  <a href="https://wepp1.nkn.uidaho.edu/weppcloud/" title="Description">WEPPcloud</a> 
                                    
                                )
                            )),
                        
                        fluidPage(#HTML("<br>"),
                            fluidRow(
                                column(
                                    12,
                                    offset = 3,
                                    align  = "center",
                                    column(4,
                                           align  = "center",
                                           thumbnail_label1(
                                               image = 'background.jpg',
                                               label = 'Watershed Analysis',
                                               content = "Inter-watershed comparison of impacts of management on annual water yield and 
                                      water quality at the watershed outlet"
                                           ),
                                           actionBttn("Wbutton", "Navigate to Watershed", icon = icon("line-chart"),style = "pill",
                                                      color = "warning")
                                    ),
                                    column(
                                        4,
                                        align  = "center",
                                        thumbnail_label1(
                                            image = 'hillslope_img.jpg',
                                            label = 'Hillslope Analysis',
                                            content = 'Identifying targeted pollutant hotspots within a watershed and quantifying the impacts of disturbance
                                      and management on the detachment and delivery of pollutants from these hotspots'
                                        ),
                                        actionBttn("Hbutton", "Navigate to Hillslope", icon = icon("line-chart"),style = "pill",
                                                   color = "warning")
                                    ),
                                    column(
                                        4,
                                        align  = "center",
                                        thumbnail_label1(
                                            image = 'spatial_imp.PNG',
                                            label = 'Spatial Visualization',
                                            content = 'Visualize hillslope scale output and targeted hotspots across multiple watersheds for multiple treatments.'
                                        ),
                                        actionBttn("Sbutton", "Navigate to Spatial-Viz", icon = icon("line-chart"),style = "pill",
                                                   color = "warning")
                                    )
                                )
                            )),
                        
                        
                        HTML("<br style = “line-height:30;”><br>"),
                        fluidPage(
                            fluidRow(
                                column(
                                    12,
                                    offset = 3,
                                    align  = "center",
                                    style = "height:140px;background-color:#17141D;padding-left:20px;padding-top:20px;padding-bottom:20px;color:#ffffff",
                                    
                                    tags$div(
                                        tags$p(
                                            "viz-WEPPcloud is currently designed to analyze output from WEPPcloud and provides an option for users to upload their own output data files.",
                                            align = "center"
                                        ),
                                        tags$p(
                                            a(href = 'https://wepp1.nkn.uidaho.edu/weppcloud/', 'WEPPcloud', .noWS = "outside", style = "color:#FFAE42"),
                                            ' is a cloud based simulation tool based on the process based Watershed Erosion Prediction Project',
                                            tags$a(href="https://www.fs.usda.gov/ccrc/tools/watershed-erosion-prediction-project",
                                                   "WEPP",  style = "color:#FFAE42"), 'model. It estimates
                                                          hillslope soil erosion, runoff, and sediment yields from anywhere in the continental U.S. It is especially useful for
                                                          post-wildfire assessments, fuel treatment planning, and prescribed fire analysis.',
                                            .noWS = c("after-begin", "before-end"),
                                            align = "center"
                                        ),
                                        
                                    )
                                    
                                )
                            ),
                            HTML("<br style = “line-height:10;”><br>")
                        )
                        
                        
                        
                    ),
                    
                ),
                
                
                
                ## -----------------------------------------Watershed Tab---------------------------------------------##
                tabPanel(
                    "Watershed",
                    
                    tags$head(includeHTML((
                        "google-analytics.html"
                    ))),
                    
                    
                    
                    # tags$style(type = "text/css", "body {padding-top: 10px;}"),
                    
                    column(width = 12,
                           style = 'padding-top:0px;',
                           
                           useShinyalert(),  # Set up shinyalert
                           
                           sidebarPanel(
                               style = "position:fixed;width:inherit;margin-top: 0px;",
                               width = 3,
                               
                               awesomeRadio(
                                   inputId = "DefOrUserUpload_W",
                                   label = "Data Import Options:",
                                   choices = c(
                                       "Default Data (Portland)" = "Default_Data_Portland",
                                       "Default Data (Seattle)" = "Default_Data_Seattle",
                                       "Default Data (Lake Tahoe)" = "Default_Data_LT",
                                       "Default Data (Palouse)" = "Default_Data_Palouse",
                                       "Upload your own data" = "Upload data"
                                   ),
                                   selected = "Default_Data_LT",
                                   status = 'warning'
                               ),
                               
                               
                               # uiOutput("Wshed_selectfile"),
                               
                               uiOutput("W_FileInput"),
                               
                               awesomeRadio(
                                   inputId = "AreaVsScen",
                                   label = "Management/watershed Options: ",
                                   choices = c(
                                       "One Watershed, All Scenarios" = "allscen",
                                       "One Scenario, Selected Watersheds" =
                                           "allwat"
                                   ),
                                   selected = "allscen",
                                   status = 'warning'
                               ),
                               # %>% 
                               #     helper(icon = "question-circle", colour = "#FF0000",
                               #            content = "W_compare",
                               #            type = "markdown", size = "l",
                               #            buttonLabel = "Okay", easyClose = TRUE, fade = TRUE) ,
                               
                               uiOutput("Wshed_wshed_S"),
                               
                               
                               uiOutput("Wshed_wshed"),
                               
                               
                               uiOutput("wshed_var"),
                               
                               awesomeRadio(
                                   inputId = "ScenVvar",
                                   label = "Visualization type:",
                                   choices = c("Heatmap" = "Heatmap", "Bar Chart" =
                                                   "Bar Chart"),
                                   selected = "Heatmap",
                                   status = 'warning'
                               ),
                               
                               # img(src="vizweppcloud_hex_wsj.png",width="100%")
                               
                               
                               
                           ),
                           
                           # Main panel for displaying outputs ----
                           mainPanel(
                               width = 9,
                               style = 'padding-left:100px;padding-top:0px;padding-bottom:10px;',
                               fluidRow(
                                   column(
                                       9,
                                       offset = 0,
                                       plotlyOutput("Plot9", height = "700px", width =
                                                        "800px") %>% 
                                           withSpinner(type = 6 ,color = "#ffffff")
                                       
                                   ))
                           )
                    )
                ),
                
                
                ## -----------------------------------------Hillslope Tab---------------------------------------------##
                tabPanel(
                    "Hillslope",
                    
                    tags$head(includeHTML((
                        "google-analytics.html"
                    ))),
                    
                    tags$style(type = "text/css", "body {padding-top: 80px; }"),
                    
                    useShinyalert(),  # Set up shinyalert
                    fluidPage(
                        fluidRow(
                            
                            sidebarPanel(
                                # style = "position:fixed;width:inherit;",
                                style = "position:fixed;width:inherit;overflow-y:scroll;scroll-snap-type: y mandatory;height:600px",
                                width = 3,
                                
                                
                                awesomeRadio(
                                    inputId = "DefOrUserUpload_H",
                                    label = "Data Import Options:",
                                    choices = c(
                                        "Default Data (Portland)" = "Default_Data_Portland",
                                        "Default Data (Seattle)" = "Default_Data_Seattle",
                                        "Default Data (Lake Tahoe)" = "Default_Data_LT",
                                        "Default Data (Palouse)" = "Default_Data_Palouse",
                                        "Upload your own data" =
                                            "Upload data"
                                    ),
                                    selected = "Default_Data_LT",
                                    status = 'warning'
                                ),
                                
                                uiOutput("H_FileInput"),
                                uiOutput("Hill_selectfile"),
                                uiOutput("Hill_wshed"),
                                uiOutput("Hill_var"),
                                uiOutput("Hill_scen_base"),
                                uiOutput("Hill_scen_comp"),
                                
                                sliderInput(
                                    "thresh_H",
                                    "Zoom Threshold (%):",
                                    min = 0,
                                    max = 100,
                                    value = 100,
                                    step = NULL,
                                    round = TRUE,
                                    ticks = TRUE,
                                    animate = FALSE
                                )%>% 
                                    helper(icon = "question-circle", colour = "#FF0000",
                                           content = "H_plot_thresh",
                                           type = "markdown", size = "l",
                                           buttonLabel = "Okay", easyClose = TRUE, fade = TRUE),
                                
                                
                                # uiOutput("Hill_scen"),
                                
                                awesomeRadio(
                                    inputId = "summary_DT_by_var_H",
                                    label = "Summarize by:",
                                    choices = c(
                                        "Land Use" = "Landuse",
                                        "Soil Type" = "Soiltype",
                                        "Both" = "Both"
                                    ),
                                    selected = "Both",
                                    status = 'warning'
                                ),
                                
                            ),
                            
                            mainPanel(
                                width = 9,
                                style = 'padding:80px;',
                                fluidRow(
                                    column(
                                        6,
                                        align = "center",
                                        plotlyOutput("Plot_vs_cumPercArea") %>% withSpinner(type = 6 ,color = "#ffffff")
                                    ),
                                    column(
                                        6,
                                        align = "center",
                                        plotlyOutput("Plot_vs_cumPercArea_abs") %>% withSpinner(type = 6 ,color = "#ffffff")
                                    )
                                ),
                                # HTML("<br style = “line-height:5;”><br>"),
                                # uiOutput("Exp3_Exp4") %>% withSpinner(color =
                                #                                           "#0dc5c1"),
                                HTML("<br style = “line-height:5;”><br>"),
                                fluidRow(
                                    column(
                                        6,
                                        align = "center",
                                        plotlyOutput("Plot_vs_cumPercLen") %>% withSpinner(type = 6 ,color = "#ffffff")
                                    ),
                                    column(
                                        6,
                                        align = "center",
                                        plotlyOutput("Plot_vs_cumPercLen_abs") %>% withSpinner(type = 6 ,color = "#ffffff")
                                    )
                                ),
                                HTML("<br style = “line-height:5;”><br>"),
                                
                                
                                uiOutput("tab_H")
                                
                                
                            )
                        )
                    )
                ),
                
                ## -----------------------------------------Spatial-Viz Tab---------------------------------------------##
                
                tabPanel(
                    "Spatial-Viz",
                    
                    tags$head(includeHTML((
                        "google-analytics.html"
                    ))),
                    
                    tags$style(type = "text/css", "body {padding-top: 80px;}"),
                    
                    useShinyalert(),  # Set up shinyalert
                    
                    sidebarPanel(
                        style = "position:fixed;width:inherit;overflow-y:scroll;height:600px",
                        width = 3,
                        
                        awesomeRadio(
                            inputId = "DefOrUserUpload_S",
                            label = "Data Import Options:",
                            choices = c(
                                "Default Data (Portland)" = "Default_Data_Portland",
                                "Default Data (Seattle)" = "Default_Data_Seattle",
                                "Default Data (Lake Tahoe)" = "Default_Data_LT",
                                "Upload your own data" = "Upload data"
                            ),
                            selected = "Default_Data_LT",
                            status = 'warning'
                        ),
                        
                        
                        uiOutput("S_FileInput"),
                        uiOutput("S_FileInput_Chan"),
                        uiOutput("Spatial_wshed"),
                        uiOutput("S_var"),
                        uiOutput("Spatial_scen_base"),
                        uiOutput("Spatial_scen_comp"),
                        # uiOutput("Spatial_scen"),
                        
                        
                        sliderInput(
                            "thresh_S",
                            "Zoom Threshold (%):",
                            min = 0,
                            max = 100,
                            value = 100,
                            step = NULL,
                            round = TRUE,
                            ticks = TRUE,
                            animate = FALSE
                        )%>% 
                            helper(icon = "question-circle", colour = "#FF0000",
                                   content = "S_plot_thresh",
                                   type = "markdown", size = "l",
                                   buttonLabel = "Okay", easyClose = TRUE, fade = TRUE),
                        
                        
                        
                        sliderInput(
                            "thresh_slope_S",
                            "Slope Threshold:",
                            min = 0,
                            max = 1,
                            value = c(0,1),
                            step = NULL,
                            round = TRUE,
                            ticks = TRUE,
                            animate = FALSE
                        )%>% 
                            helper(icon = "question-circle", colour = "#FF0000",
                                   content = "S_slope_thresh",
                                   type = "markdown", size = "l",
                                   buttonLabel = "Okay", easyClose = TRUE, fade = TRUE),
                        
                        # awesomeRadio(
                        #     inputId = "summary_DT_by_var_S",
                        #     label = "Summarize by:",
                        #     choices = c(
                        #         "Land Use" = "Landuse",
                        #         "Soil Type" = "Soiltype",
                        #         "Both" = "Both"
                        #     ),
                        #     selected = "Both",
                        #     status = 'warning'
                        # )
                        
                        # img(src="vizweppcloud_hex_wsj.png",width="100%")
                        
                        # awesomeRadio(inputId = "showchan_S",label = "Display Channels?",
                        #              choices = c("Yes"="Yes","No"="No"), selected = "No")
                        #
                        
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                        width = 9,
                        style = 'padding:80px;',
                        
                        fluidRow(
                            #     column(6,
                            #                 # align = "center",
                            #                 # offset = 0,
                            # leaflet::leafletOutput("Plot11")%>%
                            #     withSpinner(type = 6 ,color = "#ffffff")
                            # ),
                            column(12,
                                   # align = "center",
                                   offset = 0, 
                                   leaflet::leafletOutput("Plot12")%>%
                                       withSpinner(type = 6 ,color = "#ffffff")
                            )
                            
                        ),
                        HTML("<br style = “line-height:5;”><br>"),
                        
                        uiOutput("tab_sp") %>%
                            withSpinner(type = 6 ,color = "#ffffff")
                        
                    )
                    
                    
                ),
                
                
                br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
                
                # header = column(12, "this is a header"),
                
)

## --------------------------------------------------------------------------------------##
## ----------------------------------define server logic------------------------------------------##
## --------------------------------------------------------------------------------------##

server <- function(input, output, session) {
    
    sever(html = disconnected, bg_color = "#000")
    
    observe({
        if (input$DefOrUserUpload_W == 'Upload data') {
            shinyalert("", "Please provide the URL pointing to the WEPPCloud watershed file in the Input box", type = "success")
        }
    })
    
    observe({
        if (input$DefOrUserUpload_H == 'Upload data') {
            shinyalert("", "Please provide the URL pointing to the WEPPCloud hillslope file in the Input box", type = "success")
        }
    })
    
    observe({
        if (input$DefOrUserUpload_S == 'Upload data') {
            shinyalert("", "Please provide the URL pointing to the WEPPCloud spaial file in the Input box", type = "success")
        }
    })
    
    
    
    observeEvent(input$Wbutton, {
        updateTabsetPanel(session = session,
                          inputId = "tabs",
                          selected = "Watershed")
    })
    
    
    observeEvent(input$Hbutton, {
        updateTabsetPanel(session = session,
                          inputId = "tabs",
                          selected = "Hillslope")
    })
    
    observeEvent(input$Sbutton, {
        updateTabsetPanel(session = session,
                          inputId = "tabs",
                          selected = "Spatial-Viz")
    })
    
    ######## Server logic for UI generation for hillslope tab ##########
    
    
    #
    ## ----------------------------------Hillslope server logic------------------------------------------##
    output$H_FileInput <- renderUI({
        if (input$DefOrUserUpload_H == 'Upload data') {
            message = 'max. file size is 32MB'
            fileInput(
                "Hill_file",
                label = "Uplaod 'Hillslope' file (*_hill_*.csv)",
                multiple = F,
                placeholder = "No file selected",
                accept = ".csv"
            )%>% 
                helper(icon = "question-circle", colour = "#FF0000",
                       content = "H_upload",
                       type = "markdown", size = "l",
                       buttonLabel = "Okay", easyClose = TRUE, fade = TRUE)
        } else
            if (input$DefOrUserUpload_H == 'Default_Data_Portland' |input$DefOrUserUpload_H == 'Default_Data_Seattle' |
                input$DefOrUserUpload_H == 'Default_Data_LT' | input$DefOrUserUpload_H == 'Default_Data_Palouse') {
            }
    })
    
    Hill_data <- reactive({
        req(input$DefOrUserUpload_H)
        if (input$DefOrUserUpload_H == 'Default_Data_Portland') {
            # file1 <- url("https://wepp1.nkn.uidaho.edu/weppcloud/static/mods/lt/results/lt2020_6_hill_summary.csv")
            file1 <- "data/portland202009_hill_summary_cd.csv"
            #"data/lt2020_6_hill_summary_with_all_scenarios_04_15_2020.csv"
            read.table(file = file1,
                       header = TRUE,
                       sep = ",")%>% dplyr::mutate(Soil_Loss_kg = Soil.Loss..kg.ha. *Hillslope.Area..ha.,
                                                   Sediment_Deposition_kg = Sediment.Deposition..kg.ha. *Hillslope.Area..ha.,
                                                   Sediment_Yield_kg = Sediment.Yield..kg.ha. *Hillslope.Area..ha.,
                                                   Soluble_Reactive_P_kg = Solub..React..P..kg.ha.3. *Hillslope.Area..ha.,
                                                   Particulate_P_kg = Particulate.P..kg.ha.3. *Hillslope.Area..ha.,
                                                   Total_P_kg = Total.P..kg.ha.3. *Hillslope.Area..ha.,
                                                   Particle_Class_1_Fraction_kg = Particle.Class.1.Fraction *Hillslope.Area..ha.,
                                                   Particle_Class_2_Fraction_kg = Particle.Class.2.Fraction *Hillslope.Area..ha.,
                                                   Particle_Class_3_Fraction_kg = Particle.Class.3.Fraction *Hillslope.Area..ha.,
                                                   Particle_Class_4_Fraction_kg = Particle.Class.4.Fraction *Hillslope.Area..ha.,
                                                   Particle_Class_5_Fraction_kg = Particle.Class.5.Fraction *Hillslope.Area..ha.,
                                                   Particle_Fraction_Under_0.016_mm_kg = Particle.Fraction.Under.0.016.mm *Hillslope.Area..ha.,
                                                   Sediment_Yield_of_Particles_Under_0.016_mm_kg = Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha. *Hillslope.Area..ha.
                       )
        } else
            if (input$DefOrUserUpload_H == 'Default_Data_Seattle') {
                # file1 <- url("https://wepp1.nkn.uidaho.edu/weppcloud/static/mods/lt/results/lt2020_6_hill_summary.csv")
                file1 <- "data/seattle202009_hill_summary_cd.csv"
                #"data/lt2020_6_hill_summary_with_all_scenarios_04_15_2020.csv"
                a<-read.table(file = file1,
                              header = TRUE,
                              sep = ",")%>% dplyr::mutate(Soil_Loss_kg = Soil.Loss..kg.ha. *Hillslope.Area..ha.,
                                                          Sediment_Deposition_kg = Sediment.Deposition..kg.ha. *Hillslope.Area..ha.,
                                                          Sediment_Yield_kg = Sediment.Yield..kg.ha. *Hillslope.Area..ha.,
                                                          Soluble_Reactive_P_kg = Solub..React..P..kg.ha.3. *Hillslope.Area..ha.,
                                                          Particulate_P_kg = Particulate.P..kg.ha.3. *Hillslope.Area..ha.,
                                                          Total_P_kg = Total.P..kg.ha.3. *Hillslope.Area..ha.,
                                                          Particle_Class_1_Fraction_kg = Particle.Class.1.Fraction *Hillslope.Area..ha.,
                                                          Particle_Class_2_Fraction_kg = Particle.Class.2.Fraction *Hillslope.Area..ha.,
                                                          Particle_Class_3_Fraction_kg = Particle.Class.3.Fraction *Hillslope.Area..ha.,
                                                          Particle_Class_4_Fraction_kg = Particle.Class.4.Fraction *Hillslope.Area..ha.,
                                                          Particle_Class_5_Fraction_kg = Particle.Class.5.Fraction *Hillslope.Area..ha.,
                                                          Particle_Fraction_Under_0.016_mm_kg = Particle.Fraction.Under.0.016.mm *Hillslope.Area..ha.,
                                                          Sediment_Yield_of_Particles_Under_0.016_mm_kg = Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha. *Hillslope.Area..ha.
                              )
            } else
                if (input$DefOrUserUpload_H == 'Default_Data_LT') {
                    # file1 <- url("https://wepp1.nkn.uidaho.edu/weppcloud/static/mods/lt/results/lt2020_6_hill_summary.csv")
                    file1 <- "data/lt_202010_hill_summary_cd.csv"
                    #"data/lt2020_6_hill_summary_with_all_scenarios_04_15_2020.csv"
                    b<-read.table(file = file1,
                                  header = TRUE,
                                  sep = ",")%>% dplyr::mutate(Soil_Loss_kg = Soil.Loss..kg.ha. *Hillslope.Area..ha.,
                                                              Sediment_Deposition_kg = Sediment.Deposition..kg.ha. *Hillslope.Area..ha.,
                                                              Sediment_Yield_kg = Sediment.Yield..kg.ha. *Hillslope.Area..ha.,
                                                              Soluble_Reactive_P_kg = Solub..React..P..kg.ha.3. *Hillslope.Area..ha.,
                                                              Particulate_P_kg = Particulate.P..kg.ha.3. *Hillslope.Area..ha.,
                                                              Total_P_kg = Total.P..kg.ha.3. *Hillslope.Area..ha.,
                                                              Particle_Class_1_Fraction_kg = Particle.Class.1.Fraction *Hillslope.Area..ha.,
                                                              Particle_Class_2_Fraction_kg = Particle.Class.2.Fraction *Hillslope.Area..ha.,
                                                              Particle_Class_3_Fraction_kg = Particle.Class.3.Fraction *Hillslope.Area..ha.,
                                                              Particle_Class_4_Fraction_kg = Particle.Class.4.Fraction *Hillslope.Area..ha.,
                                                              Particle_Class_5_Fraction_kg = Particle.Class.5.Fraction *Hillslope.Area..ha.,
                                                              Particle_Fraction_Under_0.016_mm_kg = Particle.Fraction.Under.0.016.mm *Hillslope.Area..ha.,
                                                              Sediment_Yield_of_Particles_Under_0.016_mm_kg = Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha. *Hillslope.Area..ha.
                                  )
                }else
                    if (input$DefOrUserUpload_H == 'Default_Data_Palouse') {
                        file1 <- "data/Palouse202103_hill_summary.csv"
                        a<-read.table(file = file1,
                                      header = TRUE,
                                      sep = ",")%>% dplyr::mutate(Soil_Loss_kg = Soil.Loss..kg.ha. *Hillslope.Area..ha.,
                                                                  Sediment_Deposition_kg = Sediment.Deposition..kg.ha. *Hillslope.Area..ha.,
                                                                  Sediment_Yield_kg = Sediment.Yield..kg.ha. *Hillslope.Area..ha.,
                                                                  Soluble_Reactive_P_kg = Solub..React..P..kg.ha.3. *Hillslope.Area..ha.,
                                                                  Particulate_P_kg = Particulate.P..kg.ha.3. *Hillslope.Area..ha.,
                                                                  Total_P_kg = Total.P..kg.ha.3. *Hillslope.Area..ha.,
                                                                  Particle_Class_1_Fraction_kg = Particle.Class.1.Fraction *Hillslope.Area..ha.,
                                                                  Particle_Class_2_Fraction_kg = Particle.Class.2.Fraction *Hillslope.Area..ha.,
                                                                  Particle_Class_3_Fraction_kg = Particle.Class.3.Fraction *Hillslope.Area..ha.,
                                                                  Particle_Class_4_Fraction_kg = Particle.Class.4.Fraction *Hillslope.Area..ha.,
                                                                  Particle_Class_5_Fraction_kg = Particle.Class.5.Fraction *Hillslope.Area..ha.,
                                                                  Particle_Fraction_Under_0.016_mm_kg = Particle.Fraction.Under.0.016.mm *Hillslope.Area..ha.,
                                                                  Sediment_Yield_of_Particles_Under_0.016_mm_kg = Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha. *Hillslope.Area..ha.
                                      )
                    }  else
                    if (input$DefOrUserUpload_H == 'Upload data') {
                        file1 <- input$Hill_file
                        if (is.null(file1)) {
                            return()
                        }
                        validate(
                            need(
                                grepl("hill", input$Hill_file) == TRUE,
                                "Wrong file provided. Hillslope filename should have '_hill_' in filename"
                            )
                        )
                        read.table(
                            file = file1$datapath,
                            header = TRUE,
                            sep = ","
                        )
                        
                    }
        
    })
    
    
    
    output$Hill_var <- renderUI({
        if (input$DefOrUserUpload_H == 'Upload data') {
            req(Hill_data())
            pickerInput(
                "Hill_variable",
                "Select the Water quantity/quality metric of interest",
                # choices = colnames(Hill_data()),
                # selected = colnames(Hill_data()[1]),
                choices =   c( "Runoff (mm)" = "Runoff..mm.",
                               # "Lateral flow (mm)" = "Lateral.Flow..mm.",
                               # "Baseflow (mm)" = "Baseflow..mm.",
                               "Soil loss (kg/ha)" = "Soil.Loss..kg.ha.",
                               "Sediment deposition (kg/ha)" = "Sediment.Deposition..kg.ha.",
                               "Sediment yield (kg/ha)" = "Sediment.Yield..kg.ha.",
                               "Soluble reactive phosphorus (kg/ha)" = "Solub..React..P..kg.ha.3." ,
                               "Particulate phosphorus (kg/ha)" = "Particulate.P..kg.ha.3.",
                               "Total phoshorus (kg/ha)" = "Total.P..kg.ha.3.",
                               "Particle Class 1 Fraction (kg/ha)" = "Particle.Class.1.Fraction",
                               "Particle Class 2 Fraction (kg/ha)" = "Particle.Class.2.Fraction" ,
                               "Particle Class 3 Fraction (kg/ha)" = "Particle.Class.3.Fraction",
                               "Particle Class 4 Fraction (kg/ha)" = "Particle.Class.4.Fraction" ,
                               "Particle Class 5 Fraction (kg/ha)" = "Particle.Class.5.Fraction" ,
                               "Particle Fraction.Under.0.016.mm (kg/ha)" = "Particle_Fraction_Under_0.016_mm",
                               "Sediment yield of particles under 0.016 mm (kg/ha)" = "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha."
                               
                ),
                selected = "Sediment.Yield..kg.ha.",
                multiple = F,
                options = list(`actions-box` = TRUE,
                               `header` = "Select metric ",
                               `windowPadding` = 2,
                               `width` = " css-width ")
            )
        } else
            if (input$DefOrUserUpload_H == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "Hill_variable",
                    label = "Select the Water quantity/quality metric of interest",
                    choices =   c( "Runoff (mm)" = "Runoff..mm.",
                                   # "Lateral flow (mm)" = "Lateral.Flow..mm.",
                                   # "Baseflow (mm)" = "Baseflow..mm.",
                                   "Soil loss (kg)" = "Soil_Loss_kg",
                                   "Sediment deposition (kg)" = "Sediment_Deposition_kg",
                                   "Sediment yield (kg)" = "Sediment_Yield_kg",
                                   "Soluble reactive phosphorus (kg)" = "Soluble_Reactive_P_kg" ,
                                   "Particulate phosphorus (kg)" = "Particulate_P_kg",
                                   "Total phoshorus (kg)" = "Total_P_kg",
                                   "Particle Class 1 Fraction (kg)" = "Particle_Class_1_Fraction_kg",
                                   "Particle Class 2 Fraction (kg)" = "Particle_Class_2_Fraction_kg" ,
                                   "Particle Class 3 Fraction (kg)" = "Particle_Class_3_Fraction_kg",
                                   "Particle Class 4 Fraction (kg)" = "Particle_Class_4_Fraction_kg" ,
                                   "Particle Class 5 Fraction (kg)" = "Particle_Class_5_Fraction_kg" ,
                                   "Particle Fraction.Under.0.016.mm (kg)" = "Particle_Fraction_Under_0.016_mm_kg",
                                   "Sediment yield of particles under 0.016 mm (kg)" = "Sediment_Yield_of_Particles_Under_0.016_mm_kg"
                                   
                    ),
                    selected = "Sediment_Yield_kg",
                    multiple = F,
                    options = list(`actions-box` = TRUE,
                                   `header` = "Select metric ",
                                   `windowPadding` = 1,
                                   `width` = " css-width ")
                )
                
            }else
                if (input$DefOrUserUpload_H == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "Hill_variable",
                        label = "Select the Water quantity/quality metric of interest",
                        choices =   c( "Runoff (mm)" = "Runoff..mm.",
                                       # "Lateral flow (mm)" = "Lateral.Flow..mm.",
                                       # "Baseflow (mm)" = "Baseflow..mm.",
                                       "Soil loss (kg)" = "Soil_Loss_kg",
                                       "Sediment deposition (kg)" = "Sediment_Deposition_kg",
                                       "Sediment yield (kg)" = "Sediment_Yield_kg",
                                       "Soluble reactive phosphorus (kg)" = "Soluble_Reactive_P_kg" ,
                                       "Particulate phosphorus (kg)" = "Particulate_P_kg",
                                       "Total phoshorus (kg)" = "Total_P_kg",
                                       "Particle Class 1 Fraction (kg)" = "Particle_Class_1_Fraction_kg",
                                       "Particle Class 2 Fraction (kg)" = "Particle_Class_2_Fraction_kg" ,
                                       "Particle Class 3 Fraction (kg)" = "Particle_Class_3_Fraction_kg",
                                       "Particle Class 4 Fraction (kg)" = "Particle_Class_4_Fraction_kg" ,
                                       "Particle Class 5 Fraction (kg)" = "Particle_Class_5_Fraction_kg" ,
                                       "Particle Fraction.Under.0.016.mm (kg)" = "Particle_Fraction_Under_0.016_mm_kg",
                                       "Sediment yield of particles under 0.016 mm (kg)" = "Sediment_Yield_of_Particles_Under_0.016_mm_kg"
                                       
                        ),
                        selected = "Sediment_Yield_kg",
                        multiple = F,
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select metric ",
                                       `windowPadding` = 1,
                                       `width` = " css-width ")
                    )
                    
                }else
                    if (input$DefOrUserUpload_H == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "Hill_variable",
                            label = "Select the Water quantity/quality metric of interest",
                            choices =   c( "Runoff (mm)" = "Runoff..mm.",
                                           # "Lateral flow (mm)" = "Lateral.Flow..mm.",
                                           # "Baseflow (mm)" = "Baseflow..mm.",
                                           "Soil loss (kg)" = "Soil_Loss_kg",
                                           "Sediment deposition (kg)" = "Sediment_Deposition_kg",
                                           "Sediment yield (kg)" = "Sediment_Yield_kg",
                                           "Soluble reactive phosphorus (kg)" = "Soluble_Reactive_P_kg" ,
                                           "Particulate phosphorus (kg)" = "Particulate_P_kg",
                                           "Total phoshorus (kg)" = "Total_P_kg",
                                           "Particle Class 1 Fraction (kg)" = "Particle_Class_1_Fraction_kg",
                                           "Particle Class 2 Fraction (kg)" = "Particle_Class_2_Fraction_kg" ,
                                           "Particle Class 3 Fraction (kg)" = "Particle_Class_3_Fraction_kg",
                                           "Particle Class 4 Fraction (kg)" = "Particle_Class_4_Fraction_kg" ,
                                           "Particle Class 5 Fraction (kg)" = "Particle_Class_5_Fraction_kg" ,
                                           "Particle Fraction.Under.0.016.mm (kg)" = "Particle_Fraction_Under_0.016_mm_kg",
                                           "Sediment yield of particles under 0.016 mm (kg)" = "Sediment_Yield_of_Particles_Under_0.016_mm_kg"
                                           
                            ),
                            selected = "Sediment_Yield_kg",
                            multiple = F,
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select metric ",
                                           `windowPadding` = 1,
                                           `width` = " css-width "),
                            choicesOpt = list(
                                content = stringr::str_trunc(c("Runoff (mm)",
                                                               # "Lateral flow (mm)",
                                                               # "Baseflow (mm)",
                                                               "Soil loss (kg)",
                                                               "Sediment deposition (kg)",
                                                               "Sediment yield (kg)",
                                                               "Soluble reactive phosphorus (kg)",
                                                               "Particulate phosphorus (kg)",
                                                               "Total phoshorus (kg)",
                                                               "Particle Class 1 Fraction (kg)",
                                                               "Particle Class 2 Fraction (kg)"  ,
                                                               "Particle Class 3 Fraction (kg)" ,
                                                               "Particle Class 4 Fraction (kg)" ,
                                                               "Particle Class 5 Fraction (kg)" ,
                                                               "Particle Fraction.Under.0.016.mm (kg)"  ,
                                                               "Sediment yield of particles under 0.016 mm (kg)" 
                                ),
                                width = 35))
                        )
                        
                    }else
                        if (input$DefOrUserUpload_H == 'Default_Data_Palouse') {
                            pickerInput(
                                inputId = "Hill_variable",
                                label = "Select the Water quantity/quality metric of interest",
                                choices =   c( "Runoff (mm)" = "Runoff..mm.",
                                               "Lateral flow (mm)" = "Lateral.Flow..mm.",
                                               "Baseflow (mm)" = "Baseflow..mm.",
                                               "Soil loss (kg)" = "Soil_Loss_kg",
                                               "Sediment deposition (kg)" = "Sediment_Deposition_kg",
                                               "Sediment yield (kg)" = "Sediment_Yield_kg",
                                               "Soluble reactive phosphorus (kg)" = "Soluble_Reactive_P_kg" ,
                                               "Particulate phosphorus (kg)" = "Particulate_P_kg",
                                               "Total phoshorus (kg)" = "Total_P_kg",
                                               "Particle Class 1 Fraction (kg)" = "Particle_Class_1_Fraction_kg",
                                               "Particle Class 2 Fraction (kg)" = "Particle_Class_2_Fraction_kg" ,
                                               "Particle Class 3 Fraction (kg)" = "Particle_Class_3_Fraction_kg",
                                               "Particle Class 4 Fraction (kg)" = "Particle_Class_4_Fraction_kg" ,
                                               "Particle Class 5 Fraction (kg)" = "Particle_Class_5_Fraction_kg" ,
                                               "Particle Fraction.Under.0.016.mm (kg)" = "Particle_Fraction_Under_0.016_mm_kg",
                                               "Sediment yield of particles under 0.016 mm (kg)" = "Sediment_Yield_of_Particles_Under_0.016_mm_kg"
                                               
                                ),
                                selected = "Sediment_Yield_kg",
                                multiple = F,
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select metric ",
                                               `windowPadding` = 1,
                                               `width` = " css-width ")
                            )
                            
                        }
        
    })
    
    
    output$Hill_wshed <- renderUI({
        if (input$DefOrUserUpload_H == 'Upload data') {
            req(Hill_data())
            pickerInput(
                inputId = "Hill_wshed",
                label = "Select the watershed of interest",
                choices =   unique(Hill_data()$Watershed),
                selected =   unique(Hill_data()$Watershed)[1],
                options = list(`actions-box` = TRUE,
                               `header` = "Select watershed ",
                               `windowPadding` = 1,
                               `width` = " css-width ")) 
            
        } else
            if (input$DefOrUserUpload_H == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "Hill_wshed",
                    label = "Select the watershed of interest",
                    choices =   unique(Hill_data()$Watershed),
                    selected =   unique(Hill_data()$Watershed)[1],
                    options = list(`actions-box` = TRUE,
                                   `header` = "Select watershed ",
                                   `windowPadding` = 1,
                                   `width` = " css-width ")
                ) 
                
            }else
                if (input$DefOrUserUpload_H == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "Hill_wshed",
                        label = "Select the watershed of interest",
                        choices =   unique(Hill_data()$Watershed),
                        selected =   unique(Hill_data()$Watershed)[1],
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select watershed ",
                                       `windowPadding` = 1,
                                       `width` = " css-width ")
                    ) 
                    
                }else
                    if (input$DefOrUserUpload_H == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "Hill_wshed",
                            label = "Select the watershed of interest",
                            choices =   unique(Hill_data()$Watershed),
                            selected =   unique(Hill_data()$Watershed)[1],
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select watershed ",
                                           `windowPadding` = 1,
                                           `width` = " css-width "),
                            choicesOpt = list(
                                content = stringr::str_trunc(unique(Hill_data()$Watershed),
                                                             width = 35)
                            ) 
                        )   
                    }else
                        if (input$DefOrUserUpload_H == 'Default_Data_Palouse') {
                            pickerInput(
                                inputId = "Hill_wshed",
                                label = "Select the watershed of interest",
                                choices =   unique(Hill_data()$Watershed),
                                selected =   unique(Hill_data()$Watershed)[1],
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select watershed ",
                                               `windowPadding` = 1,
                                               `width` = " css-width ")
                            ) 
                            
                        }
        
    })
    
    output$Hill_scen_base <- renderUI({
        if (input$DefOrUserUpload_H == 'Upload data') {
            req(Spatial_data())
            pickerInput(
                "Hill_scen_base",
                "Select the baseline management scenario",
                unique(Hill_data()$Scenario),
                unique(Hill_data()$Scenario)[1],
                multiple = F,
                options = list(`actions-box` = TRUE,
                               `header` = "Select baseline scenario ",
                               `windowPadding` = 1,
                               `width` = " css-width ")
            )
        } else
            if (input$DefOrUserUpload_H == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "Hill_scen_base",
                    label = "Select the baseline management scenario",
                    choices = unique(Hill_data()$Scenario),
                    # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                    #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                    #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                    #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                    #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                    #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                    #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                    #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                    #              "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                    #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                    #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                    #              
                    # ),
                    unique(Hill_data()$Scenario)[1],
                    multiple = F,
                    options = list(`actions-box` = TRUE,
                                   `header` = "Select baseline scenario ",
                                   `windowPadding` = 1,
                                   `width` = " css-width ")
                )
                
            }else
                if (input$DefOrUserUpload_H == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "Hill_scen_base",
                        label = "Select the baseline management scenario",
                        choices = unique(Hill_data()$Scenario),
                        # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                        #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                        #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                        #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                        #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                        #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                        #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                        #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                        #              "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                        #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                        #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                        #              
                        # ),
                        unique(Hill_data()$Scenario)[1],
                        multiple = F,
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select baseline scenario ",
                                       `windowPadding` = 1,
                                       `width` = " css-width ")
                    )
                    
                }else
                    if (input$DefOrUserUpload_H == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "Hill_scen_base",
                            label = "Select the baseline management scenario",
                            choices = unique(Hill_data()$Scenario),
                            # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                            #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                            #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                            #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                            #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                            #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                            #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                            #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                            #              "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                            #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                            #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                            #              
                            # ),
                            unique(Hill_data()$Scenario)[1],
                            multiple = F,
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select baseline scenario ",
                                           `windowPadding` = 1,
                                           `width` = " css-width ")
                        )
                        
                    }else
                        if (input$DefOrUserUpload_H == 'Default_Data_Palouse') {
                            pickerInput(
                                inputId = "Hill_scen_base",
                                label = "Select the baseline management scenario",
                                choices = unique(Hill_data()$Scenario),
                                # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                                #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                                #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                                #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                                #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                                #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                                #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                                #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                                #              "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                                #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                                #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                                #              
                                # ),
                                unique(Hill_data()$Scenario)[1],
                                multiple = F,
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select baseline scenario ",
                                               `windowPadding` = 1,
                                               `width` = " css-width ")
                            )
                            
                        }
        
    })
    
    
    output$Hill_scen_comp <- renderUI({
        if (input$DefOrUserUpload_H == 'Upload data') {
            req(Hill_data())
            pickerInput(
                inputId = "Hill_scen_comp",
                label = "Select the management scenario to compare",
                choices = unique(Hill_data()$Scenario),
                selected = unique(Hill_data()$Scenario)[2],
                multiple = T,
                options = list(`actions-box` = TRUE,
                               `header` = "Select comparison scenario ",
                               `windowPadding` = 1,
                               `width` = " css-width ")
            )
        } else
            if (input$DefOrUserUpload_H == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "Hill_scen_comp",
                    label = "Select the management scenario to compare",
                    choices = unique(Hill_data()$Scenario),
                    # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                    #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                    #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                    #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                    #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                    #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                    #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                    #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                    #              "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                    #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                    #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                    #              
                    # ),
                    selected = unique(Hill_data()$Scenario)[7],
                    multiple = T,
                    options = list(`actions-box` = TRUE,
                                   `header` = "Select comparison scenario ",
                                   `windowPadding` = 1,
                                   `width` = " css-width ")
                    
                )
                
            }else
                if (input$DefOrUserUpload_H == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "Hill_scen_comp",
                        label = "Select the management scenario to compare",
                        choices = unique(Hill_data()$Scenario),
                        # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                        #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                        #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                        #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                        #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                        #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                        #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                        #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                        #              "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                        #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                        #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                        #              
                        # ),
                        selected = unique(Hill_data()$Scenario)[7],
                        multiple = T,
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select comparison scenario ",
                                       `windowPadding` = 1,
                                       `width` = " css-width ")
                        
                    )
                    
                }else
                    if (input$DefOrUserUpload_H == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "Hill_scen_comp",
                            label = "Select the management scenario to compare",
                            choices = unique(Hill_data()$Scenario),
                            # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                            #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                            #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                            #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                            #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                            #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                            #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                            #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                            #              "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                            #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                            #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                            #              
                            # ),
                            selected = unique(Hill_data()$Scenario)[7],
                            multiple = T,
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select comparison scenario ",
                                           `windowPadding` = 1,
                                           `width` = " css-width ")
                            
                        )
                        
                    }else
                        if (input$DefOrUserUpload_H == 'Default_Data_Palouse') {
                            pickerInput(
                                inputId = "Hill_scen_comp",
                                label = "Select the management scenario to compare",
                                choices = unique(Hill_data()$Scenario),
                                # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                                #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                                #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                                #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                                #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                                #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                                #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                                #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                                #              "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                                #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                                #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                                #              
                                # ),
                                selected = unique(Hill_data()$Scenario)[3],
                                multiple = T,
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select comparison scenario ",
                                               `windowPadding` = 1,
                                               `width` = " css-width ")
                                
                            )
                            
                        }
        
    })
    
    
    
    output$Hill_scen <- renderUI({
        if (input$DefOrUserUpload_H == 'Upload data') {
            req(Hill_data())
            pickerInput(
                "Hill_scen",
                "Select Scenario do display data summary",
                unique(Hill_data()$Scenario),
                unique(Hill_data()$Scenario)[1],
                multiple = F,
                options = list(`actions-box` = TRUE,
                               `header` = "Select scenario ",
                               `windowPadding` = 1,
                               `width` = " css-width ")
            )
        } else
            if (input$DefOrUserUpload_H == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "Hill_scen",
                    label = "Select Scenario do display data summary",
                    unique(Hill_data()$Scenario),
                    # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                    #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                    #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                    #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                    #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                    #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                    #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                    #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                    #              "Simulated fire-fccs fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                    #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                    #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                    # 
                    # ),
                    unique(Hill_data()$Scenario)[5],
                    multiple = F,
                    options = list(`actions-box` = TRUE,
                                   `header` = "Select scenario ",
                                   `windowPadding` = 1,
                                   `width` = " css-width ")
                )
                
            }else
                if (input$DefOrUserUpload_H == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "Hill_scen",
                        label = "Select Scenario do display data summary",
                        unique(Hill_data()$Scenario),
                        # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                        #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                        #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                        #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                        #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                        #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                        #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                        #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                        #              "Simulated fire-fccs fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                        #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                        #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                        # 
                        # ),
                        unique(Hill_data()$Scenario)[5],
                        multiple = F,
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select scenario ",
                                       `windowPadding` = 1,
                                       `width` = " css-width ")
                    )
                    
                }else
                    if (input$DefOrUserUpload_H == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "Hill_scen",
                            label = "Select Scenario do display data summary",
                            unique(Hill_data()$Scenario),
                            # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                            #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                            #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                            #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                            #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                            #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                            #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                            #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                            #              "Simulated fire-fccs fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                            #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                            #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                            # 
                            # ),
                            unique(Hill_data()$Scenario)[5],
                            multiple = F,
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select scenario ",
                                           `windowPadding` = 1,
                                           `width` = " css-width ")
                        )
                        
                    }else
                        if (input$DefOrUserUpload_H == 'Default_Data_Palouse') {
                            pickerInput(
                                inputId = "Hill_scen",
                                label = "Select Scenario do display data summary",
                                unique(Hill_data()$Scenario),
                                # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                                #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                                #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                                #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                                #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                                #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                                #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                                #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                                #              "Simulated fire-fccs fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                                #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                                #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                                # 
                                # ),
                                unique(Hill_data()$Scenario)[1],
                                multiple = F,
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select scenario ",
                                               `windowPadding` = 1,
                                               `width` = " css-width ")
                            )
                            
                        }
        
    })
    
    
    output$tab_H <- renderUI({
        req(sed_stats_df())
        
        fluidRow(
            column(
                12,
                align = "center",
                offset = 0,
                style = "background-color:#ECF0F1;",
                DT::dataTableOutput("Sed_stats_by_category") %>%
                    withSpinner(type = 6 ,color = "#ffffff")
            )
        )
        
    })
    
    ## ----------------------------------Watershed Server logic------------------------------------------##
    ######## Server logic for UI generation for  Watersheds tab ##########
    
    output$W_FileInput <- renderUI({
        if (input$DefOrUserUpload_W == 'Upload data') {
            message = 'max. file size is 32MB'
            fileInput(
                "Wshed_file",
                label = "Uplaod 'Watershed' file (*_out_*.csv)",
                multiple = F,
                placeholder = "No file selected",
                accept = ".csv"
            ) 
            # %>% 
            #     helper(icon = "question-circle", colour = "#FF0000",
            #            content = "W_upload",
            #            type = "markdown", size = "l",
            #            buttonLabel = "Okay", easyClose = TRUE, fade = TRUE)
        } else
            if (input$DefOrUserUpload_W == 'Default_Data_Portland' | input$DefOrUserUpload_W == 'Default_Data_Seattle' | input$DefOrUserUpload_W == 'Default_Data_LT' | input$DefOrUserUpload_W == 'Default_Data_Palouse') {
            }
    })
    
    
    Wshed_data <- reactive({
        req(input$DefOrUserUpload_W)
        if (input$DefOrUserUpload_W == 'Default_Data_Portland') {
            # file3 <- url("https://wepp1.nkn.uidaho.edu/weppcloud/static/mods/lt/results/lt2020_6_out_summary.csv")
            file3 <-"data/portland202009_out_summary_cd.csv"
            # "data/lt2020_6_out_summary_with_all_scenarios_04_15_2020.csv"
            read.table(file = file3,
                       header = TRUE,
                       sep = ",")
        } else
            if (input$DefOrUserUpload_W == 'Default_Data_Seattle') {
                # file3 <- url("https://wepp1.nkn.uidaho.edu/weppcloud/static/mods/lt/results/lt2020_6_out_summary.csv")
                file3 <-"data/seattle202009_out_summary_cd.csv"
                # "data/lt2020_6_out_summary_with_all_scenarios_04_15_2020.csv"
                read.table(file = file3,
                           header = TRUE,
                           sep = ",")
            }else
                if (input$DefOrUserUpload_W == 'Default_Data_LT') {
                    # file3 <- url("https://wepp1.nkn.uidaho.edu/weppcloud/static/mods/lt/results/lt2020_6_out_summary.csv")
                    file3 <-"data/lt_202010_out_summary_cd.csv"
                    read.table(file = file3,
                               header = TRUE,
                               sep = ",")
                } else
                    if (input$DefOrUserUpload_W == 'Default_Data_Palouse') {
                        # file3 <- url("https://wepp1.nkn.uidaho.edu/weppcloud/static/mods/lt/results/lt2020_6_out_summary.csv")
                        file3 <-"data/Palouse202103_out_summary.csv"
                        # "data/lt2020_6_out_summary_with_all_scenarios_04_15_2020.csv"
                        read.table(file = file3,
                                   header = TRUE,
                                   sep = ",")
                    } else
                        if (input$DefOrUserUpload_W == 'Upload data') {
                            file3 <- input$Wshed_file
                            if (is.null(file3)) {
                                return()
                                }
                            validate(
                            need(
                                grepl("out", input$Wshed_file) == TRUE,
                                "Wrong file provided. Watershed filename should have '_out_' in filename"
                            )
                            )
                        read.table(
                            file = file3$datapath,
                            header = TRUE,
                            sep = ","
                        )
                        
                    }
        
    })
    
    output$Wshed_wshed <- renderUI({
        if (input$DefOrUserUpload_W == 'Upload data') {
            req(Wshed_data())
            if (input$AreaVsScen == 'allscen') {
                pickerInput(
                    "Wshed_wshed",
                    "Select the watershed of interest",
                    unique(Wshed_data()$Watershed),
                    options = list(`actions-box` = TRUE,
                                   `header` = "Select watershed ",
                                   `windowPadding` = 1,
                                   `width` = " css-width "),
                    choicesOpt = list(
                        content = stringr::str_trunc(unique(Wshed_data()$Watershed),
                                                     width = 35))
                )
            } else
                if (input$AreaVsScen == 'allwat') {
                    pickerInput(
                        "Wshed_wshed",
                        "Select the scenario of interest",
                        unique(Wshed_data()$Scenario),
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select scenario ",
                                       `windowPadding` = 1,
                                       `width` = " css-width ")
                    )
                }
        } else
            if (input$DefOrUserUpload_W == 'Default_Data_Portland') {
                if (input$AreaVsScen == 'allscen') {
                    pickerInput(
                        inputId = "Wshed_wshed",
                        label = "Select the watershed of interest",
                        choices =   unique(Wshed_data()$Watershed),
                        selected =   unique(Wshed_data()$Watershed)[11],
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select watershed ",
                                       `windowPadding` = 1,
                                       `width` = " css-width ")
                    )
                } else
                    if (input$AreaVsScen == 'allwat') {
                        pickerInput(
                            "Wshed_wshed",
                            "Select the scenario of interest",
                            unique(Wshed_data()$Scenario),
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select scenario ",
                                           `windowPadding` = 1,
                                           `width` = " css-width ")
                        )
                    }
                
            }else
                if (input$DefOrUserUpload_W == 'Default_Data_Seattle') {
                    if (input$AreaVsScen == 'allscen') {
                        pickerInput(
                            inputId = "Wshed_wshed",
                            label = "Select the watershed of interest",
                            choices =   unique(Wshed_data()$Watershed),
                            selected =   unique(Wshed_data()$Watershed)[11],
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select watershed ",
                                           `windowPadding` = 1,
                                           `width` = " css-width ")
                        )
                    } else
                        if (input$AreaVsScen == 'allwat') {
                            pickerInput(
                                "Wshed_wshed",
                                "Select the scenario of interest",
                                unique(Wshed_data()$Scenario),
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select scenario ",
                                               `windowPadding` = 1,
                                               `width` = " css-width ")
                            )
                        }
                    
                }else
                    if (input$DefOrUserUpload_W == 'Default_Data_LT') {
                        if (input$AreaVsScen == 'allscen') {
                            pickerInput(
                                inputId = "Wshed_wshed",
                                label = "Select the watershed of interest",
                                choices =   unique(Wshed_data()$Watershed),
                                selected =   unique(Wshed_data()$Watershed)[11],
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select watershed ",
                                               `windowPadding` = 1,
                                               `width` = " css-width "),
                                choicesOpt = list(
                                    content = stringr::str_trunc(unique(Wshed_data()$Watershed),
                                                                 width = 35))
                            )
                        } else
                            if (input$AreaVsScen == 'allwat') {
                                pickerInput(
                                    "Wshed_wshed",
                                    "Select the scenario of interest",
                                    unique(Wshed_data()$Scenario),
                                    options = list(`actions-box` = TRUE,
                                                   `header` = "Select scenario ",
                                                   `windowPadding` = 1,
                                                   `width` = " css-width ")
                                )
                            }
                        
                    }else
                        if (input$DefOrUserUpload_W == 'Default_Data_Palouse') {
                            if (input$AreaVsScen == 'allscen') {
                                pickerInput(
                                    inputId = "Wshed_wshed",
                                    label = "Select the watershed of interest",
                                    choices =   unique(Wshed_data()$Watershed),
                                    selected =   unique(Wshed_data()$Watershed)[1],
                                    options = list(`actions-box` = TRUE,
                                                   `header` = "Select watershed ",
                                                   `windowPadding` = 1,
                                                   `width` = " css-width ")
                                )
                            } else
                                if (input$AreaVsScen == 'allwat') {
                                    pickerInput(
                                        "Wshed_wshed",
                                        "Select the scenario of interest",
                                        unique(Wshed_data()$Scenario),
                                        options = list(`actions-box` = TRUE,
                                                       `header` = "Select scenario ",
                                                       `windowPadding` = 1,
                                                       `width` = " css-width ")
                                    )
                                }
                            
                        }
        
    })
    
    output$Wshed_wshed_S <- renderUI({
        if (input$DefOrUserUpload_W == 'Upload data') {
            req(Wshed_data())
            if (input$AreaVsScen == 'allscen') {
                
            } else
                if (input$AreaVsScen == 'allwat') {
                    pickerInput(
                        "Wshed_wshed_S",
                        "Select the watersheds of interest",
                        unique(Wshed_data()$Watershed),
                        selected =   unique(Wshed_data()$Watershed)[1:2],
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select watersheds ",
                                       `windowPadding` = 1,
                                       `width` = " css-width ")
                    )
                }
        }else
            if (input$DefOrUserUpload_W == 'Default_Data_Portland') {
                if (input$AreaVsScen == 'allscen') {
                    
                } else
                    if (input$AreaVsScen == 'allwat') {
                        pickerInput(
                            "Wshed_wshed_S",
                            "Select the watersheds of interest",
                            unique(Wshed_data()$Watershed),
                            selected =   unique(Wshed_data()$Watershed)[1:10],
                            multiple = TRUE,
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select watersheds ",
                                           `windowPadding` = 1,
                                           `width` = " css-width ")
                        )
                    }
                
            }else
                if (input$DefOrUserUpload_W == 'Default_Data_Seattle') {
                    if (input$AreaVsScen == 'allscen') {
                        
                    } else
                        if (input$AreaVsScen == 'allwat') {
                            pickerInput(
                                "Wshed_wshed_S",
                                "Select the watersheds of interest",
                                unique(Wshed_data()$Watershed),
                                selected =   unique(Wshed_data()$Watershed)[1:10],
                                multiple = TRUE,
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select watersheds ",
                                               `windowPadding` = 1,
                                               `width` = " css-width ")
                            )
                        }
                    
                }else
                    if (input$DefOrUserUpload_W == 'Default_Data_LT') {
                        if (input$AreaVsScen == 'allscen') {
                            
                        } else
                            if (input$AreaVsScen == 'allwat') {
                                pickerInput(
                                    "Wshed_wshed_S",
                                    "Select the watersheds of interest",
                                    unique(Wshed_data()$Watershed),
                                    selected =   unique(Wshed_data()$Watershed)[1:10],
                                    multiple = TRUE,
                                    options = list(`actions-box` = TRUE,
                                                   `header` = "Select watersheds ",
                                                   `windowPadding` = 1,
                                                   `width` = "css-width"),
                                    choicesOpt = list(
                                        content = stringr::str_trunc(unique(Wshed_data()$Watershed),
                                                                     width = 35)
                                    ))
                            }
                        
                    }else
                        if (input$DefOrUserUpload_W == 'Default_Data_Palouse') {
                            if (input$AreaVsScen == 'allscen') {
                                
                            } else
                                if (input$AreaVsScen == 'allwat') {
                                    pickerInput(
                                        "Wshed_wshed_S",
                                        "Select the watersheds of interest",
                                        unique(Wshed_data()$Watershed),
                                        selected =   unique(Wshed_data()$Watershed)[1:2],
                                        multiple = TRUE,
                                        options = list(`actions-box` = TRUE,
                                                       `header` = "Select watersheds ",
                                                       `windowPadding` = 1,
                                                       `width` = " css-width ")
                                    )
                                }
                            
                        }
        
        
    })
    
    
    output$wshed_var <- renderUI({
        if (input$DefOrUserUpload_W == 'Upload data') {
            req(Wshed_data())
            pickerInput(
                inputId = "wshed_var",
                label = "Select the Water quantity/quality metric of interest",
                choices = colnames(Wshed_data())[!(colnames(Wshed_data()) %in% c("ProjectName",
                                                                                 "Watershed",
                                                                                 "Scenario"))],
                selected = colnames(Wshed_data()[1:7]),
                multiple = T,
                options = list(`actions-box` = TRUE,
                               `header` = "Select metric ",
                               `windowPadding` = 1,
                               `width` = " css-width "),
                choicesOpt = list(
                    content = stringr::str_trunc(colnames(Wshed_data())[!(colnames(Wshed_data()) %in% c("ProjectName",
                                                                                                        "Watershed",
                                                                                                        "Scenario"))], width = 35)
                )
            )
        } else
            if (input$DefOrUserUpload_W == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "wshed_var",
                    label = "Select the Water quantity/quality metric of interest",
                    options = list(`actions-box` = TRUE,
                                   `header` = "Select metric ",
                                   `windowPadding` = 1,
                                   `width` = " css-width "),
                    choices =   colnames(Wshed_data())[7:15],
                    selected = colnames(Wshed_data()[c(8,10,12,15)]),
                    # choices =   colnames(Wshed_data())[c(8:10,12,14,15,17,20)],
                    # selected = colnames(Wshed_data()[c(10,12,17,20)]),
                    multiple = T,
                    # choicesOpt = list(
                    #     content = stringr::str_trunc(colnames(Wshed_data())[7:20], width = 60)
                    # )
                )
                
            }else
                if (input$DefOrUserUpload_W == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "wshed_var",
                        label = "Select the Water quantity/quality metric of interest",
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select metric ",
                                       `windowPadding` = 1,
                                       `width` = " css-width "),
                        choices =   colnames(Wshed_data())[7:15],
                        selected = colnames(Wshed_data()[c(8,10,12,15)]),
                        # choices =   colnames(Wshed_data())[c(8:10,12,14,15,17,20)],
                        # selected = colnames(Wshed_data()[c(10,12,17,20)]),
                        multiple = T,
                        # choicesOpt = list(
                        #     content = stringr::str_trunc(colnames(Wshed_data())[7:20], width = 60)
                        # )
                    )
                    
                }else
                    if (input$DefOrUserUpload_W == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "wshed_var",
                            label = "Select the Water quantity/quality metric of interest",
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select metric ",
                                           `windowPadding` = 1,
                                           `width` = " css-width "),
                            choices =   colnames(Wshed_data())[7:15],
                            selected = colnames(Wshed_data()[c(8,10,12,15)]),
                            # choices =   colnames(Wshed_data())[c(8:10,12,14,15,17,20)],
                            # selected = colnames(Wshed_data()[c(10,12,17,20)]),
                            multiple = T,
                            # choicesOpt = list(
                            #     content = stringr::str_trunc(colnames(Wshed_data())[7:20], width = 60)
                            # )
                        )
                        
                    }else
                        if (input$DefOrUserUpload_W == 'Default_Data_Palouse') {
                            pickerInput(
                                inputId = "wshed_var",
                                label = "Select the Water quantity/quality metric of interest",
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select metric ",
                                               `windowPadding` = 1,
                                               `width` = " css-width "),
                                choices =   colnames(Wshed_data())[7:15],
                                selected = colnames(Wshed_data()[c(8,10,12,15)]),
                                multiple = T,
                               
                            )
                            
                        }
        
    })
    
    
    
    ## ----------------------------------Spatial-Viz tab server logic------------------------------------------##
    ######## Server logic for UI generation for spatial-Viz tab ##########
    
    output$S_FileInput <- renderUI({
        if (input$DefOrUserUpload_S == 'Upload data') {
            message = 'max. file size is 32MB'
            fileInput(
                "Spatial_file",
                label = "Uplaod subcatchements RDS file",
                multiple = F,
                placeholder = "No file selected",
                accept = c(".RDS", ".qs")
            )%>% 
                helper(icon = "question-circle", colour = "#FF0000",
                       content = "S_upload",
                       type = "markdown", size = "l",
                       buttonLabel = "Okay", easyClose = TRUE, fade = TRUE)
        } else
            if (input$DefOrUserUpload_S == 'Default_Data_Portland' | input$DefOrUserUpload_S == 'Default_Data_Seattle' | input$DefOrUserUpload_S == 'Default_Data_LT' ) {
            }
    })
    
    Spatial_data1 <- reactive({
        req(input$DefOrUserUpload_S)
        if (input$DefOrUserUpload_S == 'Default_Data_Portland') {
            # sf::st_read("data/lt_allcond_subcatchments_wgs84_split_wshed_and_scen.geojson")
            # readRDS("data/lt2020_6_subcatchments_wgs84_split_wshed_and_scen.RDS")
            readRDS("data/portland202009_shps_subcatchments_wgs84_split_wshed_and_scen.RDS")
        } else
            if (input$DefOrUserUpload_S == 'Default_Data_Seattle') {
                # sf::st_read("data/lt_allcond_subcatchments_wgs84_split_wshed_and_scen.geojson")
                # readRDS("data/lt2020_6_subcatchments_wgs84_split_wshed_and_scen.RDS")
                readRDS("data/seattle202009_shps_subcatchments_wgs84_split_wshed_and_scen.RDS")
            }else
                if (input$DefOrUserUpload_S == 'Default_Data_LT') {
                    # sf::st_read("data/lt_allcond_subcatchments_wgs84_split_wshed_and_scen.geojson")
                    qs::qread("data/lt_202010_shps_subcatchments_wgs84_split_wshed_and_scen.qs")
                    # readRDS("data/lt2020_6_subcatchments_wgs84_split_wshed_and_scen.RDS")
                } else
                    if (input$DefOrUserUpload_S == 'Upload data') {
                        file4 <- input$Spatial_file
                        if (is.null(file4)) {
                            return()
                        }
                        if (tools::file_ext(file4$datapath) == "RDS") {
                            readRDS(file4$datapath)
                        }else
                            if (tools::file_ext(file4$datapath) == "qs") {
                                qs::qread(file4$datapath)
                            }
                        
                    }
        
    })
    
    Spatial_data <- reactive({
        req(Spatial_data1())
        Spatial_data1() %>%sf::st_set_crs(value = 4326)
    })
    
    output$Spatial_wshed <- renderUI({
        if (input$DefOrUserUpload_S == 'Upload data') {
            req(Spatial_data())
            pickerInput(
                "S_wshed",
                "Select the watershed of interest",
                choices = unique(as.character(Spatial_data()$Watershed)),
                options = list(`actions-box` = TRUE,
                               `header` = "Select Watershed",
                               `windowPadding` = 1,
                               `width` = " css-width "),
                selected = unique(Spatial_data()$Watershed)[1],
                multiple = T
            )
        } else
            if (input$DefOrUserUpload_S == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "S_wshed",
                    label = "Select the watershed of interest",
                    choices =  unique(as.character(Spatial_data()$Watershed)),
                    options = list(`actions-box` = TRUE,
                                   `header` = "Select Watershed",
                                   `windowPadding` = 1,
                                   `width` = " css-width "),
                    selected = unique(Spatial_data()$Watershed)[1],
                    multiple = T
                )
                
            }else
                if (input$DefOrUserUpload_S == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "S_wshed",
                        label = "Select the watershed of interest",
                        choices =  unique(as.character(Spatial_data()$Watershed)),
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select Watershed",
                                       `windowPadding` = 1,
                                       `width` = " css-width "),
                        selected = unique(Spatial_data()$Watershed)[1],
                        multiple = T
                    )
                    
                }else
                    if (input$DefOrUserUpload_S == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "S_wshed",
                            label = "Select the watershed of interest",
                            choices =  unique(as.character(Spatial_data()$Watershed)),
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select Watershed",
                                           `windowPadding` = 1,
                                           `width` = " css-width "),
                            choicesOpt = list(
                                content = stringr::str_trunc(unique(Spatial_data()$Watershed),
                                                             width = 25)),
                            selected = unique(Spatial_data()$Watershed)[60],
                            multiple = T
                        )
                        
                    }
        
    })
    
    
    output$Spatial_scen <- renderUI({
        if (input$DefOrUserUpload_S == 'Upload data') {
            req(Spatial_data())
            pickerInput(
                "S_scen",
                "Select the scenario of interest",
                unique(Spatial_data()$Scenario),
                unique(Spatial_data()$Scenario)[1],
                multiple = F,
                options = list(`actions-box` = TRUE,
                               `header` = " ",
                               `windowPadding` = 1,
                               `width` = " css-width ")
            )
        } else
            if (input$DefOrUserUpload_S == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "S_scen",
                    label = "Select the scenario of interest",
                    choices = unique(Spatial_data()$Scenario),
                    # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                    #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                    #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                    #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                    #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                    #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                    #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                    #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                    #              "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                    #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                    #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                    #     
                    # ),
                    unique(Spatial_data()$Scenario)[1],
                    multiple = F,
                    
                )
                
            }else
                if (input$DefOrUserUpload_S == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "S_scen",
                        label = "Select the scenario of interest",
                        choices = unique(Spatial_data()$Scenario),
                        # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                        #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                        #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                        #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                        #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                        #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                        #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                        #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                        #              "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                        #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                        #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                        #     
                        # ),
                        unique(Spatial_data()$Scenario)[1],
                        multiple = F,
                        
                    )
                }else
                    if (input$DefOrUserUpload_S == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "S_scen",
                            label = "Select the scenario of interest",
                            choices = unique(Spatial_data()$Scenario),
                            # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                            #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                            #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                            #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                            #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                            #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                            #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                            #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                            #              "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                            #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                            #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                            # 
                            # ),
                            # "PrescFireS.2020.ki5krcs.chn_12",
                            unique(Spatial_data()$Scenario)[1],
                            multiple = F,
                            
                        )
                        
                    }
        
    })
    
    
    output$Spatial_scen_base <- renderUI({
        if (input$DefOrUserUpload_S == 'Upload data') {
            req(Spatial_data())
            pickerInput(
                "S_scen_base",
                "Select the baseline management scenario",
                unique(Spatial_data()$Scenario),
                unique(Spatial_data()$Scenario)[1],
                multiple = F,
                options = list(`actions-box` = TRUE,
                               `header` = "Select baseline scenario ",
                               `windowPadding` = 1,
                               `width` = " css-width ")
            )
        } else
            if (input$DefOrUserUpload_S == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "S_scen_base",
                    label = "Select the baseline management scenario",
                    choices = unique(Spatial_data()$Scenario),
                    # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                    #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                    #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                    #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                    #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                    #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                    #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                    #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                    #              "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                    #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                    #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                    #              
                    # ),
                    unique(Spatial_data()$Scenario)[1],
                    multiple = F,
                    options = list(`actions-box` = TRUE,
                                   `header` = "Select baseline scenario ",
                                   `windowPadding` = 1,
                                   `width` = " css-width ")
                )
                
            }else
                if (input$DefOrUserUpload_S == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "S_scen_base",
                        label = "Select the baseline management scenario",
                        choices = unique(Spatial_data()$Scenario),
                        # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                        #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                        #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                        #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                        #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                        #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                        #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                        #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                        #              "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                        #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                        #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                        #              
                        # ),
                        unique(Spatial_data()$Scenario)[1],
                        multiple = F,
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select baseline scenario ",
                                       `windowPadding` = 1,
                                       `width` = " css-width ")
                    )
                    
                }else
                    if (input$DefOrUserUpload_S == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "S_scen_base",
                            label = "Select the baseline management scenario",
                            choices = unique(Spatial_data()$Scenario),
                            # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                            #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                            #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                            #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                            #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                            #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                            #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                            #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                            #              "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                            #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                            #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                            # 
                            # ),
                            unique(Spatial_data()$Scenario)[1],
                            multiple = F,
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select baseline scenario ",
                                           `windowPadding` = 1,
                                           `width` = " css-width ")
                        )
                        
                    }
        
    })
    
    
    output$Spatial_scen_comp <- renderUI({
        if (input$DefOrUserUpload_S == 'Upload data') {
            req(Spatial_data())
            pickerInput(
                "S_scen_comp",
                "Select the management scenario to compare",
                unique(Spatial_data()$Scenario),
                unique(Spatial_data()$Scenario)[2],
                multiple = F,
                options = list(`actions-box` = TRUE,
                               `header` = "Select comparison scenario ",
                               `windowPadding` = 1,
                               `width` = " css-width ")
            )
        } else
            if (input$DefOrUserUpload_S == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "S_scen_comp",
                    label = "Select the management scenario to compare",
                    choices = unique(Spatial_data()$Scenario),
                    # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                    #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                    #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                    #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                    #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                    #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                    #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                    #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                    #              "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                    #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                    #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                    #              
                    # ),
                    unique(Spatial_data()$Scenario)[2],
                    multiple = F,
                    options = list(`actions-box` = TRUE,
                                   `header` = "Select comparison scenario ",
                                   `windowPadding` = 1,
                                   `width` = " css-width ")
                )
                
            }else
                if (input$DefOrUserUpload_S == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "S_scen_comp",
                        label = "Select the management scenario to compare",
                        choices = unique(Spatial_data()$Scenario),
                        # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                        #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                        #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                        #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                        #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                        #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                        #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                        #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                        #              "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                        #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                        #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                        #              
                        # ),
                        unique(Spatial_data()$Scenario)[2],
                        multiple = F,
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select comparison scenario ",
                                       `windowPadding` = 1,
                                       `width` = " css-width ")
                    )
                    
                }else
                    if (input$DefOrUserUpload_S == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "S_scen_comp",
                            label = "Select the management scenario to compare",
                            choices = unique(Spatial_data()$Scenario),
                            # choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                            #              "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                            #              "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                            #              "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                            #              "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                            #              "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                            #              "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                            #              "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                            #              "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                            #              "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                            #              "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                            # 
                            # ),
                            unique(Spatial_data()$Scenario)[2],
                            multiple = F,
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select comparison scenario ",
                                           `windowPadding` = 1,
                                           `width` = " css-width ")
                        )
                        
                    }
        
    })
    
    
    output$S_var <- renderUI({
        if (input$DefOrUserUpload_S == 'Upload data') {
            req(Spatial_data())
            pickerInput(
                "S_variable",
                "Select the Water quantity/quality metric of interest",
                choices = c (
                    "Sediment Yield (kg/ha)" = "SdYd_kg_ha",
                    "Sediment deposition (kg/ha)" = "SdDp_kg_ha",
                    "Soil Loss (kg/ha)" = "SoLs_kg_ha",
                    "Total Phosphorus (kg/ha)" = "TP_kg_ha_" ,
                    "Soluble Reactive Phosphorus (kg/ha)" = "SRP_kg_ha_",
                    "Particulate Phosphorus (kg/ha)" = "PP_kg_ha_",
                    "Runoff (mm)" = "Runoff_mm_",
                    # "Slope (%)" = "slope",
                    "DepLos_kg_" = "DepLos_kg_"
                ),
                selected = "SdYd_kg_ha",
                multiple = F,
                options = list(`actions-box` = TRUE,
                               `header` = "Select metric ",
                               `windowPadding` = 1,
                               `width` = " css-width ")
            )
        } else
            if (input$DefOrUserUpload_S == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "S_variable",
                    label = "Select the Water quantity/quality metric of interest",
                    choices = c (
                        "Sediment Yield (kg/ha)" = "SdYd_kg_ha",
                        "Sediment deposition (kg/ha)" = "SdDp_kg_ha",
                        "Soil Loss (kg/ha)" = "SoLs_kg_ha",
                        "Total Phosphorus (kg/ha)" = "TP_kg_ha_" ,
                        "Soluble Reactive Phosphorus (kg/ha)" = "SRP_kg_ha_",
                        "Particulate Phosphorus (kg/ha)" = "PP_kg_ha_",
                        "Runoff (mm)" = "Runoff_mm_",
                        # "Slope (%)" = "slope",
                        "DepLos_kg_" = "DepLos_kg_"
                    ),
                    selected = "SdYd_kg_ha",
                    multiple = F,
                    options = list(`actions-box` = TRUE,
                                   `header` = "Select metric ",
                                   `windowPadding` = 1,
                                   `width` = " css-width ")
                )
                
            }else
                if (input$DefOrUserUpload_S == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "S_variable",
                        label = "Select the Water quantity/quality metric of interest",
                        choices = c (
                            "Sediment Yield (kg/ha)" = "SdYd_kg_ha",
                            "Sediment deposition (kg/ha)" = "SdDp_kg_ha",
                            "Soil Loss (kg/ha)" = "SoLs_kg_ha",
                            "Total Phosphorus (kg/ha)" = "TP_kg_ha_" ,
                            "Soluble Reactive Phosphorus (kg/ha)" = "SRP_kg_ha_",
                            "Particulate Phosphorus (kg/ha)" = "PP_kg_ha_",
                            "Runoff (mm)" = "Runoff_mm_",
                            # "Slope (%)" = "slope",
                            "DepLos_kg_" = "DepLos_kg_"
                        ),
                        selected = "SdYd_kg_ha",
                        multiple = F,
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select metric ",
                                       `windowPadding` = 1,
                                       `width` = " css-width ")
                    )
                    
                }else
                    if (input$DefOrUserUpload_S == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "S_variable",
                            label = "Select the Water quantity/quality metric of interest",
                            choices = c (
                                "Sediment Yield (kg/ha)" = "SdYd_kg_ha",
                                "Sediment deposition (kg/ha)" = "SdDp_kg_ha",
                                "Soil Loss (kg/ha)" = "SoLs_kg_ha",
                                "Total Phosphorus (kg/ha)" = "TP_kg_ha_" ,
                                "Soluble Reactive Phosphorus (kg/ha)" = "SRP_kg_ha_",
                                "Particulate Phosphorus (kg/ha)" = "PP_kg_ha_",
                                "Runoff (mm)" = "Runoff_mm_",
                                # "Slope (%)" = "slope",
                                "DepLos_kg_" = "DepLos_kg_"
                            ),
                            selected = "SdYd_kg_ha",
                            multiple = F,
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select metric ",
                                           `windowPadding` = 1,
                                           `width` = " css-width ")
                        )
                        
                    }else
                        if (input$DefOrUserUpload_S == 'Default_Data_Palouse') {
                            pickerInput(
                                inputId = "S_variable",
                                label = "Select the Water quantity/quality metric of interest",
                                choices = c (
                                    "Sediment Yield (kg/ha)" = "SdYd_kg_ha",
                                    "Sediment deposition (kg/ha)" = "SdDp_kg_ha",
                                    "Soil Loss (kg/ha)" = "SoLs_kg_ha",
                                    "Total Phosphorus (kg/ha)" = "TP_kg_ha_" ,
                                    "Soluble Reactive Phosphorus (kg/ha)" = "SRP_kg_ha_",
                                    "Particulate Phosphorus (kg/ha)" = "PP_kg_ha_",
                                    "Runoff (mm)" = "Runoff_mm_",
                                    # "Slope (%)" = "slope",
                                    "DepLos_kg_" = "DepLos_kg_"
                                ),
                                selected = "SdYd_kg_ha",
                                multiple = F,
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select metric ",
                                               `windowPadding` = 1,
                                               `width` = " css-width ")
                            )
                            
                        }
        
    })
    
    
    output$tab_sp <- renderUI({
        req(spdftab())
        fluidRow(
            column(
                12,
                align = "center",
                offset = 0,
                style = "background-color:#ECF0F1;",
                DT::dataTableOutput("spatial_table")
            )
        )
    })
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------Plotting logic-------------------------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    
    
    ################# Filtering logic for HILLSLOPE DF#################
    
    hill_subset <- reactive({
        req(Hill_data())
        Hill_data() %>%
            dplyr::filter(Watershed %in% input$Hill_wshed)
    })
    
    
    ################# Filtering logic for CHANNEL DF ################
    
    Chan_subset <- reactive({
        req(Chan_data())
        Chan_data() %>%
            dplyr::filter(Watershed %in% input$Chan_wshed)
    })
    
    
    ################# Filtering logic for WATERSHED DF ################
    
    Wshed_subset <- reactive({
        req(Wshed_data())
        req(input$AreaVsScen)
        if (input$AreaVsScen == 'allscen') {
            Wshed_data() %>% dplyr::filter(Watershed %in% input$Wshed_wshed)
        } else
            if (input$AreaVsScen == 'allwat') {
                Wshed_data() %>% dplyr::filter(Scenario %in% input$Wshed_wshed,
                                               Watershed %in% input$Wshed_wshed_S)
            }
    })
    
    ################# Filtering logic for spatial DF #################
    
    Spatial_subset <- reactive({
        req(Spatial_data())
        req(input$S_wshed)
        Spatial_data() %>%
            dplyr::filter(Watershed %in% input$S_wshed &
                              Scenario %in% input$S_scen)%>% 
            arrange_at(.vars = input$S_variable, desc) %>%
            mutate(cumPercArea = cumsum(area_ha_) / sum(area_ha_) *
                       100) %>% dplyr::mutate_if(is.numeric, round, 2) %>% 
            dplyr::filter(cumPercArea < input$thresh_S  & slope > min(input$thresh_slope_S) & slope < max(input$thresh_slope_S) )
    })
    
    Spatial_subset_base <- reactive({
        req(Spatial_data())
        req(input$S_wshed)
        Spatial_data() %>%
            dplyr::filter(Watershed %in% input$S_wshed &
                              Scenario %in% input$S_scen_base)%>% 
            arrange_at(.vars = input$S_variable, desc) %>%
            mutate(cumPercArea = cumsum(area_ha_) / sum(area_ha_) *
                       100) %>% dplyr::mutate_if(is.numeric, round, 2) %>% 
            dplyr::filter(cumPercArea < input$thresh_S  & slope > min(input$thresh_slope_S) & slope < max(input$thresh_slope_S) )
    })
    
    
    ############## Creates column in spatial df for each variable with values  relative to the chosen baseline scenario   ##############
    spsub <- reactive({ Spatial_data() %>%
            dplyr::filter(Watershed %in% input$S_wshed) 
    })
    
    Spatial_data_rel<- reactive({ spsub() %>%
            dplyr::filter(Watershed %in% input$S_wshed) %>%
            dplyr::filter(Scenario != input$S_scen_base) %>%
            dplyr::mutate(AbsChange_SoLs_kg_ha = SoLs_kg_ha- spsub()$SoLs_kg_ha[spsub()$Scenario==input$S_scen_base],
                          AbsChange_SdDp_kg_ha = SdDp_kg_ha- spsub()$SdDp_kg_ha[spsub()$Scenario==input$S_scen_base],
                          AbsChange_SdYd_kg_ha = SdYd_kg_ha- spsub()$SdYd_kg_ha[spsub()$Scenario==input$S_scen_base],
                          AbsChange_SRP_kg_ha_ = SRP_kg_ha_- spsub()$SRP_kg_ha_[spsub()$Scenario==input$S_scen_base],
                          AbsChange_PP_kg_ha_ = PP_kg_ha_- spsub()$PP_kg_ha_[spsub()$Scenario==input$S_scen_base],
                          AbsChange_TP_kg_ha_ = TP_kg_ha_- spsub()$TP_kg_ha_[spsub()$Scenario==input$S_scen_base],
                          AbsChange_Runoff_mm_ = Runoff_mm_- spsub()$Runoff_mm_[spsub()$Scenario==input$S_scen_base],
                          AbsChange_DepLos_kg_ = DepLos_kg_- spsub()$DepLos_kg_[spsub()$Scenario==input$S_scen_base],
            )
    })
    
    
    # Spatial_subset_comp <- reactive({
    #     req(Spatial_data_rel())
    #     #req(input$S_wshed)
    #     Spatial_data_rel() %>%
    #         dplyr::filter(Scenario %in% input$S_scen_comp)%>% 
    #         arrange_at(.vars = input$S_variable, desc) %>%
    #         mutate(cumPercArea = cumsum(area_ha_) / sum(area_ha_) *
    #                    100) %>% dplyr::mutate_if(is.numeric, round, 2) %>% 
    #         dplyr::filter(cumPercArea < input$thresh_S  & slope > min(input$thresh_slope_S) & slope < max(input$thresh_slope_S) )
    # })
    
    Spatial_subset_comp <- reactive({
        req(Spatial_data_rel())
        req(input$S_wshed)
        Spatial_data_rel() %>%
            dplyr::filter(Watershed %in% input$S_wshed &
                              Scenario %in% input$S_scen_comp)%>%
            arrange_at(.vars = input$S_variable, desc) %>%
            mutate(cumPercArea = cumsum(area_ha_) / sum(area_ha_) *
                       100) %>% dplyr::mutate_if(is.numeric, round, 2) %>%
            dplyr::filter(cumPercArea < input$thresh_S  & slope > min(input$thresh_slope_S) & slope < max(input$thresh_slope_S))
    })
    
    
    # spatial_shared <-  reactive({ SharedData$new(Spatial_subset())
    
    
    ################# Filtering logic for spatial channel DF #################
    
    Spatial_subset_chan <- reactive({
        req(Spatial_data_chan())
        req(input$S_wshed)
        Spatial_data_chan() %>%
            dplyr::filter(Watershed %in% input$S_wshed &
                              Scenario %in% input$S_scen)
    })
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------Dataframe Calculations for hillslopes-------------------------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    
    ############## Takes in df filtered by input watershed and creates column for each variable with values   ##############
    ##############            relative to the chosen baseline scenario   ##############
    
    ####  
    
    hill_subset_rel<- reactive({ hill_subset() %>% 
            dplyr::filter(Scenario != input$Hill_scen_base) %>%
            dplyr::mutate(AbsChange_Runoff..mm. = Runoff..mm.- hill_subset()$Runoff..mm.[hill_subset()$Scenario==input$Hill_scen_base],
                          # AbsChange_Lateral.Flow..mm. = Lateral.Flow..mm.- hill_subset()$Lateral.Flow..mm.[hill_subset()$Scenario==input$Hill_scen_base],
                          # AbsChange_Baseflow..mm.  = Baseflow..mm.- hill_subset()$Baseflow..mm.[hill_subset()$Scenario==input$Hill_scen_base],
                          AbsChange_Soil_Loss_kg = Soil_Loss_kg- hill_subset()$Soil_Loss_kg[hill_subset()$Scenario==input$Hill_scen_base],
                          AbsChange_Sediment_Deposition_kg = Sediment_Deposition_kg- hill_subset()$Sediment_Deposition_kg[hill_subset()$Scenario==input$Hill_scen_base],
                          AbsChange_Sediment_Yield_kg = Sediment_Yield_kg- hill_subset()$Sediment_Yield_kg[hill_subset()$Scenario==input$Hill_scen_base],
                          AbsChange_Soluble_Reactive_P_kg = Soluble_Reactive_P_kg- hill_subset()$Soluble_Reactive_P_kg[hill_subset()$Scenario==input$Hill_scen_base],
                          AbsChange_Particulate_P_kg = Particulate_P_kg- hill_subset()$Particulate_P_kg[hill_subset()$Scenario==input$Hill_scen_base],
                          AbsChange_Total_P_kg = Total_P_kg- hill_subset()$Total_P_kg[hill_subset()$Scenario==input$Hill_scen_base],
                          AbsChange_Particle_Class_1_Fraction_kg = Particle_Class_1_Fraction_kg- hill_subset()$Particle_Class_1_Fraction_kg[hill_subset()$Scenario==input$Hill_scen_base],
                          AbsChange_Particle_Class_2_Fraction_kg = Particle_Class_2_Fraction_kg- hill_subset()$Particle_Class_2_Fraction_kg[hill_subset()$Scenario==input$Hill_scen_base],
                          AbsChange_Particle_Class_3_Fraction_kg = Particle_Class_3_Fraction_kg- hill_subset()$Particle_Class_3_Fraction_kg[hill_subset()$Scenario==input$Hill_scen_base],
                          AbsChange_Particle_Class_4_Fraction_kg = Particle_Class_4_Fraction_kg- hill_subset()$Particle_Class_4_Fraction_kg[hill_subset()$Scenario==input$Hill_scen_base],
                          AbsChange_Particle_Class_5_Fraction_kg = Particle_Class_5_Fraction_kg- hill_subset()$Particle_Class_5_Fraction_kg[hill_subset()$Scenario==input$Hill_scen_base],
                          AbsChange_Particle_Fraction_Under_0.016_mm_kg = Particle_Fraction_Under_0.016_mm_kg- hill_subset()$Particle_Fraction_Under_0.016_mm_kg[hill_subset()$Scenario==input$Hill_scen_base],
                          AbsChange_Sediment_Yield_of_Particles_Under_0.016_mm_kg = Sediment_Yield_of_Particles_Under_0.016_mm_kg- hill_subset()$Sediment_Yield_of_Particles_Under_0.016_mm_kg[hill_subset()$Scenario==input$Hill_scen_base]
            )
    })
    
    
    
    ############## Dataframe calculating cumulative percent of total variable: Hillslope   ##############
    ### this is the DF for plot 1 on hillslopes tab
    hill_arr_by_var_HA <- reactive({
        hill_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$Hill_variable, desc) %>%
            mutate(
                cumPercLen = cumsum(Length..m.) / sum(Length..m.) * 100,
                cumPercArea = cumsum(Hillslope.Area..ha.) / sum(Hillslope.Area..ha.) *
                    100,
                cumRunoff.mm = cumsum(Runoff..mm.) / sum(Runoff..mm.) *
                    100,
                # cumLateralflow.mm = cumsum(Lateral.Flow..mm.) / sum(Lateral.Flow..mm.) *
                #     100,
                # cumBaseflow.mm = cumsum(Baseflow..mm.) / sum(Baseflow..mm.) *
                #     100,
                cumSoilLoss.kg.ha = cumsum(Soil_Loss_kg) / sum(Soil_Loss_kg) *
                    100,
                cumSedDep.kg.ha = cumsum(Sediment_Deposition_kg) /
                    sum(Sediment_Deposition_kg) * 100,
                cumSedYield.kg.ha = cumsum(Sediment_Yield_kg) /
                    sum(Sediment_Yield_kg) * 100,
                cumSRP.kg.ha.3 = cumsum(Soluble_Reactive_P_kg) /
                    sum(Soluble_Reactive_P_kg) * 100,
                cumParticulateP.kg.ha.3 = cumsum(Particulate_P_kg) /
                    sum(Particulate_P_kg) * 100,
                cumTotalP.kg.ha.3 = cumsum(Total_P_kg) / sum(Total_P_kg) *
                    100,
                cumParticle.Class.1.Fraction = cumsum(Particle_Class_1_Fraction_kg) /
                    sum(Particle_Class_1_Fraction_kg) * 100,
                cumParticle.Class.2.Fraction = cumsum(Particle_Class_2_Fraction_kg) /
                    sum(Particle_Class_2_Fraction_kg) * 100,
                cumParticle.Class.3.Fraction = cumsum(Particle_Class_3_Fraction_kg) /
                    sum(Particle_Class_3_Fraction_kg) * 100,
                cumParticle.Class.4.Fraction = cumsum(Particle_Class_4_Fraction_kg) /
                    sum(Particle_Class_4_Fraction_kg) * 100,
                cumParticle.Class.5.Fraction = cumsum(Particle_Class_5_Fraction_kg) /
                    sum(Particle_Class_5_Fraction_kg) * 100,
                cumParticle.Fraction.Under.0.016.mm = cumsum(Particle_Fraction_Under_0.016_mm_kg) /
                    sum(Particle_Fraction_Under_0.016_mm_kg) * 100,
                cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. = cumsum(Sediment_Yield_of_Particles_Under_0.016_mm_kg) /
                    sum(Sediment_Yield_of_Particles_Under_0.016_mm_kg) * 100
            ) %>% dplyr::filter(Scenario %in% c(input$Hill_scen_base,  input$Hill_scen_comp)) %>% dplyr::filter(cumPercArea < input$thresh_H) %>%
            ungroup()
    })
    
    
    
    # 
    ## this is the dataframe for plot 3 on the hillslopes tab (the channel length plot)
    hill_arr_by_var_CL <- reactive({
        hill_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$Hill_variable, desc) %>%
            mutate(
                cumPercLen = cumsum(Length..m.) / sum(Length..m.) * 100,
                cumPercArea = cumsum(Hillslope.Area..ha.) / sum(Hillslope.Area..ha.) *
                    100,
                cumRunoff.mm = cumsum(Runoff..mm.) / sum(Runoff..mm.) *
                    100,
                # cumLateralflow.mm = cumsum(Lateral.Flow..mm.) / sum(Lateral.Flow..mm.) *
                #     100,
                # cumBaseflow.mm = cumsum(Baseflow..mm.) / sum(Baseflow..mm.) *
                #     100,
                cumSoilLoss.kg.ha = cumsum(Soil_Loss_kg) / sum(Soil_Loss_kg) *
                    100,
                cumSedDep.kg.ha = cumsum(Sediment_Deposition_kg) /
                    sum(Sediment_Deposition_kg) * 100,
                cumSedYield.kg.ha = cumsum(Sediment_Yield_kg) /
                    sum(Sediment_Yield_kg) * 100,
                cumSRP.kg.ha.3 = cumsum(Soluble_Reactive_P_kg) /
                    sum(Soluble_Reactive_P_kg) * 100,
                cumParticulateP.kg.ha.3 = cumsum(Particulate_P_kg) /
                    sum(Particulate_P_kg) * 100,
                cumTotalP.kg.ha.3 = cumsum(Total_P_kg) / sum(Total_P_kg) *
                    100,
                cumParticle.Class.1.Fraction = cumsum(Particle_Class_1_Fraction_kg) /
                    sum(Particle_Class_1_Fraction_kg) * 100,
                cumParticle.Class.2.Fraction = cumsum(Particle_Class_2_Fraction_kg) /
                    sum(Particle_Class_2_Fraction_kg) * 100,
                cumParticle.Class.3.Fraction = cumsum(Particle_Class_3_Fraction_kg) /
                    sum(Particle_Class_3_Fraction_kg) * 100,
                cumParticle.Class.4.Fraction = cumsum(Particle_Class_4_Fraction_kg) /
                    sum(Particle_Class_4_Fraction_kg) * 100,
                cumParticle.Class.5.Fraction = cumsum(Particle_Class_5_Fraction_kg) /
                    sum(Particle_Class_5_Fraction_kg) * 100,
                cumParticle.Fraction.Under.0.016.mm = cumsum(Particle_Fraction_Under_0.016_mm_kg) /
                    sum(Particle_Fraction_Under_0.016_mm_kg) * 100,
                cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. = cumsum(Sediment_Yield_of_Particles_Under_0.016_mm_kg) /
                    sum(Sediment_Yield_of_Particles_Under_0.016_mm_kg) * 100
            )%>% dplyr::filter(Scenario %in% c(input$Hill_scen_base,  input$Hill_scen_comp)) %>% dplyr::filter(cumPercLen < input$thresh_H) %>%
            ungroup()
    })
    
    
    
    # ############## Dataframe calculating cumulative absolute value of variable: Hillslope   ##############
    # ### this is the DF for plot 2 on hillslopes tab
    hill_arr_by_var_HA_abs <- reactive({
        hill_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$Hill_variable, desc) %>%
            mutate(
                cumPercLen = cumsum(Length..m.) / sum(Length..m.) * 100,
                cumPercArea = cumsum(Hillslope.Area..ha.) / sum(Hillslope.Area..ha.) *
                    100,
                cumRunoff.mm = cumsum(Runoff..mm.),
                # cumLateralflow.mm = cumsum(Lateral.Flow..mm.),
                # cumBaseflow.mm = cumsum(Baseflow..mm.),
                cumSoilLoss.kg.ha = cumsum(Soil_Loss_kg),
                cumSedDep.kg.ha = cumsum(Sediment_Deposition_kg),
                cumSedYield.kg.ha = cumsum(Sediment_Yield_kg),
                cumSRP.kg.ha.3 = cumsum(Soluble_Reactive_P_kg),
                cumParticulateP.kg.ha.3 = cumsum(Particulate_P_kg),
                cumTotalP.kg.ha.3 = cumsum(Total_P_kg),
                cumParticle.Class.1.Fraction = cumsum(Particle_Class_1_Fraction_kg),
                cumParticle.Class.2.Fraction = cumsum(Particle_Class_2_Fraction_kg),
                cumParticle.Class.3.Fraction = cumsum(Particle_Class_3_Fraction_kg),
                cumParticle.Class.4.Fraction = cumsum(Particle_Class_4_Fraction_kg),
                cumParticle.Class.5.Fraction = cumsum(Particle_Class_5_Fraction_kg),
                cumParticle.Fraction.Under.0.016.mm = cumsum(Particle_Fraction_Under_0.016_mm_kg),
                cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. = cumsum(Sediment_Yield_of_Particles_Under_0.016_mm_kg)
            ) %>% dplyr::filter(Scenario %in% c(input$Hill_scen_base,  input$Hill_scen_comp))%>% dplyr::filter(cumPercArea < input$thresh_H) %>%
            ungroup()
    })
    
    
    # ### this is the DF for plot 4 on hillslopes tab
    hill_arr_by_var_CL_abs <- reactive({
        hill_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$Hill_variable, desc) %>%
            mutate(
                cumPercLen = cumsum(Length..m.) / sum(Length..m.) * 100,
                cumPercArea = cumsum(Hillslope.Area..ha.) / sum(Hillslope.Area..ha.) *
                    100,
                cumRunoff.mm = cumsum(Runoff..mm.),
                # cumLateralflow.mm = cumsum(Lateral.Flow..mm.),
                # cumBaseflow.mm = cumsum(Baseflow..mm.),
                cumSoilLoss.kg.ha = cumsum(Soil_Loss_kg),
                cumSedDep.kg.ha = cumsum(Sediment_Deposition_kg),
                cumSedYield.kg.ha = cumsum(Sediment_Yield_kg),
                cumSRP.kg.ha.3 = cumsum(Soluble_Reactive_P_kg),
                cumParticulateP.kg.ha.3 = cumsum(Particulate_P_kg),
                cumTotalP.kg.ha.3 = cumsum(Total_P_kg),
                cumParticle.Class.1.Fraction = cumsum(Particle_Class_1_Fraction_kg),
                cumParticle.Class.2.Fraction = cumsum(Particle_Class_2_Fraction_kg),
                cumParticle.Class.3.Fraction = cumsum(Particle_Class_3_Fraction_kg),
                cumParticle.Class.4.Fraction = cumsum(Particle_Class_4_Fraction_kg),
                cumParticle.Class.5.Fraction = cumsum(Particle_Class_5_Fraction_kg),
                cumParticle.Fraction.Under.0.016.mm = cumsum(Particle_Fraction_Under_0.016_mm_kg),
                cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. = cumsum(Sediment_Yield_of_Particles_Under_0.016_mm_kg)
            )%>% dplyr::filter(Scenario %in% c(input$Hill_scen_base,  input$Hill_scen_comp)) %>% dplyr::filter(cumPercLen < input$thresh_H) %>%
            ungroup()
    })
    ## -----------------------------------------------------------------------------------------------------------##   
    
    output$Plot_vs_cumPercArea <- renderPlotly({
        req(input$Hill_variable)
        req(hill_arr_by_var_HA())
        
        p1 <- hill_arr_by_var_HA()  %>% ggplot(aes(x = cumPercArea))
        if (input$Hill_variable == "Runoff..mm.") {
            p1 <-
                p1 + geom_line(aes(y = cumRunoff.mm  , color = Scenario), size = 0.5)
        } else
            if (input$Hill_variable == "Lateral.Flow..mm.") {
                p1 <-
                    p1 + geom_line(aes(y = cumLateralflow.mm, color = Scenario), size = 0.5)
            } else
                if (input$Hill_variable == "Baseflow..mm.") {
                    p1 <-
                        p1 + geom_line(aes(y = cumBaseflow.mm, color = Scenario), size = 0.5)
                } else
                    if (input$Hill_variable == "Soil_Loss_kg") {
                        p1 <-
                            p1 + geom_line(aes(y = cumSoilLoss.kg.ha, color = Scenario), size = 0.5)
                    } else
                        if (input$Hill_variable == "Sediment_Deposition_kg") {
                            p1 <-
                                p1 + geom_line(aes(y = cumSedDep.kg.ha, color = Scenario), size = 0.5)
                        } else
                            if (input$Hill_variable == "Sediment_Yield_kg") {
                                p1 <-
                                    p1 + geom_line(aes(y = cumSedYield.kg.ha, color = Scenario), size = 0.5)
                            } else
                                if (input$Hill_variable == "Soluble_Reactive_P_kg") {
                                    p1 <-
                                        p1 + geom_line(aes(y = cumSRP.kg.ha.3, color = Scenario), size = 0.5)
                                } else
                                    if (input$Hill_variable == "Particulate_P_kg") {
                                        p1 <-
                                            p1 + geom_line(aes(y = cumParticulateP.kg.ha.3, color = Scenario),
                                                           size = 0.5)
                                    } else
                                        if (input$Hill_variable == "Total_P_kg") {
                                            p1 <-
                                                p1 + geom_line(aes(y = cumTotalP.kg.ha.3, color = Scenario), size = 0.5)
                                        } else
                                            if (input$Hill_variable == "Particle_Class_1_Fraction_kg") {
                                                p1 <-
                                                    p1 + geom_line(aes(y = cumParticle.Class.1.Fraction, color = Scenario),
                                                                   size = 0.5)
                                            } else
                                                if (input$Hill_variable == "Particle_Class_2_Fraction_kg") {
                                                    p1 <-
                                                        p1 + geom_line(aes(y = cumParticle.Class.2.Fraction, color = Scenario),
                                                                       size = 0.5)
                                                } else
                                                    if (input$Hill_variable == "Particle_Class_3_Fraction_kg") {
                                                        p1 <-
                                                            p1 + geom_line(aes(y = cumParticle.Class.3.Fraction, color = Scenario),
                                                                           size = 0.5)
                                                    } else
                                                        if (input$Hill_variable == "Particle_Class_4_Fraction_kg") {
                                                            p1 <-
                                                                p1 + geom_line(aes(y = cumParticle.Class.4.Fraction, color = Scenario),
                                                                               size = 0.5)
                                                        } else
                                                            if (input$Hill_variable == "Particle_Class_5_Fraction_kg") {
                                                                p1 <-
                                                                    p1 + geom_line(aes(y = cumParticle.Class.5.Fraction, color = Scenario),
                                                                                   size = 0.5)
                                                            } else
                                                                if (input$Hill_variable == "Particle_Fraction_Under_0.016_mm_kg") {
                                                                    p1 <-
                                                                        p1 + geom_line(aes(y = cumParticle.Fraction.Under.0.016.mm, color = Scenario),
                                                                                       size = 0.5)
                                                                } else
                                                                    if (input$Hill_variable == "Sediment_Yield_of_Particles_Under_0.016_mm_kg") {
                                                                        p1 <-
                                                                            p1 + geom_line(
                                                                                aes(y = cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. , color = Scenario),
                                                                                size = 0.5
                                                                            )
                                                                    }
        
        
        p1 <- p1 + theme_bw() +
            theme(
                axis.title = element_text(
                    size = 10,
                    color = "Black",
                    face = "bold"
                ),
                axis.text = element_text(
                    size = 10,
                    color = "BLACK",
                    face = "bold"
                ),
                legend.title = element_text(
                    size = 10,
                    color = "BLACK",
                    face = "bold"
                ),
                legend.text = element_text(size = 10, color = "BLACK"),
                legend.position = "none",
                plot.margin=unit(c(2,0.5,1,0.5),"cm"),
                plot.title = element_text(
                    size = 10,
                    color = "#000000",
                    face = "bold",
                    family="Bauhaus 93",
                    vjust = 1,
                    hjust = 0.5
                    
                )
            ) +
            labs(
                x = "Percent of total hillslope area",
                y = "Cumulative Percent of Total selected variable",
                # y = paste("Percent of total", input$Hill_variable, sep = " "),
                title= paste(
                    "Cumulative percent of total",
                    input$Hill_variable ,
                    "\ncontribution by percent hillslope area"
                )
                ,
                colour = "Scenario"
            )
        if (input$DefOrUserUpload_H == 'Default_Data_Portland'| input$DefOrUserUpload_H == 'Default_Data_Seattle') {
            p1 <- p1 +
                scale_color_manual(
                    values = c(
                        "SimFireEagle_202009_cl532" =
                            "#B22222",
                        "SimFireNorse_202009_cl532" = "#FF0000",
                        "HighSevS_202009" = "#DC143C",
                        "ModSevS_202009" =
                            "#DC143C",
                        "LowSevS_202009" =
                            "#FF6347",
                        "PrescFireS_202009" =
                            "#E9967A",
                        "CurCond_202009_cl532future" =
                            "#32CD32",
                        "CurCond_202009_cl532gridmet" =
                            "#00FF00",
                        "CurCond_202009_cl532" =
                            "#008000",
                        "Thinn85.2020.ki5krcs.chn_12" ="#7CFC00"
                    )
                )
        }else
            if (input$DefOrUserUpload_H == 'Default_Data_LT') {
                p1 <- p1 +
                    scale_color_manual(
                        values = c(
                            "SimFire_landisFuels_fut_cli_A2" = "#FF0000",
                            "SimFire_landisFuels_obs_cli" = "#B22222",
                            "SimFire_fccsFuels_obs_cli" = "#4d2525",
                            "HighSev" = "#DC143C",
                            "ModSev" = "#DC143C",
                            "LowSev" = "#FF6347",
                            "PrescFire" = "#E9967A",
                            "Thinn85" = "#7CFC00",
                            "Thinn93" = "#32CD32",
                            "Thinn96" = "#00FF00",
                            "CurCond" = "#008000"
                        )
                    )
            } else
                if (input$DefOrUserUpload_H == 'Upload Data') {
                    p1 <- p1 +
                        scale_color_brewer(palette = "virdis")
                }
        
        
        p1
        
        # ggplotly(p1,dynamicTicks = TRUE) %>%
        #     rangeslider() %>%
        #     layout(hovermode = "x")
        
    })
    #
    #
    output$Plot_vs_cumPercLen <- renderPlotly({
        req(input$Hill_variable)
        req(hill_arr_by_var_CL())
        
        p3 <- hill_arr_by_var_CL()  %>% ggplot(aes(x = cumPercLen))
        if (input$Hill_variable == "Runoff..mm.") {
            p3 <-
                p3 + geom_line(aes(y = cumRunoff.mm  , color = Scenario), size = 0.5)
        } else
            if (input$Hill_variable == "Lateral.Flow..mm.") {
                p3 <-
                    p3 + geom_line(aes(y = cumLateralflow.mm, color = Scenario), size = 0.5)
            } else
                if (input$Hill_variable == "Baseflow..mm.") {
                    p3 <-
                        p3 + geom_line(aes(y = cumBaseflow.mm, color = Scenario), size = 0.5)
                } else
                    if (input$Hill_variable == "Soil_Loss_kg") {
                        p3 <-
                            p3 + geom_line(aes(y = cumSoilLoss.kg.ha, color = Scenario), size = 0.5)
                    } else
                        if (input$Hill_variable == "Sediment_Deposition_kg") {
                            p3 <-
                                p3 + geom_line(aes(y = cumSedDep.kg.ha, color = Scenario), size = 0.5)
                        } else
                            if (input$Hill_variable == "Sediment_Yield_kg") {
                                p3 <-
                                    p3 + geom_line(aes(y = cumSedYield.kg.ha, color = Scenario), size = 0.5)
                            } else
                                if (input$Hill_variable == "Soluble_Reactive_P_kg") {
                                    p3 <-
                                        p3 + geom_line(aes(y = cumSRP.kg.ha.3, color = Scenario), size = 0.5)
                                } else
                                    if (input$Hill_variable == "Particulate_P_kg") {
                                        p3 <-
                                            p3 + geom_line(aes(y = cumParticulateP.kg.ha.3, color = Scenario),
                                                           size = 0.5)
                                    } else
                                        if (input$Hill_variable == "Total_P_kg") {
                                            p3 <-
                                                p3 + geom_line(aes(y = cumTotalP.kg.ha.3, color = Scenario), size = 0.5)
                                        } else
                                            if (input$Hill_variable == "Particle_Class_1_Fraction_kg") {
                                                p3 <-
                                                    p3 + geom_line(aes(y = cumParticle.Class.1.Fraction, color = Scenario),
                                                                   size = 0.5)
                                            } else
                                                if (input$Hill_variable == "Particle_Class_2_Fraction_kg") {
                                                    p3 <-
                                                        p3 + geom_line(aes(y = cumParticle.Class.2.Fraction, color = Scenario),
                                                                       size = 0.5)
                                                } else
                                                    if (input$Hill_variable == "Particle_Class_3_Fraction_kg") {
                                                        p3 <-
                                                            p3 + geom_line(aes(y = cumParticle.Class.3.Fraction, color = Scenario),
                                                                           size = 0.5)
                                                    } else
                                                        if (input$Hill_variable == "Particle_Class_4_Fraction_kg") {
                                                            p3 <-
                                                                p3 + geom_line(aes(y = cumParticle.Class.4.Fraction, color = Scenario),
                                                                               size = 0.5)
                                                        } else
                                                            if (input$Hill_variable == "Particle_Class_5_Fraction_kg") {
                                                                p3 <-
                                                                    p3 + geom_line(aes(y = cumParticle.Class.5.Fraction, color = Scenario),
                                                                                   size = 0.5)
                                                            } else
                                                                if (input$Hill_variable == "Particle_Fraction_Under_0.016_mm_kg") {
                                                                    p3 <-
                                                                        p3 + geom_line(aes(y = cumParticle.Fraction.Under.0.016.mm, color = Scenario),
                                                                                       size = 0.5)
                                                                } else
                                                                    if (input$Hill_variable == "Sediment_Yield_of_Particles_Under_0.016_mm_kg") {
                                                                        p3 <-
                                                                            p3 + geom_line(
                                                                                aes(y = cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. , color = Scenario),
                                                                                size = 0.5
                                                                            )
                                                                    }
        
        
        p3 <- p3 +  theme_bw() +
            theme(
                axis.title = element_text(
                    size = 10,
                    color = "Black",
                    face = "bold"
                ),
                axis.text = element_text(
                    size = 10,
                    color = "BLACK",
                    face = "bold"
                ),
                legend.title = element_text(
                    size = 10,
                    color = "BLACK",
                    face = "bold"
                ),
                legend.text = element_text(size = 10, color = "BLACK"),
                legend.position = "none",
                plot.margin=unit(c(2,0.5,1,0.5),"cm"),
                plot.title = element_text(
                    size = 10,
                    color = "#000000",
                    face = "bold",
                    family="Bauhaus 93",
                    vjust = 1,
                    hjust = 0.5
                    
                )
            ) +
            labs(
                x = "Percent of total channel length",
                y = "Cumulative Percent of Total selected variable",
                # y = paste("Percent of total", input$Hill_variable, sep = " "),
                title= paste(
                    "Cumulative percent of total",
                    input$Hill_variable ,
                    "\ncontribution by percent channel length"
                ),
                colour = "Scenario"
            )
        
        if (input$DefOrUserUpload_H == 'Default_Data_Portland' | input$DefOrUserUpload_H == 'Default_Data_Seattle') {
            p3 <- p3 +
                scale_color_manual(
                    values = c(
                        "SimFireEagle_202009_cl532" =
                            "#B22222",
                        "SimFireNorse_202009_cl532" = "#FF0000",
                        "HighSevS_202009" = "#DC143C",
                        "ModSevS_202009" =
                            "#DC143C",
                        "LowSevS_202009" =
                            "#FF6347",
                        "PrescFireS_202009" =
                            "#E9967A",
                        "CurCond_202009_cl532future" =
                            "#32CD32",
                        "CurCond_202009_cl532gridmet" =
                            "#00FF00",
                        "CurCond_202009_cl532" =
                            "#008000"
                    )
                )
            # Tahoe
            # scale_color_manual(
            #     values = c(
            #         "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2" = "#FF0000",
            #         "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli" =
            #             "#B22222",
            #         "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
            #         "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
            #         "ModSevS.2020.ki5krcs.chn_12" =
            #             "#DC143C",
            #         "LowSevS.2020.ki5krcs.chn_12" =
            #             "#FF6347",
            #         "PrescFireS.2020.ki5krcs.chn_12" =
            #             "#E9967A",
            #         "Thinn85.2020.ki5krcs.chn_12" =
            #             "#7CFC00",
            #         "Thinn93.2020.kikrcs.chn_12" =
            #             "#32CD32",
            #         "Thinn96.2020.kikrcs.chn_12" =
            #             "#00FF00",
            #         "CurCond.2020.ki5krcs.chn_cs12" =
            #             "#008000"
            #     )
            # )
        }else
            if (input$DefOrUserUpload_H == 'Default_Data_LT') {
                p3 <- p3 +
                    scale_color_manual(
                        values = c(
                            "SimFire_landisFuels_fut_cli_A2" = "#FF0000",
                            "SimFire_landisFuels_obs_cli" = "#B22222",
                            "SimFire_fccsFuels_obs_cli" = "#4d2525",
                            "HighSev" = "#DC143C",
                            "ModSev" = "#DC143C",
                            "LowSev" = "#FF6347",
                            "PrescFire" = "#E9967A",
                            "Thinn85" = "#7CFC00",
                            "Thinn93" = "#32CD32",
                            "Thinn96" = "#00FF00",
                            "CurCond" = "#008000"
                        )
                    )
            }else
                if (input$DefOrUserUpload_H == 'Upload Data') {
                    p3 <- p3 +
                        scale_color_brewer(palette = "virdis")
                }
        
        
        
        
        p3
        
    })
    #
    #
    # ############## plots of cumulative absolute values of variable   ##############
    # ############## vs cumulative percent of total hillslope area/ channel length   ##############
    #
    output$Plot_vs_cumPercArea_abs <- renderPlotly({
        req(input$Hill_variable)
        req(hill_arr_by_var_HA_abs())
        
        p2 <-
            hill_arr_by_var_HA_abs()  %>% ggplot(aes(x = cumPercArea))
        if (input$Hill_variable == "Runoff..mm.") {
            p2 <-
                p2 + geom_line(aes(y = cumRunoff.mm  , color = Scenario), size = 0.5)
        } else
            if (input$Hill_variable == "Lateral.Flow..mm.") {
                p2 <-
                    p2 + geom_line(aes(y = cumLateralflow.mm, color = Scenario), size = 0.5)
            } else
                if (input$Hill_variable == "Baseflow..mm.") {
                    p2 <-
                        p2 + geom_line(aes(y = cumBaseflow.mm, color = Scenario), size = 0.5)
                } else
                    if (input$Hill_variable == "Soil_Loss_kg") {
                        p2 <-
                            p2 + geom_line(aes(y = cumSoilLoss.kg.ha, color = Scenario), size = 0.5)
                    } else
                        if (input$Hill_variable == "Sediment_Deposition_kg") {
                            p2 <-
                                p2 + geom_line(aes(y = cumSedDep.kg.ha, color = Scenario), size = 0.5)
                        } else
                            if (input$Hill_variable == "Sediment_Yield_kg") {
                                p2 <-
                                    p2 + geom_line(aes(y = cumSedYield.kg.ha, color = Scenario), size = 0.5)
                            } else
                                if (input$Hill_variable == "Soluble_Reactive_P_kg") {
                                    p2 <-
                                        p2 + geom_line(aes(y = cumSRP.kg.ha.3, color = Scenario), size = 0.5)
                                } else
                                    if (input$Hill_variable == "Particulate_P_kg") {
                                        p2 <-
                                            p2 + geom_line(aes(y = cumParticulateP.kg.ha.3, color = Scenario),
                                                           size = 0.5)
                                    } else
                                        if (input$Hill_variable == "Total_P_kg") {
                                            p2 <-
                                                p2 + geom_line(aes(y = cumTotalP.kg.ha.3, color = Scenario), size = 0.5)
                                        } else
                                            if (input$Hill_variable == "Particle_Class_1_Fraction_kg") {
                                                p2 <-
                                                    p2 + geom_line(aes(y = cumParticle.Class.1.Fraction, color = Scenario),
                                                                   size = 0.5)
                                            } else
                                                if (input$Hill_variable == "Particle_Class_2_Fraction_kg") {
                                                    p2 <-
                                                        p2 + geom_line(aes(y = cumParticle.Class.2.Fraction, color = Scenario),
                                                                       size = 0.5)
                                                } else
                                                    if (input$Hill_variable == "Particle_Class_3_Fraction_kg") {
                                                        p2 <-
                                                            p2 + geom_line(aes(y = cumParticle.Class.3.Fraction, color = Scenario),
                                                                           size = 0.5)
                                                    } else
                                                        if (input$Hill_variable == "Particle_Class_4_Fraction_kg") {
                                                            p2 <-
                                                                p2 + geom_line(aes(y = cumParticle.Class.4.Fraction, color = Scenario),
                                                                               size = 0.5)
                                                        } else
                                                            if (input$Hill_variable == "Particle_Class_5_Fraction_kg") {
                                                                p2 <-
                                                                    p2 + geom_line(aes(y = cumParticle.Class.5.Fraction, color = Scenario),
                                                                                   size = 0.5)
                                                            } else
                                                                if (input$Hill_variable == "Particle_Fraction_Under_0.016_mm_kg") {
                                                                    p2 <-
                                                                        p2 + geom_line(aes(y = cumParticle.Fraction.Under.0.016.mm, color = Scenario),
                                                                                       size = 0.5)
                                                                } else
                                                                    if (input$Hill_variable == "Sediment_Yield_of_Particles_Under_0.016_mm_kg") {
                                                                        p2 <-
                                                                            p2 + geom_line(
                                                                                aes(y = cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. , color = Scenario),
                                                                                size = 0.5
                                                                            )
                                                                    }
        
        
        p2 <- p2 +  theme_bw() +
            theme(
                axis.title = element_text(
                    size = 10,
                    color = "Black",
                    face = "bold"
                ),
                axis.text = element_text(
                    size = 10,
                    color = "BLACK",
                    face = "bold"
                ),
                legend.title = element_text(
                    size = 10,
                    color = "BLACK",
                    face = "bold"
                ),
                legend.text = element_text(size = 10, color = "BLACK"),
                legend.position = "none",
                plot.margin=unit(c(2,0.5,1,0.5),"cm"),
                plot.title = element_text(
                    size = 10,
                    color = "#000000",
                    face = "bold",
                    family="Bauhaus 93",
                    vjust = 1,
                    hjust = 0.5
                    
                )
            ) +
            # scale_color_brewer(palette="RdYlGn") +
            labs(
                x = "Percent of total hillslope area",
                y = "Cumulative selected variable",
                # y = paste("Cumulative", input$Hill_variable, sep = " "),
                title= paste(
                    "Cumulative total",
                    input$Hill_variable ,
                    "\ncontribution by percent hillslope area"
                ),
                colour = "Scenario"
            )
        if (input$DefOrUserUpload_H == 'Default_Data_Portland' | input$DefOrUserUpload_H == 'Default_Data_Seattle') {
            p2 <- p2 +
                scale_color_manual(
                    values = c(
                        "SimFireEagle_202009_cl532" =
                            "#B22222",
                        "SimFireNorse_202009_cl532" = "#FF0000",
                        "HighSevS_202009" = "#DC143C",
                        "ModSevS_202009" =
                            "#DC143C",
                        "LowSevS_202009" =
                            "#FF6347",
                        "PrescFireS_202009" =
                            "#E9967A",
                        "CurCond_202009_cl532future" =
                            "#32CD32",
                        "CurCond_202009_cl532gridmet" =
                            "#00FF00",
                        "CurCond_202009_cl532" =
                            "#008000"
                    )
                )
            # Tahoe
            # scale_color_manual(
            #     values = c(
            #         "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2" = "#FF0000",
            #         "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli" =
            #             "#B22222",
            #         "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
            #         "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
            #         "ModSevS.2020.ki5krcs.chn_12" =
            #             "#DC143C",
            #         "LowSevS.2020.ki5krcs.chn_12" =
            #             "#FF6347",
            #         "PrescFireS.2020.ki5krcs.chn_12" =
            #             "#E9967A",
            #         "Thinn85.2020.ki5krcs.chn_12" =
            #             "#7CFC00",
            #         "Thinn93.2020.kikrcs.chn_12" =
            #             "#32CD32",
            #         "Thinn96.2020.kikrcs.chn_12" =
            #             "#00FF00",
            #         "CurCond.2020.ki5krcs.chn_cs12" =
            #             "#008000"
            #     )
            # )
        }else
            if (input$DefOrUserUpload_H == 'Default_Data_LT') {
                p2 <- p2 +
                    scale_color_manual(
                        values = c(
                            "SimFire_landisFuels_fut_cli_A2" = "#FF0000",
                            "SimFire_landisFuels_obs_cli" = "#B22222",
                            "SimFire_fccsFuels_obs_cli" = "#4d2525",
                            "HighSev" = "#DC143C",
                            "ModSev" = "#DC143C",
                            "LowSev" = "#FF6347",
                            "PrescFire" = "#E9967A",
                            "Thinn85" = "#7CFC00",
                            "Thinn93" = "#32CD32",
                            "Thinn96" = "#00FF00",
                            "CurCond" = "#008000"
                        )
                    )
            }else
                if (input$DefOrUserUpload_H == 'Upload Data') {
                    p2 <- p2 +
                        scale_color_brewer(palette = "virdis")
                }
        
        
        
        
        p2
        
    })
    #
    #
    #
    output$Plot_vs_cumPercLen_abs <- renderPlotly({
        req(input$Hill_variable)
        req(hill_arr_by_var_CL_abs())
        
        p4 <-
            hill_arr_by_var_CL_abs()  %>% ggplot(aes(x = cumPercLen))
        if (input$Hill_variable == "Runoff..mm.") {
            p4 <-
                p4 + geom_line(aes(y = cumRunoff.mm  , color = Scenario), size = 0.5)
        } else
            if (input$Hill_variable == "Lateral.Flow..mm.") {
                p4 <-
                    p4 + geom_line(aes(y = cumLateralflow.mm, color = Scenario), size = 0.5)
            } else
                if (input$Hill_variable == "Baseflow..mm.") {
                    p4 <-
                        p4 + geom_line(aes(y = cumBaseflow.mm, color = Scenario), size = 0.5)
                } else
                    if (input$Hill_variable == "Soil_Loss_kg") {
                        p4 <-
                            p4 + geom_line(aes(y = cumSoilLoss.kg.ha, color = Scenario), size = 0.5)
                    } else
                        if (input$Hill_variable == "Sediment_Deposition_kg") {
                            p4 <-
                                p4 + geom_line(aes(y = cumSedDep.kg.ha, color = Scenario), size = 0.5)
                        } else
                            if (input$Hill_variable == "Sediment_Yield_kg") {
                                p4 <-
                                    p4 + geom_line(aes(y = cumSedYield.kg.ha, color = Scenario), size = 0.5)
                            } else
                                if (input$Hill_variable == "Soluble_Reactive_P_kg") {
                                    p4 <-
                                        p4 + geom_line(aes(y = cumSRP.kg.ha.3, color = Scenario), size = 0.5)
                                } else
                                    if (input$Hill_variable == "Particulate_P_kg") {
                                        p4 <-
                                            p4 + geom_line(aes(y = cumParticulateP.kg.ha.3, color = Scenario),
                                                           size = 0.5)
                                    } else
                                        if (input$Hill_variable == "Total_P_kg") {
                                            p4 <-
                                                p4 + geom_line(aes(y = cumTotalP.kg.ha.3, color = Scenario), size = 0.5)
                                        } else
                                            if (input$Hill_variable == "Particle_Class_1_Fraction_kg") {
                                                p4 <-
                                                    p4 + geom_line(aes(y = cumParticle.Class.1.Fraction, color = Scenario),
                                                                   size = 0.5)
                                            } else
                                                if (input$Hill_variable == "Particle_Class_2_Fraction_kg") {
                                                    p4 <-
                                                        p4 + geom_line(aes(y = cumParticle.Class.2.Fraction, color = Scenario),
                                                                       size = 0.5)
                                                } else
                                                    if (input$Hill_variable == "Particle_Class_3_Fraction_kg") {
                                                        p4 <-
                                                            p4 + geom_line(aes(y = cumParticle.Class.3.Fraction, color = Scenario),
                                                                           size = 0.5)
                                                    } else
                                                        if (input$Hill_variable == "Particle_Class_4_Fraction_kg") {
                                                            p4 <-
                                                                p4 + geom_line(aes(y = cumParticle.Class.4.Fraction, color = Scenario),
                                                                               size = 0.5)
                                                        } else
                                                            if (input$Hill_variable == "Particle_Class_5_Fraction_kg") {
                                                                p4 <-
                                                                    p4 + geom_line(aes(y = cumParticle.Class.5.Fraction, color = Scenario),
                                                                                   size = 0.5)
                                                            } else
                                                                if (input$Hill_variable == "Particle_Fraction_Under_0.016_mm_kg") {
                                                                    p4 <-
                                                                        p4 + geom_line(aes(y = cumParticle.Fraction.Under.0.016.mm, color = Scenario),
                                                                                       size = 0.5)
                                                                } else
                                                                    if (input$Hill_variable == "Sediment_Yield_of_Particles_Under_0.016_mm_kg") {
                                                                        p4 <-
                                                                            p4 + geom_line(
                                                                                aes(y = cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. , color = Scenario),
                                                                                size = 0.5
                                                                            )
                                                                    }
        
        
        p4 <- p4 +  theme_bw() +
            theme(
                axis.title = element_text(
                    size = 10,
                    color = "Black",
                    face = "bold"
                ),
                axis.text = element_text(
                    size = 10,
                    color = "BLACK",
                    face = "bold"
                ),
                legend.title = element_text(
                    size = 10,
                    color = "BLACK",
                    face = "bold"
                ),
                legend.text = element_text(size = 10, color = "BLACK"),
                legend.position = "none",
                plot.margin=unit(c(2,0.5,1,0.5),"cm"),
                plot.title = element_text(
                    size = 10,
                    color = "#000000",
                    face = "bold",
                    family="Bauhaus 93",
                    vjust = 1,
                    hjust = 0.5
                    
                )
            ) +
            labs(
                x = "Percent of total channel length",
                y = "Cumulative selected variable",
                # y = paste("Cumulative", input$Hill_variable, sep = " "),
                title= paste(
                    "Cumulative total",
                    input$Hill_variable ,
                    "\ncontribution by percent channel length"
                ),
                colour = "Scenario"
            )
        if (input$DefOrUserUpload_H == 'Default_Data_Portland' | input$DefOrUserUpload_H == 'Default_Data_Seattle') {
            p4 <- p4 +
                scale_color_manual(
                    values = c(
                        "SimFireEagle_202009_cl532" =
                            "#B22222",
                        "SimFireNorse_202009_cl532" = "#FF0000",
                        "HighSevS_202009" = "#DC143C",
                        "ModSevS_202009" =
                            "#DC143C",
                        "LowSevS_202009" =
                            "#FF6347",
                        "PrescFireS_202009" =
                            "#E9967A",
                        "CurCond_202009_cl532future" =
                            "#32CD32",
                        "CurCond_202009_cl532gridmet" =
                            "#00FF00",
                        "CurCond_202009_cl532" =
                            "#008000"
                    )
                )
            # Tahoe
            # scale_color_manual(
            #     values = c(
            #         "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2" = "#FF0000",
            #         "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli" =
            #             "#B22222",
            #         "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
            #         "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
            #         "ModSevS.2020.ki5krcs.chn_12" =
            #             "#DC143C",
            #         "LowSevS.2020.ki5krcs.chn_12" =
            #             "#FF6347",
            #         "PrescFireS.2020.ki5krcs.chn_12" =
            #             "#E9967A",
            #         "Thinn85.2020.ki5krcs.chn_12" =
            #             "#7CFC00",
            #         "Thinn93.2020.kikrcs.chn_12" =
            #             "#32CD32",
            #         "Thinn96.2020.kikrcs.chn_12" =
            #             "#00FF00",
            #         "CurCond.2020.ki5krcs.chn_cs12" =
            #             "#008000"
            #     )
            # )
        }else
            if (input$DefOrUserUpload_H == 'Default_Data_LT') {
                p4 <- p4 +
                    scale_color_manual(
                        values = c(
                            "SimFire_landisFuels_fut_cli_A2" = "#FF0000",
                            "SimFire_landisFuels_obs_cli" = "#B22222",
                            "SimFire_fccsFuels_obs_cli" = "#4d2525",
                            "HighSev" = "#DC143C",
                            "ModSev" = "#DC143C",
                            "LowSev" = "#FF6347",
                            "PrescFire" = "#E9967A",
                            "Thinn85" = "#7CFC00",
                            "Thinn93" = "#32CD32",
                            "Thinn96" = "#00FF00",
                            "CurCond" = "#008000"
                        )
                    )
            } else
                if (input$DefOrUserUpload_H == 'Upload Data') {
                    p4 <- p4 +
                        scale_color_brewer(palette = "virdis")
                }
        
        
        
        p4
        
    })
    
    
    
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------Plots:Watershed-------------------------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    
    output$Plot9 <- renderPlotly({
        req(input$wshed_var)
        
        
        Wshed_subset <- Wshed_subset()
        if (input$AreaVsScen == 'allscen') {
            if (input$ScenVvar == "Heatmap") {
                d <-
                    Wshed_subset() %>% dplyr::select(Scenario, input$wshed_var) %>% dplyr::mutate_if(is.numeric, scale)
                d.m <- reshape2::melt(d)
                
                
                # # TEST To SEE if the dataframe from the reactive func is accessible
                # output$tab1 <- renderTable(
                #     d.m %>% head(100) )
                
                p <- plot_ly(x=d.m$Scenario, y=d.m$variable, z = d.m$value, type = "heatmap") %>%
                    layout(
                        xaxis=list(tickfont = list(size = 12), tickangle = 45),
                        margin = list(l = 150, r = 20, b = 150, t = 100, pad = 4),
                        title = list(text = paste0('<b>Relative impacts of management and disturbance on the watershed</b>',
                                                                                         '<br>',
                                                                                         '<sup>',
                                                                                         '<i>Plot displays the relative impact of the all disturbances on the delivered\nwater quantity/quality metric at the watershed outlet.</i>',
                                                                                         '<br><br>',
                                                                                         '</sup>')))
                    # layout(margin = list(l=120))
                p
                
                # a <-
                #     ggplot(d.m, aes(Scenario, variable,  fill = value)) +
                #     geom_tile(inherit.aes = TRUE)  +
                #     scale_fill_distiller(palette =  "Spectral", direction = -1) +
                #     theme(
                #         axis.text.x = element_text(angle = 90, colour = "Black"),
                #         axis.text.y = element_text(colour = "Black"),
                #         axis.title = element_blank(),
                #         legend.position='right')
                # 
                # ggplotly(a) %>%
                #     layout(title = list(text = paste0('<b>Relative impacts of management and disturbance on the watershed</b>',
                #                                       '<br>',
                #                                       '<sup>',
                #                                       '<i>Plot displays the relative impact of the all disturbances on the delivered\nwater quantity/quality metric at the watershed outlet.</i>',
                #                                       '<br><br>',
                #                                       '</sup>')),
                #            margin = list(l=10, r=20, b=5, t=150, pad=0))
                
            } else
                if (input$ScenVvar == "Bar Chart") {
                    d <-  Wshed_subset() %>% dplyr::select(Scenario, input$wshed_var)
                    
                    d.m <- reshape2::melt(d)
                    
                    ## Calculates percent contribution of each variable across all
                    ## the simulated scenarios
                    d.m <- d.m %>%
                        group_by(variable) %>%
                        mutate(total = sum(value),
                               share = (value / total) * 100) %>% dplyr::select(-total) %>%
                        ungroup()
                    
                    # # TEST To SEE if the dataframe from the reactive func is accessible
                    # output$tab1 <- renderTable(
                    #     d.m %>% head(100) )
                        
                    
                    # ggplot(d.m, aes(x=reorder(variable, -value), y=share, fill=Scenario)) + 
                    #     geom_bar(stat="identity", position = position_dodge(0.8))
                    # 
                    
                    
                    b <- ggplot(d.m) +

                        geom_bar(
                            aes(
                                y = share,
                                x = variable,
                                fill = reorder(Scenario, share)
                                # fill = reorder(Scenario,-share)
                            ),
                            stat = "identity",
                            position = "dodge"
                        ) +
                        theme_bw(base_rect_size = 0.1)+
                        theme(
                            axis.text.x = element_text(
                                angle = 45,
                                vjust = ,
                                colour = "Black"
                            ),
                            axis.text.y = element_text(colour = "Black"),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            legend.title = element_blank(),
                            legend.position ="none"
                        ) + coord_flip() +
                        scale_fill_brewer(palette = "RdYlGn") +
                        scale_y_continuous(labels = function(x) paste0(x*1, "%"))

                       ggplotly(b) %>%
                        layout(title = list(text = paste0('<b>Relative impacts of management and disturbance on the watershed</b>',
                                                          '<br>',
                                                          '<sup>',
                                                          '<i>Plot displays the relative impact of the all disturbances on the delivered\nwater quantity/quality metric at the watershed outlet.</i>',
                                                          '<br><br>',
                                                          '</sup>')),
                               margin = list(l=10, r=20, b=5, t=150, pad=0))
                    
                    
                }
        } else
            if (input$AreaVsScen == 'allwat') {
                if (input$ScenVvar == "Heatmap") {
                    d <-
                        Wshed_subset() %>% dplyr::select(Watershed, input$wshed_var) %>% dplyr::mutate_if(is.numeric, scale)
                    d.m <- reshape2::melt(d)
                    
                    
                    # # TEST To SEE if the dataframe from the reactive func is accessible
                    # output$tab1 <- renderTable(
                    #     d.m %>% head(100) )
                         
                    p <- plot_ly(x=d.m$Watershed, y=d.m$variable, z = d.m$value, type = "heatmap") %>%
                        layout(
                            xaxis=list(tickfont = list(size = 12), tickangle = 45),
                            margin = list(l = 150, r = 20, b = 150, t = 100, pad = 4),
                            title = list(text = paste0('<b>Relative impacts of the disturbance scenario across all watersheds </b>',
                                                       '<br>',
                                                       '<sup>',
                                                       '<i>Plot displays the relative impact of the disturbance on the delivered particular\nwater quantity/quality metric at each of the watersheds</i>',
                                                       '<br><br>',
                                                       '</sup>')))
                        # layout(margin = list(l=120))
                    p
                    
                    # a <-
                    #     ggplot(d.m, aes(Watershed, variable,  fill = value)) +
                    #     geom_tile(inherit.aes = TRUE)  +
                    #     scale_fill_distiller(palette =  "Spectral", direction = -1) +
                    #     theme(
                    #         axis.text.x = element_text(angle = 90, colour = "Black"),
                    #         axis.text.y = element_text(colour = "Black"),
                    #         axis.title = element_blank(),
                    #         legend.position='right'
                    #         
                    #     )
                    # ggplotly(a) %>%
                    #     layout(title = list(text = paste0('<b>Relative impacts of the disturbance scenario across all watersheds </b>',
                    #                                       '<br>',
                    #                                       '<sup>',
                    #                                       '<i>Plot displays the relative impact of the disturbance on the delivered particular\nwater quantity/quality metric at each of the watersheds</i>',
                    #                                       '</sup>')),
                    #            margin = list(l=10, r=20, b=5, t=150, pad=0))
                    
                } else
                    if (input$ScenVvar == "Bar Chart") {
                        d <-  Wshed_subset() %>% dplyr::select(Watershed, input$wshed_var) 
                        
                        d.m <- reshape2::melt(d)
                        
                        ## Calculates percent contribution of each variable across all
                        ## the simulated scenarios
                        d.m <- d.m %>%
                            group_by(variable) %>%
                            mutate(total = sum(value),
                                   share = (value / total) * 100) %>%
                            ungroup()
                        
                        # # TEST To SEE if the dataframe from the reactive func is accessible
                        # output$tab1 <- renderTable(
                        #     d.m %>% head(100) )
                        
                        
                        b <- ggplot(d.m) +
                            
                            geom_bar(
                                aes(
                                    y = share,
                                    x = variable,
                                    fill = reorder(Watershed,-share)
                                ),
                                stat = "identity",
                                position = "dodge"
                            )  +
                            theme_bw(base_rect_size = 0.1)+
                            theme(
                                axis.text.x = element_text(
                                    angle = 45,
                                    vjust = ,
                                    colour = "Black"
                                ),
                                axis.text.y = element_text(colour = "Black"),
                                axis.title.x = element_blank(),
                                axis.title.y = element_blank(),
                                legend.title = element_blank()
                            ) + labs(y = "Percent of total across all Watersheds") + 
                            coord_flip() +
                            scale_fill_brewer(palette = "RdYlGn") + theme(legend.position ="none")
                        ggplotly(b)  %>%
                            layout(title = list(text = paste0('<b>Relative impacts of the disturbance scenario across all watersheds </b>',
                                                              '<br>',
                                                              '<sup>',
                                                              '<i>Plot displays the relative impact of the disturbance on the delivered particular\nwater quantity/quality metric at each of the watersheds</i>',
                                                              '</sup>')),
                                   margin = list(l=10, r=20, b=5, t=150, pad=0))
                        
                        
                    }
            }
    })
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------Plots:Spatial DF-------------------------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    
    output$Plot12 <- leaflet::renderLeaflet({
        req(Spatial_data())
        req(Spatial_subset_comp())
        req(input$S_scen_comp)
        req(input$S_variable)
        req(Spatial_subset_base())
        req(input$S_scen_base)
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
    })
    
    
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------Data Summary Tables logic-------------------------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    
    ##### DF for summary datatables on the hillslopes tab
    
    
    
    # sed_stats_df <-reactive({
    #     if (input$summary_DT_by_var_H == "Landuse") {
    #         hill_subset_rel() %>% dplyr::filter(Scenario %in% input$Hill_scen) %>%
    #             dplyr::arrange_at(.vars = input$Hill_variable, desc) %>%
    #             dplyr::mutate(cumPercArea = cumsum(Hillslope.Area..ha.) /
    #                       sum(Hillslope.Area..ha.) * 100) %>%
    #             dplyr::filter(cumPercArea < 50) %>%
    #             dplyr::select(LanduseDesc, Slope, Sediment.Yield..kg.ha. , RelSedYield.kg.ha) %>%
    #             group_by(LanduseDesc) %>% dplyr::summarise_if(is.numeric, list(mean =
    #                                                                        mean)) %>%
    #             dplyr::arrange(desc(Sediment.Yield..kg.ha._mean)) %>% dplyr::mutate_if(is.numeric, round, 2)
    #     }else
    #         if (input$summary_DT_by_var_H == "Soiltype") {}
    #     else
    #         if (input$summary_DT_by_var_H == "Both") {}
    #     })
    
    
    
    
    sed_stats_df <- reactive({
        if (input$summary_DT_by_var_H == "Landuse") {
            hill_subset_rel() %>% dplyr::filter(Scenario %in% input$Hill_scen_comp) %>%
                dplyr::arrange_at(.vars = input$Hill_variable, desc) %>%
                dplyr::mutate(cumPercArea = cumsum(Hillslope.Area..ha.) /
                                  sum(Hillslope.Area..ha.) * 100) %>%
                dplyr::filter(cumPercArea < input$thresh_H) %>%
                dplyr::select(LanduseDesc, Slope, input$Hill_variable , paste0("AbsChange_", input$Hill_variable)) %>%
                group_by(LanduseDesc) %>% dplyr::summarise_if(is.numeric, list(mean =
                                                                                   mean)) %>%
                dplyr::arrange(desc(paste0(input$Hill_variable, "_mean"))) %>% 
                dplyr::mutate_if(is.numeric, round, 2)
        } else
            if (input$summary_DT_by_var_H == "Soiltype") {
                hill_subset_rel() %>% dplyr::filter(Scenario %in% input$Hill_scen_comp) %>%
                    dplyr::arrange_at(.vars = input$Hill_variable, desc) %>%
                    dplyr::mutate(cumPercArea = cumsum(Hillslope.Area..ha.) /
                                      sum(Hillslope.Area..ha.) * 100) %>%
                    dplyr::filter(cumPercArea < input$thresh_H) %>%
                    dplyr::select(SoilDesc, Slope, input$Hill_variable , paste0("AbsChange_", input$Hill_variable)) %>%
                    group_by(SoilDesc) %>% dplyr::summarise_if(is.numeric, list(mean =mean)) %>%
                    dplyr::arrange(desc(paste0(input$Hill_variable, "_mean"))) %>%
                    dplyr::mutate_if(is.numeric, round, 2)
                
            } else
                if (input$summary_DT_by_var_H == "Both") {
                    hill_subset_rel() %>% dplyr::filter(Scenario %in% input$Hill_scen_comp) %>%
                        dplyr::arrange_at(.vars = input$Hill_variable, desc) %>%
                        dplyr::mutate(cumPercArea = cumsum(Hillslope.Area..ha.) /
                                          sum(Hillslope.Area..ha.) * 100) %>%
                        dplyr::filter(cumPercArea < input$thresh_H) %>%
                        dplyr::select(SoilDesc,
                                      LanduseDesc,
                                      Slope,
                                      input$Hill_variable , 
                                      paste0("AbsChange_", input$Hill_variable)
                        ) %>%
                        group_by(SoilDesc, LanduseDesc) %>% dplyr::summarise_if(is.numeric, list(mean =mean)) %>%
                        dplyr::arrange(desc(paste0(input$Hill_variable, "_mean"))) %>% 
                        dplyr::mutate_if(is.numeric, round, 2)
                    
                }
    })
    
    output$Sed_stats_by_category <- DT::renderDataTable(
        sed_stats_df(),
        extensions = list("Buttons" = NULL, 'Scroller'= NULL),
        options = list(
            deferRender = TRUE,
            autoWidth = TRUE,
            scrollY = 350,
            scrollX = 200,
            scroller = TRUE,
            dom = 'BRSfrti',
            buttons =
                list('copy', 'print', list(
                    extend = 'collection',
                    buttons = c('csv', 'excel', 'pdf'),
                    text = 'Download'
                )),
            # pageLength = 5,
            fixedHeader = TRUE,
            fillContainer = F,
            class = "display",
            columnDefs = list(list(className = 'dt-left'))
        ),
        rownames= FALSE
    )
    
    
    
    # output$Sed_stats_by_category <- DT::renderDataTable(
    #     sed_stats_df(),
    #     extensions = list("Buttons" = NULL),
    #     options = list(
    #         dom = 'Bfrtip',
    #         buttons =
    #             list('copy', 'print', list(
    #                 extend = 'collection',
    #                 buttons = c('csv', 'excel', 'pdf'),
    #                 text = 'Download'
    #             )),
    #         # scroller = TRUE,
    #         scrollX = TRUE,
    #         scrollY = TRUE,
    #         pageLength = 5,
    #         fixedHeader = TRUE,
    #         fillContainer = F,
    #         class = "display",
    #         columnDefs = list(list(className = 'dt-left'))
    #     )
    # )
    
    
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## Summary table on spatial tab corressponding to the leaflet map
    ## -----------------------------------------------------------------------------------------------------------##
    
    
    spdftab <- reactive({
        req(Spatial_data())
        req(Spatial_subset_comp())
        Spatial_subset_comp() %>% as.data.frame() %>% 
            select(TopazID, landuse, soil,slope,
                   input$S_variable, paste0("AbsChange_",input$S_variable))%>% #
            dplyr::filter(slope < input$thresh_slope_S)
    })
    
    # output$Sed_stats_by_category <- DT::renderDataTable(
    #     sed_stats_df(),
    #     extensions = list("Buttons" = NULL, 'Scroller'= NULL),
    #     options = list(
    #         deferRender = TRUE,
    #         autoWidth = TRUE,
    #         scrollY = 350,
    #         scrollX = 200,
    #         scroller = TRUE,
    #         dom = 'BRSfrtip',
    #         buttons =
    #             list('copy', 'print', list(
    #                 extend = 'collection',
    #                 buttons = c('csv', 'excel', 'pdf'),
    #                 text = 'Download'
    #             )),
    #         # pageLength = 5,
    #         fixedHeader = TRUE,
    #         fillContainer = F,
    #         class = "display",
    #         columnDefs = list(list(className = 'dt-left'))
    #     ),
    #     rownames= FALSE
    # )
    
    output$spatial_table <- DT::renderDataTable(
        spdftab(),
        # caption = htmltools::tags$caption( style = 'caption-side: top; text-align: center; color:black; font-size:200% ;','Table1: Iris Dataset Table'),
        extensions = list("Buttons" = NULL, 'Scroller'= NULL),
        options = list(
            deferRender = TRUE,
            autoWidth = TRUE,
            scrollY = 300,
            scrollX = 200,
            scroller = TRUE,
            dom = 'BRSfrti',
            buttons =
                list('copy', 'print', list(
                    extend = 'collection',
                    buttons = c('csv', 'excel', 'pdf'),
                    text = 'Download'
                )),
            fixedHeader = TRUE,
            fillContainer = F,
            class = "display",
            columnDefs = list(list(className = 'dt-left'))
            
        ),
        rownames = F
    ) 
    
    observe_helpers()
    
}

# Run the application
shinyApp(ui = ui, server = server)