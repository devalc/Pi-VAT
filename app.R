## --------------------------------------------------------------------------------------##
##
## Script name: app.R
##
## Purpose of the script: 
##
## @author: Chinmay Deval
##
## Created on Fri Sept 04 19:35:48 2020
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
library(qs,quietly = TRUE,warn.conflicts = FALSE)
library(tools,quietly = TRUE)
library(sf,quietly = TRUE)
# library(echarts4r, quietly = TRUE)
library(tidyverse,quietly = TRUE,warn.conflicts = FALSE)
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
# library(crosstalk,quietly = TRUE)
library(sever,quietly = TRUE)
library(showtext, quietly = TRUE)
library(ragg, quietly = TRUE)
library(thematic, quietly = TRUE)
library(bslib, quietly = TRUE)


source("global.R")


## ----------------------------------Init Options---------------------------------------##
options(shiny.maxRequestSize = 200 * 1024 ^ 2)
thematic_shiny(font = "auto")



ui <- navbarPage(
    
    title = div("In-WPaT",
                div(
                    tags$a(
                        href = "https://forest.moscowfsl.wsu.edu/fswepp/",
                        tags$img(
                            src = 'FS.png',
                            style = "position:fixed;right: 105px;top: 5px;padding-bottom:10px;",
                            height = 50
                        )
                    ),
                    tags$a(
                        href = "https://www.uidaho.edu/",
                        tags$img(
                            src = 'UI.jpg',
                            style = "position:fixed;right: 150px;top: 5px;padding-bottom:10px;",
                            height = 50
                        )
                    ),
                    tags$a(
                        href = "https://nifa.usda.gov/",
                        tags$img(
                            src = 'nifa.jpg',
                            style = "position:fixed;right: 60px;top: 5px;padding-bottom:10px;",
                            height = 50
                        )
                    ),
                    tags$a(
                        href = "https://github.com/devalc/In-WPaT",
                        tags$img(
                            src = "GitHub-Mark.png",
                            style = "position:fixed;right: 10px;top: 5px;padding-bottom:10px;",
                            height = 50
                        )
                    )
                )),
    
    windowTitle = "In-WMPT",
    position = "fixed-top",
    # fluid = TRUE,
    collapsible = TRUE,
    id = 'tabs',
    
    
    
    
    ## ----------------------------------Set Theme------------------------------------------##
    ## set the theme
    
   theme = bslib::bs_theme(bootswatch = "minty",
     bg = "#202123", 
     fg = "#E4E4E4", 
     primary = "#EA80FC", 
     secondary = "#00DAC6",
     success = "#86d57b",
     info = "#28B3ED",
     warning = "#FD7424",
     danger = "#F7367E",
     base_font = font_google("Open Sans"),
     heading_font = font_google("Proza Libre"),
     code_font = font_google("Fira Code")
   ),
   
    
    ## ----------------------------------Start defining Tabs------------------------------------------##
    
    
    ## -----------------------------------------Landing Page---------------------------------------------##
    
    tabPanel(
        "Home",
        icon = icon("home"),
        
        # tags$head(includeHTML((
        #     "google-analytics.html"
        # ))),
        
        
        use_sever(),
        #h1("sever" )
        fluidPage(HTML("<br style = 'line-height:10;'><br>"),
                  HTML("<br>"),
                  
                  # fluidRow(
                  div(class = "banner",
                      tags$style(HTML("
                                      .banner{
                                      position: relative;
                                      background-image: url('banner-bk_g.jpg');
                                      background-repeat: no-repeat;
                                      background-size: cover;
                                      height: 300px;
                                      border-style: none!important;
                                      }"
                      )
                      ),
                      div(class = "bannerright",
                      # tags$h1("In-WPaT"),
                      tags$h2(("An Interactive Watershed Prioritization and Targeting tool for synthesis and decision support using outputs from spatially complex, geospatial water quality models.")),
                      tags$style(HTML("
                                      .bannerright{position: absolute;top: 5%;right: 40%;;left: 18%;
                                      background-color:#59ebeb; color:#fff;transition: margin 10s;}"))
                      ),
                      div(class = "bannerleft",
                          tags$img(src= "In-WPaT_hex.svg", height =300)
                          ),
                      
                      ),
                  HTML("<br/>"),
                  
                  fluidRow(align  = "center",
                  column(width=3,
                         align = "centre",
                         
                         thumbnail_label1(
                                        image = 'background.jpg',
                                        label = 'Watershed Analysis',
                                        content = "Inter-watershed comparison of impacts of management on annual water yield and
                                      water quality at the watershed outlet"
                                    ),
                         HTML("<br/>"),
                                    actionBttn("Wbutton", "Navigate to Watershed", icon = icon("line-chart"),style = "pill",
                                               color = "royal")
                  ),
                  column(width=3,
                         align = "centre",
                         
                         thumbnail_label1(
                             image = 'hillslope_img.jpg',
                             label = 'Hillslope Analysis',
                             content = "Identifying targeted pollutant hotspots within a watershed and quantifying the impacts of disturbance
                                      and management on the detachment and delivery of pollutants from these hotspots"
                         ),
                         HTML("<br/>"),
                         actionBttn("Hbutton", "Navigate to Hillslope", icon = icon("line-chart"),style = "pill",
                                    color = "royal")
                  ),
                  column(width=3,
                         align = "centre",
                         
                         thumbnail_label1(
                             image = 'spatial_imp.PNG',
                             label = 'Spatial Visualization',
                             content = "Identifying targeted pollutant hotspots within a watershed and quantifying the impacts of disturbance
                                      and management on the detachment and delivery of pollutants from these hotspots"
                         ),
                         HTML("<br/>"),
                         actionBttn("Sbutton", "Navigate to Spatial-Viz", icon = icon("line-chart"),style = "pill",
                                    color = "royal")
                  ),
                  column(width=3,
                         align = "centre",
                         
                         thumbnail_label1(
                             image = 'hru1.jpg',
                             label = 'SWAT-Viz',
                             content = "Synthesize and visualize subbasin,reach,and HRU scale outputs and targeted HRU scale hotspots across multiple watersheds for multiple treatments."
                         ),
                         HTML("<br/>"),
                         actionBttn("Swatbutton", "Navigate to SWAT-Viz", icon = icon("line-chart"),style = "pill",
                                    color = "royal")
                  )
                  ),
                  HTML("<br/><br/>"),
                  
                  div(class = "footer",
                          tags$style(HTML("
                                      .footer{
                                      position: relative;
                                      background-image: url('footerimg4.jpg');
                                      background-repeat: no-repeat;
                                      background-size: cover;
                                      height: 200px;
                                      border-style: none!important;
                                      margin:0%
                                      }"
                          )),
                    div(class = "footerright",
                              tags$p(
                                  "In-WPaT is currently designed to analyze outputs from the online interface for WEPP model (WEPPcloud) and SWAT model. It provides an option for users to upload their own runs.",
                                  align = "left"
                              ),
                              tags$p(
                                  a(href = 'https://wepp1.nkn.uidaho.edu/weppcloud/', 'WEPPcloud', .noWS = "outside"),
                                  ' is a cloud based simulation tool based on the process based Watershed Erosion Prediction Project',
                                  tags$a(href="https://www.fs.usda.gov/ccrc/tools/watershed-erosion-prediction-project",
                                         "(WEPP)"), 'model. It estimates
                                                         hillslope soil erosion, runoff, and sediment yields from anywhere in the continental U.S. It is especially useful for
                                                         post-wildfire assessments, fuel treatment planning, and prescribed fire analysis.',
                                  .noWS = c("after-begin", "before-end"),
                                  align = "left"
                              ),
                              tags$style(HTML("
                                      .footerright{position: absolute;top: 20%;right: 5%;;left: 40%;color:#fff
                                      }"))
                          ),
                    div(class = "footerleft",
                        
                        tags$h3("Powered by",
                               img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png",
                                   height= 80),
                               "and",
                               img(src = "https://www.r-project.org/logo/Rlogo.svg",
                                   height= 80),
                               tags$style(HTML("
                                      .footerleft{position: absolute;top: 20%;right: 60%;;left: 5%;color:#fff
                                      }"))
                                   
                               
                            )
                        )
                        
                        )
                          

                  
                  
        )
    ),
    
    
    
    ## -----------------------------------------Watershed Tab---------------------------------------------##
    tabPanel(
        "Watershed",
        
        # tags$head(includeHTML((
        #     "google-analytics.html"
        # ))),
        
        
        
        column(
            width = 12,
            style = 'padding-top:80px;',
            
            # Set up shinyalert
            useShinyalert(),
            
            sidebarLayout(
                position = "left",
                sidebarPanel(
                    width = 4,
                    
                    
                    
                    awesomeRadio(
                        inputId = "DefOrUserUpload_W",
                        label = "Data Import Options:",
                        choices = c(
                            # "Default Data (Portland)" = "Default_Data_Portland",
                            # "Default Data (Seattle)" = "Default_Data_Seattle",
                            "Default Data (Lake Tahoe)" = "Default_Data_LT",
                            "Default Data (Palouse)" = "Default_Data_Palouse",
                            "Upload your own data" = "Upload data"
                        ),
                        selected = "Default_Data_LT",
                        status = 'success'
                    ),
                    
                    
                    uiOutput("W_FileInput"),
                    
                    awesomeRadio(
                        inputId = "AreaVsScen",
                        label = "Management/watershed Options: ",
                        choices = c(
                            "One Watershed, Selected Scenarios" = "allscen",
                            "One Scenario, Selected Watersheds" =
                                "allwat"
                        ),
                        selected = "allscen",
                        status = 'success'
                    ),
                    
                    uiOutput("Wshed_wshed_S"),
                    
                    
                    uiOutput("Wshed_wshed"),
                    
                    
                    uiOutput("wshed_var"),
                    
                    radioGroupButtons(
                        inputId = "ScenVvar",
                        label = "Visualization type:",
                        choices = c("Heatmap" = "Heatmap", "Bar Chart" =
                                        "Bar Chart"),
                        selected = "Heatmap",
                        checkIcon = list(
                            yes = icon("ok",
                                       lib = "glyphicon")),
                        status = 'success'
                    ),
                    # awesomeRadio(
                    #     inputId = "ScenVvar",
                    #     label = "Visualization type:",
                    #     choices = c("Heatmap" = "Heatmap", "Bar Chart" =
                    #                     "Bar Chart"),
                    #     
                    #     selected = "Heatmap",
                    #     status = 'warning'
                    # ),
                    
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                    width = 8,
                    
                    plotlyOutput("Plot9", height = "700px", width =
                                     "1000px") %>% withSpinner(type = 8 )
                    
                )
            )
        )
    ),
    #
    #
    # ## -----------------------------------------Hillslope Tab---------------------------------------------##
    
    tabPanel(
        "Hillslope",
        
        # tags$head(includeHTML((
        #     "google-analytics.html"
        # ))),
        
        
        
        column(
            width = 12,
            style = 'padding-top:70px;',
            
            # Set up shinyalert
            useShinyalert(),
            
            sidebarLayout(
                position = "left",
                sidebarPanel(
                    width = 3,
                    
                    awesomeRadio(
                        inputId = "DefOrUserUpload_H",
                        label = "Data Import Options:",
                        choices = c(
                            # "Default Data (Portland)" = "Default_Data_Portland",
                            # "Default Data (Seattle)" = "Default_Data_Seattle",
                            "Default Data (Lake Tahoe)" = "Default_Data_LT",
                            "Default Data (Palouse)" = "Default_Data_Palouse",
                            "Upload your own data" =
                                "Upload data"
                        ),
                        selected = "Default_Data_LT",
                        status = 'success'
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
                    ) %>%
                        helper(
                            icon = "question-circle",
                            colour = "#FF0000",
                            content = "H_plot_thresh",
                            type = "markdown",
                            size = "l",
                            buttonLabel = "Okay",
                            easyClose = TRUE,
                            fade = TRUE
                        ),
                    
                    
                    # uiOutput("Hill_scen"),
                    radioGroupButtons(
                        inputId = "summary_DT_by_var_H",
                        label = "Summarize by:",
                        choices = c("Land Use" = "Landuse",
                                    "Soil Type" = "Soiltype",
                                    "Both" = "Both"),
                        selected = "Both",
                        checkIcon = list(
                            yes = icon("ok",
                                       lib = "glyphicon")),
                        status = 'success'
                    ),
                    
                    # awesomeRadio(
                    #     inputId = "summary_DT_by_var_H",
                    #     label = "Summarize by:",
                    #     choices = c(
                    #         "Land Use" = "Landuse",
                    #         "Soil Type" = "Soiltype",
                    #         "Both" = "Both"
                    #     ),
                    #     selected = "Both",
                    #     status = 'warning'
                    # ),
                    
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                    width = 9,
                    tabsetPanel(
                        tabPanel("Plot",icon =icon("chart-line"), style = 'padding:20px;',
                    fluidRow(
                        column(
                            6,
                            align = "center",
                            plotlyOutput("Plot_vs_cumPercArea") %>% withSpinner(type = 8 )
                        ),
                        column(
                            6,
                            align = "center",
                            plotlyOutput("Plot_vs_cumPercArea_abs") %>% withSpinner(type = 8 )
                        )
                    ),
                    # HTML("<br style = “line-height:5;”><br>"),
                    # uiOutput("Exp3_Exp4") %>% withSpinner(color =
                    #                                           "#0dc5c1"),
                    HTML("<br <br>"),
                    fluidRow(
                        column(
                            6,
                            align = "center",
                            plotlyOutput("Plot_vs_cumPercLen") %>% withSpinner(type = 8)
                        ),
                        column(
                            6,
                            align = "center",
                            plotlyOutput("Plot_vs_cumPercLen_abs") %>% withSpinner(type = 8 )
                        )
                    )),
                    tabPanel("Table",icon =icon("table"), style = 'padding:20px;',
                    # HTML("<br style = “line-height:5;”><br>"),
                    
                    
                    uiOutput("tab_H")%>% withSpinner(type = 8 )))
                )
            )
        )
    ),
    
    
    # ## -----------------------------------------Spatial-Viz Tab---------------------------------------------##
    
    tabPanel(
        "Spatial-Viz",
        
        # tags$head(includeHTML((
        #     "google-analytics.html"
        # ))),
        
        style = 'padding-top:70px;',
        
        
        
        useShinyalert(),
        # Set up shinyalert
        
        sidebarLayout(
            sidebarPanel(
                width = 3,
                
                
                awesomeRadio(
                    inputId = "DefOrUserUpload_S",
                    label = "Data Import Options:",
                    choices = c(
                        # "Default Data (Portland)" = "Default_Data_Portland",
                        # "Default Data (Seattle)" = "Default_Data_Seattle",
                        "Default Data (Lake Tahoe)" = "Default_Data_LT",
                        "Default Data (Palouse)" = "Default_Data_Palouse",
                        "Upload your own data" = "Upload data"
                    ),
                    selected = "Default_Data_LT",
                    status = 'success'
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
                ) %>%
                    helper(
                        icon = "question-circle",
                        colour = "#FF0000",
                        content = "S_plot_thresh",
                        type = "markdown",
                        size = "l",
                        buttonLabel = "Okay",
                        easyClose = TRUE,
                        fade = TRUE
                    ),
                
                
                
                sliderInput(
                    "thresh_slope_S",
                    "Slope Threshold:",
                    min = 0,
                    max = 1,
                    value = c(0, 1),
                    step = NULL,
                    round = TRUE,
                    ticks = TRUE,
                    animate = FALSE
                ) %>%
                    helper(
                        icon = "question-circle",
                        colour = "#FF0000",
                        content = "S_slope_thresh",
                        type = "markdown",
                        size = "l",
                        buttonLabel = "Okay",
                        easyClose = TRUE,
                        fade = TRUE
                    ),
            ),
            
            # Main panel for displaying outputs ----
            mainPanel(
                width = 9,
                # style = 'padding:80px;',
                tabsetPanel(
                    tabPanel("Plot",icon =icon("layer-group"), style = 'padding:20px;',
                fluidRow(column(
                    12,
                    # offset = 1,
                    leaflet::leafletOutput("Plot12",height = "600px") %>%
                        withSpinner(type = 8)
                ))),
                tabPanel("Table",icon =icon("table"),style = 'padding:20px;',
                # HTML("<br style = “line-height:5;”><br>"),
                # fluidRow(column(
                #     10,
                #     offset = 1,
                    uiOutput("tab_sp") %>%
                    withSpinner(type = 8)
                )),
                # HTML("<br style = “line-height:5;”><br>")
            )
            
            
        )
    ),
    
    # ## -----------------------------------------SWAT-viz Tab---------------------------------------------##
    
    tabPanel(
        "SWAT-Viz",
        
        # tags$head(includeHTML((
        #     "google-analytics.html"
        # ))),
        
        
        
        column(
            width = 12,
            style = 'padding-top:70px;',
            
            # Set up shinyalert
            useShinyalert(),
            
            sidebarLayout(
                position = "left",
                sidebarPanel(
                    width = 4,
                    
                    awesomeRadio(
                        inputId = "DefOrUserUpload_SWAT",
                        label = "Data Import Options:",
                        choices = c(
                            "Default Data (WE38)" = "Default_Data_WE38",
                            "Upload data" = "Upload data"
                        ),
                        selected = "Default_Data_WE38",
                        status = 'success'
                    )%>%
                      helper(
                        icon = "question-circle",
                        colour = "#FF0000",
                        content = "SWAT_upload",
                        type = "markdown",
                        size = "l",
                        buttonLabel = "Okay",
                        easyClose = TRUE,
                        fade = TRUE
                      ),
                    
                    
                    uiOutput("SWAT_FileInput"),
                    uiOutput("Which_SWAT_out"),
                    uiOutput("SWAT_wshed_mgmt_optns"),
                    uiOutput("SWAT_wshed"),
                    uiOutput("SWAT_scen"),
                    uiOutput("SWAT_scen_comp"),
                    uiOutput("SWAT_variable")%>%
                      helper(
                        icon = "question-circle",
                        colour = "#FF0000",
                        content = "SWAT_variable_exp",
                        type = "markdown",
                        size = "l",
                        buttonLabel = "Okay",
                        easyClose = TRUE,
                        fade = TRUE
                      ),
                    uiOutput("SWAT_reachno"),
                    uiOutput("SWAT_subnum"),
                    uiOutput("swat_hru_subno"),
                    uiOutput("swat_hru_lulc"),
                    uiOutput("hru_slp_slider")
                    
                    # 
                    
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                    width = 8,
                    
                uiOutput("swat_tabpanels")
                    
                    
                )
            )
        )
    )
    
    
)


## ----------------------------------define server logic------------------------------------------##
server <- function(input, output, session) {
    
    ## BEtter disconnected screen
    sever()
    
    observe({
        if (input$DefOrUserUpload_W == 'Upload data') {
            shinyalert(
                "",
                "Please provide the URL pointing to the WEPPCloud watershed file in the Input box",
                type = "success"
            )
        }
    })
    
    observe({
        if (input$DefOrUserUpload_H == 'Upload data') {
            shinyalert(
                "",
                "Please provide the URL pointing to the WEPPCloud hillslope file in the Input box",
                type = "success"
            )
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
    
    observeEvent(input$Swatbutton, {
        updateTabsetPanel(session = session,
                          inputId = "tabs",
                          selected = "SWAT-Viz")
    })
    
    
    
    ## ----------------------------------Watershed Server logic------------------------------------------## 
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
        } else
            if (input$DefOrUserUpload_W == 'Default_Data_Portland' |
                input$DefOrUserUpload_W == 'Default_Data_Seattle' |
                input$DefOrUserUpload_W == 'Default_Data_LT' |
                input$DefOrUserUpload_W == 'Default_Data_Palouse') {
                
            }
    })
    
    
    Wshed_data <- reactive({
        req(input$DefOrUserUpload_W)
        if (input$DefOrUserUpload_W == 'Default_Data_Portland') {
            file3 <- "data/portland202009_out_summary_cd.csv"
            read.table(file = file3,
                       header = TRUE,
                       sep = ",")
        } else
            if (input$DefOrUserUpload_W == 'Default_Data_Seattle') {
                file3 <- "data/seattle202009_out_summary_cd.csv"
                read.table(file = file3,
                           header = TRUE,
                           sep = ",")
            } else
                if (input$DefOrUserUpload_W == 'Default_Data_LT') {
                    file3 <- "data/lt_202010_out_summary_cd.csv"
                    read.table(file = file3,
                               header = TRUE,
                               sep = ",")
                } else
                    if (input$DefOrUserUpload_W == 'Default_Data_Palouse') {
                        file3 <- "data/Palouse202103_out_summary.csv"
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
                    options = list(
                        `actions-box` = TRUE,
                        `header` = "Select watershed ",
                        `windowPadding` = 1,
                        `width` = " css-width ",
                        `size` = 6
                    ),
                    choicesOpt = list(content = stringr::str_trunc(
                        unique(Wshed_data()$Watershed),
                        width = 35
                    ))
                )
            } else
                if (input$AreaVsScen == 'allwat') {
                    pickerInput(
                        "Wshed_wshed",
                        "Select the scenario of interest",
                        unique(Wshed_data()$Scenario),
                        options = list(
                            `actions-box` = TRUE,
                            `header` = "Select scenario ",
                            `windowPadding` = 1,
                            `width` = " css-width ",
                            `size` = 6
                        )
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
                        options = list(
                            `actions-box` = TRUE,
                            `header` = "Select watershed ",
                            `windowPadding` = 1,
                            `width` = " css-width ",
                            `size` = 6
                        )
                    )
                } else
                    if (input$AreaVsScen == 'allwat') {
                        pickerInput(
                            "Wshed_wshed",
                            "Select the scenario of interest",
                            unique(Wshed_data()$Scenario),
                            options = list(
                                `actions-box` = TRUE,
                                `header` = "Select scenario ",
                                `windowPadding` = 1,
                                `width` = " css-width ",
                                `size` = 6
                            )
                        )
                    }
                
            } else
                if (input$DefOrUserUpload_W == 'Default_Data_Seattle') {
                    if (input$AreaVsScen == 'allscen') {
                        pickerInput(
                            inputId = "Wshed_wshed",
                            label = "Select the watershed of interest",
                            choices =   unique(Wshed_data()$Watershed),
                            selected =   unique(Wshed_data()$Watershed)[11],
                            options = list(
                                `actions-box` = TRUE,
                                `header` = "Select watershed ",
                                `windowPadding` = 1,
                                `width` = " css-width ",
                                `size` = 6
                            )
                        )
                    } else
                        if (input$AreaVsScen == 'allwat') {
                            pickerInput(
                                "Wshed_wshed",
                                "Select the scenario of interest",
                                unique(Wshed_data()$Scenario),
                                options = list(
                                    `actions-box` = TRUE,
                                    `header` = "Select scenario ",
                                    `windowPadding` = 1,
                                    `width` = " css-width ",
                                    `size` = 6
                                )
                            )
                        }
                    
                } else
                    if (input$DefOrUserUpload_W == 'Default_Data_LT') {
                        if (input$AreaVsScen == 'allscen') {
                            pickerInput(
                                inputId = "Wshed_wshed",
                                label = "Select the watershed of interest",
                                choices =   unique(Wshed_data()$Watershed),
                                selected =   unique(Wshed_data()$Watershed)[11],
                                options = list(
                                    `actions-box` = TRUE,
                                    `header` = "Select watershed ",
                                    `windowPadding` = 1,
                                    `width` = " css-width ",
                                    `size` = 6
                                ),
                                choicesOpt = list(content = stringr::str_trunc(
                                    unique(Wshed_data()$Watershed),
                                    width = 35
                                ))
                            )
                        } else
                            if (input$AreaVsScen == 'allwat') {
                                pickerInput(
                                    "Wshed_wshed",
                                    "Select the scenario of interest",
                                    unique(Wshed_data()$Scenario),
                                    options = list(
                                        `actions-box` = TRUE,
                                        `header` = "Select scenario ",
                                        `windowPadding` = 1,
                                        `width` = " css-width ",
                                        `size` = 6
                                    )
                                )
                            }
                        
                    } else
                        if (input$DefOrUserUpload_W == 'Default_Data_Palouse') {
                            if (input$AreaVsScen == 'allscen') {
                                pickerInput(
                                    inputId = "Wshed_wshed",
                                    label = "Select the watershed of interest",
                                    choices =   unique(Wshed_data()$Watershed),
                                    selected =   unique(Wshed_data()$Watershed)[1],
                                    options = list(
                                        `actions-box` = TRUE,
                                        `header` = "Select watershed ",
                                        `windowPadding` = 1,
                                        `width` = " css-width ",
                                        `size` = 6
                                    )
                                )
                            } else
                                if (input$AreaVsScen == 'allwat') {
                                    pickerInput(
                                        "Wshed_wshed",
                                        "Select the scenario of interest",
                                        unique(Wshed_data()$Scenario),
                                        options = list(
                                            `actions-box` = TRUE,
                                            `header` = "Select scenario ",
                                            `windowPadding` = 1,
                                            `width` = " css-width ",
                                            `size` = 6
                                        )
                                    )
                                }
                            
                        }
        
    })
    
    
    output$Wshed_wshed_S <- renderUI({
        if (input$DefOrUserUpload_W == 'Upload data') {
            req(Wshed_data())
            if (input$AreaVsScen == 'allscen') {
                pickerInput(
                    "Wshed_wshed_S",
                    "Select the scenarios of interest",
                    unique(Wshed_data()$Scenario),
                    selected =   unique(Wshed_data()$Scenario)[1:10],
                    multiple = TRUE,
                    options = list(
                        `actions-box` = TRUE,
                        `header` = "Select scenarios ",
                        `windowPadding` = 1,
                        `width` = " css-width ",
                        `size` = 6
                    )
                )
            } else
                if (input$AreaVsScen == 'allwat') {
                    pickerInput(
                        "Wshed_wshed_S",
                        "Select the watersheds of interest",
                        unique(Wshed_data()$Watershed),
                        selected =   unique(Wshed_data()$Watershed)[1:2],
                        multiple = TRUE,
                        options = list(
                            `actions-box` = TRUE,
                            `header` = "Select watersheds ",
                            `windowPadding` = 1,
                            `width` = " css-width ",
                            `size` = 6
                        )
                    )
                }
        } else
            if (input$DefOrUserUpload_W == 'Default_Data_Portland') {
                if (input$AreaVsScen == 'allscen') {
                    pickerInput(
                        "Wshed_wshed_S",
                        "Select the scenarios of interest",
                        unique(Wshed_data()$Scenario),
                        selected =   unique(Wshed_data()$Scenario)[1:10],
                        multiple = TRUE,
                        options = list(
                            `actions-box` = TRUE,
                            `header` = "Select scenarios ",
                            `windowPadding` = 1,
                            `width` = " css-width ",
                            `size` = 6
                        )
                    ) 
                } else
                    if (input$AreaVsScen == 'allwat') {
                        pickerInput(
                            "Wshed_wshed_S",
                            "Select the watersheds of interest",
                            unique(Wshed_data()$Watershed),
                            selected =   unique(Wshed_data()$Watershed)[1:10],
                            multiple = TRUE,
                            options = list(
                                `actions-box` = TRUE,
                                `header` = "Select watersheds ",
                                `windowPadding` = 1,
                                `width` = " css-width ",
                                `size` = 6
                            )
                        )
                    }
                
            } else
                if (input$DefOrUserUpload_W == 'Default_Data_Seattle') {
                    if (input$AreaVsScen == 'allscen') {
                        pickerInput(
                            "Wshed_wshed_S",
                            "Select the scenarios of interest",
                            unique(Wshed_data()$Scenario),
                            selected =   unique(Wshed_data()$Scenario)[1:10],
                            multiple = TRUE,
                            options = list(
                                `actions-box` = TRUE,
                                `header` = "Select scenarios ",
                                `windowPadding` = 1,
                                `width` = " css-width ",
                                `size` = 6
                            )
                        ) 
                    } else
                        if (input$AreaVsScen == 'allwat') {
                            pickerInput(
                                "Wshed_wshed_S",
                                "Select the watersheds of interest",
                                unique(Wshed_data()$Watershed),
                                selected =   unique(Wshed_data()$Watershed)[1:10],
                                multiple = TRUE,
                                options = list(
                                    `actions-box` = TRUE,
                                    `header` = "Select watersheds ",
                                    `windowPadding` = 1,
                                    `width` = " css-width ",
                                    `size` = 6
                                )
                            )
                        }
                    
                } else
                    if (input$DefOrUserUpload_W == 'Default_Data_LT') {
                        if (input$AreaVsScen == 'allscen') {
                            pickerInput(
                                "Wshed_wshed_S",
                                "Select the scenarios of interest",
                                unique(Wshed_data()$Scenario),
                                selected =   unique(Wshed_data()$Scenario)[1:10],
                                multiple = TRUE,
                                options = list(
                                    `actions-box` = TRUE,
                                    `header` = "Select scenarios ",
                                    `windowPadding` = 1,
                                    `width` = " css-width ",
                                    `size` = 6
                                )
                            ) 
                        } else
                            if (input$AreaVsScen == 'allwat') {
                                pickerInput(
                                    "Wshed_wshed_S",
                                    "Select the watersheds of interest",
                                    unique(Wshed_data()$Watershed),
                                    selected =   unique(Wshed_data()$Watershed)[1:10],
                                    multiple = TRUE,
                                    options = list(
                                        `actions-box` = TRUE,
                                        `header` = "Select watersheds ",
                                        `windowPadding` = 1,
                                        `width` = "css-width",
                                        `size` = 6
                                    ),
                                    choicesOpt = list(content = stringr::str_trunc(
                                        unique(Wshed_data()$Watershed),
                                        width = 35
                                    ))
                                )
                            }
                        
                    } else
                        if (input$DefOrUserUpload_W == 'Default_Data_Palouse') {
                            if (input$AreaVsScen == 'allscen') {
                                pickerInput(
                                    "Wshed_wshed_S",
                                    "Select the scenarios of interest",
                                    unique(Wshed_data()$Scenario),
                                    selected =   unique(Wshed_data()$Scenario)[1:10],
                                    multiple = TRUE,
                                    options = list(
                                        `actions-box` = TRUE,
                                        `header` = "Select scenarios ",
                                        `windowPadding` = 1,
                                        `width` = " css-width ",
                                        `size` = 6
                                    )
                                )  
                            } else
                                if (input$AreaVsScen == 'allwat') {
                                    pickerInput(
                                        "Wshed_wshed_S",
                                        "Select the watersheds of interest",
                                        unique(Wshed_data()$Watershed),
                                        selected =   unique(Wshed_data()$Watershed)[1:2],
                                        multiple = TRUE,
                                        options = list(
                                            `actions-box` = TRUE,
                                            `header` = "Select watersheds ",
                                            `windowPadding` = 1,
                                            `width` = " css-width ",
                                            `size` = 6
                                        )
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
                selected = colnames(Wshed_data())[c(8, 10, 12, 15)],
                multiple = T,
                options = list(
                    `actions-box` = TRUE,
                    `header` = "Select metric ",
                    `windowPadding` = 1,
                    `width` = " css-width ",
                    `size` = 6
                ),
                choicesOpt = list(content = stringr::str_trunc(
                    colnames(Wshed_data())[!(colnames(Wshed_data()) %in% c("ProjectName",
                                                                           "Watershed",
                                                                           "Scenario"))], width = 35
                ))
            )
        } else
            if (input$DefOrUserUpload_W == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "wshed_var",
                    label = "Select the Water quantity/quality metric of interest",
                    options = list(
                        `actions-box` = TRUE,
                        `header` = "Select metric ",
                        `windowPadding` = 1,
                        `width` = " css-width ",
                        `size` = 6
                    ),
                    choices =   colnames(Wshed_data())[!(colnames(Wshed_data()) %in% c("ProjectName",
                                                                                       "Watershed",
                                                                                       "Scenario"))],
                    selected = colnames(Wshed_data()[c(8, 10, 12, 15)]),
                    multiple = T,
                    choicesOpt = list(content = stringr::str_trunc(
                        colnames(Wshed_data())[!(colnames(Wshed_data()) %in% c("ProjectName",
                                                                               "Watershed",
                                                                               "Scenario"))], width = 35
                    ))
                )
                
            } else
                if (input$DefOrUserUpload_W == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "wshed_var",
                        label = "Select the Water quantity/quality metric of interest",
                        options = list(
                            `actions-box` = TRUE,
                            `header` = "Select metric ",
                            `windowPadding` = 1,
                            `width` = " css-width ",
                            `size` = 6
                        ),
                        choices =   colnames(Wshed_data())[!(colnames(Wshed_data()) %in% c("ProjectName",
                                                                                           "Watershed",
                                                                                           "Scenario"))],
                        selected = colnames(Wshed_data()[c(8, 10, 12, 15)]),
                        multiple = T,
                        choicesOpt = list(content = stringr::str_trunc(
                            colnames(Wshed_data())[!(colnames(Wshed_data()) %in% c("ProjectName",
                                                                                   "Watershed",
                                                                                   "Scenario"))], width = 35
                        ))
                        
                    )
                    
                } else
                    if (input$DefOrUserUpload_W == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "wshed_var",
                            label = "Select the Water quantity/quality metric of interest",
                            options = list(
                                `actions-box` = TRUE,
                                `header` = "Select metric ",
                                `windowPadding` = 1,
                                `width` = " css-width ",
                                `size` = 6
                            ),
                            choices =   colnames(Wshed_data())[!(colnames(Wshed_data()) %in% c("ProjectName",
                                                                                               "Watershed",
                                                                                               "Scenario"))],
                            selected = colnames(Wshed_data()[c(8, 10, 12, 14)]),
                            multiple = T,
                            choicesOpt = list(content = stringr::str_trunc(
                                colnames(Wshed_data())[!(colnames(Wshed_data()) %in% c("ProjectName",
                                                                                       "Watershed",
                                                                                       "Scenario"))], width = 35
                            ))
                        )
                        
                    } else
                        if (input$DefOrUserUpload_W == 'Default_Data_Palouse') {
                            pickerInput(
                                inputId = "wshed_var",
                                label = "Select the Water quantity/quality metric of interest",
                                options = list(
                                    `actions-box` = TRUE,
                                    `header` = "Select metric ",
                                    `windowPadding` = 1,
                                    `width` = " css-width ",
                                    `size` = 6
                                ),
                                choices =   colnames(Wshed_data())[!(colnames(Wshed_data()) %in% c("ProjectName",
                                                                                                   "Watershed",
                                                                                                   "Scenario"))],
                                selected = colnames(Wshed_data()[c(8, 10, 12, 15)]),
                                multiple = T,
                                choicesOpt = list(content = stringr::str_trunc(
                                    colnames(Wshed_data())[!(colnames(Wshed_data()) %in% c("ProjectName",
                                                                                           "Watershed",
                                                                                           "Scenario"))], width = 35
                                ))
                                
                            )
                            
                        }
        
    })
    
    
    ## ----------------------------------HILLSLOPE Server logic------------------------------------------##
    ######## Server logic for UI generation for  HILLSLOPE tab ##########
    
    output$H_FileInput <- renderUI({
        if (input$DefOrUserUpload_H == 'Upload data') {
            message = 'max. file size is 32MB'
            fileInput(
                "Hill_file",
                label = "Uplaod 'Hillslope' file (*_hill_*.csv)",
                multiple = F,
                placeholder = "No file selected",
                accept = ".csv"
            ) %>%
                helper(
                    icon = "question-circle",
                    colour = "#FF0000",
                    content = "H_upload",
                    type = "markdown",
                    size = "l",
                    buttonLabel = "Okay",
                    easyClose = TRUE,
                    fade = TRUE
                )
        } else
            if (input$DefOrUserUpload_H == 'Default_Data_Portland' |
                input$DefOrUserUpload_H == 'Default_Data_Seattle' |
                input$DefOrUserUpload_H == 'Default_Data_LT' |
                input$DefOrUserUpload_H == 'Default_Data_Palouse') {
                
            }
    })
    
    
    Hill_data <- reactive({
        req(input$DefOrUserUpload_H)
        if (input$DefOrUserUpload_H == 'Default_Data_Portland') {
            file1 <- "data/portland202009_hill_summary_cd.csv"
            a <- read.table(file = file1,
                            header = TRUE,
                            sep = ",") %>% dplyr::mutate(
                                Soil_Loss_kg = Soil.Loss..kg.ha. * Hillslope.Area..ha.,
                                Sediment_Deposition_kg = Sediment.Deposition..kg.ha. *
                                    Hillslope.Area..ha.,
                                Sediment_Yield_kg = Sediment.Yield..kg.ha. *
                                    Hillslope.Area..ha.,
                                Soluble_Reactive_P_kg = Solub..React..P..kg.ha.3. *
                                    Hillslope.Area..ha.,
                                Particulate_P_kg = Particulate.P..kg.ha.3. *
                                    Hillslope.Area..ha.,
                                Total_P_kg = Total.P..kg.ha.3. *
                                    Hillslope.Area..ha.,
                                Particle_Class_1_Fraction_kg = Particle.Class.1.Fraction *
                                    Hillslope.Area..ha.,
                                Particle_Class_2_Fraction_kg = Particle.Class.2.Fraction *
                                    Hillslope.Area..ha.,
                                Particle_Class_3_Fraction_kg = Particle.Class.3.Fraction *
                                    Hillslope.Area..ha.,
                                Particle_Class_4_Fraction_kg = Particle.Class.4.Fraction *
                                    Hillslope.Area..ha.,
                                Particle_Class_5_Fraction_kg = Particle.Class.5.Fraction *
                                    Hillslope.Area..ha.,
                                Particle_Fraction_Under_0.016_mm_kg = Particle.Fraction.Under.0.016.mm *
                                    Hillslope.Area..ha.,
                                Sediment_Yield_of_Particles_Under_0.016_mm_kg = Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha. *
                                    Hillslope.Area..ha.
                            )
        } else
            if (input$DefOrUserUpload_H == 'Default_Data_Seattle') {
                file1 <- "data/seattle202009_hill_summary_cd.csv"
                b <- read.table(file = file1,
                                header = TRUE,
                                sep = ",") %>% dplyr::mutate(
                                    Soil_Loss_kg = Soil.Loss..kg.ha. * Hillslope.Area..ha.,
                                    Sediment_Deposition_kg = Sediment.Deposition..kg.ha. *
                                        Hillslope.Area..ha.,
                                    Sediment_Yield_kg = Sediment.Yield..kg.ha. *
                                        Hillslope.Area..ha.,
                                    Soluble_Reactive_P_kg = Solub..React..P..kg.ha.3. *
                                        Hillslope.Area..ha.,
                                    Particulate_P_kg = Particulate.P..kg.ha.3. *
                                        Hillslope.Area..ha.,
                                    Total_P_kg = Total.P..kg.ha.3. *
                                        Hillslope.Area..ha.,
                                    Particle_Class_1_Fraction_kg = Particle.Class.1.Fraction *
                                        Hillslope.Area..ha.,
                                    Particle_Class_2_Fraction_kg = Particle.Class.2.Fraction *
                                        Hillslope.Area..ha.,
                                    Particle_Class_3_Fraction_kg = Particle.Class.3.Fraction *
                                        Hillslope.Area..ha.,
                                    Particle_Class_4_Fraction_kg = Particle.Class.4.Fraction *
                                        Hillslope.Area..ha.,
                                    Particle_Class_5_Fraction_kg = Particle.Class.5.Fraction *
                                        Hillslope.Area..ha.,
                                    Particle_Fraction_Under_0.016_mm_kg = Particle.Fraction.Under.0.016.mm *
                                        Hillslope.Area..ha.,
                                    Sediment_Yield_of_Particles_Under_0.016_mm_kg = Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha. *
                                        Hillslope.Area..ha.
                                )
            } else
                if (input$DefOrUserUpload_H == 'Default_Data_LT') {
                    file1 <- "data/lt_202010_hill_summary_cd.csv"
                    c <- read.table(file = file1,
                                    header = TRUE,
                                    sep = ",") %>% dplyr::mutate(
                                        Soil_Loss_kg = Soil.Loss..kg.ha. * Hillslope.Area..ha.,
                                        Sediment_Deposition_kg = Sediment.Deposition..kg.ha. *
                                            Hillslope.Area..ha.,
                                        Sediment_Yield_kg = Sediment.Yield..kg.ha. *
                                            Hillslope.Area..ha.,
                                        Soluble_Reactive_P_kg = Solub..React..P..kg.ha.3. *
                                            Hillslope.Area..ha.,
                                        Particulate_P_kg = Particulate.P..kg.ha.3. *
                                            Hillslope.Area..ha.,
                                        Total_P_kg = Total.P..kg.ha.3. *
                                            Hillslope.Area..ha.,
                                        Particle_Class_1_Fraction_kg = Particle.Class.1.Fraction *
                                            Hillslope.Area..ha.,
                                        Particle_Class_2_Fraction_kg = Particle.Class.2.Fraction *
                                            Hillslope.Area..ha.,
                                        Particle_Class_3_Fraction_kg = Particle.Class.3.Fraction *
                                            Hillslope.Area..ha.,
                                        Particle_Class_4_Fraction_kg = Particle.Class.4.Fraction *
                                            Hillslope.Area..ha.,
                                        Particle_Class_5_Fraction_kg = Particle.Class.5.Fraction *
                                            Hillslope.Area..ha.,
                                        Particle_Fraction_Under_0.016_mm_kg = Particle.Fraction.Under.0.016.mm *
                                            Hillslope.Area..ha.,
                                        Sediment_Yield_of_Particles_Under_0.016_mm_kg = Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha. *
                                            Hillslope.Area..ha.
                                    )
                } else
                    if (input$DefOrUserUpload_H == 'Default_Data_Palouse') {
                        file1 <- "data/Palouse202103_hill_summary_cd.csv"
                        d <- read.table(file = file1,
                                        header = TRUE,
                                        sep = ",") %>% dplyr::mutate(
                                            Soil_Loss_kg = Soil.Loss..kg.ha. * Hillslope.Area..ha.,
                                            Sediment_Deposition_kg = Sediment.Deposition..kg.ha. *
                                                Hillslope.Area..ha.,
                                            Sediment_Yield_kg = Sediment.Yield..kg.ha. *
                                                Hillslope.Area..ha.,
                                            Soluble_Reactive_P_kg = Solub..React..P..kg.ha.3. *
                                                Hillslope.Area..ha.,
                                            Particulate_P_kg = Particulate.P..kg.ha.3. *
                                                Hillslope.Area..ha.,
                                            Total_P_kg = Total.P..kg.ha.3. *
                                                Hillslope.Area..ha.,
                                            Particle_Class_1_Fraction_kg = Particle.Class.1.Fraction *
                                                Hillslope.Area..ha.,
                                            Particle_Class_2_Fraction_kg = Particle.Class.2.Fraction *
                                                Hillslope.Area..ha.,
                                            Particle_Class_3_Fraction_kg = Particle.Class.3.Fraction *
                                                Hillslope.Area..ha.,
                                            Particle_Class_4_Fraction_kg = Particle.Class.4.Fraction *
                                                Hillslope.Area..ha.,
                                            Particle_Class_5_Fraction_kg = Particle.Class.5.Fraction *
                                                Hillslope.Area..ha.,
                                            Particle_Fraction_Under_0.016_mm_kg = Particle.Fraction.Under.0.016.mm *
                                                Hillslope.Area..ha.,
                                            Sediment_Yield_of_Particles_Under_0.016_mm_kg = Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha. *
                                                Hillslope.Area..ha.
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
                            
                            e <- read.table(
                                file = file1$datapath,
                                header = TRUE,
                                sep = ","
                            )
                            
                            nms <-
                                c(
                                    "ProjectName",
                                    "WeppID",
                                    "TopazID",
                                    "Landuse",
                                    "Soil",
                                    "Length..m.",
                                    "Hillslope.Area..ha.",
                                    "Runoff..mm.",
                                    "Lateral.Flow..mm.",
                                    "Baseflow..mm.",
                                    "Soil.Loss..kg.ha.",
                                    "Sediment.Deposition..kg.ha.",
                                    "Sediment.Yield..kg.ha.",
                                    "Width..m.",
                                    "Slope",
                                    "LanduseDesc",
                                    "SoilDesc",
                                    "Solub..React..P..kg.ha.3.",
                                    "Particulate.P..kg.ha.3.",
                                    "Total.P..kg.ha.3.",
                                    "Particle.Class.1.Fraction",
                                    "Particle.Class.2.Fraction",
                                    "Particle.Class.3.Fraction",
                                    "Particle.Class.4.Fraction",
                                    "Particle.Class.5.Fraction",
                                    "Particle.Fraction.Under.0.016.mm",
                                    "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.",
                                    "Watershed",
                                    "Scenario"
                                )
                            
                            missingcolvals <-
                                setdiff(nms, names(e))  # Find names of missing columns
                            
                            if (identical(missingcolvals, character(0))) {
                                e <- e  %>%
                                    dplyr::mutate(
                                        Soil_Loss_kg = Soil.Loss..kg.ha. * Hillslope.Area..ha.,
                                        Sediment_Deposition_kg = Sediment.Deposition..kg.ha. *
                                            Hillslope.Area..ha.,
                                        Sediment_Yield_kg = Sediment.Yield..kg.ha. *
                                            Hillslope.Area..ha.,
                                        Soluble_Reactive_P_kg = Solub..React..P..kg.ha.3. *
                                            Hillslope.Area..ha.,
                                        Particulate_P_kg = Particulate.P..kg.ha.3. *
                                            Hillslope.Area..ha.,
                                        Total_P_kg = Total.P..kg.ha.3. *
                                            Hillslope.Area..ha.,
                                        Particle_Class_1_Fraction_kg = Particle.Class.1.Fraction *
                                            Hillslope.Area..ha.,
                                        Particle_Class_2_Fraction_kg = Particle.Class.2.Fraction *
                                            Hillslope.Area..ha.,
                                        Particle_Class_3_Fraction_kg = Particle.Class.3.Fraction *
                                            Hillslope.Area..ha.,
                                        Particle_Class_4_Fraction_kg = Particle.Class.4.Fraction *
                                            Hillslope.Area..ha.,
                                        Particle_Class_5_Fraction_kg = Particle.Class.5.Fraction *
                                            Hillslope.Area..ha.,
                                        Particle_Fraction_Under_0.016_mm_kg = Particle.Fraction.Under.0.016.mm *
                                            Hillslope.Area..ha.,
                                        Sediment_Yield_of_Particles_Under_0.016_mm_kg = Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha. *
                                            Hillslope.Area..ha.
                                    )
                            } else
                                if (!identical(missingcolvals, character(0))) {
                                    e[missingcolvals] <- 0 # Add them, filled with ''s
                                    e <- e  %>%
                                        dplyr::mutate(
                                            Soil_Loss_kg = Soil.Loss..kg.ha. * Hillslope.Area..ha.,
                                            Sediment_Deposition_kg = Sediment.Deposition..kg.ha. *
                                                Hillslope.Area..ha.,
                                            Sediment_Yield_kg = Sediment.Yield..kg.ha. *
                                                Hillslope.Area..ha.,
                                            Soluble_Reactive_P_kg = Solub..React..P..kg.ha.3. *
                                                Hillslope.Area..ha.,
                                            Particulate_P_kg = Particulate.P..kg.ha.3. *
                                                Hillslope.Area..ha.,
                                            Total_P_kg = Total.P..kg.ha.3. *
                                                Hillslope.Area..ha.,
                                            Particle_Class_1_Fraction_kg = Particle.Class.1.Fraction *
                                                Hillslope.Area..ha.,
                                            Particle_Class_2_Fraction_kg = Particle.Class.2.Fraction *
                                                Hillslope.Area..ha.,
                                            Particle_Class_3_Fraction_kg = Particle.Class.3.Fraction *
                                                Hillslope.Area..ha.,
                                            Particle_Class_4_Fraction_kg = Particle.Class.4.Fraction *
                                                Hillslope.Area..ha.,
                                            Particle_Class_5_Fraction_kg = Particle.Class.5.Fraction *
                                                Hillslope.Area..ha.,
                                            Particle_Fraction_Under_0.016_mm_kg = Particle.Fraction.Under.0.016.mm *
                                                Hillslope.Area..ha.,
                                            Sediment_Yield_of_Particles_Under_0.016_mm_kg = Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha. *
                                                Hillslope.Area..ha.
                                        )
                                    
                                }
                            
                        }
        
    })
    
    
    output$Hill_var <- renderUI({
        if (input$DefOrUserUpload_H == 'Upload data') {
            req(Hill_data())
            pickerInput(
                "Hill_variable",
                "Select the Water quantity/quality metric of interest",
                choices =   c(
                    "Runoff (mm)" = "Runoff..mm.",
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
                options = list(
                    `actions-box` = TRUE,
                    `header` = "Select metric ",
                    `windowPadding` = 2,
                    `width` = " css-width ",
                    `size` = 6
                )
            )
        } else
            if (input$DefOrUserUpload_H == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "Hill_variable",
                    label = "Select the Water quantity/quality metric of interest",
                    choices =   c(
                        "Runoff (mm)" = "Runoff..mm.",
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
                    options = list(
                        `actions-box` = TRUE,
                        `header` = "Select metric ",
                        `windowPadding` = 1,
                        `width` = " css-width ",
                        `size` = 6
                    )
                )
                
            } else
                if (input$DefOrUserUpload_H == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "Hill_variable",
                        label = "Select the Water quantity/quality metric of interest",
                        choices =   c(
                            "Runoff (mm)" = "Runoff..mm.",
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
                        options = list(
                            `actions-box` = TRUE,
                            `header` = "Select metric ",
                            `windowPadding` = 1,
                            `width` = " css-width ",
                            `size` = 6
                        )
                    )
                    
                } else
                    if (input$DefOrUserUpload_H == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "Hill_variable",
                            label = "Select the Water quantity/quality metric of interest",
                            choices =   c(
                                "Runoff (mm)" = "Runoff..mm.",
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
                            options = list(
                                `actions-box` = TRUE,
                                `header` = "Select metric ",
                                `windowPadding` = 1,
                                `width` = " css-width ",
                                `size` = 6
                            ),
                            choicesOpt = list(content = stringr::str_trunc(
                                c(
                                    "Runoff (mm)",
                                    "Lateral flow (mm)",
                                    "Baseflow (mm)",
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
                                width = 35
                            ))
                        )
                        
                    } else
                        if (input$DefOrUserUpload_H == 'Default_Data_Palouse') {
                            pickerInput(
                                inputId = "Hill_variable",
                                label = "Select the Water quantity/quality metric of interest",
                                choices =   c(
                                    "Runoff (mm)" = "Runoff..mm.",
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
                                options = list(
                                    `actions-box` = TRUE,
                                    `header` = "Select metric ",
                                    `windowPadding` = 1,
                                    `width` = " css-width ",
                                    `size` = 6
                                )
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
                options = list(
                    `actions-box` = TRUE,
                    `header` = "Select watershed ",
                    `windowPadding` = 1,
                    `width` = " css-width ",
                    `size` = 6
                )
            )
            
        } else
            if (input$DefOrUserUpload_H == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "Hill_wshed",
                    label = "Select the watershed of interest",
                    choices =   unique(Hill_data()$Watershed),
                    selected =   unique(Hill_data()$Watershed)[1],
                    options = list(
                        `actions-box` = TRUE,
                        `header` = "Select watershed ",
                        `windowPadding` = 1,
                        `width` = " css-width ",
                        `size` = 6
                    )
                )
                
            } else
                if (input$DefOrUserUpload_H == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "Hill_wshed",
                        label = "Select the watershed of interest",
                        choices =   unique(Hill_data()$Watershed),
                        selected =   unique(Hill_data()$Watershed)[1],
                        options = list(
                            `actions-box` = TRUE,
                            `header` = "Select watershed ",
                            `windowPadding` = 1,
                            `width` = " css-width ",
                            `size` = 6
                        )
                    )
                    
                } else
                    if (input$DefOrUserUpload_H == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "Hill_wshed",
                            label = "Select the watershed of interest",
                            choices =   unique(Hill_data()$Watershed),
                            selected =   unique(Hill_data()$Watershed)[1],
                            options = list(
                                `actions-box` = TRUE,
                                `header` = "Select watershed ",
                                `windowPadding` = 1,
                                `width` = " css-width ",
                                `size` = 6
                            ),
                            choicesOpt = list(content = stringr::str_trunc(
                                unique(Hill_data()$Watershed),
                                width = 35
                            ))
                        )
                    } else
                        if (input$DefOrUserUpload_H == 'Default_Data_Palouse') {
                            pickerInput(
                                inputId = "Hill_wshed",
                                label = "Select the watershed of interest",
                                choices =   unique(Hill_data()$Watershed),
                                selected =   unique(Hill_data()$Watershed)[1],
                                options = list(
                                    `actions-box` = TRUE,
                                    `header` = "Select watershed ",
                                    `windowPadding` = 1,
                                    `width` = " css-width ",
                                    `size` = 6
                                )
                            )
                            
                        }
        
    })
    
    
    
    output$Hill_scen_base <- renderUI({
        if (input$DefOrUserUpload_H == 'Upload data') {
            req(Hill_data())
            pickerInput(
                "Hill_scen_base",
                "Select the baseline management scenario",
                unique(Hill_data()$Scenario),
                unique(Hill_data()$Scenario)[1],
                multiple = F,
                options = list(
                    `actions-box` = TRUE,
                    `header` = "Select baseline scenario ",
                    `windowPadding` = 1,
                    `width` = " css-width ",
                    `size` = 6
                )
            )
        } else
            if (input$DefOrUserUpload_H == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "Hill_scen_base",
                    label = "Select the baseline management scenario",
                    choices = unique(Hill_data()$Scenario),
                    unique(Hill_data()$Scenario)[1],
                    multiple = F,
                    options = list(
                        `actions-box` = TRUE,
                        `header` = "Select baseline scenario ",
                        `windowPadding` = 1,
                        `width` = " css-width ",
                        `size` = 6
                    )
                )
                
            } else
                if (input$DefOrUserUpload_H == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "Hill_scen_base",
                        label = "Select the baseline management scenario",
                        choices = unique(Hill_data()$Scenario),
                        unique(Hill_data()$Scenario)[1],
                        multiple = F,
                        options = list(
                            `actions-box` = TRUE,
                            `header` = "Select baseline scenario ",
                            `windowPadding` = 1,
                            `width` = " css-width ",
                            `size` = 6
                        )
                    )
                    
                } else
                    if (input$DefOrUserUpload_H == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "Hill_scen_base",
                            label = "Select the baseline management scenario",
                            choices = unique(Hill_data()$Scenario),
                            unique(Hill_data()$Scenario)[1],
                            multiple = F,
                            options = list(
                                `actions-box` = TRUE,
                                `header` = "Select baseline scenario ",
                                `windowPadding` = 1,
                                `width` = " css-width ",
                                `size` = 6
                            )
                        )
                        
                    } else
                        if (input$DefOrUserUpload_H == 'Default_Data_Palouse') {
                            pickerInput(
                                inputId = "Hill_scen_base",
                                label = "Select the baseline management scenario",
                                choices = unique(Hill_data()$Scenario),
                                unique(Hill_data()$Scenario)[1],
                                multiple = F,
                                options = list(
                                    `actions-box` = TRUE,
                                    `header` = "Select baseline scenario ",
                                    `windowPadding` = 1,
                                    `width` = " css-width ",
                                    `size` = 6
                                )
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
                options = list(
                    `actions-box` = TRUE,
                    `header` = "Select comparison scenario ",
                    `windowPadding` = 1,
                    `width` = " css-width ",
                    `size` = 6
                )
            )
        } else
            if (input$DefOrUserUpload_H == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "Hill_scen_comp",
                    label = "Select the management scenario to compare",
                    choices = unique(Hill_data()$Scenario),
                    selected = unique(Hill_data()$Scenario)[7],
                    multiple = T,
                    options = list(
                        `actions-box` = TRUE,
                        `header` = "Select comparison scenario ",
                        `windowPadding` = 1,
                        `width` = " css-width ",
                        `size` = 6
                    )
                    
                )
                
            } else
                if (input$DefOrUserUpload_H == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "Hill_scen_comp",
                        label = "Select the management scenario to compare",
                        choices = unique(Hill_data()$Scenario),
                        selected = unique(Hill_data()$Scenario)[7],
                        multiple = T,
                        options = list(
                            `actions-box` = TRUE,
                            `header` = "Select comparison scenario ",
                            `windowPadding` = 1,
                            `width` = " css-width ",
                            `size` = 6
                        )
                        
                    )
                    
                } else
                    if (input$DefOrUserUpload_H == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "Hill_scen_comp",
                            label = "Select the management scenario to compare",
                            choices = unique(Hill_data()$Scenario),
                            selected = unique(Hill_data()$Scenario)[7],
                            multiple = T,
                            options = list(
                                `actions-box` = TRUE,
                                `header` = "Select comparison scenario ",
                                `windowPadding` = 1,
                                `width` = " css-width ",
                                `size` = 6
                            )
                            
                        )
                        
                    } else
                        if (input$DefOrUserUpload_H == 'Default_Data_Palouse') {
                            pickerInput(
                                inputId = "Hill_scen_comp",
                                label = "Select the management scenario to compare",
                                choices = unique(Hill_data()$Scenario),
                                selected = unique(Hill_data()$Scenario)[3],
                                multiple = T,
                                options = list(
                                    `actions-box` = TRUE,
                                    `header` = "Select comparison scenario ",
                                    `windowPadding` = 1,
                                    `width` = " css-width ",
                                    `size` = 6
                                )
                                
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
                options = list(
                    `actions-box` = TRUE,
                    `header` = "Select scenario ",
                    `windowPadding` = 1,
                    `width` = " css-width ",
                    `size` = 6
                )
            )
        } else
            if (input$DefOrUserUpload_H == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "Hill_scen",
                    label = "Select Scenario do display data summary",
                    unique(Hill_data()$Scenario),
                    unique(Hill_data()$Scenario)[5],
                    multiple = F,
                    options = list(
                        `actions-box` = TRUE,
                        `header` = "Select scenario ",
                        `windowPadding` = 1,
                        `width` = " css-width ",
                        `size` = 6
                    )
                )
                
            } else
                if (input$DefOrUserUpload_H == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "Hill_scen",
                        label = "Select Scenario do display data summary",
                        unique(Hill_data()$Scenario),
                        unique(Hill_data()$Scenario)[5],
                        multiple = F,
                        options = list(
                            `actions-box` = TRUE,
                            `header` = "Select scenario ",
                            `windowPadding` = 1,
                            `width` = " css-width ",
                            `size` = 6
                        )
                    )
                    
                } else
                    if (input$DefOrUserUpload_H == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "Hill_scen",
                            label = "Select Scenario do display data summary",
                            unique(Hill_data()$Scenario),
                            unique(Hill_data()$Scenario)[5],
                            multiple = F,
                            options = list(
                                `actions-box` = TRUE,
                                `header` = "Select scenario ",
                                `windowPadding` = 1,
                                `width` = " css-width ",
                                `size` = 6
                            )
                        )
                        
                    } else
                        if (input$DefOrUserUpload_H == 'Default_Data_Palouse') {
                            pickerInput(
                                inputId = "Hill_scen",
                                label = "Select Scenario do display data summary",
                                unique(Hill_data()$Scenario),
                                unique(Hill_data()$Scenario)[1],
                                multiple = F,
                                options = list(
                                    `actions-box` = TRUE,
                                    `header` = "Select scenario ",
                                    `windowPadding` = 1,
                                    `width` = " css-width ",
                                    `size` = 6
                                )
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
            if (input$DefOrUserUpload_S == 'Default_Data_Portland' | 
                input$DefOrUserUpload_S == 'Default_Data_Seattle' | 
                input$DefOrUserUpload_S == 'Default_Data_LT' |
                input$DefOrUserUpload_S == 'Default_Data_Palouse') {
            }
    }) 
     
    
    Spatial_data1 <- reactive({
        req(input$DefOrUserUpload_S)
        if (input$DefOrUserUpload_S == 'Default_Data_Portland') {
            readRDS("data/portland202009_shps_subcatchments_wgs84_split_wshed_and_scen.RDS")
        } else
            if (input$DefOrUserUpload_S == 'Default_Data_Seattle') {
                readRDS("data/seattle202009_shps_subcatchments_wgs84_split_wshed_and_scen.RDS")
            }else
                if (input$DefOrUserUpload_S == 'Default_Data_LT') {
                    qs::qread("data/lt_202010_shps_subcatchments_wgs84_split_wshed_and_scen.qs")
                }else
                    if (input$DefOrUserUpload_S == 'Default_Data_Palouse') {
                        qs::qread("data/Palouse202103_shps_subcatchments_wgs84_split_wshed_and_scen.qs")
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
                               `width` = " css-width ",
                               `size` = 6),
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
                                   `width` = " css-width ",
                                   `size` = 6),
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
                                       `width` = " css-width ",
                                       `size` = 6),
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
                                           `width` = " css-width ",
                                           `size` = 6),
                            choicesOpt = list(
                                content = stringr::str_trunc(unique(Spatial_data()$Watershed),
                                                             width = 25)),
                            selected = unique(Spatial_data()$Watershed)[60],
                            multiple = T
                        )
                        
                    }else
                        if (input$DefOrUserUpload_S == 'Default_Data_Palouse') {
                            pickerInput(
                                inputId = "S_wshed",
                                label = "Select the watershed of interest",
                                choices =  unique(as.character(Spatial_data()$Watershed)),
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select Watershed",
                                               `windowPadding` = 1,
                                               `width` = " css-width ",
                                               `size` = 6),
                                selected = unique(Spatial_data()$Watershed)[1],
                                multiple = T
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
                               `width` = " css-width ",
                               `size` = 6)
            )
        } else
            if (input$DefOrUserUpload_S == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "S_scen_base",
                    label = "Select the baseline management scenario",
                    choices = unique(Spatial_data()$Scenario),
                    unique(Spatial_data()$Scenario)[1],
                    multiple = F,
                    options = list(`actions-box` = TRUE,
                                   `header` = "Select baseline scenario ",
                                   `windowPadding` = 1,
                                   `width` = " css-width ",
                                   `size` = 6)
                )
                
            }else
                if (input$DefOrUserUpload_S == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "S_scen_base",
                        label = "Select the baseline management scenario",
                        choices = unique(Spatial_data()$Scenario),
                        unique(Spatial_data()$Scenario)[1],
                        multiple = F,
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select baseline scenario ",
                                       `windowPadding` = 1,
                                       `width` = " css-width ",
                                       `size` = 6)
                    )
                    
                }else
                    if (input$DefOrUserUpload_S == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "S_scen_base",
                            label = "Select the baseline management scenario",
                            choices = unique(Spatial_data()$Scenario),
                            unique(Spatial_data()$Scenario)[1],
                            multiple = F,
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select baseline scenario ",
                                           `windowPadding` = 1,
                                           `width` = " css-width ",
                                           `size` = 6)
                        )
                        
                    }else
                        if (input$DefOrUserUpload_S == 'Default_Data_Palouse') {
                            pickerInput(
                                inputId = "S_scen_base",
                                label = "Select the baseline management scenario",
                                choices = unique(Spatial_data()$Scenario),
                                unique(Spatial_data()$Scenario)[1],
                                multiple = F,
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select baseline scenario ",
                                               `windowPadding` = 1,
                                               `width` = " css-width ",
                                               `size` = 6)
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
                               `width` = " css-width ",
                               `size` = 6)
            )
        } else
            if (input$DefOrUserUpload_S == 'Default_Data_Portland') {
                pickerInput(
                    inputId = "S_scen_comp",
                    label = "Select the management scenario to compare",
                    choices = unique(Spatial_data()$Scenario),
                    unique(Spatial_data()$Scenario)[2],
                    multiple = F,
                    options = list(`actions-box` = TRUE,
                                   `header` = "Select comparison scenario ",
                                   `windowPadding` = 1,
                                   `width` = " css-width ",
                                   `size` = 6)
                )
                
            }else
                if (input$DefOrUserUpload_S == 'Default_Data_Seattle') {
                    pickerInput(
                        inputId = "S_scen_comp",
                        label = "Select the management scenario to compare",
                        choices = unique(Spatial_data()$Scenario),
                        unique(Spatial_data()$Scenario)[2],
                        multiple = F,
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select comparison scenario ",
                                       `windowPadding` = 1,
                                       `width` = " css-width ",
                                       `size` = 6)
                    )
                    
                }else
                    if (input$DefOrUserUpload_S == 'Default_Data_LT') {
                        pickerInput(
                            inputId = "S_scen_comp",
                            label = "Select the management scenario to compare",
                            choices = unique(Spatial_data()$Scenario),
                            unique(Spatial_data()$Scenario)[2],
                            multiple = F,
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select comparison scenario ",
                                           `windowPadding` = 1,
                                           `width` = " css-width ",
                                           `size` = 6)
                        )
                        
                    }else
                        if (input$DefOrUserUpload_S == 'Default_Data_Palouse') {
                            pickerInput(
                                inputId = "S_scen_comp",
                                label = "Select the management scenario to compare",
                                choices = unique(Spatial_data()$Scenario),
                                unique(Spatial_data()$Scenario)[2],
                                multiple = F,
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select comparison scenario ",
                                               `windowPadding` = 1,
                                               `width` = " css-width ",
                                               `size` = 6)
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
                               `width` = " css-width ",
                               `size` = 6)
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
                                   `width` = " css-width ",
                                   `size` = 6)
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
                                       `width` = " css-width ",
                                       `size` = 6)
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
                                           `width` = " css-width ",
                                           `size` = 6)
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
                                               `width` = " css-width ",
                                               `size` = 6)
                            )
                            
                        }
        
    })
    
    ## ----------------------------------Spatial-Viz tab server logic------------------------------------------##
    ######## Server logic for UI generation for spatial-Viz tab ##########
    
    output$Which_SWAT_out <- renderUI({
        radioGroupButtons(
                inputId = "which_file",
                label = "Select Output",
                choices = c("Subbasin", "Reach", "HRU"),
                selected = "Reach",
                checkIcon = list(
                    yes = icon("ok",
                               lib = "glyphicon")),
                status = "success"
            )
        })
    
    output$SWAT_wshed_mgmt_optns <- renderUI({
        
        if (input$DefOrUserUpload_SWAT == 'Upload data') {
            req(SWAT_data())
            req(input$which_file)
            # req(input$swat_file)
            if(input$which_file == 'Reach'){
                awesomeRadio(
                    inputId = "AreaVsScen_swat",
                    label = "Management/Watershed Options: ",
                    choices = c(
                        "One Watershed, All Scenarios" = "allscen_swat",
                        "One Scenario, Selected Watersheds" =
                            "allwat_swat"
                    ),
                    selected = "allscen_swat",
                    status = 'success'
                )
            }}else
            if (input$DefOrUserUpload_SWAT == 'Default_Data_WE38') {
                req(input$which_file)
                if(input$which_file == 'Reach'){
                    awesomeRadio(
                        inputId = "AreaVsScen_swat",
                        label = "Management/Watershed Options: ",
                        choices = c(
                            "One Watershed, All Scenarios" = "allscen_swat",
                            "One Scenario, Selected Watersheds" =
                                "allwat_swat"
                        ),
                        selected = "allscen_swat",
                        status = 'success'
                    )
                }
            }
    })
    
    output$swat_tabpanels <- renderUI({
        req(input$which_file)
        if(input$which_file == 'Subbasin'){
            tabsetPanel(
                tabPanel("Subwatersheds",icon =icon("layer-group"),style = 'padding:20px;',
                         leaflet::leafletOutput("Plotsub", height = "600px", width =
                                          "800px") %>% withSpinner(type = 8 )),
                tabPanel("Table",icon =icon("tablesub"),style = 'padding:20px;',
                         DT::dataTableOutput("tablesub")  %>% withSpinner(type = 8 ))
            )
        }else
            if(input$which_file == 'Reach'){
        tabsetPanel(
            tabPanel("Heatmap",icon =icon("th"),style = 'padding:20px;',
                     plotlyOutput("Plotheatrch", height = "600px", width =
                                      "950px") %>% withSpinner(type = 8 )),
            tabPanel("Bar Chart",icon =icon("chart-bar"),style = 'padding:20px;',
                     plotlyOutput("Plotbarrch", height = "600px", width =
                                       "950px")  %>% withSpinner(type = 8 ))
        )
            }else
                if(input$which_file == 'HRU'){
                    tabsetPanel(
                        tabPanel("HRUs",icon =icon("layer-group"),style = 'padding:20px;',
                                 leaflet::leafletOutput("Plothru", height = "600px", width =
                                                  "800px") %>% withSpinner(type = 8 )),
                        tabPanel("Table",icon =icon("table"),style = 'padding:20px;',
                                 DT::dataTableOutput("tablehru")  %>% withSpinner(type = 8 ))
                    )
                }
    })
    
    # output$swat_tabpanels <- renderUI({
    #     req(input$DefOrUserUpload_SWAT)
    #     req(input$which_file)
    #     if (input$DefOrUserUpload_SWAT == 'Upload data') {
    #         if(input$which_file == 'Subbasin'){
    #             tabsetPanel(
    #                 tabPanel("Plot",icon =icon("layer-group"),style = 'padding:20px;',
    #                          plotlyOutput("Plotsub", height = "600px", width =
    #                                           "800px") %>% withSpinner(type = 8 )),
    #                 tabPanel("Table",icon =icon("table"),style = 'padding:20px;',
    #                          tableOutput("tablesub")  %>% withSpinner(type = 8 ))
    #             )
    #         }else
    #             if(input$which_file == 'Reach'){
    #                 tabsetPanel(
    #                     tabPanel("Heatmap",icon =icon("th"),style = 'padding:20px;',
    #                              plotlyOutput("Plotheatrch", height = "600px", width =
    #                                               "800px") %>% withSpinner(type = 8 )),
    #                     tabPanel("Bar Chart",icon =icon("chart-bar"),style = 'padding:20px;',
    #                              plotlyOutput("Plotbarrch", height = "600px", width =
    #                                               "800px")  %>% withSpinner(type = 8 ))
    #                 )
    #             }else
    #                 if(input$which_file == 'HRU'){
    #                     tabsetPanel(
    #                         tabPanel("Plot",icon =icon("layer-group"),style = 'padding:20px;',
    #                                  plotlyOutput("Plothru", height = "600px", width =
    #                                                   "800px") %>% withSpinner(type = 8 )),
    #                         tabPanel("Table",icon =icon("table"),style = 'padding:20px;',
    #                                  tableOutput("tablehru")  %>% withSpinner(type = 8 ))
    #                     )
    #                 }
    #     }else
    #         if (input$DefOrUserUpload_SWAT == 'Default_Data_WE38') {
    #             if(input$which_file == 'Subbasin'){
    #                 tabsetPanel(
    #                     tabPanel("Plot",icon =icon("layer-group"),style = 'padding:20px;',
    #                              plotlyOutput("Plotsub", height = "600px", width =
    #                                               "800px") %>% withSpinner(type = 8 )),
    #                     tabPanel("Table",icon =icon("table"),style = 'padding:20px;',
    #                              tableOutput("Plotsub")  %>% withSpinner(type = 8 ))
    #                 )
    #             }else
    #                 if(input$which_file == 'Reach'){
    #                     tabsetPanel(
    #                         tabPanel("Heatmap",icon =icon("th"),style = 'padding:20px;',
    #                                  plotlyOutput("Plotheatrch", height = "600px", width =
    #                                                   "800px") %>% withSpinner(type = 8 )),
    #                         tabPanel("Bar Chart",icon =icon("chart-bar"),style = 'padding:20px;',
    #                                  plotlyOutput("Plotbarrch", height = "600px", width =
    #                                                   "800px")  %>% withSpinner(type = 8 ))
    #                     )
    #                 }else
    #                     if(input$which_file == 'HRU'){
    #                         tabsetPanel(
    #                             tabPanel("Plot",icon =icon("layer-group"),style = 'padding:20px;',
    #                                      plotlyOutput("Plothru", height = "600px", width =
    #                                                       "800px") %>% withSpinner(type = 8 )),
    #                             tabPanel("Table",icon =icon("table"),style = 'padding:20px;',
    #                                      tableOutput("tablehru")  %>% withSpinner(type = 8 ))
    #                         )
    #                     }
    #         }
    # })
    
    output$SWAT_FileInput <- renderUI({
        req(input$DefOrUserUpload_SWAT)
        req(input$which_file)
        if (input$DefOrUserUpload_SWAT == 'Upload data') {
            if(input$which_file == 'Subbasin'){
                fileInput(
                    "swat_file_sub",
                    label = "Uplaod subbasin RDS/qs file",
                    multiple = F,
                    placeholder = "No file selected",
                    accept = c(".RDS", ".qs")
                )%>%
                    helper(icon = "question-circle",colour = "#FF0000",
                           content = "SWAT_upload_sub",
                           type = "markdown", size = "l",
                           buttonLabel = "Okay", easyClose = TRUE, fade = TRUE)
            }else
                if(input$which_file == 'Reach'){
                    fileInput(
                        "swat_file_rch",
                        label = "Uplaod reach RDS/qs/csv file",
                        multiple = F,
                        placeholder = "No file selected",
                        accept = c(".RDS", ".qs", ".csv")
                    )%>%
                        helper(icon = "question-circle",colour = "#FF0000",
                               content = "SWAT_upload_rch",
                               type = "markdown", size = "l",
                               buttonLabel = "Okay", easyClose = TRUE, fade = TRUE)
                }else
                    if(input$which_file == 'HRU'){
                        fileInput(
                            "swat_file_hru",
                            label = "Uplaod hru RDS/qs file",
                            multiple = F,
                            placeholder = "No file selected",
                            accept = c(".RDS", ".qs")
                        )%>%
                            helper(icon = "question-circle",colour = "#FF0000",
                                   content = "SWAT_upload_hru",
                                   type = "markdown", size = "l",
                                   buttonLabel = "Okay", easyClose = TRUE, fade = TRUE)
                    }
            } else
                if (input$DefOrUserUpload_SWAT == 'Default_Data_WE38') {
                    req(input$which_file_def)
                    if(input$which_file_def == 'Subbasin'){

                    }else
                        if(input$which_file_def == 'Reach'){

                    }else
                        if(input$which_file_def == 'HRU'){

                    }
                }
    })
    
    SWAT_data <- reactive({
        req(input$DefOrUserUpload_SWAT)
        req(input$which_file)
        if (input$DefOrUserUpload_SWAT == 'Default_Data_WE38') {
            
            if(input$which_file == 'Reach'){
                d<- qs::qread("data/summary_output_rch.qs")
            }else
                if(input$which_file == 'Subbasin'){
                    d<-qs::qread("data/summary_output_sub.qs")%>%sf::st_set_crs(value = 4326)
                }else
                    if(input$which_file == 'HRU'){
                        d<-qs::qread("data/summary_output_hru.qs")%>%sf::st_set_crs(value = 4326)
                    }

        }else
            if (input$DefOrUserUpload_SWAT == 'Upload data'){
                req(input$which_file)
                if(input$which_file == 'Reach'){
                    req(input$swat_file_rch)
                    file <- input$swat_file_rch
                    if (is.null(file)) {
                        return()
                    }
                    if (tools::file_ext(file$datapath) == "RDS") {
                        d<-readRDS(file$datapath)
                    }else
                        if (tools::file_ext(file$datapath) == "qs") {
                            d<-qs::qread(file$datapath)
                        }else
                            if (tools::file_ext(file$datapath) == "csv") {
                                d<- read.csv(file$datapath)
                            }
                }else
                    if(input$which_file == 'Subbasin'){
                        req(input$swat_file_sub)
                        file <- input$swat_file_sub
                        if (is.null(file)) {
                            return()
                        }
                        if (tools::file_ext(file$datapath) == "RDS") {
                            d<-readRDS(file$datapath)
                            d<- d %>%sf::st_set_crs(value = 4326)
                        }else
                            if (tools::file_ext(file$datapath) == "qs") {
                                d<- qs::qread(file$datapath)
                                d<- d %>%sf::st_set_crs(value = 4326)
                            }

                    }else
                        if(input$which_file == 'HRU'){
                            req(input$swat_file_hru)
                            file <- input$swat_file_hru
                            if (is.null(file)) {
                                return()
                            }
                            if (tools::file_ext(file$datapath) == "RDS") {
                                d<- readRDS(file$datapath)
                                d<- d%>%sf::st_set_crs(value = 4326)
                            }else
                                if (tools::file_ext(file$datapath) == "qs") {
                                    d<- qs::qread(file$datapath)
                                    d<- d%>%sf::st_set_crs(value = 4326)
                                }

                        }
            }
    })
    
    
    
    output$SWAT_wshed <- renderUI({
        req(input$DefOrUserUpload_SWAT)
        req(SWAT_data())
        req(input$which_file)
        if (input$DefOrUserUpload_SWAT == 'Upload data') {
            if(input$which_file == 'Subbasin'){
            pickerInput(
                "SWAT_wshed",
                "Select the watershed of interest",
                choices = unique(as.character(SWAT_data()$Watershed)),
                options = list(`actions-box` = TRUE,
                               `header` = "Select Watershed",
                               `windowPadding` = 1,
                               `width` = " css-width ",
                               `size` = 6),
                selected = unique(SWAT_data()$Watershed)[1],
                multiple = F
            )
            }else
                if(input$which_file == 'Reach'){
                    req(input$AreaVsScen_swat)
                    if(input$AreaVsScen_swat == "allwat_swat"){
                    pickerInput(
                        "SWAT_wshed",
                        "Select the watershed of interest",
                        choices = unique(as.character(SWAT_data()$Watershed)),
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select Watershed",
                                       `windowPadding` = 1,
                                       `width` = " css-width ",
                                       `size` = 6),
                        selected = unique(SWAT_data()$Watershed)[1],
                        multiple = F
                    )}else
                        if(input$AreaVsScen_swat == "allscen_swat"){
                            pickerInput(
                                "SWAT_wshed",
                                "Select the watershed of interest",
                                choices = unique(as.character(SWAT_data()$Watershed)),
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select Watershed",
                                               `windowPadding` = 1,
                                               `width` = " css-width "),
                                selected = unique(SWAT_data()$Watershed)[1],
                                multiple = F
                            )
                        }
                }else
                    if(input$which_file == 'HRU'){
                        pickerInput(
                            "SWAT_wshed",
                            "Select the watershed of interest",
                            choices = unique(as.character(SWAT_data()$Watershed)),
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select Watershed",
                                           `windowPadding` = 1,
                                           `width` = " css-width ",
                                           `size` = 6),
                            selected = unique(SWAT_data()$Watershed)[1],
                            multiple = F
                        )
                    }
        } else
            if (input$DefOrUserUpload_SWAT == 'Default_Data_WE38') {
                req(input$which_file)
                if(input$which_file == 'Subbasin'){
                    pickerInput(
                        "SWAT_wshed",
                        "Select the watershed of interest",
                        choices = unique(as.character(SWAT_data()$Watershed)),
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select Watershed",
                                       `windowPadding` = 1,
                                       `width` = " css-width ",
                                       `size` = 6),
                        selected = unique(SWAT_data()$Watershed)[1],
                        multiple = F
                    )
                }else
                    if(input$which_file == 'Reach'){
                        req(input$AreaVsScen_swat)
                        if(input$AreaVsScen_swat == "allwat_swat"){
                            pickerInput(
                                "SWAT_wshed",
                                "Select the watershed of interest",
                                choices = unique(as.character(SWAT_data()$Watershed)),
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select Watershed",
                                               `windowPadding` = 1,
                                               `width` = " css-width ",
                                               `size` = 6),
                                selected = unique(SWAT_data()$Watershed)[1],
                                multiple = F
                            )}else
                                if(input$AreaVsScen_swat == "allscen_swat"){
                                    pickerInput(
                                        "SWAT_wshed",
                                        "Select the watershed of interest",
                                        choices = unique(as.character(SWAT_data()$Watershed)),
                                        options = list(`actions-box` = TRUE,
                                                       `header` = "Select Watershed",
                                                       `windowPadding` = 1,
                                                       `width` = " css-width "),
                                        selected = unique(SWAT_data()$Watershed)[1],
                                        multiple = F
                                    )
                                }
                    }else
                        if(input$which_file == 'HRU'){
                            pickerInput(
                                "SWAT_wshed",
                                "Select the watershed of interest",
                                choices = unique(as.character(SWAT_data()$Watershed)),
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select Watershed",
                                               `windowPadding` = 1,
                                               `width` = " css-width ",
                                               `size` = 6),
                                selected = unique(SWAT_data()$Watershed)[1],
                                multiple = F
                            )
                        }
            }
        })
    
    
    
    output$SWAT_scen <- renderUI({
        if (input$DefOrUserUpload_SWAT == 'Upload data') {
            req(SWAT_data())
            req(input$which_file)
            if(input$which_file == 'Subbasin'){
                pickerInput(
                    "SWAT_scen_sub_base",
                    "Select the scenario of interest",
                    choices = unique(as.character(SWAT_data()$Scenario)),
                    options = list(`actions-box` = TRUE,
                                   `header` = "Select Scenario",
                                   `windowPadding` = 1,
                                   `width` = " css-width ",
                                   `size` = 6),
                    selected = unique(SWAT_data()$Scenario)[1],
                    multiple = F
                )
            }else
                if(input$which_file == 'Reach'){
                    req(input$AreaVsScen_swat)
                    if(input$AreaVsScen_swat == "allscen_swat"){
                        }else
                            if(input$AreaVsScen_swat == "allwat_swat"){
                                pickerInput(
                                    "SWAT_scen_rch",
                                    "Select the scenario of interest",
                                    choices = unique(as.character(SWAT_data()$Scenario)),
                                    options = list(`actions-box` = TRUE,
                                                   `header` = "Select Scenario",
                                                   `windowPadding` = 1,
                                                   `width` = " css-width ",
                                                   `size` = 6),
                                    selected = unique(SWAT_data()$Scenario)[1],
                                    multiple = F
                                )
                            }
                }else
                    if(input$which_file == 'HRU'){
                        pickerInput(
                            "SWAT_scen_hru_base",
                            "Select the scenario of interest",
                            choices = unique(as.character(SWAT_data()$Scenario)),
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select Scenario",
                                           `windowPadding` = 1,
                                           `width` = " css-width ",
                                           `size` = 6),
                            selected = unique(SWAT_data()$Scenario)[1],
                            multiple = F
                        )
                    }
        } else
            if (input$DefOrUserUpload_SWAT == 'Default_Data_WE38') {
                req(input$which_file)
                if(input$which_file == 'Subbasin'){
                    pickerInput(
                        "SWAT_scen_sub_base",
                        "Select the scenario of interest",
                        choices = unique(as.character(SWAT_data()$Scenario)),
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select Scenario",
                                       `windowPadding` = 1,
                                       `width` = " css-width ",
                                       `size` = 6),
                        selected = unique(SWAT_data()$Scenario)[1],
                        multiple = F
                    )
                }else
                    if(input$which_file == 'Reach'){
                        req(input$AreaVsScen_swat)
                        if(input$AreaVsScen_swat == "allscen_swat"){
                        }else
                            if(input$AreaVsScen_swat == "allwat_swat"){
                                pickerInput(
                                    "SWAT_scen_rch",
                                    "Select the scenario of interest",
                                    choices = unique(as.character(SWAT_data()$Scenario)),
                                    options = list(`actions-box` = TRUE,
                                                   `header` = "Select Scenario",
                                                   `windowPadding` = 1,
                                                   `width` = " css-width ",
                                                   `size` = 6),
                                    selected = unique(SWAT_data()$Scenario)[1],
                                    multiple = F
                                )
                            }
                    }else
                        if(input$which_file == 'HRU'){
                            pickerInput(
                                "SWAT_scen_hru_base",
                                "Select the scenario of interest",
                                choices = unique(as.character(SWAT_data()$Scenario)),
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select Scenario",
                                               `windowPadding` = 1,
                                               `width` = " css-width ",
                                               `size` = 6),
                                selected = unique(SWAT_data()$Scenario)[1],
                                multiple = F
                            )
                        }
            }
    })
    
    
    output$SWAT_scen_comp <- renderUI({
        if (input$DefOrUserUpload_SWAT == 'Upload data') {
            req(SWAT_data())
            req(input$which_file)
            if(input$which_file == 'Subbasin'){
                pickerInput(
                    "SWAT_scen_sub_comp",
                    "Select the comparison scenario",
                    choices = unique(as.character(SWAT_data()$Scenario)),
                    options = list(`actions-box` = TRUE,
                                   `header` = "Select Scenario",
                                   `windowPadding` = 1,
                                   `width` = " css-width ",
                                   `size` = 6),
                    selected = unique(SWAT_data()$Scenario)[2],
                    multiple = F
                )
            }else
                if(input$which_file == 'HRU'){
                    pickerInput(
                        "SWAT_scen_hru_comp",
                        "Select the comparison scenario",
                        choices = unique(as.character(SWAT_data()$Scenario)),
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select Scenario",
                                       `windowPadding` = 1,
                                       `width` = " css-width ",
                                       `size` = 6),
                        selected = unique(SWAT_data()$Scenario)[2],
                        multiple = F )  
                }
        } else
            if (input$DefOrUserUpload_SWAT == 'Default_Data_WE38') {
                req(input$which_file)
                if(input$which_file == 'Subbasin'){
                    pickerInput(
                        "SWAT_scen_sub_comp",
                        "Select the comparison scenario",
                        choices = unique(as.character(SWAT_data()$Scenario)),
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select Scenario",
                                       `windowPadding` = 1,
                                       `width` = " css-width ",
                                       `size` = 6),
                        selected = unique(SWAT_data()$Scenario)[2],
                        multiple = F
                    )
                }else
                    if(input$which_file == 'HRU'){
                        pickerInput(
                            "SWAT_scen_hru_comp",
                            "Select the comparison scenario",
                            choices = unique(as.character(SWAT_data()$Scenario)),
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select Scenario",
                                           `windowPadding` = 1,
                                           `width` = " css-width ",
                                           `size` = 6),
                            selected = unique(SWAT_data()$Scenario)[2],
                            multiple = F
                        ) 
                    }
            }
    })
    
    
    
    output$SWAT_reachno <- renderUI({
        if (input$DefOrUserUpload_SWAT == 'Upload data') {
            req(SWAT_data())
            req(input$which_file)
            if(input$which_file == 'Subbasin'){
                NULL

            }else
                if(input$which_file == 'Reach'){
                    req(input$AreaVsScen_swat)
                    if (input$AreaVsScen_swat == 'allscen_swat') {
                        pickerInput(
                            "SWAT_reachnum",
                            "Select the reach of interest",
                            choices = unique(as.character(SWAT_data()$RCH)),
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select Reach",
                                           `windowPadding` = 1,
                                           `width` = " css-width ",
                                           `size` = 6),
                            selected = unique(SWAT_data()$RCH)[1],
                            multiple = F
                        )
                    }else
                        if (input$AreaVsScen_swat == 'allwat_swat') {
                            pickerInput(
                                "SWAT_reachnum",
                                "Select the reach of interest",
                                choices = unique(as.character(SWAT_data()$RCH)),
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select Reach",
                                               `windowPadding` = 1,
                                               `width` = " css-width ",
                                               `size` = 6),
                                selected = unique(SWAT_data()$RCH)[1:2],
                                multiple = T
                            )
                        }
                }else
                    if(input$which_file == 'HRU'){

                    }
        } else
            if (input$DefOrUserUpload_SWAT == 'Default_Data_WE38') {
                req(input$which_file)
                if(input$which_file == 'Subbasin'){

                }else
                    if(input$which_file == 'Reach'){
                        req(input$AreaVsScen_swat)
                        if (input$AreaVsScen_swat == 'allscen_swat') {
                        pickerInput(
                            "SWAT_reachnum",
                            "Select the reach of interest",
                            choices = unique(as.character(SWAT_data()$RCH)),
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select Reach",
                                           `windowPadding` = 1,
                                           `width` = " css-width ",
                                           `size` = 6),
                            selected = unique(SWAT_data()$RCH)[1],
                            multiple = F
                        )
                        }else
                            if (input$AreaVsScen_swat == 'allwat_swat') {
                                    pickerInput(
                                        "SWAT_reachnum",
                                        "Select the reach of interest",
                                        choices = unique(as.character(SWAT_data()$RCH)),
                                        options = list(`actions-box` = TRUE,
                                                       `header` = "Select Reach",
                                                       `windowPadding` = 1,
                                                       `width` = " css-width ",
                                                       `size` = 6),
                                        selected = unique(SWAT_data()$RCH)[1:2],
                                        multiple = T
                                    )
                                }
                    }else
                        if(input$which_file == 'HRU'){

                        }
            }
    })
    
    
    
    
    output$hru_slp_slider <- renderUI({
        if (input$DefOrUserUpload_SWAT == 'Upload data') {
            req(input$which_file)
            if(input$which_file == 'Subbasin'){
                }else
                if(input$which_file == 'Reach'){
                    
                }else
                    if(input$which_file == 'HRU'){
                        sliderInput(
                            "swat_thresh_slp",
                            "Slope Threshold (%):",
                            min = 0,
                            max = 100,
                            value = c(0,100),
                            step = NULL,
                            round = TRUE,
                            ticks = TRUE,
                            animate = FALSE
                        ) 
                        # %>%
                        #     helper(
                        #         icon = "question-circle",
                        #         colour = "#FF0000",
                        #         content = "H_plot_thresh",
                        #         type = "markdown",
                        #         size = "l",
                        #         buttonLabel = "Okay",
                        #         easyClose = TRUE,
                        #         fade = TRUE
                        #     )  
                    }
        } else
            if (input$DefOrUserUpload_SWAT == 'Default_Data_WE38') {
                req(input$which_file)
                if(input$which_file == 'Subbasin'){
                    
                }else
                    if(input$which_file == 'Reach'){
                        
                    }else
                        if(input$which_file == 'HRU'){
                            sliderInput(
                                "swat_thresh_slp",
                                "Slope Threshold (%):",
                                min = 0,
                                max = 100,
                                value = c(0,100),
                                step = NULL,
                                round = TRUE,
                                ticks = TRUE,
                                animate = FALSE
                            )
                            # %>%
                            #     helper(
                            #         icon = "question-circle",
                            #         colour = "#FF0000",
                            #         content = "H_plot_thresh",
                            #         type = "markdown",
                            #         size = "l",
                            #         buttonLabel = "Okay",
                            #         easyClose = TRUE,
                            #         fade = TRUE
                            #     )  
                        }
            }
    })
    
    output$swat_hru_lulc <- renderUI({
        req(SWAT_data())
        if (input$DefOrUserUpload_SWAT == 'Upload data') {
            req(input$which_file)
            if(input$which_file == 'Subbasin'){
            }else
                if(input$which_file == 'Reach'){
                    
                }else
                    if(input$which_file == 'HRU'){
                        pickerInput(
                            "SWAT_hru_lulc",
                            "Select the Land Use/Land Cover of interest",
                            choices = unique(as.character(SWAT_data()$LULC)),
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select Reach",
                                           `windowPadding` = 1,
                                           `width` = " css-width ",
                                           `size` = 6),
                            selected = unique(SWAT_data()$LULC)[1],
                            multiple = TRUE
                        )
                    }
        } else
            if (input$DefOrUserUpload_SWAT == 'Default_Data_WE38') {
                req(input$which_file)
                if(input$which_file == 'Subbasin'){
                    
                }else
                    if(input$which_file == 'Reach'){
                        
                    }else
                        if(input$which_file == 'HRU'){
                            pickerInput(
                                "SWAT_hru_lulc",
                                "Select the Land Use/Land Cover of interest",
                                choices = unique(as.character(SWAT_data()$LULC)),
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select Reach",
                                               `windowPadding` = 1,
                                               `width` = " css-width ",
                                               `size` = 6),
                                selected = unique(SWAT_data()$LULC)[1],
                                multiple = TRUE
                            )  
                        }
            }
    })
    
    
    output$swat_hru_subno <- renderUI({
        req(SWAT_data())
        if (input$DefOrUserUpload_SWAT == 'Upload data') {
            req(input$which_file)
            if(input$which_file == 'Subbasin'){
            }else
                if(input$which_file == 'Reach'){
                    
                }else
                    if(input$which_file == 'HRU'){
                        pickerInput(
                            "SWAT_hru_sub",
                            "Select the Subbasin of interest",
                            choices = unique(as.character(SWAT_data()$SUBBASIN)),
                            options = list(`actions-box` = TRUE,
                                           `header` = "Select Reach",
                                           `windowPadding` = 1,
                                           `width` = " css-width ",
                                           `size` = 6),
                            selected = unique(SWAT_data()$SUBBASIN)[1],
                            multiple = TRUE
                        )
                    }
        } else
            if (input$DefOrUserUpload_SWAT == 'Default_Data_WE38') {
                req(input$which_file)
                if(input$which_file == 'Subbasin'){
                    
                }else
                    if(input$which_file == 'Reach'){
                        
                    }else
                        if(input$which_file == 'HRU'){
                            pickerInput(
                                "SWAT_hru_sub",
                                "Select the Subbasin of interest",
                                choices = unique(as.character(SWAT_data()$SUBBASIN)),
                                options = list(`actions-box` = TRUE,
                                               `header` = "Select Reach",
                                               `windowPadding` = 1,
                                               `width` = " css-width ",
                                               `size` = 6),
                                selected = unique(SWAT_data()$SUBBASIN)[1],
                                multiple = TRUE
                            ) 
                        }
            }
    })
    
    
    
    output$SWAT_subnum <- renderUI({
        if (input$DefOrUserUpload_SWAT == 'Upload data') {
            req(SWAT_data())
            req(input$which_file)
            if(input$which_file == 'Subbasin'){
                pickerInput(
                    "SWAT_subnum",
                    "Select the subbasin of interest",
                    choices = unique(as.character(SWAT_data()$SUB)),
                    options = list(`actions-box` = TRUE,
                                   `header` = "Select Reach",
                                   `windowPadding` = 1,
                                   `width` = " css-width ",
                                   `size` = 6),
                    selected = unique(SWAT_data()$SUB),
                    multiple = TRUE
                )
                
            }else
                if(input$which_file == 'Reach'){
                    
                }else
                    if(input$which_file == 'HRU'){
                        
                    }
        } else
            if (input$DefOrUserUpload_SWAT == 'Default_Data_WE38') {
                req(input$which_file)
                if(input$which_file == 'Subbasin'){
                    pickerInput(
                        "SWAT_subnum",
                        "Select the subbasin of interest",
                        choices = unique(as.character(SWAT_data()$SUB)),
                        options = list(`actions-box` = TRUE,
                                       `header` = "Select Reach",
                                       `windowPadding` = 1,
                                       `width` = " css-width ",
                                       `size` = 6),
                        selected = unique(SWAT_data()$SUB),
                        multiple = TRUE
                    )
                    
                }else
                    if(input$which_file == 'Reach'){
                        
                    }else
                        if(input$which_file == 'HRU'){
                            
                        }
            }
    })
    
    
    output$SWAT_variable <- renderUI({
        req(SWAT_data())
        req(input$which_file)
        if (input$DefOrUserUpload_SWAT == 'Upload data') {

            if(input$which_file == 'Subbasin'){
                pickerInput(
                    inputId = "swat_var_sub",
                    label = "Select the Water quantity/quality metric of interest",
                    options = list(
                        `actions-box` = TRUE,
                        `header` = "Select metric ",
                        `windowPadding` = 1,
                        `width` = " css-width ",
                        `size` = 8
                    ),
                    choices =   c("PRECIPmm","SNOMELTmm","PETmm","ETmm","SWmm",
                                  "PERCmm","SURQmm","GW_Qmm","WYLDmm","SYLDt_ha","ORGNkg_ha",
                                  "ORGPkg_ha","NSURQkg_ha","SOLPkg_ha", "SEDPkg_ha",
                                  "LATQ_mm","LATNO3kg_ha","GWNO3kg_ha","CHOLAmic_L",
                                  "CBODUmg_L","DOXQmg_L","TNO3kg_ha","QTILEmm","TVAPkg_ha"),
                    selected = "SYLDt_ha",
                    multiple = F,
                    choicesOpt = list(content = stringr::str_trunc(c("PRECIPmm","SNOMELTmm","PETmm","ETmm","SWmm",
                                                                     "PERCmm","SURQmm","GW_Qmm","WYLDmm","SYLDt_ha","ORGNkg_ha",
                                                                     "ORGPkg_ha","NSURQkg_ha","SOLPkg_ha", "SEDPkg_ha",
                                                                     "LATQ_mm","LATNO3kg_ha","GWNO3kg_ha","CHOLAmic_L",
                                                                     "CBODUmg_L","DOXQmg_L","TNO3kg_ha","QTILEmm","TVAPkg_ha"), width = 35
                    ))

                )
            }else
                if(input$which_file == 'Reach'){
                    pickerInput(
                        inputId = "swat_var_rch",
                        label = "Select the Water quantity/quality metric of interest",
                        options = list(
                            `actions-box` = TRUE,
                            `header` = "Select metric ",
                            `windowPadding` = 1,
                            `width` = " css-width ",
                            `size` = 8
                        ),
                        choices =   c("FLOW_OUTcms","EVAPcms","TLOSScms","SED_OUTtons",
                                      "SEDCONCmg/L","ORGN_OUTkg","ORGP_OUTkg","NO3_OUTkg","NH4_OUTkg","NO2_OUTkg",
                                      "MINP_OUTkg","CHLA_OUTkg", "CBOD_OUTkg","DISOX_OUTkg","SOLPST_OUTmg","SORPST_OUTmg",
                                                                                         "REACTPSTmg","VOLPSTmg","SETTLPSTmg","SETTLPSTmg",
                                                                                         "DIFFUSEPSTmg","REACBEDPSTmg","BURYPSTmg","BED_PSTmg",
                                                                                         "BACTP_OUTct","BACTLP_OUTct","CMETAL#1kg","CMETAL#2kg","CMETAL#3kg"),
                        selected = colnames(SWAT_data()[c(4:8)]),
                        multiple = T,
                        choicesOpt = list(content = stringr::str_trunc(c("FLOW_OUTcms","EVAPcms","TLOSScms","SED_OUTtons",
                                                                         "SEDCONCmg/L","ORGN_OUTkg","ORGP_OUTkg","NO3_OUTkg","NH4_OUTkg","NO2_OUTkg",
                                                                         "MINP_OUTkg","CHLA_OUTkg", "CBOD_OUTkg","DISOX_OUTkg","SOLPST_OUTmg","SORPST_OUTmg",
                                                                         "REACTPSTmg","VOLPSTmg","SETTLPSTmg","SETTLPSTmg",
                                                                         "DIFFUSEPSTmg","REACBEDPSTmg","BURYPSTmg","BED_PSTmg",
                                                                         "BACTP_OUTct","BACTLP_OUTct","CMETAL#1kg","CMETAL#2kg","CMETAL#3kg"), width = 35
                        ))

                    )
                }else
                    if(input$which_file == 'HRU'){
                        pickerInput(
                            inputId = "swat_var_hru",
                            label = "Select the Water quantity/quality metric of interest",
                            options = list(
                                `actions-box` = TRUE,
                                `header` = "Select metric ",
                                `windowPadding` = 1,
                                `width` = " css-width ",
                                `size` = 8
                            ),
                            choices =   c("PRECIPmm","SNOFALLmm","SNOMELTmm","IRRmm","PETmm","ETmm",
                                          "SW_INITmm","SW_ENDmm","PERCmm","GW_RCHGmm","DA_RCHGmm",
                                          "REVAPmm","SA_IRRmm","DA_IRRmm","SA_STmm","DA_STmm","SURQ_GENmm",
                                          "SURQ_CNTmm","TLOSSmm","LATQGENmm","GW_Qmm","WYLDmm","DAILYCN",
                                          "TMP_AVdgC","TMP_MXdgC","TMP_MNdgC","SOL_TMPdgC","SOLARMJ_m2",
                                          "SYLDt_ha","USLEt_ha","N_APPkg_ha","P_APPkg_ha","NAUTOkg_ha",
                                          "PAUTOkg_ha","NGRZkg_ha","PGRZkg_ha","NCFRTkg_ha","PCFRTkg_ha",
                                          "NRAINkg_ha","NFIXkg_ha","F_MNkg_ha","A_MNkg_ha","A_SNkg_ha",
                                          "F_MPkg_ha","AO_LPkg_ha","L_APkg_ha", "A_SPkg_ha", "DNITkg_ha",
                                          "NUPkg_ha","PUPkg_ha","ORGNkg_ha","ORGPkg_ha","SEDPkg_ha","NSURQkg_ha",
                                          "NLATQkg_ha","NO3Lkg_ha","NO3GWkg_ha","SOLPkg_ha","P_GWkg_ha",
                                          "W_STRS","TMP_STRS","N_STRS","P_STRS","BIOMt_ha","LAI",
                                          "YLDt_ha","BACTPct","BACTLPct","SNOmm", 
                                          "CMUPkg_ha",  "CMTOTkg_ha", "QTILEmm","TNO3kg_ha",
                                          "LNO3kg_ha","GW_Q_Dmm","LATQCNTmm","TVAPkg_ha", "LULC", "MEAN_SLOPE",
                                          "AREA","SUBBASIN","LU_CODE"),
                            selected =  "SYLDt_ha",
                            multiple = F,
                            choicesOpt = list(content = stringr::str_trunc(c("PRECIPmm","SNOFALLmm","SNOMELTmm","IRRmm","PETmm","ETmm",
                                                                             "SW_INITmm","SW_ENDmm","PERCmm","GW_RCHGmm","DA_RCHGmm",
                                                                             "REVAPmm","SA_IRRmm","DA_IRRmm","SA_STmm","DA_STmm","SURQ_GENmm",
                                                                             "SURQ_CNTmm","TLOSSmm","LATQGENmm","GW_Qmm","WYLDmm","DAILYCN",
                                                                             "TMP_AVdgC","TMP_MXdgC","TMP_MNdgC","SOL_TMPdgC","SOLARMJ_m2",
                                                                             "SYLDt_ha","USLEt_ha","N_APPkg_ha","P_APPkg_ha","NAUTOkg_ha",
                                                                             "PAUTOkg_ha","NGRZkg_ha","PGRZkg_ha","NCFRTkg_ha","PCFRTkg_ha",
                                                                             "NRAINkg_ha","NFIXkg_ha","F_MNkg_ha","A_MNkg_ha","A_SNkg_ha",
                                                                             "F_MPkg_ha","AO_LPkg_ha","L_APkg_ha", "A_SPkg_ha", "DNITkg_ha",
                                                                             "NUPkg_ha","PUPkg_ha","ORGNkg_ha","ORGPkg_ha","SEDPkg_ha","NSURQkg_ha",
                                                                             "NLATQkg_ha","NO3Lkg_ha","NO3GWkg_ha","SOLPkg_ha","P_GWkg_ha",
                                                                             "W_STRS","TMP_STRS","N_STRS","P_STRS","BIOMt_ha","LAI",
                                                                             "YLDt_ha","BACTPct","BACTLPct","SNOmm",
                                                                             "CMUPkg_ha",  "CMTOTkg_ha", "QTILEmm","TNO3kg_ha",
                                                                             "LNO3kg_ha","GW_Q_Dmm","LATQCNTmm","TVAPkg_ha", "LULC", "MEAN_SLOPE",
                                                                             "AREA","SUBBASIN","LU_CODE"), width = 35
                            ))

                        )

                    }
        } else
            if (input$DefOrUserUpload_SWAT == 'Default_Data_WE38') {
                req(input$which_file)
                if(input$which_file == 'Subbasin'){
                    pickerInput(
                        inputId = "swat_var_sub",
                        label = "Select the Water quantity/quality metric of interest",
                        options = list(
                            `actions-box` = TRUE,
                            `header` = "Select metric ",
                            `windowPadding` = 1,
                            `width` = " css-width ",
                            `size` = 8
                        ),
                        choices =   c("PRECIPmm","SNOMELTmm","PETmm","ETmm","SWmm",
                                      "PERCmm","SURQmm","GW_Qmm","WYLDmm","SYLDt_ha","ORGNkg_ha",
                                      "ORGPkg_ha","NSURQkg_ha","SOLPkg_ha", "SEDPkg_ha",
                                      "LATQ_mm","LATNO3kg_ha","GWNO3kg_ha","CHOLAmic_L",
                                      "CBODUmg_L","DOXQmg_L","TNO3kg_ha","QTILEmm","TVAPkg_ha"),
                        selected = "SYLDt_ha",
                        multiple = F,
                        choicesOpt = list(content = stringr::str_trunc(c("PRECIPmm","SNOMELTmm","PETmm","ETmm","SWmm",
                                                                         "PERCmm","SURQmm","GW_Qmm","WYLDmm","SYLDt_ha","ORGNkg_ha",
                                                                         "ORGPkg_ha","NSURQkg_ha","SOLPkg_ha", "SEDPkg_ha",
                                                                         "LATQ_mm","LATNO3kg_ha","GWNO3kg_ha","CHOLAmic_L",
                                                                         "CBODUmg_L","DOXQmg_L","TNO3kg_ha","QTILEmm","TVAPkg_ha"), width = 35
                        ))

                    )
                }else
                    if(input$which_file == 'Reach'){
                        pickerInput(
                            inputId = "swat_var_rch",
                            label = "Select the Water quantity/quality metric of interest",
                            options = list(
                                `actions-box` = TRUE,
                                `header` = "Select metric ",
                                `windowPadding` = 1,
                                `width` = " css-width ",
                                `size` = 8
                            ),
                            choices =   c("FLOW_OUTcms","EVAPcms","TLOSScms","SED_OUTtons",
                                          "SEDCONCmg/L","ORGN_OUTkg","ORGP_OUTkg","NO3_OUTkg","NH4_OUTkg","NO2_OUTkg",
                                          "MINP_OUTkg","CHLA_OUTkg", "CBOD_OUTkg","DISOX_OUTkg","SOLPST_OUTmg","SORPST_OUTmg",
                                          "REACTPSTmg","VOLPSTmg","SETTLPSTmg","SETTLPSTmg",
                                          "DIFFUSEPSTmg","REACBEDPSTmg","BURYPSTmg","BED_PSTmg",
                                          "BACTP_OUTct","BACTLP_OUTct","CMETAL#1kg","CMETAL#2kg","CMETAL#3kg"),
                            selected = colnames(SWAT_data()[c(4:8)]),
                            multiple = T,
                            choicesOpt = list(content = stringr::str_trunc(c("FLOW_OUTcms","EVAPcms","TLOSScms","SED_OUTtons",
                                                                             "SEDCONCmg/L","ORGN_OUTkg","ORGP_OUTkg","NO3_OUTkg","NH4_OUTkg","NO2_OUTkg",
                                                                             "MINP_OUTkg","CHLA_OUTkg", "CBOD_OUTkg","DISOX_OUTkg","SOLPST_OUTmg","SORPST_OUTmg",
                                                                             "REACTPSTmg","VOLPSTmg","SETTLPSTmg","SETTLPSTmg",
                                                                             "DIFFUSEPSTmg","REACBEDPSTmg","BURYPSTmg","BED_PSTmg",
                                                                             "BACTP_OUTct","BACTLP_OUTct","CMETAL#1kg","CMETAL#2kg","CMETAL#3kg"), width = 35
                            ))

                        )
                    }else
                        if(input$which_file == 'HRU'){
                            pickerInput(
                                inputId = "swat_var_hru",
                                label = "Select the Water quantity/quality metric of interest",
                                options = list(
                                    `actions-box` = TRUE,
                                    `header` = "Select metric ",
                                    `windowPadding` = 1,
                                    `width` = " css-width ",
                                    `size` = 8
                                ),
                                choices =   c("PRECIPmm","SNOFALLmm","SNOMELTmm","IRRmm","PETmm","ETmm",
                                              "SW_INITmm","SW_ENDmm","PERCmm","GW_RCHGmm","DA_RCHGmm",
                                              "REVAPmm","SA_IRRmm","DA_IRRmm","SA_STmm","DA_STmm","SURQ_GENmm",
                                              "SURQ_CNTmm","TLOSSmm","LATQGENmm","GW_Qmm","WYLDmm","DAILYCN",
                                              "TMP_AVdgC","TMP_MXdgC","TMP_MNdgC","SOL_TMPdgC","SOLARMJ_m2",
                                              "SYLDt_ha","USLEt_ha","N_APPkg_ha","P_APPkg_ha","NAUTOkg_ha",
                                              "PAUTOkg_ha","NGRZkg_ha","PGRZkg_ha","NCFRTkg_ha","PCFRTkg_ha",
                                              "NRAINkg_ha","NFIXkg_ha","F_MNkg_ha","A_MNkg_ha","A_SNkg_ha",
                                              "F_MPkg_ha","AO_LPkg_ha","L_APkg_ha", "A_SPkg_ha", "DNITkg_ha",
                                              "NUPkg_ha","PUPkg_ha","ORGNkg_ha","ORGPkg_ha","SEDPkg_ha","NSURQkg_ha",
                                              "NLATQkg_ha","NO3Lkg_ha","NO3GWkg_ha","SOLPkg_ha","P_GWkg_ha",
                                              "W_STRS","TMP_STRS","N_STRS","P_STRS","BIOMt_ha","LAI",
                                              "YLDt_ha","BACTPct","BACTLPct","SNOmm",
                                              "CMUPkg_ha",  "CMTOTkg_ha", "QTILEmm","TNO3kg_ha",
                                              "LNO3kg_ha","GW_Q_Dmm","LATQCNTmm","TVAPkg_ha", "LULC", "MEAN_SLOPE",
                                              "AREA","SUBBASIN","LU_CODE"),
                                selected =  "SYLDt_ha",
                                multiple = F,
                                choicesOpt = list(content = stringr::str_trunc(c("PRECIPmm","SNOFALLmm","SNOMELTmm","IRRmm","PETmm","ETmm",
                                                                                 "SW_INITmm","SW_ENDmm","PERCmm","GW_RCHGmm","DA_RCHGmm",
                                                                                 "REVAPmm","SA_IRRmm","DA_IRRmm","SA_STmm","DA_STmm","SURQ_GENmm",
                                                                                 "SURQ_CNTmm","TLOSSmm","LATQGENmm","GW_Qmm","WYLDmm","DAILYCN",
                                                                                 "TMP_AVdgC","TMP_MXdgC","TMP_MNdgC","SOL_TMPdgC","SOLARMJ_m2",
                                                                                 "SYLDt_ha","USLEt_ha","N_APPkg_ha","P_APPkg_ha","NAUTOkg_ha",
                                                                                 "PAUTOkg_ha","NGRZkg_ha","PGRZkg_ha","NCFRTkg_ha","PCFRTkg_ha",
                                                                                 "NRAINkg_ha","NFIXkg_ha","F_MNkg_ha","A_MNkg_ha","A_SNkg_ha",
                                                                                 "F_MPkg_ha","AO_LPkg_ha","L_APkg_ha", "A_SPkg_ha", "DNITkg_ha",
                                                                                 "NUPkg_ha","PUPkg_ha","ORGNkg_ha","ORGPkg_ha","SEDPkg_ha","NSURQkg_ha",
                                                                                 "NLATQkg_ha","NO3Lkg_ha","NO3GWkg_ha","SOLPkg_ha","P_GWkg_ha",
                                                                                 "W_STRS","TMP_STRS","N_STRS","P_STRS","BIOMt_ha","LAI",
                                                                                 "YLDt_ha","BACTPct","BACTLPct","SNOmm",
                                                                                 "CMUPkg_ha",  "CMTOTkg_ha", "QTILEmm","TNO3kg_ha",
                                                                                 "LNO3kg_ha","GW_Q_Dmm","LATQCNTmm","TVAPkg_ha", "LULC", "MEAN_SLOPE",
                                                                                 "AREA","SUBBASIN","LU_CODE"), width = 35
                                ))

                            )
                        }
            }
    })
    
    
    
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------Dataframe Calculations for Reach df SWAT-------------------------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    
    
    reachdf <- reactive({
        req(input$DefOrUserUpload_SWAT)
        if (input$DefOrUserUpload_SWAT == 'Upload data') {
            req(SWAT_data())
            req(input$which_file)
            if(input$which_file == 'Subbasin'){

            }else
                if(input$which_file == 'Reach'){
                    SWAT_data()


                }else
                    if(input$which_file == 'HRU'){

                    }
        } else
            if (input$DefOrUserUpload_SWAT == 'Default_Data_WE38') {
                req(input$which_file)
                if(input$which_file == 'Subbasin'){

                }else
                    if(input$which_file == 'Reach'){
                        SWAT_data()

                    }else
                        if(input$which_file == 'HRU'){

                        }
            }
    })
    
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------Dataframe Calculations for subwatersheds df SWAT-------------------------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    
    subdf <- reactive({
        req(input$DefOrUserUpload_SWAT)
        if (input$DefOrUserUpload_SWAT == 'Upload data') {
            req(SWAT_data())
            req(input$which_file)
            if(input$which_file == 'Subbasin'){
                SWAT_data()
            }else
                if(input$which_file == 'Reach'){



                }else
                    if(input$which_file == 'HRU'){

                    }
        } else
            if (input$DefOrUserUpload_SWAT == 'Default_Data_WE38') {
                req(input$which_file)
                if(input$which_file == 'Subbasin'){
                    SWAT_data()
                }else
                    if(input$which_file == 'Reach'){

                       }else
                        if(input$which_file == 'HRU'){

                        }
            }
    })
    
    SWATSub_subset_base <- reactive({
        req(subdf())
        req(input$SWAT_wshed)
        req(input$SWAT_scen_sub_base)
        req(input$swat_var_sub)
        # req(input$SWAT_subnum)
        subdf() %>%
            dplyr::filter(Watershed %in% input$SWAT_wshed &
                              Scenario %in% input$SWAT_scen_sub_base)%>% 
            arrange_at(.vars = input$swat_var_sub, desc) %>% ungroup() %>%
            dplyr::mutate_if(is.numeric, round, 2) 
        })
    
    ### Cool approch to be implemented some other time.
    ### !!paste0("AbsChange_", input$swat_var_sub) := 
    #                       input$swat_var_sub - input$swat_var_sub[Scenario == input$SWAT_scen_sub_base]) %>% ungroup()
    
    SWATSub_data_rel<- reactive({
        req(input$SWAT_wshed)
        subdf() %>%
            dplyr::filter(Watershed %in% input$SWAT_wshed)%>%
            dplyr::group_by(SUB) %>%
            dplyr::mutate(AbsChange_PRECIPmm = PRECIPmm - PRECIPmm[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_SNOMELTmm = SNOMELTmm - SNOMELTmm[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_PETmm = PETmm - PETmm[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_ETmm = ETmm - ETmm[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_SWmm = SWmm - SWmm[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_PERCmm = PERCmm - PERCmm[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_SURQmm = SURQmm - SURQmm[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_GW_Qmm = GW_Qmm - GW_Qmm[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_WYLDmm = WYLDmm - WYLDmm[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_SYLDt_ha = SYLDt_ha - SYLDt_ha[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_ORGNkg_ha = ORGNkg_ha - ORGNkg_ha[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_ORGPkg_ha = ORGPkg_ha - ORGPkg_ha[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_NSURQkg_ha = NSURQkg_ha - NSURQkg_ha[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_SOLPkg_ha = SOLPkg_ha - SOLPkg_ha[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_SEDPkg_ha = SEDPkg_ha - SEDPkg_ha[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_LATQ_mm = LATQ_mm - LATQ_mm[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_LATNO3kg_ha = LATNO3kg_ha - LATNO3kg_ha[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_GWNO3kg_ha = GWNO3kg_ha - GWNO3kg_ha[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_CHOLAmic_L = CHOLAmic_L - CHOLAmic_L[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_CBODUmg_L = CBODUmg_L - CBODUmg_L[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_DOXQmg_L = DOXQmg_L - DOXQmg_L[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_TNO3kg_ha = TNO3kg_ha - TNO3kg_ha[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_QTILEmm = QTILEmm - QTILEmm[Scenario == input$SWAT_scen_sub_base],
                          AbsChange_TVAPkg_ha = TVAPkg_ha - TVAPkg_ha[Scenario == input$SWAT_scen_sub_base])%>%
            ungroup()
            
    })
    
    SWATSub_data_comp <- reactive({
        req(input$SWAT_wshed)
        req(input$SWAT_scen_sub_comp)
        req(SWATSub_data_rel())
        req(input$SWAT_wshed)
        SWATSub_data_rel() %>%
            dplyr::filter(Watershed %in% input$SWAT_wshed &
                              Scenario %in% input$SWAT_scen_sub_comp)%>%
            arrange_at(.vars = input$swat_var_sub, desc) %>% ungroup()%>%
            dplyr::mutate_if(is.numeric, round, 2)
            })
    
    swat_sub_table <- reactive({
        req(SWATSub_data_comp())
        req(input$swat_var_sub)
        
        SWATSub_data_comp() %>% as.data.frame() %>%
            dplyr::select(-geometry) %>%
            dplyr::select(SUB,
                          input$swat_var_sub,
                          paste0("AbsChange_",input$swat_var_sub))%>% dplyr::filter(SUB %in% input$SWAT_subnum)
    })
    
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------Dataframe Calculations for HRU df SWAT-------------------------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    hrudf <- reactive({
        req(input$DefOrUserUpload_SWAT)
        if (input$DefOrUserUpload_SWAT == 'Upload data') {
            req(SWAT_data())
            req(input$which_file)
            if(input$which_file == 'Subbasin'){
                
            }else
                if(input$which_file == 'Reach'){
                }else
                    if(input$which_file == 'HRU'){
                        SWAT_data()
                    }
        } else
            if (input$DefOrUserUpload_SWAT == 'Default_Data_WE38') {
                req(input$which_file)
                if(input$which_file == 'Subbasin'){
                    
                }else
                    if(input$which_file == 'Reach'){
                        
                    }else
                        if(input$which_file == 'HRU'){
                            SWAT_data()
                        }
            }
    })
    
    
    SWATHru_subset_base <- reactive({
        req(hrudf())
        req(input$SWAT_wshed)
        req(input$SWAT_scen_hru_base)
        req(input$swat_var_hru)
        hrudf() %>%
            dplyr::filter(Watershed %in% input$SWAT_wshed &
                              Scenario %in% input$SWAT_scen_hru_base)%>% 
            arrange_at(.vars = input$swat_var_hru, desc) %>% ungroup() %>%
            dplyr::mutate_if(is.numeric, round, 2) %>%
            dplyr::filter(SUBBASIN %in% input$SWAT_hru_sub)%>%
            dplyr::filter(LULC %in% input$SWAT_hru_lulc)%>%
            dplyr::filter(MEAN_SLOPE >= min(input$swat_thresh_slp) & MEAN_SLOPE <= max(input$swat_thresh_slp) )
        
        
    })
    
    SWATHru_data_rel<- reactive({ 
        req(hrudf())
        req(input$SWAT_wshed)
        req(input$SWAT_scen_hru_base)    
        hrudf() %>%
            dplyr::filter(Watershed %in% input$SWAT_wshed)%>%
            dplyr::group_by(GIS) %>%
            dplyr::mutate(AbsChange_PRECIPmm = PRECIPmm - PRECIPmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_SNOFALLmm = SNOFALLmm - SNOFALLmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_SNOMELTmm = SNOMELTmm - SNOMELTmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_IRRmm = IRRmm - IRRmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_PETmm = PETmm - PETmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_ETmm = ETmm - ETmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_SW_INITmm = SW_INITmm - SW_INITmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_SW_ENDmm = SW_ENDmm - SW_ENDmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_PERCmm = PERCmm - PERCmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_GW_RCHGmm = GW_RCHGmm - GW_RCHGmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_DA_RCHGmm = DA_RCHGmm - DA_RCHGmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_REVAPmm = REVAPmm - REVAPmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_SA_IRRmm = SA_IRRmm - SA_IRRmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_DA_IRRmm = DA_IRRmm - DA_IRRmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_SA_STmm = SA_STmm - SA_STmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_DA_STmm = DA_STmm - DA_STmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_SURQ_GENmm = SURQ_GENmm - SURQ_GENmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_SURQ_CNTmm = SURQ_CNTmm - SURQ_CNTmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_TLOSSmm = TLOSSmm - TLOSSmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_LATQGENmm = LATQGENmm - LATQGENmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_GW_Qmm = GW_Qmm - GW_Qmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_WYLDmm = WYLDmm - WYLDmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_DAILYCN = DAILYCN - DAILYCN[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_TMP_AVdgC = TMP_AVdgC - TMP_AVdgC[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_TMP_MXdgC = TMP_MXdgC - TMP_MXdgC[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_TMP_MNdgC = TMP_MNdgC - TMP_MNdgC[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_SOL_TMPdgC = SOL_TMPdgC - SOL_TMPdgC[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_SOLARMJ_m2 = SOLARMJ_m2 - SOLARMJ_m2[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_SYLDt_ha = SYLDt_ha - SYLDt_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_USLEt_ha = USLEt_ha - USLEt_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_N_APPkg_ha = N_APPkg_ha - N_APPkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_P_APPkg_ha = P_APPkg_ha - P_APPkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_NAUTOkg_ha = NAUTOkg_ha - NAUTOkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_PAUTOkg_ha = PAUTOkg_ha - PAUTOkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_NGRZkg_ha = NGRZkg_ha - NGRZkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_PGRZkg_ha = PGRZkg_ha - PGRZkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_NCFRTkg_ha = NCFRTkg_ha - NCFRTkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_PCFRTkg_ha = PCFRTkg_ha - PCFRTkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_NRAINkg_ha = NRAINkg_ha - NRAINkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_NFIXkg_ha = NFIXkg_ha - NFIXkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_F_MPkg_ha = F_MPkg_ha - F_MPkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_AO_LPkg_ha = AO_LPkg_ha - AO_LPkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_L_APkg_ha = L_APkg_ha - L_APkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_A_SPkg_ha = A_SPkg_ha - A_SPkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_DNITkg_ha = DNITkg_ha - DNITkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_NUPkg_ha = NUPkg_ha - NUPkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_PUPkg_ha = PUPkg_ha - PUPkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_ORGNkg_ha= ORGNkg_ha - ORGNkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_ORGPkg_ha = ORGPkg_ha - ORGPkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_SEDPkg_ha = SEDPkg_ha - SEDPkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_NSURQkg_ha = NSURQkg_ha - NSURQkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_NLATQkg_ha = NLATQkg_ha - NLATQkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_NO3Lkg_ha = NO3Lkg_ha - NO3Lkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_NO3GWkg_ha = NO3GWkg_ha - NO3GWkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_SOLPkg_ha = SOLPkg_ha - SOLPkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_P_GWkg_ha = P_GWkg_ha - P_GWkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_W_STRS = W_STRS - W_STRS[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_TMP_STRS = TMP_STRS - TMP_STRS[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_N_STRS = N_STRS - N_STRS[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_P_STRS = P_STRS - P_STRS[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_BIOMt_ha = BIOMt_ha - BIOMt_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_LAI = LAI - LAI[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_GW_Qmm = GW_Qmm - GW_Qmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_WYLDmm = WYLDmm - WYLDmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_YLDt_ha = YLDt_ha - YLDt_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_BACTPct = BACTPct - BACTPct[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_BACTLPct = BACTLPct - BACTLPct[Scenario == input$SWAT_scen_hru_base],
                          # AbsChange_unknown1 = unknown1 - unknown1[Scenario == input$SWAT_scen_hru_base],
                          # AbsChange_unknown2 = unknown2 - unknown2[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_SNOmm = SNOmm - SNOmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_CMUPkg_ha = CMUPkg_ha - CMUPkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_CMTOTkg_ha = CMTOTkg_ha - CMTOTkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_QTILEmm = QTILEmm - QTILEmm[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_TNO3kg_ha = TNO3kg_ha - TNO3kg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_NAUTOkg_ha = NAUTOkg_ha - NAUTOkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_LNO3kg_ha = LNO3kg_ha - LNO3kg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_GW_Q_Dmm = NGRZkg_ha - NGRZkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_LATQCNTmm = PGRZkg_ha - PGRZkg_ha[Scenario == input$SWAT_scen_hru_base],
                          AbsChange_TVAPkg_ha = NCFRTkg_ha - NCFRTkg_ha[Scenario == input$SWAT_scen_hru_base],
                          )%>%ungroup()%>%
            dplyr::mutate_if(is.numeric, round, 2)
        
    })
    
    
    SWATHru_data_comp <- reactive({
        req(SWATHru_data_rel())
        SWATHru_data_rel() %>%
            dplyr::filter(Watershed %in% input$SWAT_wshed &
                              Scenario %in% input$SWAT_scen_hru_comp)%>%
            dplyr::filter(SUBBASIN %in% input$SWAT_hru_sub)%>%
            dplyr::filter(LULC %in% input$SWAT_hru_lulc)%>%
            dplyr::filter(MEAN_SLOPE >= min(input$swat_thresh_slp) & MEAN_SLOPE <= max(input$swat_thresh_slp) )%>%
            arrange_at(.vars = input$swat_var_hru, desc)
    })
    
    
    swat_hru_table <- reactive({
        req(SWATHru_data_comp())
        
        SWATHru_data_comp() %>% as.data.frame() %>%
            dplyr::select(-geometry) %>%
            dplyr::select(GIS, LULC, SUBBASIN, LU_CODE, SOIL_CODE, MEAN_SLOPE, AREA,
                          input$swat_var_hru,
                          paste0("AbsChange_",input$swat_var_hru))
    })
    
    
    
    
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------TAble output subwatersheds df SWAT----------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    
    
    output$tablesub <- DT::renderDataTable(
        swat_sub_table(),
        extensions = list("Buttons" = NULL, 'Scroller' = NULL),
        options = list(
            deferRender = TRUE,
            autoWidth = TRUE,
            scrollY = 350,
            scrollX = 200,
            scroller = TRUE,
            dom = 'BRSfrti',
            buttons =
                list(
                    'copy',
                    'print',
                    list(
                        extend = 'collection',
                        buttons = c('csv', 'excel', 'pdf'),
                        text = 'Download'
                    )
                ),
            # pageLength = 5,
            fixedHeader = TRUE,
            fillContainer = F,
            class = "display",
            columnDefs = list(list(className = 'dt-left'))
        ),
        rownames = FALSE 
        
    )
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------TAble output HRU df SWAT-------------------------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    
    
    output$tablehru <- DT::renderDataTable(
        swat_hru_table(),
        extensions = list("Buttons" = NULL, 'Scroller' = NULL),
        options = list(
            deferRender = TRUE,
            autoWidth = TRUE,
            scrollY = 350,
            scrollX = 200,
            scroller = TRUE,
            dom = 'BRSfrti',
            buttons =
                list(
                    'copy',
                    'print',
                    list(
                        extend = 'collection',
                        buttons = c('csv', 'excel', 'pdf'),
                        text = 'Download'
                    )
                ),
            # pageLength = 5,
            fixedHeader = TRUE,
            fillContainer = F,
            class = "display",
            columnDefs = list(list(className = 'dt-left'))
        ),
        rownames = FALSE  
    )
    
    # output$testtab2 <- renderTable({
    #     reach_subset()
    # })
    
    ## -----------------------------------------------------------------------------------------------------------##
    ################# Filtering logic for Reach DF ################
    ## -----------------------------------------------------------------------------------------------------------##    
    reach_subset <- reactive({
        req(reachdf())
        req(input$SWAT_wshed)
        req(input$SWAT_reachnum)
        req(input$AreaVsScen_swat)
        if (input$AreaVsScen_swat == 'allscen_swat') {
            reachdf() %>% dplyr::filter(Watershed %in% input$SWAT_wshed) %>%
                dplyr::filter(RCH %in% input$SWAT_reachnum)
        } else
            if (input$AreaVsScen_swat == 'allwat_swat') {
                reachdf() %>%
                    dplyr::filter(RCH %in% input$SWAT_reachnum)%>%
                    dplyr::filter(Watershed %in% input$SWAT_wshed & Scenario %in% input$SWAT_scen_rch)
            }
    })
    
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------Plots:Reach data SWAT-------------------------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    
    output$Plotheatrch <- renderPlotly({
        req(reach_subset())
        req(input$AreaVsScen_swat)
        req(input$swat_var_rch)


        reach_subset <- reach_subset()
        if (input$AreaVsScen_swat == 'allscen_swat') {
            d <-
                reach_subset() %>% dplyr::select(Scenario, input$swat_var_rch) %>%
                dplyr::mutate_if(is.numeric, scale)
            d.m <- reshape2::melt(d)

            p <-
                plot_ly(
                    x = d.m$Scenario,
                    y = d.m$variable,
                    z = d.m$value,
                    type = "heatmap"
                ) %>%
                layout(
                    xaxis = list(
                        tickfont = list(size = 12),
                        tickangle = 45
                    ),
                    margin = list(
                        l = 150,
                        r = 20,
                        b = 150,
                        t = 100,
                        pad = 4
                    ),
                    title = list(
                        text = paste0(
                            '<b>Relative impacts of management and disturbance on the watershed</b>',
                            '<br>',
                            '<sup>',
                            '<i>Plot displays the relative impact of the all disturbances on the delivered\nwater quantity/quality metric at the watershed outlet.</i>',
                            '<br><br>',
                            '</sup>'
                        )
                    )
                )
            p


        } else
            if (input$AreaVsScen_swat == 'allwat_swat') {

                if (length(unique(reach_subset()$RCH))<2) {
                    d <-
                        reach_subset() %>% dplyr::select(RCH, input$swat_var_rch) %>% dplyr::mutate_at(vars(RCH), as.factor)
                }else
                    if (length(unique(reach_subset()$RCH))>=2) {
                d <-
                    reach_subset() %>% dplyr::select(RCH, input$swat_var_rch) %>% 
                    dplyr::mutate_at(vars(RCH), as.factor)%>% 
                    dplyr::mutate_if(is.numeric, scale)}

                d.m <- reshape2::melt(d)


                p <-
                    plot_ly(
                        x = d.m$RCH,
                        y = d.m$variable,
                        z = d.m$value,
                        type = "heatmap"
                    ) %>%
                    layout(
                        xaxis = list(
                            tickfont = list(size = 12),
                            tickangle = 45
                        ),
                        margin = list(
                            l = 150,
                            r = 20,
                            b = 150,
                            t = 100,
                            pad = 4
                        ),
                        title = list(
                            text = paste0(
                                '<b>Relative impacts of the disturbance scenario across all watersheds </b>',
                                '<br>',
                                '<sup>',
                                '<i>Plot displays the relative impact of the disturbance on the delivered particular\nwater quantity/quality metric at each of the watersheds</i>',
                                '<br><br>',
                                '</sup>'
                            )
                        )
                    )

                p
            }

    })
    
    
    output$Plotbarrch <- renderPlotly({
        req(reach_subset())
        req(input$AreaVsScen_swat)
        req(input$swat_var_rch)


        reach_subset <- reach_subset()
        if (input$AreaVsScen_swat == 'allscen_swat') {

            d <-  reach_subset() %>% dplyr::select(Scenario, input$swat_var_rch)

                    d.m <- reshape2::melt(d)

                    ## Calculates percent contribution of each variable across all
                    ## the simulated scenarios
                    d.m <- d.m %>%
                        group_by(variable) %>%
                        mutate(total = sum(value),
                               share = (value / total) * 100) %>% dplyr::select(-total) %>%
                        ungroup()

                    b <- ggplot(d.m) +

                        geom_bar(
                            aes(
                                y = share,
                                x = variable,
                                fill = reorder(Scenario, share)

                            ),
                            stat = "identity",
                            position = "dodge"
                        ) + guides(fill = FALSE)+
                        theme_bw(base_rect_size = 0.1) +
                        theme(
                            axis.text.x = element_text(
                                angle = 45,
                                size= 13,
                                vjust = ,
                                colour = "Black"
                            ),
                            axis.text.y = element_text(colour = "Black", size =12),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            legend.title = element_blank(),
                            legend.position = "right"
                        ) + coord_flip() + labs(y = "Percent of total across all Scenarios", x = "") +
                        scale_fill_brewer(palette = "RdYlGn", direction = 1) +
                        scale_y_continuous(
                            labels = function(x)
                                paste0(x * 1, "%")
                        )

                    ggplotly(b) %>%
                        layout(
                            title = list(
                                text = paste0(
                                    '<b>Relative impacts of management and disturbance on the watershed</b>',
                                    '<br>',
                                    '<sup>',
                                    '<i>Plot displays the relative impact of the all disturbances on the delivered\nwater quantity/quality metric at the watershed outlet.</i>',
                                    '<br><br>',
                                    '</sup>'
                                )
                            ),
                            margin = list(
                                l = 10,
                                r = 20,
                                b = 5,
                                t = 150,
                                pad = 0
                            ),
                            legend = list(
                                orientation = "v",
                                title=list(text='<b> Scenario </b>'),
                                font = list(size = 12)
                            )
                        )


                }else
                    if (input$AreaVsScen_swat == 'allwat_swat') {

                        d <-  reach_subset() %>% dplyr::select(RCH, input$swat_var_rch)%>%
                            dplyr::mutate_at(vars(RCH), as.factor)

                        d.m <- reshape2::melt(d)

                        ## Calculates percent contribution of each variable across all
                        ## the simulated scenarios
                        d.m <- d.m %>%
                            group_by(variable) %>%
                            mutate(total = sum(value),
                                   share = (value / total) * 100) %>%
                            ungroup()

                        b <- ggplot(d.m) +

                            geom_bar(
                                aes(
                                    y = share,
                                    x = variable,
                                    fill = reorder(RCH,share)
                                ),
                                stat = "identity",
                                position = "dodge"
                            ) +guides(fill = FALSE)  +
                            theme_bw(base_rect_size = 0.1) +
                            theme(
                                axis.text.x = element_text(
                                    angle = 45,
                                    size=13,
                                    vjust = ,
                                    colour = "Black"
                                ),
                                axis.text.y = element_text(colour = "Black", size =12),
                                axis.title.x = element_blank(),
                                axis.title.y = element_blank(),
                                legend.title = element_blank(),
                                legend.position = "right"
                            ) + labs(y = "Percent of total across all Watersheds", x = "") +
                            coord_flip() +
                            scale_fill_brewer(palette = "RdYlGn", direction = 1) +
                            scale_y_continuous(
                                labels = function(x)
                                    paste0(x * 1, "%")
                            )
                        ggplotly(b)  %>%
                            layout(
                                title = list(
                                    text = paste0(
                                        '<b>Relative impacts of the disturbance scenario across all watersheds </b>',
                                        '<br>',
                                        '<sup>',
                                        '<i>Plot displays the relative impact of the disturbance on the delivered particular\nwater quantity/quality metric at each of the watersheds</i>',
                                        '</sup>'
                                    )
                                ),
                                margin = list(
                                    l = 10,
                                    r = 20,
                                    b = 5,
                                    t = 150,
                                    pad = 0
                                ),
                                legend = list(
                                    orientation = "v",
                                    title=list(text='<b> Reach </b>'),
                                    font = list(size = 12)
                                )
                            )


                    }

    })
    
    
    
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------Dataframe Calculations for hillslopes-------------------------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    
    ################# Filtering logic for HILLSLOPE DF#################
    
    hill_subset <- reactive({
        req(Hill_data())
        Hill_data() %>%
            dplyr::filter(Watershed %in% input$Hill_wshed)
    })
    
    
    ############## Takes in df filtered by input watershed and creates column for each variable with values   ##############
    ##############            relative to the chosen baseline scenario   ##############
    
    
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
                cumLateralflow.mm = cumsum(Lateral.Flow..mm.) / sum(Lateral.Flow..mm.) *
                    100,
                cumBaseflow.mm = cumsum(Baseflow..mm.) / sum(Baseflow..mm.) *
                    100,
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
                cumSediment.Yield.of.Particles.Under.0.016.mm..kg = cumsum(Sediment_Yield_of_Particles_Under_0.016_mm_kg) /
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
                cumLateralflow.mm = cumsum(Lateral.Flow..mm.) / sum(Lateral.Flow..mm.) *
                    100,
                cumBaseflow.mm = cumsum(Baseflow..mm.) / sum(Baseflow..mm.) *
                    100,
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
                cumSediment.Yield.of.Particles.Under.0.016.mm..kg = cumsum(Sediment_Yield_of_Particles_Under_0.016_mm_kg) /
                    sum(Sediment_Yield_of_Particles_Under_0.016_mm_kg) * 100
            ) %>% dplyr::filter(Scenario %in% c(input$Hill_scen_base,  input$Hill_scen_comp)) %>% dplyr::filter(cumPercLen < input$thresh_H) %>%
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
                cumLateralflow.mm = cumsum(Lateral.Flow..mm.),
                cumBaseflow.mm = cumsum(Baseflow..mm.),
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
                cumSediment.Yield.of.Particles.Under.0.016.mm..kg = cumsum(Sediment_Yield_of_Particles_Under_0.016_mm_kg)
            ) %>% dplyr::filter(Scenario %in% c(input$Hill_scen_base,  input$Hill_scen_comp)) %>% dplyr::filter(cumPercArea < input$thresh_H) %>%
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
                cumLateralflow.mm = cumsum(Lateral.Flow..mm.),
                cumBaseflow.mm = cumsum(Baseflow..mm.),
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
                cumSediment.Yield.of.Particles.Under.0.016.mm..kg = cumsum(Sediment_Yield_of_Particles_Under_0.016_mm_kg)
            ) %>% dplyr::filter(Scenario %in% c(input$Hill_scen_base,  input$Hill_scen_comp)) %>% dplyr::filter(cumPercLen < input$thresh_H) %>%
            ungroup()
    })
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------Table Hillslopes tab-------------------------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    
    sed_stats_df <- reactive({
        if (input$summary_DT_by_var_H == "Landuse") {
            hill_subset_rel() %>% dplyr::filter(Scenario %in% input$Hill_scen_comp) %>%
                dplyr::arrange_at(.vars = input$Hill_variable, desc) %>%
                dplyr::mutate(cumPercArea = cumsum(Hillslope.Area..ha.) /
                                  sum(Hillslope.Area..ha.) * 100) %>%
                dplyr::filter(cumPercArea < input$thresh_H) %>%
                dplyr::select(
                    LanduseDesc,
                    Slope,
                    input$Hill_variable ,
                    paste0("AbsChange_", input$Hill_variable)
                ) %>%
                group_by(LanduseDesc) %>% dplyr::summarise_if(is.numeric, list(mean =
                                                                                   mean)) %>%
                dplyr::arrange(desc(paste0(
                    input$Hill_variable, "_mean"
                ))) %>%
                dplyr::mutate_if(is.numeric, round, 2)
        } else
            if (input$summary_DT_by_var_H == "Soiltype") {
                hill_subset_rel() %>% dplyr::filter(Scenario %in% input$Hill_scen_comp) %>%
                    dplyr::arrange_at(.vars = input$Hill_variable, desc) %>%
                    dplyr::mutate(cumPercArea = cumsum(Hillslope.Area..ha.) /
                                      sum(Hillslope.Area..ha.) * 100) %>%
                    dplyr::filter(cumPercArea < input$thresh_H) %>%
                    dplyr::select(
                        SoilDesc,
                        Slope,
                        input$Hill_variable ,
                        paste0("AbsChange_", input$Hill_variable)
                    ) %>%
                    group_by(SoilDesc) %>% dplyr::summarise_if(is.numeric, list(mean =
                                                                                    mean)) %>%
                    dplyr::arrange(desc(paste0(
                        input$Hill_variable, "_mean"
                    ))) %>%
                    dplyr::mutate_if(is.numeric, round, 2)
                
            } else
                if (input$summary_DT_by_var_H == "Both") {
                    hill_subset_rel() %>% dplyr::filter(Scenario %in% input$Hill_scen_comp) %>%
                        dplyr::arrange_at(.vars = input$Hill_variable, desc) %>%
                        dplyr::mutate(cumPercArea = cumsum(Hillslope.Area..ha.) /
                                          sum(Hillslope.Area..ha.) * 100) %>%
                        dplyr::filter(cumPercArea < input$thresh_H) %>%
                        dplyr::select(
                            SoilDesc,
                            LanduseDesc,
                            Slope,
                            input$Hill_variable ,
                            paste0("AbsChange_", input$Hill_variable)
                        ) %>%
                        group_by(SoilDesc, LanduseDesc) %>% dplyr::summarise_if(is.numeric, list(mean =
                                                                                                     mean)) %>%
                        dplyr::arrange(desc(paste0(
                            input$Hill_variable, "_mean"
                        ))) %>%
                        dplyr::mutate_if(is.numeric, round, 2)
                    
                }
    })
    
    output$Sed_stats_by_category <- DT::renderDataTable(
        sed_stats_df(),
        extensions = list("Buttons" = NULL, 'Scroller' = NULL),
        options = list(
            deferRender = TRUE,
            autoWidth = TRUE,
            scrollY = 350,
            scrollX = 200,
            scroller = TRUE,
            dom = 'BRSfrti',
            buttons =
                list(
                    'copy',
                    'print',
                    list(
                        extend = 'collection',
                        buttons = c('csv', 'excel', 'pdf'),
                        text = 'Download'
                    )
                ),
            # pageLength = 5,
            fixedHeader = TRUE,
            fillContainer = F,
            class = "display",
            columnDefs = list(list(className = 'dt-left'))
        ),
        rownames = FALSE
    )
    
    
    
    output$tab_H <- renderUI({
        req(sed_stats_df())
        
        fluidRow(
            column(
                12,
                align = "center",
                offset = 0,
                # style = "background-color:#ECF0F1;",
                if (is.null(sed_stats_df())) {
                    
                } else{
                    DT::dataTableOutput("Sed_stats_by_category") %>%
                        withSpinner(type = 6 )
                }
            )
        )
        
    })
    
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------Plotting logic-------------------------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    
    ################# Filtering logic for WATERSHED DF ################
    
    Wshed_subset <- reactive({
        req(Wshed_data())
        req(input$AreaVsScen)
        if (input$AreaVsScen == 'allscen') {
            Wshed_data() %>% dplyr::filter(Watershed %in% input$Wshed_wshed,
                                           Scenario %in% input$Wshed_wshed_S)
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
            arrange_at(.vars = input$S_variable, desc) %>% ungroup() %>%
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
            arrange_at(.vars = input$S_variable, desc) %>% ungroup() %>%
            mutate(cumPercArea = cumsum(area_ha_) / sum(area_ha_) *
                       100) %>% dplyr::mutate_if(is.numeric, round, 2) %>% 
            dplyr::filter(cumPercArea < input$thresh_S  & slope > min(input$thresh_slope_S) & slope < max(input$thresh_slope_S) )
    })
    
    
    ############## Creates column in spatial df for each variable with values  relative to the chosen baseline scenario   ##############
    spsub <- reactive({ Spatial_data() %>%
            dplyr::filter(Watershed %in% input$S_wshed) 
    })
    
    # Spatial_data_rel<- reactive({ spsub() %>%
    #         dplyr::filter(Watershed %in% input$S_wshed) %>%
    #         dplyr::filter(Scenario != input$S_scen_base) %>%
    #         dplyr::mutate(AbsChange_SoLs_kg_ha = SoLs_kg_ha- spsub()$SoLs_kg_ha[spsub()$Scenario==input$S_scen_base],
    #                       AbsChange_SdDp_kg_ha = SdDp_kg_ha- spsub()$SdDp_kg_ha[spsub()$Scenario==input$S_scen_base],
    #                       AbsChange_SdYd_kg_ha = SdYd_kg_ha- spsub()$SdYd_kg_ha[spsub()$Scenario==input$S_scen_base],
    #                       AbsChange_SRP_kg_ha_ = SRP_kg_ha_- spsub()$SRP_kg_ha_[spsub()$Scenario==input$S_scen_base],
    #                       AbsChange_PP_kg_ha_ = PP_kg_ha_- spsub()$PP_kg_ha_[spsub()$Scenario==input$S_scen_base],
    #                       AbsChange_TP_kg_ha_ = TP_kg_ha_- spsub()$TP_kg_ha_[spsub()$Scenario==input$S_scen_base],
    #                       AbsChange_Runoff_mm_ = Runoff_mm_- spsub()$Runoff_mm_[spsub()$Scenario==input$S_scen_base],
    #                       AbsChange_DepLos_kg_ = DepLos_kg_- spsub()$DepLos_kg_[spsub()$Scenario==input$S_scen_base],
    #         )
    # })

    Spatial_data_rel<- reactive({ spsub() %>%
            dplyr::group_by(TopazID) %>%
            dplyr::mutate(AbsChange_SoLs_kg_ha = SoLs_kg_ha - SoLs_kg_ha[Scenario == input$S_scen_base],
                          AbsChange_SdDp_kg_ha = SdDp_kg_ha - SdDp_kg_ha[Scenario == input$S_scen_base],
                          AbsChange_SdYd_kg_ha = SdYd_kg_ha - SdYd_kg_ha[Scenario == input$S_scen_base],
                          AbsChange_SRP_kg_ha_ = SRP_kg_ha_ - SRP_kg_ha_[Scenario == input$S_scen_base],
                          AbsChange_PP_kg_ha_ = PP_kg_ha_ - PP_kg_ha_[Scenario == input$S_scen_base],
                          AbsChange_TP_kg_ha_ = TP_kg_ha_ - TP_kg_ha_[Scenario == input$S_scen_base],
                          AbsChange_Runoff_mm_ = Runoff_mm_ - Runoff_mm_[Scenario == input$S_scen_base],
                          AbsChange_DepLos_kg_ = DepLos_kg_ - DepLos_kg_[Scenario == input$S_scen_base]
                          ) %>% ungroup()
    })

    Spatial_subset_comp <- reactive({
        req(Spatial_data_rel())
        req(input$S_wshed)
        Spatial_data_rel() %>%
            dplyr::filter(Watershed %in% input$S_wshed &
                              Scenario %in% input$S_scen_comp)%>%
            arrange_at(.vars = input$S_variable, desc) %>% ungroup() %>%
            mutate(cumPercArea = cumsum(area_ha_) / sum(area_ha_) *
                       100) %>% dplyr::mutate_if(is.numeric, round, 2) %>%
            dplyr::filter(cumPercArea < input$thresh_S  & slope > min(input$thresh_slope_S) & slope < max(input$thresh_slope_S))
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
                
                p <-
                    plot_ly(
                        x = d.m$Scenario,
                        y = d.m$variable,
                        z = d.m$value,
                        type = "heatmap"
                    ) %>%
                    layout(
                        xaxis = list(
                            tickfont = list(size = 16),
                            tickangle = 45
                        ),
                        yaxis = list(
                            tickfont = list(size = 16)
                        ),
                        margin = list(
                            l = 150,
                            r = 20,
                            b = 150,
                            t = 100,
                            pad = 4
                        ),
                        title = list(
                            text = paste0(
                                '<b>Relative impacts of management and disturbance on the watershed</b>',
                                '<br>',
                                '<sup>',
                                '<i>Plot displays the relative impact of the all disturbances on the delivered\nwater quantity/quality metric at the watershed outlet.</i>',
                                '<br><br>',
                                '</sup>'
                            )
                        )
                    )
                p
                
                
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
                    
                    b <- ggplot(d.m) +
                        
                        geom_bar(
                            aes(
                                y = share,
                                x = variable,
                                fill = reorder(Scenario, share)
                                
                            ),
                            stat = "identity",
                            position = "dodge"
                        ) + guides(fill = FALSE)+
                        theme_bw(base_rect_size = 0.1) +
                        theme(
                            axis.text.x = element_text(
                                angle = 45,
                                size = 13,
                                vjust = ,
                                colour = "Black"
                            ),
                            axis.text.y = element_text(colour = "Black", size =12),
                            axis.title.x = element_blank(),
                            axis.title.y = element_blank(),
                            legend.title = element_blank(),
                            legend.position = "right"
                        ) + coord_flip() + labs(y = "Percent of total across all Scenarios", x = "") +
                        scale_fill_brewer(palette = "RdYlGn") +
                        scale_y_continuous(
                            labels = function(x)
                                paste0(x * 1, "%")
                        )
                    
                    ggplotly(b) %>%
                        layout(
                            title = list(
                                text = paste0(
                                    '<b>Relative impacts of management and disturbance on the watershed</b>',
                                    '<br>',
                                    '<sup>',
                                    '<i>Plot displays the relative impact of the all disturbances on the delivered\nwater quantity/quality metric at the watershed outlet.</i>',
                                    '<br><br>',
                                    '</sup>'
                                )
                            ),
                            margin = list(
                                l = 10,
                                r = 20,
                                b = 5,
                                t = 150,
                                pad = 0
                            ),
                            legend = list(
                                orientation = "v",
                                title=list(text='<b> Scenario </b>'),
                                font = list(size = 12)
                            )
                        )
                    
                    
                }
        } else
            if (input$AreaVsScen == 'allwat') {
                if (input$ScenVvar == "Heatmap") {
                    
                    if (length(unique(Wshed_subset()$Watershed))<2) {
                        d <-
                            Wshed_subset() %>% dplyr::select(Watershed, input$wshed_var) %>% dplyr::mutate_at(vars(Watershed), as.factor)
                    }else
                        if (length(unique(Wshed_subset()$Watershed))>=2) {
                            d <-
                                Wshed_subset() %>% dplyr::select(Watershed, input$wshed_var) %>% 
                                dplyr::mutate_at(vars(Watershed), as.factor)%>% 
                                dplyr::mutate_if(is.numeric, scale)}
                    
                    # d <-
                    #     Wshed_subset() %>% dplyr::select(Watershed, input$wshed_var) %>% dplyr::mutate_if(is.numeric, scale)
                    d.m <- reshape2::melt(d)
                    
                    
                    p <-
                        plot_ly(
                            x = d.m$Watershed,
                            y = d.m$variable,
                            z = d.m$value,
                            type = "heatmap"
                        ) %>%
                        layout(
                            xaxis = list(
                                tickfont = list(size = 16),
                                tickangle = 45
                            ),
                            yaxis = list(
                                tickfont = list(size = 16)
                            ),
                            margin = list(
                                l = 150,
                                r = 20,
                                b = 150,
                                t = 100,
                                pad = 4
                            ),
                            title = list(
                                text = paste0(
                                    '<b>Relative impacts of the disturbance scenario across all watersheds </b>',
                                    '<br>',
                                    '<sup>',
                                    '<i>Plot displays the relative impact of the disturbance on the delivered particular\nwater quantity/quality metric at each of the watersheds</i>',
                                    '<br><br>',
                                    '</sup>'
                                )
                            )
                        )
                    
                    p
                    
                    
                    
                } else
                    if (input$ScenVvar == "Bar Chart") {
                        
                        if (length(unique(Wshed_subset()$Watershed))<2) {
                            d <-
                                Wshed_subset() %>% dplyr::select(Watershed, input$wshed_var) %>% dplyr::mutate_at(vars(Watershed), as.factor)
                        }else
                            if (length(unique(Wshed_subset()$Watershed))>=2) {
                                d <-
                                    Wshed_subset() %>% dplyr::select(Watershed, input$wshed_var) %>% 
                                    dplyr::mutate_at(vars(Watershed), as.factor)%>% 
                                    dplyr::mutate_if(is.numeric, scale)}
                        # d <-  Wshed_subset() %>% dplyr::select(Watershed, input$wshed_var)
                        
                        d.m <- reshape2::melt(d)
                        
                        ## Calculates percent contribution of each variable across all
                        ## the simulated scenarios
                        d.m <- d.m %>%
                            group_by(variable) %>%
                            mutate(total = sum(value),
                                   share = (value / total) * 100) %>%
                            ungroup()
                        
                        b <- ggplot(d.m) +
                            
                            geom_bar(
                                aes(
                                    y = share,
                                    x = variable,
                                    fill = reorder(Watershed,share)
                                ),
                                stat = "identity",
                                position = "dodge"
                            )  +guides(fill = FALSE)+
                            theme_bw(base_rect_size = 0.1) +
                            theme(
                                axis.text.x = element_text(
                                    angle = 45,
                                    size =13,
                                    vjust = ,
                                    colour = "Black"
                                ),
                                axis.text.y = element_text(colour = "Black", size =12),
                                axis.title.x = element_blank(),
                                axis.title.y = element_blank(),
                                legend.title = element_blank(),
                                legend.position = "right"
                            ) + labs(y = "Percent of total across all Watersheds", x = "") +
                            coord_flip() +
                            scale_fill_brewer(palette = "RdYlGn") +
                            scale_y_continuous(
                                labels = function(x)
                                    paste0(x * 1, "%")
                            )
                        
                        ggplotly(b)  %>%
                            layout(
                                title = list(
                                    text = paste0(
                                        '<b>Relative impacts of the disturbance scenario across all watersheds </b>',
                                        '<br>',
                                        '<sup>',
                                        '<i>Plot displays the relative impact of the disturbance on the delivered particular\nwater quantity/quality metric at each of the watersheds</i>',
                                        '</sup>'
                                    )
                                ),
                                margin = list(
                                    l = 10,
                                    r = 20,
                                    b = 5,
                                    t = 150,
                                    pad = 0
                                ),
                                legend = list(
                                    orientation = "v",
                                    title=list(text='<b> Watershed </b>'),
                                    font = list(size = 12)
                                )
                            )
                        
                        
                    }
            }
    })
    
    
    
    
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------Plots Hillslopes tab-------------------------------------------------------##
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
                                                                                aes(y = cumSediment.Yield.of.Particles.Under.0.016.mm..kg , color = Scenario),
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
                plot.margin = unit(c(2, 0.5, 1, 0.5), "cm"),
                plot.title = element_text(
                    size = 10,
                    color = "#000000",
                    face = "bold",
                    family = "Bauhaus 93",
                    vjust = 1,
                    hjust = 0.5
                    
                )
            ) +
            labs(
                x = "Percent of total hillslope area",
                y = "Cumulative Percent of Total selected variable",
                # y = paste("Percent of total", input$Hill_variable, sep = " "),
                title = paste(
                    "Cumulative percent of total",
                    input$Hill_variable ,
                    "\ncontribution by percent hillslope area"
                )
                ,
                colour = "Scenario"
            )
        if (input$DefOrUserUpload_H == 'Default_Data_Portland' |
            input$DefOrUserUpload_H == 'Default_Data_Seattle') {
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
                        "Thinn85.2020.ki5krcs.chn_12" = "#7CFC00"
                    )
                )
        } else
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
                if (input$DefOrUserUpload_H == 'Default_Data_Palouse') {
                    p1 <- p1 +
                        scale_colour_colorblind()
                }
        else
            if (input$DefOrUserUpload_H == 'Upload Data') {
                p1 <- p1 + scale_color_colorblind()
                # scale_color_brewer(palette = "virdis")
            }
        
        
        p1
        
        
    })
    
    
    
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
                                                                                aes(y = cumSediment.Yield.of.Particles.Under.0.016.mm..kg , color = Scenario),
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
                plot.margin = unit(c(2, 0.5, 1, 0.5), "cm"),
                plot.title = element_text(
                    size = 10,
                    color = "#000000",
                    face = "bold",
                    family = "Bauhaus 93",
                    vjust = 1,
                    hjust = 0.5
                    
                )
            ) +
            labs(
                x = "Percent of total channel length",
                y = "Cumulative Percent of Total selected variable",
                # y = paste("Percent of total", input$Hill_variable, sep = " "),
                title = paste(
                    "Cumulative percent of total",
                    input$Hill_variable ,
                    "\ncontribution by percent channel length"
                ),
                colour = "Scenario"
            )
        
        if (input$DefOrUserUpload_H == 'Default_Data_Portland' |
            input$DefOrUserUpload_H == 'Default_Data_Seattle') {
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
        } else
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
            } else
                if (input$DefOrUserUpload_H == 'Default_Data_Palouse') {
                    p3 <- p3 +
                        scale_color_colorblind()
                }
        else
            if (input$DefOrUserUpload_H == 'Upload Data') {
                p3 <- p3 +
                    scale_color_colorblind()
            }
        
        
        
        
        p3
        
    })
    
    
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
                                                                                aes(y = cumSediment.Yield.of.Particles.Under.0.016.mm..kg , color = Scenario),
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
                plot.margin = unit(c(2, 0.5, 1, 0.5), "cm"),
                plot.title = element_text(
                    size = 10,
                    color = "#000000",
                    face = "bold",
                    family = "Bauhaus 93",
                    vjust = 1,
                    hjust = 0.5
                    
                )
            ) +
            labs(
                x = "Percent of total hillslope area",
                y = "Cumulative selected variable",
                # y = paste("Cumulative", input$Hill_variable, sep = " "),
                title = paste(
                    "Cumulative total",
                    input$Hill_variable ,
                    "\ncontribution by percent hillslope area"
                ),
                colour = "Scenario"
            )
        if (input$DefOrUserUpload_H == 'Default_Data_Portland' |
            input$DefOrUserUpload_H == 'Default_Data_Seattle') {
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
        } else
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
            } else
                if (input$DefOrUserUpload_H == 'Default_Data_Palouse') {
                    p2 <- p2 + scale_color_colorblind()
                } else
                    if (input$DefOrUserUpload_H == 'Upload Data') {
                        p2 <- p2 +
                            scale_color_colorblind()
                    }
        p2
        
    })
    
    
    
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
                                                                                aes(y = cumSediment.Yield.of.Particles.Under.0.016.mm..kg , color = Scenario),
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
                plot.margin = unit(c(2, 0.5, 1, 0.5), "cm"),
                plot.title = element_text(
                    size = 10,
                    color = "#000000",
                    face = "bold",
                    family = "Bauhaus 93",
                    vjust = 1,
                    hjust = 0.5
                    
                )
            ) +
            labs(
                x = "Percent of total channel length",
                y = "Cumulative selected variable",
                # y = paste("Cumulative", input$Hill_variable, sep = " "),
                title = paste(
                    "Cumulative total",
                    input$Hill_variable ,
                    "\ncontribution by percent channel length"
                ),
                colour = "Scenario"
            )
        if (input$DefOrUserUpload_H == 'Default_Data_Portland' |
            input$DefOrUserUpload_H == 'Default_Data_Seattle') {
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
        } else
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
                if (input$DefOrUserUpload_H == 'Default_Data_Palouse') {
                    p4 <- p4 + scale_color_colorblind()
                } else
                    if (input$DefOrUserUpload_H == 'Upload Data') {
                        p4 <- p4 +
                            scale_color_colorblind()
                    }
        
        
        
        p4
        
    })
    
    
    
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
    
    output$spatial_table <- DT::renderDataTable(
        spdftab(),
        # caption = htmltools::tags$caption( style = 'caption-side: top; text-align: center; color:black; font-size:200% ;','Table1: Iris Dataset Table'),
        extensions = list("Buttons" = NULL, 'Scroller'= NULL),
        options = list(
            deferRender = TRUE,
            autoWidth = FALSE,
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
            fixedHeader = TRUE,
            fillContainer = F,
            class = "display",
            columnDefs = list(list(className = 'dt-left'))
            # pageLength = 15
            
        ),
        rownames = F
    ) 
    
    
    output$tab_sp <- renderUI({
        req(spdftab())
        fluidRow(
            column(
                12,
                align = "center",
                offset = 0,
                # style = "background-color:#ECF0F1;",
                DT::dataTableOutput("spatial_table",height = "600px")
            )
        )
    })
    
    # output$spatial_table <- DT::renderDataTable(
    #     spdftab(),
    #     # caption = htmltools::tags$caption( style = 'caption-side: top; text-align: center; color:black; font-size:200% ;','Table1: Iris Dataset Table'),
    #     extensions = list("Buttons" = NULL, 'Scroller'= NULL),
    #     options = list(
    #         deferRender = TRUE,
    #         autoWidth = TRUE,
    #         scrollY = 300,
    #         scrollX = 200,
    #         scroller = TRUE,
    #         dom = 'BRSfrti',
    #         buttons =
    #             list('copy', 'print', list(
    #                 extend = 'collection',
    #                 buttons = c('csv', 'excel', 'pdf'),
    #                 text = 'Download'
    #             )),
    #         fixedHeader = TRUE,
    #         fillContainer = F,
    #         class = "display",
    #         columnDefs = list(list(className = 'dt-left'))
    #         
    #     ),
    #     rownames = F
    # ) 
     
    
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
                palette = "-inferno",
                style = "quantile",
                midpoint = TRUE,
                title = "Comparison minus Baseline"
            )+
            tm_shape(Spatial_subset_base(), name = "Baseline Scenario") +
            # tm_borders(lwd = 0, alpha=0.0) +
            tmap::tm_polygons(
                input$S_variable,
                id = "watershed",
                palette = "-inferno",
                style = "fixed",
                breaks = c(0, 1, 10, 50, 250, 500, 750, 1000,
                           5000, 10000, 15000,Inf),
                title = ""
            )+
            tm_shape(Spatial_subset_comp(), name = "Comparison Scenario") +
            tmap::tm_polygons(
                input$S_variable,
                id = "watershed",
                palette = "-inferno",
                legend.hist = TRUE,
                style = "fixed",
                breaks = c(0, 1, 10, 50, 250, 500, 750, 1000,
                           5000, 10000, 15000,Inf),
                legend.show = FALSE,
                # title = "Comparison Scenario"
            ) +
            tmap::tm_layout(scale = 0.1,
                            title = "")
        
        tmap_leaflet(tm2,in.shiny = TRUE)  %>%
            addMiniMap(tiles = providers$Esri.WorldStreetMap,
                       toggleDisplay = TRUE,
                       zoomAnimation = TRUE,position = "bottomleft",height = 100)
    })
    
    
    
    
    ## --------------------------------------------------------------------------------------------------##
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------Plots:subbasin DF-------------------------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    
    output$Plotsub <- leaflet::renderLeaflet({
        req(SWATSub_subset_base())
        req(SWATSub_data_comp())
        req(input$swat_var_sub)
        
        
        tmsub<-tm_shape(SWATSub_data_comp(), name = "Difference between comparison & baseline scenario") +
            tmap::tm_polygons(
                paste0("AbsChange_",input$swat_var_sub),
                id = "watershed",
                palette = "viridis",
                legend.hist = TRUE,
                style = "pretty",
                # style = "fixed",
                # breaks = c(0, 1, 10, 100, 1000,
                #            5000, 10000, 15000,20000,Inf),
                # legend.show = FALSE,
                title = "Comparison minus Baseline"
            )+
            tm_shape(SWATSub_subset_base(), name = "Baseline Scenario") +
            tmap::tm_polygons(
                        input$swat_var_sub,
                        id = "watershed",
                        palette = "viridis",
                        legend.hist = TRUE,
                        style = "pretty",
                        # style = "fixed",
                        # breaks = seq(from = 0, 
                                     # to = 0.4, by = 8), 
                            # c(0, 1, 10, 100, 1000,
                            #        5000, 10000, 15000,20000,Inf),
                        title = "Baseline Scenario "
                    )+
                    tm_shape(SWATSub_data_comp(), name = "Comparison Scenario") +
                    tmap::tm_polygons(
                        input$swat_var_sub,
                        id = "watershed",
                        palette = "viridis",
                        legend.hist = TRUE,
                        style = "pretty",
                        # style = "fixed",
                        # breaks = c(0, 1, 10, 100, 1000,
                        #            5000, 10000, 15000,20000,Inf),
                        # legend.show = FALSE,
                        title = "Comparison Scenario"
                )+
                    tmap::tm_layout(scale = 0.1,
                                    title = "")
        
        
        tmap_leaflet(tmsub,in.shiny = TRUE)  %>%
                    addMiniMap(tiles = providers$Esri.WorldStreetMap,
                               toggleDisplay = TRUE,
                               zoomAnimation = TRUE,position = "bottomleft",height = 100)
    })
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------Plots:HRU DF-------------------------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    
    output$Plothru <- leaflet::renderLeaflet({
        tmhru<-tm_shape(SWATHru_data_comp(), name = "Difference between comparison & baseline scenario") +
            tmap::tm_polygons(
                paste0("AbsChange_",input$swat_var_hru),
                id = "watershed",
                palette = "viridis",
                legend.hist = TRUE,
                style = "pretty",
                title = "Comparison minus Baseline"
            )+
            tm_shape(SWATHru_subset_base(), name = "Baseline Scenario") +
            tmap::tm_polygons(
                input$swat_var_hru,
                id = "watershed",
                palette = "viridis",
                legend.hist = TRUE,
                style = "pretty",
                title = "Baseline Scenario "
            )+
            tm_shape(SWATHru_data_comp(), name = "Comparison Scenario") +
            tmap::tm_polygons(
                input$swat_var_hru,
                id = "watershed",
                palette = "viridis",
                legend.hist = TRUE,
                style = "pretty",
                title = "Comparison Scenario"
            )+
            tmap::tm_layout(scale = 0.1,
                            title = "")
        
        
        tmap_leaflet(tmhru,in.shiny = TRUE)  %>%
            addMiniMap(tiles = providers$Esri.WorldStreetMap,
                       toggleDisplay = TRUE,
                       zoomAnimation = TRUE,position = "bottomleft",height = 100)
        
    })
    
    
    observe_helpers()
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
# run_with_themer(shinyApp(ui = ui, server = server))
#bg = "#14213D", fg = "#FFF"