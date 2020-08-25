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

library(pacman)

pacman::p_load(shiny,tidyverse,shinythemes,shinycssloaders,shinyWidgets,
               plotly, stringr,leaflet, tmap,ggthemes, DT,shinyhelper, shinyalert,
               crosstalk, sever)

# library(shiny, quietly = TRUE)
# library(tidyverse, quietly = TRUE)
# library(shinythemes)
# library(shinycssloaders)
# library(shinyWidgets)
# library(plotly, quietly = TRUE)
# library(stringr)
# library(leaflet)
# library(tmap)
# library(ggthemes)
# # library(shinyBS, quietly = TRUE)
# # library(shinyLP, quietly = TRUE)
# library(DT, quietly = TRUE)
# library(shinyhelper, quietly = TRUE)
# # library(shinydisconnect)
# library(shinyalert, quietly = TRUE)
# library(crosstalk)
# library(sever)
source("global.R")


## ----------------------------------Init Options---------------------------------------##
options(shiny.maxRequestSize = 100 * 1024 ^ 2)




## ----------------------------------define UI------------------------------------------##
# 

ui <-navbarPage(title = div("viz-WEPPcloud",
                            div(tags$a(href="https://forest.moscowfsl.wsu.edu/fswepp/", tags$img(src='FS.png',style="position:fixed;right: 105px;top: 5px;padding-bottom:10px;", height = 50)),
                                tags$a(href="https://www.uidaho.edu/", tags$img(src='UI.jpg',style="position:fixed;right: 150px;top: 5px;padding-bottom:10px;", height = 50)),
                                tags$a(href="https://nifa.usda.gov/", tags$img(src='nifa.jpg',style="position:fixed;right: 60px;top: 5px;padding-bottom:10px;", height = 50)),
                                tags$a(href= "https://github.com/devalc/Viz-WEPPCloud", tags$img(src="GitHub-Mark.png",style="position:fixed;right: 10px;top: 5px;padding-bottom:10px;", height = 50))
                            )),
                 
                windowTitle = "viz-WEPPcloud",
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
        
        
        setBackgroundImage(src = "bg_pixabay.jpg", shinydashboard = FALSE),
        
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
                                  '<div class="jumbotron" style="background-color:	#f2f2f2;">
                                                      <h1  style="color:black;">viz-WEPPcloud </h1>
                                                      <h4  style="color:black;">A post-processing tool for identifying and examining impacts of management on pollutant 
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
                    style = "height:140px;background-color:#f2f2f2;padding-left:20px;padding-top:20px;padding-bottom:20px;color:#000000",

                    tags$div(
                        tags$p(
                            "viz-WEPPcloud is currently designed to analyze output from WEPPCloud and provides an option for users to upload their own output data files.",
                            align = "center"
                        ),
                        tags$p(
                            a(href = 'https://wepp1.nkn.uidaho.edu/weppcloud/', 'WEPPcloud', .noWS = "outside", style = "color:#FFAE42"),
                            ' is a cloud based simulation tool based on the process based Watershed Erosion Prediction Project', tags$a(href="https://www.fs.usda.gov/ccrc/tools/watershed-erosion-prediction-project", "WEPP",  style = "color:#FFAE42"), 'model. It estimates
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
        
    #     fluidPage(fluidRow(
    #         column(width = 12,
    #         tags$footer(
    #             tags$div(
    #                 tags$p(
    #                     "viz-WEPPcloud is currently designed to analyze output from WEPPCloud and provides an option for users to upload their own output data files.",
    #                     align = "center"
    #                 ),
    #                 tags$p(
    #                     a(href = 'https://wepp1.nkn.uidaho.edu/weppcloud/', 'WEPPcloud', .noWS = "outside"),
    #                     ' is a cloud based simulation tool based on the process based Watershed Erosion Prediction Project', tags$a(href="https://www.fs.usda.gov/ccrc/tools/watershed-erosion-prediction-project", "WEPP"), 'model. It estimates
    #                                                       hillslope soil erosion, runoff, and sediment yields from anywhere in the continental U.S. It is especially useful for
    #                                                       post-wildfire assessments, fuel treatment planning, and prescribed fire analysis.',
    #                     .noWS = c("after-begin", "before-end"),
    #                     align = "center"
    #                 ),
    #                 
    #             ),style = "position:absolute;;
    # bottom:0;
    # float: left;
    # width:100%;
    # height:100px;
    # color: #292929;
    # padding:0px;
    # background-color: #ECF0F1;
    # text-align: left;
    # z-index: 1000;"
    #         ))
    #     ),
    #     # HTML("<br style = “line-height:30;”><br>")
    #     )
    
    # column(12,
    #        align = "center",
    #        offset = 0,
    #        tags$footer(HTML("<footer class='page-footer font-large indigo'>
    #                        <!-- Copyright -->
    #                        <div class='footer-copyright text-center py-3'>© 2020 Copyright:
    #                        <a href='https://github.com/devalc/Viz-WEPPCloud'> This is footer</a>
    #                        <p>Author: Hege Refsnes<br>
    #                        <a href='mailto:hege@example.com'>hege@example.com</a></p>
    # 
    #                        </div>
    #                        
    #                        <!-- Copyright -->
    #                        
    #                        
    # 
    #                        </footer>"),
    #                    style = "position:fixed;
    # bottom:0;
    # float: left;
    # width:100%;
    # height:50px; /* Height of the footer */
    # color: #292929;
    # padding:0px;
    # background-color: #ECF0F1;
    # text-align: left;
    # z-index: 1000;"))
    
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
                    "Use default data (Lake Tahoe simulations)" = "Default Data",
                    "Upload your own data" =
                        "Upload data"
                ),
                selected = "Default Data",
                status = 'warning'
            ),
            
            
            # uiOutput("Wshed_selectfile"),
            
            uiOutput("W_FileInput"),
            
            awesomeRadio(
                inputId = "AreaVsScen",
                label = "Management/watershed Options: ",
                choices = c(
                    "One Watershed, All Scenarios" = "allscen",
                    "One Scenario, All Watersheds" =
                        "allwat"
                ),
                selected = "allscen",
                status = 'warning'
            ) %>% 
                helper(icon = "question-circle", colour = "#FF0000",
                        content = "W_compare",
                        type = "markdown", size = "l",
                        buttonLabel = "Okay", easyClose = TRUE, fade = TRUE),
            
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
            # style = "position:fixed;width:inherit;padding-left:180px;",
            offset= 1,
            style = 'padding-top:0px;padding-bottom:10px;',
            # setBackgroundColor("#ffffff",shinydashboard = FALSE),
            uiOutput("Wshed_Exp"),
            # fluidPage(# plotlyOutput("Plot5" ,height = "800px", width ="1200px")
                # column(12, tableOutput("tab1"))
                fluidRow(
                    column(
                        9,
                        offset = 2,
                        plotlyOutput("Plot9", height = "700px", width =
                                         "800px") %>% withSpinner(type = 6 ,color = "#ffffff")
                    
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
            style = "position:fixed;width:inherit;overflow-y:scroll;max-height: 640px;  ",
            width = 3,
            
            awesomeRadio(
                inputId = "DefOrUserUpload_H",
                label = "Data Import Options:",
                choices = c(
                    "Use default data (Lake Tahoe simulations)" = "Default Data",
                    "Upload your own data" =
                        "Upload data"
                ),
                selected = "Default Data",
                status = 'warning'
            ),
            
            # prettyRadioButtons(inputId = "analysis_method",
            #         label = "Show analysis:",
            #         choices = c("Relative to the baseline scenario", "Individual scenarios"),
            #         icon = icon("check"),
            #         bigger = TRUE,
            #         status = "warning",
            #         animation = "jelly",
            #         selected = "Individual scenarios"
            # ),
            
            uiOutput("H_FileInput"),
            uiOutput("Hill_selectfile"),
            uiOutput("Hill_wshed"),
            uiOutput("Hill_var"),
            uiOutput("Hill_scen_base"),
            uiOutput("Hill_scen_comp"),
            
            sliderInput(
                "thresh_H",
                "Plot Threshold (%):",
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
            
            
            uiOutput("Hill_scen"),
            
            awesomeRadio(
                inputId = "summary_DT_by_var_H",
                label = "Summarize Sediment by:",
                choices = c(
                    "Land Use" = "Landuse",
                    "Soil Type" = "Soiltype",
                    "Both" = "Both"
                ),
                selected = "Landuse",
                status = 'warning'
            ),
            
            # img(src="vizweppcloud_hex_wsj.png",width="100%")
            
            # awesomeRadio(inputId = "AvgWestShoreNos_H",
            # label = "Do you want average sediment summary for West shore?",
            #              choices = c("Yes"="Yes","No"="No"), selected = "Yes"),
            #
            # uiOutput("H_AvgWestShoreSummary")
            
            
            
        ),
        
        mainPanel(
            width = 9,
            style = 'padding:80px;',
            # style = "flex-grow:1; resize:vertical; overflow: hidden; position:relative; margin-left: 400px",
            # uiOutput("Exp1_Exp2") %>% withSpinner(color =
            #                                           "#0dc5c1"),
            # HTML("<br style = “line-height:5;”><br>"),
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
            
            # fluidRow(
            #     column(
            #         12,
            #         align = "center",
            #         offset = 0,
            #         style = "background-color:#ECF0F1;",
            #         DT::dataTableOutput("Sed_stats_by_category") %>%
            #             withSpinner(type = 6 ,color = "#ffffff")
            #     )
            # )
            # HTML("<br><br><br>"),
            # fluidRow(
            # column(6, DT::dataTableOutput("WS_Sed_stats_by_category") %>% withSpinner(color="#0dc5c1"))
            #
            # )
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
            style = "position:fixed;width:inherit;",
            width = 3,
            
            awesomeRadio(
                inputId = "DefOrUserUpload_S",
                label = "Data Import Options:",
                choices = c(
                    "Use default data (Lake Tahoe simulations)" = "Default Data",
                    "Upload your own data" = "Upload data"
                ),
                selected = "Default Data",
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
                "Plot Threshold (%):",
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
            
            # img(src="vizweppcloud_hex_wsj.png",width="100%")
            
            # awesomeRadio(inputId = "showchan_S",label = "Display Channels?",
            #              choices = c("Yes"="Yes","No"="No"), selected = "No")
            #
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            width = 9,
            style = 'padding:80px;',
            
            fluidRow(column(6,
                            # align = "center",
                            # offset = 0,
            leaflet::leafletOutput("Plot11")%>%
                withSpinner(type = 6 ,color = "#ffffff")
            ),
            column(6,
                   # align = "center",
                   # offset = 0,
                   leaflet::leafletOutput("Plot12")%>%
                       withSpinner(type = 6 ,color = "#ffffff")
            )
            
        ),
        HTML("<br style = “line-height:5;”><br>"),
        
        uiOutput("tab_sp") %>%
            withSpinner(type = 6 ,color = "#ffffff")
        
        )
        
        
    ),
    
    ## --------------------------------------------------------------------------------------##
    # tabPanel("Channel",
    #          sidebarPanel(
    #
    #
    #              awesomeRadio(inputId = "DefOrUserUpload_C",label = "Data Import Options:",
    #                           choices = c("Use default data (Lake Tahoe simulations)"="Default Data","Upload your own data"="Upload data"), selected = "Default Data"),
    #
    #
    #              uiOutput("C_FileInput"),
    #              uiOutput("Chan_selectfile"),
    #              uiOutput("Chan_wshed"),
    #              uiOutput("Chan_var"),
    #              uiOutput("Chan_scen"),
    #
    #
    #              sliderInput("thresh_C", "Thresholding Percent:",
    #                          min = 0, max = 100,
    #                          value = 100, step = 5)
    #          ),
    #
    #          # Main panel for displaying outputs ----
    #          mainPanel(
    #
    #              fluidRow(
    #                  # column(6,  plotlyOutput("Plot5")%>% withSpinner(color="#0dc5c1")),
    #                  column(6, plotlyOutput("Plot6")%>% withSpinner(color="#0dc5c1")),
    #                  column(6, plotlyOutput("Plot6_abs")%>% withSpinner(color="#0dc5c1"))
    #              ),
    #              fluidRow(
    #                  column(6, plotlyOutput("Plot7")%>% withSpinner(color="#0dc5c1")),
    #                  column(6, plotlyOutput("Plot7_abs")%>% withSpinner(color="#0dc5c1"))
    #                  # column(6, plotlyOutput("Plot8"))
    #              )
    #          )),

       

    
    br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
    
    # header = column(12, "this is a header"),
    

    
)
## ----------------------------------define server logic------------------------------------------##


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
            )
        } else
            if (input$DefOrUserUpload_H == 'Default Data') {
            }
    })
    
    Hill_data <- reactive({
        req(input$DefOrUserUpload_H)
        if (input$DefOrUserUpload_H == 'Default Data') {
            # file1 <- url("https://wepp1.nkn.uidaho.edu/weppcloud/static/mods/lt/results/lt2020_6_hill_summary.csv")
            file1 <-
                "data/lt2020_6_hill_summary_with_all_scenarios_04_15_2020.csv"
            read.table(file = file1,
                       header = TRUE,
                       sep = ",")
        } else
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
                "Select the variable of interest",
                colnames(Hill_data()[10:29]),
                selected = colnames(Hill_data()[10])
            )
        } else
            if (input$DefOrUserUpload_H == 'Default Data') {
                pickerInput(
                    inputId = "Hill_variable",
                    label = "Select the variable of interest",
                    choices =   c( "Runoff (mm)" = "Runoff..mm.",
                                   "Lateral flow (mm)" = "Lateral.Flow..mm.",
                                   "Baseflow (mm)" = "Baseflow..mm.",
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
                                   "Particle Fraction.Under.0.016.mm (kg/ha)" = "Particle.Fraction.Under.0.016.mm",
                                   "Sediment yield of particles under 0.016 mm (kg/ha)" = "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha."
                                   
                    ),
                    selected = "Runoff..mm.",
                    multiple = F
                )
                
            }
        
    })
    
    
    output$Hill_wshed <- renderUI({
        if (input$DefOrUserUpload_H == 'Upload data') {
            req(Hill_data())
            pickerInput(
                "Hill_wshed",
                "Select the watershed of interest",
                unique(Hill_data()$Watershed) %>% 
                    helper(icon = "question-circle", colour = "#FF0000",
                           content = "H_upload",
                           type = "markdown", size = "l",
                           buttonLabel = "Okay", easyClose = TRUE, fade = TRUE)
            )
        } else
            if (input$DefOrUserUpload_H == 'Default Data') {
                pickerInput(
                    inputId = "Hill_wshed",
                    label = "Select the watershed of interest",
                    choices =   unique(Hill_data()$Watershed),
                    selected =   unique(Hill_data()$Watershed)[11]
                )
                
            }
        
    })
    
    output$Hill_scen_base <- renderUI({
        if (input$DefOrUserUpload_S == 'Upload data') {
            req(Spatial_data())
            pickerInput(
                "Hill_scen_base",
                "Select the baseline scenario",
                unique(Spatial_data()$Scenario),
                unique(Spatial_data()$Scenario)[1],
                multiple = F
            )
        } else
            if (input$DefOrUserUpload_S == 'Default Data') {
                pickerInput(
                    inputId = "Hill_scen_base",
                    label = "Select the baseline scenario",
                    choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                                 "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                                 "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                                 "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                                 "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                                 "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                                 "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                                 "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                                 "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                                 "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                                 "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                                 
                    ),
                    unique(Spatial_data()$Scenario)[1],
                    multiple = F
                )
                
            }
        
    })
    
    
    output$Hill_scen_comp <- renderUI({
        if (input$DefOrUserUpload_S == 'Upload data') {
            req(Spatial_data())
            pickerInput(
                inputId = "Hill_scen_comp",
                label = "Select the scenario to compare",
                choices = unique(Spatial_data()$Scenario),
                options = list(`actions-box` = TRUE),
                selected = unique(Spatial_data()$Scenario)[2],
                multiple = T
            )
        } else
            if (input$DefOrUserUpload_S == 'Default Data') {
                pickerInput(
                    inputId = "Hill_scen_comp",
                    label = "Select the scenario to compare",
                    choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                                 "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                                 "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                                 "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                                 "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                                 "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                                 "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                                 "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                                 "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                                 "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                                 "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                                 
                    ),
                    options = list(`actions-box` = TRUE),
                    selected = "LowSevS.2020.ki5krcs.chn_12",
                    multiple = T
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
                multiple = F
            )
        } else
            if (input$DefOrUserUpload_H == 'Default Data') {
                pickerInput(
                    inputId = "Hill_scen",
                    label = "Select Scenario do display data summary",
                    choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                                 "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                                 "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                                 "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                                 "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                                 "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                                 "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                                 "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                                 "Simulated fire-fccs fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                                 "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                                 "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"

                    ),
                    unique(Hill_data()$Scenario)[1],
                    multiple = F
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
    
    
    
    ## -----------------------------------------------------------------------------------------------------------##
    ##  Generate plot descriptions ##
    ## -----------------------------------------------------------------------------------------------------------##
    # output$Exp1 <- renderText({
    #     paste(
    #         "What percent of total hillslope area contributes a large fraction of total",
    #         " ",
    #         input$Hill_variable ,
    #         " ",
    #         "?"
    #     )
    # })
    # 
    # output$Exp2 <- renderText({
    #     paste(
    #         "What percent of total hillslope area contributes a large fraction of cumulative",
    #         " ",
    #         input$Hill_variable ,
    #         " ",
    #         "?"
    #     )
    # })
    # 
    # output$Exp3 <- renderText({
    #     paste(
    #         "What percent of total channel length contributes a large fraction of total",
    #         " ",
    #         input$Hill_variable ,
    #         " ",
    #         "?"
    #     )
    # })
    # 
    # output$Exp4 <- renderText({
    #     paste(
    #         "What percent of total channel length contributes a large fraction of cumulative",
    #         " ",
    #         input$Hill_variable ,
    #         " ",
    #         "?"
    #     )
    # })
    #
    
    
    # output$Exp1_Exp2 <- renderUI({
    #     req(hill_arr_by_var_HA())
    #     req(hill_arr_by_var_HA_abs())
    #     
    #     fluidRow(
    #         column(
    #             6,
    #             style = "height:60px;background-color:#F5F5F5;padding-left:0px;",
    #             offset = 0,
    #             textOutput("Exp1")
    #         ),
    #         tags$head(
    #             tags$style(
    #                 "#Exp1{color: black;
    #                               font-size: 16px;
    #                               font-style: normal;
    #                               font-family: Helvetica;
    #                               text-align: center;
    #                               }"
    #             )
    #         ),
    #         column(
    #             5,
    #             style = "height:60px;background-color:#F5F5F5;padding-left:0px;",
    #             offset = 1,
    #             textOutput("Exp2")
    #         ),
    #         tags$head(
    #             tags$style(
    #                 "#Exp2{color: black;
    #                               font-size: 16px;
    #                               font-style: normal;
    #                               font-family: Helvetica;
    #                               text-align: center;
    #                               }"
    #             )
    #         )
    #     )
    # })
    
    
    # output$Exp3_Exp4 <- renderUI({
    #     req(hill_arr_by_var_CL())
    #     req(hill_arr_by_var_CL_abs())
    #     fluidRow(
    #         column(
    #             6,
    #             style = "height:60px;background-color:#F5F5F5;padding-left:10px;",
    #             offset = 0,
    #             textOutput("Exp3")
    #         ),
    #         tags$head(
    #             tags$style(
    #                 "#Exp1{color: black;
    #                               font-size: 16px;
    #                               font-style: normal;
    #                               font-family: Helvetica;
    #                               text-align: center;
    #                               }"
    #             )
    #         ),
    #         column(
    #             5,
    #             style = "height:60px;background-color:#F5F5F5;padding-left:10px;",
    #             offset = 1,
    #             textOutput("Exp4")
    #         ),
    #         tags$head(
    #             tags$style(
    #                 "#Exp2{color: black;
    #                               font-size: 16px;
    #                               font-style: normal;
    #                               font-family: Helvetica;
    #                               text-align: center;
    #                               }"
    #             )
    #         )
    #     )
    # })
    
    ## ----------------------------------Channel server logic------------------------------------------##
    
    ######## Server logic for UI generation for Channel tab ##########
    
    # output$C_FileInput <- renderUI({
    #     if (input$DefOrUserUpload_C == 'Upload data') {
    #         message = 'max. file size is 32MB'
    #         fileInput(
    #             "Chan_file",
    #             label = "Uplaod 'Channel' file (*_chn_*.csv)",
    #             multiple = F,
    #             placeholder = "No file selected",
    #             accept = ".csv"
    #         )
    #     } else
    #         if (input$DefOrUserUpload_C == 'Default Data') {
    #         }
    # })
    
    # Chan_data <- reactive({
    #     req(input$DefOrUserUpload_C)
    #     if (input$DefOrUserUpload_C == 'Default Data') {
    #         file2 <-
    #             url(
    #                 "https://wepp1.nkn.uidaho.edu/weppcloud/static/mods/lt/results/lt2020_6_chn_summary.csv"
    #             )
    #         # file2 <- "data/lt2020_6_chn_summary_with_all_scenarios_03_11_2020.csv"
    #         read.table(file = file2,
    #                    header = TRUE,
    #                    sep = ",")
    #     } else
    #         if (input$DefOrUserUpload_C == 'Upload data') {
    #             file2 <- input$Chan_file
    #             if (is.null(file2)) {
    #                 return()
    #             }
    #             validate(
    #                 need(
    #                     grepl("chn", input$Chan_file) == TRUE,
    #                     "Wrong file provided. Channel filename should have '_chn_' in filename"
    #                 )
    #             )
    #             read.table(
    #                 file = file2$datapath,
    #                 header = TRUE,
    #                 sep = ","
    #             )
    #             
    #         }
    #     
    # })
    # 
    # 
    # output$Chan_var <- renderUI({
    #     if (input$DefOrUserUpload_C == 'Upload data') {
    #         req(Chan_data())
    #         pickerInput(
    #             "Chan_variable",
    #             "Select the variable of interest",
    #             colnames(Chan_data()[7:25]),
    #             selected = colnames(Chan_data()[10])
    #         )
    #     } else
    #         if (input$DefOrUserUpload_C == 'Default Data') {
    #             pickerInput(
    #                 inputId = "Chan_variable",
    #                 label = "Select the variable of interest",
    #                 choices =  as.character(unique(colnames(
    #                     Chan_data()
    #                 )))[c(7:14, 23:25)],
    #                 selected = as.character(unique(colnames(
    #                     Chan_data()
    #                 )))[10],
    #                 multiple = F
    #             )
    #             
    #         }
    #     
    # })
    # 
    # 
    # output$Chan_wshed <- renderUI({
    #     if (input$DefOrUserUpload_C == 'Upload data') {
    #         req(Chan_data())
    #         pickerInput(
    #             "Chan_wshed",
    #             "Select the variable of interest",
    #             unique(Chan_data()$Watershed)
    #         )
    #     } else
    #         if (input$DefOrUserUpload_C == 'Default Data') {
    #             pickerInput(
    #                 inputId = "Chan_wshed",
    #                 label = "Select the variable of interest",
    #                 choices =   unique(Chan_data()$Watershed),
    #                 selected =   unique(Hill_data()$Watershed)[11]
    #             )
    #             
    #         }
    #     
    # })
    
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
            ) %>% 
                helper(icon = "question-circle", colour = "#FF0000",
                       content = "W_upload",
                       type = "markdown", size = "l",
                       buttonLabel = "Okay", easyClose = TRUE, fade = TRUE)
        } else
            if (input$DefOrUserUpload_W == 'Default Data') {
            }
    })
    
    
    Wshed_data <- reactive({
        req(input$DefOrUserUpload_W)
        if (input$DefOrUserUpload_W == 'Default Data') {
            # file3 <- url("https://wepp1.nkn.uidaho.edu/weppcloud/static/mods/lt/results/lt2020_6_out_summary.csv")
            file3 <-
                "data/lt2020_6_out_summary_with_all_scenarios_04_15_2020.csv"
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
                    unique(Wshed_data()$Watershed)
                )
            } else
                if (input$AreaVsScen == 'allwat') {
                    pickerInput(
                        "Wshed_wshed",
                        "Select the scenario of interest",
                        unique(Wshed_data()$Scenario)
                    )
                }
        } else
            if (input$DefOrUserUpload_W == 'Default Data') {
                if (input$AreaVsScen == 'allscen') {
                    pickerInput(
                        inputId = "Wshed_wshed",
                        label = "Select the watershed of interest",
                        choices =   unique(Wshed_data()$Watershed),
                        selected =   unique(Hill_data()$Watershed)[11]
                    )
                } else
                    if (input$AreaVsScen == 'allwat') {
                        pickerInput(
                            "Wshed_wshed",
                            "Select the scenario of interest",
                            unique(Wshed_data()$Scenario)
                        )
                    }
                
            }
        
    })
    
    
    output$wshed_var <- renderUI({
        if (input$DefOrUserUpload_W == 'Upload data') {
            req(Hill_data())
            pickerInput(
                inputId = "wshed_var",
                label = "Select the variables of interest",
                options = list(`actions-box` = TRUE),
                colnames(Wshed_data())[7:20],
                selected = colnames(Wshed_data()[7:10]),
                multiple = T
            )
        } else
            if (input$DefOrUserUpload_W == 'Default Data') {
                pickerInput(
                    inputId = "wshed_var",
                    label = "Select the variables of interest",
                    options = list(`actions-box` = TRUE),
                    choices =   colnames(Wshed_data())[7:20],
                    selected = colnames(Wshed_data()[7:10]),
                    multiple = T,
                    # choicesOpt = list(
                    #     content = stringr::str_trunc(colnames(Wshed_data())[7:20], width = 60)
                    # )
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
                label = "Uplaod subcatchements JSON/geojson/RDS file",
                multiple = F,
                placeholder = "No file selected",
                accept = c(".JSON", ".geojson", ".RDS")
            )%>% 
        helper(icon = "question-circle", colour = "#FF0000",
               content = "S_upload",
               type = "markdown", size = "l",
               buttonLabel = "Okay", easyClose = TRUE, fade = TRUE)
        } else
            if (input$DefOrUserUpload_S == 'Default Data') {
            }
    })
    
    # output$S_FileInput_Chan <- renderUI({
    #     if (input$DefOrUserUpload_S == 'Upload data') {
    #         message = 'max. file size is 32MB'
    #         fileInput(
    #             "Spatial_file_chan",
    #             label = "Uplaod Channels JSON/geojson/RDS file",
    #             multiple = F,
    #             placeholder = "No file selected",
    #             accept = c(".JSON", ".geojson", ".RDS")
    #         )%>% 
    #             helper(icon = "question-circle", colour = "#FF0000",
    #                    content = "S_upload",
    #                    type = "markdown", size = "l",
    #                    buttonLabel = "Okay", easyClose = TRUE, fade = TRUE)
    #     } else
    #         if (input$DefOrUserUpload_S == 'Default Data') {
    #         }
    # })
    
    Spatial_data <- reactive({
        req(input$DefOrUserUpload_S)
        if (input$DefOrUserUpload_S == 'Default Data') {
            # sf::st_read("data/lt_allcond_subcatchments_wgs84_split_wshed_and_scen.geojson")
            readRDS("data/lt2020_6_subcatchments_wgs84_split_wshed_and_scen.RDS")
        } else
            if (input$DefOrUserUpload_S == 'Upload data') {
                file4 <- input$Spatial_file
                if (is.null(file4)) {
                    return()
                }
                sf::st_read(file4$datapath)
            }
        
    })
    
    ## spatial channel data
    # Spatial_data_chan <- reactive({
    #     req(input$DefOrUserUpload_S)
    #     if (input$DefOrUserUpload_S == 'Default Data') {
    #         # sf::st_read("data/lt_allcond_subcatchments_wgs84_split_wshed_and_scen.geojson")
    #         readRDS("data/lt2020_6_channels_wgs84_split_wshed_and_scen.rds")
    #     } else
    #         if (input$DefOrUserUpload_S == 'Upload data') {
    #             file5 <- input$Spatial_file_chan
    #             if (is.null(file5)) {
    #                 return()
    #             }
    #             sf::st_read(file5$datapath)
    #         }
    #     
    # })
    
    
    output$Spatial_wshed <- renderUI({
        if (input$DefOrUserUpload_S == 'Upload data') {
            req(Spatial_data())
            pickerInput(
                "S_wshed",
                "Select the watershed of interest",
                choices = unique(Spatial_data()$Watershed),
                options = list(`actions-box` = TRUE),
                selected = unique(Spatial_data()$Watershed)[1],
                multiple = T
            )
        } else
            if (input$DefOrUserUpload_S == 'Default Data') {
                pickerInput(
                    inputId = "S_wshed",
                    label = "Select the watershed of interest",
                    choices =  unique(Spatial_data()$Watershed),
                    options = list(`actions-box` = TRUE),
                    selected = unique(Spatial_data()$Watershed)[19],
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
                multiple = F
            )
        } else
            if (input$DefOrUserUpload_S == 'Default Data') {
                pickerInput(
                    inputId = "S_scen",
                    label = "Select the scenario of interest",
                    choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                                 "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                                 "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                                 "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                                 "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                                 "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                                 "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                                 "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                                 "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                                 "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                                 "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                        
                    ),
                    unique(Spatial_data()$Scenario)[1],
                    multiple = F
                )
                
            }
        
    })
    
    
    output$Spatial_scen_base <- renderUI({
        if (input$DefOrUserUpload_S == 'Upload data') {
            req(Spatial_data())
            pickerInput(
                "S_scen_base",
                "Select the baseline scenario",
                unique(Spatial_data()$Scenario),
                unique(Spatial_data()$Scenario)[1],
                multiple = F
            )
        } else
            if (input$DefOrUserUpload_S == 'Default Data') {
                pickerInput(
                    inputId = "S_scen_base",
                    label = "Select the baseline scenario",
                    choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                                 "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                                 "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                                 "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                                 "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                                 "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                                 "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                                 "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                                 "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                                 "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                                 "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                                 
                    ),
                    unique(Spatial_data()$Scenario)[1],
                    multiple = F
                )
                
            }
        
    })
    
    
    output$Spatial_scen_comp <- renderUI({
        if (input$DefOrUserUpload_S == 'Upload data') {
            req(Spatial_data())
            pickerInput(
                "S_scen_comp",
                "Select the scenario to compare",
                unique(Spatial_data()$Scenario),
                unique(Spatial_data()$Scenario)[2],
                multiple = F
            )
        } else
            if (input$DefOrUserUpload_S == 'Default Data') {
                pickerInput(
                    inputId = "S_scen_comp",
                    label = "Select the scenario to compare",
                    choices =  c("Current conditions" = "CurCond.2020.ki5krcs.chn_cs12",
                                 "Thinning-85%" = "Thinn85.2020.ki5krcs.chn_12",
                                 "Thinning-93%" = "Thinn93.2020.ki5krcs.chn_12",
                                 "Thinning-96%" = "Thinn96.2020.ki5krcs.chn_12",
                                 "Low severity fire" = "LowSevS.2020.ki5krcs.chn_12",
                                 "Moderate severity fire" = "ModSevS.2020.ki5krcs.chn_12",
                                 "High severity fire" = "HighSevS.2020.ki5krcs.chn_12",
                                 "Prescribed fire" = "PrescFireS.2020.ki5krcs.chn_12",
                                 "Simulated fire-fccsFuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli",
                                 "Simulated fire-landis fuels-observed climate" = "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli",
                                 "Simulated fire-landis fuels-future climate-A2" = "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"
                                 
                    ),
                    unique(Spatial_data()$Scenario)[2],
                    multiple = F
                )
                
            }
        
    })
    
    
    output$S_var <- renderUI({
        if (input$DefOrUserUpload_S == 'Upload data') {
            req(Spatial_data())
            pickerInput(
                "S_variable",
                "Select the variable of interest",
                colnames(Spatial_data()),
                selected = colnames(Spatial_data())[1],
                multiple = F
            )
        } else
            if (input$DefOrUserUpload_S == 'Default Data') {
                pickerInput(
                    inputId = "S_variable",
                    label = "Select the variable of interest",
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
                    multiple = F
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
                Wshed_data() %>% dplyr::filter(Scenario %in% input$Wshed_wshed)
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
    
    Spatial_subset_comp <- reactive({
        req(Spatial_data())
        req(input$S_wshed)
        Spatial_data() %>%
            dplyr::filter(Watershed %in% input$S_wshed &
                              Scenario %in% input$S_scen_comp)%>% 
            arrange_at(.vars = input$S_variable, desc) %>%
            mutate(cumPercArea = cumsum(area_ha_) / sum(area_ha_) *
                       100) %>% dplyr::mutate_if(is.numeric, round, 2) %>% 
            dplyr::filter(cumPercArea < input$thresh_S  & slope > min(input$thresh_slope_S) & slope < max(input$thresh_slope_S) )
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
    
    hill_subset_rel <- reactive({
        hill_subset() %>% dplyr::group_by(WeppID)  %>%
        dplyr::mutate(RelRunoff..mm. = Runoff..mm.- Runoff..mm.[Scenario==input$S_scen_base],
               RelLateralflow.mm = Lateral.Flow..mm.- Lateral.Flow..mm.[Scenario==input$S_scen_base],
               RelBaseflow.mm  = Baseflow..mm.- Baseflow..mm.[Scenario==input$S_scen_base],
               RelSoilLoss.kg.ha = Soil.Loss..kg.ha.- Soil.Loss..kg.ha.[Scenario==input$S_scen_base],
               RelSedDep.kg.ha = Sediment.Deposition..kg.ha.- Sediment.Deposition..kg.ha.[Scenario==input$S_scen_base],
               RelSedYield.kg.ha = Sediment.Yield..kg.ha.- Sediment.Yield..kg.ha.[Scenario==input$S_scen_base],
               RelSRP.kg.ha.3 = Solub..React..P..kg.ha.3.- Solub..React..P..kg.ha.3.[Scenario==input$S_scen_base],
               RelParticulateP.kg.ha.3 = Particulate.P..kg.ha.3.- Particulate.P..kg.ha.3.[Scenario==input$S_scen_base],
               RelTotalP.kg.ha.3 = Total.P..kg.ha.3.- Total.P..kg.ha.3.[Scenario==input$S_scen_base],
               RelParticle.Class.1.Fraction = Particle.Class.1.Fraction- Particle.Class.1.Fraction[Scenario==input$S_scen_base],
               RelParticle.Class.2.Fraction = Particle.Class.2.Fraction- Particle.Class.2.Fraction[Scenario==input$S_scen_base],
               RelParticle.Class.3.Fraction = Particle.Class.3.Fraction- Particle.Class.3.Fraction[Scenario==input$S_scen_base],
               RelParticle.Class.4.Fraction = Particle.Class.4.Fraction- Particle.Class.4.Fraction[Scenario==input$S_scen_base],
               RelParticle.Class.5.Fraction = Particle.Class.5.Fraction- Particle.Class.5.Fraction[Scenario==input$S_scen_base],
               RelParticle.Fraction.Under.0.016.mm = Particle.Fraction.Under.0.016.mm- Particle.Fraction.Under.0.016.mm[Scenario==input$S_scen_base],
               RelSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. = Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.- Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.[Scenario==input$S_scen_base]
        )%>% dplyr::ungroup()
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
                cumSoilLoss.kg.ha = cumsum(Soil.Loss..kg.ha.) / sum(Soil.Loss..kg.ha.) *
                    100,
                cumSedDep.kg.ha = cumsum(Sediment.Deposition..kg.ha.) /
                    sum(Sediment.Deposition..kg.ha.) * 100,
                cumSedYield.kg.ha = cumsum(Sediment.Yield..kg.ha.) /
                    sum(Sediment.Yield..kg.ha.) * 100,
                cumSRP.kg.ha.3 = cumsum(Solub..React..P..kg.ha.3.) /
                    sum(Solub..React..P..kg.ha.3.) * 100,
                cumParticulateP.kg.ha.3 = cumsum(Particulate.P..kg.ha.3.) /
                    sum(Particulate.P..kg.ha.3.) * 100,
                cumTotalP.kg.ha.3 = cumsum(Total.P..kg.ha.3.) / sum(Total.P..kg.ha.3.) *
                    100,
                cumParticle.Class.1.Fraction = cumsum(Particle.Class.1.Fraction) /
                    sum(Particle.Class.1.Fraction) * 100,
                cumParticle.Class.2.Fraction = cumsum(Particle.Class.2.Fraction) /
                    sum(Particle.Class.2.Fraction) * 100,
                cumParticle.Class.3.Fraction = cumsum(Particle.Class.3.Fraction) /
                    sum(Particle.Class.3.Fraction) * 100,
                cumParticle.Class.4.Fraction = cumsum(Particle.Class.4.Fraction) /
                    sum(Particle.Class.4.Fraction) * 100,
                cumParticle.Class.5.Fraction = cumsum(Particle.Class.5.Fraction) /
                    sum(Particle.Class.5.Fraction) * 100,
                cumParticle.Fraction.Under.0.016.mm = cumsum(Particle.Fraction.Under.0.016.mm) /
                    sum(Particle.Fraction.Under.0.016.mm) * 100,
                cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. = cumsum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.) /
                    sum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.) * 100
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
                cumSoilLoss.kg.ha = cumsum(Soil.Loss..kg.ha.) / sum(Soil.Loss..kg.ha.) *
                    100,
                cumSedDep.kg.ha = cumsum(Sediment.Deposition..kg.ha.) /
                    sum(Sediment.Deposition..kg.ha.) * 100,
                cumSedYield.kg.ha = cumsum(Sediment.Yield..kg.ha.) /
                    sum(Sediment.Yield..kg.ha.) * 100,
                cumSRP.kg.ha.3 = cumsum(Solub..React..P..kg.ha.3.) /
                    sum(Solub..React..P..kg.ha.3.) * 100,
                cumParticulateP.kg.ha.3 = cumsum(Particulate.P..kg.ha.3.) /
                    sum(Particulate.P..kg.ha.3.) * 100,
                cumTotalP.kg.ha.3 = cumsum(Total.P..kg.ha.3.) / sum(Total.P..kg.ha.3.) *
                    100,
                cumParticle.Class.1.Fraction = cumsum(Particle.Class.1.Fraction) /
                    sum(Particle.Class.1.Fraction) * 100,
                cumParticle.Class.2.Fraction = cumsum(Particle.Class.2.Fraction) /
                    sum(Particle.Class.2.Fraction) * 100,
                cumParticle.Class.3.Fraction = cumsum(Particle.Class.3.Fraction) /
                    sum(Particle.Class.3.Fraction) * 100,
                cumParticle.Class.4.Fraction = cumsum(Particle.Class.4.Fraction) /
                    sum(Particle.Class.4.Fraction) * 100,
                cumParticle.Class.5.Fraction = cumsum(Particle.Class.5.Fraction) /
                    sum(Particle.Class.5.Fraction) * 100,
                cumParticle.Fraction.Under.0.016.mm = cumsum(Particle.Fraction.Under.0.016.mm) /
                    sum(Particle.Fraction.Under.0.016.mm) * 100,
                cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. = cumsum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.) /
                    sum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.) * 100
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
                cumLateralflow.mm = cumsum(Lateral.Flow..mm.),
                cumBaseflow.mm = cumsum(Baseflow..mm.),
                cumSoilLoss.kg.ha = cumsum(Soil.Loss..kg.ha.),
                cumSedDep.kg.ha = cumsum(Sediment.Deposition..kg.ha.),
                cumSedYield.kg.ha = cumsum(Sediment.Yield..kg.ha.),
                cumSRP.kg.ha.3 = cumsum(Solub..React..P..kg.ha.3.),
                cumParticulateP.kg.ha.3 = cumsum(Particulate.P..kg.ha.3.),
                cumTotalP.kg.ha.3 = cumsum(Total.P..kg.ha.3.),
                cumParticle.Class.1.Fraction = cumsum(Particle.Class.1.Fraction),
                cumParticle.Class.2.Fraction = cumsum(Particle.Class.2.Fraction),
                cumParticle.Class.3.Fraction = cumsum(Particle.Class.3.Fraction),
                cumParticle.Class.4.Fraction = cumsum(Particle.Class.4.Fraction),
                cumParticle.Class.5.Fraction = cumsum(Particle.Class.5.Fraction),
                cumParticle.Fraction.Under.0.016.mm = cumsum(Particle.Fraction.Under.0.016.mm),
                cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. = cumsum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.)
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
                cumLateralflow.mm = cumsum(Lateral.Flow..mm.),
                cumBaseflow.mm = cumsum(Baseflow..mm.),
                cumSoilLoss.kg.ha = cumsum(Soil.Loss..kg.ha.),
                cumSedDep.kg.ha = cumsum(Sediment.Deposition..kg.ha.),
                cumSedYield.kg.ha = cumsum(Sediment.Yield..kg.ha.),
                cumSRP.kg.ha.3 = cumsum(Solub..React..P..kg.ha.3.),
                cumParticulateP.kg.ha.3 = cumsum(Particulate.P..kg.ha.3.),
                cumTotalP.kg.ha.3 = cumsum(Total.P..kg.ha.3.),
                cumParticle.Class.1.Fraction = cumsum(Particle.Class.1.Fraction),
                cumParticle.Class.2.Fraction = cumsum(Particle.Class.2.Fraction),
                cumParticle.Class.3.Fraction = cumsum(Particle.Class.3.Fraction),
                cumParticle.Class.4.Fraction = cumsum(Particle.Class.4.Fraction),
                cumParticle.Class.5.Fraction = cumsum(Particle.Class.5.Fraction),
                cumParticle.Fraction.Under.0.016.mm = cumsum(Particle.Fraction.Under.0.016.mm),
                cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. = cumsum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.)
            )%>% dplyr::filter(Scenario %in% c(input$Hill_scen_base,  input$Hill_scen_comp)) %>% dplyr::filter(cumPercLen < input$thresh_H) %>%
            ungroup()
    })
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------Dataframe Calculations for Channels-------------------------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    
    
    ############## Dataframe calculating cumulative percent of total variable: Channel   ##############
    #### df for plot thresholded by percent of channel area
    # chn_arr_by_var_CA <- reactive({
    #     Chan_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$Chan_variable, desc) %>%
    #         mutate(
    #             cumPercChanArea = cumsum(Channel.Area..ha.) / sum(Channel.Area..ha.) * 100,
    #             cumPercLen = cumsum(Length..m.) / sum(Length..m.) * 100,
    #             cumPercContriChanArea = cumsum(Contributing.Channel.Area..ha.) /
    #                 sum(Contributing.Channel.Area..ha.) * 100,
    #             cumDischarge.mm = cumsum(Discharge..mm.) / sum(Discharge..mm.) *
    #                 100,
    #             cumSediment.Yield..tonne. = cumsum(Sediment.Yield..tonne.) /
    #                 sum(Sediment.Yield..tonne.) * 100,
    #             cumChannel.Erosion..tonne. = cumsum(Channel.Erosion..tonne.) /
    #                 sum(Channel.Erosion..tonne.) * 100,
    #             cumUpland.Charge..mm. = cumsum(Upland.Charge..mm.) /
    #                 sum(Upland.Charge..mm.) * 100,
    #             cumLateral.Flow..mm. = cumsum(Lateral.Flow..mm.) / sum(Lateral.Flow..mm.) *
    #                 100,
    #             cumSRP.kg.ha. = cumsum(Solub..React..P..kg.ha.) / sum(Solub..React..P..kg.ha.) *
    #                 100,
    #             cumParticulateP.kg.ha. = cumsum(Particulate.P..kg.ha.) /
    #                 sum(Particulate.P..kg.ha.) * 100,
    #             cumTotalP.kg.ha. = cumsum(Total.P..kg.ha.) / sum(Total.P..kg.ha.) *
    #                 100
    #         ) %>%
    #         dplyr::filter(cumPercChanArea < input$thresh_C) %>%
    #         ungroup()
    # })
    # 
    # 
    # #### df for plot thresholded by percent of channel Length
    # chn_arr_by_var_CL <- reactive({
    #     Chan_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$Chan_variable, desc) %>%
    #         mutate(
    #             cumPercChanArea = cumsum(Channel.Area..ha.) / sum(Channel.Area..ha.) * 100,
    #             cumPercLen = cumsum(Length..m.) / sum(Length..m.) * 100,
    #             cumPercContriChanArea = cumsum(Contributing.Channel.Area..ha.) /
    #                 sum(Contributing.Channel.Area..ha.) * 100,
    #             cumDischarge.mm = cumsum(Discharge..mm.) / sum(Discharge..mm.) *
    #                 100,
    #             cumSediment.Yield..tonne. = cumsum(Sediment.Yield..tonne.) /
    #                 sum(Sediment.Yield..tonne.) * 100,
    #             cumChannel.Erosion..tonne. = cumsum(Channel.Erosion..tonne.) /
    #                 sum(Channel.Erosion..tonne.) * 100,
    #             cumUpland.Charge..mm. = cumsum(Upland.Charge..mm.) /
    #                 sum(Upland.Charge..mm.) * 100,
    #             cumLateral.Flow..mm. = cumsum(Lateral.Flow..mm.) / sum(Lateral.Flow..mm.) *
    #                 100,
    #             cumSRP.kg.ha. = cumsum(Solub..React..P..kg.ha.) / sum(Solub..React..P..kg.ha.) *
    #                 100,
    #             cumParticulateP.kg.ha. = cumsum(Particulate.P..kg.ha.) /
    #                 sum(Particulate.P..kg.ha.) * 100,
    #             cumTotalP.kg.ha. = cumsum(Total.P..kg.ha.) / sum(Total.P..kg.ha.) *
    #                 100
    #         ) %>%
    #         dplyr::filter(cumPercLen < input$thresh_C) %>%
    #         ungroup()
    # })
    # 
    # 
    # ############## Dataframe calculating cumulative absolute value of variable: Channel   ##############
    # #### df for plot thresholded by percent of channel area with abs numbers
    # chn_arr_by_var_CA_abs <- reactive({
    #     Chan_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$Chan_variable, desc) %>%
    #         mutate(
    #             cumPercChanArea = cumsum(Channel.Area..ha.) / sum(Channel.Area..ha.) * 100,
    #             cumPercLen = cumsum(Length..m.) / sum(Length..m.) * 100,
    #             cumPercContriChanArea = cumsum(Contributing.Channel.Area..ha.) /
    #                 sum(Contributing.Channel.Area..ha.) * 100,
    #             ### fix total contributing area/ already a cumulative avalue in the channel file
    #             cumDischarge.mm = cumsum(Discharge..mm.),
    #             cumSediment.Yield..tonne. = cumsum(Sediment.Yield..tonne.),
    #             cumChannel.Erosion..tonne. = cumsum(Channel.Erosion..tonne.),
    #             cumUpland.Charge..mm. = cumsum(Upland.Charge..mm.),
    #             cumLateral.Flow..mm. = cumsum(Lateral.Flow..mm.),
    #             cumSRP.kg.ha. = cumsum(Solub..React..P..kg.ha.),
    #             cumParticulateP.kg.ha. = cumsum(Particulate.P..kg.ha.),
    #             cumTotalP.kg.ha. = cumsum(Total.P..kg.ha.)
    #         ) %>%
    #         dplyr::filter(cumPercChanArea < input$thresh_C) %>%
    #         ungroup()
    # })
    # 
    # #### df for plot thresholded by percent of channel area with abs numbers
    # chn_arr_by_var_CL_abs <- reactive({
    #     Chan_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$Chan_variable, desc) %>%
    #         mutate(
    #             cumPercChanArea = cumsum(Channel.Area..ha.) / sum(Channel.Area..ha.) * 100,
    #             cumPercLen = cumsum(Length..m.) / sum(Length..m.) * 100,
    #             cumPercContriChanArea = cumsum(Contributing.Channel.Area..ha.) /
    #                 sum(Contributing.Channel.Area..ha.) * 100,
    #             ### fix total contributing area/ already a cumulative avalue in the channel file
    #             cumDischarge.mm = cumsum(Discharge..mm.),
    #             cumSediment.Yield..tonne. = cumsum(Sediment.Yield..tonne.),
    #             cumChannel.Erosion..tonne. = cumsum(Channel.Erosion..tonne.),
    #             cumUpland.Charge..mm. = cumsum(Upland.Charge..mm.),
    #             cumLateral.Flow..mm. = cumsum(Lateral.Flow..mm.),
    #             cumSRP.kg.ha. = cumsum(Solub..React..P..kg.ha.),
    #             cumParticulateP.kg.ha. = cumsum(Particulate.P..kg.ha.),
    #             cumTotalP.kg.ha. = cumsum(Total.P..kg.ha.)
    #         ) %>%
    #         dplyr::filter(cumPercLen < input$thresh_C) %>%
    #         ungroup()
    # })
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## ---------------------------------Plots:Hillslopes-------------------------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    
    # ############## plots of cumulative percent of total variable   ##############
    # ############## vs cumulative percent of total hillslope area/ channel length   ##############
    #
    
    
    output$Plot_vs_cumPercArea <- renderPlotly({
        req(input$Hill_variable)
        
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
                    if (input$Hill_variable == "Soil.Loss..kg.ha.") {
                        p1 <-
                            p1 + geom_line(aes(y = cumSoilLoss.kg.ha, color = Scenario), size = 0.5)
                    } else
                        if (input$Hill_variable == "Sediment.Deposition..kg.ha.") {
                            p1 <-
                                p1 + geom_line(aes(y = cumSedDep.kg.ha, color = Scenario), size = 0.5)
                        } else
                            if (input$Hill_variable == "Sediment.Yield..kg.ha.") {
                                p1 <-
                                    p1 + geom_line(aes(y = cumSedYield.kg.ha, color = Scenario), size = 0.5)
                            } else
                                if (input$Hill_variable == "Solub..React..P..kg.ha.3.") {
                                    p1 <-
                                        p1 + geom_line(aes(y = cumSRP.kg.ha.3, color = Scenario), size = 0.5)
                                } else
                                    if (input$Hill_variable == "Particulate.P..kg.ha.3.") {
                                        p1 <-
                                            p1 + geom_line(aes(y = cumParticulateP.kg.ha.3, color = Scenario),
                                                           size = 0.5)
                                    } else
                                        if (input$Hill_variable == "Total.P..kg.ha.3.") {
                                            p1 <-
                                                p1 + geom_line(aes(y = cumTotalP.kg.ha.3, color = Scenario), size = 0.5)
                                        } else
                                            if (input$Hill_variable == "Particle.Class.1.Fraction") {
                                                p1 <-
                                                    p1 + geom_line(aes(y = cumParticle.Class.1.Fraction, color = Scenario),
                                                                   size = 0.5)
                                            } else
                                                if (input$Hill_variable == "Particle.Class.2.Fraction") {
                                                    p1 <-
                                                        p1 + geom_line(aes(y = cumParticle.Class.2.Fraction, color = Scenario),
                                                                       size = 0.5)
                                                } else
                                                    if (input$Hill_variable == "Particle.Class.3.Fraction") {
                                                        p1 <-
                                                            p1 + geom_line(aes(y = cumParticle.Class.3.Fraction, color = Scenario),
                                                                           size = 0.5)
                                                    } else
                                                        if (input$Hill_variable == "Particle.Class.4.Fraction") {
                                                            p1 <-
                                                                p1 + geom_line(aes(y = cumParticle.Class.4.Fraction, color = Scenario),
                                                                               size = 0.5)
                                                        } else
                                                            if (input$Hill_variable == "Particle.Class.5.Fraction") {
                                                                p1 <-
                                                                    p1 + geom_line(aes(y = cumParticle.Class.5.Fraction, color = Scenario),
                                                                                   size = 0.5)
                                                            } else
                                                                if (input$Hill_variable == "Particle.Fraction.Under.0.016.mm") {
                                                                    p1 <-
                                                                        p1 + geom_line(aes(y = cumParticle.Fraction.Under.0.016.mm, color = Scenario),
                                                                                       size = 0.5)
                                                                } else
                                                                    if (input$Hill_variable == "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.") {
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
                y = "Percent of total selected variable",
                # y = paste("Percent of total", input$Hill_variable, sep = " "),
                title= paste(
                    "What percent of total hillslope area\ncontributes a large fraction of total\n",
                    "",
                    input$Hill_variable ,
                    "?"
                )
                ,
                colour = "Scenario"
            )
        if (input$DefOrUserUpload_H == 'Default Data') {
            p1 <- p1 +
                scale_color_manual(
                    values = c(
                        "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2" = "#FF0000",
                        "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli" =
                            "#B22222",
                        "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
                        "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
                        "ModSevS.2020.ki5krcs.chn_12" =
                            "#DC143C",
                        "LowSevS.2020.ki5krcs.chn_12" =
                            "#FF6347",
                        "PrescFireS.2020.ki5krcs.chn_12" =
                            "#E9967A",
                        "Thinn85.2020.ki5krcs.chn_12" =
                            "#7CFC00",
                        "Thinn93.2020.kikrcs.chn" =
                            "#32CD32",
                        "Thinn96.2020.kikrcs.chn" =
                            "#00FF00",
                        "CurCond.2020.ki5krcs.chn_cs12" =
                            "#008000"
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
                    if (input$Hill_variable == "Soil.Loss..kg.ha.") {
                        p3 <-
                            p3 + geom_line(aes(y = cumSoilLoss.kg.ha, color = Scenario), size = 0.5)
                    } else
                        if (input$Hill_variable == "Sediment.Deposition..kg.ha.") {
                            p3 <-
                                p3 + geom_line(aes(y = cumSedDep.kg.ha, color = Scenario), size = 0.5)
                        } else
                            if (input$Hill_variable == "Sediment.Yield..kg.ha.") {
                                p3 <-
                                    p3 + geom_line(aes(y = cumSedYield.kg.ha, color = Scenario), size = 0.5)
                            } else
                                if (input$Hill_variable == "Solub..React..P..kg.ha.3.") {
                                    p3 <-
                                        p3 + geom_line(aes(y = cumSRP.kg.ha.3, color = Scenario), size = 0.5)
                                } else
                                    if (input$Hill_variable == "Particulate.P..kg.ha.3.") {
                                        p3 <-
                                            p3 + geom_line(aes(y = cumParticulateP.kg.ha.3, color = Scenario),
                                                           size = 0.5)
                                    } else
                                        if (input$Hill_variable == "Total.P..kg.ha.3.") {
                                            p3 <-
                                                p3 + geom_line(aes(y = cumTotalP.kg.ha.3, color = Scenario), size = 0.5)
                                        } else
                                            if (input$Hill_variable == "Particle.Class.1.Fraction") {
                                                p3 <-
                                                    p3 + geom_line(aes(y = cumParticle.Class.1.Fraction, color = Scenario),
                                                                   size = 0.5)
                                            } else
                                                if (input$Hill_variable == "Particle.Class.2.Fraction") {
                                                    p3 <-
                                                        p3 + geom_line(aes(y = cumParticle.Class.2.Fraction, color = Scenario),
                                                                       size = 0.5)
                                                } else
                                                    if (input$Hill_variable == "Particle.Class.3.Fraction") {
                                                        p3 <-
                                                            p3 + geom_line(aes(y = cumParticle.Class.3.Fraction, color = Scenario),
                                                                           size = 0.5)
                                                    } else
                                                        if (input$Hill_variable == "Particle.Class.4.Fraction") {
                                                            p3 <-
                                                                p3 + geom_line(aes(y = cumParticle.Class.4.Fraction, color = Scenario),
                                                                               size = 0.5)
                                                        } else
                                                            if (input$Hill_variable == "Particle.Class.5.Fraction") {
                                                                p3 <-
                                                                    p3 + geom_line(aes(y = cumParticle.Class.5.Fraction, color = Scenario),
                                                                                   size = 0.5)
                                                            } else
                                                                if (input$Hill_variable == "Particle.Fraction.Under.0.016.mm") {
                                                                    p3 <-
                                                                        p3 + geom_line(aes(y = cumParticle.Fraction.Under.0.016.mm, color = Scenario),
                                                                                       size = 0.5)
                                                                } else
                                                                    if (input$Hill_variable == "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.") {
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
                y = "Percent of total selected variable",
                # y = paste("Percent of total", input$Hill_variable, sep = " "),
                title= paste(
                    "What percent of total channel length\ncontributes a large fraction of total\n",
                    "",
                    input$Hill_variable ,
                    "?"
                ),
                colour = "Scenario"
            )
        
        if (input$DefOrUserUpload_H == 'Default Data') {
            p3 <- p3 +
                scale_color_manual(
                    values = c(
                        "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2" = "#FF0000",
                        "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli" =
                            "#B22222",
                        "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
                        "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
                        "ModSevS.2020.ki5krcs.chn_12" =
                            "#DC143C",
                        "LowSevS.2020.ki5krcs.chn_12" =
                            "#FF6347",
                        "PrescFireS.2020.ki5krcs.chn_12" =
                            "#E9967A",
                        "Thinn85.2020.ki5krcs.chn_12" =
                            "#7CFC00",
                        "Thinn93.2020.kikrcs.chn_12" =
                            "#32CD32",
                        "Thinn96.2020.kikrcs.chn_12" =
                            "#00FF00",
                        "CurCond.2020.ki5krcs.chn_cs12" =
                            "#008000"
                    )
                )
        } else
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
                    if (input$Hill_variable == "Soil.Loss..kg.ha.") {
                        p2 <-
                            p2 + geom_line(aes(y = cumSoilLoss.kg.ha, color = Scenario), size = 0.5)
                    } else
                        if (input$Hill_variable == "Sediment.Deposition..kg.ha.") {
                            p2 <-
                                p2 + geom_line(aes(y = cumSedDep.kg.ha, color = Scenario), size = 0.5)
                        } else
                            if (input$Hill_variable == "Sediment.Yield..kg.ha.") {
                                p2 <-
                                    p2 + geom_line(aes(y = cumSedYield.kg.ha, color = Scenario), size = 0.5)
                            } else
                                if (input$Hill_variable == "Solub..React..P..kg.ha.3.") {
                                    p2 <-
                                        p2 + geom_line(aes(y = cumSRP.kg.ha.3, color = Scenario), size = 0.5)
                                } else
                                    if (input$Hill_variable == "Particulate.P..kg.ha.3.") {
                                        p2 <-
                                            p2 + geom_line(aes(y = cumParticulateP.kg.ha.3, color = Scenario),
                                                           size = 0.5)
                                    } else
                                        if (input$Hill_variable == "Total.P..kg.ha.3.") {
                                            p2 <-
                                                p2 + geom_line(aes(y = cumTotalP.kg.ha.3, color = Scenario), size = 0.5)
                                        } else
                                            if (input$Hill_variable == "Particle.Class.1.Fraction") {
                                                p2 <-
                                                    p2 + geom_line(aes(y = cumParticle.Class.1.Fraction, color = Scenario),
                                                                   size = 0.5)
                                            } else
                                                if (input$Hill_variable == "Particle.Class.2.Fraction") {
                                                    p2 <-
                                                        p2 + geom_line(aes(y = cumParticle.Class.2.Fraction, color = Scenario),
                                                                       size = 0.5)
                                                } else
                                                    if (input$Hill_variable == "Particle.Class.3.Fraction") {
                                                        p2 <-
                                                            p2 + geom_line(aes(y = cumParticle.Class.3.Fraction, color = Scenario),
                                                                           size = 0.5)
                                                    } else
                                                        if (input$Hill_variable == "Particle.Class.4.Fraction") {
                                                            p2 <-
                                                                p2 + geom_line(aes(y = cumParticle.Class.4.Fraction, color = Scenario),
                                                                               size = 0.5)
                                                        } else
                                                            if (input$Hill_variable == "Particle.Class.5.Fraction") {
                                                                p2 <-
                                                                    p2 + geom_line(aes(y = cumParticle.Class.5.Fraction, color = Scenario),
                                                                                   size = 0.5)
                                                            } else
                                                                if (input$Hill_variable == "Particle.Fraction.Under.0.016.mm") {
                                                                    p2 <-
                                                                        p2 + geom_line(aes(y = cumParticle.Fraction.Under.0.016.mm, color = Scenario),
                                                                                       size = 0.5)
                                                                } else
                                                                    if (input$Hill_variable == "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.") {
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
                y = "Percent of total selected variable",
                # y = paste("Cumulative", input$Hill_variable, sep = " "),
                title= paste(
                    "What percent of total hillslope area\ncontributes a large fraction of cumulative\n",
                    "",
                    input$Hill_variable ,
                    "?"
                ),
                colour = "Scenario"
            )
        if (input$DefOrUserUpload_H == 'Default Data') {
            p2 <- p2 +
                scale_color_manual(
                    values = c(
                        "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2" = "#FF0000",
                        "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli" =
                            "#B22222",
                        "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
                        "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
                        "ModSevS.2020.ki5krcs.chn_12" =
                            "#DC143C",
                        "LowSevS.2020.ki5krcs.chn_12" =
                            "#FF6347",
                        "PrescFireS.2020.ki5krcs.chn_12" =
                            "#E9967A",
                        "Thinn85.2020.ki5krcs.chn_12" =
                            "#7CFC00",
                        "Thinn93.2020.kikrcs.chn_12" =
                            "#32CD32",
                        "Thinn96.2020.kikrcs.chn_12" =
                            "#00FF00",
                        "CurCond.2020.ki5krcs.chn_cs12" =
                            "#008000"
                    )
                )
        } else
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
                    if (input$Hill_variable == "Soil.Loss..kg.ha.") {
                        p4 <-
                            p4 + geom_line(aes(y = cumSoilLoss.kg.ha, color = Scenario), size = 0.5)
                    } else
                        if (input$Hill_variable == "Sediment.Deposition..kg.ha.") {
                            p4 <-
                                p4 + geom_line(aes(y = cumSedDep.kg.ha, color = Scenario), size = 0.5)
                        } else
                            if (input$Hill_variable == "Sediment.Yield..kg.ha.") {
                                p4 <-
                                    p4 + geom_line(aes(y = cumSedYield.kg.ha, color = Scenario), size = 0.5)
                            } else
                                if (input$Hill_variable == "Solub..React..P..kg.ha.3.") {
                                    p4 <-
                                        p4 + geom_line(aes(y = cumSRP.kg.ha.3, color = Scenario), size = 0.5)
                                } else
                                    if (input$Hill_variable == "Particulate.P..kg.ha.3.") {
                                        p4 <-
                                            p4 + geom_line(aes(y = cumParticulateP.kg.ha.3, color = Scenario),
                                                           size = 0.5)
                                    } else
                                        if (input$Hill_variable == "Total.P..kg.ha.3.") {
                                            p4 <-
                                                p4 + geom_line(aes(y = cumTotalP.kg.ha.3, color = Scenario), size = 0.5)
                                        } else
                                            if (input$Hill_variable == "Particle.Class.1.Fraction") {
                                                p4 <-
                                                    p4 + geom_line(aes(y = cumParticle.Class.1.Fraction, color = Scenario),
                                                                   size = 0.5)
                                            } else
                                                if (input$Hill_variable == "Particle.Class.2.Fraction") {
                                                    p4 <-
                                                        p4 + geom_line(aes(y = cumParticle.Class.2.Fraction, color = Scenario),
                                                                       size = 0.5)
                                                } else
                                                    if (input$Hill_variable == "Particle.Class.3.Fraction") {
                                                        p4 <-
                                                            p4 + geom_line(aes(y = cumParticle.Class.3.Fraction, color = Scenario),
                                                                           size = 0.5)
                                                    } else
                                                        if (input$Hill_variable == "Particle.Class.4.Fraction") {
                                                            p4 <-
                                                                p4 + geom_line(aes(y = cumParticle.Class.4.Fraction, color = Scenario),
                                                                               size = 0.5)
                                                        } else
                                                            if (input$Hill_variable == "Particle.Class.5.Fraction") {
                                                                p4 <-
                                                                    p4 + geom_line(aes(y = cumParticle.Class.5.Fraction, color = Scenario),
                                                                                   size = 0.5)
                                                            } else
                                                                if (input$Hill_variable == "Particle.Fraction.Under.0.016.mm") {
                                                                    p4 <-
                                                                        p4 + geom_line(aes(y = cumParticle.Fraction.Under.0.016.mm, color = Scenario),
                                                                                       size = 0.5)
                                                                } else
                                                                    if (input$Hill_variable == "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.") {
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
                y = "Percent of total selected variable",
                # y = paste("Cumulative", input$Hill_variable, sep = " "),
                title= paste(
                    "What percent of total channel length \ncontributes a large fraction of cumulative\n",
                    "",
                    input$Hill_variable ,
                    "?"
                ),
                colour = "Scenario"
            )
        if (input$DefOrUserUpload_H == 'Default Data') {
            p4 <- p4 +
                scale_color_manual(
                    values = c(
                        "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2" = "#FF0000",
                        "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli" =
                            "#B22222",
                        "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
                        "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
                        "ModSevS.2020.ki5krcs.chn_12" =
                            "#DC143C",
                        "LowSevS.2020.ki5krcs.chn_12" =
                            "#FF6347",
                        "PrescFireS.2020.ki5krcs.chn_12" =
                            "#E9967A",
                        "Thinn85.2020.ki5krcs.chn_12" =
                            "#7CFC00",
                        "Thinn93.2020.kikrcs.chn_12" =
                            "#32CD32",
                        "Thinn96.2020.kikrcs.chn_12" =
                            "#00FF00",
                        "CurCond.2020.ki5krcs.chn_cs12" =
                            "#008000"
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
    ## ---------------------------------Plots:Channels-------------------------------------------------------##
    ## -----------------------------------------------------------------------------------------------------------##
    
    # output$Plot6 <- renderPlotly({
    #     req(input$Chan_variable)
    #     p6 <-
    #         chn_arr_by_var_CA() %>% ggplot(aes(x = cumPercChanArea))
    #     if (input$Chan_variable ==  "Discharge..mm.") {
    #         p6 <-
    #             p6 + geom_line(aes(y = cumDischarge.mm  , color = Scenario), size = 0.5)
    #     } else
    #         if (input$Chan_variable ==  "Sediment.Yield..tonne.") {
    #             p6 <-
    #                 p6 + geom_line(aes(y = cumSediment.Yield..tonne.  , color = Scenario),
    #                                size = 0.5)
    #         } else
    #             if (input$Chan_variable ==  "Channel.Erosion..tonne.") {
    #                 p6 <-
    #                     p6 + geom_line(aes(y = cumChannel.Erosion..tonne.  , color = Scenario),
    #                                    size = 0.5)
    #             } else
    #                 if (input$Chan_variable ==  "Upland.Charge..mm.") {
    #                     p6 <-
    #                         p6 + geom_line(aes(y = cumUpland.Charge..mm.  , color = Scenario),
    #                                        size = 0.5)
    #                 } else
    #                     if (input$Chan_variable ==  "Lateral.Flow..mm.") {
    #                         p6 <-
    #                             p6 + geom_line(aes(y = cumLateral.Flow..mm.  , color = Scenario),
    #                                            size = 0.5)
    #                     } else
    #                         if (input$Chan_variable ==  "Solub..React..P..kg.ha.") {
    #                             p6 <-
    #                                 p6 + geom_line(aes(y = cumSRP.kg.ha.  , color = Scenario), size = 0.5)
    #                         } else
    #                             if (input$Chan_variable ==  "Particulate.P..kg.ha.") {
    #                                 p6 <-
    #                                     p6 + geom_line(aes(y = cumParticulateP.kg.ha.  , color = Scenario),
    #                                                    size = 0.5)
    #                             } else
    #                                 if (input$Chan_variable ==  "Total.P..kg.ha.") {
    #                                     p6 <-
    #                                         p6 + geom_line(aes(y = cumTotalP.kg.ha.  , color = Scenario), size = 0.5)
    #                                 }
    #     
    #     p6 <- p6 +  theme_bw() +
    #         theme(
    #             axis.title = element_text(
    #                 size = 10,
    #                 color = "Black",
    #                 face = "bold"
    #             ),
    #             axis.text = element_text(
    #                 size = 10,
    #                 color = "BLACK",
    #                 face = "bold"
    #             ),
    #             legend.title = element_text(
    #                 size = 10,
    #                 color = "BLACK",
    #                 face = "bold"
    #             ),
    #             legend.text = element_text(size = 10, color = "BLACK"),
    #             legend.position = "none"
    #         ) +
    #         labs(
    #             x = "Percent of total channel area",
    #             y = paste("Percent of total ", input$Chan_variable, sep = " "),
    #             title = "",
    #             colour = "Scenario"
    #         )
    #     if (input$DefOrUserUpload_C == 'Default Data') {
    #         p6 <- p6 +
    #             scale_color_manual(
    #                 values = c(
    #                     "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2" = "#FF0000",
    #                     "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli" =
    #                         "#B22222",
    #                     "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
    #                     "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
    #                     "ModSevS.2020.ki5krcs.chn_12" =
    #                         "#DC143C",
    #                     "LowSevS.2020.ki5krcs.chn_12" =
    #                         "#FF6347",
    #                     "PrescFireS.2020.ki5krcs.chn_12" =
    #                         "#E9967A",
    #                     "Thinn85.2020.ki5krcs.chn_12" =
    #                         "#7CFC00",
    #                     "Thinn93.2020.kikrcs.chn_12" =
    #                         "#32CD32",
    #                     "Thinn96.2020.kikrcs.chn_12" =
    #                         "#00FF00",
    #                     "CurCond.2020.ki5krcs.chn_cs12" =
    #                         "#008000"
    #                 )
    #             )
    #     } else
    #         if (input$DefOrUserUpload_C == 'Upload data') {
    #             p6 <- p6 +
    #                 scale_color_brewer(palette = "virdis")
    #         }
    #     
    #     
    #     
    #     p6
    #     
    # })
    # 
    # 
    # output$Plot6_abs <- renderPlotly({
    #     req(input$Chan_variable)
    #     p6 <-
    #         chn_arr_by_var_CA_abs() %>% ggplot(aes(x = cumPercChanArea))
    #     if (input$Chan_variable ==  "Discharge..mm.") {
    #         p6 <-
    #             p6 + geom_line(aes(y = cumDischarge.mm  , color = Scenario), size = 0.5)
    #     } else
    #         if (input$Chan_variable ==  "Sediment.Yield..tonne.") {
    #             p6 <-
    #                 p6 + geom_line(aes(y = cumSediment.Yield..tonne.  , color = Scenario),
    #                                size = 0.5)
    #         } else
    #             if (input$Chan_variable ==  "Channel.Erosion..tonne.") {
    #                 p6 <-
    #                     p6 + geom_line(aes(y = cumChannel.Erosion..tonne.  , color = Scenario),
    #                                    size = 0.5)
    #             } else
    #                 if (input$Chan_variable ==  "Upland.Charge..mm.") {
    #                     p6 <-
    #                         p6 + geom_line(aes(y = cumUpland.Charge..mm.  , color = Scenario),
    #                                        size = 0.5)
    #                 } else
    #                     if (input$Chan_variable ==  "Lateral.Flow..mm.") {
    #                         p6 <-
    #                             p6 + geom_line(aes(y = cumLateral.Flow..mm.  , color = Scenario),
    #                                            size = 0.5)
    #                     } else
    #                         if (input$Chan_variable ==  "Solub..React..P..kg.ha.") {
    #                             p6 <-
    #                                 p6 + geom_line(aes(y = cumSRP.kg.ha.  , color = Scenario), size = 0.5)
    #                         } else
    #                             if (input$Chan_variable ==  "Particulate.P..kg.ha.") {
    #                                 p6 <-
    #                                     p6 + geom_line(aes(y = cumParticulateP.kg.ha.  , color = Scenario),
    #                                                    size = 0.5)
    #                             } else
    #                                 if (input$Chan_variable ==  "Total.P..kg.ha.") {
    #                                     p6 <-
    #                                         p6 + geom_line(aes(y = cumTotalP.kg.ha.  , color = Scenario), size = 0.5)
    #                                 }
    #     
    #     p6 <- p6 +  theme_bw() +
    #         theme(
    #             axis.title = element_text(
    #                 size = 10,
    #                 color = "Black",
    #                 face = "bold"
    #             ),
    #             axis.text = element_text(
    #                 size = 10,
    #                 color = "BLACK",
    #                 face = "bold"
    #             ),
    #             legend.title = element_text(
    #                 size = 10,
    #                 color = "BLACK",
    #                 face = "bold"
    #             ),
    #             legend.text = element_text(size = 10, color = "BLACK"),
    #             legend.position = "none"
    #         ) +
    #         labs(
    #             x = "Percent of total channel area",
    #             y = paste("Percent of total ", input$Chan_variable, sep = " "),
    #             title = "",
    #             colour = "Scenario"
    #         )
    #     if (input$DefOrUserUpload_C == 'Default Data') {
    #         p6 <- p6 +
    #             scale_color_manual(
    #                 values = c(
    #                     "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2" = "#FF0000",
    #                     "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli" =
    #                         "#B22222",
    #                     "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
    #                     "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
    #                     "ModSevS.2020.ki5krcs.chn_12" =
    #                         "#DC143C",
    #                     "LowSevS.2020.ki5krcs.chn_12" =
    #                         "#FF6347",
    #                     "PrescFireS.2020.ki5krcs.chn_12" =
    #                         "#E9967A",
    #                     "Thinn85.2020.ki5krcs.chn_12" =
    #                         "#7CFC00",
    #                     "Thinn93.2020.kikrcs.chn_12" =
    #                         "#32CD32",
    #                     "Thinn96.2020.kikrcs.chn_12" =
    #                         "#00FF00",
    #                     "CurCond.2020.ki5krcs.chn_cs12" =
    #                         "#008000"
    #                 )
    #             )
    #     } else
    #         if (input$DefOrUserUpload_C == 'Upload data') {
    #             p6 <- p6 +
    #                 scale_color_brewer(palette = "virdis")
    #         }
    #     
    #     
    #     
    #     p6
    #     
    # })
    # 
    # 
    # 
    # 
    # 
    # output$Plot7 <- renderPlotly({
    #     req(input$Chan_variable)
    #     p7 <- chn_arr_by_var_CL() %>% ggplot(aes(x = cumPercLen))
    #     if (input$Chan_variable ==  "Discharge..mm.") {
    #         p7 <-
    #             p7 + geom_line(aes(y = cumDischarge.mm  , color = Scenario), size = 0.5)
    #     } else
    #         if (input$Chan_variable ==  "Sediment.Yield..tonne.") {
    #             p7 <-
    #                 p7 + geom_line(aes(y = cumSediment.Yield..tonne.  , color = Scenario),
    #                                size = 0.5)
    #         } else
    #             if (input$Chan_variable ==  "Channel.Erosion..tonne.") {
    #                 p7 <-
    #                     p7 + geom_line(aes(y = cumChannel.Erosion..tonne.  , color = Scenario),
    #                                    size = 0.5)
    #             } else
    #                 if (input$Chan_variable ==  "Upland.Charge..mm.") {
    #                     p7 <-
    #                         p7 + geom_line(aes(y = cumUpland.Charge..mm.  , color = Scenario),
    #                                        size = 0.5)
    #                 } else
    #                     if (input$Chan_variable ==  "Lateral.Flow..mm.") {
    #                         p7 <-
    #                             p7 + geom_line(aes(y = cumLateral.Flow..mm.  , color = Scenario),
    #                                            size = 0.5)
    #                     } else
    #                         if (input$Chan_variable ==  "Solub..React..P..kg.ha.") {
    #                             p7 <-
    #                                 p7 + geom_line(aes(y = cumSRP.kg.ha.  , color = Scenario), size = 0.5)
    #                         } else
    #                             if (input$Chan_variable ==  "Particulate.P..kg.ha.") {
    #                                 p7 <-
    #                                     p7 + geom_line(aes(y = cumParticulateP.kg.ha.  , color = Scenario),
    #                                                    size = 0.5)
    #                             } else
    #                                 if (input$Chan_variable ==  "Total.P..kg.ha.") {
    #                                     p7 <-
    #                                         p7 + geom_line(aes(y = cumTotalP.kg.ha.  , color = Scenario), size = 0.5)
    #                                 }
    #     
    #     p7 <- p7 +  theme_bw() +
    #         theme(
    #             axis.title = element_text(
    #                 size = 10,
    #                 color = "Black",
    #                 face = "bold"
    #             ),
    #             axis.text = element_text(
    #                 size = 10,
    #                 color = "BLACK",
    #                 face = "bold"
    #             ),
    #             legend.title = element_text(
    #                 size = 10,
    #                 color = "BLACK",
    #                 face = "bold"
    #             ),
    #             legend.text = element_text(size = 10, color = "BLACK"),
    #             legend.position = "none"
    #         ) +
    #         labs(
    #             x = "Percent of total channel length",
    #             y = paste("Percent of total ", input$Chan_variable, sep = " "),
    #             title = "",
    #             colour = "Scenario"
    #         )
    #     if (input$DefOrUserUpload_C == 'Default Data') {
    #         p7 <- p7 +
    #             scale_color_manual(
    #                 values = c(
    #                     "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2" = "#FF0000",
    #                     "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli" =
    #                         "#B22222",
    #                     "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
    #                     "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
    #                     "ModSevS.2020.ki5krcs.chn_12" =
    #                         "#DC143C",
    #                     "LowSevS.2020.ki5krcs.chn_12" =
    #                         "#FF6347",
    #                     "PrescFireS.2020.ki5krcs.chn_12" =
    #                         "#E9967A",
    #                     "Thinn85.2020.ki5krcs.chn_12" =
    #                         "#7CFC00",
    #                     "Thinn93.2020.kikrcs.chn_12" =
    #                         "#32CD32",
    #                     "Thinn96.2020.kikrcs.chn_12" =
    #                         "#00FF00",
    #                     "CurCond.2020.ki5krcs.chn_cs12" =
    #                         "#008000"
    #                 )
    #             )
    #     } else
    #         if (input$DefOrUserUpload_C == 'Upload data') {
    #             p7 <- p7 +
    #                 scale_color_brewer(palette = "virdis")
    #         }
    #     
    #     
    #     
    #     p7
    #     
    # })
    # 
    # output$Plot7_abs <- renderPlotly({
    #     req(input$Chan_variable)
    #     p7 <- chn_arr_by_var_CL_abs() %>% ggplot(aes(x = cumPercLen))
    #     if (input$Chan_variable ==  "Discharge..mm.") {
    #         p7 <-
    #             p7 + geom_line(aes(y = cumDischarge.mm  , color = Scenario), size = 0.5)
    #     } else
    #         if (input$Chan_variable ==  "Sediment.Yield..tonne.") {
    #             p7 <-
    #                 p7 + geom_line(aes(y = cumSediment.Yield..tonne.  , color = Scenario),
    #                                size = 0.5)
    #         } else
    #             if (input$Chan_variable ==  "Channel.Erosion..tonne.") {
    #                 p7 <-
    #                     p7 + geom_line(aes(y = cumChannel.Erosion..tonne.  , color = Scenario),
    #                                    size = 0.5)
    #             } else
    #                 if (input$Chan_variable ==  "Upland.Charge..mm.") {
    #                     p7 <-
    #                         p7 + geom_line(aes(y = cumUpland.Charge..mm.  , color = Scenario),
    #                                        size = 0.5)
    #                 } else
    #                     if (input$Chan_variable ==  "Lateral.Flow..mm.") {
    #                         p7 <-
    #                             p7 + geom_line(aes(y = cumLateral.Flow..mm.  , color = Scenario),
    #                                            size = 0.5)
    #                     } else
    #                         if (input$Chan_variable ==  "Solub..React..P..kg.ha.") {
    #                             p7 <-
    #                                 p7 + geom_line(aes(y = cumSRP.kg.ha.  , color = Scenario), size = 0.5)
    #                         } else
    #                             if (input$Chan_variable ==  "Particulate.P..kg.ha.") {
    #                                 p7 <-
    #                                     p7 + geom_line(aes(y = cumParticulateP.kg.ha.  , color = Scenario),
    #                                                    size = 0.5)
    #                             } else
    #                                 if (input$Chan_variable ==  "Total.P..kg.ha.") {
    #                                     p7 <-
    #                                         p7 + geom_line(aes(y = cumTotalP.kg.ha.  , color = Scenario), size = 0.5)
    #                                 }
    #     
    #     p7 <- p7 +  theme_bw() +
    #         theme(
    #             axis.title = element_text(
    #                 size = 10,
    #                 color = "Black",
    #                 face = "bold"
    #             ),
    #             axis.text = element_text(
    #                 size = 10,
    #                 color = "BLACK",
    #                 face = "bold"
    #             ),
    #             legend.title = element_text(
    #                 size = 10,
    #                 color = "BLACK",
    #                 face = "bold"
    #             ),
    #             legend.text = element_text(size = 10, color = "BLACK"),
    #             legend.position = "none"
    #         ) +
    #         labs(
    #             x = "Percent of total channel length",
    #             y = paste("Percent of total ", input$Chan_variable, sep = " "),
    #             title = "",
    #             colour = "Scenario"
    #         )
    #     if (input$DefOrUserUpload_C == 'Default Data') {
    #         p7 <- p7 +
    #             scale_color_manual(
    #                 values = c(
    #                     "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2" = "#FF0000",
    #                     "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli" =
    #                         "#B22222",
    #                     "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
    #                     "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
    #                     "ModSevS.2020.ki5krcs.chn_12" =
    #                         "#DC143C",
    #                     "LowSevS.2020.ki5krcs.chn_12" =
    #                         "#FF6347",
    #                     "PrescFireS.2020.ki5krcs.chn_12" =
    #                         "#E9967A",
    #                     "Thinn85.2020.ki5krcs.chn_12" =
    #                         "#7CFC00",
    #                     "Thinn93.2020.kikrcs.chn_12" =
    #                         "#32CD32",
    #                     "Thinn96.2020.kikrcs.chn_12" =
    #                         "#00FF00",
    #                     "CurCond.2020.ki5krcs.chn_cs12" =
    #                         "#008000"
    #                 )
    #             )
    #     } else
    #         if (input$DefOrUserUpload_C == 'Upload data') {
    #             p7 <- p7 +
    #                 scale_color_brewer(palette = "virdis")
    #         }
    #     
    #     
    #     
    #     p7
    #     
    # })
    
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
                
                a <-
                    ggplot(d.m, aes(Scenario, variable,  fill = value)) +
                    geom_tile(inherit.aes = TRUE)  +
                    scale_fill_distiller(palette =  "Spectral", direction = -1) +
                    theme(
                        axis.text.x = element_text(angle = 90, colour = "Black"),
                        axis.text.y = element_text(colour = "Black"),
                        axis.title = element_blank(),
                        legend.position='right')
                
                ggplotly(a) %>%
                    layout(title = list(text = paste0('<b>Relative impacts of management and disturbance on the watershed</b>',
                                                      '<br>',
                                                      '<sup>',
                                                      '<i>Plot displays the relative impact of the all disturbances on the delivered\nwater quantity/quality metric at the watershed outlet.</i>',
                                                      '<br><br>',
                                                      '</sup>')),
                           margin = list(l=10, r=20, b=5, t=150, pad=0))
                
            } else
                if (input$ScenVvar == "Bar Chart") {
                    d <-  Wshed_subset() %>% dplyr::select(Scenario, input$wshed_var)
                    
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
                                fill = reorder(Scenario,-share)
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
                            legend.title = element_blank()
                        ) + coord_flip() + 
                        scale_fill_brewer(palette = "RdYlGn") + 
                        scale_y_continuous(labels = function(x) paste0(x*1, "%"))+ 
                        theme(legend.position ="none") 
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
                    
                    a <-
                        ggplot(d.m, aes(Watershed, variable,  fill = value)) +
                        geom_tile(inherit.aes = TRUE)  +
                        scale_fill_distiller(palette =  "Spectral", direction = -1) +
                        theme(
                            axis.text.x = element_text(angle = 90, colour = "Black"),
                            axis.text.y = element_text(colour = "Black"),
                            axis.title = element_blank(),
                            legend.position='right'
                            
                        )
                    ggplotly(a) %>%
                        layout(title = list(text = paste0('<b>Relative impacts of the disturbance scenario across all watersheds </b>',
                                                          '<br>',
                                                          '<sup>',
                                                          '<i>Plot displays the relative impact of the disturbance on the delivered particular\nwater quantity/quality metric at each of the watersheds</i>',
                                                          '</sup>')),
                               margin = list(l=10, r=20, b=5, t=150, pad=0))
                    
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
                            ) +
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
                            ) + coord_flip() + labs(y = "Percent of total across all Watersheds") + scale_fill_brewer(palette = "RdYlGn") + theme(legend.position =
                                                                                                                                                      "none")
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
    
    #### from what I understand WGS84 latlon coord system needed to use with leaflet
    
    # Spatial_subset_shared <- reactive({
    #     req(Spatial_subset())
    #    df<- SharedData$new(Spatial_subset())
    #     
    # })
    
    output$Plot11 <- leaflet::renderLeaflet({
        req(Spatial_subset_base())
        req(input$S_scen_base)
        req(input$S_variable)
        tm1 <- tm_shape(Spatial_subset_base()) +
            # tm_borders(lwd = 0, alpha=0.0) +
            tmap::tm_polygons(
                input$S_variable,
                id = "watershed",
                palette = "YlGnBu",
                # style = "log10_pretty"
                style = "fixed",
                breaks = c(0, 1, 10, 100, 1000,
                           5000, 10000, 15000,20000,Inf),
                title = input$S_scen_base
            )+ tmap::tm_layout(main.title= input$S_scen_base,
                               scale = 0.1,
                               aes.color = "#0000000"
                               ) 
        
        
        tmap_leaflet(tm1, in.shiny = TRUE)
    })
    
    
    output$Plot12 <- leaflet::renderLeaflet({
        req(Spatial_subset_comp())
        req(input$S_scen_comp)
        req(input$S_variable)
        tm2 <- tm_shape(Spatial_subset_comp()) +
            tmap::tm_polygons(
                input$S_variable,
                id = "watershed",
                palette = "YlGnBu",
                legend.hist = TRUE,
                # style = "log10_pretty",
                style = "fixed",
                breaks = c(0, 1, 10, 100, 1000,
                           5000, 10000, 15000,20000,Inf),
                title = input$S_scen_comp
            ) + tmap::tm_layout(main.title = input$S_scen_comp,
                                scale = 0.1,
                                title.size = 10,
                                aes.color = "#0000000"
            ) 
        
        
        tmap_leaflet(tm2,in.shiny = TRUE)
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
            hill_subset() %>% dplyr::filter(Scenario %in% input$Hill_scen) %>%
                dplyr::arrange_at(.vars = input$Hill_variable, desc) %>%
                dplyr::mutate(cumPercArea = cumsum(Hillslope.Area..ha.) /
                                  sum(Hillslope.Area..ha.) * 100) %>%
                dplyr::filter(cumPercArea < input$thresh_H) %>%
                dplyr::select(LanduseDesc, Slope, Sediment.Yield..kg.ha.) %>%
                group_by(LanduseDesc) %>% dplyr::summarise_if(is.numeric, list(mean =
                                                                                   mean)) %>%
                dplyr::arrange(desc(Sediment.Yield..kg.ha._mean)) %>% dplyr::mutate_if(is.numeric, round, 2)
        } else
            if (input$summary_DT_by_var_H == "Soiltype") {
                hill_subset() %>% dplyr::filter(Scenario %in% input$Hill_scen) %>%
                    dplyr::arrange_at(.vars = input$Hill_variable, desc) %>%
                    dplyr::mutate(cumPercArea = cumsum(Hillslope.Area..ha.) /
                                      sum(Hillslope.Area..ha.) * 100) %>%
                    dplyr::filter(cumPercArea < input$thresh_H) %>%
                    dplyr::select(SoilDesc, Slope, Sediment.Yield..kg.ha.) %>%
                    group_by(SoilDesc) %>% dplyr::summarise_if(is.numeric, list(mean =
                                                                                    mean)) %>%
                    dplyr::arrange(desc(Sediment.Yield..kg.ha._mean)) %>% dplyr::mutate_if(is.numeric, round, 2)

            } else
                if (input$summary_DT_by_var_H == "Both") {
                    hill_subset() %>% dplyr::filter(Scenario %in% input$Hill_scen) %>%
                        dplyr::arrange_at(.vars = input$Hill_variable, desc) %>%
                        dplyr::mutate(cumPercArea = cumsum(Hillslope.Area..ha.) /
                                          sum(Hillslope.Area..ha.) * 100) %>%
                        dplyr::filter(cumPercArea < input$thresh_H) %>%
                        dplyr::select(SoilDesc,
                                      LanduseDesc,
                                      Slope,
                                      Sediment.Yield..kg.ha.) %>%
                        group_by(SoilDesc, LanduseDesc) %>% dplyr::summarise_if(is.numeric, list(mean =
                                                                                                     mean)) %>%
                        dplyr::arrange(desc(Sediment.Yield..kg.ha._mean)) %>% dplyr::mutate_if(is.numeric, round, 2)

                }
    })
    
    ### WEst Shore summary stats df
    
    # WS_sed_stats_df <- reactive({
    #
    #     if (input$AvgWestShoreNos_H == "Landuse") {
    #         hill_data() %>% dplyr::filter(Scenario %in% input$Hill_scen) %>%
    #             dplyr::select(LanduseDesc, Slope, Sediment.Yield..kg.ha.,
    #                           Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.) %>%
    #             group_by(LanduseDesc) %>% dplyr::summarise_if(is.numeric, list(mean=mean)) %>%
    #             dplyr::arrange(desc(Sediment.Yield..kg.ha._mean))%>% dplyr::mutate_if(is.numeric, round,2)
    #     }else
    #         if(input$AvgWestShoreNos_H == "Soiltype") {
    #             hill_data() %>% dplyr::filter(Scenario %in% input$Hill_scen) %>%
    #                 dplyr::select(SoilDesc, Slope, Sediment.Yield..kg.ha.,
    #                               Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.) %>%
    #                 group_by(SoilDesc) %>% dplyr::summarise_if(is.numeric, list(mean=mean)) %>%
    #                 dplyr::arrange(desc(Sediment.Yield..kg.ha._mean)) %>% dplyr::mutate_if(is.numeric, round,2)
    #
    #         }else
    #             if(input$AvgWestShoreNos_H == "Both") {
    #                 hill_data() %>% dplyr::filter(Scenario %in% input$Hill_scen) %>%
    #                     dplyr::select(SoilDesc, LanduseDesc, Slope, Sediment.Yield..kg.ha.,
    #                                   Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.) %>%
    #                     group_by(SoilDesc, LanduseDesc) %>% dplyr::summarise_if(is.numeric, list(mean=mean)) %>%
    #                     dplyr::arrange(desc(Sediment.Yield..kg.ha._mean)) %>% dplyr::mutate_if(is.numeric, round,2)
    #
    #             }
    # })
    

    
    # output$Sed_stats_by_category <- DT::renderDataTable(
    #     sed_stats_df(),
    #     options = list(
    #         dom = 'Bfrtip',
    #         deferRender = TRUE,
    #         scrollY = 400,
    #         compact = TRUE,
    #         scroller = TRUE,
    #         scrollX = TRUE,
    #         scrollY = FALSE,
    #         pageLength = 5,
    #         fixedHeader = TRUE,
    #         fillContainer = TRUE,
    #         class = "display",
    #         columnDefs = list(list(className = 'dt-left'))
    #     )
    # )
    
    
    output$Sed_stats_by_category <- DT::renderDataTable(
        sed_stats_df(),
        extensions = list("Buttons" = NULL),
        options = list(
            dom = 'Bfrtip',
            buttons = 
                list('copy', 'print', list(
                    extend = 'collection',
                    buttons = c('csv', 'excel', 'pdf'),
                    text = 'Download'
                )),
            # scroller = TRUE,
            scrollX = TRUE,
            scrollY = FALSE,
            pageLength = 5,
            fixedHeader = TRUE,
            fillContainer = F,
            class = "display",
            columnDefs = list(list(className = 'dt-left'))
        )
    )
    
    #### West shore summary stats table
    
    # output$WS_Sed_stats_by_category <- DT::renderDataTable(
    #     WS_sed_stats_df())
    #
    
    
    ## -----------------------------------------------------------------------------------------------------------##
    ## Summary table on spatial tab corressponding to the leaflet map
    ## -----------------------------------------------------------------------------------------------------------##
    

    spdftab <- reactive({
        req(Spatial_subset_comp())
        Spatial_subset_comp() %>% as.data.frame() %>% select(WeppID,TopazID, landuse, soil, 
                                                          slope, input$S_variable ) %>% 
            dplyr::filter(slope < input$thresh_slope_S)
    })
    
   

    output$spatial_table <- DT::renderDataTable(
        spdftab(),
        # caption = htmltools::tags$caption( style = 'caption-side: top; text-align: center; color:black; font-size:200% ;','Table1: Iris Dataset Table'),
        extensions = list("Buttons" = NULL),
        options = list(
            dom = 'Bfrtip',
            buttons =
                list('copy', 'print', list(
                    extend = 'collection',
                    buttons = c('csv', 'excel', 'pdf'),
                    text = 'Download'
                )),
            # scroller = TRUE,
            scrollX = TRUE,
            scrollY = FALSE,
            pageLength = 5,
            fixedHeader = TRUE,
            fillContainer = F,
            class = "display",
            columnDefs = list(list(className = 'dt-left'))

        )
    )
    
    
    observe_helpers()
    
}

# Run the application
shinyApp(ui = ui, server = server)