## --------------------------------------------------------------------------------------##
##
## Script name: app.R
##
## Purpose of the script: Script to ingest and visualize WEPPcloud simulations 
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

library(shiny)
library(tidyverse)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(plotly)
library(leaflet)
library(tmap)
library(ggthemes)
library(shinyBS)
library(shinyLP)
source("global.R")

## ----------------------------------Init Options---------------------------------------##
options(shiny.maxRequestSize = 100*1024^2)




## ----------------------------------define UI------------------------------------------##

ui <- navbarPage("viz-WEPPcloud",
                 position = "fixed-top",
                 fluid = TRUE, 
                 collapsible = TRUE,
                 id = 'tabs',
                 
                 
                 ## ----------------------------------Set Theme------------------------------------------##                 
                 ## set the theme
                 ###I like on of these themes: readable, flatly, journal,united, sandstone
                 theme = shinytheme(theme = "flatly"),
                 
                 ## ----------------------------------google-Analytics------------------------------------------##
                 
                 # tags$head(includeHTML(("google-analytics.html"))),
                 # tags$head(HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-170134899-1'></script>
                 #                    <script>
                 #                    window.dataLayer = window.dataLayer || [];
                 #                function gtag(){dataLayer.push(arguments);}
                 #                gtag('js', new Date());
                 #                
                 #                gtag('config', 'UA-170134899-1');
                 #                </script>"
                 # )),
                 
                 ## ----------------------------------Start defining Tabs------------------------------------------##
                 
                 
                 ## -----------------------------------------About Tab---------------------------------------------##                 
                 
                 tabPanel("Home", icon = icon("home"),

                          setBackgroundImage(src = "background1.png", shinydashboard = FALSE),
                          mainPanel(
                              fluidPage(
                                  HTML("<br style = “line-height:30;”><br>"),
                                  fluidRow(
                                      column(12, offset = 3, align  = "center",
                                             
                                             HTML('<div class="jumbotron" style="background-color:#F8F8FF;">
                                                      <h1>viz-WEPPcloud</h1>
                                                      <h4>Supporting targeted management using outputs of a process based model!</h4>
                                                      </div>')
                                             
                                             # border:2px solid black;

                                             ))),
                                 
                                  fluidPage(HTML("<br style = “line-height:20;”><br>"),
                                           fluidRow(
                                               column(12, offset =3, align  = "center",
                                               column(4,  align  = "center", 
                                                      thumbnail_label1(image = 'background.png', label = 'Watershed',
                                                                       content = "Compare impacts of management on water yield and water quality variables 
                                                                       in a particular watershed in one glance."),
                                                      actionBttn("Wbutton", "Navigate to Watershed",icon = icon("list-alt"))),
                                               column(4, align  = "center", 
                                                      thumbnail_label1(image = 'hillslope_img.png', label = 'Hillslope',
                                                                         content = 'Identify hillslopes that can be targeted for management to minimize impact on the 
                                                                       variable of interest in a particular watershed'),
                                                      actionBttn("Hbutton", "Navigate to Hillslope", icon = icon("line-chart"))),
                                               column(4, align  = "center", thumbnail_label1(image = 'spatial_imp.PNG', label = 'Spatial',
                                                                         content = 'Spatially visualize hillslopes that are important for
                                                                         management'),
                                                      actionBttn("Sbutton", "Navigate to Spatial-Viz",icon = icon("list-alt")))
                                               )
                                           )),
                                  
                                  
                                  HTML("<br style = “line-height:30;”><br>"),
                                  fluidPage(
                                  fluidRow(
                                      column(12, offset = 3, align  = "center",
                                             style = "height:120px;background-color:#F5F5F5;padding-left:20px;padding-top:20px;padding-bottom:20px;",
                                             
                                             tags$div(
                                                 tags$p("viz-WEPPcloud uses simulation outputs generated by WEPPCloud.",align = "center"),
                                                 tags$p(a(href = 'https://wepp1.nkn.uidaho.edu/weppcloud/', 'WEPPCloud', .noWS = "outside"),
                                                        ' is a cloud based simulation tool based on the process based Watershed Erosion Prediction Project model. It estimates
                                                          hillslope soil erosion, runoff, and sediment yields from anywhere in the continental U.S. Especially useful for
                                                          post-wildfire assessments, fuel treatment planning, and prescribed fire analysis.',
                                                        .noWS = c("after-begin", "before-end"), align = "center"),
                                                 
                                             )
                                             
                                      )),
                                  HTML("<br style = “line-height:30;”><br>")
                                  )
                              
                          )
                 ),
                 
                 ## -----------------------------------------Watershed Tab---------------------------------------------##                 
                 tabPanel("Watershed",
                          sidebarPanel(
                              
                              style = "position:fixed;width:inherit;", width = 3,
                              awesomeRadio(inputId = "DefOrUserUpload_W",
                                           label = "What data shall I use?",
                                           choices = c("Use default data (Lake Tahoe simulations)"="Default Data",
                                                       "Upload your own data"="Upload data"),
                                           selected = "Default Data",
                                           status= 'success'),
                              
                              
                              # uiOutput("Wshed_selectfile"),
                              
                              uiOutput("W_FileInput"),
                              
                              awesomeRadio(inputId = "AreaVsScen",label = "How do you want to compare?",
                                           choices = c("One Watershed, All Scenarios"="allscen",
                                                       "One Scenario, All Watersheds"="allwat"),
                                           selected = "allscen",
                                           status= 'success'),
                              
                              uiOutput("Wshed_wshed"),
                              
                              awesomeRadio(inputId = "ScenVvar",label = "Select visualization type",
                                           choices = c("Heatmap"="Heatmap","Bar Chart"="Bar Chart"),
                                           selected = "Heatmap",
                                           status= 'success')
                              
                              
                          ),
                          
                          # Main panel for displaying outputs ----
                          mainPanel(
                              width = 9,
                              style='padding:50px;',
                              uiOutput("Wshed_Exp"),
                              fluidPage(
                                  # plotlyOutput("Plot5" ,height = "800px", width ="1200px")
                                  # column(12, tableOutput("tab1"))
                                  fluidRow(
                                      column(12,
                                  plotlyOutput("Plot9",height = "700px", width ="800px")%>% withSpinner(color="#0dc5c1")
                                  )
                                  )
                              )
                          )
                 ),
                 
                 
                 ## -----------------------------------------Hillslope Tab---------------------------------------------## 
                 tabPanel("Hillslope",
                          tags$style(type="text/css", "body {padding-top: 70px;}"),
                          sidebarPanel(
                              style = "position:fixed;width:inherit;", width = 3,
                              
                              awesomeRadio(inputId = "DefOrUserUpload_H",label = "What data shall I use?",
                                           choices = c("Use default data (Lake Tahoe simulations)"="Default Data","Upload your own data"="Upload data"), selected = "Default Data"),
                              
                              uiOutput("H_FileInput"),
                              uiOutput("Hill_selectfile"),
                              uiOutput("Hill_wshed"),
                              uiOutput("Hill_var"),
                              
                              
                              knobInput("thresh_H", "Plot Threshold (%):",
                                          min = 0, max = 100,
                                          value = 100, fgColor = "#428BCA",
                                        inputColor = "#428BCA"),
                              
                              uiOutput("Hill_scen"),
                              
                              awesomeRadio(inputId = "summary_DT_by_var_H",label = "Summarize Sediment by:",
                                           choices = c("Land Use"="Landuse","Soil Type"="Soiltype", "Both" = "Both"), 
                                           selected = "Landuse",
                                           status= 'success'
                                           )
                              
                              # awesomeRadio(inputId = "AvgWestShoreNos_H",
                              # label = "Do you want average sediment summary for West shore?",
                              #              choices = c("Yes"="Yes","No"="No"), selected = "Yes"),
                              # 
                              # uiOutput("H_AvgWestShoreSummary")
                              
                              
                              
                          ),

                          mainPanel(
                              width = 9,
                              style='padding:50px;',
                              uiOutput("Exp1_Exp2"),
                              HTML("<br style = “line-height:5;”><br>"),
                              fluidRow(
                                  column(6, align = "center", plotlyOutput("Plot_vs_cumPercArea") %>% withSpinner(color="#0dc5c1")),
                                  column(6,align = "center", plotlyOutput("Plot_vs_cumPercArea_abs")%>% withSpinner(color="#0dc5c1"))
                              ),
                              HTML("<br style = “line-height:5;”><br>"),
                              uiOutput("Exp3_Exp4"),
                              HTML("<br style = “line-height:5;”><br>"),
                              fluidRow(
                                  
                                  column(6,align = "center", plotlyOutput("Plot_vs_cumPercLen")%>% withSpinner(color="#0dc5c1")),
                                  column(6,align = "center", plotlyOutput("Plot_vs_cumPercLen_abs")%>% withSpinner(color="#0dc5c1"))
                              ),
                              HTML("<br style = “line-height:5;”><br>"),
                              
                              fluidRow(
                                  
                                  column(12, align = "center", offset = 0, DT::dataTableOutput("Sed_stats_by_category") %>% withSpinner(color="#0dc5c1"))
                              )
                              # HTML("<br><br><br>"),
                              # fluidRow(
                              # column(6, DT::dataTableOutput("WS_Sed_stats_by_category") %>% withSpinner(color="#0dc5c1"))
                              # 
                              # )
                          )),
                 
                             
                 ## -----------------------------------------Spatial-Viz Tab---------------------------------------------## 
                 
                 tabPanel("Spatial-Viz",
                          sidebarPanel(
                              
                              awesomeRadio(inputId = "DefOrUserUpload_S",label = "What data shall I use?",
                                           choices = c("Use default data (Lake Tahoe simulations)"="Default Data","Upload your own data"="Upload data"), selected = "Default Data"),
                              
                              
                              uiOutput("S_FileInput"),
                              uiOutput("S_FileInput_Chan"),
                              uiOutput("Spatial_wshed"),
                              uiOutput("Spatial_scen"),
                              uiOutput("S_var")
                              
                              # awesomeRadio(inputId = "showchan_S",label = "Display Channels?",
                              #              choices = c("Yes"="Yes","No"="No"), selected = "No")
                              # 
                              
                          ),
                          
                          # Main panel for displaying outputs ----
                          mainPanel(
                              
                              fluidPage(
                                  
                                  leaflet::leafletOutput("Plot11",height = "800px", width ="800px" )%>% 
                                      withSpinner(color="#0dc5c1")
                                  
                              )
                          )
                 )
                 
                 ## --------------------------------------------------------------------------------------##                 
                 # tabPanel("Channel",
                 #          sidebarPanel(
                 #              
                 #              
                 #              awesomeRadio(inputId = "DefOrUserUpload_C",label = "What data shall I use?",
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
                 
                 
                 
)

## ----------------------------------define server logic------------------------------------------##


server <- function(input, output, session) {
    
    
    observeEvent(input$Wbutton, {
        updateTabsetPanel(session = session, inputId = "tabs", selected = "Watershed")
    })
    
    
    observeEvent(input$Hbutton, {
        updateTabsetPanel(session = session, inputId = "tabs", selected = "Hillslope")
    })
    
    observeEvent(input$Sbutton, {
        updateTabsetPanel(session = session, inputId = "tabs", selected = "Spatial-Viz")
    })
    
    ######## Server logic for UI generation for hillslope tab ##########
    
    # output$H_FileInput <- renderUI({
    #     if(input$DefOrUserUpload_H == 'Upload data'){
    #         # message = 'max. file size is 32MB'
    #         textInput("Hill_file",label ="Provide URL pointing to 'Hillslope' file (*_hill_*.csv) on WEPPcloud",
    #                   placeholder = "URL to hillslope file" )
    #         #actionButton("goButton", "Go!")
    #         }else
    #             if(input$DefOrUserUpload_H == 'Default Data'){}
    # })
    
    # output$H_AvgWestShoreSummary <- renderUI({
    #     if(input$AvgWestShoreNos_H == 'No'){
    #         }else
    #             if(input$AvgWestShoreNos_H == 'Yes'){
    #                 awesomeRadio(inputId = "WestShore_summary_by_var",label = "By which variable:",
    #                              choices = c("Land Use"="Landuse","Soil Type"="Soiltype", "Both" = "Both"), 
    #                              selected = "Landuse")
    #             }
    # })
    # 
    ## ----------------------------------Hillslope server logic------------------------------------------##    
    output$H_FileInput <- renderUI({
        if(input$DefOrUserUpload_H == 'Upload data'){
            message = 'max. file size is 32MB'
            fileInput("Hill_file",label ="Uplaod 'Hillslope' file (*_hill_*.csv)", 
                      multiple = F, placeholder = "No file selected", accept = ".csv" 
            )}else
                if(input$DefOrUserUpload_H == 'Default Data'){}
    })
    
    Hill_data <- reactive({
        req(input$DefOrUserUpload_H)
        if(input$DefOrUserUpload_H == 'Default Data'){
            file1 <- url("https://wepp1.nkn.uidaho.edu/weppcloud/static/mods/lt/results/lt2020_6_hill_summary.csv")
            # file1 <- "data/lt2020_6_hill_summary_with_all_scenarios_03_11_2020.csv"
            read.table(file=file1,header=TRUE,sep=",")
        }else
            if(input$DefOrUserUpload_H == 'Upload data'){
                file1 <- input$Hill_file
                if(is.null(file1)){return()}
                validate(
                    need(grepl("hill", input$Hill_file) == TRUE, "Wrong file provided. Hillslope filename should have '_hill_' in filename")
                )
                read.table(file=file1$datapath,header=TRUE,sep=",")
                
            }
        
    })
    
    
    output$Hill_var <- renderUI({
        if(input$DefOrUserUpload_H == 'Upload data'){
            req(Hill_data())
            selectInput("Hill_variable", "Select the variable of interest",  colnames(Hill_data()[10:29]),
                        selected = colnames(Hill_data()[10]) )
        }else
            if(input$DefOrUserUpload_H == 'Default Data'){
                selectInput(inputId="Hill_variable",label="Select the variable of interest",
                            choices =  as.character(unique(colnames(Hill_data())))[c(10:15, 20:29)],
                            selected = as.character(unique(colnames(Hill_data())))[10],multiple = F)
                
            }
        
    })
    
    
    output$Hill_wshed <- renderUI({
        if(input$DefOrUserUpload_H == 'Upload data'){
            req(Hill_data())
            selectInput("Hill_wshed", "Select the watershed of interest",  unique(Hill_data()$Watershed))
        }else
            if(input$DefOrUserUpload_H == 'Default Data'){
                
                selectInput(inputId="Hill_wshed",label="Select the watershed of interest",
                            choices =   unique(Hill_data()$Watershed),selected =   unique(Hill_data()$Watershed)[11])
                
            }
        
    })
    
    output$Hill_scen <- renderUI({
        if(input$DefOrUserUpload_H == 'Upload data'){
            req(Hill_data())
            selectInput("Hill_scen", "Select Scenario do display data summary",  unique(Hill_data()$Scenario),
                        unique(Hill_data()$Scenario)[1],
                        multiple = F)
        }else
            if(input$DefOrUserUpload_H == 'Default Data'){
                selectInput(inputId="Hill_scen",label="Select Scenario do display data summary",
                            choices =  unique(Hill_data()$Scenario),
                            unique(Hill_data()$Scenario)[1],
                            multiple = F)
                
            }
        
    })
   
    ## -----------------------------------------------------------------------------------------------------------## 
    ##  Generate plot descriptions ##
    ## -----------------------------------------------------------------------------------------------------------##    
    output$Exp1 <- renderText({
        paste("What percent of total hillslope area contributes a large fraction of total", " ", input$Hill_variable , " ", "?")
    })
    
    output$Exp2 <- renderText({
        paste("What percent of total hillslope area contributes a large fraction of cumulative", " ", input$Hill_variable , " ", "?")
    })
    
    output$Exp3 <- renderText({
        paste("What percent of total channel length contributes a large fraction of total", " ", input$Hill_variable , " ", "?")
    })
    
    output$Exp4 <- renderText({
        paste("What percent of total channel length contributes a large fraction of cumulative", " ", input$Hill_variable , " ", "?")
    })
    # 
    
    
    output$Exp1_Exp2 <- renderUI({
        req(Hill_data())
        fluidRow(
            column(6, style = "height:60px;background-color:#F5F5F5;padding-left:0px;", offset = 0,  textOutput("Exp1")),
            tags$head(tags$style("#Exp1{color: black;
                                  font-size: 16px;
                                  font-style: normal;
                                  font-family: Helvetica;
                                  text-align: center;
                                  }")
            ),
            column(5,style = "height:60px;background-color:#F5F5F5;padding-left:0px;", offset = 1, textOutput("Exp2")),
            tags$head(tags$style("#Exp2{color: black;
                                  font-size: 16px;
                                  font-style: normal;
                                  font-family: Helvetica;
                                  text-align: center;
                                  }")
            ))
    })
    
    
    output$Exp3_Exp4 <- renderUI({
        req(Hill_data())
        fluidRow(
            column(6, style = "height:60px;background-color:#F5F5F5;padding-left:10px;", offset = 0,  textOutput("Exp3")),
            tags$head(tags$style("#Exp1{color: black;
                                  font-size: 16px;
                                  font-style: normal;
                                  font-family: Helvetica;
                                  text-align: center;
                                  }")
            ),
            column(5,style = "height:60px;background-color:#F5F5F5;padding-left:10px;", offset = 1, textOutput("Exp4")),
            tags$head(tags$style("#Exp2{color: black;
                                  font-size: 16px;
                                  font-style: normal;
                                  font-family: Helvetica;
                                  text-align: center;
                                  }")
            ))
    })
    
    ## ----------------------------------Channel server logic------------------------------------------##    
    
    ######## Server logic for UI generation for Channel tab ##########
    
    output$C_FileInput <- renderUI({
        if(input$DefOrUserUpload_C == 'Upload data'){
            message = 'max. file size is 32MB'
            fileInput("Chan_file",label ="Uplaod 'Channel' file (*_chn_*.csv)", 
                      multiple = F, placeholder = "No file selected", accept = ".csv" 
            )}else
                if(input$DefOrUserUpload_C == 'Default Data'){}
    })
    
    Chan_data <- reactive({
        req(input$DefOrUserUpload_C)
        if(input$DefOrUserUpload_C == 'Default Data'){
            file2 <- url("https://wepp1.nkn.uidaho.edu/weppcloud/static/mods/lt/results/lt2020_6_chn_summary.csv")
            # file2 <- "data/lt2020_6_chn_summary_with_all_scenarios_03_11_2020.csv"
            read.table(file=file2,header=TRUE,sep=",")
        }else
            if(input$DefOrUserUpload_C == 'Upload data'){
                file2 <- input$Chan_file
                if(is.null(file2)){return()}
                validate(
                    need(grepl("chn", input$Chan_file) == TRUE, "Wrong file provided. Channel filename should have '_chn_' in filename")
                )
                read.table(file=file2$datapath,header=TRUE,sep=",")
                
            }
        
    })
    
    
    output$Chan_var <- renderUI({
        if(input$DefOrUserUpload_C == 'Upload data'){
            req(Chan_data())
            selectInput("Chan_variable", "Select the variable of interest",  colnames(Chan_data()[7:25]),
                        selected = colnames(Chan_data()[10]) )
        }else
            if(input$DefOrUserUpload_C == 'Default Data'){
                selectInput(inputId="Chan_variable",label="Select the variable of interest",
                            choices =  as.character(unique(colnames(Chan_data())))[c(7:14,23:25)],
                            selected = as.character(unique(colnames(Chan_data())))[10],multiple = F)
                
            }
        
    })
    
    
    output$Chan_wshed <- renderUI({
        if(input$DefOrUserUpload_C == 'Upload data'){
            req(Chan_data())
            selectInput("Chan_wshed", "Select the variable of interest",  unique(Chan_data()$Watershed))
        }else
            if(input$DefOrUserUpload_C == 'Default Data'){
                
                selectInput(inputId="Chan_wshed",label="Select the variable of interest",
                            choices =   unique(Chan_data()$Watershed), selected =   unique(Hill_data()$Watershed)[11])
                
            }
        
    })
    
    ## ----------------------------------Watershed Server logic------------------------------------------##  
    ######## Server logic for UI generation for  Watersheds tab ##########
    
    output$W_FileInput <- renderUI({
        if(input$DefOrUserUpload_W == 'Upload data'){
            message = 'max. file size is 32MB'
            fileInput("Wshed_file",label ="Uplaod 'Watershed' file (*_out_*.csv)", 
                      multiple = F, placeholder = "No file selected", accept = ".csv" 
            )}else
                if(input$DefOrUserUpload_W == 'Default Data'){}
    })
    
    
    Wshed_data <- reactive({
        req(input$DefOrUserUpload_W)
        if(input$DefOrUserUpload_W == 'Default Data'){
            file3 <- url("https://wepp1.nkn.uidaho.edu/weppcloud/static/mods/lt/results/lt2020_6_out_summary.csv")
            # file3 <- "data/lt2020_6_out_summary_with_all_scenarios_03_11_2020.csv"
            read.table(file=file3,header=TRUE,sep=",")
        }else
            if(input$DefOrUserUpload_W == 'Upload data'){
                file3 <- input$Wshed_file
                if(is.null(file3)){return()}
                validate(
                    need(grepl("out", input$Wshed_file) == TRUE, "Wrong file provided. Watershed filename should have '_out_' in filename")
                )
                read.table(file=file3$datapath,header = TRUE,sep=",")
                
            }
        
    })
    
    output$Wshed_wshed <- renderUI({
        if(input$DefOrUserUpload_W == 'Upload data'){
            req(Wshed_data())
            if(input$AreaVsScen == 'allscen'){
            selectInput("Wshed_wshed", "Select the watershed of interest",
                        unique(Wshed_data()$Watershed))
                }else
                    if(input$AreaVsScen == 'allwat'){
                        selectInput("Wshed_wshed", "Select the scenario of interest",
                                    unique(Wshed_data()$Scenario))
                    }
        }else
            if(input$DefOrUserUpload_W == 'Default Data'){
                if(input$AreaVsScen == 'allscen'){
                selectInput(inputId="Wshed_wshed",label="Select the watershed of interest",
                            choices =   unique(Wshed_data()$Watershed),
                            selected =   unique(Hill_data()$Watershed)[11])
                    }else
                        if(input$AreaVsScen == 'allwat'){
                            selectInput("Wshed_wshed", "Select the scenario of interest",
                                                                     unique(Wshed_data()$Scenario))
                        }
                
            }
        
    })
    
    ## ----------------------------------Spatial-Viz tab server logic------------------------------------------##    
    ######## Server logic for UI generation for spatial-Viz tab ##########
    
    output$S_FileInput <- renderUI({
        if(input$DefOrUserUpload_S == 'Upload data'){
            message = 'max. file size is 32MB'
            fileInput("Spatial_file",label ="Uplaod subcatchements JSON/geojson/RDS file", 
                      multiple = F, placeholder = "No file selected", accept = c(".JSON", ".geojson", ".RDS") 
            )}else
                if(input$DefOrUserUpload_S == 'Default Data'){}
    })
    
    output$S_FileInput_Chan <- renderUI({
        if(input$DefOrUserUpload_S == 'Upload data'){
            message = 'max. file size is 32MB'
            fileInput("Spatial_file_chan",label ="Uplaod Channels JSON/geojson/RDS file", 
                      multiple = F, placeholder = "No file selected", accept = c(".JSON", ".geojson", ".RDS") 
            )}else
                if(input$DefOrUserUpload_S == 'Default Data'){}
    })
    
    Spatial_data <- reactive({
        req(input$DefOrUserUpload_S)
        if(input$DefOrUserUpload_S == 'Default Data'){
            # sf::st_read("data/lt_allcond_subcatchments_wgs84_split_wshed_and_scen.geojson")
            readRDS("data/lt2020_6_subcatchments_wgs84_split_wshed_and_scen.RDS")
        }else
            if(input$DefOrUserUpload_S == 'Upload data'){
                file4 <- input$Spatial_file
                if(is.null(file4)){return()}
                sf::st_read(file4$datapath)}
        
    })
    
    ## spatial channel data
    Spatial_data_chan <- reactive({
        req(input$DefOrUserUpload_S)
        if(input$DefOrUserUpload_S == 'Default Data'){
            # sf::st_read("data/lt_allcond_subcatchments_wgs84_split_wshed_and_scen.geojson")
            readRDS("data/lt2020_6_channels_wgs84_split_wshed_and_scen.rds")
        }else
            if(input$DefOrUserUpload_S == 'Upload data'){
                file5 <- input$Spatial_file_chan
                if(is.null(file5)){return()}
                sf::st_read(file5$datapath)
            }
        
    })
    
    
    output$Spatial_wshed <- renderUI({
        if(input$DefOrUserUpload_S == 'Upload data'){
            req(Spatial_data())
            pickerInput("S_wshed", "Select the watershed of interest",  unique(Spatial_data()$Watershed),
                        options = list(`actions-box` = TRUE),
                        selected = unique(Spatial_data()$Watershed)[11], multiple = T)
        }else
            if(input$DefOrUserUpload_S == 'Default Data'){
                pickerInput(inputId="S_wshed",label="Select the watershed of interest",
                            choices =  unique(Spatial_data()$Watershed),
                            options = list(`actions-box` = TRUE),
                            selected = unique(Spatial_data()$Watershed)[19],
                            multiple = T)
                
            }
        
    })
    
    
    output$Spatial_scen <- renderUI({
        if(input$DefOrUserUpload_S == 'Upload data'){
            req(Spatial_data())
            selectInput("S_scen", "Select the scenario of interest",  unique(Spatial_data()$Scenario),
                        unique(Spatial_data()$Scenario)[1],
                        multiple = F)
        }else
            if(input$DefOrUserUpload_S == 'Default Data'){
                selectInput(inputId="S_scen",label="Select the scenario of interest",
                            choices =  unique(Spatial_data()$Scenario),
                            unique(Spatial_data()$Scenario)[1],
                            multiple = F)
                
            }
        
    })
    
    
    output$S_var <- renderUI({
        if(input$DefOrUserUpload_S == 'Upload data'){
            req(Spatial_data())
            selectInput("S_variable", "Select the variable of interest",  colnames(Spatial_data()),
                        selected = colnames(Spatial_data())[1],multiple = F)
        }else
            if(input$DefOrUserUpload_S == 'Default Data'){
                selectInput(inputId="S_variable",label="Select the variable of interest",
                            choices =  as.character(colnames(Spatial_data())),
                            selected = colnames(Spatial_data())[4], multiple = F)
                
            }
        
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
        if(input$AreaVsScen == 'allscen'){
            Wshed_data() %>% dplyr::filter(Watershed %in% input$Wshed_wshed)}else
                if(input$AreaVsScen == 'allwat'){
                    Wshed_data() %>% dplyr::filter(Scenario %in% input$Wshed_wshed)}
    })
    
    ################# Filtering logic for spatial DF #################
    
    Spatial_subset <- reactive({
        req(Spatial_data())
        req(input$S_wshed)
        Spatial_data() %>% 
            dplyr::filter(Watershed %in% input$S_wshed & Scenario %in% input$S_scen) 
    })
    
    ################# Filtering logic for spatial channel DF #################
    
    Spatial_subset_chan <- reactive({
        req(Spatial_data_chan())
        req(input$S_wshed)
        Spatial_data_chan() %>% 
            dplyr::filter(Watershed %in% input$S_wshed & Scenario %in% input$S_scen) 
    })
    
    ## -----------------------------------------------------------------------------------------------------------##    
    ## ---------------------------------Dataframe Calculations for hillslopes-------------------------------------------------------##    
    ## -----------------------------------------------------------------------------------------------------------##        
    
    
    ############## Dataframe calculating cumulative percent of total variable: Hillslope   ############## 
    ### this is the DF for plot 1 on hillslopes tab
    hill_arr_by_var_HA <- reactive({
        hill_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$Hill_variable, desc)%>%
            mutate(cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
                   cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                   cumRunoff.mm = cumsum(Runoff..mm.)/sum(Runoff..mm.)*100,
                   cumLateralflow.mm = cumsum(Lateral.Flow..mm.)/sum(Lateral.Flow..mm.)*100,
                   cumBaseflow.mm = cumsum(Baseflow..mm.)/sum(Baseflow..mm.)*100,
                   cumSoilLoss.kg.ha = cumsum(Soil.Loss..kg.ha.)/sum(Soil.Loss..kg.ha.)*100,
                   cumSedDep.kg.ha = cumsum(Sediment.Deposition..kg.ha.)/sum(Sediment.Deposition..kg.ha.)*100,
                   cumSedYield.kg.ha = cumsum(Sediment.Yield..kg.ha.)/sum(Sediment.Yield..kg.ha.)*100,
                   cumSRP.kg.ha.3 = cumsum(Solub..React..P..kg.ha.3.)/sum(Solub..React..P..kg.ha.3.)*100,
                   cumParticulateP.kg.ha.3 = cumsum(Particulate.P..kg.ha.3.)/sum(Particulate.P..kg.ha.3.)*100,
                   cumTotalP.kg.ha.3 = cumsum(Total.P..kg.ha.3.)/sum(Total.P..kg.ha.3.)*100,
                   cumParticle.Class.1.Fraction = cumsum(Particle.Class.1.Fraction)/sum(Particle.Class.1.Fraction)*100,
                   cumParticle.Class.2.Fraction = cumsum(Particle.Class.2.Fraction)/sum(Particle.Class.2.Fraction)*100,
                   cumParticle.Class.3.Fraction = cumsum(Particle.Class.3.Fraction)/sum(Particle.Class.3.Fraction)*100,
                   cumParticle.Class.4.Fraction = cumsum(Particle.Class.4.Fraction)/sum(Particle.Class.4.Fraction)*100,
                   cumParticle.Class.5.Fraction = cumsum(Particle.Class.5.Fraction)/sum(Particle.Class.5.Fraction)*100,
                   cumParticle.Fraction.Under.0.016.mm = cumsum(Particle.Fraction.Under.0.016.mm)/sum(Particle.Fraction.Under.0.016.mm)*100,
                   cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. = cumsum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.)/sum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.)*100
            ) %>% dplyr::filter(cumPercArea<input$thresh_H) %>%
            ungroup()
    })
    
    ## this is the dataframe for plot 3 on the hillslopes tab (the channel length plot)
    hill_arr_by_var_CL <- reactive({
        hill_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$Hill_variable, desc)%>%
            mutate(cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
                   cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                   cumRunoff.mm = cumsum(Runoff..mm.)/sum(Runoff..mm.)*100,
                   cumLateralflow.mm = cumsum(Lateral.Flow..mm.)/sum(Lateral.Flow..mm.)*100,
                   cumBaseflow.mm = cumsum(Baseflow..mm.)/sum(Baseflow..mm.)*100,
                   cumSoilLoss.kg.ha = cumsum(Soil.Loss..kg.ha.)/sum(Soil.Loss..kg.ha.)*100,
                   cumSedDep.kg.ha = cumsum(Sediment.Deposition..kg.ha.)/sum(Sediment.Deposition..kg.ha.)*100,
                   cumSedYield.kg.ha = cumsum(Sediment.Yield..kg.ha.)/sum(Sediment.Yield..kg.ha.)*100,
                   cumSRP.kg.ha.3 = cumsum(Solub..React..P..kg.ha.3.)/sum(Solub..React..P..kg.ha.3.)*100,
                   cumParticulateP.kg.ha.3 = cumsum(Particulate.P..kg.ha.3.)/sum(Particulate.P..kg.ha.3.)*100,
                   cumTotalP.kg.ha.3 = cumsum(Total.P..kg.ha.3.)/sum(Total.P..kg.ha.3.)*100,
                   cumParticle.Class.1.Fraction = cumsum(Particle.Class.1.Fraction)/sum(Particle.Class.1.Fraction)*100,
                   cumParticle.Class.2.Fraction = cumsum(Particle.Class.2.Fraction)/sum(Particle.Class.2.Fraction)*100,
                   cumParticle.Class.3.Fraction = cumsum(Particle.Class.3.Fraction)/sum(Particle.Class.3.Fraction)*100,
                   cumParticle.Class.4.Fraction = cumsum(Particle.Class.4.Fraction)/sum(Particle.Class.4.Fraction)*100,
                   cumParticle.Class.5.Fraction = cumsum(Particle.Class.5.Fraction)/sum(Particle.Class.5.Fraction)*100,
                   cumParticle.Fraction.Under.0.016.mm = cumsum(Particle.Fraction.Under.0.016.mm)/sum(Particle.Fraction.Under.0.016.mm)*100,
                   cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. = cumsum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.)/sum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.)*100
            ) %>% dplyr::filter(cumPercLen<input$thresh_H) %>%
            ungroup()
    })
    
    
    
    # ############## Dataframe calculating cumulative absolute value of variable: Hillslope   ############## 
    # ### this is the DF for plot 2 on hillslopes tab
    hill_arr_by_var_HA_abs <- reactive({
        hill_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$Hill_variable, desc)%>%
            mutate(cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
                   cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
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
            ) %>% dplyr::filter(cumPercArea<input$thresh_H) %>%
            ungroup()
    })
    
    
    # ### this is the DF for plot 4 on hillslopes tab
    hill_arr_by_var_CL_abs <- reactive({
        hill_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$Hill_variable, desc)%>%
            mutate(cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
                   cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
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
            ) %>% dplyr::filter(cumPercLen<input$thresh_H) %>% 
            ungroup()
    })
    
    ## -----------------------------------------------------------------------------------------------------------##    
    ## ---------------------------------Dataframe Calculations for Channels-------------------------------------------------------##    
    ## -----------------------------------------------------------------------------------------------------------##        
    
    
    ############## Dataframe calculating cumulative percent of total variable: Channel   ############## 
    #### df for plot thresholded by percent of channel area
    chn_arr_by_var_CA <- reactive({
        Chan_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$Chan_variable, desc)%>%
            mutate(cumPercChanArea = cumsum(Channel.Area..ha.)/sum(Channel.Area..ha.)*100,
                   cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
                   cumPercContriChanArea = cumsum(Contributing.Channel.Area..ha.)/sum(Contributing.Channel.Area..ha.)*100,
                   cumDischarge.mm = cumsum(Discharge..mm.)/sum(Discharge..mm.)*100,
                   cumSediment.Yield..tonne. = cumsum(Sediment.Yield..tonne.)/sum(Sediment.Yield..tonne.)*100,
                   cumChannel.Erosion..tonne. = cumsum(Channel.Erosion..tonne.)/sum(Channel.Erosion..tonne.)*100,
                   cumUpland.Charge..mm. = cumsum(Upland.Charge..mm.)/sum(Upland.Charge..mm.)*100,
                   cumLateral.Flow..mm. = cumsum(Lateral.Flow..mm.)/sum(Lateral.Flow..mm.)*100,
                   cumSRP.kg.ha. = cumsum(Solub..React..P..kg.ha.)/sum(Solub..React..P..kg.ha.)*100,
                   cumParticulateP.kg.ha. = cumsum(Particulate.P..kg.ha.)/sum(Particulate.P..kg.ha.)*100,
                   cumTotalP.kg.ha. = cumsum(Total.P..kg.ha.)/sum(Total.P..kg.ha.)*100) %>%
            dplyr::filter(cumPercChanArea<input$thresh_C) %>%
            ungroup()})
    
    
    #### df for plot thresholded by percent of channel Length
    chn_arr_by_var_CL <- reactive({
        Chan_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$Chan_variable, desc)%>%
            mutate(cumPercChanArea = cumsum(Channel.Area..ha.)/sum(Channel.Area..ha.)*100,
                   cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
                   cumPercContriChanArea = cumsum(Contributing.Channel.Area..ha.)/sum(Contributing.Channel.Area..ha.)*100,
                   cumDischarge.mm = cumsum(Discharge..mm.)/sum(Discharge..mm.)*100,
                   cumSediment.Yield..tonne. = cumsum(Sediment.Yield..tonne.)/sum(Sediment.Yield..tonne.)*100,
                   cumChannel.Erosion..tonne. = cumsum(Channel.Erosion..tonne.)/sum(Channel.Erosion..tonne.)*100,
                   cumUpland.Charge..mm. = cumsum(Upland.Charge..mm.)/sum(Upland.Charge..mm.)*100,
                   cumLateral.Flow..mm. = cumsum(Lateral.Flow..mm.)/sum(Lateral.Flow..mm.)*100,
                   cumSRP.kg.ha. = cumsum(Solub..React..P..kg.ha.)/sum(Solub..React..P..kg.ha.)*100,
                   cumParticulateP.kg.ha. = cumsum(Particulate.P..kg.ha.)/sum(Particulate.P..kg.ha.)*100,
                   cumTotalP.kg.ha. = cumsum(Total.P..kg.ha.)/sum(Total.P..kg.ha.)*100) %>% 
            dplyr::filter(cumPercLen<input$thresh_C) %>%
            ungroup()})
    
    
    ############## Dataframe calculating cumulative absolute value of variable: Channel   ############## 
    #### df for plot thresholded by percent of channel area with abs numbers
    chn_arr_by_var_CA_abs <- reactive({
        Chan_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$Chan_variable, desc)%>%
            mutate(cumPercChanArea = cumsum(Channel.Area..ha.)/sum(Channel.Area..ha.)*100,
                   cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
                   cumPercContriChanArea = cumsum(Contributing.Channel.Area..ha.)/sum(Contributing.Channel.Area..ha.)*100, ### fix total contributing area/ already a cumulative avalue in the channel file
                   cumDischarge.mm = cumsum(Discharge..mm.),
                   cumSediment.Yield..tonne. = cumsum(Sediment.Yield..tonne.),
                   cumChannel.Erosion..tonne. = cumsum(Channel.Erosion..tonne.),
                   cumUpland.Charge..mm. = cumsum(Upland.Charge..mm.),
                   cumLateral.Flow..mm. = cumsum(Lateral.Flow..mm.),
                   cumSRP.kg.ha. = cumsum(Solub..React..P..kg.ha.),
                   cumParticulateP.kg.ha. = cumsum(Particulate.P..kg.ha.),
                   cumTotalP.kg.ha. = cumsum(Total.P..kg.ha.)) %>%
            dplyr::filter(cumPercChanArea<input$thresh_C) %>%
            ungroup()})
    
    #### df for plot thresholded by percent of channel area with abs numbers
    chn_arr_by_var_CL_abs <- reactive({
        Chan_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$Chan_variable, desc)%>%
            mutate(cumPercChanArea = cumsum(Channel.Area..ha.)/sum(Channel.Area..ha.)*100,
                   cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
                   cumPercContriChanArea = cumsum(Contributing.Channel.Area..ha.)/sum(Contributing.Channel.Area..ha.)*100, ### fix total contributing area/ already a cumulative avalue in the channel file
                   cumDischarge.mm = cumsum(Discharge..mm.),
                   cumSediment.Yield..tonne. = cumsum(Sediment.Yield..tonne.),
                   cumChannel.Erosion..tonne. = cumsum(Channel.Erosion..tonne.),
                   cumUpland.Charge..mm. = cumsum(Upland.Charge..mm.),
                   cumLateral.Flow..mm. = cumsum(Lateral.Flow..mm.),
                   cumSRP.kg.ha. = cumsum(Solub..React..P..kg.ha.),
                   cumParticulateP.kg.ha. = cumsum(Particulate.P..kg.ha.),
                   cumTotalP.kg.ha. = cumsum(Total.P..kg.ha.)) %>%
            dplyr::filter(cumPercLen<input$thresh_C) %>%
            ungroup()})
    
    ## -----------------------------------------------------------------------------------------------------------##    
    ## ---------------------------------Plots:Hillslopes-------------------------------------------------------##    
    ## -----------------------------------------------------------------------------------------------------------##        
    
    # ############## plots of cumulative percent of total variable   ############## 
    # ############## vs cumulative percent of total hillslope area/ channel length   ############## 
    # 
    
    
    output$Plot_vs_cumPercArea <- renderPlotly({
        
        req(input$Hill_variable)
        
        p1 <- hill_arr_by_var_HA()  %>% ggplot(aes(x=cumPercArea))
        if(input$Hill_variable == "Runoff..mm."){
            p1 <- p1 + geom_line(aes(y=cumRunoff.mm  , color= Scenario),size=0.5)
        }else
            if(input$Hill_variable == "Lateral.Flow..mm."){
                p1 <- p1 + geom_line(aes(y=cumLateralflow.mm, color= Scenario),size=0.5)
            }else
                if(input$Hill_variable == "Baseflow..mm."){
                    p1 <- p1 + geom_line(aes(y=cumBaseflow.mm, color= Scenario),size=0.5)
                }else
                    if(input$Hill_variable == "Soil.Loss..kg.ha."){
                        p1 <- p1 + geom_line(aes(y=cumSoilLoss.kg.ha, color= Scenario),size=0.5)
                    }else
                        if(input$Hill_variable == "Sediment.Deposition..kg.ha."){
                            p1 <- p1 + geom_line(aes(y=cumSedDep.kg.ha, color= Scenario),size=0.5)
                        }else
                            if(input$Hill_variable == "Sediment.Yield..kg.ha."){
                                p1 <- p1 + geom_line(aes(y=cumSedYield.kg.ha, color= Scenario),size=0.5)
                            }else
                                if(input$Hill_variable == "Solub..React..P..kg.ha.3."){
                                    p1 <- p1 + geom_line(aes(y=cumSRP.kg.ha.3, color= Scenario),size=0.5)
                                }else
                                    if(input$Hill_variable == "Particulate.P..kg.ha.3."){
                                        p1 <- p1 + geom_line(aes(y=cumParticulateP.kg.ha.3, color= Scenario),size=0.5)
                                    }else
                                        if(input$Hill_variable == "Total.P..kg.ha.3."){
                                            p1 <- p1 + geom_line(aes(y=cumTotalP.kg.ha.3, color= Scenario),size=0.5)
                                        }else
                                            if(input$Hill_variable == "Particle.Class.1.Fraction"){
                                                p1 <- p1 + geom_line(aes(y=cumParticle.Class.1.Fraction, color= Scenario),size=0.5)
                                            }else
                                                if(input$Hill_variable == "Particle.Class.2.Fraction"){
                                                    p1 <- p1 + geom_line(aes(y=cumParticle.Class.2.Fraction, color= Scenario),size=0.5)
                                                }else
                                                    if(input$Hill_variable == "Particle.Class.3.Fraction"){
                                                        p1 <- p1 + geom_line(aes(y=cumParticle.Class.3.Fraction, color= Scenario),size=0.5)
                                                    }else
                                                        if(input$Hill_variable == "Particle.Class.4.Fraction"){
                                                            p1 <- p1 + geom_line(aes(y=cumParticle.Class.4.Fraction, color= Scenario),size=0.5)
                                                        }else
                                                            if(input$Hill_variable == "Particle.Class.5.Fraction"){
                                                                p1 <- p1 + geom_line(aes(y=cumParticle.Class.5.Fraction, color= Scenario),size=0.5)
                                                            }else
                                                                if(input$Hill_variable == "Particle.Fraction.Under.0.016.mm"){
                                                                    p1 <- p1 + geom_line(aes(y=cumParticle.Fraction.Under.0.016.mm, color= Scenario),size=0.5)
                                                                }else
                                                                    if(input$Hill_variable == "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha."){
                                                                        p1 <- p1 + geom_line(aes(y=cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. , color= Scenario),size=0.5)
                                                                    }
        
        
        p1 <- p1 +  theme_bw()+
            theme(axis.title = element_text(size=10,color="Black",face="bold"),
                  axis.text = element_text(size=10,color="BLACK",face="bold"),
                  legend.title = element_text(size=10,color="BLACK",face="bold"),
                  legend.text = element_text(size=10,color="BLACK"),
                  legend.position = "none",
                  title = element_text(size=10,color="Black",face="bold") )+
            labs(x="Percent of total hillslope area",y=paste("Percent of total", input$Hill_variable, sep = " ") 
                 #title= paste("% hillslope area contributing", input$Hill_variable, sep = " " )
                 ,colour="Scenario")
        if(input$DefOrUserUpload_H == 'Default Data'){
            p1 <- p1 +
                scale_color_manual(values = c( "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"="#FF0000",
                                               "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli"="#B22222",
                                               "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
                                               "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
                                               "ModSevS.2020.ki5krcs.chn_12"="#DC143C",
                                               "LowSevS.2020.ki5krcs.chn_12"="#FF6347",
                                               "PrescFireS.2020.ki5krcs.chn_12"="#E9967A",
                                               "Thinn85.2020.ki5krcs.chn_12"="#7CFC00",
                                               "Thinn93.2020.kikrcs.chn"="#32CD32",
                                               "Thinn96.2020.kikrcs.chn"="#00FF00",
                                               "CurCond.2020.ki5krcs.chn_cs12"="#008000"))}else
                                                   if(input$DefOrUserUpload_H == 'Upload Data'){
                                                       p1 <- p1 +
                                                           scale_color_brewer(palette = "virdis")}
        
        
        
        
        p1
        
    })
    # 
    # 
    output$Plot_vs_cumPercLen <- renderPlotly({
        
        req(input$Hill_variable)
        
        p3 <- hill_arr_by_var_CL()  %>% ggplot(aes(x=cumPercLen))
        if(input$Hill_variable == "Runoff..mm."){
            p3 <- p3 + geom_line(aes(y=cumRunoff.mm  , color= Scenario),size=0.5)
        }else
            if(input$Hill_variable == "Lateral.Flow..mm."){
                p3 <- p3 + geom_line(aes(y=cumLateralflow.mm, color= Scenario),size=0.5)
            }else
                if(input$Hill_variable == "Baseflow..mm."){
                    p3 <- p3 + geom_line(aes(y=cumBaseflow.mm, color= Scenario),size=0.5)
                }else
                    if(input$Hill_variable == "Soil.Loss..kg.ha."){
                        p3 <- p3 + geom_line(aes(y=cumSoilLoss.kg.ha, color= Scenario),size=0.5)
                    }else
                        if(input$Hill_variable == "Sediment.Deposition..kg.ha."){
                            p3 <- p3 + geom_line(aes(y=cumSedDep.kg.ha, color= Scenario),size=0.5)
                        }else
                            if(input$Hill_variable == "Sediment.Yield..kg.ha."){
                                p3 <- p3 + geom_line(aes(y=cumSedYield.kg.ha, color= Scenario),size=0.5)
                            }else
                                if(input$Hill_variable == "Solub..React..P..kg.ha.3."){
                                    p3 <- p3 + geom_line(aes(y=cumSRP.kg.ha.3, color= Scenario),size=0.5)
                                }else
                                    if(input$Hill_variable == "Particulate.P..kg.ha.3."){
                                        p3 <- p3 + geom_line(aes(y=cumParticulateP.kg.ha.3, color= Scenario),size=0.5)
                                    }else
                                        if(input$Hill_variable == "Total.P..kg.ha.3."){
                                            p3 <- p3 + geom_line(aes(y=cumTotalP.kg.ha.3, color= Scenario),size=0.5)
                                        }else
                                            if(input$Hill_variable == "Particle.Class.1.Fraction"){
                                                p3 <- p3 + geom_line(aes(y=cumParticle.Class.1.Fraction, color= Scenario),size=0.5)
                                            }else
                                                if(input$Hill_variable == "Particle.Class.2.Fraction"){
                                                    p3 <- p3 + geom_line(aes(y=cumParticle.Class.2.Fraction, color= Scenario),size=0.5)
                                                }else
                                                    if(input$Hill_variable == "Particle.Class.3.Fraction"){
                                                        p3 <- p3 + geom_line(aes(y=cumParticle.Class.3.Fraction, color= Scenario),size=0.5)
                                                    }else
                                                        if(input$Hill_variable == "Particle.Class.4.Fraction"){
                                                            p3 <- p3 + geom_line(aes(y=cumParticle.Class.4.Fraction, color= Scenario),size=0.5)
                                                        }else
                                                            if(input$Hill_variable == "Particle.Class.5.Fraction"){
                                                                p3 <- p3 + geom_line(aes(y=cumParticle.Class.5.Fraction, color= Scenario),size=0.5)
                                                            }else
                                                                if(input$Hill_variable == "Particle.Fraction.Under.0.016.mm"){
                                                                    p3 <- p3 + geom_line(aes(y=cumParticle.Fraction.Under.0.016.mm, color= Scenario),size=0.5)
                                                                }else
                                                                    if(input$Hill_variable == "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha."){
                                                                        p3 <- p3 + geom_line(aes(y=cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. , color= Scenario),size=0.5)
                                                                    }
        
        
        p3 <- p3 +  theme_bw()+
            theme(axis.title = element_text(size=10,color="Black",face="bold"),
                  axis.text = element_text(size=10,color="BLACK",face="bold"),
                  legend.title = element_text(size=10,color="BLACK",face="bold"),
                  legend.text = element_text(size=10,color="BLACK"),
                  legend.position = "none")+
            labs(x="Percent of total channel length",y=paste("Percent of total", input$Hill_variable, sep = " "), title="",colour="Scenario")
        
        if(input$DefOrUserUpload_H == 'Default Data'){
            p3 <- p3 +
                scale_color_manual(values = c( "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"="#FF0000",
                                               "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli"="#B22222",
                                               "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
                                               "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
                                               "ModSevS.2020.ki5krcs.chn_12"="#DC143C",
                                               "LowSevS.2020.ki5krcs.chn_12"="#FF6347",
                                               "PrescFireS.2020.ki5krcs.chn_12"="#E9967A",
                                               "Thinn85.2020.ki5krcs.chn_12"="#7CFC00",
                                               "Thinn93.2020.kikrcs.chn_12"="#32CD32",
                                               "Thinn96.2020.kikrcs.chn_12"="#00FF00",
                                               "CurCond.2020.ki5krcs.chn_cs12"="#008000"))}else
                                                   if(input$DefOrUserUpload_H == 'Upload Data'){
                                                       p3 <- p3 +
                                                           scale_color_brewer(palette = "virdis")}
        
        
        
        
        p3
        
    })
    # 
    # 
    # ############## plots of cumulative absolute values of variable   ############## 
    # ############## vs cumulative percent of total hillslope area/ channel length   ############## 
    # 
    output$Plot_vs_cumPercArea_abs <- renderPlotly({
        
        req(input$Hill_variable)
        
        p2 <- hill_arr_by_var_HA_abs()  %>% ggplot(aes(x=cumPercArea))
        if(input$Hill_variable == "Runoff..mm."){
            p2 <- p2 + geom_line(aes(y=cumRunoff.mm  , color= Scenario),size=0.5)
        }else
            if(input$Hill_variable == "Lateral.Flow..mm."){
                p2 <- p2 + geom_line(aes(y=cumLateralflow.mm, color= Scenario),size=0.5)
            }else
                if(input$Hill_variable == "Baseflow..mm."){
                    p2 <- p2 + geom_line(aes(y=cumBaseflow.mm, color= Scenario),size=0.5)
                }else
                    if(input$Hill_variable == "Soil.Loss..kg.ha."){
                        p2 <- p2 + geom_line(aes(y=cumSoilLoss.kg.ha, color= Scenario),size=0.5)
                    }else
                        if(input$Hill_variable == "Sediment.Deposition..kg.ha."){
                            p2 <- p2 + geom_line(aes(y=cumSedDep.kg.ha, color= Scenario),size=0.5)
                        }else
                            if(input$Hill_variable == "Sediment.Yield..kg.ha."){
                                p2 <- p2 + geom_line(aes(y=cumSedYield.kg.ha, color= Scenario),size=0.5)
                            }else
                                if(input$Hill_variable == "Solub..React..P..kg.ha.3."){
                                    p2 <- p2 + geom_line(aes(y=cumSRP.kg.ha.3, color= Scenario),size=0.5)
                                }else
                                    if(input$Hill_variable == "Particulate.P..kg.ha.3."){
                                        p2 <- p2 + geom_line(aes(y=cumParticulateP.kg.ha.3, color= Scenario),size=0.5)
                                    }else
                                        if(input$Hill_variable == "Total.P..kg.ha.3."){
                                            p2 <- p2 + geom_line(aes(y=cumTotalP.kg.ha.3, color= Scenario),size=0.5)
                                        }else
                                            if(input$Hill_variable == "Particle.Class.1.Fraction"){
                                                p2 <- p2 + geom_line(aes(y=cumParticle.Class.1.Fraction, color= Scenario),size=0.5)
                                            }else
                                                if(input$Hill_variable == "Particle.Class.2.Fraction"){
                                                    p2 <- p2 + geom_line(aes(y=cumParticle.Class.2.Fraction, color= Scenario),size=0.5)
                                                }else
                                                    if(input$Hill_variable == "Particle.Class.3.Fraction"){
                                                        p2 <- p2 + geom_line(aes(y=cumParticle.Class.3.Fraction, color= Scenario),size=0.5)
                                                    }else
                                                        if(input$Hill_variable == "Particle.Class.4.Fraction"){
                                                            p2 <- p2 + geom_line(aes(y=cumParticle.Class.4.Fraction, color= Scenario),size=0.5)
                                                        }else
                                                            if(input$Hill_variable == "Particle.Class.5.Fraction"){
                                                                p2 <- p2 + geom_line(aes(y=cumParticle.Class.5.Fraction, color= Scenario),size=0.5)
                                                            }else
                                                                if(input$Hill_variable == "Particle.Fraction.Under.0.016.mm"){
                                                                    p2 <- p2 + geom_line(aes(y=cumParticle.Fraction.Under.0.016.mm, color= Scenario),size=0.5)
                                                                }else
                                                                    if(input$Hill_variable == "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha."){
                                                                        p2 <- p2 + geom_line(aes(y=cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. , color= Scenario),size=0.5)
                                                                    }
        
        
        p2 <- p2 +  theme_bw()+
            theme(axis.title = element_text(size=10,color="Black",face="bold"),
                  axis.text = element_text(size=10,color="BLACK",face="bold"),
                  legend.title = element_text(size=10,color="BLACK",face="bold"),
                  legend.text = element_text(size=10,color="BLACK"),
                  legend.position = "none")+
            # scale_color_brewer(palette="RdYlGn") +
            labs(x="Percent of total hillslope area",y=paste("Cumulative", input$Hill_variable, sep = " "), title="",colour="Scenario") 
        if(input$DefOrUserUpload_H == 'Default Data'){
            p2 <- p2 +
                scale_color_manual(values = c( "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"="#FF0000",
                                               "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli"="#B22222",
                                               "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
                                               "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
                                               "ModSevS.2020.ki5krcs.chn_12"="#DC143C",
                                               "LowSevS.2020.ki5krcs.chn_12"="#FF6347",
                                               "PrescFireS.2020.ki5krcs.chn_12"="#E9967A",
                                               "Thinn85.2020.ki5krcs.chn_12"="#7CFC00",
                                               "Thinn93.2020.kikrcs.chn_12"="#32CD32",
                                               "Thinn96.2020.kikrcs.chn_12"="#00FF00",
                                               "CurCond.2020.ki5krcs.chn_cs12"="#008000"))}else
                                                   if(input$DefOrUserUpload_H == 'Upload Data'){
                                                       p2 <- p2 +
                                                           scale_color_brewer(palette = "virdis")}
        
        
        
        
        p2
        
    })
    # 
    # 
    # 
    output$Plot_vs_cumPercLen_abs <- renderPlotly({
        
        req(input$Hill_variable)
        
        p4 <- hill_arr_by_var_CL_abs()  %>% ggplot(aes(x=cumPercLen))
        if(input$Hill_variable == "Runoff..mm."){
            p4 <- p4 + geom_line(aes(y=cumRunoff.mm  , color= Scenario),size=0.5)
        }else
            if(input$Hill_variable == "Lateral.Flow..mm."){
                p4 <- p4 + geom_line(aes(y=cumLateralflow.mm, color= Scenario),size=0.5)
            }else
                if(input$Hill_variable == "Baseflow..mm."){
                    p4 <- p4 + geom_line(aes(y=cumBaseflow.mm, color= Scenario),size=0.5)
                }else
                    if(input$Hill_variable == "Soil.Loss..kg.ha."){
                        p4 <- p4 + geom_line(aes(y=cumSoilLoss.kg.ha, color= Scenario),size=0.5)
                    }else
                        if(input$Hill_variable == "Sediment.Deposition..kg.ha."){
                            p4 <- p4 + geom_line(aes(y=cumSedDep.kg.ha, color= Scenario),size=0.5)
                        }else
                            if(input$Hill_variable == "Sediment.Yield..kg.ha."){
                                p4 <- p4 + geom_line(aes(y=cumSedYield.kg.ha, color= Scenario),size=0.5)
                            }else
                                if(input$Hill_variable == "Solub..React..P..kg.ha.3."){
                                    p4 <- p4 + geom_line(aes(y=cumSRP.kg.ha.3, color= Scenario),size=0.5)
                                }else
                                    if(input$Hill_variable == "Particulate.P..kg.ha.3."){
                                        p4 <- p4 + geom_line(aes(y=cumParticulateP.kg.ha.3, color= Scenario),size=0.5)
                                    }else
                                        if(input$Hill_variable == "Total.P..kg.ha.3."){
                                            p4 <- p4 + geom_line(aes(y=cumTotalP.kg.ha.3, color= Scenario),size=0.5)
                                        }else
                                            if(input$Hill_variable == "Particle.Class.1.Fraction"){
                                                p4 <- p4 + geom_line(aes(y=cumParticle.Class.1.Fraction, color= Scenario),size=0.5)
                                            }else
                                                if(input$Hill_variable == "Particle.Class.2.Fraction"){
                                                    p4 <- p4 + geom_line(aes(y=cumParticle.Class.2.Fraction, color= Scenario),size=0.5)
                                                }else
                                                    if(input$Hill_variable == "Particle.Class.3.Fraction"){
                                                        p4 <- p4 + geom_line(aes(y=cumParticle.Class.3.Fraction, color= Scenario),size=0.5)
                                                    }else
                                                        if(input$Hill_variable == "Particle.Class.4.Fraction"){
                                                            p4 <- p4 + geom_line(aes(y=cumParticle.Class.4.Fraction, color= Scenario),size=0.5)
                                                        }else
                                                            if(input$Hill_variable == "Particle.Class.5.Fraction"){
                                                                p4 <- p4 + geom_line(aes(y=cumParticle.Class.5.Fraction, color= Scenario),size=0.5)
                                                            }else
                                                                if(input$Hill_variable == "Particle.Fraction.Under.0.016.mm"){
                                                                    p4 <- p4 + geom_line(aes(y=cumParticle.Fraction.Under.0.016.mm, color= Scenario),size=0.5)
                                                                }else
                                                                    if(input$Hill_variable == "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha."){
                                                                        p4 <- p4 + geom_line(aes(y=cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. , color= Scenario),size=0.5)
                                                                    }
        
        
        p4 <- p4 +  theme_bw()+
            theme(axis.title = element_text(size=10,color="Black",face="bold"),
                  axis.text = element_text(size=10,color="BLACK",face="bold"),
                  legend.title = element_text(size=10,color="BLACK",face="bold"),
                  legend.text = element_text(size=10,color="BLACK"),
                  legend.position = "none")+
            labs(x="Percent of total channel length",y=paste("Cumulative", input$Hill_variable, sep = " "), title="",colour="Scenario") 
        if(input$DefOrUserUpload_H == 'Default Data'){
            p4 <- p4 +
                scale_color_manual(values = c( "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"="#FF0000",
                                               "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli"="#B22222",
                                               "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
                                               "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
                                               "ModSevS.2020.ki5krcs.chn_12"="#DC143C",
                                               "LowSevS.2020.ki5krcs.chn_12"="#FF6347",
                                               "PrescFireS.2020.ki5krcs.chn_12"="#E9967A",
                                               "Thinn85.2020.ki5krcs.chn_12"="#7CFC00",
                                               "Thinn93.2020.kikrcs.chn_12"="#32CD32",
                                               "Thinn96.2020.kikrcs.chn_12"="#00FF00",
                                               "CurCond.2020.ki5krcs.chn_cs12"="#008000"))}else
                                                   if(input$DefOrUserUpload_H == 'Upload Data'){
                                                       p4 <- p4 +
                                                           scale_color_brewer(palette = "virdis")}
        
        
        
        p4
        
    })
    
   
    ## -----------------------------------------------------------------------------------------------------------##    
    ## ---------------------------------Plots:Channels-------------------------------------------------------##    
    ## -----------------------------------------------------------------------------------------------------------##        
    
    output$Plot6 <- renderPlotly({
        req(input$Chan_variable)
        p6 <- chn_arr_by_var_CA() %>% ggplot(aes(x= cumPercChanArea))
        if(input$Chan_variable ==  "Discharge..mm."){
            p6 <- p6 + geom_line(aes(y=cumDischarge.mm  , color= Scenario),size=0.5)}else
                if(input$Chan_variable ==  "Sediment.Yield..tonne."){
                    p6 <- p6 + geom_line(aes(y=cumSediment.Yield..tonne.  , color= Scenario),size=0.5)}else
                        if(input$Chan_variable ==  "Channel.Erosion..tonne."){
                            p6 <- p6 + geom_line(aes(y=cumChannel.Erosion..tonne.  , color= Scenario),size=0.5)}else
                                if(input$Chan_variable ==  "Upland.Charge..mm."){
                                    p6 <- p6 + geom_line(aes(y=cumUpland.Charge..mm.  , color= Scenario),size=0.5)}else
                                        if(input$Chan_variable ==  "Lateral.Flow..mm."){
                                            p6 <- p6 + geom_line(aes(y=cumLateral.Flow..mm.  , color= Scenario),size=0.5)}else
                                                if(input$Chan_variable ==  "Solub..React..P..kg.ha."){
                                                    p6 <- p6 + geom_line(aes(y=cumSRP.kg.ha.  , color= Scenario),size=0.5)}else
                                                        if(input$Chan_variable ==  "Particulate.P..kg.ha."){
                                                            p6 <- p6 + geom_line(aes(y=cumParticulateP.kg.ha.  , color= Scenario),size=0.5)}else
                                                                if(input$Chan_variable ==  "Total.P..kg.ha."){
                                                                    p6 <- p6 + geom_line(aes(y=cumTotalP.kg.ha.  , color= Scenario),size=0.5)}
        
        p6 <- p6 +  theme_bw()+
            theme(axis.title = element_text(size=10,color="Black",face="bold"),
                  axis.text = element_text(size=10,color="BLACK",face="bold"),
                  legend.title = element_text(size=10,color="BLACK",face="bold"),
                  legend.text = element_text(size=10,color="BLACK"),
                  legend.position = "none")+
            labs(x="Percent of total channel area",y=paste("Percent of total ", input$Chan_variable, sep = " "), title="",colour="Scenario")
        if(input$DefOrUserUpload_C == 'Default Data'){
            p6 <- p6 +
                scale_color_manual(values = c( "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"="#FF0000",
                                               "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli"="#B22222",
                                               "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
                                               "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
                                               "ModSevS.2020.ki5krcs.chn_12"="#DC143C",
                                               "LowSevS.2020.ki5krcs.chn_12"="#FF6347",
                                               "PrescFireS.2020.ki5krcs.chn_12"="#E9967A",
                                               "Thinn85.2020.ki5krcs.chn_12"="#7CFC00",
                                               "Thinn93.2020.kikrcs.chn_12"="#32CD32",
                                               "Thinn96.2020.kikrcs.chn_12"="#00FF00",
                                               "CurCond.2020.ki5krcs.chn_cs12"="#008000"))}else
                                                   if(input$DefOrUserUpload_C == 'Upload data'){
                                                       p6 <- p6 +
                                                           scale_color_brewer(palette = "virdis")}
        
        
        
        p6
        
    })
    
    
    output$Plot6_abs <- renderPlotly({
        req(input$Chan_variable)
        p6 <- chn_arr_by_var_CA_abs() %>% ggplot(aes(x= cumPercChanArea))
        if(input$Chan_variable ==  "Discharge..mm."){
            p6 <- p6 + geom_line(aes(y=cumDischarge.mm  , color= Scenario),size=0.5)}else
                if(input$Chan_variable ==  "Sediment.Yield..tonne."){
                    p6 <- p6 + geom_line(aes(y=cumSediment.Yield..tonne.  , color= Scenario),size=0.5)}else
                        if(input$Chan_variable ==  "Channel.Erosion..tonne."){
                            p6 <- p6 + geom_line(aes(y=cumChannel.Erosion..tonne.  , color= Scenario),size=0.5)}else
                                if(input$Chan_variable ==  "Upland.Charge..mm."){
                                    p6 <- p6 + geom_line(aes(y=cumUpland.Charge..mm.  , color= Scenario),size=0.5)}else
                                        if(input$Chan_variable ==  "Lateral.Flow..mm."){
                                            p6 <- p6 + geom_line(aes(y=cumLateral.Flow..mm.  , color= Scenario),size=0.5)}else
                                                if(input$Chan_variable ==  "Solub..React..P..kg.ha."){
                                                    p6 <- p6 + geom_line(aes(y=cumSRP.kg.ha.  , color= Scenario),size=0.5)}else
                                                        if(input$Chan_variable ==  "Particulate.P..kg.ha."){
                                                            p6 <- p6 + geom_line(aes(y=cumParticulateP.kg.ha.  , color= Scenario),size=0.5)}else
                                                                if(input$Chan_variable ==  "Total.P..kg.ha."){
                                                                    p6 <- p6 + geom_line(aes(y=cumTotalP.kg.ha.  , color= Scenario),size=0.5)}
        
        p6 <- p6 +  theme_bw()+
            theme(axis.title = element_text(size=10,color="Black",face="bold"),
                  axis.text = element_text(size=10,color="BLACK",face="bold"),
                  legend.title = element_text(size=10,color="BLACK",face="bold"),
                  legend.text = element_text(size=10,color="BLACK"),
                  legend.position = "none")+
            labs(x="Percent of total channel area",y=paste("Percent of total ", input$Chan_variable, sep = " "), title="",colour="Scenario")
        if(input$DefOrUserUpload_C == 'Default Data'){
            p6 <- p6 +
                scale_color_manual(values = c( "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"="#FF0000",
                                               "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli"="#B22222",
                                               "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
                                               "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
                                               "ModSevS.2020.ki5krcs.chn_12"="#DC143C",
                                               "LowSevS.2020.ki5krcs.chn_12"="#FF6347",
                                               "PrescFireS.2020.ki5krcs.chn_12"="#E9967A",
                                               "Thinn85.2020.ki5krcs.chn_12"="#7CFC00",
                                               "Thinn93.2020.kikrcs.chn_12"="#32CD32",
                                               "Thinn96.2020.kikrcs.chn_12"="#00FF00",
                                               "CurCond.2020.ki5krcs.chn_cs12"="#008000"))}else
                                                   if(input$DefOrUserUpload_C == 'Upload data'){
                                                       p6 <- p6 +
                                                           scale_color_brewer(palette = "virdis")}
        
        
        
        p6
        
    })
    
    
    
    
    
    output$Plot7 <- renderPlotly({
        req(input$Chan_variable)
        p7 <- chn_arr_by_var_CL() %>% ggplot(aes(x= cumPercLen))
        if(input$Chan_variable ==  "Discharge..mm."){
            p7 <- p7 + geom_line(aes(y=cumDischarge.mm  , color= Scenario),size=0.5)}else
                if(input$Chan_variable ==  "Sediment.Yield..tonne."){
                    p7 <- p7 + geom_line(aes(y=cumSediment.Yield..tonne.  , color= Scenario),size=0.5)}else
                        if(input$Chan_variable ==  "Channel.Erosion..tonne."){
                            p7 <- p7 + geom_line(aes(y=cumChannel.Erosion..tonne.  , color= Scenario),size=0.5)}else
                                if(input$Chan_variable ==  "Upland.Charge..mm."){
                                    p7 <- p7 + geom_line(aes(y=cumUpland.Charge..mm.  , color= Scenario),size=0.5)}else
                                        if(input$Chan_variable ==  "Lateral.Flow..mm."){
                                            p7 <- p7 + geom_line(aes(y=cumLateral.Flow..mm.  , color= Scenario),size=0.5)}else
                                                if(input$Chan_variable ==  "Solub..React..P..kg.ha."){
                                                    p7 <- p7 + geom_line(aes(y=cumSRP.kg.ha.  , color= Scenario),size=0.5)}else
                                                        if(input$Chan_variable ==  "Particulate.P..kg.ha."){
                                                            p7 <- p7 + geom_line(aes(y=cumParticulateP.kg.ha.  , color= Scenario),size=0.5)}else
                                                                if(input$Chan_variable ==  "Total.P..kg.ha."){
                                                                    p7 <- p7 + geom_line(aes(y=cumTotalP.kg.ha.  , color= Scenario),size=0.5)}
        
        p7 <- p7 +  theme_bw()+
            theme(axis.title = element_text(size=10,color="Black",face="bold"),
                  axis.text = element_text(size=10,color="BLACK",face="bold"),
                  legend.title = element_text(size=10,color="BLACK",face="bold"),
                  legend.text = element_text(size=10,color="BLACK"),
                  legend.position = "none")+
            labs(x="Percent of total channel length",y=paste("Percent of total ", input$Chan_variable, sep = " "), title="",colour="Scenario")
        if(input$DefOrUserUpload_C == 'Default Data'){
            p7 <- p7 +
                scale_color_manual(values = c( "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"="#FF0000",
                                               "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli"="#B22222",
                                               "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
                                               "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
                                               "ModSevS.2020.ki5krcs.chn_12"="#DC143C",
                                               "LowSevS.2020.ki5krcs.chn_12"="#FF6347",
                                               "PrescFireS.2020.ki5krcs.chn_12"="#E9967A",
                                               "Thinn85.2020.ki5krcs.chn_12"="#7CFC00",
                                               "Thinn93.2020.kikrcs.chn_12"="#32CD32",
                                               "Thinn96.2020.kikrcs.chn_12"="#00FF00",
                                               "CurCond.2020.ki5krcs.chn_cs12"="#008000"))}else
                                                   if(input$DefOrUserUpload_C == 'Upload data'){
                                                       p7 <- p7 +
                                                           scale_color_brewer(palette = "virdis")}
        
        
        
        p7
        
    })
    
    output$Plot7_abs <- renderPlotly({
        req(input$Chan_variable)
        p7 <- chn_arr_by_var_CL_abs() %>% ggplot(aes(x= cumPercLen))
        if(input$Chan_variable ==  "Discharge..mm."){
            p7 <- p7 + geom_line(aes(y=cumDischarge.mm  , color= Scenario),size=0.5)}else
                if(input$Chan_variable ==  "Sediment.Yield..tonne."){
                    p7 <- p7 + geom_line(aes(y=cumSediment.Yield..tonne.  , color= Scenario),size=0.5)}else
                        if(input$Chan_variable ==  "Channel.Erosion..tonne."){
                            p7 <- p7 + geom_line(aes(y=cumChannel.Erosion..tonne.  , color= Scenario),size=0.5)}else
                                if(input$Chan_variable ==  "Upland.Charge..mm."){
                                    p7 <- p7 + geom_line(aes(y=cumUpland.Charge..mm.  , color= Scenario),size=0.5)}else
                                        if(input$Chan_variable ==  "Lateral.Flow..mm."){
                                            p7 <- p7 + geom_line(aes(y=cumLateral.Flow..mm.  , color= Scenario),size=0.5)}else
                                                if(input$Chan_variable ==  "Solub..React..P..kg.ha."){
                                                    p7 <- p7 + geom_line(aes(y=cumSRP.kg.ha.  , color= Scenario),size=0.5)}else
                                                        if(input$Chan_variable ==  "Particulate.P..kg.ha."){
                                                            p7 <- p7 + geom_line(aes(y=cumParticulateP.kg.ha.  , color= Scenario),size=0.5)}else
                                                                if(input$Chan_variable ==  "Total.P..kg.ha."){
                                                                    p7 <- p7 + geom_line(aes(y=cumTotalP.kg.ha.  , color= Scenario),size=0.5)}
        
        p7 <- p7 +  theme_bw()+
            theme(axis.title = element_text(size=10,color="Black",face="bold"),
                  axis.text = element_text(size=10,color="BLACK",face="bold"),
                  legend.title = element_text(size=10,color="BLACK",face="bold"),
                  legend.text = element_text(size=10,color="BLACK"),
                  legend.position = "none")+
            labs(x="Percent of total channel length",y=paste("Percent of total ", input$Chan_variable, sep = " "), title="",colour="Scenario")
        if(input$DefOrUserUpload_C == 'Default Data'){
            p7 <- p7 +
                scale_color_manual(values = c( "SimFire.2020.ki5krcs.chn_12_landisFuels_fut_cli_A2"="#FF0000",
                                               "SimFire.2020.ki5krcs.chn_12_landisFuels_obs_cli"="#B22222",
                                               "SimFire.2020.ki5krcs.chn_12_fccsFuels_obs_cli" = "#4d2525",
                                               "HighSevS.2020.ki5krcs.chn_12" = "#DC143C",
                                               "ModSevS.2020.ki5krcs.chn_12"="#DC143C",
                                               "LowSevS.2020.ki5krcs.chn_12"="#FF6347",
                                               "PrescFireS.2020.ki5krcs.chn_12"="#E9967A",
                                               "Thinn85.2020.ki5krcs.chn_12"="#7CFC00",
                                               "Thinn93.2020.kikrcs.chn_12"="#32CD32",
                                               "Thinn96.2020.kikrcs.chn_12"="#00FF00",
                                               "CurCond.2020.ki5krcs.chn_cs12"="#008000"))}else
                                                   if(input$DefOrUserUpload_C == 'Upload data'){
                                                       p7 <- p7 +
                                                           scale_color_brewer(palette = "virdis")}
        
        
        
        p7
        
    })
    
    ## -----------------------------------------------------------------------------------------------------------##    
    ## ---------------------------------Plots:Watershed-------------------------------------------------------##    
    ## -----------------------------------------------------------------------------------------------------------##        
    
    output$Plot9 <- renderPlotly({
        # req(input$Wshed_wshed)
        
        Wshed_subset <- Wshed_subset()
        if(input$AreaVsScen == 'allscen'){
            if (input$ScenVvar == "Heatmap") {
            d <-  Wshed_subset[,c(2,7:20)] %>% dplyr::mutate_if(is.numeric, scale)
            d.m <- reshape2::melt(d)
            
            
            # # TEST To SEE if the dataframe from the reactive func is accessible
            # output$tab1 <- renderTable(
            #     d.m %>% head(100) )
            
            a<-  ggplot(d.m, aes(Scenario, variable,  fill= value)) +
                geom_tile(inherit.aes = TRUE)  +
                scale_fill_distiller(palette =  "Spectral", direction = -1) +
                theme(
                    axis.text.x = element_text(angle = 90,colour = "Black"),
                    axis.text.y = element_text(colour = "Black"),
                    axis.title = element_blank()
                    
                )
            ggplotly(a)
            
        }else
            if (input$ScenVvar == "Bar Chart") {
                
                d <-  Wshed_subset[,c(2,7:20)]
                
                d.m <- reshape2::melt(d)
                
                ## Calculates percent contribution of each variable across all 
                ## the simulated scenarios
                d.m <- d.m %>%
                    group_by(variable) %>%
                    mutate(total = sum(value),
                           share = (value/total)*100) %>% 
                    ungroup()
                
                # # TEST To SEE if the dataframe from the reactive func is accessible
                # output$tab1 <- renderTable(
                #     d.m %>% head(100) )
                
                
                b<- ggplot(d.m) +
                    
                    geom_bar(aes(y = share, x = variable, fill = reorder(Scenario, -share)), stat = "identity", position = "dodge") +
                    theme(
                        axis.text.x = element_text(angle = 45, vjust = ,colour = "Black"),
                        axis.text.y = element_text(colour = "Black"),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank(),
                        legend.title = element_blank()
                    ) +coord_flip() + labs(y="Percent of total across all scenarios") + scale_fill_brewer(
                        palette = "RdYlGn") + theme(legend.position="none")
                ggplotly(b)
                
                
            }}else
                if(input$AreaVsScen == 'allwat'){
                    if (input$ScenVvar == "Heatmap") {
                        d <-  Wshed_subset[,c(3,7:20)] %>% dplyr::mutate_if(is.numeric, scale)
                        d.m <- reshape2::melt(d)
                        
                        
                        # # TEST To SEE if the dataframe from the reactive func is accessible
                        # output$tab1 <- renderTable(
                        #     d.m %>% head(100) )
                        
                        a<-  ggplot(d.m, aes(Watershed, variable,  fill= value)) +
                            geom_tile(inherit.aes = TRUE)  +
                            scale_fill_distiller(palette =  "Spectral", direction = -1) +
                            theme(
                                axis.text.x = element_text(angle = 90,colour = "Black"),
                                axis.text.y = element_text(colour = "Black"),
                                axis.title = element_blank()
                                
                            )
                        ggplotly(a)
                        
                    }else
                        if (input$ScenVvar == "Bar Chart") {
                            
                            d <-  Wshed_subset[,c(3,7:20)]
                            
                            d.m <- reshape2::melt(d)
                            
                            ## Calculates percent contribution of each variable across all 
                            ## the simulated scenarios
                            d.m <- d.m %>%
                                group_by(variable) %>%
                                mutate(total = sum(value),
                                       share = (value/total)*100) %>% 
                                ungroup()
                            
                            # # TEST To SEE if the dataframe from the reactive func is accessible
                            # output$tab1 <- renderTable(
                            #     d.m %>% head(100) )
                            
                            
                            b<- ggplot(d.m) +
                                
                                geom_bar(aes(y = share, x = variable, fill = reorder(Watershed, -share)), stat = "identity", position = "dodge") +
                                theme(
                                    axis.text.x = element_text(angle = 45, vjust = ,colour = "Black"),
                                    axis.text.y = element_text(colour = "Black"),
                                    axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                                    legend.title = element_blank()
                                ) +coord_flip() + labs(y="Percent of total across all Watersheds") + scale_fill_brewer(
                                    palette = "RdYlGn") + theme(legend.position="none")
                            ggplotly(b)
                            
                            
                        }
                }
    })
    
    ## -----------------------------------------------------------------------------------------------------------##    
    ## ---------------------------------Plots:Spatial DF-------------------------------------------------------##    
    ## -----------------------------------------------------------------------------------------------------------##        
    
    #### from what I understand WGS84 latlon coord system needed to use with leaflet 
    
    output$Plot11 <- leaflet::renderLeaflet({
        req(Spatial_subset())
        req(input$S_scen)
        req(input$S_variable)
        tm <- tm_shape(Spatial_subset()) + 
            tmap::tm_polygons(input$S_variable,
                              id = "watershed",
                              palette = "viridis",
                              legend.hist = TRUE, style = "log10_pretty")
        
        
        tmap_leaflet(tm)
    })
    
    
    
    ## -----------------------------------------------------------------------------------------------------------##    
    ## ---------------------------------Data Summary Tables logic-------------------------------------------------------##    
    ## -----------------------------------------------------------------------------------------------------------##        
    
    ##### DF for summary datatables on the hillslopes tab 
    
    sed_stats_df <- reactive({
        
        if (input$summary_DT_by_var_H == "Landuse") {
            hill_subset() %>% dplyr::filter(Scenario %in% input$Hill_scen) %>%
                dplyr::arrange_at(.vars = input$Hill_variable, desc)%>%
                dplyr::mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100) %>%
                dplyr::filter(cumPercArea<input$thresh_H)%>% 
                dplyr::select(LanduseDesc, Slope, Sediment.Yield..kg.ha.) %>%
                group_by(LanduseDesc) %>% dplyr::summarise_if(is.numeric, list(mean=mean)) %>%
                dplyr::arrange(desc(Sediment.Yield..kg.ha._mean))%>% dplyr::mutate_if(is.numeric, round,2) 
        }else
            if(input$summary_DT_by_var_H == "Soiltype") {
                hill_subset() %>% dplyr::filter(Scenario %in% input$Hill_scen) %>%
                    dplyr::arrange_at(.vars = input$Hill_variable, desc)%>% 
                    dplyr::mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100) %>%
                    dplyr::filter(cumPercArea<input$thresh_H)%>%
                    dplyr::select(SoilDesc, Slope, Sediment.Yield..kg.ha.) %>%
                    group_by(SoilDesc) %>% dplyr::summarise_if(is.numeric, list(mean=mean)) %>% 
                    dplyr::arrange(desc(Sediment.Yield..kg.ha._mean)) %>% dplyr::mutate_if(is.numeric, round,2) 
                
            }else
                if(input$summary_DT_by_var_H == "Both") {
                    hill_subset() %>% dplyr::filter(Scenario %in% input$Hill_scen) %>%
                        dplyr::arrange_at(.vars = input$Hill_variable, desc)%>% 
                        dplyr::mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100) %>%
                        dplyr::filter(cumPercArea<input$thresh_H)%>%
                        dplyr::select(SoilDesc, LanduseDesc, Slope, Sediment.Yield..kg.ha.) %>%
                        group_by(SoilDesc, LanduseDesc) %>% dplyr::summarise_if(is.numeric, list(mean=mean)) %>% 
                        dplyr::arrange(desc(Sediment.Yield..kg.ha._mean)) %>% dplyr::mutate_if(is.numeric, round,2) 
                    
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
    
    
    ##### render table summary output
    output$Sed_stats_by_category <- DT::renderDataTable(
        sed_stats_df(), extensions = 'Scroller', options = list(
            deferRender = TRUE,
            scroller = TRUE,
            scrollY=400,
            compact = TRUE,
            columnDefs = list(list(className = 'dt-left')),
            fillContainer = T,
            class = "display"
            
            
        ))
    
    #### West shore summary stats table
    
    # output$WS_Sed_stats_by_category <- DT::renderDataTable(
    #     WS_sed_stats_df())
    # 
    
    
    ## -----------------------------------------------------------------------------------------------------------##    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)