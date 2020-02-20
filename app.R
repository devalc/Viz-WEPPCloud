# -*- coding: utf-8 -*-
#"""
#Created on Fri Jan 17 19:35:48 2020

#@author: Chinmay Deval

### 

library(shiny)
library(tidyverse)
library(shinythemes)
library(plotly)

options(shiny.maxRequestSize = 32*1024^2)


ui <- navbarPage("viz-WEPPCloud",
                 
                 
                 ## set the theme
                 
                 theme = shinytheme(theme = "sandstone"),
                 
                 tabPanel("Hillslope",
                          sidebarPanel(
                              #uploading the file 
                              # fileinput() function is used to get the file upload contorl option
                              fileInput("Hill_file",label ="Uplaod 'Hillslope' file (*_hill_*.csv)", 
                                        multiple = F, placeholder = "No file selected", accept = ".csv" ),
                              helpText("max. file size is 32MB"),
                              
                              
                              uiOutput("Hill_selectfile"),
                              uiOutput("Hill_wshed"),
                              uiOutput("Hill_var"),
                              uiOutput("Hill_scen")
                          ),
                          
                          # Main panel for displaying outputs ----
                          mainPanel(
                              
                              fluidRow(
                                  # column(6, tableOutput("tab1")),
                                  column(6, plotlyOutput("Plot_vs_cumPercArea")),
                                  column(6, plotlyOutput("Plot_vs_cumPercArea_abs"))
                              ),
                              fluidRow(
                                  column(6, plotlyOutput("Plot_vs_cumPercLen")),
                                  column(6, plotlyOutput("Plot_vs_cumPercLen_abs"))
                              )
                          )),
                 
                 tabPanel("Channel",
                          sidebarPanel(
                              #uploading the file 
                              # fileinput() function is used to get the file upload contorl option
                              fileInput("Chan_file",label ="Uplaod 'Channel' file (*_chn_*.csv)", 
                                        multiple = F, placeholder = "No file selected", accept = ".csv" ),
                              helpText("max. file size is 32MB"),
                              
                              
                              uiOutput("Chan_selectfile"),
                              uiOutput("Chan_wshed"),
                              uiOutput("Chan_var"),
                              uiOutput("Chan_scen")
                          ),
                          
                          # Main panel for displaying outputs ----
                          mainPanel(
                              
                              fluidRow(
                                  column(6,  plotlyOutput("Plot5")),
                                  column(6, plotlyOutput("Plot6"))
                              ),
                              fluidRow(
                                  column(12, plotlyOutput("Plot7"))
                                  # column(6, plotlyOutput("Plot8"))
                              )
                          )),
                 
                 tabPanel("Watershed",
                          sidebarPanel(
                              #uploading the file 
                              # fileinput() function is used to get the file upload contorl option
                              fileInput("Wshed_file",label ="Uplaod 'Watershed' file (*_out_*.csv)", 
                                        multiple = F, placeholder = "No file selected", accept = ".csv" ),
                              helpText("max. file size is 32MB"),
                              
                              
                              # uiOutput("Wshed_selectfile"),
                              uiOutput("Wshed_wshed"),
                              
                              radioButtons(inputId = "ScenVvar",label = "Select heatmap or specific variable",
                                           choices = c("Heatmap"="Heatmap","Bar Chart"="Bar Chart"), selected = "Heatmap")
                              
                              # uiOutput("Wshed_var"),
                              # uiOutput("Wshed_scen")
                          ),
                          
                          # Main panel for displaying outputs ----
                          mainPanel(
                              
                              fluidPage(
                                  # plotlyOutput("Plot5" ,height = "800px", width ="1200px")
                                  # column(12, tableOutput("tab1"))
                                  plotOutput("Plot9",height = "800px", width ="800px" )
                                  
                              )
                              )
                 ),
                 
                 tabPanel("Spatial-Viz",
                          sidebarPanel(
                              #uploading the file 
                              # fileinput() function is used to get the file upload contorl option
                              fileInput("spfile",label ="Uplaod WEPPCloud Outputs (*.csv files)", 
                                        multiple = F, placeholder = "No file selected", accept = ".csv" ),
                              helpText("max. file size is 32MB"),
                              
                              
                              uiOutput("sp_selectfile"),
                              uiOutput("sp_wshed"),
                              uiOutput("sp_var"),
                              uiOutput("sp_scen")
                          ),
                          
                          # Main panel for displaying outputs ----
                          mainPanel(
                              
                              fluidRow(
                                  column(6, plotOutput("Plot10")),
                                  column(6, plotOutput("Plot11"))
                              ),
                              fluidRow(
                                  column(6, plotOutput("Plot12")),
                                  column(6, plotOutput("Plot13"))
                              )
                          ))
                 
                 
)



# Define server logic required to draw a histogram

server <- function(input, output, session) {
    
    ######## Server logic for UI generation for hillslope tab ##########
    
    Hill_data <- reactive({
        file1 <- input$Hill_file
        if(is.null(file1)){return()}
        validate(
            need(grepl("hill", input$Hill_file) == TRUE, "Wrong file provided. Hillslope filename should have '_hill_' in filename")
        )
        read.table(file=file1$datapath,head=TRUE,sep=",")
        
    })
    
    output$Hill_var <- renderUI({
        req(input$Hill_file)
        selectInput("Hill_variable", "Select the variable of interest",  colnames(Hill_data()[1:25]),
                    selected = colnames(Hill_data()[10]) )
    })
    
    output$Hill_wshed <- renderUI({
        req(input$Hill_file)
        selectInput("Hill_wshed", "Select the watershed",  unique(Hill_data()$Watershed))
    })
    
    
    output$Wshed_wshed <- renderUI({
        req(input$Wshed_file)
        selectInput("Wshed_wshed", "Select the watershed",  unique(Wshed_data()$Watershed))
    })
    
    
    
    ######## Server logic for UI generation for  Channel tab ##########
    
    Chan_data <- reactive({
        file2 <- input$Chan_file
        if(is.null(file2)){return()}
        validate(
            need(grepl("chn", input$Chan_file) == TRUE, "Wrong file provided. Channel filename should have '_chn_' in filename")
        )
        read.table(file=file2$datapath,head=TRUE,sep=",")
        
    })
    
    output$Chan_var <- renderUI({
        req(input$Chan_file)
        selectInput("Chan_variable", "Select the variable of interest",  colnames(Chan_data()[7:17]),
                    selected = colnames(Chan_data()[10]) )
    })
    
    output$Chan_wshed <- renderUI({
        req(input$Chan_file)
        selectInput("Chan_wshed", "Select the watershed",  unique(Chan_data()$Watershed))
    })
    
    
    ######## Server logic for UI generation for  Watersheds tab ##########
    
    Wshed_data <- reactive({
        file3 <- input$Wshed_file
        if(is.null(file3)){return()}
        validate(
            need(grepl("out", input$Wshed_file) == TRUE, "Wrong file provided. Watershed filename should have '_out_' in filename")
        )
        read.table(file=file3$datapath,head=TRUE,sep=",")
        
    })
    
    output$Wshed_var <- renderUI({
        req(input$Wshed_file)
        selectInput("Wshed_variable", "Select the variable of interest",  colnames(Wshed_data()[7:17]),
                    selected = colnames(Wshed_data()[10]) )
    })
    
    output$Wshed_wshed <- renderUI({
        req(input$Wshed_file)
        selectInput("wshed", "Select the watershed",  unique(Wshed_data()$Watershed))
    })
    
    ################# Plotting logic for HILLSLOPE DF (subsetting)#################
    
    hill_subset <- reactive({
        req(input$Hill_file)
        Hill_data() %>% 
            dplyr::filter(Watershed %in% input$Hill_wshed) 
    })
    
    
    ################# Plotting logic for CHANNEL DF (subsetting)#################
    
    Chan_subset <- reactive({
        req(input$Chan_file)
        Chan_data() %>% 
            dplyr::filter(Watershed %in% input$Chan_wshed) 
    })
    
    
    ################# Plotting logic for WATERSHED DF (subsetting)#################
    
    Wshed_subset <- reactive({
        req(input$Wshed_file)
        Wshed_data() %>% 
            dplyr::filter(Watershed %in% input$wshed) 
    })
    
    # # TEST To SEE if the dataframe from the reactive func is accessible
                    output$tab1 <- renderTable(
                        Wshed_subset() %>% head(100) )
    
    ############## Dataframe calculating cumulative percent of total variable: Hillslope   ############## 
    
    hill_arr_by_var <- reactive({
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
            ) %>%
            ungroup()
    })
    
    
    ############## Dataframe calculating cumulative absolute value of variable: Hillslope   ############## 
    
    hill_arr_by_var_abs <- reactive({
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
            ) %>%
            ungroup()
    })  
    
    
    ############## Dataframe calculating cumulative percent of total variable: Channel   ############## 
    
    chn_arr_by_var <- reactive({
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
            ungroup()})
    
    
    ############## Dataframe calculating cumulative absolute value of variable: Channel   ############## 
    
    chn_arr_by_var_abs <- reactive({
        Chan_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$Chan_variable, desc)%>%
            mutate(cumPercChanArea = cumsum(Channel.Area..ha.)/sum(Channel.Area..ha.)*100,
                   cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
                   cumPercContriChanArea = cumsum(Contributing.Channel.Area..ha.)/sum(Contributing.Channel.Area..ha.)*100,
                   cumDischarge.mm = cumsum(Discharge..mm.),
                   cumSediment.Yield..tonne. = cumsum(Sediment.Yield..tonne.),
                   cumChannel.Erosion..tonne. = cumsum(Channel.Erosion..tonne.),
                   cumUpland.Charge..mm. = cumsum(Upland.Charge..mm.),
                   cumLateral.Flow..mm. = cumsum(Lateral.Flow..mm.),
                   cumSRP.kg.ha. = cumsum(Solub..React..P..kg.ha.),
                   cumParticulateP.kg.ha. = cumsum(Particulate.P..kg.ha.),
                   cumTotalP.kg.ha. = cumsum(Total.P..kg.ha.)) %>%
            ungroup()})
    
    
    ##############    ##############    ##############    ############## 
    ############## Hillslope plotting server logic   ############## 
    ##############    ##############    ##############    ############## 
    
    
    ############## plots of cumulative percent of total variable   ############## 
    ############## vs cumulative percent of total hillslope area/ channel length   ############## 
    
    output$Plot_vs_cumPercArea <- renderPlotly({
        
        req(input$Hill_variable)
        
        p1 <- hill_arr_by_var()  %>% ggplot(aes(x=cumPercArea))
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
                  legend.position = "none")+
            labs(x="Percent of total hillslope area",y=paste("Percent of total", input$Hill_variable, sep = " "), title="",colour="Scenario") +
            scale_color_manual(values = c( "SimFire.2020.kikrcs.chn"="#FF0000",
                                           "HighSev.2020.kikrcs.chn"="#B22222",
                                           "ModSev.2020.kikrcs.chn"="#DC143C",
                                           "LowSev.2020.kikrcs.chn"="#FF6347",
                                           "PrescFire.2020.kikrcs.chn"="#E9967A",
                                           "Thinn85.2020.kikrcs.chn"="#7CFC00",
                                           "Thinn93.2020.kikrcs.chn"="#32CD32",
                                           "Thinn96.2020.kikrcs.chn"="#00FF00",
                                           "CurCond.2020.ki5krcs.chn"="#008000"))
        
        
        
        
        p1
        
    })
    
    
    output$Plot_vs_cumPercLen <- renderPlotly({
        
        req(input$Hill_variable)
        
        p3 <- hill_arr_by_var()  %>% ggplot(aes(x=cumPercLen))
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
            labs(x="Percent of total channel length",y=paste("Percent of total", input$Hill_variable, sep = " "), title="",colour="Scenario") +
            scale_color_manual(values = c( "SimFire.2020.kikrcs.chn"="#FF0000",
                                           "HighSev.2020.kikrcs.chn"="#B22222",
                                           "ModSev.2020.kikrcs.chn"="#DC143C",
                                           "LowSev.2020.kikrcs.chn"="#FF6347",
                                           "PrescFire.2020.kikrcs.chn"="#E9967A",
                                           "Thinn85.2020.kikrcs.chn"="#7CFC00",
                                           "Thinn93.2020.kikrcs.chn"="#32CD32",
                                           "Thinn96.2020.kikrcs.chn"="#00FF00",
                                           "CurCond.2020.ki5krcs.chn"="#008000"))
        
        
        
        
        p3
        
    })
    
    
    ############## plots of cumulative absolute values of variable   ############## 
    ############## vs cumulative percent of total hillslope area/ channel length   ############## 
    
    output$Plot_vs_cumPercArea_abs <- renderPlotly({
        
        req(input$Hill_variable)
        
        p2 <- hill_arr_by_var_abs()  %>% ggplot(aes(x=cumPercArea))
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
            labs(x="Percent of total hillslope area",y=paste("Cumulative", input$Hill_variable, sep = " "), title="",colour="Scenario") +
            scale_color_manual(values = c( "SimFire.2020.kikrcs.chn"="#FF0000",
                                           "HighSev.2020.kikrcs.chn"="#B22222",
                                           "ModSev.2020.kikrcs.chn"="#DC143C",
                                           "LowSev.2020.kikrcs.chn"="#FF6347",
                                           "PrescFire.2020.kikrcs.chn"="#E9967A",
                                           "Thinn85.2020.kikrcs.chn"="#7CFC00",
                                           "Thinn93.2020.kikrcs.chn"="#32CD32",
                                           "Thinn96.2020.kikrcs.chn"="#00FF00",
                                           "CurCond.2020.ki5krcs.chn"="#008000"))
        
        
        
        
        p2
        
    })
    
    
    
    output$Plot_vs_cumPercLen_abs <- renderPlotly({
        
        req(input$Hill_variable)
        
        p4 <- hill_arr_by_var_abs()  %>% ggplot(aes(x=cumPercLen))
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
            labs(x="Percent of total channel length",y=paste("Cumulative", input$Hill_variable, sep = " "), title="",colour="Scenario") +
            scale_color_manual(values = c( "SimFire.2020.kikrcs.chn"="#FF0000",
                                           "HighSev.2020.kikrcs.chn"="#B22222",
                                           "ModSev.2020.kikrcs.chn"="#DC143C",
                                           "LowSev.2020.kikrcs.chn"="#FF6347",
                                           "PrescFire.2020.kikrcs.chn"="#E9967A",
                                           "Thinn85.2020.kikrcs.chn"="#7CFC00",
                                           "Thinn93.2020.kikrcs.chn"="#32CD32",
                                           "Thinn96.2020.kikrcs.chn"="#00FF00",
                                           "CurCond.2020.ki5krcs.chn"="#008000"))
        # +scale_fill_brewer(palette="spectral")
        
        
        
        p4
        
    })
    
    
    ##############    ##############    ##############    ############## 
    ############## Channel plotting server logic   ############## 
    ##############    ##############    ##############    ############## 
    
    
    output$Plot5 <- renderPlotly({
        req(input$Chan_variable)
        p5 <- chn_arr_by_var() %>% ggplot(aes(x= cumPercContriChanArea))
        if(input$Chan_variable ==  "Discharge..mm."){
            p5 <- p5 + geom_line(aes(y=cumDischarge.mm  , color= Scenario),size=0.5)}else
                if(input$Chan_variable ==  "Sediment.Yield..tonne."){
                    p5 <- p5 + geom_line(aes(y=cumSediment.Yield..tonne.  , color= Scenario),size=0.5)}else
                        if(input$Chan_variable ==  "Channel.Erosion..tonne."){
                            p5 <- p5 + geom_line(aes(y=cumChannel.Erosion..tonne.  , color= Scenario),size=0.5)}else
                                if(input$Chan_variable ==  "Upland.Charge..mm."){
                                    p5 <- p5 + geom_line(aes(y=cumUpland.Charge..mm.  , color= Scenario),size=0.5)}else
                                        if(input$Chan_variable ==  "Lateral.Flow..mm."){
                                            p5 <- p5 + geom_line(aes(y=cumLateral.Flow..mm.  , color= Scenario),size=0.5)}else
                                                if(input$Chan_variable ==  "Solub..React..P..kg.ha."){
                                                    p5 <- p5 + geom_line(aes(y=cumSRP.kg.ha.  , color= Scenario),size=0.5)}else
                                                        if(input$Chan_variable ==  "Particulate.P..kg.ha."){
                                                            p5 <- p5 + geom_line(aes(y=cumParticulateP.kg.ha.  , color= Scenario),size=0.5)}else
                                                                if(input$Chan_variable ==  "Total.P..kg.ha."){
                                                                    p5 <- p5 + geom_line(aes(y=cumTotalP.kg.ha.  , color= Scenario),size=0.5)}
        
        p5 <- p5 +  theme_bw()+
            theme(axis.title = element_text(size=10,color="Black",face="bold"),
                  axis.text = element_text(size=10,color="BLACK",face="bold"),
                  legend.title = element_text(size=10,color="BLACK",face="bold"),
                  legend.text = element_text(size=10,color="BLACK"),
                  legend.position = "none")+
            labs(x="Percent of total contributing channel area",y=paste("Percent of total ", input$Chan_variable, sep = " "), title="",colour="Scenario")+
            scale_color_manual(values = c( "SimFire.2020.kikrcs.chn"="#FF0000",
                                           "HighSev.2020.kikrcs.chn"="#B22222",
                                           "ModSev.2020.kikrcs.chn"="#DC143C",
                                           "LowSev.2020.kikrcs.chn"="#FF6347",
                                           "PrescFire.2020.kikrcs.chn"="#E9967A",
                                           "Thinn85.2020.kikrcs.chn"="#7CFC00",
                                           "Thinn93.2020.kikrcs.chn"="#32CD32",
                                           "Thinn96.2020.kikrcs.chn"="#00FF00",
                                           "CurCond.2020.ki5krcs.chn"="#008000"))
            # +scale_color_brewer(palette="RdYlGn") 
        
        
        
        p5
        
    })
    
    
    
    output$Plot6 <- renderPlotly({
        req(input$Chan_variable)
        p6 <- chn_arr_by_var() %>% ggplot(aes(x= cumPercChanArea))
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
            labs(x="Percent of total channel area",y=paste("Percent of total ", input$Chan_variable, sep = " "), title="",colour="Scenario")+
            scale_color_manual(values = c( "SimFire.2020.kikrcs.chn"="#FF0000",
                                           "HighSev.2020.kikrcs.chn"="#B22222",
                                           "ModSev.2020.kikrcs.chn"="#DC143C",
                                           "LowSev.2020.kikrcs.chn"="#FF6347",
                                           "PrescFire.2020.kikrcs.chn"="#E9967A",
                                           "Thinn85.2020.kikrcs.chn"="#7CFC00",
                                           "Thinn93.2020.kikrcs.chn"="#32CD32",
                                           "Thinn96.2020.kikrcs.chn"="#00FF00",
                                           "CurCond.2020.ki5krcs.chn"="#008000"))
        # +scale_color_brewer(palette="RdYlGn") 
        
        
        
        p6
        
    })
    
    
    
    output$Plot7 <- renderPlotly({
        req(input$Chan_variable)
        p7 <- chn_arr_by_var() %>% ggplot(aes(x= cumPercLen))
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
            labs(x="Percent of total channel length",y=paste("Percent of total ", input$Chan_variable, sep = " "), title="",colour="Scenario")+
            scale_color_manual(values = c( "SimFire.2020.kikrcs.chn"="#FF0000",
                                           "HighSev.2020.kikrcs.chn"="#B22222",
                                           "ModSev.2020.kikrcs.chn"="#DC143C",
                                           "LowSev.2020.kikrcs.chn"="#FF6347",
                                           "PrescFire.2020.kikrcs.chn"="#E9967A",
                                           "Thinn85.2020.kikrcs.chn"="#7CFC00",
                                           "Thinn93.2020.kikrcs.chn"="#32CD32",
                                           "Thinn96.2020.kikrcs.chn"="#00FF00",
                                           "CurCond.2020.ki5krcs.chn"="#008000"))
        # +scale_color_brewer(palette="RdYlGn") 
        
        
        
        p7
        
    })
    
    ##############    ##############    ##############    ############## 
    ############## Watershed Summary plotting server logic   ############## 
    ##############    ##############    ##############    ##############  
    
    output$Plot9 <- renderPlot({
        # req(input$Wshed_wshed)

        Wshed_subset <- Wshed_subset()

        if (input$ScenVvar == "Heatmap") {
            d <-  Wshed_subset[,c(2,7:20)] %>% dplyr::mutate_if(is.numeric, scale)

            d.m <- reshape2::melt(d)


            # # TEST To SEE if the dataframe from the reactive func is accessible
            output$tab1 <- renderTable(
                d.m %>% head(100) )

            # a <- plotly::plot_ly(x = ~Scenario, y= ~variable,  z= d.m, type = "heatmap")

            ggplot(d.m, aes(Scenario, variable,  fill= value)) +
                geom_tile(inherit.aes = TRUE)  +
                scale_fill_distiller(palette = "BrBG", direction = -1) +
                theme(
                    axis.text.x = element_text(angle = 90,colour = "Black", size = 12, face = "bold"),
                    axis.text.y = element_text(colour = "Black", size = 12, face = 'bold'),
                    axis.title = element_blank()

                )

            }else
                if (input$ScenVvar == "Bar Chart") {

                    d <-  Wshed_subset[,c(2,7:20)]

                    d.m <- reshape2::melt(d)

                    d.m <- d.m %>%
                        group_by(variable) %>%
                        mutate(total = sum(value),
                               share = (value/total)*100) %>%
                        ungroup()

                    # # TEST To SEE if the dataframe from the reactive func is accessible
                    # output$tab1 <- renderTable(
                    #     d.m %>% head(100) )


                    ggplot(d.m) +

                        geom_bar(aes(y = share, x = variable, fill = Scenario), stat = "identity", position = "dodge") +
                        theme(
                            axis.text.x = element_text(angle = 45, vjust = ,colour = "Black", size = 12, face = "bold"),
                            axis.text.y = element_text(colour = "Black", size = 12, face = 'bold'),
                            axis.title.x = element_blank(),
                            axis.title.y = element_text(colour = "Black", size = 12, face = 'bold')
                        ) +coord_flip() + labs(x="Percent of total across all scenarios") + scale_fill_brewer(
                            palette = "RdYlGn")


                }
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

