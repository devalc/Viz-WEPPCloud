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


ui <- navbarPage("viz-WEPP",
                 
                 
                 ## set the theme
                 
                 theme = shinytheme(theme = "sandstone"),
                 
                 tabPanel("Hillslope",
                          sidebarPanel(
                              #uploading the file 
                              # fileinput() function is used to get the file upload contorl option
                              fileInput("Hill_file",label ="Uplaod WEPPCloud Outputs (*.csv files)", 
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
                              fileInput("Chan_file",label ="Uplaod WEPPCloud Outputs (*.csv files)", 
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
                                  column(6,  plotlyOutput("Plot1")),
                                  column(6, plotlyOutput("Plot2"))
                              ),
                              fluidRow(
                                  column(6, plotlyOutput("Plot3")),
                                  column(6, plotlyOutput("Plot4"))
                              )
                          )),
                 
                 tabPanel("Watershed",
                          sidebarPanel(
                              #uploading the file 
                              # fileinput() function is used to get the file upload contorl option
                              fileInput("Wshed_file",label ="Uplaod WEPPCloud Outputs (*.csv files)", 
                                        multiple = F, placeholder = "No file selected", accept = ".csv" ),
                              helpText("max. file size is 32MB"),
                              
                              
                              uiOutput("Wshed_selectfile"),
                              uiOutput("Wshed_wshed"),
                              uiOutput("Wshed_var"),
                              uiOutput("Wshed_scen")
                          ),
                          
                          # Main panel for displaying outputs ----
                          mainPanel(
                              
                              fluidPage(
                                  # plotlyOutput("Plot5" ,height = "800px", width ="1200px")
                                  plotOutput("Plot5",height = "800px", width ="800px" )
                                  
                              ))
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
                                  column(6, plotOutput("Plot9")),
                                  column(6, plotOutput("Plot10"))
                              ),
                              fluidRow(
                                  column(6, plotOutput("Plot11")),
                                  column(6, plotOutput("Plot12"))
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
    
    ################# Plotting logic for HILLSLOPE DF #################
    
    hill_subset <- reactive({
        req(input$Hill_file)
        Hill_data() %>% 
            dplyr::filter(Watershed %in% input$Hill_wshed) 
    })
    
    ############## Dataframe calculating cumulative percent of total variable   ############## 
    
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
    
    
    ############## Dataframe calculating cumulative absolute value of variable   ############## 
    
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
    
    
    # TEST To SEE if the dataframe from the reactive func is accessible
    # output$tab1 <- renderTable(
    #   hill_arr_by_var() %>% head(100) )
    
    
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
            labs(x="Percent of total hillslope area",y=paste("Percent of total", input$Hill_variable, sep = " "), title="",colour="Scenario")
        
        
        
        
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
            labs(x="Percent of total channel length",y=paste("Percent of total", input$Hill_variable, sep = " "), title="",colour="Scenario")
        
        
        
        
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
            labs(x="Percent of total hillslope area",y=paste("Cumulative", input$Hill_variable, sep = " "), title="",colour="Scenario")
        
        
        
        
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
            labs(x="Percent of total channel length",y=paste("Cumulative", input$Hill_variable, sep = " "), title="",colour="Scenario")
        # +scale_fill_brewer(palette="spectral")
        
        
        
        p4
        
    })
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

