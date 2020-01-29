# @ Chinmay Deval
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# library(dplyr)
library(tidyverse)


# Data Preparation Steps

data <- read.csv("C:/Chinmay/Github/Process-WEPPCloud-Outputs/new_data/lt2020_2_hill_summary.csv")
unique_watsheds <- as.character(unique(data$Watershed))
unique_scenario <- as.character(unique(data$Scenario))


ui <- fluidPage(
  
  # App title ----
  titlePanel("viz-WEPP-Results"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId="Watershed",label="Choose Watershed",choices = unique_watsheds,
                  selected = "Blue",multiple = F),
      
      selectInput(inputId="Scenario",label="Choose Scenario",choices = unique_scenario,
                  selected = "Blue",multiple = F),
      
      # radioButtons(inputId = "border1",label = "Select Border",choices = c("Black"="#000000","White"="#ffffff")),
      
      selectInput(inputId="var1",label="Choose Variable",choices = c("Runoff..mm."="Runoff..mm.",
                                                                        "Lateral.Flow..mm."="Lateral.Flow..mm.",
                                                                        "Baseflow..mm."="Baseflow..mm.",
                                                                        "Soil.Loss..kg.ha."="Soil.Loss..kg.ha.",
                                                                        "Sediment.Deposition..kg.ha."="Sediment.Deposition..kg.ha.",
                                                                        "Sediment.Yield..kg.ha."="Sediment.Yield..kg.ha.",
                                                                        "Solub..React..P..kg.ha.3."="Solub..React..P..kg.ha.3.",
                                                                     "Particulate.P..kg.ha.3."="Particulate.P..kg.ha.3.",
                                                                     "Total.P..kg.ha.3."="Total.P..kg.ha.3.",
                                                                     "Particle.Class.1.Fraction" = "Particle.Class.1.Fraction",
                                                                     "Particle.Class.2.Fraction" = "Particle.Class.2.Fraction",
                                                                     "Particle.Class.3.Fraction" = "Particle.Class.3.Fraction",
                                                                     "Particle.Class.4.Fraction" = "Particle.Class.4.Fraction",
                                                                     "Particle.Class.5.Fraction" = "Particle.Class.5.Fraction",
                                                                     "Particle.Fraction.Under.0.016.mm" = "Particle.Fraction.Under.0.016.mm",
                                                                     "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha." = "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha."),
                  selected = "Runoff..mm.",multiple = F)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      plotOutput(outputId = "distPlot1")
      # plotOutput(outputId = "distPlot2")
    )
  )
)




# Define server logic required to draw a histogram ----
server <- function(input, output){
  
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
   
  data_subset <- reactive({
      filter(data, Watershed %in% input$Watershed, Scenario %in% input$Scenario) %>%
          mutate(Percent_HillSlopeArea..ha.= (Hillslope.Area..ha./sum(Hillslope.Area..ha.))*100,
                 Percent_Slope_length..m = (Length..m./sum(Length..m.))*100)
  })
  
  data_arr_by_area <- reactive({
      data_subset() %>% arrange(Percent_HillSlopeArea..ha.) %>% mutate(cumPercArea = cumsum(Percent_HillSlopeArea..ha.),
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
                                                                       )
  })
    
  
  data_arr_by_len <- reactive({
      data_subset() %>% arrange(Percent_Slope_length..m) %>% mutate(cumPercLength = cumsum(Percent_Slope_length..m),
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
      )
  })

  
    output$distPlot <- renderPlot({
    
        p1 <- data_arr_by_area()  %>% ggplot(aes(x=cumPercArea))
        if(input$var1 == "Runoff..mm."){
            p1 <- p1 + geom_line(aes(y=cumRunoff.mm),size=1)
        }else
            if(input$var1 == "Lateral.Flow..mm."){
                p1 <- p1 + geom_line(aes(y=cumLateralflow.mm),size=1)
            }else
                if(input$var1 == "Baseflow..mm."){
                    p1 <- p1 + geom_line(aes(y=cumBaseflow.mm),size=1)
                }else
                    if(input$var1 == "Soil.Loss..kg.ha."){
                        p1 <- p1 + geom_line(aes(y=cumSoilLoss.kg.ha),size=1)
                    }else
                        if(input$var1 == "Sediment.Deposition..kg.ha."){
                            p1 <- p1 + geom_line(aes(y=cumSedDep.kg.ha),size=1)
                        }else
                            if(input$var1 == "Sediment.Yield..kg.ha."){
                                p1 <- p1 + geom_line(aes(y=cumSedYield.kg.ha),size=1)
                            }else
                                if(input$var1 == "Solub..React..P..kg.ha.3."){
                                    p1 <- p1 + geom_line(aes(y=cumSRP.kg.ha.3),size=1)
                                }else
                                    if(input$var1 == "Particulate.P..kg.ha.3."){
                                        p1 <- p1 + geom_line(aes(y=cumParticulateP.kg.ha.3),size=1)
                                    }else
                                        if(input$var1 == "Total.P..kg.ha.3."){
                                            p1 <- p1 + geom_line(aes(y=cumTotalP.kg.ha.3),size=1)
                                        }else
                                            if(input$var1 == "Particle.Class.1.Fraction"){
                                                p1 <- p1 + geom_line(aes(y=cumParticle.Class.1.Fraction),size=1)
                                            }else
                                                if(input$var1 == "Particle.Class.2.Fraction"){
                                                    p1 <- p1 + geom_line(aes(y=cumParticle.Class.2.Fraction),size=1)
                                                }else
                                                    if(input$var1 == "Particle.Class.3.Fraction"){
                                                        p1 <- p1 + geom_line(aes(y=cumParticle.Class.3.Fraction),size=1)
                                                    }else
                                                        if(input$var1 == "Particle.Class.4.Fraction"){
                                                            p1 <- p1 + geom_line(aes(y=cumParticle.Class.4.Fraction),size=1)
                                                        }else
                                                            if(input$var1 == "Particle.Class.5.Fraction"){
                                                                p1 <- p1 + geom_line(aes(y=cumParticle.Class.5.Fraction),size=1)
                                                            }else
                                                                if(input$var1 == "Particle.Fraction.Under.0.016.mm"){
                                                                    p1 <- p1 + geom_line(aes(y=cumParticle.Fraction.Under.0.016.mm),size=1)
                                                                }else
                                                                    if(input$var1 == "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha."){
                                                                        p1 <- p1 + geom_line(aes(y=cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha.),size=1)
                                                                    }
        
        
    p1 <- p1 +  theme_bw()+
      theme(axis.title = element_text(size=14,color="Black",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x="Percent Area",y=input$var1,title="",colour="Channel")
    
    p1
    
  })
  
    
    output$distPlot1 <- renderPlot({
        
        p1 <- data_arr_by_len()  %>% ggplot(aes(x=cumPercLength ))
        if(input$var1 == "Runoff..mm."){
            p1 <- p1 + geom_line(aes(y=cumRunoff.mm),size=1)
        }else
            if(input$var1 == "Lateral.Flow..mm."){
                p1 <- p1 + geom_line(aes(y=cumLateralflow.mm),size=1)
            }else
                if(input$var1 == "Baseflow..mm."){
                    p1 <- p1 + geom_line(aes(y=cumBaseflow.mm),size=1)
                }else
                    if(input$var1 == "Soil.Loss..kg.ha."){
                        p1 <- p1 + geom_line(aes(y=cumSoilLoss.kg.ha),size=1)
                    }else
                        if(input$var1 == "Sediment.Deposition..kg.ha."){
                            p1 <- p1 + geom_line(aes(y=cumSedDep.kg.ha),size=1)
                        }else
                            if(input$var1 == "Sediment.Yield..kg.ha."){
                                p1 <- p1 + geom_line(aes(y=cumSedYield.kg.ha),size=1)
                            }else
                                if(input$var1 == "Solub..React..P..kg.ha.3."){
                                    p1 <- p1 + geom_line(aes(y=cumSRP.kg.ha.3),size=1)
                                }else
                                    if(input$var1 == "Particulate.P..kg.ha.3."){
                                        p1 <- p1 + geom_line(aes(y=cumParticulateP.kg.ha.3),size=1)
                                    }else
                                        if(input$var1 == "Total.P..kg.ha.3."){
                                            p1 <- p1 + geom_line(aes(y=cumTotalP.kg.ha.3),size=1)
                                        }else
                                            if(input$var1 == "Particle.Class.1.Fraction"){
                                                p1 <- p1 + geom_line(aes(y=cumParticle.Class.1.Fraction),size=1)
                                            }else
                                                if(input$var1 == "Particle.Class.2.Fraction"){
                                                    p1 <- p1 + geom_line(aes(y=cumParticle.Class.2.Fraction),size=1)
                                                }else
                                                    if(input$var1 == "Particle.Class.3.Fraction"){
                                                        p1 <- p1 + geom_line(aes(y=cumParticle.Class.3.Fraction),size=1)
                                                    }else
                                                        if(input$var1 == "Particle.Class.4.Fraction"){
                                                            p1 <- p1 + geom_line(aes(y=cumParticle.Class.4.Fraction),size=1)
                                                        }else
                                                            if(input$var1 == "Particle.Class.5.Fraction"){
                                                                p1 <- p1 + geom_line(aes(y=cumParticle.Class.5.Fraction),size=1)
                                                            }else
                                                                if(input$var1 == "Particle.Fraction.Under.0.016.mm"){
                                                                    p1 <- p1 + geom_line(aes(y=cumParticle.Fraction.Under.0.016.mm),size=1)
                                                                }else
                                                                    if(input$var1 == "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha."){
                                                                        p1 <- p1 + geom_line(aes(y=cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha.),size=1)
                                                                    }
        
        
        p1 <- p1 +  theme_bw()+
            theme(axis.title = element_text(size=14,color="BLACK",face="bold"),
                  axis.text = element_text(size=14,color="BLACK",face="bold"))+
            labs(x="Percent Channel Length",y=input$var1,title="",colour="Channel")
        
        p1
        
    })
     
  # output$distPlot1 <- renderPlot({
  #     
  #     p1 <- data_subset()  %>% ggplot(aes(x=Length..m.))
  #     if(input$var1 == "Runoff..mm."){
  #         p1 <- p1 + geom_line(aes(y=Runoff..mm.,col="Runoff.mm"),size=0.5)
  #     }else
  #         if(input$var1 == "Subrunoff..mm."){
  #             p1 <- p1 + geom_line(aes(y=Subrunoff..mm.,col="Subrunoff..mm."),size=0.5)
  #         }else
  #             if(input$var1 == "Baseflow..mm."){
  #                 p1 <- p1 + geom_line(aes(y=Baseflow..mm.,col="Baseflow..mm."),size=0.5)
  #             }else
  #                 if(input$var1 == "Soil.Loss..kg.ha."){
  #                     p1 <- p1 + geom_line(aes(y=Soil.Loss..kg.ha.,col="Soil.Loss..kg.ha."),size=0.5)
  #                 }else
  #                     if(input$var1 == "Sediment.Deposition..kg.ha."){
  #                         p1 <- p1 + geom_line(aes(y=Sediment.Deposition..kg.ha.,col="Sediment.Deposition..kg.ha."),size=0.5)
  #                     }else
  #                         if(input$var1 == "Sediment.Yield..kg.ha."){
  #                             p1 <- p1 + geom_line(aes(y=Sediment.Yield..kg.ha.,col="Sediment.Yield..kg.ha."),size=0.5)
  #                         }else
  #                             if(input$var1 == "Solub..React..P..kg.ha."){
  #                                 p1 <- p1 + geom_line(aes(y=Solub..React..P..kg.ha.,col="Solub..React..P..kg.ha."),size=0.5)
  #                             }else
  #                                 if(input$var1 == "Particulate.P..kg.ha."){
  #                                     p1 <- p1 + geom_line(aes(y=Particulate.P..kg.ha.,col="Particulate.P..kg.ha."),size=0.5)
  #                                 }else
  #                                     if(input$var1 == "Total.P..kg.ha."){
  #                                         p1 <- p1 + geom_line(aes(y=Total.P..kg.ha.,col="Total.P..kg.ha."),size=0.5)
  #                                     }
  #     p1 <- p1 +  theme_bw()+
  #         theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
  #               axis.text = element_text(size=14,color="BLACK",face="bold"))+
  #         labs(x="Length [m]",y="",title="",colour="Channel")
  #     
  #     p1
  #     
  # })
  # output$distPlot1 <- renderPlot({
  #   d <- data  
  #   
  #   d <- ddply(d, .variables = c("Hour"),function(x){
  #     
  #     Runoff.mmavg <- mean(x$Runoff.mm,na.rm = T)
  #     BC2avg <- mean(x$BC2,na.rm = T)
  #     BC3avg <- mean(x$BC3,na.rm = T)
  #     BC4avg <- mean(x$BC4,na.rm = T)
  #     BC5avg <- mean(x$BC5,na.rm = T)
  #     BC6avg <- mean(x$BC6,na.rm = T)
  #     BC7avg <- mean(x$BC7,na.rm = T)
  #     
  #     data.frame(Runoff.mmavg,BC2avg,BC3avg,BC4avg,BC5avg,BC6avg,BC7avg)
  #   })
  #   
  #   p1 <- d %>% ggplot(aes(x=Hour))
  #   if(input$channel1 == "Runoff.mm"){
  #     p1 <- p1 + geom_line(aes(y=Runoff.mmavg,col="Runoff.mm"),size=1)
  #     p1 <- p1 + geom_point(aes(y=Runoff.mmavg))
  #   }else if(input$channel1 == "BC2"){
  #     p1 <- p1 + geom_line(aes(y=BC2avg,col="BC2"),size=1)
  #     p1 <- p1 + geom_point(aes(y=BC2avg))
  #   }else if(input$channel1 == "BC3"){
  #     p1 <- p1 + geom_line(aes(y=BC3avg,col="BC3"),size=1)
  #     p1 <- p1 + geom_point(aes(y=BC3avg))
  #   }else if(input$channel1 == "BC4"){
  #     p1 <- p1 + geom_line(aes(y=BC4avg,col="BC4"),size=1)
  #     p1 <- p1 + geom_point(aes(y=BC4avg))
  #   }else if(input$channel1 == "BC5"){
  #     p1 <- p1 + geom_line(aes(y=BC5avg,col="BC5"),size=1)
  #     p1 <- p1 + geom_point(aes(y=BC5avg))
  #   }else if(input$channel1 == "BC6"){
  #     p1 <- p1 + geom_line(aes(y=BC6avg,col="BC6"),size=1)
  #     p1 <- p1 + geom_point(aes(y=BC6avg))
  #   }else if(input$channel1 == "BC7"){
  #     p1 <- p1 + geom_line(aes(y=BC7avg,col="BC7"),size=1)
  #     p1 <- p1 + geom_point(aes(y=BC7avg))
  #   }
  #   p1 <- p1 +  theme_bw()+
  #     theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
  #           axis.text = element_text(size=14,color="BLACK",face="bold"))+
  #     labs(x="Time",y="Black Carbon (ng/m3)",title="Black Carbon Concentration in Air - Average Diurnal Variation - Dec, 2017",colour="Channel")
  #   
  #   p1
  #   
  # })
}


# Run the application 
shinyApp(ui = ui, server = server)

