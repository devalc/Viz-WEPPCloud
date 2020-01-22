# @ Chinmay Deval
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)


# Data Preparation Steps

data <- read.csv("D:/GitHub/Process-WEPPCloud-Outputs/test_data/lt4_hill_summary.csv")
data<-data[data$Watershed == "Watershed_12_Meeks", ]
data<-data %>% mutate(PercentArea= (Hillslope.Area..ha./sum(Hillslope.Area..ha.))*100)
# View(data)


ui <- fluidPage(
  
  # App title ----
  titlePanel("viz-WEPP-Results"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      # selectInput(inputId="color1",label="Choose Color",choices = c("Red"="Red","Blue"="Blue","Green"="Green"),
                  # selected = "Blue",multiple = F),
      
      # radioButtons(inputId = "border1",label = "Select Border",choices = c("Black"="#000000","White"="#ffffff")),
      
      selectInput(inputId="var1",label="Choose Variable",choices = c("Runoff..mm."="Runoff..mm.",
                                                                        "Subrunoff..mm."="Subrunoff..mm.",
                                                                        "Baseflow..mm."="Baseflow..mm.",
                                                                        "Soil.Loss..kg.ha."="Soil.Loss..kg.ha.",
                                                                        "Sediment.Deposition..kg.ha."="Sediment.Deposition..kg.ha.",
                                                                        "Sediment.Yield..kg.ha."="Sediment.Yield..kg.ha.",
                                                                        "Solub..React..P..kg.ha."="Solub..React..P..kg.ha.",
                                                                     "Particulate.P..kg.ha."="Particulate.P..kg.ha.",
                                                                     "Total.P..kg.ha."="Total.P..kg.ha."),
                  selected = "Runoff..mm.",multiple = F)
      # ,
      
      # sliderInput(inputId = "bins1xz",
      #             label = "Number of bins:",
      #             min = 1,
      #             max = 50,
      #             value = 30),
      
      # sliderInput(inputId = "range1",
      #             label = "Data Range",
      #             min = 1,
      #             max = 31,
      #             value = c(1,31))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      # plotOutput(outputId = "distPlot1")
      # plotOutput(outputId = "distPlot2")
    )
  )
)




# Define server logic required to draw a histogram ----
server <- function(input, output){
  
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  output$distPlot <- renderPlot({
    
    p1 <- data  %>% ggplot(aes(x=PercentArea))
    if(input$var1 == "Runoff..mm."){
      p1 <- p1 + geom_line(aes(y=Runoff..mm.,col="Runoff.mm"),size=0.5)
    }else
      if(input$var1 == "Subrunoff..mm."){
        p1 <- p1 + geom_line(aes(y=Subrunoff..mm.,col="Subrunoff..mm."),size=0.5)
      }else
        if(input$var1 == "Baseflow..mm."){
          p1 <- p1 + geom_line(aes(y=Baseflow..mm.,col="Baseflow..mm."),size=0.5)
        }else
          if(input$var1 == "Soil.Loss..kg.ha."){
            p1 <- p1 + geom_line(aes(y=Soil.Loss..kg.ha.,col="Soil.Loss..kg.ha."),size=0.5)
          }else
            if(input$var1 == "Sediment.Deposition..kg.ha."){
              p1 <- p1 + geom_line(aes(y=Sediment.Deposition..kg.ha.,col="Sediment.Deposition..kg.ha."),size=0.5)
            }else
              if(input$var1 == "Sediment.Yield..kg.ha."){
                p1 <- p1 + geom_line(aes(y=Sediment.Yield..kg.ha.,col="Sediment.Yield..kg.ha."),size=0.5)
              }else
                if(input$var1 == "Solub..React..P..kg.ha."){
                  p1 <- p1 + geom_line(aes(y=Solub..React..P..kg.ha.,col="Solub..React..P..kg.ha."),size=0.5)
                }else
                  if(input$var1 == "Particulate.P..kg.ha."){
                    p1 <- p1 + geom_line(aes(y=Particulate.P..kg.ha.,col="Particulate.P..kg.ha."),size=0.5)
                  }else
                    if(input$var1 == "Total.P..kg.ha."){
                      p1 <- p1 + geom_line(aes(y=Total.P..kg.ha.,col="Total.P..kg.ha."),size=0.5)
                    }
    p1 <- p1 +  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x="Percent Area",y="",title="",colour="Channel")
    
    p1
    
  })
  
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

