# @ Chinmay Deval
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)


# Data Preparation Steps

data <- read.csv("C:/Chinmay/Github/Process-WEPPCloud-Outputs/data/lt2020_2_hill_summary.csv")
unique_watsheds <- as.character(unique(data$Watershed))
unique_scenario <- as.character(unique(data$Scenario))


ui <- fluidPage(
    
    # # App title ----
    titlePanel(p("viz-WEPP", style = "color:#3474A7")),
    
    ## set the theme
    
    theme = shinytheme(theme = "sandstone"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Slider for the number of bins ----
            selectInput(inputId="Watershed",label="Choose Watershed",choices = unique_watsheds,
                        selected = "Blue",multiple = F),
            
            # selectInput(inputId="Scenario",label="Choose Scenario",choices = unique_scenario,
            #             selected = "Blue",multiple = F),
            
            # radioButtons(inputId = "border1",label = "Select Border",choices = c("Black"="#000000","White"="#ffffff")),
            
            selectInput(inputId="var1",label="Choose Variable",choices = c("Runoff"="Runoff..mm.",
                                                                           "Lateral Flow"="Lateral.Flow..mm.",
                                                                           "Baseflow "="Baseflow..mm.",
                                                                           "Soil Loss"="Soil.Loss..kg.ha.",
                                                                           "Sediment Deposition ."="Sediment.Deposition..kg.ha.",
                                                                           "Sediment Yield "="Sediment.Yield..kg.ha.",
                                                                           "Soluble Reactive Phosphorus"="Solub..React..P..kg.ha.3.",
                                                                           "Particulate Phosphorus "="Particulate.P..kg.ha.3.",
                                                                           "Total Phosphorus"="Total.P..kg.ha.3.",
                                                                           "Particle Class 1 Fraction" = "Particle.Class.1.Fraction",
                                                                           "Particle Class 2 Fraction" = "Particle.Class.2.Fraction",
                                                                           "Particle Class 3 Fraction" = "Particle.Class.3.Fraction",
                                                                           "Particle Class 4 Fraction" = "Particle.Class.4.Fraction",
                                                                           "Particle Class 5 Fraction" = "Particle.Class.5.Fraction",
                                                                           "Particle Fraction Under 0.016 mm" = "Particle.Fraction.Under.0.016.mm",
                                                                           "Sediment Yield of Particles Under 0.016 mm" = "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha."),
                        selected = character(0), multiple = F)
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Histogram ----
            plotOutput(outputId = "Plot_vs_CumArea"),
            plotOutput(outputId = "Plot_vs_CumLen")
            # plotOutput(outputId = "distPlot2")
        )
    )
)




# Define server logic required to draw a histogram ----
server <- function(input, output){
    

    data_subset <- reactive({
        filter(data, Watershed %in% input$Watershed) 
    })
    
    data_arr_by_var <- reactive({
        if(input$var1 == "Runoff..mm."){
            data_subset() %>% group_by(Scenario) %>% arrange(desc(Runoff..mm.)) %>%
                mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                       cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
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
                ungroup()}else
                if(input$var1 == "Lateral.Flow..mm."){
                    data_subset() %>% group_by(Scenario) %>% arrange(desc(Lateral.Flow..mm.)) %>%
                        mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                               cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
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
                        )%>% 
                        ungroup()}else
                            if(input$var1 == "Baseflow..mm."){
                                data_subset() %>% group_by(Scenario) %>% arrange(desc(Baseflow..mm.)) %>%
                                    mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                                           cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
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
                                    )%>% 
                                    ungroup()}else
                                        if(input$var1 == "Soil.Loss..kg.ha."){
                                            data_subset() %>% group_by(Scenario) %>% arrange(desc(Soil.Loss..kg.ha.)) %>%
                                                mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                                                       cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
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
                                                )%>% 
                                                ungroup()}else
                                                    if(input$var1 == "Sediment.Deposition..kg.ha."){
                                                        data_subset() %>% group_by(Scenario) %>% arrange(desc(Sediment.Deposition..kg.ha.)) %>%
                                                            mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                                                                   cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
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
                                                            )%>% 
                                                            ungroup()}else
                                                                if(input$var1 == "Sediment.Yield..kg.ha."){
                                                                    data_subset() %>% group_by(Scenario) %>% arrange(desc(Sediment.Yield..kg.ha.)) %>%
                                                                        mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                                                                               cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
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
                                                                        )%>% 
                                                                        ungroup()}else
                                                                            if(input$var1 == "Solub..React..P..kg.ha.3."){
                                                                                data_subset() %>% group_by(Scenario) %>% arrange(desc(Solub..React..P..kg.ha.3.)) %>%
                                                                                    mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                                                                                           cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
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
                                                                                    )%>% 
                                                                                    ungroup()}else
                                                                                        if(input$var1 == "Particulate.P..kg.ha.3."){
                                                                                            data_subset() %>% group_by(Scenario) %>% arrange(desc(Particulate.P..kg.ha.3.)) %>%
                                                                                                mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                                                                                                       cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
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
                                                                                                )%>% 
                                                                                                ungroup()}else
                                                                                                    if(input$var1 == "Total.P..kg.ha.3."){
                                                                                                        data_subset() %>% group_by(Scenario) %>% arrange(desc(Total.P..kg.ha.3.)) %>%
                                                                                                            mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                                                                                                                   cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
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
                                                                                                            )%>% 
                                                                                                            ungroup()}else
                                                                                                                if(input$var1 == "Particle.Class.1.Fraction"){
                                                                                                                    data_subset() %>% group_by(Scenario) %>% arrange(desc(Particle.Class.1.Fraction)) %>%
                                                                                                                        mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                                                                                                                               cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
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
                                                                                                                        )%>% 
                                                                                                                        ungroup()}else
                                                                                                                            if(input$var1 == "Particle.Class.2.Fraction"){
                                                                                                                                data_subset() %>% group_by(Scenario) %>% arrange(desc(Particle.Class.2.Fraction)) %>%
                                                                                                                                    mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                                                                                                                                           cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
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
                                                                                                                                    )%>% 
                                                                                                                                    ungroup()}else
                                                                                                                                        if(input$var1 == "Particle.Class.3.Fraction"){
                                                                                                                                            data_subset() %>% group_by(Scenario) %>% arrange(desc(Particle.Class.3.Fraction)) %>%
                                                                                                                                                mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                                                                                                                                                       cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
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
                                                                                                                                                )%>% 
                                                                                                                                                ungroup()}else
                                                                                                                                                    if(input$var1 == "Particle.Class.4.Fraction"){
                                                                                                                                                        data_subset() %>% group_by(Scenario) %>% arrange(desc(Particle.Class.4.Fraction)) %>%
                                                                                                                                                            mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                                                                                                                                                                   cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
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
                                                                                                                                                            )%>% 
                                                                                                                                                            ungroup()}else
                                                                                                                                                                if(input$var1 == "Particle.Class.5.Fraction"){
                                                                                                                                                                    data_subset() %>% group_by(Scenario) %>% arrange(desc(Particle.Class.5.Fraction)) %>%
                                                                                                                                                                        mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                                                                                                                                                                               cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
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
                                                                                                                                                                        )%>% 
                                                                                                                                                                        ungroup()}else
                                                                                                                                                                            if(input$var1 == "Particle.Fraction.Under.0.016.mm"){
                                                                                                                                                                                data_subset() %>% group_by(Scenario) %>% arrange(desc(Particle.Fraction.Under.0.016.mm)) %>%
                                                                                                                                                                                    mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                                                                                                                                                                                           cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
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
                                                                                                                                                                                    )%>% 
                                                                                                                                                                                    ungroup()}else
                                                                                                                                                                                        if(input$var1 == "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha."){
                                                                                                                                                                                            data_subset() %>% group_by(Scenario) %>% arrange(desc(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.)) %>%
                                                                                                                                                                                                mutate(cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
                                                                                                                                                                                                       cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
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
                                                                                                                                                                                                )%>% 
                                                                                                                                                                                                ungroup()}
    })
    
    
    output$Plot_vs_CumArea <- renderPlot({
        
        p1 <- data_arr_by_var()  %>% ggplot(aes(x=cumPercArea))
        if(input$var1 == "Runoff..mm."){
            p1 <- p1 + geom_line(aes(y=cumRunoff.mm , color= Scenario),size=1)
        }else
            if(input$var1 == "Lateral.Flow..mm."){
                p1 <- p1 + geom_line(aes(y=cumLateralflow.mm, color= Scenario),size=1)
            }else
                if(input$var1 == "Baseflow..mm."){
                    p1 <- p1 + geom_line(aes(y=cumBaseflow.mm, color= Scenario),size=1)
                }else
                    if(input$var1 == "Soil.Loss..kg.ha."){
                        p1 <- p1 + geom_line(aes(y=cumSoilLoss.kg.ha, color= Scenario),size=1)
                    }else
                        if(input$var1 == "Sediment.Deposition..kg.ha."){
                            p1 <- p1 + geom_line(aes(y=cumSedDep.kg.ha, color= Scenario),size=1)
                        }else
                            if(input$var1 == "Sediment.Yield..kg.ha."){
                                p1 <- p1 + geom_line(aes(y=cumSedYield.kg.ha, color= Scenario),size=1)
                            }else
                                if(input$var1 == "Solub..React..P..kg.ha.3."){
                                    p1 <- p1 + geom_line(aes(y=cumSRP.kg.ha.3, color= Scenario),size=1)
                                }else
                                    if(input$var1 == "Particulate.P..kg.ha.3."){
                                        p1 <- p1 + geom_line(aes(y=cumParticulateP.kg.ha.3, color= Scenario),size=1)
                                    }else
                                        if(input$var1 == "Total.P..kg.ha.3."){
                                            p1 <- p1 + geom_line(aes(y=cumTotalP.kg.ha.3, color= Scenario),size=1)
                                        }else
                                            if(input$var1 == "Particle.Class.1.Fraction"){
                                                p1 <- p1 + geom_line(aes(y=cumParticle.Class.1.Fraction, color= Scenario),size=1)
                                            }else
                                                if(input$var1 == "Particle.Class.2.Fraction"){
                                                    p1 <- p1 + geom_line(aes(y=cumParticle.Class.2.Fraction, color= Scenario),size=1)
                                                }else
                                                    if(input$var1 == "Particle.Class.3.Fraction"){
                                                        p1 <- p1 + geom_line(aes(y=cumParticle.Class.3.Fraction, color= Scenario),size=1)
                                                    }else
                                                        if(input$var1 == "Particle.Class.4.Fraction"){
                                                            p1 <- p1 + geom_line(aes(y=cumParticle.Class.4.Fraction, color= Scenario),size=1)
                                                        }else
                                                            if(input$var1 == "Particle.Class.5.Fraction"){
                                                                p1 <- p1 + geom_line(aes(y=cumParticle.Class.5.Fraction, color= Scenario),size=1)
                                                            }else
                                                                if(input$var1 == "Particle.Fraction.Under.0.016.mm"){
                                                                    p1 <- p1 + geom_line(aes(y=cumParticle.Fraction.Under.0.016.mm, color= Scenario),size=1)
                                                                }else
                                                                    if(input$var1 == "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha."){
                                                                        p1 <- p1 + geom_line(aes(y=cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. , color= Scenario),size=1)
                                                                    }
        
        
        p1 <- p1 +  theme_bw()+
            theme(axis.title = element_text(size=14,color="Black",face="bold"),
                  axis.text = element_text(size=14,color="BLACK",face="bold"))+
            labs(x="Percent Area",y=input$var1,title="",colour="Channel")
        
        p1
        
    })
    
    
    output$Plot_vs_CumLen <- renderPlot({
        
        p1 <- data_arr_by_var()  %>% ggplot(aes(x=cumPercLen ))
        if(input$var1 == "Runoff..mm."){
            p1 <- p1 + geom_line(aes(y=cumRunoff.mm, color= Scenario),size=1)
        }else
            if(input$var1 == "Lateral.Flow..mm."){
                p1 <- p1 + geom_line(aes(y=cumLateralflow.mm, color= Scenario),size=1)
            }else
                if(input$var1 == "Baseflow..mm."){
                    p1 <- p1 + geom_line(aes(y=cumBaseflow.mm, color= Scenario),size=1)
                }else
                    if(input$var1 == "Soil.Loss..kg.ha."){
                        p1 <- p1 + geom_line(aes(y=cumSoilLoss.kg.ha, color= Scenario),size=1)
                    }else
                        if(input$var1 == "Sediment.Deposition..kg.ha."){
                            p1 <- p1 + geom_line(aes(y=cumSedDep.kg.ha, color= Scenario),size=1)
                        }else
                            if(input$var1 == "Sediment.Yield..kg.ha."){
                                p1 <- p1 + geom_line(aes(y=cumSedYield.kg.ha, color= Scenario),size=1)
                            }else
                                if(input$var1 == "Solub..React..P..kg.ha.3."){
                                    p1 <- p1 + geom_line(aes(y=cumSRP.kg.ha.3, color= Scenario),size=1)
                                }else
                                    if(input$var1 == "Particulate.P..kg.ha.3."){
                                        p1 <- p1 + geom_line(aes(y=cumParticulateP.kg.ha.3, color= Scenario),size=1)
                                    }else
                                        if(input$var1 == "Total.P..kg.ha.3."){
                                            p1 <- p1 + geom_line(aes(y=cumTotalP.kg.ha.3, color= Scenario),size=1)
                                        }else
                                            if(input$var1 == "Particle.Class.1.Fraction"){
                                                p1 <- p1 + geom_line(aes(y=cumParticle.Class.1.Fraction, color= Scenario),size=1)
                                            }else
                                                if(input$var1 == "Particle.Class.2.Fraction"){
                                                    p1 <- p1 + geom_line(aes(y=cumParticle.Class.2.Fraction, color= Scenario),size=1)
                                                }else
                                                    if(input$var1 == "Particle.Class.3.Fraction"){
                                                        p1 <- p1 + geom_line(aes(y=cumParticle.Class.3.Fraction, color= Scenario),size=1)
                                                    }else
                                                        if(input$var1 == "Particle.Class.4.Fraction"){
                                                            p1 <- p1 + geom_line(aes(y=cumParticle.Class.4.Fraction, color= Scenario),size=1)
                                                        }else
                                                            if(input$var1 == "Particle.Class.5.Fraction"){
                                                                p1 <- p1 + geom_line(aes(y=cumParticle.Class.5.Fraction, color= Scenario),size=1)
                                                            }else
                                                                if(input$var1 == "Particle.Fraction.Under.0.016.mm"){
                                                                    p1 <- p1 + geom_line(aes(y=cumParticle.Fraction.Under.0.016.mm, color= Scenario),size=1)
                                                                }else
                                                                    if(input$var1 == "Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha."){
                                                                        p1 <- p1 + geom_line(aes(y=cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha., color= Scenario),size=1)
                                                                    }
        
        
        p1 <- p1 +  theme_bw()+
            theme(axis.title = element_text(size=14,color="BLACK",face="bold"),
                  axis.text = element_text(size=14,color="BLACK",face="bold"))+
            labs(x="Percent Channel Length",y=input$var1,title="",colour="Channel")
        
        p1
        
    })
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)