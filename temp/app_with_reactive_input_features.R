library(shiny)
library(shinythemes)
library(vroom)


### Set maximum upload file size to 10MB
options(shiny.maxRequestSize = 10 * 1024^2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
    # # App title ----
    titlePanel(p("viz-WEPP", style = "color:#3474A7")),
    
    ## set the theme
    
    theme = shinytheme(theme = "sandstone"),
    
    fileInput("hillslope",label ="Browse to WEPPCloud Hillslope output file", 
              multiple = F, placeholder = "No file selected" ),
    helpText("Default max. file size is 10MB"),
    
    
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Slider for the number of bins ----
            selectInput(inputId="Watershed",label="Choose Watershed",choices = NULL,
                        selected = "Blue",multiple = F),
            
            # selectInput(inputId="Scenario",label="Choose Scenario",choices = NULL,
            #             selected = "Blue",multiple = F),
            
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
            textOutput("average")
            # uiOutput("watershedcontrols")
            # plotOutput(outputId = "distPlot"),
            # plotOutput(outputId = "distPlot1")
            # plotOutput(outputId = "distPlot2")
        )
    )
)
  


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    data <- reactive({
        file1 <- input$hillslope
        if(is.null(file1)){return()} 
        read.table(file=file1$datapath, sep=input$sep,
                   header = input$header, stringsAsFactors = input$stringAsFactors)
        
    })
    
    # data <- reactive({
    #     # req(input$hillslope)
    #     # ext <- tools::file_ext(input$hillslope$name)
    #     # switch(ext,
    #     #        csv = vroom::vroom(input$hillslope$datapath, delim = ","),
    #     #        validate("Invalid file; Please upload a .csv file")
    #     # )
    # 
    #     hillslope <- read.csv(input$hillslope, header = input$header)
    #     unique_waterhseds <- as.character(unique(hillslope$Watershed))
    #     unique_scenarios <- as.character(unique(hillslope$Scenarios))
    #     
    #     # updateSelectInput(session, "Watershed",
    #     #                   choices = as.character(unique(hillslope$Watershed)))
    #     # updateSelectInput(session, "Scenario",
    #     #                   choices = as.character(unique(hillslope$Scenario)))
    #     
    # })
    
    
  
    # output$wshedcontrols <- renderUI({
    #     req(input$hillslope)
    #     ext <- tools::file_ext(input$hillslope$name)
    #             switch(ext,
    #                    csv = vroom::vroom(input$hillslope$datapath, delim = ","),
    #                    validate("Invalid file; Please upload a .csv file")
    #             )
        # data <- read.csv(input$hillslope$datapath, header = TRUE)
        # unique_watsheds <- as.character(unique(data$Watershed))
        # unique_scenario <- as.character(unique(data$Scenario))
        # var1 <- as.character(colnames(data))
        # list(unique_watsheds)
    
         
        

        
        # output$head <- renderTable({
    #     head(data())
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)

