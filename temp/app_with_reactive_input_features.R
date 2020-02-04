library(shinythemes)
library(shiny)
library(tidyverse)

options(shiny.maxRequestSize = 32*1024^2)


ui <- fluidPage(
    
    ## set the theme
    
    theme = shinytheme(theme = "sandstone"),
    
    titlePanel("viz-WEPP"),
    sidebarLayout(
        sidebarPanel(
            
            # uploading the file 
            # fileinput() function is used to get the file upload contorl option
            fileInput("file",label ="Uplaod WEPPCloud Outputs (*.csv files)", 
                      multiple = TRUE, placeholder = "No file selected", accept = ".csv" ),
            helpText("max. file size is 32MB"),
            
            # # Input: Select separator ----
            # radioButtons("scale", "Spatial scale",
            #              choices = c(Hillslope = "Hillslope",
            #                          Channel = "Channel",
            #                          Catchment = "Catchment"),
            #              selected = "Hillslope"),
            
            
            
            uiOutput("selectfile"),
            uiOutput("wshed"),
            uiOutput("var"),
            uiOutput("scen")
            
            
        ),
        
        mainPanel( 
            tableOutput('table')
            # plotOutput(outputId = "Plot_vs_CumArea")
            # plotOutput(outputId = "Plot_vs_CumLen")
            )
        
    )
    
)



server <- (
    
    
    function(input, output, session){ 
        
        output$selectfile <- renderUI({
            if(is.null(input$file)) {return()}
            list(hr(), 
                 helpText("Select the file you need from the dropdown"),
                 selectInput("scale", "Select", choices=input$file$name)
            )
        
        })
        
        data <- reactive({
            if(is.null(input$file)){return()}
            
            ### not sure how to give a default selection here

            file <- read.csv(file=input$file$datapath[input$file$name==input$scale],header = TRUE,
                                               stringsAsFactors = FALSE)
            # ext <- tools::file_ext(input$file$name)
            # switch(ext,
            #        csv = vroom::vroom(input$file$datapath, delim = ","),
            #        validate("Invalid file; Please upload a .csv file")
            # )

            # if(input$scale == "Hillslope"){
            # 
            # file1 <- read.csv(file=input$file[[1,"datapath"]],header = TRUE,
            #                   stringsAsFactors = FALSE)}else
            #                       if(input$scale == "Channel"){
            #                           file2 <- read.csv(file=input$file[[2,"datapath"]],header = TRUE)}else
            #                               if(input$scale == "Catchment"){
            #                                   file3 <- read.csv(file=input$file[[3,"datapath"]],header = TRUE)}
        })
        
        
        
        output$var <- renderUI({
            if(is.null(input$file)) {return()}
            if(grepl("hill", input$scale) == TRUE){
            selectInput("variable", "Select the variable of interest",  colnames(data()[1:25]),
                        selected = colnames(data()[10]) )}else
                            if(grepl("chn", input$scale) == TRUE){
            selectInput("variable", "Select the variable of interest",  colnames(data()[1:17]),
                        selected = colnames(data()[1]) )}else
                            if(grepl("out", input$scale) == TRUE){
            selectInput("variable", "Select the variable of interest",  colnames(data()[1:20])                        ,
                        selected = colnames(data()[1]) )}
            
        })
        
        output$wshed <- renderUI({
            if(is.null(input$file)) {return()}

            selectInput("wshed", "Select the watershed",  unique(data()$Watershed))
        })

        output$scen <- renderUI({
            if(is.null(input$file)) {return()}

            selectInput("scenario", "Select the scenario",  unique(data()$Scenario), 
                        multiple = TRUE, selected = unique(data()$Scenario)[1])
        })
        
        
        data_subset <- reactive({
            df <- as.data.frame(data())
            df %>% filter(Watershed == input$wshed) 
        })
        
        
        
        data_arr_by_var <- reactive({ data_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$variable, desc)%>%
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
        
        
        # output head of the dataframe to check if these cals works #delete later
        
        output$table <- renderTable(
            data_arr_by_var() %>% tail(20))
        
          
        
    })


# Run the application 
shinyApp(ui = ui, server = server)

