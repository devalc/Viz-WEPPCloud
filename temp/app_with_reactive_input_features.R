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
            fileInput("file",label ="Uplaod WEPPCloud Hillslope *.csv file", 
                      multiple = F, placeholder = "No file selected" ),
            helpText("max. file size is 32MB"),
            
            # fileInput("file","Upload *.csv file"), 
            
            
            uiOutput("wshed"),
            uiOutput("var"),
            uiOutput("scen")
            
            
        ),
        
        mainPanel( 
            tableOutput('table')
            # plotOutput(outputId = "Plot_vs_CumArea"),
            # plotOutput(outputId = "Plot_vs_CumLen")
            )
        
    )
    
)



server <- (
    
    
    function(input, output, session){ 
        
        data <- reactive({
            file1 <- input$file
            if(is.null(file1)){return()} 
            read.table(file=file1$datapath,head=TRUE,sep=",")
            
        })
        
        output$var <- renderUI({
            
            selectInput("variable", "Select the variable of interest",  colnames(data()[1:25]),
                        selected = colnames(data()[10]) )
            
        })
        
        output$wshed <- renderUI({

            selectInput("wshed", "Select the watershed",  unique(data()$Watershed))
        })

        output$scen <- renderUI({

            selectInput("scenario", "Select the scenario",  unique(data()$Scenario), 
                        multiple = TRUE, selected = unique(data()$Scenario)[1])
        })
        
        
        df_of_data <- reactive({
            df <- as.data.frame(data())
            df
        })
        
        data_subset <- reactive({
            
            df_of_data() %>% filter(Watershed == input$wshed) 
        })
        
        
        # data_arr_by_var <- reactive({
        #     data_subset() %>% group_by(Scenario) %>% arrange(.data[[(input$var)]])%>%
        #         mutate(cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
        #                cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
        #                cumRunoff.mm = cumsum(Runoff..mm.)/sum(Runoff..mm.)*100
        #                # cumLateralflow.mm = cumsum(Lateral.Flow..mm.)/sum(Lateral.Flow..mm.)*100,
        #                # cumBaseflow.mm = cumsum(Baseflow..mm.)/sum(Baseflow..mm.)*100,
        #                # cumSoilLoss.kg.ha = cumsum(Soil.Loss..kg.ha.)/sum(Soil.Loss..kg.ha.)*100,
        #                # cumSedDep.kg.ha = cumsum(Sediment.Deposition..kg.ha.)/sum(Sediment.Deposition..kg.ha.)*100,
        #                # cumSedYield.kg.ha = cumsum(Sediment.Yield..kg.ha.)/sum(Sediment.Yield..kg.ha.)*100,
        #                # cumSRP.kg.ha.3 = cumsum(Solub..React..P..kg.ha.3.)/sum(Solub..React..P..kg.ha.3.)*100,
        #                # cumParticulateP.kg.ha.3 = cumsum(Particulate.P..kg.ha.3.)/sum(Particulate.P..kg.ha.3.)*100,
        #                # cumTotalP.kg.ha.3 = cumsum(Total.P..kg.ha.3.)/sum(Total.P..kg.ha.3.)*100,
        #                # cumParticle.Class.1.Fraction = cumsum(Particle.Class.1.Fraction)/sum(Particle.Class.1.Fraction)*100,
        #                # cumParticle.Class.2.Fraction = cumsum(Particle.Class.2.Fraction)/sum(Particle.Class.2.Fraction)*100,
        #                # cumParticle.Class.3.Fraction = cumsum(Particle.Class.3.Fraction)/sum(Particle.Class.3.Fraction)*100,
        #                # cumParticle.Class.4.Fraction = cumsum(Particle.Class.4.Fraction)/sum(Particle.Class.4.Fraction)*100,
        #                # cumParticle.Class.5.Fraction = cumsum(Particle.Class.5.Fraction)/sum(Particle.Class.5.Fraction)*100,
        #                # cumParticle.Fraction.Under.0.016.mm = cumsum(Particle.Fraction.Under.0.016.mm)/sum(Particle.Fraction.Under.0.016.mm)*100,
        #                # cumSediment.Yield.of.Particles.Under.0.016.mm..kg.ha. = cumsum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.)/sum(Sediment.Yield.of.Particles.Under.0.016.mm..kg.ha.)*100
        #         ) %>%
        #         ungroup()})
        
        # output head of the dataframe to check if this step works
        output$table <- renderTable(
            data_subset() %>% dplyr::arrange(!!!(input$variable)) %>%
            head(50))
        
        
        # output$Plot_vs_CumLen <- renderPlot({
        #     req(input$variable)
        #     ggplot(data = data_subset() , mapping = aes(data_subset()$Hillslope.Area..ha. , data_subset()$Runoff..mm.))+
        #         geom_line()
        # })
                
        
    })


# Run the application 
shinyApp(ui = ui, server = server)

