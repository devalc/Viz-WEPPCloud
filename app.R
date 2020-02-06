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
            tabsetPanel(
                tabPanel("Table", tableOutput("table")),
                tabPanel("Plot", plotOutput("plot1"))
            )
            # tableOutput('table'),
            # plotOutput(outputId = "Plot1")
            # plotOutput(outputId = "Plot_vs_CumLen")
            )
        
    )
    
)



server <- (
    
    
    function(input, output, session){ 
        
        output$selectfile <- renderUI({
            req(input$file)
            if(is.null(input$file)) {return()}
            list(hr(), 
                 helpText("Select the file you need from the dropdown"),
                 selectInput("scale", "Select scale (channel/hillslope/watershed)", choices=input$file$name)
            )
        
        })
        
        data <- reactive({
            req(input$file)
            file <- read.csv(file=input$file$datapath[input$file$name==input$scale],header = TRUE)
            
        })
        
        
        output$var <- renderUI({
            req(input$file)
            req(input$scale)
            if(grepl("hill", input$scale) == TRUE){
            selectInput("variable", "Select the variable of interest",  colnames(data()[6:25]),
                        selected = colnames(data()[10]) )}else
                            if(grepl("chn", input$scale) == TRUE){
            selectInput("variable", "Select the variable of interest",  colnames(data()[7:17]),
                        selected = colnames(data()[10]) )}else
                            if(grepl("out", input$scale) == TRUE){
            selectInput("variable", "Select the variable of interest",  colnames(data()[1:20])                        ,
                        selected = colnames(data()[7]) )}

        })
        
        output$wshed <- renderUI({
            req(input$file)
            selectInput("wshed", "Select the watershed",  unique(data()$Watershed))
        })

        output$scen <- renderUI({
            selectInput("scenario", "Select the scenario",  unique(data()$Scenario),
                        multiple = TRUE, selected = unique(data()$Scenario)[1])
        })
        
        
        data_subset <- reactive({
            df <- as.data.frame(data())
            df %>% dplyr::filter(Watershed == input$wshed)
        })
        
        
        # data_arr_by_var <- reactive({
        #     if(is.null(input$file)) {
        #         return()}else
        #             if(grepl("hill", input$scale) == TRUE){
        #                 data_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$variable, desc)%>%
        #                     mutate(cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
        #                            cumPercArea = cumsum(Hillslope.Area..ha.)/sum(Hillslope.Area..ha.)*100,
        #                            cumRunoff.mm = cumsum(Runoff..mm.)/sum(Runoff..mm.)*100,
        #                            cumLateralflow.mm = cumsum(Lateral.Flow..mm.)/sum(Lateral.Flow..mm.)*100,
        #                            cumBaseflow.mm = cumsum(Baseflow..mm.)/sum(Baseflow..mm.)*100
        #                     ) }else
        #                         # req(input$scale)
        #                         if(grepl("chn", input$scale) == TRUE){
        #                             return}else
        #                                 # req(input$scale)
        #                                 if(grepl("out", input$scale) == TRUE){
        #                                     return()}
        # 
        # })
        
        data_arr_by_var <- reactive({
            # req(input$file)
            # req(input$scale)
            if(is.null(input$file)) {return()}
            if(grepl("hill", input$scale) == TRUE){
            data_subset() %>% group_by(data_subset()$Scenario) %>% arrange_at(.vars = input$variable, desc)%>%
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
                ungroup()}else
                    # req(input$scale)
                    if(grepl("chn", input$scale) == TRUE){
                        data_subset() %>% group_by(Scenario) %>% arrange_at(.vars = input$variable, desc)%>%
                            mutate(cumPercLen = cumsum(Length..m.)/sum(Length..m.)*100,
                                   cumPercChanArea = cumsum(Channel.Area..ha.)/sum(Channel.Area..ha.)*100,
                                   cumPercContriChanArea = cumsum(Contributing.Channel.Area..ha.)/sum(Contributing.Channel.Area..ha.)*100,
                                   cumDischarge = cumsum(Discharge..mm.)/sum(Discharge..mm.)*100,
                                   cumDischarge = cumsum(Sediment.Yield..tonne.)/sum(Sediment.Yield..tonne.)*100,
                                   cumDischarge = cumsum(Channel.Erosion..tonne.)/sum(Channel.Erosion..tonne.)*100,
                                   cumDischarge = cumsum(Upland.Charge..mm.)/sum(Upland.Charge..mm.)*100,
                                   cumDischarge = cumsum(Lateral.Flow..mm.)/sum(Lateral.Flow..mm.)*100,
                                   cumDischarge = cumsum(Solub..React..P..kg.ha.)/sum(Solub..React..P..kg.ha.)*100,
                                   cumDischarge = cumsum(Particulate.P..kg.ha.)/sum(Particulate.P..kg.ha.)*100,
                                   cumDischarge = cumsum(Total.P..kg.ha.)/sum(Total.P..kg.ha.)*100
                            ) %>%
                            ungroup()}else
                                # req(input$scale)
                                if(grepl("out", input$scale) == TRUE){
                                    data_subset()}

        })

        
        # output head of the dataframe to check if these cals works #delete later
        
        output$table <- renderTable(
            data_arr_by_var() )

        # output$Plot1 <- renderPlot(
        #     
        #     if(grepl("hill", input$scale) == TRUE){
        #         df <- as.data.frame(data_arr_by_var)
        #         p1 <- df %>% ggplot(aes(x= df$cumPercLen ))+
        #             geom_line(aes(y=df$input$variable),size=1) +  
        #         theme_bw()+
        #         theme(axis.title = element_text(size=14,color="BLACK",face="bold"),
        #               axis.text = element_text(size=14,color="BLACK",face="bold"),
        #               legend.title = element_text(size=14,color="BLACK",face="bold"),
        #               legend.text = element_text(size=14,color="BLACK"))+
        #         labs(x="Percent Channel Length",y=df$input$variable,title="",colour="Scenario")
        # 
        #     p1
        #         }
        #     )

})
        
# Run the application 
shinyApp(ui = ui, server = server)

