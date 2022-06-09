library(shiny)
library(dplyr)
library(ggplot2)

inc_plot_df <- read.csv("/Users/mahmood/UBCMDS/591_capstone/plot_inci_df.csv")

ui <- fluidPage(
  titlePanel(h2("Blood Test Result System",align = "center")),
  sidebarLayout(    
    sidebarPanel(
      selectInput(inputId = "DISEASE",
                  label = "Choose DISEASE",
                  choices = sort(unique(inc_plot_df$DISEASE)),
                  selected = "ASTHMA"),
      selectInput(inputId = "HEALTH_BOUNDARIES",
                  label = "Choose HEALTH_BOUNDARIES",
                  choices = sort(unique(inc_plot_df$HEALTH_BOUNDARIES)),
                  selected = "1130 Kimberley")),
    mainPanel(
      plotOutput("ts_plot"))))

server <- shinyServer(
  
  function(input,output){
    
    datasetInput <- reactive({
      inc_plot_df %>% 
        filter(DISEASE == input$DISEASE) %>%
        filter(HEALTH_BOUNDARIES == input$HEALTH_BOUNDARIES)
    })
    
    # plot time series
    output$ts_plot <- renderPlot({
      dataset <- datasetInput()
      ggplot(dataset,
             aes(x = YEAR,
                 y = pred)) +
        geom_point() +
        geom_line(size = 1.5, color = "black") +
        labs(x = "Time in Years",
             y = "Predicted Age Standardized Incidence Rate",
             title = "Predicted Age Standardized Incidence Rate") +
        theme_minimal(base_size = 14, base_family = "Georgia")
    })
  })

shiny::shinyApp(ui = ui, server = server)