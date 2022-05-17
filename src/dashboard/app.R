# Load packages ----
library(shiny)
library(tidyverse)
library(leaflet)
library(rgdal)

# Load data ----



# Source helper functions -----
source('global.R', local = T)


# User interface ----

ui <-fluidPage(
  
  includeCSS("www/mytheme.css"),
  
  navbarPage("BC Chronic Disease Dashboard",
      tabPanel("Information", 
               h2("Welcome to the BC Chronic Disease Dashboard"),
               h4("Developed by Jessie Wong, Jennifer Hoang, Mahmoodur Rahman, and Irene Yan"),
               helpText(HTML("info about dashboard <br/>Instructions and descriptions of tabs, <br/>rate definitions,
                        note about fiscal year  "))),
      tabPanel("By Disease",
               sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h2("Filters"),
                 hr(style = "border-top: 1px solid #000000"),
                 
                 selectInput("dataset_d", 
                             label = "Select Rate Type",
                             choices = c("Crude Incidence Rate",
                                         "Age Standardized Incidence Rate",
                                         "Crude Life Prevalence",
                                         "Age Standardized Life Prevalence",
                                         "Crude HSC Prevalence",
                                         "Age Standardized HSC Prevalence")),
                 
                 uiOutput("disease_d"),
                 
                 radioButtons("health_bound",
                              label= "Select Geography",
                              choices = c("Health Authorities","Community Health Service Areas"),
                              selected="Health Authorities"),
                 
                 uiOutput("region_d"),
                 
                 # selectInput("region_d", 
                 #             label = "Select Community Health Service Area(s)",
                 #             choices = append("All",unique(inc_rate_df$HEALTH_BOUND_NAME)),
                 #             multiple = TRUE,
                 #             selected = "All"),
                 
                 selectInput("year_d", 
                             label = "Select Year",
                             choices = c(seq(2001,2020))
                             ),

                 radioButtons("gender_d", 
                              label = ("Select Gender"),
                              choices = c("Male","Female","Both"), 
                              selected = "Both",
                              inline=TRUE),
                 
               ),
               mainPanel(
                 width = 9,
                 # fluidRow(
                 #   column (12 , div(verbatimTextOutput("summary")))
                 # ),
                 fluidRow(
                   column(6, leafletOutput("map",height = 700)),
                   column(6, 
                          fluidRow(column(12,plotOutput("disease_graph1",height=350))),
                          fluidRow(column(12,plotOutput("disease_graph2",height=350))),
                          ))
                 )
               )),
      
      tabPanel("By Region",
               sidebarLayout(
                 sidebarPanel(
                   h2("Filters"),
                   hr(style = "border-top: 1px solid #000000"),
                   
                   selectInput("dataset_r", 
                               label = "Select Rate Type",
                               choices = c("Crude Incidence Rate",
                                           "Age Standardized Incidence Rate",
                                           "Crude Life Prevalence",
                                           "Age Standardized Life Prevalence",
                                           "Crude HSC Prevalence",
                                           "Age Standardized HSC Prevalence")),
                   
                   uiOutput("disease_r"),
                   
                   selectInput("region_r", 
                               label = "Select Community Health Service Area(s)",
                               choices = append("All",unique(inc_rate_df$HEALTH_BOUND_NAME)),
                               multiple = FALSE,
                               selected = "All"),
                   
                   sliderInput("year_range_r", 
                               label = "Select Year Range",
                               min = 2001, max = 2020, value = c(2001, 2020)),
                   
                   radioButtons("gender_r", 
                                label = ("Select Gender"),
                                choices = c("Male","Female","Both"), 
                                selected = "Both"),
                 ),
                 mainPanel(
                   plotOutput("graph2"))
               )),

      tabPanel("Data",
               sidebarLayout(
                 sidebarPanel(
                   h2("Filters"),
                   hr(style = "border-top: 1px solid #000000"),
                   
                   selectInput("dataset_data", 
                               label = "Select Rate Type",
                               choices = c("Crude Incidence Rate",
                                           "Age Standardized Incidence Rate",
                                           "Crude Life Prevalence",
                                           "Age Standardized Life Prevalence",
                                           "Crude HSC Prevalence",
                                           "Age Standardized HSC Prevalence")),
                   
                   uiOutput("disease_data"),
                   
                   selectInput("region_data", 
                               label = "Select Community Health Service Area(s)",
                               choices = append("All",unique(inc_rate_df$HEALTH_BOUND_NAME)),
                               multiple = TRUE,
                               selected = "All"),
                   
                   sliderInput("year_range_data", 
                               label = "Select Year Range",
                               min = 2001, max = 2020, value = c(2001, 2020)),
                   
                   radioButtons("gender_data", 
                                label = ("Select Gender"),
                                choices = c("Male","Female","Both"), 
                                selected = "Both"),
                 ),
                 mainPanel(
                   downloadButton("download_data", label = "Download Data"),
                   dataTableOutput("data_table"))
               ))
               
  )
)

# Server logic ----
server <- function(input, output) {
  
  datasetInput_d <- reactive({
    switch(input$dataset_d,
           "Crude Incidence Rate" = inc_rate_df,
           "Age Standardized Incidence Rate" = inc_rate_df,
           "Crude Life Prevalence" = life_prev_df,
           "Age Standardized Life Prevalence" = life_prev_df,
           "Crude HSC Prevalence" = hsc_prev_df,
           "Age Standardized HSC Prevalence" = hsc_prev_df)
  })
  
  datasetInput_r <- reactive({
    switch(input$dataset_r,
           "Crude Incidence Rate" = inc_rate_df,
           "Age Standardized Incidence Rate" = inc_rate_df,
           "Crude Life Prevalence" = life_prev_df,
           "Age Standardized Life Prevalence" = life_prev_df,
           "Crude HSC Prevalence" = hsc_prev_df,
           "Age Standardized HSC Prevalence" = hsc_prev_df)
  })
  
  datasetInput_data <- reactive({
    switch(input$dataset_data,
           "Crude Incidence Rate" = inc_rate_df,
           "Age Standardized Incidence Rate" = inc_rate_df,
           "Crude Life Prevalence" = life_prev_df,
           "Age Standardized Life Prevalence" = life_prev_df,
           "Crude HSC Prevalence" = hsc_prev_df,
           "Age Standardized HSC Prevalence" = hsc_prev_df)
  })
  
  rateInput_d <- reactive({
    switch(input$dataset_d,
           "Crude Incidence Rate" = "CRUDE_RATE_PER_1000",
           "Age Standardized Incidence Rate" = "STD_RATE_PER_1000",
           "Crude Life Prevalence" = "CRUDE_RATE_PER_1000",
           "Age Standardized Life Prevalence" = "STD_RATE_PER_1000",
           "Crude HSC Prevalence" = "CRUDE_RATE_PER_1000",
           "Age Standardized HSC Prevalence" = "STD_RATE_PER_1000")
  })
  
  rateInput_r <- reactive({
    switch(input$dataset_r,
           "Crude Incidence Rate" = "CRUDE_RATE_PER_1000",
           "Age Standardized Incidence Rate" = "STD_RATE_PER_1000",
           "Crude Life Prevalence" = "CRUDE_RATE_PER_1000",
           "Age Standardized Life Prevalence" = "STD_RATE_PER_1000",
           "Crude HSC Prevalence" = "CRUDE_RATE_PER_1000",
           "Age Standardized HSC Prevalence" = "STD_RATE_PER_1000")
  })
  
  rateInput_data <- reactive({
    switch(input$dataset_data,
           "Crude Incidence Rate" = "CRUDE_RATE_PER_1000",
           "Age Standardized Incidence Rate" = "STD_RATE_PER_1000",
           "Crude Life Prevalence" = "CRUDE_RATE_PER_1000",
           "Age Standardized Life Prevalence" = "STD_RATE_PER_1000",
           "Crude HSC Prevalence" = "CRUDE_RATE_PER_1000",
           "Age Standardized HSC Prevalence" = "STD_RATE_PER_1000")
  })
  
  output$region_d <- renderUI({
    selectInput("region_d",
                label = "Select Health Region(s)",
                choices = (if(input$health_bound == "Health Authorities")  c("Interior","Fraser","Vancouver Coastal","Vancouver Island","Northern")
                           else c(append("All",unique(inc_rate_df$HEALTH_BOUND_NAME)))),
                multiple = TRUE,
                selected = "All")
  })
  
  output$disease_d <- renderUI({
    selectInput("disease_d", 
                label = "Select Disease",
                # choices = append("All",unique(datasetInput_d()$DISEASE)),
                choices = unique(datasetInput_d()$DISEASE),
                multiple = FALSE,
                # selected = "All"
                )
  })
  
  output$disease_r <- renderUI({
    selectInput("disease_r", 
                label = "Select Disease",
                choices = append("All",unique(datasetInput_r()$DISEASE)),
                multiple = TRUE,
                selected = "All")
  })
  
  output$disease_data <- renderUI({
    selectInput("disease_data", 
                label = "Select Disease",
                choices = append("All",unique(datasetInput_data()$DISEASE)),
                multiple = TRUE,
                selected = "All")
  })
  
  filter_df_d <- reactive({
    datasetInput_d() |> 
      filter ((if ("All" %in% input$region_d) TRUE else (HEALTH_BOUND_NAME %in% input$region_d)) &
              # (if ("All" %in% input$disease_d)TRUE else (DISEASE %in% input$disease_d)) &
              (DISEASE %in% input$disease_d) &
              # (YEAR %in% seq(from=min(input$year_range_d),to=max(input$year_range_d))) &
              (if (input$gender_d =='Both') TRUE else (CLNT_GENDER_LABEL ==input$gender_d)))
  })
  
  filter_df_r <- reactive({
    datasetInput_r() |> 
      filter ((if ("All" %in% input$region_r) TRUE else (HEALTH_BOUND_NAME %in% input$region_r)) &
                (if ("All" %in% input$disease_r)TRUE else (DISEASE %in% input$disease_r)) &
                (YEAR %in% seq(from=min(input$year_range_r),to=max(input$year_range_r))) &
                (if (input$gender_r =='Both') TRUE else (CLNT_GENDER_LABEL ==input$gender_r)))
  })
  
  filter_df_data <- reactive({
    datasetInput_data() |> 
      filter ((if ("All" %in% input$region_data) TRUE else (HEALTH_BOUND_NAME %in% input$region_data)) &
                (if ("All" %in% input$disease_data)TRUE else (DISEASE %in% input$disease_data)) &
                (YEAR %in% seq(from=min(input$year_range_data),to=max(input$year_range_data))) &
                (if (input$gender_data =='Both') TRUE else (CLNT_GENDER_LABEL ==input$gender_data)))
  })
  
  output$summary <- renderText({paste0("Some summary info \nselected disease(s):", list(input$disease_d) )})
  
  output$disease_graph1 <- renderPlot({
    filter_df_d()|>
      filter(YEAR == input$year_d) |>
      ggplot(aes_string(x="HEALTH_BOUND_NAME",y= rateInput_d()))+
      geom_bar(stat='summary',fun=mean)+
      labs(x="Health Region",
           y=rateInput_d(),
           title = paste0("Average ", input$dataset_d))
  })
  output$disease_graph2 <- renderPlot({
    filter_df_d()|>
      ggplot(aes_string(y=rateInput_d(),x="YEAR",color = "HEALTH_BOUND_NAME"))+
      geom_line(stat='summary',fun=mean)+
      labs(y=rateInput_d(),
           x="Year",
           title = paste0("Average ", input$dataset_d,"Over Time"))
  })
  
  output$graph2 <- renderPlot({
    filter_df_r()|>
      ggplot(aes_string(y=rateInput_r(),x= "YEAR",color="DISEASE"))+
      geom_line(stat='summary',fun=mean)+
      labs(y=rateInput_r(),
           x="Year",
           legend ="Disease",
           title = paste0("Average ", input$dataset_r," Over Time"))
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filter_df_data(), file)
    }
  )
  
  output$data_table <- renderDataTable(filter_df_data())
  
  output$map <- renderLeaflet({
    new_spdf<-merge(chsa_spdf,filter(filter_df_d(),(YEAR == input$year_d)),by.x="CHSA_CD",by.y="HEALTH_BOUND_CODE")
    
    mybins <- c(0,4,8,12,Inf)
    mypalette <- colorBin( palette="YlOrBr", domain=new_spdf@data$CRUDE_RATE_PER_1000, na.color="transparent", bins=mybins)
    
    mytext <- paste(
      "CHSA: ", new_spdf@data$CHSA_Name,"<br/>", 
      "HA: ", new_spdf@data$HA_Name, "<br/>", 
      "CRUDE_RATE_PER_1000: ", new_spdf@data$CRUDE_RATE_PER_1000, 
      sep="") %>%
      lapply(htmltools::HTML)
    m<-leaflet(new_spdf) %>% 
      setView( lat=55, lng=-123.1 , zoom=4.5) %>%
      addPolygons( 
        fillColor = ~mypalette(new_spdf@data$CRUDE_RATE_PER_1000), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="gray", 
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend( pal=mypalette, values=~new_spdf@data$CRUDE_RATE_PER_1000, opacity=0.9, title = input$dataset_d, position = "bottomleft" )
    m
    
  })
  
  
}

# Run app ----
shinyApp(ui, server)