# Load packages ----
library(shiny)
library(tidyverse)
library(leaflet)
library(rgdal)
library(shinythemes)
library(plotly)


# Source helper functions -----
source('global.R', local = T) #load in data


# User interface ----

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  includeCSS("www/mytheme.css"), #add custom styling
  
  navbarPage("BC Chronic Disease Dashboard",
      
      #Info Tab
      tabPanel("Information", 
               h2("Welcome to the BC Chronic Disease Dashboard"),
               h5("Developed by Jessie Wong, Jennifer Hoang, Mahmoodur Rahman, and Irene Yan"),
               helpText(HTML("<br/><br/>
                             This dashboard facilitates the exploration and visualization of spatial and temporal
                             trends of 25 different chronic diseases across the Province of British Columbia. The data 
                             is sourced from the Chronic Disease Registry (BCCDR).
                             <br/><br/>
                             The three tabs in the dashboard and their respective features are described below.<br/>
                             <ul>
                               <li><b>By Disease</b></li>
                               This tab allows for the trend comparisons of one disease over several Health Authorities(HA)  
                               or Community Health Service Areas (CHSA). In this tab the user should select a rate type, 
                               a disease, geography type, year, and gender. The user can also optionally specify 
                               multiple HAs or CHSAs. <br/><br/>
                               <li><b>By Region</b></li>
                               This tab allows for the trend comparisons of several diseases in one particular HA or CHSA.
                               In this tab the user should select a rate type, disease(s) of interest, geography type,
                               year, and gender. <br/><br/>
                               <li><b>Data</b></li>
                               This tab retrieves all data specified by the user. In this tab the user should select a rate type,
                               disease(s), health region(s), year range, and gender. There is also an option for the user to
                               download the selected data.<br/><br/>
                             </ul>
                             The definitions of the rate types are provided below.
                             <ul>
                               <li><b>Incidence Rate</b> : The rate at which new cases of disease occur in a 
                               specified population during a specified time period. 
                               It is calculated as the number of new cases in the population at-risk in a 
                               specified period of time divided by the person-time at risk or the number of persons at risk 
                               (i.e., mid-year population in a reporting year minus previous year's prevalent cases) in the same period. 
                               <br/><br/>
                               <p style='margin-left: 40px'>Incidence rate  = (number of newly identified cases in a reporting year) / 
                               (mid-year population at risk in the reporting year) * 10<sup>n</sup></p></li>
                               <li><b>Lifetime Prevalence</b>: proportion of individuals who have had the condition for at least part of 
                               their lives at any time during their life course. In the BCCDR, this refers to the proportion of residents 
                               who were diagnosed/identified as a case at least once and were still alive and residing in the province during 
                               a reporting time period (fiscal year). Once the case definition criteria are met in a year, cases are then
                               carried forward to count as a case every year thereafter until the person's death, their migration out of BC,
                               or the absence of follow-up. 
                               <br/><br/>
                               <p style='margin-left: 40px'> Lifetime prevalence = (number of residents ever identified with a disease 
                               in a reporting year) / (mid-year population in the reporting year) * 10<sup>n</sup></p></li>
                               <li><b>Active Healthcare Contact (HSC) Prevalence</b>: For relapsing-remitting diseases, the BCCDR measures 
                               active healthcare contact prevalence. Cases are counted if they previously met case definition criteria 
                               for a disease, continued to live and receive healthcare services for the disease again in BC during a
                               later reporting period. That is, cases are counted for a reporting period if the patient seek healthcare 
                               services for relapsing-remitting conditions again after the fiscal year when they were ascertained as a case. 
                               <br/><br/>
                               <p style='margin-left: 40px'> Active healthcare contact prevalence = (number of patients receiving
                               healthcare services for a disease in a reporting year) /(mid-year population in the reporting year) * 10<sup>n</sup>
                               </p></li>
                             </ul>
                        note about fiscal year  "))),
      
      #By Disease Tab
      tabPanel("By Disease",
               sidebarLayout(
                 
              # Filters
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
                          fluidRow(column(12,plotlyOutput("disease_graph1",height=350))),
                          fluidRow(column(12,plotlyOutput("disease_graph2",height=350))),
                          ))
                 )
               )),
      
      #By Region Tab
      tabPanel(
        "By Region",
        sidebarLayout(
          sidebarPanel(
            h2("Filters"),
            hr(style = "border-top: 1px solid #000000"),
            
            radioButtons(
              "health_bound_r",
              label = "Select Geography",
              choices = c("Health Authorities", "Community Health Service Areas"),
              selected = "Health Authorities"
            ),
            
            uiOutput("region_r"),
            selectInput(
              "dataset_r",
              label = "Select Rate Type",
              choices = c(
                "Crude Incidence Rate",
                "Age Standardized Incidence Rate",
                "Crude Life Prevalence",
                "Age Standardized Life Prevalence",
                "Crude HSC Prevalence",
                "Age Standardized HSC Prevalence"
              )
            ),
            
            uiOutput("disease_r"),
            
            sliderInput(
              "year_range_r",
              label = "Select Year Range",
              min = 2001,
              max = 2020,
              value = c(2001, 2020),
              step = 1
            ),
            
            radioButtons(
              "gender_r",
              label = ("Select Gender"),
              choices = c("Male", "Female", "Both"),
              selected = "Both"
            ),
          ),
          
                 mainPanel(
                   fluidRow(
                     column(6,
                            plotOutput("region_tab_line_chart"),
                     ),
                     column(6,
                            plotOutput("region_tab_pie_chart")
                     )
                   ))
               )),
      
      #Data Tab
      tabPanel("Data",
               sidebarLayout(
                 
                 #Filters
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
  
  output$region_r <- renderUI({
    selectInput("region_r",
                label = "Select Health Region",
                choices = (if(input$health_bound_r == "Health Authorities")  c("Interior","Fraser","Vancouver Coastal","Vancouver Island","Northern")
                           else unique(inc_rate_df$HEALTH_BOUND_NAME)),
                multiple = FALSE)
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
                label = "Select Disease(s)",
                choices = unique(datasetInput_r()$DISEASE),
                multiple = TRUE,
                selected = unique(datasetInput_r()$DISEASE)[1])
  }) 
  
  output$disease_data <- renderUI({
    selectInput("disease_data", 
                label = "Select Disease",
                choices = append("All", unique(datasetInput_data()$DISEASE)),
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
      filter ((HEALTH_BOUND_NAME %in% input$region_r) &
                (if ("All" %in% input$disease_r) TRUE else (DISEASE %in% input$disease_r)) &
                (YEAR %in% seq(from=min(input$year_range_r),to=max(input$year_range_r))) &
                (if (input$gender_r == 'Both') TRUE else (CLNT_GENDER_LABEL == input$gender_r)))
  })
  
  filter_df_data <- reactive({
    datasetInput_data() |> 
      filter ((if ("All" %in% input$region_data) TRUE else (HEALTH_BOUND_NAME %in% input$region_data)) &
                (if ("All" %in% input$disease_data)TRUE else (DISEASE %in% input$disease_data)) &
                (YEAR %in% seq(from=min(input$year_range_data),to=max(input$year_range_data))) &
                (if (input$gender_data =='Both') TRUE else (CLNT_GENDER_LABEL ==input$gender_data)))
  })
  
  output$summary <- renderText({paste0("Some summary info \nselected disease(s):", list(input$disease_d) )})
  
  output$disease_graph1 <- renderPlotly({
    p<-filter_df_d()|>
      filter(YEAR == input$year_d) |>
      ggplot(aes_string(x="HEALTH_BOUND_NAME",y= rateInput_d(),color = "HEALTH_BOUND_NAME",fill = "HEALTH_BOUND_NAME"))+
      geom_bar(stat='summary',fun=mean)+
      labs(x="Health Region",
           y=rateInput_d(),
           title = paste0("Average ", input$dataset_d))
    ggplotly(p)
  })
  output$disease_graph2 <- renderPlotly({
   p2<- filter_df_d()|>
      ggplot(aes_string(y=rateInput_d(),x="YEAR",color = "HEALTH_BOUND_NAME"))+
      geom_line(stat='summary',fun=mean)+
      labs(y=rateInput_d(),
           x="Year",
           title = paste0("Average ", input$dataset_d,"Over Time"))
   ggplotly(p2)
  })
  
  output$region_tab_line_chart <- renderPlot({
    filter_df_r() |>
      ggplot(aes_string(y = rateInput_r(), x = "YEAR", color = "DISEASE")) +
      geom_line(stat = 'summary', fun = mean) +
      labs(
        y = rateInput_r(),
        x = "Year",
        legend = "Disease",
        title = paste0(input$dataset_r, " Over Time")
      )
  })
  
  output$region_tab_pie_chart <- renderPlot({
    filter_df_r() |> 
      ggplot(aes_string(x= "2", y = rateInput_r(), fill = "DISEASE")) +
      geom_bar(stat = "identity") +
      coord_polar("y", start = 0) +
      labs(
        y = rateInput_r(),
        legend = "Disease",
        title = paste0("Distribution of Diseases by ", input$dataset_r)
      )
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