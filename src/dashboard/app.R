################################
# Load packages
# ################################
suppressPackageStartupMessages({
library(shiny)
library(shinyjs)
library(plyr)
library(tidyverse)
library(leaflet)
library(sp)
library(rgdal)
library(shinythemes)
library(plotly)
library(scales) # used to format the axis values
library(shinycssloaders)
library(rgeos)
library(shinyWidgets)
library(DT)
library(shinyBS)
})

################################
# Source helper functions and templates
# 
# Define and load all the global variables, including the data frames and shape files
################################
source('global.R', local = T) 
source('info.R', local = T)
source('helpers.R', local = T)
options(spinner.color="#003366")

################################
# UI Side Logic
# 
# Define the ui of the four main tabs of the dashboard from left to right
################################
ui <- fluidPage(
  theme = "mytheme.css",
  shinyjs::useShinyjs(),
  leafletjs,
  tab_colsjs,
  id="body",
  list(tags$head(HTML('<link rel="icon", href="bc-gov-logo.png", type="image/png" />'))),
  navbarPage(title = div(img(id = "logo",src="bc-gov-logo.png"),"BC Chronic Disease Dashboard"),
             position = "fixed-top", 
             id = "navbarID",
      ################################
      # Information Tab UI Side Logic
      ################################
      navbarMenu("Information",
        tabPanel("About", 
                 p(HTML("<h1><u>Welcome to the BC Chronic Disease Dashboard!</u></h1>")),
                 h5("Developed by Jessie Wong, Jennifer Hoang, Mahmoodur Rahman, and Irene Yan"),
                 h6("UBC Master of Data Science Capstone Project"),
                 p(HTML(about_info)),
                 hr(style = "margin-bottom: 20px;
                             border: 0;
                             border-top: 1px solid #eee"),
                 helpText(HTML("For internal use only. Do not distribute.<br/>
                                For questions about this dashboard, please contact hlth.cdrwg@gov.bc.ca"))
                 ),
        tabPanel("Rate Types",
                 p(HTML("<u><h2>Rate Types</h2></u></br>")),
                 p(HTML(rate_info))
                 ),
        tabPanel("Diseases",
                 p(HTML("<u><h2>Diseases</h2></u></br>")),
                 p(HTML(disease_info)),
                 actionButton("show_pdf", "Show PDF"),
                 uiOutput("pdfviewer")
                 ),
        tabPanel("Data Dictionary",
                 p(HTML("<u><h2>Data Dictionary</h2></u></br>")),
                 p(HTML(data_dict_info))
                ),
        ),
      
      ################################
      # "By Disease" Tab UI Side Logic
      ################################
      tabPanel("By Disease",
               sidebarLayout(
                 sidebarPanel(
                   id="filters_d",
                   width = 3,
                   h2("Data Selection"),
                   hr(),
                   selectInput("disease_d",
                               label= "Select Disease",
                               choices = ALL_DISEASES),
                   uiOutput("dataset_d"),
                   geography_radio_buttons("health_bound_d"),
                   uiOutput("region_d"),
                   sex_radio_buttons("gender_d"),
                   year_slider("year_d"),
                   fisc_year_tt("year_info_d"),
                   br(),
                   actionButton("reset_d", "Reset")
               ),
               mainPanel(
                 width = 9,
                 fluidRow(
                   column(3,htmlOutput("text_d1")),
                   column(3,htmlOutput("text_d2")),
                   column(3,htmlOutput("text_d3")),
                   column(3,htmlOutput("text_d4"))
                 ),
                 fluidRow(
                   column(6, leafletOutput("map",height=645)%>% withSpinner()),
                   column(6, 
                          fluidRow(column(12,plotlyOutput("disease_graph_bar",height=295)%>% withSpinner())),
                          fluidRow(column(4,material_switch("yax_switch_d","Y-axis from 0")),
                                   column(8,uiOutput("modeldata_d"))),
                          fluidRow(column(12,plotlyOutput("disease_graph_line",height=295)%>% withSpinner())),
                          )))
               )),
      
      ################################
      # "By Region" Tab UI Side Logic
      ################################
      tabPanel("By Region",
               sidebarLayout(
                 sidebarPanel(
                   id="filters_r",
                   width = 3,
                   h2("Data Selection"),
                   hr(),
                   geography_radio_buttons("region_tab_geography_selected"),
                   uiOutput("region_tab_region_selected"),
                   rate_type_input("region_tab_rate_type_selected"),
                   uiOutput("region_tab_diseases_selected"),
                   sex_radio_buttons("region_tab_sex_selected"),
                   year_slider("region_tab_year_range_selected"),
                   fisc_year_tt("region_tab_year_slider_info"),
                   br(),
                   actionButton("region_tab_reset_button", "Reset")
                 ),
                 mainPanel(
                   width = 9,
                   fluidRow(plotlyOutput("region_tab_bar_chart") %>% withSpinner()),
                   fluidRow(column(4,material_switch("region_tab_line_y0switch","Y-axis from 0"))),
                   fluidRow(plotlyOutput("region_tab_line_chart") %>% withSpinner()))
                 
              )), 
      
      ################################
      # Download Data Tab UI Side Logic
      ################################
      tabPanel("Data",
               sidebarLayout(
                 #Filters
                 sidebarPanel(
                   id="filters_data",
                   width = 3,
                   h2("Data Selection"),
                   hr(),
                   rate_type_input("dataset_data"),
                   uiOutput("disease_data"),
                   geography_radio_buttons("health_bound_data"),
                   uiOutput("region_data"),
                   year_slider("year_range_data",anim = FALSE),
                   fisc_year_tt("year_info_data"),
                   sex_radio_buttons("gender_data"),
                   br(),br(),
                   actionButton("reset_data", "Reset")
                 ),
                 mainPanel(
                   width = 9,
                   downloadButton("download_data", label = "Download Data"),
                   hr(style = "margin-bottom: 20px;
                               border: 0;
                               border-top: 1px solid #eee"),
                   DTOutput("data_table")
                 )
               )),

      #############
      # MAHMOOD TAB
      #############
      tabPanel("Mahmood",
               mainPanel(
                 img(src='model_image2.png',align="center",style="width: 1000px"),
               ))
  )
)

################################
# Server Side Logic
################################
server <- function(input, output,session) {
  
  # Change bg color depending on tab
  observeEvent(input$navbarID, {
    if(input$navbarID %in% c("About","Rate Types","Diseases","Data Dictionary")){
      session$sendCustomMessage("background-color", "white")
    } else {
      session$sendCustomMessage("background-color", "#cccccc")
    }
  })
  
  ################################
  # Info Tab Server Side Logic
  ################################
  
  # Show disease pdf on button click
  observeEvent(input$show_pdf, {
    output$pdfviewer<-renderUI({
      tags$iframe(
        src="CDR_Case_Definitions.pdf",
        width="100%",
        height="800px")
      
    })
  })
  
  ################################
  # By Disease Tab Server Side Logic
  ################################
  
  # Reset filters
  observeEvent(input$reset_d, {
    reset("filters_d")
  })
  
  # Conditionally show modeled data toggle switch
  observe({
    if(input$gender_d =="Total"&& 
       input$health_bound_d=="Community Health Service Areas" &&
       startsWith(input$dataset_d,"Age")){
      output$modeldata_d <- renderUI({
        material_switch("modeldata_d_switch","Smoothed Time Trends ")
      })
    }else{
      output$modeldata_d <- renderUI({ })
    }
  })

  # Dynamic UI for rate selection
  output$dataset_d <- renderUI({
    selectInput("dataset_d", 
                label = "Select Rate Type",
                choices = (
                  if(input$disease_d %in% HSC_DISEASES) RATE_TYPE_CHOICES
                  else
                    c("Crude Incidence Rate",
                      "Age Standardized Incidence Rate",
                      "Crude Life Prevalence",
                      "Age Standardized Life Prevalence")
                ),
                selected = "Crude Incidence Rate",
                multiple = FALSE,
    )
  })
  
  # Dynamic UI for region selection
  output$region_d <- renderUI({
    selectInput("region_d",
                label = "Select Health Boundaries",
                choices = health_bounds(input$health_bound_d),
                multiple = TRUE,
                selected = (
                  if(input$health_bound_d == "Health Authorities") HA_CHOICES
                  else c("100 Mile House","Comox","Mackenzie","Port Coquitlam","Kitsilano")
                ))
  })
  
  # Dataset selection based on user input
  datasetInput_d <- reactive({
    shiny::validate(need(input$dataset_d, message=F))
    if(!is.null(input$dataset_d)){
      dataset_switch(input$dataset_d)
    }
  })
  
  # Rate selection based on user input
  rateInput_d <- reactive({
    shiny::validate(need(input$dataset_d, message=F))
    if(!is.null(input$dataset_d)){
      rate_switch(input$dataset_d)
    }
  })
  
  # Geography selection based on user input
  healthboundInput_d <- reactive ({
    shiny::validate(need(input$health_bound_d, message=F))
    if(!is.null(input$health_bound_d)){
    switch(input$health_bound_d,
           "Health Authorities" = "HA",
           "Community Health Service Areas" = "CHSA")
    }
  })
  
  # Map geography selection based on user input
  spdf_d <- reactive ({
    shiny::validate(need(input$health_bound_d, message=F))
    if(!is.null(input$health_bound_d)){
    switch(input$health_bound_d,
           "Health Authorities" = ha_spdf,
           "Community Health Service Areas" = chsa_spdf)
    }
  })
  
  # Filter dataset based on user input
  filter_df_d <- reactive({
    datasetInput_d() |> 
      filter ((GEOGRAPHY == healthboundInput_d())&
              (DISEASE == input$disease_d) &
              (CLNT_GENDER_LABEL == substr(input$gender_d,1,1))
      )
  })
  
  filter_df_reg_d <- reactive({
    filter_df_d() |> 
      filter (HEALTH_BOUND_NAME %in% input$region_d)
  })
  
  filter_df_yr_d <- reactive({
    filter_df_d() |> 
      filter (YEAR == input$year_d)
  })
  
  # Define reactive error bounds 
  error <- reactiveValues(
    lower = NULL,
    upper = NULL
  )
  
  #Render disease stats
  output$text_d1 <- renderText({
    data <- filter_df_d()
    reg_max <-data|>
      arrange(desc(data[[rateInput_d()]]))|>
      slice(1)|>
      pull(HEALTH_BOUND_NAME)
    
    paste0(
      healthboundInput_d()," with Highest Maximum ",input$dataset_d,
      " in 2001-2020 <br>", "<div id=stat>",reg_max,"</div>"
    )
  })
  
  output$text_d2 <- renderText({
    data <- filter_df_d()|>
      group_by(HEALTH_BOUND_NAME)|>
      summarize_at(rateInput_d(), mean)
    reg_avg<- data|>
      arrange(desc(data[[rateInput_d()]]))|>
      slice(1)|>
      pull(HEALTH_BOUND_NAME)
    
    paste0(
      healthboundInput_d()," with Highest Average ",input$dataset_d,
      " in 2001-2020 <br>","<div id=stat>", reg_avg,"</div>"
    )
  })
  
  output$text_d3 <- renderText({
    avg_rate <- median(filter_df_d()[[rateInput_d()]])
    
    paste0(
      "Median Recorded ",input$dataset_d," Over All ", healthboundInput_d(),"s",
      "<br>", "<div id=stat>",format_round(avg_rate),"</div>"
    )
  })
  
  output$text_d4 <- renderText({
    data<-filter_df_d()|>
      group_by(YEAR)|>
      summarize_at(rateInput_d(),median)
    year_max <- data|>
      arrange(desc(data[[rateInput_d()]]))|>
      slice(1)|>
      pull(YEAR)
  
    paste0(
      "Year of Highest Median Recorded ",input$dataset_d,
      "<br>", "<div id=stat>", year_max,"</div>"
    )
  })  
  
  # Render disease bar graph for each rate/disease 
  output$disease_graph_bar <- renderPlotly({
    
    dummyData <- filter_df_d()|>
      filter(HEALTH_BOUND_NAME %in% input$region_d,
             YEAR == 2001
             )
    error$lower <- paste0(sub("\\_.*", "", rateInput_d()),"_LCL_95")
    error$upper <- paste0(sub("\\_.*", "", rateInput_d()),"_UCL_95")

    p <- plot_ly(source = "disease_graph_bar")
    for(reg in input$region_d){
      dummy_df <- dummyData[which(dummyData$HEALTH_BOUND_NAME==reg),]
      p <- p |>
        add_trace(x=dummy_df$HEALTH_BOUND_NAME,
                  y=dummy_df[[rateInput_d()]],
                  type = 'bar',
                  error_y=list(
                    type = "data",
                    symmetric = FALSE,
                    arrayminus =dummy_df[[rateInput_d()]]- dummy_df[[error$lower]],
                    array = dummy_df[[error$upper]]- dummy_df[[rateInput_d()]],
                    color = '#000000',
                    width = 10),
                  marker = list(color = if(input$health_bound_d == "Health Authorities")
                                  HA_colours$Colors[match(dummy_df$HEALTH_BOUND_NAME,HA_colours$Regions)]
                                else
                                  CHSA_colours$Colors[match(dummy_df$HEALTH_BOUND_NAME,CHSA_colours$Regions)]
                  ),
                  hovertemplate = paste('<b>Health Region</b>: %{x}',
                                        '<br><b>%{yaxis.title.text}</b>: %{y:.2f}',
                                        '<br><b>95% Confidence Interval</b>: (',
                                        format_round(dummy_df[[error$lower]]), ',',
                                        format_round(dummy_df[[error$upper]]),')',
                                        '<extra></extra>')
        )
    }
    
   p %>%
      layout(yaxis = append(list(range=list(0,max(filter_df_reg_d()[[error$upper]],na.rm=TRUE)*1.05)),
                             y_axis_spec(input$dataset_d,"nonnegative")),
             xaxis = x_axis_bar_spec('Health Region'),
             title = list(text = paste0('<b>',input$dataset_d," of \n",input$disease_d, " in 2001 </b>"),
                          y=0.92,
                          font = list(size = 16)),
             barmode = "overlay",
             margin = list(t = 80,b=50),
             showlegend = FALSE
      ) %>%
      event_register('plotly_hover')

  })
  
  # Update Disease Bar Graph with filter changes
  observe({
    invalidateLater(500)
    newdata <- filter_df_d()|>
      filter((YEAR == input$year_d)&
               ((HEALTH_BOUND_NAME %in% input$region_d)))
    
    error$lower <- paste0(sub("\\_.*", "", rateInput_d()),"_LCL_95")
    error$upper <- paste0(sub("\\_.*", "", rateInput_d()),"_UCL_95") 
    
    p <- plotlyProxy("disease_graph_bar", session)
    
    for(reg in seq_along(input$region_d)){
      df <- newdata[which(newdata$HEALTH_BOUND_NAME==input$region_d[reg]),]
      p |>
        plotlyProxyInvoke("restyle",
                          "y",
                          list(list(df[[rateInput_d()]])),
                          as.integer(reg)-1
        )|>
        plotlyProxyInvoke("restyle",
                          list(
                          error_y=list(
                            type = "data",
                            symmetric = FALSE,
                            arrayminus =list(df[[rateInput_d()]]- df[[error$lower]]),
                            array = list(df[[error$upper]]- df[[rateInput_d()]]),
                            color = '#000000',
                            width = 10),
                          hovertemplate = paste('<b>Health Region</b>: %{x}',
                                                '<br><b>%{yaxis.title.text}</b>: %{y:.2f}',
                                                '<br><b>95% Confidence Interval</b>: (',
                                                format_round(df[[error$lower]]),',',
                                                format_round(df[[error$upper]]),')',
                                                '<extra></extra>'
                          )),
                          as.integer(reg)-1)
                          
    }
  
    p %>%  plotlyProxyInvoke("relayout",
                        list(
                          autosize = F,
                          yaxis = append(list(range=list(0,max(filter_df_reg_d()[[error$upper]],na.rm=TRUE)*1.05)),
                                        y_axis_spec(input$dataset_d,"nonnegative")),
                          xaxis = append(list(fixedrange = TRUE),
                                         x_axis_bar_spec('Health Region')),
                          title = list(text = HTML(paste0('<b>',input$dataset_d," of<br>",input$disease_d, " in ",input$year_d, "</b><br>   ")),
                                       y=0.92,
                                       font = list(size = 16)
                                       )
                        ))
    
  })
  
  # Render disease line graph 
  output$disease_graph_line <- renderPlotly({
    
    d <- filter_df_d()|>
      filter(HEALTH_BOUND_NAME %in% input$region_d)
    
    d |>
    plot_ly(
      x= d$YEAR,
      y=d[[rateInput_d()]],
      source = "disease_graph_line",
      type = "scatter",
      mode="lines",
      customdata = ~HEALTH_BOUND_NAME,
      line = list(width=2),
      color = ~HEALTH_BOUND_NAME,
      colors = if(input$health_bound_d == "Health Authorities")
                  setNames(HA_colours$Colors,HA_colours$Regions) 
               else 
                setNames(CHSA_colours$Colors,CHSA_colours$Regions),
      hovertemplate = hovertemplate_line
      
      
    )%>%
      layout(yaxis = append(list(range=list(0,max(filter_df_reg_d()[[rateInput_d()]],na.rm=TRUE)*1.05)),
                                    y_axis_spec(input$dataset_d,"nonnegative")),
             xaxis = x_axis_line_spec('Year'),
             title = list(text = paste0('<b>',input$dataset_d," of  \n",input$disease_d, " Over Time </b>"),
                          y=0.92,
                          font = list(size = 16)),
             margin = list(t=80,b=50),
             legend = list(title=list(text='Health Region')),
             shapes = list(vline(2001))
      ) %>%
      event_register('plotly_hover')
  })
  
  # Update disease line graph with year 
  observe({
    invalidateLater(500)
    p <- plotlyProxy("disease_graph_line", session)
    
    p %>%
      plotlyProxyInvoke("relayout",
                        list(
                          shapes = list(vline(input$year_d))
                        ))
  })
  
  
  # Switch to change line graph to start at 0 
  observeEvent(input$yax_switch_d,{
    p <- plotlyProxy("disease_graph_line", session)
    if(input$yax_switch_d==TRUE){
    p%>%
      plotlyProxyInvoke("relayout",
                        list(
                          append(list(range=list(0,max(filter_df_reg_d()[[rateInput_d()]],na.rm=TRUE)*1.05)),
                                 y_axis_spec(input$dataset_d,"tozero")),
                        ))
    }else{
      p%>%
        plotlyProxyInvoke("relayout",
                          list(
                            yaxis = list(title = list(text = paste0(input$dataset_d," Per 1000"),
                                                      font = list(size = ifelse(startsWith(rateInput_d(),"STD"),12,14))),
                                         range=list(0,max(filter_df_reg_d()[[rateInput_d()]],na.rm=TRUE)*1.05),
                                         gridcolor = "#d9dadb",
                                         showline= T, linewidth=1, linecolor='black',
                                         rangemode = "nonnegative")
                          ))
      
    }
  })

  # Switch to change line graph to modeled data
  observeEvent(input$modeldata_d_switch,{
    data <- filter_df_d()|>
      filter(HEALTH_BOUND_NAME %in% input$region_d)

    p <- plotlyProxy("disease_graph_line", session)
    if(input$modeldata_d_switch==TRUE){
      p %>%
        plotlyProxyInvoke("deleteTraces",as.list(seq(0,length(input$region_d)-1)))
      
      for(reg in input$region_d){
        df <- data[which(data$HEALTH_BOUND_NAME==reg),]
        p |>
          plotlyProxyInvoke("addTraces",
                            list(
                              x = df$YEAR,
                              y = df$y_fitted,
                              type = "scatter",
                              mode="lines",
                              name=reg,
                              customdata = df$HEALTH_BOUND_NAME,
                              line = list(width=2,
                                          color = CHSA_colours$Colors[match(reg,CHSA_colours$Regions)]),
                              hovertemplate = hovertemplate_line
                            ))

      }

    }else{
      p %>%
        plotlyProxyInvoke("deleteTraces",as.list(seq(0,length(input$region_d)-1)))
      for(reg in input$region_d){
        df <- data[which(data$HEALTH_BOUND_NAME==reg),]
        p |>
          plotlyProxyInvoke("addTraces",
                            list(
                              x = df$YEAR,
                              y = df[[rateInput_d()]],
                              type = "scatter",
                              mode="lines",
                              name=reg,
                              customdata = df$HEALTH_BOUND_NAME,
                              line = list(width=2,
                                          color = CHSA_colours$Colors[match(reg,CHSA_colours$Regions)]),
                              hovertemplate = hovertemplate_line
                            ))
      }
    }
    })

  # Render map once per Input Rate/Disease
  output$map <- renderLeaflet({
    
    #select dummy data
    dummyData <- datasetInput_d() |>
      filter(CLNT_GENDER_LABEL=='T',
             GEOGRAPHY=="HA",
             DISEASE=="Asthma",
             YEAR==2001) 
    
    dummy_spdf <- data.table::copy(spdf_d())
    
    if(input$health_bound_d == "Health Authorities"){
      dummy_spdf@data <- spdf_d()@data|>
          left_join(dummyData,by=c("HA_Name"="HEALTH_BOUND_NAME"))
    }else{
      dummy_spdf@data <- spdf_d()@data|>
        left_join(dummyData,by=c("CHSA_Name"="HEALTH_BOUND_NAME"))
    }
    
    leaflet(dummy_spdf)%>%
      setView( lat=53.5, lng=-127 , zoom=4.5) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels)%>%
      addPolygons(
        layerId = (if(input$health_bound_d == "Health Authorities") ~HA_Name else ~CHSA_Name),
        stroke=TRUE,
        fillOpacity = 0.9,
        color="gray",
        weight=1,
        highlight = highlightOptions(
          weight = 2,
          color = "black",
          opacity = 1.0)
    )
  })
  
#Update map with filter changes
  observe({
  
    year_filtered_map_df <- filter_df_yr_d()
    error$lower <- paste0(sub("\\_.*", "", rateInput_d()),"_LCL_95")
    error$upper <- paste0(sub("\\_.*", "", rateInput_d()),"_UCL_95")
    current_map_spdf <- data.table::copy(spdf_d())
    if(input$health_bound_d == "Health Authorities"){
      current_map_spdf@data <- spdf_d()@data|>
        left_join(year_filtered_map_df,by=c("HA_Name"="HEALTH_BOUND_NAME"))
    }else{
      current_map_spdf@data <- spdf_d()@data|>
        left_join(year_filtered_map_df,by=c("CHSA_Name"="HEALTH_BOUND_NAME"))
    }

    if(input$health_bound_d == "Health Authorities"){
      current_map_spdf@data$text <- paste0(
        "<b>HA</b>: ", current_map_spdf@data$HA_Name, "<br/>",
        "<b>",input$dataset_d,": ","</b>",
        format_round(current_map_spdf@data[[rateInput_d()]]),"<br/>",
        "<b>95% Confidence Interval</b>: (",
        format_round(current_map_spdf@data[[error$lower]]),",",
        format_round(current_map_spdf@data[[error$upper]]),")")
    }else{
      current_map_spdf@data$text <- paste0(
        "<b>CHSA</b>: ", current_map_spdf@data$CHSA_Name,"<br/>",
        "<b>HA</b>: ", current_map_spdf@data$HA_Name, "<br/>",
        "<b>",input$dataset_d,": ","</b>",
        format_round(current_map_spdf@data[[rateInput_d()]]),"<br/>",
        "<b>95% Confidence Interval</b>: (",
        format_round(current_map_spdf@data[[error$lower]]),",",
        format_round(current_map_spdf@data[[error$upper]]),")")
  }

    legend_inc <- round_any(unname(quantile(filter_df_d()[[rateInput_d()]],0.85))/5,
                            ifelse(max(filter_df_d()[[rateInput_d()]])<10,0.005,0.1))
    mybins <- append(seq(round_any(min(filter_df_d()[[rateInput_d()]]),0.05, f=floor),
                         by=legend_inc,length.out=5),Inf)
    mypalette <- colorBin( palette="YlOrBr", domain=current_map_spdf@data[[rateInput_d()]], 
                           bins=mybins, na.color="#cccccc")
    labels<-c(paste0("< ",mybins[2]),
              paste0(mybins[2]," - ",mybins[3]),
              paste0(mybins[3]," - ",mybins[4]),
              paste0(mybins[4]," - ",mybins[5]),
              paste0(mybins[5]," + ")
    )
    
    leafletProxy("map",data = current_map_spdf) %>%
      clearMarkers() %>%
      clearControls()%>%
      addLegend( pal=mypalette, 
                 values=current_map_spdf@data[[rateInput_d()]], 
                 opacity=0.9,
                 title = paste0(input$dataset_d," Per 1000"),
                 position = "bottomleft",
                 labFormat = function(type, cuts, p) {  
                   paste0(labels)
                 })%>%
      setShapeStyle(layerId = (if(input$health_bound_d == "Health Authorities") 
                                ~HA_Name
                               else ~CHSA_Name),
                    fillColor = mypalette(current_map_spdf@data[[rateInput_d()]]),
                    label = current_map_spdf@data$text
                    )

  })

  ## Linked highlighting when hovering on map
  rv_shape <- reactiveVal(FALSE)
  rv_location <- reactiveValues(id=NULL,lat=NULL,lng=NULL)
  rv_location_move_old <- reactiveValues(lat=NULL,lng=NULL)
  
  # Track mouseover activity
  observeEvent(input$map_shape_mouseover,{
    rv_shape(TRUE)
    event_info <- input$map_shape_mouseover
    error$lower <- paste0(sub("\\_.*", "", rateInput_d()),"_LCL_95")
    error$upper <- paste0(sub("\\_.*", "", rateInput_d()),"_UCL_95")
    bar_data<- filter_df_yr_d()
    ppl <-  plotlyProxy("disease_graph_line", session)
    ppb <- plotlyProxy("disease_graph_bar", session)
    rv_location$id <- event_info$id
    rv_location$lat <- event_info$lat
    rv_location$lng <- event_info$lng
    if ((event_info$id %in% input$region_d)){
      for (reg in my_traces()){
        ppl %>%
          plotlyProxyInvoke(
            method = "restyle",
            list(line = list(width = 0.5,
                             color = CHSA_colours$Colors[match(reg,CHSA_colours$Regions)])),
            as.integer(match(reg,my_traces())-1)
          )
      }
      ppl %>%
        plotlyProxyInvoke(
          method = "restyle",
          "line",
          list(width = 3,
               color = CHSA_colours$Colors[match(event_info$id,CHSA_colours$Regions)]),
          as.integer(match(event_info$id,my_traces())-1)
        )
      ppb %>%
        plotlyProxyInvoke(
          method = "restyle",
          list(opacity=0.2)
        ) %>%
        plotlyProxyInvoke(
          method = "restyle",
          list(opacity = 1),
          as.integer(match(event_info$id, my_traces())-1))
          
   }
  })
  
  # Track mouseout activity
  observeEvent(input$map_shape_mouseout, {
    event_info <- input$map_shape_mouseover
    event_info_old <- reactiveValuesToList(rv_location_move_old)
    ppl <-  plotlyProxy("disease_graph_line", session)
    ppb <- plotlyProxy("disease_graph_bar", session)
    
    if (all(unlist(event_info[c('lat','lng')]) == unlist(event_info_old[c('lat','lng')]))){
      rv_shape(FALSE)
      for (reg in my_traces()){
        ppl %>% plotlyProxyInvoke(method="restyle",
                                  list(line = list(width=2,
                                                   color = CHSA_colours$Colors[match(reg,CHSA_colours$Regions)])),
                                  as.integer(match(reg,my_traces())-1))
        
      } 
      ppb %>% plotlyProxyInvoke(method = "restyle",list(opacity = 1))

    }else{
      rv_location_move_old$lat <- event_info$lat
      rv_location_move_old$lng <- event_info$lng
    }
  })
  
  # Define graph traces 
  my_traces <- reactive({
    sort(input$region_d)
  })
  
  # Link highlighting when hovering on bar graph
  observe({
    req(filter_df_d())
      event <- event_data("plotly_hover",source = "disease_graph_bar")
      error$lower <- paste0(sub("\\_.*", "", rateInput_d()),"_LCL_95")
      error$upper <- paste0(sub("\\_.*", "", rateInput_d()),"_UCL_95")
      bar_data<- filter_df_yr_d()
      ppl <-plotlyProxy("disease_graph_line", session)
      ppb <- plotlyProxy("disease_graph_bar", session)
      lp <- leafletProxy("map",session)
      if (is.null(event)){
        for (reg in my_traces()){
          ppl %>% plotlyProxyInvoke(method="restyle",
                                    list(line = list(width=2,
                                                     color = CHSA_colours$Colors[match(reg,CHSA_colours$Regions)])),
                                    as.integer(match(reg,my_traces())-1))
          
        } 
        ppb %>% plotlyProxyInvoke(method = "restyle",list(opacity = 1))   
        lp %>% clearGroup('selected')
      }else{
        for (reg in my_traces()){
          ppl %>%
            plotlyProxyInvoke(
              method = "restyle",
              list(line = list(width = 0.5,
                               color = CHSA_colours$Colors[match(reg,CHSA_colours$Regions)])),
              as.integer(match(reg,my_traces())-1)
            )
        }
        ppl %>%
          plotlyProxyInvoke(
            method = "restyle",
            "line",
            list(width = 3,
                 color = CHSA_colours$Colors[match(event[["x"]],CHSA_colours$Regions)]),
            as.integer(match(event[["x"]],my_traces())-1)
          )
        ppb %>%
          plotlyProxyInvoke(
            method = "restyle",
            list(opacity=0.2)
          ) %>%
          plotlyProxyInvoke(
            method = "restyle",
            list(opacity = 1),
            as.integer(match(event[["x"]], my_traces())-1)
          )
      lp %>%
        addPolygons(
          data=subset(spdf_d(),
                      (if(input$health_bound_d == "Health Authorities") HA_Name 
                       else CHSA_Name) 
                      == event[["x"]]),
          stroke= TRUE,
          weight = 2,
          color = "black",
          fill= NaN,
          group = "selected"
        )}
    })
  
  ## Linked highlighting when hovering on line graph
  observe({
    req(filter_df_d())
    event <- event_data("plotly_hover",source = "disease_graph_line")
    error$lower <- paste0(sub("\\_.*", "", rateInput_d()),"_LCL_95")
    error$upper <- paste0(sub("\\_.*", "", rateInput_d()),"_UCL_95")
    bar_data<- filter_df_yr_d()
    ppl <-plotlyProxy("disease_graph_line", session)
    ppb <- plotlyProxy("disease_graph_bar", session)
    lp <- leafletProxy("map",session)
    if (is.null(event)){
      for (reg in my_traces()){
      ppl %>% plotlyProxyInvoke(method="restyle",list(line = list(width=2,
                                                                  color = CHSA_colours$Colors[match(reg,CHSA_colours$Regions)])),
                                as.integer(match(reg,my_traces())-1))
                                
      }                                  
      ppb %>%  plotlyProxyInvoke(method = "restyle",list(opacity = 1)) 
      lp %>% clearGroup('selected')
    }else{
      for (reg in my_traces()){
        ppl %>%
          plotlyProxyInvoke(
            method = "restyle",
            list(line = list(width = 0.5,
                             color = CHSA_colours$Colors[match(reg,CHSA_colours$Regions)])),
            as.integer(match(reg,my_traces())-1)
          )
      }
      ppl %>%
        plotlyProxyInvoke(
          method = "restyle",
          "line",
          list(width = 3,
               color = CHSA_colours$Colors[match(event[["customdata"]],CHSA_colours$Regions)]),
          as.integer(match(event[["customdata"]],my_traces())-1)
        )
      
      ppb %>%
        plotlyProxyInvoke(
          method = "restyle",
          list(opacity=0.2)
        ) %>%
        plotlyProxyInvoke(
          method = "restyle",
          list(opacity = 1),
          as.integer(match(event[["customdata"]],my_traces())-1)
          )
      lp %>%
        addPolygons(
          data=subset(spdf_d(),
                      (if(input$health_bound_d == "Health Authorities") HA_Name 
                       else CHSA_Name) 
                      == event[["customdata"]]),
          stroke= TRUE,
          weight = 2,
          color = "black",
          fill= NaN,
          group = "selected"
        )}
  })
   
  
  ################################
  # By Region Tab Server Side Logic
  ################################
  
  # Reset filters
  observeEvent(input$region_tab_reset_button, {
    reset("filters_r")
  })
  
  
  region_tab_dataset_used <- reactive({
    dataset_switch(input$region_tab_rate_type_selected)
  })
  
  region_tab_rate_as_variable <- reactive({
    rate_switch(input$region_tab_rate_type_selected)
  })
  
  output$region_tab_region_selected <- renderUI({
    selectInput(
      "region_tab_region_selected",
      label = "Select Health Boundary",
      choices = health_bounds(input$region_tab_geography_selected),
      multiple = FALSE,
      selected = HA_CHOICES[1]
    )
  })
  
  output$region_tab_diseases_selected <- renderUI({
    selectizeInput("region_tab_diseases_selected", 
                   label = "Select Disease(s)",
                   choices = unique(region_tab_dataset_used()$DISEASE),
                   multiple = TRUE,
                   selected = unique(region_tab_dataset_used()$DISEASE)[1])
  }) 
  
  region_tab_filtered_data <- reactive({
    region_tab_dataset_used() |>
      filter((HEALTH_BOUND_NAME %in% input$region_tab_region_selected) &
             (DISEASE %in% input$region_tab_diseases_selected) &
             (CLNT_GENDER_LABEL == substr(input$region_tab_sex_selected, 1, 1)))
  })
  
  #####################
  # Line Chart Related Logic
  #####################
  
  # Plot a line chart showing trends of diseases over time
  # x is YEAR, y is the selected rate type, color is DISEASE
  output$region_tab_line_chart <- renderPlotly({
    data <- region_tab_filtered_data()
    
    data |>
      plot_ly(
        x = data$YEAR,
        y = data[[region_tab_rate_as_variable()]],
        type = "scatter",
        mode = "lines",
        line = list(width = 2),
        color = ~ DISEASE,
        showlegend = T,
        hovertemplate = hovertemplate_line
      ) |>
      layout(
        yaxis = y_axis_spec(input$region_tab_rate_type_selected,"nonnegative"),
        xaxis = x_axis_line_spec('Year'),
        title = list(
          text = paste0('<b>',
                        input$region_tab_rate_type_selected,
                        " Over Time </b>"),
          y = 0.92,
          font = list(size = 16)
        ),
        margin = list(t = 80, b = 50),
        legend = list(title = list(text = 'Disease')),
        shapes = list(vline(2001))
      )  |>
      event_register('plotly_hover')
      
  })
  
  # Update the vertical line in the line graph with year input
  observe({
    p <- plotlyProxy("region_tab_line_chart", session)
    
    p |>
      plotlyProxyInvoke("relayout",
                        list(
                          shapes = list(vline(input$region_tab_year_range_selected))
                        ))
  })
   
  # Modify line chart's yaxis to start from 0 when user toggles the switch
  observeEvent(input$region_tab_line_y0switch, {
    p <- plotlyProxy("region_tab_line_chart", session)
    
    if (input$region_tab_line_y0switch == TRUE) {
      p |>
        plotlyProxyInvoke("relayout",
                          list(
                            yaxis = y_axis_spec(input$region_tab_rate_type_selected,"tozero")
                          ))
    } else {
      p %>%
        plotlyProxyInvoke("relayout",
                          list(
                            yaxis = y_axis_spec(input$region_tab_rate_type_selected,"nonnegative")
                          ))
      
    }
  })
  
  # Plot a bar chart comparing rates for diseases in a year
  # x is DISEASE, y is the selected rate type, color is DISEASE
   
  output$region_tab_bar_chart <- renderPlotly({
    bar_chart_data <- region_tab_filtered_data() |>
      filter(YEAR == input$region_tab_year_range_selected)
    error$lower <-
      paste0(sub("\\_.*", "", region_tab_rate_as_variable()), "_LCL_95")
    error$upper <-
      paste0(sub("\\_.*", "", region_tab_rate_as_variable()), "_UCL_95")
    
    bar_chart_data |>
      plot_ly(
        x = bar_chart_data$DISEASE,
        y = bar_chart_data[[region_tab_rate_as_variable()]],
        color = ~ DISEASE,
        type = 'bar',
        error_y = list(
          type = "data",
          symmetric = FALSE,
          arrayminus = bar_chart_data[[region_tab_rate_as_variable()]] - bar_chart_data[[error$lower]],
          array = bar_chart_data[[error$upper]] - bar_chart_data[[region_tab_rate_as_variable()]],
          color = '#000000',
          width = 10
        ),
        hovertemplate = paste('<br><b>Disease</b>: %{fullData.name}',
                              '<br><b>Year</b>: ', input$region_tab_year_range_selected,
                              '<br><b>%{yaxis.title.text}</b>: %{y:.2f}',
                              '<br><b>95% Confidence Interval</b>: (',
                              format_round(bar_chart_data[[error$lower]]), ',',
                              format_round(bar_chart_data[[error$upper]]),')',
                              '<extra></extra>')
      ) %>%
      layout(
        yaxis = append(c(range=list(0, max(region_tab_filtered_data()[[error$upper]]) * 1.05)),
                       y_axis_spec(input$region_tab_rate_type_selected,"tozero")),
        xaxis = x_axis_bar_spec(''),
        title = list(
          text = paste0(
                    "<b>Disease Distribution by ",
                    input$region_tab_rate_type_selected,
                    " in ",
                    input$region_tab_year_range_selected,
                    "</b>"),
          y = 0.92,
          font = list(size = 16)
        ),
        barmode = "overlay",
        margin = list(t = 80, b = 50),
        showlegend = FALSE
      )
    #   event_register('plotly_hover')
  })
  
  
  ################################
  # Download Data Tab Server Side Logic
  ################################
  
  # Reset filters
  observeEvent(input$reset_data, {
    reset("filters_data")
  })
  
  # Select dataset based on user input
  datasetInput_data <- reactive({
    dataset_switch(input$dataset_data)
  })
  
  # Select rate based on user input
  rateInput_data <- reactive({
    rate_switch(input$dataset_data)
  })
  
  # Select geography based on user input
  healthboundInput_data <- reactive ({
    switch(input$health_bound_data,
           "Health Authorities" = "HA",
           "Community Health Service Areas" = "CHSA")
  })
  
  # Dynamic UI for region selection
  output$region_data <- renderUI({
    selectInput("region_data",
                label = "Select Health Boundaries",
                choices = append("All",health_bounds(input$health_bound_data)),
                multiple = TRUE,
                selected = "All")
  })
  
  # Dynamic UI for disease selection
  output$disease_data <- renderUI({
    selectInput("disease_data", 
                label = "Select Disease(s)",
                choices = append("All", unique(datasetInput_data()$DISEASE)),
                multiple = TRUE,
                selected = "All")
  })
  
  # Filter data and reformat dataframe
  filter_df_data <- reactive({
    data <- datasetInput_data() |> 
      filter (
        (GEOGRAPHY == healthboundInput_data()) & 
          (if ("All" %in% input$region_data) TRUE else (HEALTH_BOUND_NAME %in% input$region_data)) &
          (if ("All" %in% input$disease_data)TRUE else (DISEASE %in% input$disease_data)) &
          (YEAR %in% seq(from=min(input$year_range_data),to=max(input$year_range_data))) &
          (CLNT_GENDER_LABEL == substr(input$gender_data,1,1)))|>
      mutate(CRUDE_CI=paste0("(",CRUDE_LCL_95,",",CRUDE_UCL_95,")"),
             STD_CI=paste0("(",STD_LCL_95,",",STD_UCL_95,")"),
             YEAR = paste0(YEAR,"/",YEAR+1))|>
      rename(SEX =CLNT_GENDER_LABEL,
             HEALTH_BOUNDARY=HEALTH_BOUND_NAME,
             FISCAL_YEAR = YEAR)|>
      select(FISCAL_YEAR,DISEASE,SEX,GEOGRAPHY,HEALTH_BOUNDARY,NUMERATOR,DENOMINATOR,
             CRUDE_RATE_PER_1000, CRUDE_CI,CRUDE_VARIANCE,STD_RATE_PER_1000,
             STD_CI,STD_VARIANCE)
    
    names(data)<-snakecase::to_title_case(names(data))
    
    data|>
      rename("Crude 95% CI" = "Crude Ci",
             "Standardized 95% CI" = "Std Ci")
    
  })
  
  # Render download data button
  output$download_data <- downloadHandler(
    filename = function() {
      paste("BC_Chronic_Disease_Data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filter_df_data(), file)
    }
  )

  # Render data table  
  output$data_table <- renderDT(filter_df_data()|>
                                  select(-"Crude Variance",-"Std Variance"),
                                rownames= FALSE,
                                options = list(
                                  scrollX = TRUE, 
                                  scrollY = "520px",
                                  autoWidth = TRUE,
                                  columnDefs = list(list(width = '150px', targets = c(1)),
                                                    list(className = 'dt-center', targets = "_all"),
                                                    list(width = '100px', targets = c(7,9))
                                                    )
                                  ))
  
}
    
    ################################
    # Mahmood Tab Server Side Logic
    ################################


################################
# Run App
################################
shinyApp(ui, server)