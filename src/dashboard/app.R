################################
# Load packages
# ################################
library(shiny)
library(shinyjs)
library(tidyverse)
library(leaflet)
library(rgdal)
library(shinythemes)
library(plotly)
library(scales) # used to format the axis values
library(shinycssloaders)
library(crosstalk)


################################
# Source helper functions
# 
# Define and load all the global variables, including the data frames and shape files
################################
source('global.R', local = T) 
options(spinner.color="#003366")

################################
# UI Side Logic
# 
# Define the ui of the four main tabs of the dashboard from left to right
################################
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  includeCSS("www/mytheme.css"), 
  shinyjs::useShinyjs(),
  
  navbarPage("BC Chronic Disease Dashboard",
      
      ################################
      # Information Tab UI Side Logic
      ################################
      navbarMenu("Information",
        tabPanel("About", 
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
                               </ul>"))),
           tabPanel("Rate Types",
                    helpText(HTML(
                               "The definitions of the rate types are provided below.
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
        tabPanel("Diseases")),
      
      ################################
      # "By Disease" Tab UI Side Logic
      ################################
      tabPanel("By Disease",
               sidebarLayout(
                 sidebarPanel(
                   id="filters",
                   width = 3,
                   h2("Filters"),
                   hr(style = "border-top: 1px solid #000000"),
  
                   selectInput("disease_d",
                               label= "Select Disease",
                               choices = sort(unique(inc_rate_df$DISEASE))),
                   
                   uiOutput("dataset_d"),
                   
                   radioButtons("health_bound_d",
                                label= "Select Geography",
                                choices = GEOGRAPHY_CHOICES,
                                selected= GEOGRAPHY_CHOICES[1]),
                   
                   uiOutput("region_d"),
                   
                   radioButtons("gender_d", 
                                label = ("Select Sex"),
                                choices = c("Male","Female","Total"), 
                                selected = "Total",
                                inline=TRUE),
                
                   sliderInput("year_d", 
                               label = "Select Fiscal Year",
                               min = 2001,
                               max=2020,
                               value = 2001,
                               animate = TRUE
                              ),
        
                   actionButton("resetAll", "Reset")
                    
               ),
               mainPanel(
                 width = 9,
                 fluidRow(
                   column(6, leafletOutput("map",height = 700)%>% withSpinner(),
                          verbatimTextOutput("hover_stuff"),
                          verbatimTextOutput("hover_stuff2")),
                   column(6, 
                          fluidRow(column(12,plotlyOutput("disease_graph_bar",height=350)%>% withSpinner())),
                          br(),br(),
                          fluidRow(column(12,plotlyOutput("disease_graph_line",height=350)%>% withSpinner())),
                          )))
               )),
      
      ################################
      # "By Region" Tab UI Side Logic
      ################################
      tabPanel("By Region",
               sidebarLayout(
                 
                 sidebarPanel(
                   width = 3,
                   h2("Filters"),
                   hr(style = "border-top: 1px solid #000000"),
                   
                   radioButtons(
                     "region_tab_geography_selected",
                     label = "Select Geography",
                     choices = GEOGRAPHY_CHOICES,
                     selected = GEOGRAPHY_CHOICES[1]
                   ),
                   
                   uiOutput("region_tab_region_selected"),
                   
                   selectInput(
                     "region_tab_rate_type_selected",
                     label = "Select Rate Type",
                     choices = RATE_TYPE_CHOICES
                   ),
                   
                   uiOutput("region_tab_diseases_selected"),
                   
                   sliderInput(
                     "region_tab_year_range_selected",
                     label = "Select Year Range",
                     min = 2001,
                     max = 2020,
                     value = c(2001, 2020),
                     step = 1,
                     sep = ""
                   ),
                   
                   radioButtons(
                     "region_tab_sex_selected",
                     label = ("Select Sex"),
                     choices = c("Male", "Female", "Total"),
                     selected = "Total",
                     inline = TRUE
                   ),
                 ),
                 
                 mainPanel(
                   width = 9,
                   fluidRow(plotlyOutput(
                   "region_tab_line_chart"
                   )),
                   fluidRow(plotlyOutput(
                     "region_tab_bar_chart"
                   )))
               )), 
      
      ################################
      # Download Data Tab UI Side Logic
      ################################
      tabPanel("Data",
               sidebarLayout(
                 
                 #Filters
                 sidebarPanel(
                   width = 3,
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
                   
                   radioButtons("health_bound_data",
                                label= "Select Geography",
                                choices = GEOGRAPHY_CHOICES,
                                selected=GEOGRAPHY_CHOICES[1]),
                   
                   uiOutput("region_data"),
                   
                   sliderInput("year_range_data", 
                               label = "Select Year Range",
                               min = 2001, max = 2020, value = c(2001, 2020)),
                   
                   radioButtons("gender_data", 
                                label = ("Select Sex"),
                                choices = c("Male","Female","Total"), 
                                selected = "Total",
                                inline = TRUE),
                 ),
                 mainPanel(
                   width = 9,
                   downloadButton("download_data", label = "Download Data"),
                   hr(),
                   dataTableOutput("data_table"))
               ))
               
  )
)

################################
# Server Side Logic
################################
server <- function(input, output,session) {
  
  ################################
  # By Disease Tab Server Side Logic
  ################################
  
  observeEvent(input$resetAll, {
    reset("filters")
  })

  output$dataset_d <- renderUI({
    selectInput("dataset_d", 
                label = "Select Rate Type",
                choices = (
                  if(input$disease_d %in% HSC_disease) RATE_TYPE_CHOICES
                  else
                    c("Crude Incidence Rate",
                      "Age Standardized Incidence Rate",
                      "Crude Life Prevalence",
                      "Age Standardized Life Prevalence")
                ),
                multiple = FALSE,
    )
  })
  
  datasetInput_d <- reactive({
    switch(input$dataset_d,
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
  
  healthboundInput_d <- reactive ({
    switch(input$health_bound_d,
           "Health Authorities" = "HA",
           "Community Health Service Areas" = "CHSA")
  })
  
  output$region_d <- renderUI({
    selectInput("region_d",
                label = "Select Health Region(s)",
                choices = (
                  if(input$health_bound_d == "Health Authorities") 
                    c(append("All",sort(unique(filter(inc_rate_df,GEOGRAPHY=="HA")$HEALTH_BOUND_NAME))))
                  else 
                    c(append("All",sort(unique(filter(inc_rate_df,GEOGRAPHY=="CHSA")$HEALTH_BOUND_NAME))))),
                multiple = TRUE,
                selected = (
                  if(input$health_bound_d == "Health Authorities") "All"
                  else c("100 Mile House","Abbotsford Rural")
                ))
  })
  
  
  filter_df_d <- reactive({
    datasetInput_d() |> 
      filter ((GEOGRAPHY == healthboundInput_d())&
                # (if ("All" %in% input$region_d) TRUE else (HEALTH_BOUND_NAME %in% input$region_d)) &
                (DISEASE == input$disease_d) &
                (CLNT_GENDER_LABEL == substr(input$gender_d,1,1))
      )
  })
  
  output$disease_graph_bar <- renderPlotly({
    d<-filter_df_d()|>
      filter((YEAR == input$year_d)&
             (if ("All" %in% input$region_d) TRUE else (HEALTH_BOUND_NAME %in% input$region_d))) |>
      highlight_key(~HEALTH_BOUND_NAME)
    p <- d |>
      ggplot(aes_string(x="HEALTH_BOUND_NAME",y= rateInput_d(),color = "HEALTH_BOUND_NAME",fill = "HEALTH_BOUND_NAME"))+
      geom_bar(stat='identity')+
      labs(x="Health Region",
           y=paste0(input$dataset_d," Per 1000"),
           title = paste0(input$dataset_d," of ",input$disease_d, " in ",input$year_d))+
      theme(legend.position="none",
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            plot.title = element_text(size=12),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=10))
    ggplotly(p,source = "disease_graph_bar")|>
      # highlight(on = "plotly_hover",off = "plotly_doubleclick",persistent = FALSE)|>
      event_register('plotly_hover')
  })
  
  output$disease_graph_line <- renderPlotly({
    d <- filter_df_d()|>
      filter((if ("All" %in% input$region_d) TRUE else (HEALTH_BOUND_NAME %in% input$region_d)))|>
      highlight_key(~HEALTH_BOUND_NAME)
    p2<- d|>
      ggplot( aes_string(y=rateInput_d(),x="YEAR",color = "HEALTH_BOUND_NAME",group = "HEALTH_BOUND_NAME"
                        # text = paste(
                        #   input$dataset_d, rateInput_d(), "\n",
                        #   "Year: ", "YEAR", "\n",
                        #   sep = ""
                        # )
    ))+
      geom_line(stat="identity")+
      labs(y=paste0(input$dataset_d, " Per 1000"),
           x="Year",
           title = paste0(input$dataset_d," of ",input$disease_d," Over Time"),
           col = "Health Region")+
      theme(plot.title = element_text(size=12),
            axis.title.x = element_text(size=10),
            axis.title.y = element_text(size=10))
    ggplotly(p2, source = "disease_graph_line",tooltip=c("YEAR"))|>
      # highlight( on = "plotly_hover",
      #            off = "plotly_doubleclick",
      #            persistent = FALSE,
      #            selected = attrs_selected(showlegend = FALSE))|>
      event_register('plotly_hover')
  })
  
  
  map_spdf<- reactive({
    (if(input$health_bound_d == "Health Authorities") ha_spdf else chsa_spdf)|>
      merge(filter(filter_df_d(),(YEAR == input$year_d)),
            by.x= (if(input$health_bound_d == "Health Authorities")"HA_CD" else "CHSA_CD"),
            by.y="HEALTH_BOUND_CODE")
    
  })
  
  output$map <- renderLeaflet({
    
    mybins <- bin_dict[[input$disease_d]]
    mypalette <- colorBin( palette="YlOrBr", domain=map_spdf()@data[[rateInput_d()]], na.color="transparent", bins=mybins)
    
    mytext <- paste(
      "CHSA: ",(if(input$health_bound_d == "Health Authorities")"N/A" else map_spdf()@data$CHSA_Name),"<br/>", 
      "HA: ", map_spdf()@data$HA_Name, "<br/>", 
      paste0(input$dataset_d,":"), map_spdf()@data[[rateInput_d()]], 
      sep="") |>
      lapply(htmltools::HTML)
    
    m<-leaflet(map_spdf()) %>% 
      setView( lat=55, lng=-127 , zoom=4.5) %>%
      addPolygons( 
        layerId = (if(input$health_bound_d == "Health Authorities") ~HA_Name else ~CHSA_Name),
        fillColor = ~mypalette(map_spdf()@data[[rateInput_d()]]), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="gray", 
        weight=0.3,
        label = mytext,
        highlight = highlightOptions(
          weight = 2,
          color = "black",
          opacity = 1.0),
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) |>
      addLegend( pal=mypalette, values=~map_spdf()@data[[rateInput_d()]], opacity=0.9, 
                 title = input$dataset_d, position = "bottomleft" )
    m
    
  })
  
  ## Linked highlighting when hovering on map
  observe({
    event <- input$map_shape_mouseover
    ppl <-  plotlyProxy("disease_graph_line", session) 
    ppb <- plotlyProxy("disease_graph_bar", session)
    if(is.null(event)){
      print("IS NULLL!! ")
      plotlyProxyInvoke(ppl,method = "restyle",list(line = list(width = 0.5)))
      plotlyProxyInvoke(ppb,method = "restyle",list(opacity = 1))
    }else{
   ppl%>%
      plotlyProxyInvoke(
        method = "restyle",
        list(line = list(width = 0.5))
      ) %>%
      plotlyProxyInvoke(
        method = "restyle",
        "line",
        list(width = 4),
        as.integer(match(event$id,
                         my_traces())-1)
      )
    ppb %>%
      plotlyProxyInvoke(
        method = "restyle",
        list(opacity=0.2)
      ) %>%
      plotlyProxyInvoke(
        method = "restyle",
        list(opacity=1),
        as.integer(match(event$id,my_traces())-1)
      )}
  })
  
  output$hover_stuff <- renderPrint({
    input$map_shape_mouseover$id
    # my_traces()
  })
  
  output$hover_stuff2 <- renderPrint({
    input$map_shape_mouseout$id
  })
  

  my_traces <- reactive({
    if ("All" %in% input$region_d) sort(unique(filter_df_d()$HEALTH_BOUND_NAME))
    else sort(c(input$region_d))
  })
  
  ## Linked highlighting when hovering on bar graph
  observe({
      event <- event_data("plotly_hover",source = "disease_graph_bar")
      ppl <-plotlyProxy("disease_graph_line", session)
      ppb <- plotlyProxy("disease_graph_bar", session)
      lp <- leafletProxy("map",session)
      if (is.null(event)){
        print("IS NULLL!! ")
        ppl %>% plotlyProxyInvoke(method="restyle",list(line = list(width=2)))
        ppb %>% plotlyProxyInvoke(method = "restyle",list(opacity = 1))
        lp %>% clearGroup('selected')
      }else{
       ppl %>%
        plotlyProxyInvoke(
          method = "restyle",
          list(line = list(width = 0.5))
        ) %>%
        plotlyProxyInvoke(
          method = "restyle",
          "line",
          list(width = 4),
          as.integer(match(event[["key"]],my_traces())-1)
        )
        ppb %>%
          plotlyProxyInvoke(
            method = "restyle",
            list(opacity=0.2)
          ) %>%
          plotlyProxyInvoke(
            method = "restyle",
            list(opacity=1),
            as.integer(match(event[["key"]],my_traces())-1)
          )
      lp %>%
        addPolygons(
          data=subset(map_spdf(),
                      (if(input$health_bound_d == "Health Authorities") HA_Name 
                       else CHSA_Name) 
                      == event[["key"]]),
          stroke= TRUE,
          weight = 2,
          color = "black",
          fill= NaN,
          group = "selected"
        )}
    })
  
  ## Linked highlighting when hovering on line graph
  observe({
    event <- event_data("plotly_hover",source = "disease_graph_line")
    ppl <-plotlyProxy("disease_graph_line", session)
    ppb <- plotlyProxy("disease_graph_bar", session)
    lp <- leafletProxy("map",session)
    if (is.null(event)){
      print("IS NULLL!! ")
      ppl %>% plotlyProxyInvoke(method="restyle",list(line = list(width=2)))
      ppb %>% plotlyProxyInvoke(method = "restyle",list(opacity = 1))
      lp %>% clearGroup('selected')
    }else{
      ppl %>%
        plotlyProxyInvoke(
          method = "restyle",
          list(line = list(width = 0.5))
        ) %>%
        plotlyProxyInvoke(
          method = "restyle",
          "line",
          list(width = 4),
          as.integer(match(event[["key"]],my_traces())-1)
        )
      ppb %>%
        plotlyProxyInvoke(
          method = "restyle",
          list(opacity=0.2)
        ) %>%
        plotlyProxyInvoke(
          method = "restyle",
          list(opacity=1),
          as.integer(match(event[["key"]],my_traces())-1)
        )
      lp %>%
        addPolygons(
          data=subset(map_spdf(),
                      (if(input$health_bound_d == "Health Authorities") HA_Name 
                       else CHSA_Name) 
                      == event[["key"]]),
          stroke= TRUE,
          weight = 2,
          color = "black",
          fill= NaN,
          group = "selected"
        )}
  })
  
  # observeEvent(event_data("plotly_hover",
  #                         source = "disease_graph_bar"), {
  #                           leafletProxy("map",session)%>%
  #                           
  #                         })

  # observeEvent(input$map_shape_mouseover, {
  #   rv$regions <- c(rv$regions, input$map_shape_mouseover)
  #   
  #   leafletProxy("map", session) %>%
  #     removeShape(layerId = input$map_shape_mouseover$id) %>%
  #     addPolygons(
  #       data = shared_data(d),
  #       color = "black",
  #       fillOpacity = 1,
  #       weight = 1,
  #       highlightOptions = highlightOptions(fillOpacity = 0.2),
  #       label = ~label,
  #       layerId = ~label,
  #       group = "foo"
  #     )
  # })
   
  
  ################################
  # By Region Tab Server Side Logic
  ################################
  
  region_tab_dataset_used <- reactive({
    switch(input$region_tab_rate_type_selected,
           "Crude Incidence Rate" = inc_rate_df,
           "Age Standardized Incidence Rate" = inc_rate_df,
           "Crude Life Prevalence" = life_prev_df,
           "Age Standardized Life Prevalence" = life_prev_df,
           "Crude HSC Prevalence" = hsc_prev_df,
           "Age Standardized HSC Prevalence" = hsc_prev_df)
  })
  
  region_tab_rate_as_variable <- reactive({
    switch(input$region_tab_rate_type_selected,
           "Crude Incidence Rate" = "CRUDE_RATE_PER_1000",
           "Age Standardized Incidence Rate" = "STD_RATE_PER_1000",
           "Crude Life Prevalence" = "CRUDE_RATE_PER_1000",
           "Age Standardized Life Prevalence" = "STD_RATE_PER_1000",
           "Crude HSC Prevalence" = "CRUDE_RATE_PER_1000",
           "Age Standardized HSC Prevalence" = "STD_RATE_PER_1000")
  })
  
  output$region_tab_region_selected <- renderUI({
    selectInput(
      "region_tab_region_selected",
      label = "Select Health Region",
      choices = (
        if (input$region_tab_geography_selected == "Health Authorities")
          HA_CHOICES
        else
          sort(unique(filter(inc_rate_df, GEOGRAPHY == "CHSA")$HEALTH_BOUND_NAME))
      ),
      multiple = FALSE,
      selected = "Interior"
    )
  })
  
  
  output$region_tab_diseases_selected <- renderUI({
    selectizeInput("region_tab_diseases_selected", 
                   label = "Select Disease(s)",
                choices = unique(region_tab_dataset_used()$DISEASE),
                multiple = TRUE,
                selected = unique(region_tab_dataset_used()$DISEASE)[1],
                options = list(maxItems = 5))
  }) 
  
  region_tab_filtered_data <- reactive({
    region_tab_dataset_used() |>
      filter((HEALTH_BOUND_NAME %in% input$region_tab_region_selected) &
                (if ("All" %in% input$region_tab_diseases_selected)
                  TRUE
                 else
                   (DISEASE %in% input$region_tab_diseases_selected)
                ) &
                (
                  YEAR %in% seq(input$region_tab_year_range_selected[1], input$region_tab_year_range_selected[2], by = 1)
                 ) &
                (CLNT_GENDER_LABEL == substr(input$region_tab_sex_selected, 1, 1))
                )
  })
  
  # plot the line chart showing trends of diseases over time
  output$region_tab_line_chart <- renderPlotly({
    line_chart <- region_tab_filtered_data() |>
      ggplot(aes_string(y = region_tab_rate_as_variable(), x = "YEAR", color = "DISEASE")) +
      geom_line(stat = 'identity') +
      labs(
        y = input$region_tab_rate_type_selected,
        x = "Year",
        legend = "Disease",
        title = paste0(input$region_tab_rate_type_selected, " Over Time")
      ) +
      scale_x_continuous(breaks = breaks_width(1))
    
    ggplotly(line_chart)
  })
  
  # plot a bar chart showing the distribution of cumulative rates for diseases
  output$region_tab_bar_chart <- renderPlotly({
    bar_chart <- region_tab_filtered_data() |> 
      filter(YEAR == input$region_tab_year_range_selected[1]) |> 
      ggplot(aes_string(x = "DISEASE", y = region_tab_rate_as_variable(), fill = "DISEASE")) +
      geom_bar(stat = "identity") +
      labs(
        y = input$region_tab_rate_type_selected,
        legend = "Disease",
        title = paste0("Distribution of Diseases by ", input$region_tab_rate_type_selected,
                       " in ", input$region_tab_year_range_selected[1])
      ) + 
      scale_x_discrete(labels = wrap_format(10))
    ggplotly(bar_chart)
  })
  
  # a map highlighting the selected health region to provide context
  
  
  ################################
  # Download Data Tab Server Side Logic
  ################################
  datasetInput_data <- reactive({
    switch(input$dataset_data,
           "Crude Incidence Rate" = inc_rate_df,
           "Age Standardized Incidence Rate" = inc_rate_df,
           "Crude Life Prevalence" = life_prev_df,
           "Age Standardized Life Prevalence" = life_prev_df,
           "Crude HSC Prevalence" = hsc_prev_df,
           "Age Standardized HSC Prevalence" = hsc_prev_df)
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
  
  healthboundInput_data <- reactive ({
    switch(input$health_bound_data,
           "Health Authorities" = "HA",
           "Community Health Service Areas" = "CHSA")
  })
  
  
  output$region_data <- renderUI({
    selectInput("region_data",
                label = "Select Health Region(s)",
                choices = (
                  if(input$health_bound_data == "Health Authorities") 
                    c(append("All",sort(unique(filter(inc_rate_df,GEOGRAPHY=="HA")$HEALTH_BOUND_NAME))))
                  else 
                    c(append("All",sort(unique(filter(inc_rate_df,GEOGRAPHY=="CHSA")$HEALTH_BOUND_NAME))))),
                multiple = TRUE,
                selected = "All")
  })
  
  output$disease_data <- renderUI({
    selectInput("disease_data", 
                label = "Select Disease",
                choices = append("All", unique(datasetInput_data()$DISEASE)),
                multiple = TRUE,
                selected = "All")
  })
  
  filter_df_data <- reactive({
    datasetInput_data() |> 
      filter (
        (GEOGRAPHY == healthboundInput_data()) & 
          (if ("All" %in% input$region_data) TRUE else (HEALTH_BOUND_NAME %in% input$region_data)) &
          (if ("All" %in% input$disease_data)TRUE else (DISEASE %in% input$disease_data)) &
          (YEAR %in% seq(from=min(input$year_range_data),to=max(input$year_range_data))) &
          (CLNT_GENDER_LABEL == substr(input$gender_data,1,1)))|>
      select(-HEALTH_BOUND_CODE)
  })
  
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("BC_Chronic_Disease_Data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filter_df_data(), file)
    }
  )
  
  output$data_table <- renderDataTable(filter_df_data())
  
}

################################
# Run App
################################
shinyApp(ui, server)