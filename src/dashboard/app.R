################################
# Load packages
# ################################
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

################################
# Source helper functions
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
  # theme = shinytheme("sandstone"),
  theme = "mytheme.css",
  # theme = bs_theme(bootswatch = "minty"),
  # includeCSS("www/mytheme.css"),
  shinyjs::useShinyjs(),
  leafletjs,
  tab_colsjs,
  id="body",
  list(tags$head(HTML('<link rel="icon", href="bc-gov-logo.png", type="image/png" />'))),
  navbarPage(title = div(img(src="bc-gov-logo.png"),"BC Chronic Disease Dashboard"),
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
                 hr(),
                 helpText(HTML("For internal use only. Do not distribute.<br/>
                               For questions about this dashboard, please contact hlth.cdrwg@gov.bc.ca"))
                 ),
        tabPanel("Rate Types",
                 p(HTML("<u><h2>Rate Types</h2></u></br>")),
                 p(HTML(rate_info))
                 ),
        tabPanel("Diseases",
                 position = c("fixed-top"),
                 p(HTML("<u><h2>Diseases</h2></u></br>")),
                 p(HTML(disease_info))
                 ),
        tabPanel("Data Dictionary",
                 p(HTML("<u><h2>Data Dictionary</h2></u></br>")),
                 p(HTML(""))
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
                               sep = "",
                               ticks = TRUE,
                               animate = animationOptions(interval = 1000)
                              ),
        
                   actionButton("reset_d", "Reset")
                    
               ),
               mainPanel(
                 width = 9,
                 fluidRow(
                   column(6, leafletOutput("map",height = 700)%>% withSpinner(),
                          verbatimTextOutput("hover_stuff"),
                          verbatimTextOutput("hover_stuff2")
                          ),
                   column(6, 
                          br(),
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
                   fluidRow(plotlyOutput("region_tab_line_chart") %>% withSpinner()),
                   br(),
                   fluidRow(
                     column(8, plotlyOutput("region_tab_bar_chart") %>% withSpinner()),
                     br(),
                     column(4, leafletOutput("region_tab_map") %>% withSpinner())))
                 
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
                   h2("Filters"),
                   hr(style = "border-top: 1px solid #000000"),
                   
                   selectInput("dataset_data", 
                               label = "Select Rate Type",
                               choices = RATE_TYPE_CHOICES,
                               selected = RATE_TYPE_CHOICES[1]),
                   
                   uiOutput("disease_data"),
                   
                   radioButtons("health_bound_data",
                                label= "Select Geography",
                                choices = GEOGRAPHY_CHOICES,
                                selected=GEOGRAPHY_CHOICES[1]),
                   
                   uiOutput("region_data"),
                   
                   sliderInput("year_range_data", 
                               label = "Select Year Range",
                               min = 2001, max = 2020, value = c(2001, 2020),
                               sep = ""),
                   
                   radioButtons("gender_data", 
                                label = ("Select Sex"),
                                choices = c("Male","Female","Total"), 
                                selected = "Total",
                                inline = TRUE),
                   
                   actionButton("reset_data", "Reset")
                 ),
                 mainPanel(
                   width = 9,
                   downloadButton("download_data", label = "Download Data"),
                   hr(),
                   div(style = "overflow-x:scroll;max-height: 80vh;overflow-y:scroll",
                   dataTableOutput("data_table"))
                 )
               ))
     
  )
)

################################
# Server Side Logic
################################
server <- function(input, output,session) {
  
  observeEvent(input$navbarID, {
    if(input$navbarID %in% c("About","Rate Types","Diseases","Data Dictionary")){
      session$sendCustomMessage("background-color", "white")
    } else {
      session$sendCustomMessage("background-color", "#cccccc")
    }
  })
  
  ################################
  # By Disease Tab Server Side Logic
  ################################
  
  observeEvent(input$reset_d, {
    reset("filters_d")
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
                selected = "Crude Incidence Rate",
                multiple = FALSE,
    )
  })
  
  output$region_d <- renderUI({
    selectInput("region_d",
                label = "Select Health Region(s)",
                choices = (
                  if(input$health_bound_d == "Health Authorities") 
                    c(sort(unique(filter(inc_rate_df,GEOGRAPHY=="HA")$HEALTH_BOUND_NAME)))
                  else 
                    c(sort(unique(filter(inc_rate_df,GEOGRAPHY=="CHSA")$HEALTH_BOUND_NAME)))),
                multiple = TRUE,
                selected = (
                  if(input$health_bound_d == "Health Authorities") HA_CHOICES
                  else c("100 Mile House","Abbotsford Rural","Agassiz/Harrison","Alberni Valley/Bamfield")
                ))
  })
  
  datasetInput_d <- reactive({
    shiny::validate(need(input$dataset_d, message=F))
    if(!is.null(input$dataset_d)){
    switch(input$dataset_d,
           "Crude Incidence Rate" = inc_rate_df,
           "Age Standardized Incidence Rate" = inc_rate_df,
           "Crude Life Prevalence" = life_prev_df,
           "Age Standardized Life Prevalence" = life_prev_df,
           "Crude HSC Prevalence" = hsc_prev_df,
           "Age Standardized HSC Prevalence" = hsc_prev_df)
    }
  })
  
  rateInput_d <- reactive({
    shiny::validate(need(input$dataset_d, message=F))
    if(!is.null(input$dataset_d)){
    switch(input$dataset_d,
           "Crude Incidence Rate" = "CRUDE_RATE_PER_1000",
           "Age Standardized Incidence Rate" = "STD_RATE_PER_1000",
           "Crude Life Prevalence" = "CRUDE_RATE_PER_1000",
           "Age Standardized Life Prevalence" = "STD_RATE_PER_1000",
           "Crude HSC Prevalence" = "CRUDE_RATE_PER_1000",
           "Age Standardized HSC Prevalence" = "STD_RATE_PER_1000")
    }
  })
  
  healthboundInput_d <- reactive ({
    shiny::validate(need(input$health_bound_d, message=F))
    if(!is.null(input$health_bound_d)){
    switch(input$health_bound_d,
           "Health Authorities" = "HA",
           "Community Health Service Areas" = "CHSA")
    }
  })
  
  spdf_d <- reactive ({
    shiny::validate(need(input$health_bound_d, message=F))
    if(!is.null(input$health_bound_d)){
    switch(input$health_bound_d,
           "Health Authorities" = ha_spdf,
           "Community Health Service Areas" = chsa_spdf)
    }
  })
  
  
  filter_df_d <- reactive({
    datasetInput_d() |> 
      filter ((GEOGRAPHY == healthboundInput_d())&
                (DISEASE == input$disease_d) &
                (CLNT_GENDER_LABEL == substr(input$gender_d,1,1))
      )
  })
  
  error <- reactiveValues(
    lower = NULL,
    upper = NULL
  )
  
  # Render bar graph for each rate/disease 
  output$disease_graph_bar <- renderPlotly({
    
    # dummyData <- datasetInput_d() |>
    #   filter(CLNT_GENDER_LABEL=='T',
    #          GEOGRAPHY =="HA",
    #          DISEASE == "Acute Myocardial Infarction",
    #          YEAR==2001)
    
    dummyData <- filter_df_d()|>
      filter(HEALTH_BOUND_NAME %in% input$region_d,
             YEAR == 2001
             )
    error$lower <- paste0(sub("\\_.*", "", rateInput_d()),"_LCL_95")
    error$upper <- paste0(sub("\\_.*", "", rateInput_d()),"_UCL_95")

      plot_ly(data=dummyData,
              x=dummyData$HEALTH_BOUND_NAME,
              y=dummyData[[rateInput_d()]],
              source = "disease_graph_bar",
              type = 'bar',
              error_y=list(
                type = "data",
                symmetric = FALSE,
                arrayminus =dummyData[[rateInput_d()]]- dummyData[[error$lower]],
                array = dummyData[[error$upper]]- dummyData[[rateInput_d()]],
                color = '#000000',
                width = 10),
              marker = list(color = HA_colours$Colors[match(dummyData$HEALTH_BOUND_NAME,HA_colours$Regions)]),
              # hovertemplate = paste('<b>Health Region</b>: %{x}',
              #                       '<br><b>%{yaxis.title.text}</b>: %{y:.2f}<br>',
              #                       '<b>Year</b>:', input$year_d,
              #                       '<extra></extra>'
              #                       )
              hoverinfo="skip"
              )%>%
      layout(yaxis=list(range=list(0,max(filter(filter_df_d(),HEALTH_BOUND_NAME %in% input$region_d)[[rateInput_d()]])*1.1),
                        title = paste0(input$dataset_d," Per 1000"),
                        gridcolor = "#d9dadb",
                        showline= T, linewidth=1, linecolor='black'),
             xaxis = list(title = list(text = 'Health Region', standoff = 15),
                          categoryorder = "category ascending",
                          showline= T, linewidth=1, linecolor='black'),
             title = list(text = paste0('<b>',input$dataset_d," of \n",input$disease_d, " in 2001 </b>"),
                          font = list(size = 16)),
             barmode = "overlay",
             margin = list(t = 50)
             # plot_bgcolor= '#d9dadb'
             # showlegend = FALSE
            ) %>%
      event_register('plotly_hover')
    
    # d<-filter_df_d()|>
    #       filter((YEAR == input$year_d)&
    #                (if ("All" %in% input$region_d) TRUE else (HEALTH_BOUND_NAME %in% input$region_d))) |>
    #       highlight_key(~HEALTH_BOUND_NAME)
    # 
    # p <- dummyData |>
    #   ggplot(aes_string(x="HEALTH_BOUND_NAME",y= "CRUDE_RATE_PER_1000",fill="HEALTH_BOUND_NAME"
    #                     # frame = "YEAR"
    #                     ))+
    #   geom_col(position = "identity")+
    #   # scale_fill_manual("legend", values = c("A" = "black", "B" = "orange", "C" = "blue"))+
    #   labs(x="Health Region",
    #        y=paste0(input$dataset_d," Per 1000"),
    #        title = paste0(input$dataset_d," of "," in 2001" ))+
    #   theme(
    #     # legend.position="none",
    #         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    #         plot.title = element_text(size=12),
    #         axis.title.x = element_text(size=10),
    #         axis.title.y = element_text(size=10))
    # ggplotly(p,source = "disease_graph_bar",tooltip=c("YEAR","HEALTH_BOUND_NAME"))|>
    #   event_register('plotly_hover')
  })
  
  # Update Disease Bar Graph with filter changes
  observe({
    
    newdata <- filter_df_d()|>
      filter((YEAR == input$year_d)&
               ((HEALTH_BOUND_NAME %in% input$region_d)))
    
    error$lower <- paste0(sub("\\_.*", "", rateInput_d()),"_LCL_95")
    error$upper <- paste0(sub("\\_.*", "", rateInput_d()),"_UCL_95") 
    
    p <- plotlyProxy("disease_graph_bar", session)
    
    p %>%
      plotlyProxyInvoke("restyle",
                        list(
                          x = list(newdata$HEALTH_BOUND_NAME),
                          y= list(newdata[[rateInput_d()]]),
                          error_y=list(
                            type = "data",
                            symmetric = FALSE,
                            arrayminus =newdata[[rateInput_d()]]- newdata[[error$lower]],
                            array = newdata[[error$upper]]- newdata[[rateInput_d()]],
                            color = '#000000',
                            width = 10),
                          marker = list(color = HA_colours$Colors[match(newdata$HEALTH_BOUND_NAME,HA_colours$Regions)])
                        ))|>
      plotlyProxyInvoke("relayout",
                        list(
                          autosize = F,
                          yaxis=list(range=list(0,max(filter(filter_df_d(),HEALTH_BOUND_NAME %in% input$region_d)[[rateInput_d()]])*1.1),
                                     title = paste0(input$dataset_d," Per 1000"),
                                     gridcolor = "#d9dadb",
                                     showline= T, linewidth=1, linecolor='black'),
                          xaxis=list(fixedrange = TRUE,
                                     title = list(text = 'Health Region', standoff = 15),
                                     categoryorder = "category ascending",
                                     automargin = TRUE,
                                     showline= T, linewidth=1, linecolor='black'),
                          title = list(text = HTML(paste0('<b>',input$dataset_d," of<br>",input$disease_d, " in ",input$year_d, "</b>")),
                                       font = list(size = 16))
                        ))
    
  })
  
  # Render line graph 
  output$disease_graph_line <- renderPlotly({
    
    d <- filter_df_d()|>
      filter(HEALTH_BOUND_NAME %in% input$region_d)
    
    d |>
    highlight_key(~HEALTH_BOUND_NAME)|>
    plot_ly(
      x= d$YEAR,
      y=d[[rateInput_d()]],
      source = "disease_graph_line",
      type = "scatter",
      mode="lines",
      line = list(width=2),
      color = ~HEALTH_BOUND_NAME,
      colors = setNames(HA_colours$Colors,HA_colours$Regions),
      hovertemplate = paste0('<b>Health Region</b>: %{fullData.name}',
                            '<br><b>%{yaxis.title.text}</b>: %{y:.2f}',
                            '<br><b>Year</b>: %{x}',
                            '<extra></extra>'
                            )
    )%>%
      layout(yaxis=list(title = paste0(input$dataset_d," Per 1000"),
                        gridcolor = "#d9dadb",
                        showline= T, linewidth=1, linecolor='black'),
             xaxis = list(title = 'Year',
                          gridcolor = "#d9dadb",
                          showline= T, linewidth=1, linecolor='black'),
             title = list(text = paste0('<b>',input$dataset_d," of  \n",input$disease_d, " Over Time </b>"),
                          font = list(size = 16)),
             margin = list(t = 50),
             legend=list(title=list(text='Health Region'))
             # plot_bgcolor= '#d9dadb'
             # hovermode = "x unified"
             # showlegend = FALSE
      ) %>%
      event_register('plotly_hover')
    
    # p2<- d_hl|>
    #   ggplot( aes_string(y=rateInput_d(),x="YEAR",color = "HEALTH_BOUND_NAME",group = "HEALTH_BOUND_NAME"
    #                     # text = paste(
    #                     #   input$dataset_d, rateInput_d(), "\n",
    #                     #   "Year: ", "YEAR", "\n",
    #                     #   sep = ""
    #                     # )
    # ))+
    #   geom_line(stat="identity")+
    #   scale_color_manual(values= setNames(HA_colours$Colors,HA_colours$Regions))+
    #   labs(y=paste0(input$dataset_d, " Per 1000"),
    #        x="Year",
    #        title = paste0(input$dataset_d," of ",input$disease_d," Over Time"),
    #        col = "Health Region")+
    #   theme(plot.title = element_text(size=12),
    #         axis.title.x = element_text(size=10),
    #         axis.title.y = element_text(size=10))
    # ggplotly(p2, source = "disease_graph_line",tooltip=c("YEAR",rateInput_d(),"HEALTH_BOUND_NAME"))|>
    #   event_register('plotly_hover')

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
    
    legend_inc <- round_any(unname(quantile(dummyData[[rateInput_d()]],0.8))/5,0.1)
    mybins <- append(seq(round_any(min(dummyData[[rateInput_d()]]),0.05, f = floor),by=legend_inc,length.out=5),Inf)
    mypalette <- colorBin( palette="YlOrBr", domain=dummy_spdf@data[[rateInput_d()]], bins=mybins,na.color="#d9dadb")
    
    mytext <- paste(
      "<b>CHSA</b>: ",(if(input$health_bound_d == "Health Authorities")"N/A" else dummy_spdf@data$CHSA_Name),"<br/>",
      "<b>HA</b>: ", dummy_spdf@data$HA_Name, "<br/>",
      paste0(input$dataset_d,":"), format(round(dummy_spdf@data[[rateInput_d()]],1),1),
      sep="") |>
      lapply(htmltools::HTML)
    
    m<-leaflet(dummy_spdf) %>% 
      setView( lat=55, lng=-127 , zoom=4.5) %>%
      addPolygons( 
        layerId = (if(input$health_bound_d == "Health Authorities") ~HA_Name else ~CHSA_Name),
        fillColor = ~mypalette(dummy_spdf@data[[rateInput_d()]]), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="gray", 
        weight=1,
        label = mytext,
        highlight = highlightOptions(
          weight = 2,
          color = "black",
          opacity = 1.0),
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        ),
      ) |>
      addLegend( pal=mypalette, values=dummy_spdf@data[[rateInput_d()]], opacity=0.9, 
                 title = paste0(input$dataset_d," Per 1000"), position = "bottomleft" )
    m
    
  })
  
#Update map with filter changes
  observe({
  
    year_filtered_map_df <- filter(filter_df_d(),YEAR == input$year_d)
    
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
        "<b>",input$dataset_d,": ","</b>",format(round(current_map_spdf@data[[rateInput_d()]],1),1),"<br/>",
        "<b>Confidence Interval</b>: (",current_map_spdf@data[[error$lower]],",",current_map_spdf@data[[error$upper]],")")
    }else{
      current_map_spdf@data$text <- paste0(
        "<b>CHSA</b>: ", current_map_spdf@data$CHSA_Name,"<br/>",
        "<b>HA</b>: ", current_map_spdf@data$HA_Name, "<br/>",
        "<b>",input$dataset_d,": ","</b>", format(round(current_map_spdf@data[[rateInput_d()]],1),1),"<br/>",
        "<b>Confidence Interval</b>: (",current_map_spdf@data[[error$lower]],",",current_map_spdf@data[[error$upper]],")")
  }

    legend_inc <- round_any(unname(quantile(filter_df_d()[[rateInput_d()]],0.8))/5,ifelse(max(filter_df_d()[[rateInput_d()]])<1,0.005,0.1))
    mybins <- append(seq(round_any(min(filter_df_d()[[rateInput_d()]]),0.05, f=floor),by=legend_inc,length.out=5),Inf)
    mypalette <- colorBin( palette="YlOrBr", domain=current_map_spdf@data[[rateInput_d()]], bins=mybins, na.color="#d9dadb")

    leafletProxy("map",data = current_map_spdf) %>%
      clearMarkers() %>%
      clearControls()%>%
      addLegend( pal=mypalette, values=current_map_spdf@data[[rateInput_d()]], opacity=0.9,
                 title = paste0(input$dataset_d," Per 1000"), position = "bottomleft" )%>%
      setShapeStyle(layerId = (if(input$health_bound_d == "Health Authorities") ~HA_Name else ~CHSA_Name),
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
    bar_data<- filter(filter_df_d(),YEAR == input$year_d)
    ppl <-  plotlyProxy("disease_graph_line", session)
    ppb <- plotlyProxy("disease_graph_bar", session)
    rv_location$id <- event_info$id
    rv_location$lat <- event_info$lat
    rv_location$lng <- event_info$lng
    if ((event_info$id %in% input$region_d)){
    ppl%>%
      plotlyProxyInvoke(
        method = "restyle",
        list(line = list(width = 0.5),
             color = list(~HEALTH_BOUND_NAME),
             colors = list(setNames(HA_colours$Colors,HA_colours$Regions)))
      ) %>%
      plotlyProxyInvoke(
        method = "restyle",
        "line",
        list(width = 3),
        as.integer(match(event_info$id,
                         my_traces())-1)
      )
      ppb %>%
        plotlyProxyInvoke(
          method = "restyle",
          list(opacity=0.2)
        ) %>%
        plotlyProxyInvoke(
          method = "addTraces",
          list(
            x=list(event_info$id),
            y=list(bar_data[[rateInput_d()]][match(event_info$id,bar_data$HEALTH_BOUND_NAME)]),
            error_y=list(
              type = "data",
              symmetric = FALSE,
              arrayminus = list(bar_data[[rateInput_d()]][match(event_info$id,bar_data$HEALTH_BOUND_NAME)]- bar_data[[error$lower]][match(event_info$id,bar_data$HEALTH_BOUND_NAME)]),
              array = list(bar_data[[error$upper]][match(event_info$id,bar_data$HEALTH_BOUND_NAME)]- bar_data[[rateInput_d()]][match(event_info$id,bar_data$HEALTH_BOUND_NAME)]),
              color = '#000000',
              width = 10),
            type='bar',
            marker = list(opacity = 1,
                          color = HA_colours$Colors[match(event_info$id,HA_colours$Regions)])
          ))
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
      plotlyProxyInvoke(ppl,method = "restyle",list(line = list(width = 2)))
      plotlyProxyInvoke(ppb, "deleteTraces",list(as.integer(1)))%>%
        plotlyProxyInvoke(method = "restyle",list(opacity = 1))

    }else{
      rv_location_move_old$lat <- event_info$lat
      rv_location_move_old$lng <- event_info$lng
    }
  })
  
  # TEST
  output$hover_stuff <- renderPrint({
    input$navbarID

  })
  
  output$hover_stuff2 <- renderPrint({
    event_data("plotly_hover",source = "disease_graph_line")
  })
  

  my_traces <- reactive({
    sort(input$region_d)
  })
  
  ## Link highlighting when hovering on bar graph
  observe({
      event <- event_data("plotly_hover",source = "disease_graph_bar")
      error$lower <- paste0(sub("\\_.*", "", rateInput_d()),"_LCL_95")
      error$upper <- paste0(sub("\\_.*", "", rateInput_d()),"_UCL_95")
      bar_data<- filter(filter_df_d(),YEAR == input$year_d)
      ppl <-plotlyProxy("disease_graph_line", session)
      ppb <- plotlyProxy("disease_graph_bar", session)
      lp <- leafletProxy("map",session)
      if (is.null(event)){
        ppl %>% plotlyProxyInvoke(method="restyle",list(line = list(width=2)))
        ppb %>% plotlyProxyInvoke("deleteTraces",list(as.integer(1)))%>%
                plotlyProxyInvoke(method = "restyle",list(opacity = 1))   
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
          list(width = 3),
          as.integer(match(event[["x"]],my_traces())-1)
        )
        ppb %>%
          plotlyProxyInvoke(
            method = "restyle",
            list(opacity=0.2)
          ) %>%
          plotlyProxyInvoke(
            method = "addTraces",
            list(
              x=list(event[["x"]]),
              y=list(event[["y"]]),
              type='bar',
              error_y=list(
                type = "data",
                symmetric = FALSE,
                arrayminus = list(event[["y"]]- bar_data[[error$lower]][match(event[["x"]],bar_data$HEALTH_BOUND_NAME)]),
                array = list(bar_data[[error$upper]][match(event[["x"]],bar_data$HEALTH_BOUND_NAME)]- event[["y"]]),
                color = '#000000',
                width = 10),
              marker = list(opacity = 1,
                            color = HA_colours$Colors[match(event[["x"]],HA_colours$Regions)]),
              hovertemplate = paste('<b>Health Region</b>: %{x}',
                                    '<b>Year</b>: ',input$year_d,
                                    '<br><b>%{yaxis.title.text}</b>: %{y:.2f}<br>',
                                    '<b>Confidence Interval</b>: (',bar_data[[error$lower]][match(event[["x"]],bar_data$HEALTH_BOUND_NAME)], ',',
                                                                    bar_data[[error$upper]][match(event[["x"]],bar_data$HEALTH_BOUND_NAME)],')',
                                    '<extra></extra>')
            )
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
    event <- event_data("plotly_hover",source = "disease_graph_line")
    error$lower <- paste0(sub("\\_.*", "", rateInput_d()),"_LCL_95")
    error$upper <- paste0(sub("\\_.*", "", rateInput_d()),"_UCL_95")
    bar_data<- filter(filter_df_d(),YEAR == input$year_d)
    ppl <-plotlyProxy("disease_graph_line", session)
    ppb <- plotlyProxy("disease_graph_bar", session)
    lp <- leafletProxy("map",session)
    if (is.null(event)){
      ppl %>% plotlyProxyInvoke(method="restyle",list(line = list(width=2)))
      ppb %>% plotlyProxyInvoke("deleteTraces",list(as.integer(1)))%>%
              plotlyProxyInvoke(method = "restyle",list(opacity = 1)) 
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
          list(width = 3),
          as.integer(match(event[["key"]],my_traces())-1)
        )
      
      ppb %>%
        plotlyProxyInvoke(
          method = "restyle",
          list(opacity=0.2)
        ) %>%
        plotlyProxyInvoke(
          method = "addTraces",
          list(
            x=list(event[["key"]]),
            y=list(bar_data[[rateInput_d()]][match(event[["key"]],bar_data$HEALTH_BOUND_NAME)]),
            error_y=list(
              type = "data",
              symmetric = FALSE,
              arrayminus = list(bar_data[[rateInput_d()]][match(event[["key"]],bar_data$HEALTH_BOUND_NAME)]- bar_data[[error$lower]][match(event[["key"]],bar_data$HEALTH_BOUND_NAME)]),
              array = list(bar_data[[error$upper]][match(event[["key"]],bar_data$HEALTH_BOUND_NAME)]- bar_data[[rateInput_d()]][match(event[["key"]],bar_data$HEALTH_BOUND_NAME)]),
              color = '#000000',
              width = 10),
            type='bar',
            marker = list(opacity = 1,
                          color = HA_colours$Colors[match(event[["key"]],HA_colours$Regions)])
          ))
      lp %>%
        addPolygons(
          data=subset(spdf_d(),
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
        y = paste0(input$region_tab_rate_type_selected, " Per 1000"),
        x = NULL,
        legend = "Disease",
        title = paste0(input$region_tab_rate_type_selected, " Over Time")
      ) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
      scale_x_continuous(breaks = breaks_width(1))
    
    ggplotly(line_chart)
  })
  
  # plot a bar chart showing the distribution of cumulative rates for diseases
  output$region_tab_bar_chart <- renderPlotly({
    bar_chart <- region_tab_filtered_data() |>
      filter(YEAR == input$region_tab_year_range_selected[1]) |>
      ggplot(aes_string(x = "DISEASE", y = region_tab_rate_as_variable(),
                        fill = "DISEASE")) +
      geom_bar(stat = "identity") +
      labs(
        y = paste0(input$region_tab_rate_type_selected, " Per 1000"),
        x = NULL,
        title = paste0(
          "Distribution of Diseases by ",
          input$region_tab_rate_type_selected,
          " in ",
          input$region_tab_year_range_selected[1]
        )
      ) + theme(plot.title = element_text(size = 8),
                legend.position = "none") +
      scale_x_discrete(labels = wrap_format(10))
    
    ggplotly(bar_chart)
  })
  
  # a map highlighting the selected health region to provide context
  region_tab_map_data <- reactive({
    
    if (input$region_tab_geography_selected == "Health Authorities") {
      region_level <- ha_spdf
      region_index <- chmatch(input$region_tab_region_selected, ha_spdf$HA_Name)
      selected_region_lat <- ha_spdf$Latitude[region_index]
      selected_region_lon <- ha_spdf$Longitude[region_index]
    }
    else {
      region_level <- chsa_spdf
      region_index <- chmatch(input$region_tab_region_selected, chsa_spdf$CHSA_Name)
      selected_region_lat <- chsa_spdf$Latitude[region_index]
      selected_region_lon <- chsa_spdf$Longitude[region_index]
    }
    to_return <- list(region_level = region_level,
                      selected_region_lat = selected_region_lat, 
                      selected_region_lon = selected_region_lon)
    to_return
  })
  
  
  output$region_tab_map <- renderLeaflet({
    location_to_be_tagged <- region_tab_map_data()
    map <- leaflet(location_to_be_tagged$region_level) |> 
      addTiles() |> 
      addPolygons(weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE)) |>
      addMarkers(lng = location_to_be_tagged$selected_region_lon,
                 lat = location_to_be_tagged$selected_region_lat,
                 label = input$region_tab_region_selected)
    map
    })
  
  
  ################################
  # Download Data Tab Server Side Logic
  ################################
  
  observeEvent(input$reset_data, {
    reset("filters_data")
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