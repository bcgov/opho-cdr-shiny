# Load packages ----
library(shiny)
library(tidyverse)


# Load data ----
inc_rate_df <- data.frame()
hsc_prev_df <- data.frame()
life_prev_df <- data.frame()

for (dir in list.dirs("data")[-1]){
  for (file in list.files(dir)){
    new_df <- read_csv(paste0(dir,"/",file),
                       col_select = c("DISEASE",
                                      "FISC_YR_LABEL",
                                      "CLNT_GENDER_LABEL",
                                      "HEALTH_BOUNDARIES",
                                      "CRUDE_RATE_PER_1000",
                                      "STD_RATE_PER_1000"))|>
      drop_na(CRUDE_RATE_PER_1000)
    if (dir == "data/IncidenceRate"){
      inc_rate_df <- rbind(inc_rate_df,new_df)
    }else if (dir == "data/HSCPrevalence"){
      hsc_prev_df <- rbind(hsc_prev_df,new_df)
    }else if (dir == "data/LifePrevalence"){
      life_prev_df <- rbind(life_prev_df,new_df)
    }
  }
}


inc_rate_df <- inc_rate_df |>
  separate(HEALTH_BOUNDARIES,c("HEALTH_BOUND_CODE","HEALTH_BOUND_NAME")," ", extra = "merge")|>
  mutate(YEAR = as.numeric(str_sub(FISC_YR_LABEL,4,7)))|>
  select(-FISC_YR_LABEL)

hsc_prev_df <- hsc_prev_df |>
  separate(HEALTH_BOUNDARIES,c("HEALTH_BOUND_CODE","HEALTH_BOUND_NAME")," ", extra = "merge")|>
  mutate(YEAR = as.numeric(str_sub(FISC_YR_LABEL,4,7)))|>
  select(-FISC_YR_LABEL)

life_prev_df <- life_prev_df |>
  separate(HEALTH_BOUNDARIES,c("HEALTH_BOUND_CODE","HEALTH_BOUND_NAME")," ", extra = "merge")|>
  mutate(YEAR = as.numeric(str_sub(FISC_YR_LABEL,4,7)))|>
  select(-FISC_YR_LABEL)

# Source helper functions -----



# User interface ----
# ui <- fluidPage(
#   titlePanel("Chronic Disease Dashboard"),
#   mainPanel(
#     tabsetPanel(

ui <-fluidPage(
  
  includeCSS("www/mytheme.css"),
  
  navbarPage("Chronic Disease Dashboard",
      tabPanel("Information", 
               h2("Welcome to Chronic Disease Dashboard"),
               h4("Developed by Jessie Wong, Jennifer Hoang, Mahmoodur Rahman, and Irene Yan"),
               helpText(HTML("info about dashboard <br/>Instructions and descriptions of tabs, <br/>rate definitions,
                        note about fiscal year  "))),
      tabPanel("By Disease",
               sidebarLayout(
               sidebarPanel(
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
                 
                 selectInput("region_d", 
                             label = "Select Community Health Service Area(s)",
                             choices = append("All",unique(inc_rate_df$HEALTH_BOUND_NAME)),
                             multiple = TRUE,
                             selected = "All"),
                 
                 sliderInput("year_range_d", 
                             label = "Select Year Range",
                             min = 2001, max = 2020, value = c(2001, 2020)),
                 
                 radioButtons("gender_d", 
                              label = ("Select Gender"),
                              choices = c("Male","Female","Both"), 
                              selected = "Both"),
               ),
               mainPanel(
               verbatimTextOutput("summary"),
               plotOutput("graph1"))
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
  
  output$disease_d <- renderUI({
    selectInput("disease_d", 
                label = "Select Disease",
                choices = append("All",unique(datasetInput_d()$DISEASE)),
                multiple = FALSE,
                selected = "All")
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
              (if ("All" %in% input$disease_d)TRUE else (DISEASE %in% input$disease_d)) &
              (YEAR %in% seq(from=min(input$year_range_d),to=max(input$year_range_d))) &
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
  
  output$graph1 <- renderPlot({
    filter_df_d()|>
      ggplot(aes_string(y="DISEASE",x= rateInput_d()))+
      geom_bar(stat='summary',fun=mean)+
      labs(x=rateInput_d(),
           y="Disease",
           title = paste0("Average ", input$dataset_d))
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
  
  output$map <- renderPlot({})
  
  
}

# Run app ----
shinyApp(ui, server)