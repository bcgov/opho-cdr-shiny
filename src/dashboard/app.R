# Load packages ----
library(shiny)
library(tidyverse)


# Load data ----
inc_rate_df <- data.frame()
hsc_prev_df <- data.frame()
life_prev_df <- data.frame()

for (dir in list.dirs("data")[-1]){
  for (file in list.files(dir)){
    new_df <- read_csv(paste0(dir,"/",file),col_select = c("DISEASE","FISC_YR_LABEL","CLNT_GENDER_LABEL","GEOGRAPHY","HEALTH_BOUNDARIES","CRUDE_RATE_PER_1000"))|>
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
  mutate(YEAR = as.numeric(str_sub(FISC_YR_LABEL,4,7)))

hsc_prev_df <- hsc_prev_df |>
  separate(HEALTH_BOUNDARIES,c("HEALTH_BOUND_CODE","HEALTH_BOUND_NAME")," ", extra = "merge")|>
  mutate(YEAR = as.numeric(str_sub(FISC_YR_LABEL,4,7)))

life_prev_df <- life_prev_df |>
  separate(HEALTH_BOUNDARIES,c("HEALTH_BOUND_CODE","HEALTH_BOUND_NAME")," ", extra = "merge")|>
  mutate(YEAR = as.numeric(str_sub(FISC_YR_LABEL,4,7)))

# Source helper functions -----



# User interface ----
ui <- fluidPage(
  titlePanel("Chronic Disease Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Info about dashboard"),
      
      selectInput("dataset", 
                  label = "Select Rate Type",
                  choices = c("Incidence Rate","Life Prevalence","HSC Prevalence")),
      
      uiOutput("disease"),

      selectInput("region", 
                  label = "Select Health Region",
                  choices = append("All",unique(inc_rate_df$HEALTH_BOUND_NAME)),
                  multiple = TRUE,
                  selected = "All"),
      
      sliderInput("year_range", 
                  label = "Select Year Range",
                  min = 2001, max = 2020, value = c(2001, 2020)),
      
      radioButtons("gender", 
                   label = ("Select Gender"),
                   choices = c("Male","Female","Both"), 
                   selected = "Both"),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 verbatimTextOutput("summary"),
                 plotOutput("graph1")),
        tabPanel("Analysis",
                 plotOutput("graph2")),
        tabPanel("Map", 
                 plotOutput("map")),
        tabPanel("Table", 
                 dataTableOutput("table"))
      )
    )
  )
)

# Server logic ----
server <- function(input, output) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Incidence Rate" = inc_rate_df,
           "Life Prevalence" = life_prev_df,
           "HSC Prevalence" = hsc_prev_df)
  })
  
  output$disease <- renderUI({
    selectInput("disease", 
                label = "Select Disease",
                choices = append("All",unique(datasetInput()$DISEASE)),
                multiple = TRUE,
                selected = "All")
  })
  
  filter_df <- reactive({
    datasetInput() |> 
      filter ((if (input$region == "All") TRUE else (HEALTH_BOUND_NAME %in% input$region)) &
              (if (input$disease == "All")TRUE else (DISEASE %in% input$disease)) &
              (YEAR %in% seq(from=min(input$year_range),to=max(input$year_range))) &
              (if (input$gender =='Both') TRUE else (CLNT_GENDER_LABEL ==input$gender)))
  })
  
  output$summary <- renderText({paste0("Some summary info \nselected disease(s):", list(input$disease) )})
  
  output$graph1 <- renderPlot({
    filter_df()|>
      ggplot(aes(y=DISEASE,x= CRUDE_RATE_PER_1000))+
      geom_bar(stat='summary',fun=mean)+
      labs(x="Crude Rate Per 1000",
           y="Disease",
           title = paste0("Average Crude ", input$dataset))
  })
  
  output$graph2 <- renderPlot({
    filter_df()|>
      ggplot(aes(y=CRUDE_RATE_PER_1000,x= YEAR,color=DISEASE))+
      geom_line(stat='summary',fun=mean)+
      labs(y="Crude Rate Per 1000",
           x="Year",
           legend ="Disease",
           title = paste0("Average Crude ", input$dataset," Over Time"))
  })
  
  
  output$table <- renderDataTable(filter_df())
  
  output$map <- renderPlot({
    
  })
}

# Run app ----
shinyApp(ui, server)