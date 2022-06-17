library(shiny)
library(fst)
library(data.table)
library(plotly)
library(shinyWidgets)
set.seed(666)

data= reactiveVal(NULL)
tmp_all = reactiveValues(fst = NULL, rows_fst = NULL, cols_fst = NULL)

tmp_fst = fst("/Users/mahmood/UBCMDS/591_capstone/joinfast.fst")

cols_fst = c("RATE", 
             "DISEASE", 
             "HEALTH_BOUNDARIES",
             "YEAR",
             "join_obs",
             "join_fitted")
tmp_all$fst = tmp_fst

tmp = tmp_fst[cols_fst] %>% setDT()

ui <- fluidPage(
  pickerInput("rates", "Choose Type of Rate", multiple = F, choices = unique(levels(as.factor(tmp$RATE)))                                          
              ),
  br(),
  pickerInput("chsa", "Choose CHSA", multiple = F, choices = unique(levels(as.factor(tmp$HEALTH_BOUNDARIES)))                                          ,
  ),
  br(),
  pickerInput("disease", "Choose Disease", multiple = F, choices = unique(levels(as.factor(tmp$DISEASE)))                                          ,
  ),
  plotlyOutput('plot')
  
)

server <- function(input, output) {

trend<- reactive({
  tmp %>% 
    filter(RATE %in% input$rates) %>% 
    filter(HEALTH_BOUNDARIES %in% input$chsa) %>% 
    filter(DISEASE %in% input$disease) %>%
    droplevels()
})
output$plot <- renderPlotly({
  t <- trend()
  p <-plot_ly(data=t, x=~YEAR,  y = ~join_obs, name = "Observed points",
              type = 'scatter', mode = 'markers')
  p = add_lines(p, x=~YEAR, y=~join_fitted, name="Predicted trend")

})
}

shinyApp(ui = ui, server = server)