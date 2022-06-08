# Function to wrangle year and create fitted columns in preparation for merging
# raw data with modeled data
wrangle_df_for_merge <- function(df){
  df|>
    mutate(YEAR = as.numeric(str_sub(FISC_YR_LABEL, 4, 7)),
           y_fitted = NA,
           ci_95_ll = NA,
           ci_95_ul = NA,
    )|>
    select(-FISC_YR_LABEL) 
}


# Function to merge raw data with modeled data
merge_df <- function(df,new_df_model){
  df|>
    left_join(new_df_model, by =c("DISEASE","HEALTH_BOUNDARIES","YEAR"="year"))|>
    mutate(y_fitted = coalesce(y_fitted.x, y_fitted.y),
           ci_95_ll = coalesce(ci_95_ll.x, ci_95_ll.y),
           ci_95_ul = coalesce(ci_95_ul.x, ci_95_ul.y)) |>
    select(-y_fitted.x, -y_fitted.y,
           -ci_95_ll.x, -ci_95_ll.y,
           -ci_95_ul.x, -ci_95_ul.y)
  
}


# Clean the data frames using a function

# This function wrangles a data frame based on the following steps:
#  1. extract the HEALTH_BOUND_CODE and HEALTH_BOUND_NAME from the HEALTH_BOUNDARIES column
#  2. replace the acronyms used in the DISEASE column with their full names
#  3. reorder the the data frame based on YEAR
#  4. remove the rows where HEALTH_BOUND_NAME is unknown
#  
#  It returns the cleaned data frame
wrangle_data_frame <- function(df) {
  df |>
    separate(HEALTH_BOUNDARIES,
             c("HEALTH_BOUND_CODE", "HEALTH_BOUND_NAME"),
             " ",
             extra = "merge") |>
    mutate(DISEASE = str_replace_all(DISEASE, disease_dict),
           DISEASE = ifelse(endsWith(DISEASE, "_EPI"), str_sub(DISEASE, 1, -5), DISEASE)) |>
    data.table::setcolorder(c("YEAR")) |>
    filter(!str_detect(HEALTH_BOUND_NAME, "Unknown"))
}


#helper function for choropleth animation
setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL, label = NULL,
                           options = NULL){
  
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip, label = label
               )))
  
  options <- evalFormula(options, data = data)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1]
  if("label" %in% colnames(style)){
    labelData = style[,"label", FALSE]
    style = style[,-which(colnames(style)=="label"), FALSE]
    leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label)
  }
  leaflet::invokeMethod(map, data, "setStyle", "shape", layerId, style);
}


#helper function in JS for choropleth animation
leafletjs <-  tags$head(
  tags$script(HTML('
  
window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  style = HTMLWidgets.dataframeToD3(style);
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.setStyle(style[i]);
    }
  });
};
window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.unbindTooltip();
      layer.bindTooltip(label[i])
    }
  });
};
'
  ))
)


#helper function in JS for changing tab backgrounds
tab_colsjs <- tags$head(
  tags$script("
      Shiny.addCustomMessageHandler('background-color', function(color) {
        document.body.style.backgroundColor = color;
      });
    ")
  )



# To draw vertical lines on line graph
vline <- function(x = 0, color = "gray40") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash="dot")
  )
}

# To format and round all values to 2 decimals
format_round <- function(x, dec = 2){
  format(round(x,2),2)
}

# Dynamic Health Boundaries selection
health_bounds <- function(input){
  (
  if(input == "Health Authorities") 
   return(c(sort(unique(filter(inc_rate_df,GEOGRAPHY=="HA")$HEALTH_BOUND_NAME))))
  else 
    return(c(sort(unique(filter(inc_rate_df,GEOGRAPHY=="CHSA")$HEALTH_BOUND_NAME))))
  )

}

# Fiscal year tooltip
 fisc_year_tt <- function(id){
   bsTooltip(id = id, 
             title="Years are based on Ministry of Health fiscal years. For example, the year 2001 represents data from April 1, 2001 to March 31, 2002",
             placement = "right"
   )
 }
 
 # Sex radio buttons template
 sex_radio_buttons <- function(id){
   radioButtons(id, 
                label = ("Select Sex"),
                choices = c("Male","Female","Total"), 
                selected = "Total",
                inline = TRUE)
 }
 
 # Geography radio buttons template
 geography_radio_buttons <- function(id){
   radioButtons(id,
                label= "Select Health Boundary Type",
                choices = GEOGRAPHY_CHOICES,
                selected=GEOGRAPHY_CHOICES[1])
 }
 
 # Year slider input template
 year_slider <- function(id, year_info_id, anim = TRUE) {
   if (anim == TRUE) {
     return(
       sliderInput(
         id,
         label = tags$span(
           "Select Year  ",
           tags$i(
             id = year_info_id,
             class = "glyphicon glyphicon-info-sign",
             style = "color:#0072B2;"
           )
         ),
         min = 2001,
         max = 2020,
         value = 2001,
         sep = "",
         ticks = TRUE,
         animate = animationOptions(interval = 1000)
       )
     )
   } else{
     sliderInput(
       id,
       label = tags$span(
         "Select Year Range  ",
         tags$i(
           id = year_info_id,
           class = "glyphicon glyphicon-info-sign",
           style = "color:#0072B2;"
         )
       ),
       min = 2001,
       max = 2020,
       value = c(2001, 2020),
       sep = ""
     )
   }
 }
 
 # Rate type dropdown template
 rate_type_input <- function(id){
   selectInput(
     id,
     label = "Select Rate Type",
     choices = RATE_TYPE_CHOICES
   )
 }
 
 # Dataset switch function
 dataset_switch <- function (input){
   switch(input,
          "Crude Incidence Rate" = inc_rate_df,
          "Age Standardized Incidence Rate" = inc_rate_df,
          "Crude Life Prevalence" = life_prev_df,
          "Age Standardized Life Prevalence" = life_prev_df,
          "Crude HSC Prevalence" = hsc_prev_df,
          "Age Standardized HSC Prevalence" = hsc_prev_df)
 }
 
 # Rate input switch function
 rate_switch <- function(input){
   ifelse(startsWith(input, "Age"),
          "STD_RATE_PER_1000",
          "CRUDE_RATE_PER_1000")
 }
 
 # Toggle template
 material_switch <- function(id, lab){
   materialSwitch(
     inputId = id,
     label = lab,
     right = TRUE,
     inline= TRUE
   )
 }
 
 # Hover template for line chart 
 hovertemplate_line <- paste0('<b>Health Region</b>: %{fullData.name}',
                              '<br><b>%{yaxis.title.text}</b>: %{y:.2f}',
                              '<br><b>Year</b>: %{x}',
                              '<extra></extra>'
 )
 
 # Hover template for line chart in the region tab
 region_tab_hovertemplate_line <- paste0('<b>Disease</b>: %{fullData.name}',
                              '<br><b>%{yaxis.title.text}</b>: %{y:.2f}',
                              '<br><b>Year</b>: %{x}',
                              '<extra></extra>'
 )
 
 # Graph y-axis options template
 y_axis_spec <- function(input,range_mode){
   list(title = list(text = paste0(input," Per 1000"),
                     font = list(size = ifelse(startsWith(input,"Age"),12,14))),
        gridcolor = "#d9dadb",
        showline= T, 
        linewidth=1, 
        linecolor='black',
        rangemode = range_mode)
   
 }
 
 # Bar Graph x-axis options template
 x_axis_bar_spec <- function(title,so=0 ){
   list(title = list(text = title, standoff = so),
        categoryorder = "category ascending",
        tickfont = list(size = 10),
        showline= T, linewidth=1, linecolor='black')
 }
 
 # Line Graph x-axis options template
 x_axis_line_spec <- function(title,so=10 ){
   list(title = list(text = title, standoff = 10),
        gridcolor = "#d9dadb",
        showline= T, linewidth=1, linecolor='black')
 }