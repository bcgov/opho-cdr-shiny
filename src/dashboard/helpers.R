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