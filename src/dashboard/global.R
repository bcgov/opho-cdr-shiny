################################
# This file creates the global variables and the shape files that are 
#   going to be used in app.R
################################  

################################
# Define and initialize global variables
################################

# Define a dictionary of disease names, which maps acronyms to user-friendly full names
disease_dict <- c("ALZHEIMER_DEMENTIA" = "Alzheimer's and Other Types of Dementia",
                  "AMI" = "Acute Myocardial Infarction",
                  "ASTHMA" = "Asthma",
                  "CKD_GBD" = "Chronic Kidney Disease",
                  "COPD" = "Chronic Obstructive Pulmonary Disease",
                  "DEPRESSION" = "Depression",
                  "DIABETES" = "Diabetes Mellitus",
                  "EPILEPSY" = "Epilepsy",
                  "GOUT" = "Gout and Crystal Arthropathies",
                  "HAEMORR_STROKE" = "Stroke (Hospitalized Haemorrhagic)",
                  "HEART_FAILURE" = "Heart Failure",
                  "HOSP_STROKE" = "Stroke (Hospitalized)",
                  "HOSP_TIA" = "Stroke (Hospitalized Transient Ischemic Attack)",
                  "HYPERTENSION" = "Hypertension",
                  "IHD" = "Ischemic Heart Disease",
                  "ISCH_STROKE" = "Stroke (Hospitalized Ischemic)",
                  "JUVENILE_ARTHRITIS" = "Juvenile Idiopathic Arthritis",
                  "MOOD_ANX" = "Mood and Anxiety Disorders",
                  "MS" = "Multiple Sclerosis",
                  "OSTEOARTHRITIS" = "Osteoarthritis",
                  "OSTEOPOROSIS" = "Osteoporosis",
                  "PARKINSONISM" = "Parkinson's Disease and Parkinsonism",
                  "RHEUMATOID_ARTHRITIS" = "Rheumatoid Arthritis",
                  "SCHIZOPHRENIA" = "Schizophrenia and Delusional Disorders",
                  "SUD" = "Substance Use Disorders")


# Define other global variables for the filters to speed up the server
GEOGRAPHY_CHOICES <- c("Health Authorities","Community Health Service Areas")
HA_CHOICES <- c("Fraser", "Interior", "Northern", "Vancouver Coastal", "Vancouver Island")

HSC_disease<- c("Acute Myocardial Infarction",
                "Asthma",
                "Depression",
                "Gout and Crystal Arthropathies",
                "Stroke (Hospitalized Haemorrhagic)",
                "Stroke (Hospitalized)",
                "Stroke (Hospitalized Transient Ischemic Attack)",
                "Stroke (Hospitalized Ischemic)",
                "Mood and Anxiety Disorders",
                "Schizophrenia and Delusional Disorders",
                "Substance Use Disorders")

RATE_TYPE_CHOICES <- c(
  "Crude Incidence Rate",
  "Age Standardized Incidence Rate",
  "Crude Life Prevalence",
  "Age Standardized Life Prevalence",
  "Crude HSC Prevalence",
  "Age Standardized HSC Prevalence"
)

# CHSA_CHOICES <- sort(unique(filter(inc_rate_df, GEOGRAPHY == "CHSA")$HEALTH_BOUND_NAME))

################################
# Read data from files and prepare data frames for analysis
################################

# Create 3 empty data frames that correspond to three rate types:
#   Incidence Rate, Active Healthcare Contact (HSC) Prevalence, and Lifetime Prevalence
inc_rate_df <- data.frame()
hsc_prev_df <- data.frame()
life_prev_df <- data.frame()

# Read csv files and concatenate rows with the same rate type
for (dir in list.dirs("data/raw")[-1]) {
  for (file in list.files(dir)) {
    new_df <- data.table::fread(paste0(dir, "/", file),
                                verbose = FALSE,
                                drop = c("STDPOP")) |>
      drop_na(NUMERATOR)
    if (dir == "data/raw/IncidenceRate") {
      inc_rate_df <- rbind(inc_rate_df, new_df)
    } else if (dir == "data/raw/LifePrevalence") {
      life_prev_df <- rbind(life_prev_df, new_df)
    } else if (dir == "data/raw/HSCPrevalence") {
      hsc_prev_df <- rbind(hsc_prev_df, new_df)
    }
  }
}

# Function to wrangle year and create fitted columns in preparation for merging
wrangle_df <- function(df){
  df|>
    mutate(YEAR = as.numeric(str_sub(FISC_YR_LABEL, 4, 7)),
           y_fitted = NA,
           ci_95_ll = NA,
           ci_95_ul = NA,
           )|>
    select(-FISC_YR_LABEL) 
}

inc_rate_df <- wrangle_df(inc_rate_df)
hsc_prev_df <- wrangle_df(hsc_prev_df)
life_prev_df <- wrangle_df(life_prev_df)

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

# Read in modeled data and merge with raw data
for(dir in list.dirs("data/model")[-1]){
  for (file in list.files(dir)){
    new_df_model<- data.table::fread(paste0(dir, "/", file),
                                     verbose = FALSE,
                                     drop = c("dic","waic","model","y_obs"))
    
    if (dir == "data/model/IncidenceRate") {
      inc_rate_df <- merge_df(inc_rate_df,new_df_model)
    } else if (dir == "data/model/LifePrevalence") {
      life_prev_df <- merge_df(life_prev_df,new_df_model)
    } else if (dir == "data/model/HSCPrevalence") {
      hsc_prev_df <- merge_df(hsc_prev_df,new_df_model)
    }

  }
}

# Clean the data frames using a function

# This function wrangles a data frame based on the following steps:
#  1. extract the HEALTH_BOUND_CODE and HEALTH_BOUND_NAME from the HEALTH_BOUNDARIES column
#  2. extract only the YEAR part from the FISC_YR_LABEL column and keep only the extracted YEAR
#  3. replace the acronyms used in the DISEASE column with their full names
#  4. reorder the the data frame based on YEAR
#  5. remove the rows where HEALTH_BOUND_NAME is unknown
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

inc_rate_df <- wrangle_data_frame(inc_rate_df)
hsc_prev_df <- wrangle_data_frame(hsc_prev_df)
life_prev_df <- wrangle_data_frame(life_prev_df)

# Define dataframe of HA colours
HA_colours <- data.frame(Regions = c("Interior","Fraser","Vancouver Coastal","Vancouver Island","Northern"),
                         Colors = c("#3891A7","#C3860D","#C42E2E", "#67A63C","#914FAB"))

# Define dataframe of CHSA colours
CHSA_colours<-data.frame()
for (i in seq(1,5)){
  chsas <- inc_rate_df|>
    filter(GEOGRAPHY=="CHSA",
           startsWith(HEALTH_BOUND_CODE,toString(i)))|>
    select(HEALTH_BOUND_NAME)|>
    unique()
  colfunc <- colorRampPalette(c("gray30",HA_colours[i,2],"white"))
  cols <- colfunc(nrow(chsas)+10)
  cols <- cols[6:(length(cols)-5)]
  chsas_colors <- chsas|>
    mutate(Colors = cols)|>
    dplyr::rename(Regions = HEALTH_BOUND_NAME)
  CHSA_colours<- rbind(CHSA_colours,chsas_colors)
}

# Read the shape files for the Community Health Service Areas (CHSA) level
chsa_spdf <- readOGR(
  dsn = paste0(getwd(), "/geo_data/chsa_2018"),
  layer = "CHSA_2018",
  verbose = FALSE
) |>
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Simplify Spatial Polygons for faster rendering
regions_df <- chsa_spdf@data
chsa_spdf <- gSimplify(chsa_spdf,0.01,topologyPreserve = TRUE)
chsa_spdf <- SpatialPolygonsDataFrame(chsa_spdf, regions_df)

# Read the shape files for the Health Authorities (HA) level
ha_spdf <- readOGR(
  dsn = paste0(getwd(), "/geo_data/ha_2018"),
  layer = "HA_2018",
  verbose = FALSE
) |>
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))


# Simplify Spatial Polygons for faster rendering
regions_df <- ha_spdf@data
ha_spdf <- gSimplify(ha_spdf,0.01,topologyPreserve = TRUE)
ha_spdf <- SpatialPolygonsDataFrame(ha_spdf, regions_df)


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
