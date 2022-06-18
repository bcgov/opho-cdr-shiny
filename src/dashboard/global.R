################################
# This file creates the global variables and the shape files that are 
#   going to be used in app.R
################################  

# Load in helper  functions
source('helpers.R', local = T)


################################
# Read geographic data from files and simplify polygons
################################

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

HSC_DISEASES<- c("Acute Myocardial Infarction",
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

inc_rate_df <- wrangle_df_for_merge(inc_rate_df)
hsc_prev_df <- wrangle_df_for_merge(hsc_prev_df)
life_prev_df <- wrangle_df_for_merge(life_prev_df)


# Read in modeled data and merge with raw data
for(dir in list.dirs("data/model")[-1]){
  for (file in list.files(dir)){
    new_df_model<- data.table::fread(paste0(dir, "/", file),
                                     verbose = FALSE,
                                     select = c("DISEASE","HEALTH_BOUNDARIES","year","y_fitted"))
    
    if (dir == "data/model/IncidenceRate") {
      inc_rate_df <- merge_df(inc_rate_df,new_df_model)
    } else if (dir == "data/model/LifePrevalence") {
      life_prev_df <- merge_df(life_prev_df,new_df_model)
    } else if (dir == "data/model/HSCPrevalence") {
      hsc_prev_df <- merge_df(hsc_prev_df,new_df_model)
    }
  }
}

inc_rate_df <- wrangle_data_frame(inc_rate_df)
hsc_prev_df <- wrangle_data_frame(hsc_prev_df)
life_prev_df <- wrangle_data_frame(life_prev_df)

# Load, Read, Define fst file for joinpoint regression:

data= reactiveVal(NULL)
tmp_all = reactiveValues(fst = NULL, 
                         cols_fst = NULL)

tmp_fst = fst("data/joinpoint/joinfast.fst")

cols_fst = c("RATE", 
             "DISEASE", 
             "HEALTH_BOUNDARIES",
             "YEAR",
             "join_obs",
             "join_fitted")
tmp_all$fst = tmp_fst

joinpoint_df = tmp_fst[cols_fst] %>% setDT()

join_rates <- sort(unique(levels(as.factor(joinpoint_df$RATE))))
join_chsa <- sort(unique(levels(as.factor(joinpoint_df$HEALTH_BOUNDARIES))))
join_disease <- sort(unique(levels(as.factor(joinpoint_df$DISEASE))))





# Define dataframe of HA colours
HA_colours <- data.frame(Regions = c("Interior","Fraser","Vancouver Coastal","Vancouver Island","Northern"),
                         Colors = c("#3891A7","#C3860D","#C42E2E", "#67A63C","#914FAB"))

# Define dataframe of CHSA colours
CHSA_colours <- data.frame()
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

# Define dataframe of Disease colour mappings
set.seed(2001)
DISEASE_colors <- data.frame(DISEASE = sample(unique(inc_rate_df$DISEASE)))
DISEASE_colors$Colors <- colorRampPalette(HA_colours[,2])(length(unique(inc_rate_df$DISEASE))) 

# Define list of all diseases
ALL_DISEASES <- sort(unique(inc_rate_df$DISEASE))

