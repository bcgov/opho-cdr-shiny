################################
# This file creates the global variables and the shape files that are 
#   going to be used in app.R
################################  

################################
# Define and initialize global variables
################################

# Create 3 empty data frames that correspond to three rate types:
#   Incidence Rate, Active Healthcare Contact (HSC) Prevalence, and Lifetime Prevalence
inc_rate_df <- data.frame()
hsc_prev_df <- data.frame()
life_prev_df <- data.frame()

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


# Define a dictionary of bins to use in choropleth map for each disease
bin_dict <-list("Alzheimer's and Other Types of Dementia" = c(0,2,4,6,8,Inf),
                "Acute Myocardial Infarction"= c(0,1,2,3,4,Inf),
                "Asthma"= c(0,2,4,6,8,Inf),
                "Chronic Kidney Disease"= c(0,1,2,3,4,Inf),
                "Chronic Obstructive Pulmonary Disease"= c(0,2,4,6,8,Inf),
                "Depression"= c(0,5,10,15,20,Inf),
                "Diabetes Mellitus"= c(0,2,4,6,8,Inf),
                "Epilepsy"= c(0,0.2,0.4,0.6,0.8,Inf),
                "Gout and Crystal Arthropathies"= c(0,1,2,3,4,Inf),
                "Stroke (Hospitalized Haemorrhagic)"= c(0,0.1,0.2,0.3,0.4,Inf),
                "Heart Failure"= c(0,1,2,3,4,Inf),
                "Stroke (Hospitalized)"= c(0,0.5,1,1.5,2,Inf),
                "Stroke (Hospitalized Transient Ischemic Attack)"= c(0,0.2,0.4,0.6,0.8,Inf),
                "Hypertension"= c(0,6,12,18,24,Inf),
                "Ischemic Heart Disease"= c(0,2,4,6,8,Inf),
                "Stroke (Hospitalized Ischemic)"= c(0,0.5,1,1.5,2,Inf),
                "Juvenile Idiopathic Arthritis"= c(0,0.01,0.02,0.03,0.04,Inf),
                "Mood and Anxiety Disorders"= c(0,8,16,24,32,Inf),
                "Multiple Sclerosis"= c(0,0.05,0.1,0.15,0.2,Inf),
                "Osteoarthritis"= c(0,3,6,9,12,Inf),
                "Osteoporosis"= c(0,3,6,9,12,Inf),
                "Parkinson's Disease and Parkinsonism"= c(0,0.2,0.4,0.6,0.8,Inf),
                "Rheumatoid Arthritis"= c(0,0.2,0.4,0.6,0.8,Inf),
                "Schizophrenia and Delusional Disorders"= c(0,0.2,0.4,0.6,0.8,Inf),
                "Substance Use Disorders"= c(0,1,2,3,4,Inf))

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

# Read csv files and concatenate rows with the same rate type
for (dir in list.dirs("data")[-1]) {
  for (file in list.files(dir)) {
    new_df <- data.table::fread(paste0(dir, "/", file),
                                verbose = FALSE,
                                drop = c("STDPOP")) |>
      drop_na(CRUDE_RATE_PER_1000)
    
    if (dir == "data/IncidenceRate") {
      inc_rate_df <- rbind(inc_rate_df, new_df)
    } else if (dir == "data/LifePrevalence") {
      life_prev_df <- rbind(life_prev_df, new_df)
    } else {
      hsc_prev_df <- rbind(hsc_prev_df, new_df)
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
    mutate(YEAR = as.numeric(str_sub(FISC_YR_LABEL, 4, 7)),
           DISEASE = str_replace_all(DISEASE, disease_dict)) |>
    select(-FISC_YR_LABEL) |>
    data.table::setcolorder(c("YEAR")) |>
    filter(!str_detect(HEALTH_BOUND_NAME, "Unknown"))
}

inc_rate_df <- wrangle_data_frame(inc_rate_df)
hsc_prev_df <- wrangle_data_frame(hsc_prev_df)
life_prev_df <- wrangle_data_frame(life_prev_df)

# Read the shape files for the Community Health Service Areas (CHSA) level
chsa_spdf <- readOGR(
  dsn = paste0(getwd(), "/geo_data/chsa_2018"),
  layer = "CHSA_2018",
  verbose = FALSE
) |>
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Read the shape files for the Health Authorities (HA) level
ha_spdf <- readOGR(
  dsn = paste0(getwd(), "/geo_data/ha_2018"),
  layer = "HA_2018",
  verbose = FALSE
) |>
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

