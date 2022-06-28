"Fits a Joinpoint Regression model and results in fitted values where data does
not conmtain Zero, or missing values. When no significant changepoints can be 
identified by the model, then the fitted values are results of only log-linear 
model itself
Outputs a CSV with model results (joinpoint_resukts.csv) and a datafile to be 
used in R shiny app (joinpoint_for_shiny_df.fst).

Usage: src/joinpoint/joinpoint_results.R --input=<input> --output=<output>

Options:
--input=<input>       Path to data directory with joinpoint_df
--output=<output>     Path to results directory for model output
" -> doc

library(docopt)
library(here)
library(tidyverse)
library(segmented)
library(broom)
library(tidyr)
library(modelr)
library(purrr)
library(fst)

path <- here("data", "processed")
opt <- docopt(doc)

main <- function(opt) {
  
  # defining factors and ignoring missing values and Zero values: 
  
  CHSA_df <- read_csv(here(path, "joinpoint_df.csv")) |>
    mutate( RATE = as.factor(RATE),
            DISEASE = as.factor(DISEASE),
            HEALTH_BOUNDARIES = as.factor(HEALTH_BOUNDARIES),
            anyTRUE = !if_any(everything(), ~. %in% c(NA,"",0)))
  
  
  # ignore data of particular region and disease, as the changepioints are not compatible for joinpoint regression
  
  main_CHSA_df <- CHSA_df  |>
    mutate(anyTRUE = ifelse((RATE == 'INCIDENCE' & (
        (DISEASE == 'JUVENILE ARTHRITIS') |
          (HEALTH_BOUNDARIES == 'Downtown Victoria/Vic West' &
              DISEASE == 'EPILEPSY') |
          (HEALTH_BOUNDARIES == 'Snow Country' &
              DISEASE == 'HAEMORR STROKE') |
          (HEALTH_BOUNDARIES == 'Oak Bay' &
             DISEASE == 'EPILEPSY') |
          (HEALTH_BOUNDARIES == 'Vanderhoof Rural' &
              DISEASE == 'HAEMORR STROKE') |
          (HEALTH_BOUNDARIES == 'Panorama' &
             DISEASE == 'HOSP STROKE') |
          (HEALTH_BOUNDARIES == 'Grand Forks' &
             DISEASE == 'HOSP TIA') |
          (HEALTH_BOUNDARIES == 'Agassiz/Harrison' &
              DISEASE == 'HOSP TIA') |
          (HEALTH_BOUNDARIES == 'Burns Lake South' &
              DISEASE == 'HOSP TIA') |
          (HEALTH_BOUNDARIES == 'West Cariboo' &
              DISEASE == 'PARKINSONISM') |
          (HEALTH_BOUNDARIES == 'Agassiz/Harrison' &
              DISEASE == 'RHEUMATOID ARTHRITIS') |
          (HEALTH_BOUNDARIES == 'Burnaby Southwest' &
              DISEASE == 'RHEUMATOID ARTHRITIS') |
          (HEALTH_BOUNDARIES == 'Panorama' &
              DISEASE == 'OSTEOARTHRITIS') |
          (HEALTH_BOUNDARIES == 'Snow Country' &
             DISEASE == 'MS') |
          (HEALTH_BOUNDARIES == 'Snow Country' &
              DISEASE == 'PARKINSONISM') |
          (HEALTH_BOUNDARIES == 'Snow Country' &
              DISEASE == 'SCHIZOPHRENIA') |
          (HEALTH_BOUNDARIES == 'Burns Lake South' &
              DISEASE == 'PARKINSONISM') |
          (HEALTH_BOUNDARIES == 'Vanderhoof Rural' &
              DISEASE == 'PARKINSONISM') |
          (HEALTH_BOUNDARIES == "Hudson's Hope" &
             DISEASE == 'MS') |
          (HEALTH_BOUNDARIES == "Tumbler Ridge" &
              DISEASE == 'PARKINSONISM') |
          (HEALTH_BOUNDARIES == "Fort Nelson Population Centre" &
              DISEASE == 'MS') |
          (HEALTH_BOUNDARIES == "Fernie" &
              DISEASE == 'ALZHEIMER_DEMENTIA'))) |
      ((RATE == 'HSC' & (HEALTH_BOUNDARIES == 'Castlegar' &
                           DISEASE == 'HOSP TIA_EPI') |
          (HEALTH_BOUNDARIES == 'Enderby' &
              DISEASE == 'SCHIZOPHRENIA EPI') |
          (HEALTH_BOUNDARIES == 'North Thompson' &
              DISEASE == 'HOSP STROKE_EPI') |
          (HEALTH_BOUNDARIES == 'Brookswood/Murrayville' &
              DISEASE == 'HAEMORR STROKE_EPI') |
          (HEALTH_BOUNDARIES == 'Snow Country' &
              DISEASE == 'HAEMORR STROKE_EPI') |
          (HEALTH_BOUNDARIES == 'Vanderhoof Rural' &
              DISEASE == 'HAEMORR STROKE_EPI'))) |
      ((RATE == 'LIFE_PREV' & 
          (HEALTH_BOUNDARIES == 'West Cariboo' &
            DISEASE == 'JUVENILE ARTHRITIS') |
          (HEALTH_BOUNDARIES == 'Fort Nelson Population Centre' &
              DISEASE == 'JUVENILE ARTHRITIS'))), FALSE, anyTRUE))
    
  # generate results (log-linear segmented model)
  nested_df <- main_CHSA_df |>
    filter(anyTRUE==TRUE)|>
    group_by(RATE, HEALTH_BOUNDARIES, DISEASE) |>
    arrange(RATE, HEALTH_BOUNDARIES, DISEASE, YEAR) |> 
    nest() |>
    mutate(fit = map(data, ~segmented(lm(log(STD_RATE_PER_1000)~ YEAR,
                                         na.action= na.exclude,
                                         data=.), seg.Z = ~YEAR)),
           results = map(fit, augment))|>
    unnest(results)
  
  # Create data for R shiny app
  temp_df <- nested_df |>
    mutate(join_fitted = exp(.fitted)) |>
    dplyr::select(c(RATE,
                    DISEASE,
                    HEALTH_BOUNDARIES,
                    YEAR,
                    join_fitted))
  
  joinpoint_for_shiny_df <- main_CHSA_df |>
    full_join(temp_df, by = c("RATE", 
                              "DISEASE", 
                              "HEALTH_BOUNDARIES", 
                              "YEAR")) |>
    rename(join_obs = STD_RATE_PER_1000) |>
    mutate(
      RATE = recode_factor(
        RATE,
        'HSC' = "Active Healthcare Contact (HSC) Prevalence",
        'INCIDENCE' = "Incidence Rate",
        'LIFE_PREV' = "Lifetime Prevalence"
      ),
    )|>
    dplyr::select(c(RATE,
                    DISEASE,
                    HEALTH_BOUNDARIES,
                    YEAR,
                    join_obs,
                    join_fitted))
  
  
  # Write Data
  if (!file.exists(here(opt$output))) {
    file.create(here(opt$output), recursive = TRUE)
  }
  write_csv(nested_df,
            here(opt$output, "joinpoint_results.csv"))
  write_fst(joinpoint_for_shiny_df,
            here(opt$output, "joinpoint_for_shiny_df.fst"))
  print(paste("Files output to:", opt$output))
}

main(opt)