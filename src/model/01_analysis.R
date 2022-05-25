# Author: Jennifer Hoang
# Date: May 24, 2022

"Fits a Bayesian Random Walk (RW) temporal model (order 1 & 2) 
and selects the RW model with lowest WAIC for a given disease rate metric
(Incidence Rate, HSC Prevalence, Life Prevalence). 
Outputs a CSV with model results.

Usage: src/model/01_analysis.R --input=<input> --output=<output>

Options:
--input=<input>       Path to data directory with rate data
--output=<output>   Path to results directory for model output
" -> doc

library(docopt)
library(tidyverse)
library(INLA)
library(here)

opt <- docopt(doc)

load_data <- function(file) {
  read_csv(file, col_types = cols(CLNT_GENDER_LABEL = col_factor())) |>
    mutate(YEAR = substr(FISC_YR_LABEL, 4, 7))
}

#' Fit a random walk 1 model with STD_RATE_PER_1000 as response variable and 
#' YEAR as explanatory variable with INLA. Assumes Gamma likelihood.
#'
#' @param data data frame containing original data
#' @return data frame with model results
fit_inla_rw1 <- function(data) {
  model <- inla(STD_RATE_PER_1000 ~ 1 + f(YEAR, model = "rw1"),
                data = data,
                family = "gamma",
                control.compute = list(dic = TRUE, waic = TRUE)
  )
  
  model_results <- tibble(
    DISEASE = data$DISEASE,
    HEALTH_BOUNDARIES = data$HEALTH_BOUNDARIES, 
    year = unique(data$YEAR),
    y_fitted = model$summary.fitted$mean,
    ci_95_ll = model$summary.fitted$`0.025quant`,
    ci_95_ul = model$summary.fitted$`0.975quant`,
    dic = model$dic$dic,
    waic = model$waic$waic,
    model = "rw1"
  )
  
  model_results
}

#' Fit a random walk 2 model with STD_RATE_PER_1000 as response variable and 
#' YEAR as explanatory variable with INLA. Assumes Gamma likelihood.
#'
#' @param data data frame containing original data
#' @return data frame with model results
fit_inla_rw2 <- function(data) {
  model <- inla(STD_RATE_PER_1000 ~ 1 + f(YEAR, model = "rw2"),
                data = data,
                family = "gamma",
                control.compute = list(dic = TRUE, waic = TRUE)
  )
  
  model_results <- tibble(
    DISEASE = data$DISEASE,
    HEALTH_BOUNDARIES = data$HEALTH_BOUNDARIES, 
    year = unique(data$YEAR),
    y_fitted = model$summary.fitted$mean,
    ci_95_ll = model$summary.fitted$`0.025quant`,
    ci_95_ul = model$summary.fitted$`0.975quant`,
    dic = model$dic$dic,
    waic = model$waic$waic,
    model = "rw2"
  )
  
  model_results
}

main <- function(opt) {
  for (file in list.files(opt$input)) {
    # Load Data ---------------------------------------------------------------
    disease_data <- load_data(here(opt$input, file))
    disease <- disease_data$DISEASE[1]
    region_list <- unique(disease_data$HEALTH_BOUNDARIES)
    model_results <- tibble()

    for (region in region_list) {
      region_data <- disease_data |>
        filter(HEALTH_BOUNDARIES == region)

      # Fit Model on CHSA Data (without suppression or zero values) ------------
      if (!anyNA(region_data$STD_RATE_PER_1000) & !any(region_data$STD_RATE_PER_1000 == 0)) {
        rw1_model <- fit_inla_rw1(region_data)
        rw2_model <- fit_inla_rw2(region_data)
      }

      # Select RW model using WAIC --------------------------------------------
      if (rw1_model$waic[1] < rw2_model$waic[1]) {
        model_results <- rbind(model_results, rw1_model)
      } else {
        model_results <- rbind(model_results, rw2_model)
      }
    }

    # Write Model Results per Disease -----------------------------------------
    model_results <- model_results |>
      mutate_if(is.numeric, round, 3)

    if (!dir.exists(here(opt$output))) {
      dir.create(here(opt$output), recursive = TRUE)
      write_csv(model_results, here(opt$output, paste0(disease, ".csv")))
    } else {
      write_csv(model_results, here(opt$output, paste0(disease, ".csv")))
    }
  }
}

main(opt)