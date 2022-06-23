# Author: Jennifer Hoang
# Updated: July 17, 2022

"Fits a Bayesian Random Walk (RW) temporal model (order 1 & 2)
and selects the RW model with lowest WAIC for a given disease rate metric
(Incidence Rate, HSC Prevalence, Life Prevalence) when data does not contain
zero values. Otherwise, fits a local regression (LOESS) on data containing zeroes.
Outputs a CSV with model results.

Usage: src/model/01_analysis.R --input=<input> --output=<output>

Options:
--input=<input>       Path to data directory with rate data
--output=<output>     Path to results directory for model output
" -> doc

library(docopt)
library(tidyverse)
library(INLA)
library(here)
library(fANCOVA)

opt <- docopt(doc)

#' Loads original rate data and wrangles fiscal year into a numeric YEAR column
#'
#' @param file file path to data
#' @return data frame with wrangled data
load_data <- function(file) {
  read_csv(file, col_types = cols(CLNT_GENDER_LABEL = col_factor())) |>
    mutate(YEAR = as.numeric(substr(FISC_YR_LABEL, 4, 7)))
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
    y_obs = data$STD_RATE_PER_1000,
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
    y_obs = data$STD_RATE_PER_1000,
    y_fitted = model$summary.fitted$mean,
    ci_95_ll = model$summary.fitted$`0.025quant`,
    ci_95_ul = model$summary.fitted$`0.975quant`,
    dic = model$dic$dic,
    waic = model$waic$waic,
    model = "rw2"
  )

  model_results
}

#' Fit a loess model with STD_RATE_PER_1000 as response variable and
#' YEAR as explanatory variable. Confidence intervals generated
#' using t-distribution approximation.
#' The loess smoothing parameter is optimized using the AICC and GCV
#' using the fANCOVA::loess.as() function.
#'
#' @param data data frame containing original data
#' @return data frame with model results
fit_loess <- function(data) {
  model <- loess.as(x = data$YEAR, y = data$STD_RATE_PER_1000)
  model_predict <- predict(model, data, se = TRUE)

  t <- qt(p = 0.975, df = model_predict$df)

  model_results <- tibble(
    DISEASE = data$DISEASE,
    HEALTH_BOUNDARIES = data$HEALTH_BOUNDARIES,
    year = unique(data$YEAR),
    y_obs = data$STD_RATE_PER_1000,
    y_fitted = model_predict$fit,
    ci_95_ll = model_predict$fit - t * model_predict$se.fit,
    ci_95_ul = model_predict$fit + t * model_predict$se.fit,
    dic = NA,
    waic = NA,
    model = "loess",
  )

  model_results
}

#' Returns the results from the Random Walk model with the smaller WAIC
#'
#' @param rw1_model data frame of rw1 model results
#' @param rw2_model data frame of rw2 model results
#' @return data frame with selected rw model results
select_rw_model <- function(rw1_model, rw2_model) {
  # Select RW model using WAIC --------------------------------------------
  if (rw1_model$waic[1] < rw2_model$waic[1]) {
    rw1_model
  } else {
    rw2_model
  }
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

      # Skip Data Suppressed Regions -------------------------------------------
      if (anyNA(region_data$STD_RATE_PER_1000)) {
        next
      # Fit RW Models on region data WITHOUT zero values) ----------------------
      } else if (!any(region_data$STD_RATE_PER_1000 == 0)) {
        tryCatch(
          {
            rw1_model <- fit_inla_rw1(region_data)
            rw2_model <- fit_inla_rw2(region_data)

            selected_rw <- select_rw_model(rw1_model, rw2_model)
            model_results <- rbind(model_results, selected_rw)
          },
          error = function(e) {
            message(paste("Error with", disease, region))
          }
        )
      # Fit LOESS Models on region data WITH zero values -----------------------
      } else if (any(region_data$STD_RATE_PER_1000 == 0)) {
        loess_model <- fit_loess(region_data)
        model_results <- rbind(model_results, loess_model)
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
