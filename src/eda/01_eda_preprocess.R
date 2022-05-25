# Author: Jennifer Hoang
# Date: May 11, 2022

"Combines and pre-processes the BC Chronic Disease data into a single csv file
for each of the HSC Prevalence, IncidenceRate, and LifePrevalence data folders.
Used to create files for modeling EDA.

Usage: src/eda/01_eda_preprocess.R --input=<input> --out_dir=<out_dir>

Options:
--input=<input>       Path to data directory
--out_dir=<out_dir>   Path to directory to output processed data
" -> doc

library(docopt)
library(tidyverse)
library(here)

opt <- docopt(doc)


main <- function(opt) {
  inc_rate_df <- data.frame()
  hsc_prev_df <- data.frame()
  life_prev_df <- data.frame()
  
  # Load and Wrangle Data
  for (dir in list.dirs(opt$input)[-1]) {
    for (file in list.files(dir)) {
      new_df <- read_csv(
        file = here(dir, file),
        col_types = cols(CLNT_GENDER_LABEL = col_factor())
      ) |>
        mutate(YEAR = substr(FISC_YR_LABEL, 4, 7))
      
      if (here(dir) == here(opt$input, "IncidenceRate")) {
        inc_rate_df <- rbind(inc_rate_df, new_df)
      } else if (here(dir) == here(opt$input, "HSCPrevalence")) {
        hsc_prev_df <- rbind(hsc_prev_df, new_df)
      } else if (here(dir) == here(opt$input, "LifePrevalence")) {
        life_prev_df <- rbind(life_prev_df, new_df)
      }
    }
  }
  
  # Write Data
  write_csv(inc_rate_df, here(opt$out_dir, "incidence_rate_combined.csv"))
  write_csv(hsc_prev_df, here(opt$out_dir, "hsc_prevalence_combined.csv"))
  write_csv(life_prev_df, here(opt$out_dir, "life_prevalence_combined.csv"))
  
  print(paste("Files output to:", opt$out_dir))
}

main(opt)