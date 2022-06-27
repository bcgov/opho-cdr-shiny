# author: Mahmoodur Rahnman
# date: 2022-06-27

"Cleans, aggregates and pre-processes the Raw data (Data_T_CHSA) into a single 
csv file (joinpoint_df.csv)

Usage: src/joinpoint/joinpoint_wrangling.R --input=<input> --out_dir=<out_dir>
  
Options:
--input=<input>       Path to raw data
--out_dir=<out_dir>   Path to directory of processed data
" -> doc

library(docopt)
library(tidyverse)
library(here)

opt <-  docopt(doc)

main <- function(opt) {
  
  # reads and aggregates all the diseases files (.csv) within each rates
  
  incidence_df <- data.frame()
  hsc_df <- data.frame()
  life_df <- data.frame()
  
  for (dir in list.dirs(opt$input)[-1]) {
    for (file in list.files(dir)) {
      temp_df <- read_csv(
        file = here(dir, file)
      )
      
      if (here(dir) == here(opt$input, "IncidenceRate")) {
        incidence_df <- rbind(inc_rate_df, temp_df)
      } else if (here(dir) == here(opt$input, "HSCPrevalence")) {
        hsc_df <- rbind(hsc_prev_df, temp_df)
      } else if (here(dir) == here(opt$input, "LifePrevalence")) {
        life_df <- rbind(life_prev_df, temp_df)
      }
    }
  } 

  # merge all rates data into a single datarframe (joinpoint_df.csv)
  dfList = list(inc_rate_df, hsc_prev_df, life_prev_df)
  
  joinpoint_df <- bind_rows(dfList, .id = "keys") %>%
    mutate(keys = as.factor(keys),
           RATE = recode_factor(keys, 
                                '1' = "HSC", 
                                '2' = "INCIDENCE", 
                                '3' = "LIFE_PREV"),
           DISEASE = str_replace(DISEASE, "_", " "),
           HEALTH_BOUNDARIES =str_trim(gsub('^[[:digit:]]+', '', HEALTH_BOUNDARIES)),
           YEAR = as.numeric(substr(FISC_YR_LABEL, 4, 7)),
           across(c(STD_RATE_PER_1000, 
                    STD_UCL_95, 
                    STD_LCL_95), round, 2)
           ) %>%
    dplyr::select(-c(keys)) %>%
    dplyr::select(RATE, YEAR, DISEASE,
                  HEALTH_BOUNDARIES,
                  STD_RATE_PER_1000, 
                  STD_UCL_95, 
                  STD_LCL_95)

  # Write Data
  if (!dir.exists(here(opt$out_dir))) {
    dir.create(here(opt$out_dir), recursive = TRUE)
  }
  write_csv(joinpoint_df,
            here(opt$out_dir, "joinpoint_df.csv"))
  
  print(paste("Files output to:", opt$out_dir))
}

main(opt)
