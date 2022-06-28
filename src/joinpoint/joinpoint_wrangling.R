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

  df_hscp_raw <- list.files(here(opt$input, "HSCPrevalence"), full.names = TRUE) %>% 
    lapply(read_csv) %>% 
    bind_rows
  
  df_incidence_raw <- list.files(here(opt$input, "IncidenceRate"), full.names = TRUE) %>% 
    lapply(read_csv) %>% 
    bind_rows
  
  df_life_raw <- list.files(here(opt$input, "LifePrevalence"), full.names = TRUE) %>% 
    lapply(read_csv) %>% 
    bind_rows
  
  # merge all rates data into a single datarframe (joinpoint_df.csv) 
  dfList = list(df_hscp_raw, df_incidence_raw, df_life_raw)
  
  wrangling <- function(data) { 
    data %>%       
      mutate(YEAR = as.numeric(substr(FISC_YR_LABEL, 4, 7)),
             DISEASE = str_replace(DISEASE, "_", " "),
             HEALTH_BOUNDARIES =str_trim(gsub('^[[:digit:]]+', '', HEALTH_BOUNDARIES))) %>% 
      mutate(across(c(STD_RATE_PER_1000, 
                      STD_UCL_95, 
                      STD_LCL_95), round, 2)) %>%
      dplyr::select(YEAR, DISEASE,
                    HEALTH_BOUNDARIES,
                    STD_RATE_PER_1000, 
                    STD_UCL_95, 
                    STD_LCL_95)
  }
  
  df_temp <- dfList %>%
    lapply( wrangling )
  
  joinpoint_df <- bind_rows(df_temp, .id = "keys") %>%
    mutate(keys = as.factor(keys),
           RATE = recode_factor(keys, 
                                '1' = "HSC", 
                                '2' = "INCIDENCE", 
                                '3' = "LIFE_PREV"))%>%
    dplyr::select(-c(keys)) %>%
    dplyr::select(RATE, everything())

  # Write Data
  if (!file.exists(here(opt$out_dir))) {
    file.create(here(opt$out_dir), recursive = TRUE)
  }
  write_csv(joinpoint_df,
            here(opt$out_dir, "joinpoint_df.csv"))
  
  print(paste("Files output to:", opt$out_dir))
}

main(opt)
