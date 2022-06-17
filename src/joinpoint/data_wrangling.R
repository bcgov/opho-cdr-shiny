setwd("/Users/mahmood/UBCMDS/591_capstone")

library(dplyr)
library(readr)

df_hscp_raw <- list.files(path="data_source/HSCPrevalence", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows
  
df_incidence_raw <- list.files(path="data_source/IncidenceRate", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

df_life_raw <- list.files(path="data_source/LifePrevalence", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

dfList = list(df_hscp_raw, df_incidence_raw, df_life_raw)

wrangling <- function(data) { 
  data %>%       
    mutate(YEAR = as.numeric(substr(FISC_YR_LABEL, 4, 7))) %>% 
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

df_merged <- bind_rows(df_temp, .id = "keys") %>%
  mutate(keys = as.factor(keys),
         RATE = recode_factor(keys, 
                              '1' = "HSC", 
                              '2' = "INCIDENCE", 
                              '3' = "LIFE_PREV"),
         DISEASE = str_replace(DISEASE, "_", " "),
         HEALTH_BOUNDARIES =str_trim(gsub('^[[:digit:]]+', '', HEALTH_BOUNDARIES))) %>%
  dplyr::select(-c(keys)) %>%
  dplyr::select(RATE, everything())

write.csv(df_merged,"/Users/mahmood/UBCMDS/591_capstone/master_df.csv", row.names = FALSE)
