setwd("/Users/mahmood/UBCMDS/591_capstone/opho-cdr-shiny")

library(dplyr)
library(readr)

df_hscp_raw <- list.files(path="src/joinpoint/HSCPrevalence", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows
  
df_incidence_raw <- list.files(path="src/joinpoint/IncidenceRate", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

df_life_raw <- list.files(path="src/joinpoint/LifePrevalence", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows


