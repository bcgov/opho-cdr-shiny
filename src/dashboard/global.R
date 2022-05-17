inc_rate_df <- data.frame()
hsc_prev_df <- data.frame()
life_prev_df <- data.frame()

for (dir in list.dirs("data")[-1]){
  for (file in list.files(dir)){
    new_df <- read_csv(paste0(dir,"/",file),
                       col_select = c("DISEASE",
                                      "FISC_YR_LABEL",
                                      "CLNT_GENDER_LABEL",
                                      "HEALTH_BOUNDARIES",
                                      "CRUDE_RATE_PER_1000",
                                      "STD_RATE_PER_1000"),
                       show_col_types = FALSE)|>
      drop_na(CRUDE_RATE_PER_1000);
    if (dir == "data/IncidenceRate"){
      inc_rate_df <- rbind(inc_rate_df,new_df)
    }else if (dir == "data/HSCPrevalence"){
      hsc_prev_df <- rbind(hsc_prev_df,new_df)
    }else if (dir == "data/LifePrevalence"){
      life_prev_df <- rbind(life_prev_df,new_df)
    }
  }
}


inc_rate_df <- inc_rate_df |>
  separate(HEALTH_BOUNDARIES,c("HEALTH_BOUND_CODE","HEALTH_BOUND_NAME")," ", extra = "merge")|>
  mutate(YEAR = as.numeric(str_sub(FISC_YR_LABEL,4,7)))|>
  select(-FISC_YR_LABEL)

hsc_prev_df <- hsc_prev_df |>
  separate(HEALTH_BOUNDARIES,c("HEALTH_BOUND_CODE","HEALTH_BOUND_NAME")," ", extra = "merge")|>
  mutate(YEAR = as.numeric(str_sub(FISC_YR_LABEL,4,7)))|>
  select(-FISC_YR_LABEL)

life_prev_df <- life_prev_df |>
  separate(HEALTH_BOUNDARIES,c("HEALTH_BOUND_CODE","HEALTH_BOUND_NAME")," ", extra = "merge")|>
  mutate(YEAR = as.numeric(str_sub(FISC_YR_LABEL,4,7)))|>
  select(-FISC_YR_LABEL)


chsa_spdf <- readOGR(
  dsn = paste0(getwd(),"/geo_data/chsa_2018"),
  layer ="CHSA_2018",
  verbose = FALSE
) |>
  spTransform( CRS("+proj=longlat +datum=WGS84 +no_defs"))