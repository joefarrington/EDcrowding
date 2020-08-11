# ===============
# About this file
# ===============
# Loads data on ED patients locations
# Formats a matrix for input into a ML model to predict location
# Matrix holds number of patients in any location in each hour
#



# Load libraries
# ==============

library(dplyr)
library(tidyverse)
library(lubridate)




# Load data
# =========

load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_clean_extra_JanFeb_2020-08-11.rda")


# Process data
# ============

# set parameters
matrix_start_date <- date("2020-01-01")
matrix_end_date <- date("2020-03-01")

# create a series with all the required time periods by hour
date_range <- seq(as.POSIXct(matrix_start_date),as.POSIXct(matrix_end_date), by = "hours")

# make smaller datasets for speed of processing
ED_bed_moves_occ <- ED_bed_moves_extra %>% ungroup() %>% 
  filter(admission > matrix_start_date, 
         admission < matrix_end_date + days(1),
         ED_row_excl_OTF == 1) %>% 
  select(admission, discharge, room6)

ED_bed_moves_admissions <- ED_bed_moves_extra %>% ungroup() %>% 
  filter(admission > matrix_start_date, 
         admission < matrix_end_date + days(1),
         admission_row) %>% 
  select(admission, discharge, ED_discharge_dttm_final)


# process data
ml_matrix <- tribble(
  ~ DateTime,
  ~ room6, 
  ~ number
)

for (i in (1:length(date_range))) {
  
  if (i %% 100 == 0) {
    print(paste("Processed",i,"rows"))
  }
  # use these rows to get everyone in a location at any point in the hour
  # occupancy <- ED_bed_moves_reduced %>% filter(,
  #                         admission <= date_range[i+1], 
  #                         discharge >= date_range[i]) %>% 
  #   group_by(room6) %>% summarise(number = n())
  
  # use these rows to get everyone in a location at the precise moment
  occupancy <- ED_bed_moves_occ %>% filter(admission < date_range[i], 
                                             discharge >= date_range[i]) %>% 
    group_by(room6) %>% summarise(number = n(), .groups = "drop")
  
  occupancy$DateTime <- date_range[i]
  ml_matrix <- ml_matrix %>% rbind(occupancy)
  
  if (i >1) {
    # this retrieves all the admissions in the hour preceding the time
    
    admissions <- ED_bed_moves_admissions %>% ungroup()  %>% 
      filter(ED_discharge_dttm_final >= date_range[i-1], 
               ED_discharge_dttm_final <= date_range[i]) %>%
      mutate(room6 = "admission") %>% 
      group_by(room6) %>% summarise(number = n(), .groups = "drop")
    
    admissions$DateTime <- date_range[i]
    ml_matrix <- ml_matrix %>% rbind(admissions)
  }

}

outFile = paste0("EDcrowding/data-prep-for-ML/data-raw/ML_matrix_JanFeb_",today(),".rda")
save(ml_matrix, file = outFile)

# pivot to wide matrix and add columns

ml_matrix <- ml_matrix %>% mutate(time_of_day = hour(DateTime),
                            month = month(DateTime),
                            day_of_week = wday(DateTime))

ml_matrix_wide <- ml_matrix %>% pivot_wider(names_from = room6, values_from = number, values_fill = 0) %>% 
  select(DateTime, month, day_of_week,time_of_day,  Arrived, Waiting, RAT, TRIAGE, MAJORS, RESUS, TAF, UTC, admission)

outFile = paste0("EDcrowding/data-prep-for-ML/data-output/ML_matrix_JanFeb_",today(),"csv")
write.csv(ml_matrix_wide, file = outFile, row.names = FALSE)


