# ===============
# About this file
# ===============
# Loads data on ED patients locations
# Formats a matrix for input into a ML model to predict location
# Matrix holds number of patients in any location in each hour
#
# Note - this is using a very crude version of room locations
# which does not distinguish between Covid and non-Covid



# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)
library(xts)

matrix_start_date <- "2020-05-01"
matrix_end_date <- "2020-07-31"


# create a series with all the required time periods by hour
date_range <- seq(as.POSIXct(matrix_start_date),as.POSIXct(matrix_end_date), by = "hours")


ml_matrix <- tribble(
  ~ hour_slot,
  ~ room6, 
  ~ number
)

for (i in (1:(length(date_range)-1))) {
  
  print(i)
  
  occupancy <- ED_bed_moves %>% filter(ED_row == 1,
                          admission <= date_range[i+1], 
                          discharge >= date_range[i]) %>% 
    group_by(room6) %>% summarise(number = n())
  
  occupancy$hour_slot <- date_range[i]
  
  ml_matrix <- ml_matrix %>% rbind(occupancy)
  
}

outFile = paste0("EDcrowding/data-prep-for-ML/data-output/ML_matrix_MayJunJul_",today(),".rda")
save(ml_matrix, file = outFile)


ml_matrix <- ml_matrix %>% mutate(time_of_day = hour(hour_slot),
                            month = month(hour_slot),
                            day_of_week = wday(hour_slot))

ml_matrix_wide <- ml_matrix %>% pivot_wider(names_from = room6, values_from = number, values_fill = 0)

