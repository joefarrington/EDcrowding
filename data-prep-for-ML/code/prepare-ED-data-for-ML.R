# ===============
# About this file
# ===============
# Loads data on ED patients locations
# Formats a matrix for input into a ML model to predict location
# Matrix holds number of patients in any location in each hour
#


# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)


# Create functions --------------------------------------------------------

rpt <- function(dataframe) {
  print(dataframe %>% select(csn) %>% n_distinct())
}





# Load data ---------------------------------------------------------------


load("~/EDcrowding/flow-mapping/data-raw/edgedf_2021-01-27.rda")
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-01-27.rda")


# Process data ------------------------------------------------------------


# set parameters
matrix_start_date <- date("2019-05-01")
matrix_end_date <- date("2020-01-25")

# create a series with all the required time periods by hour
date_range <- seq(as.POSIXct(matrix_start_date),as.POSIXct(matrix_end_date), by = "hours")


edgedf[, from := case_when(from == "OTF POOL" ~ "OTF",
                           from %in% c("PAEDS", "SAA") ~ "Other",
                           TRUE ~ from)]

# number in location at the timepoint
moves[admission <= date_range[i] & discharge >= date_range[i] & department %in% c("ED", "UCHT00CDU"), .N, by = location]

# numbers leaving ED location within the hour up to the timepoint
edgedf[from_dept %in% c("ED", "UCHT00CDU") & dttm > date_range[i-1] & dttm <= date_range[i], .N, by = from]
# compare with moves to check I've got this right
moves[discharge > date_range[i-1] & discharge <= date_range[i] & department %in% c("ED", "UCHT00CDU"), .N, by = location]

# numbers admitted within the hour up to the timepoint
edgedf[from_dept %in% c("ED", "UCHT00CDU") & !(to_dept %in% c("ED", "UCHT00CDU") | is.na(to_dept)) & dttm > date_range[i-1] & dttm <= date_range[i], .N]
# compare with moves
moves[discharge > date_range[i-1] & discharge <= date_range[i] & inside_exit, .N] # inside_exit includes CDU


num_in_location <- data.table()
moved_from_location <- data.table()



for (i in 2:length(date_range)) {
  if (i %% 500 == 0) {
    print(paste("Processed",i,"datetimes"))
  }
  
  # number in ED location at the timepoint
  num = moves[admission <= date_range[i] & discharge >= date_range[i] & department %in% c("ED", "UCHT00CDU"), .N, by = location]
  num$DateTime = date_range[i] 
  num_in_location <- bind_rows(num_in_location, num)
  
  # numbers leaving ED location within the hour up to the timepoint
  moved = moves[discharge > date_range[i-1] & discharge <= date_range[i] & department %in% c("ED", "UCHT00CDU"), .N, by = location]
  moved$DateTime = date_range[i] 
  moved_from_location <- bind_rows(moved_from_location, moved)
}

adm_during_hour = data.table()
for (i in 2:length(date_range)) {
  adm = data.table(edgedf[from_dept %in% c("ED", "UCHT00CDU") & !(to_dept %in% c("ED", "UCHT00CDU") | is.na(to_dept)) & dttm > date_range[i-1] & dttm <= date_range[i], .N])
  adm$DateTime = date_range[i] 
  adm_during_hour = bind_rows(adm_during_hour, adm)
}

setnames(adm_during_hour, "V1", "adm")

# pivot to wide matrix and add columns

num_in_location[, time_of_day := hour(DateTime)]
num_in_location[, month := month(DateTime)]
num_in_location[, day_of_week := wday(DateTime)]

moved_from_location[, time_of_day := hour(DateTime)]
moved_from_location[, month := month(DateTime)]
moved_from_location[, day_of_week := wday(DateTime)]

moved_from_location[, location := case_when(location == "OTF POOL" ~ "OTF",
                                        location %in% c("PAEDS", "SAA") ~ "Other",
                           TRUE ~ location)]

num_in_location[, location := case_when(location == "OTF POOL" ~ "OTF",
                                            location %in% c("PAEDS", "SAA") ~ "Other",
                                            TRUE ~ location)]


moved_w <- moved_from_location %>% pivot_wider(names_from = location, values_from = N, values_fill = 0) %>% 
  left_join(adm_during_hour)
num_w <- num_in_location %>% pivot_wider(names_from = location, values_from = N, values_fill = 0)  %>% 
  left_join(adm_during_hour)
  

outFile = paste0("EDcrowding/data-prep-for-ML/data-output/moved_from_location_",today(),".csv")
write.csv(moved_w, file = outFile, row.names = FALSE)

outFile = paste0("EDcrowding/data-prep-for-ML/data-output/num_in_location_",today(),".csv")
write.csv(num_w, file = outFile, row.names = FALSE)

