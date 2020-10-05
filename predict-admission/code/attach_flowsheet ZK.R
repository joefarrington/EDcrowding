
# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)



# load data
# =========


load("~/EDcrowding/predict-admission/data-raw/matrix_2020-09-28.rda")
load("~/EDcrowding/predict-admission/data-raw/flowsheet_real_2020-10-05.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-09-30.rda")

# set index date and time
index_date = date("2020-08-25")
days_prior = 30
prior_date = index_date-30

matrix <- matrix %>% filter(date(arrival_dttm) <= index_date, date(arrival_dttm) >= prior_date)

matrix <- matrix %>% 
  mutate(weekend = ifelse(weekdays(matrix$arrival_dttm[1], abbreviate = TRUE) %in% c("Sat", "Sun"), 1, 0))

matrix <- matrix %>% 
  mutate(night = ifelse(hour(arrival_dttm) < 22 & hour(arrival_dttm) > 7, 0, 1))

flowsheet_real <- flowsheet_real %>% 
  left_join(ED_csn_summ %>% select(csn, arrival_dttm)) %>% 
  filter(date(arrival_dttm) <= index_date, date(arrival_dttm) >= prior_date) %>% 
  select(-arrival_dttm)

# take mean of result_as_real because in some dttms more than one value is recorded
flowsheet_real <- flowsheet_real %>% 
  group_by(mrn, csn, fk_bed_moves, flowsheet_datetime, mapped_name, elapsed_mins) %>% 
  summarise(result_as_real_mean = mean(result_as_real, na.rm = TRUE)) %>% 
  arrange(mrn, csn, flowsheet_datetime, elapsed_mins, mapped_name) %>% ungroup()

# # OR pivot wider with average where multiple values exist at any point in time
# flowsheet_wide <- flowsheet_real %>% 
#   group_by(mrn, csn, fk_bed_moves, flowsheet_datetime, mapped_name, elapsed_mins) %>% summarise(result_as_real_mean = mean(result_as_real, na.rm = TRUE)) %>%  
#   pivot_wider(names_from = mapped_name, values_from = result_as_real_mean) %>% ungroup()

# take mean of all measurements in a location
flowsheet_loc_mean <- flowsheet_real %>% 
  group_by(mrn, csn, fk_bed_moves, mapped_name) %>% summarise(result_as_real_mean = mean(result_as_real, na.rm = TRUE)) %>%  
  pivot_wider(names_from = mapped_name, values_from = result_as_real_mean)

# or take latest value in that row
# use fk_bed_moves to group measurements together and find the latest one
# group including mapped name as some latest results may not be taken in last batch

flowsheet_loc_max <- flowsheet_real %>%
  group_by(mrn, csn, fk_bed_moves, mapped_name) %>% 
  filter(flowsheet_datetime == max(flowsheet_datetime)) %>%  ungroup() %>% 
  pivot_wider(names_from = mapped_name, values_from = result_as_real_mean)

flowsheet_loc_max %>% ungroup() %>% pivot_longer(
  glasgow_coma_score_total:o2_delivery_o2_driven_neb, names_to = "meas", values_to = "value"
) %>% filter(is.na(value)) %>% group_by(meas) %>% count(meas) %>% arrange(desc(n))




# this works but I need pk_bed_moves in the matrix file
# will have to join to get that for now
load("~/EDcrowding/predict-admission/data-raw/bed_moves_2020-09-28.rda")
matrix <- matrix %>% left_join(
  bed_moves_raw %>% select(csn, admission, discharge, pk_bed_moves)
)

matrix_with_flow <- matrix %>% left_join(
  flowsheet_loc_max
)



# Save data
# =========

outFile = paste0("EDcrowding/predict-admission/data-raw/matrix_with_flow_",today(),".rda")
save(matrix_with_flow, file = outFile)



