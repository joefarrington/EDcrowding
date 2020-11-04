# About this file
# ==============

# Loads bed_moves adds age and gender and saves two versions
# - matrix_csn - one row per patient
# - matrix_loc - one row per patient and location


# load libraries
# ==============

library(dplyr)
library(tidyverse)
library(lubridate)


# load data
# =========

load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_2020-11-04.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-11-04.rda")


# demographic data
load("~/EDcrowding/predict-admission/data-raw/demog_2020-11-03.rda")
demog_raw2 <- demog_raw
load("~/EDcrowding/predict-admission/data-raw/demog_2020-09-21.rda")
demog_raw <- demog_raw %>% bind_rows(demog_raw2)


# process data
# ============

### attach age and sex 

ED_csn_summ <- ED_csn_summ %>% 
  left_join(demog_raw %>% select(mrn, sex, birthdate)) %>% 
  # clean records with birthdate of 1900-01-01
  mutate(age = case_when(birthdate == "1900-01-01" ~ NA_real_,
                         TRUE ~ year(arrival_dttm) - year(birthdate)))
  

# join with ED_bed_moves (only rows in ED) and join with ED_csn_summ to final admission status
bed_moves <- ED_bed_moves %>% 
  filter(ED_row_excl_OTF == 1) %>% 
  left_join(ED_csn_summ %>% select(arrival_dttm, csn, adm, sex, age)) %>% 
  select(mrn, sex, csn, csn_old, age, arrival_dttm, epoch, admission, discharge, pk_bed_moves, room4, adm)

# generate vars for time in location
bed_moves <- bed_moves %>% ungroup() %>% 
  mutate(loc_admission = floor(as.numeric(difftime(admission, arrival_dttm, units = "mins"))),
         loc_discharge = floor(as.numeric(difftime(discharge, arrival_dttm, units = "mins"))),
         loc_duration = loc_discharge - loc_admission) %>% select(-admission, -discharge)
#         loc_duration = duration_mins + 60*(duration_hours + 24*duration_days))

# select cols for matrix
# 
# matrix_loc <- bed_moves %>% select(mrn, sex, csn, csn_old, arrival_dttm, epoch, age, pk_bed_moves, room4,
#                                    loc_admission,	loc_discharge, loc_duration, adm) %>% ungroup()
# 
# save(matrix_loc, file = paste0('EDcrowding/predict-admission/data-raw/matrix_loc_',today(),'.rda'))


matrix_csn <- bed_moves %>% select(mrn, sex, csn, csn_old, arrival_dttm, epoch, age, room4,
                                   loc_duration, adm) %>% 
  group_by(mrn, sex, csn, csn_old, arrival_dttm, epoch, age, room4, adm) %>% 
  summarise(loc_duration = sum(loc_duration)) %>% 
  pivot_wider(names_from = room4, names_prefix = "loc_duration_", values_from = loc_duration)

save(matrix_csn, file = paste0('EDcrowding/predict-admission/data-raw/matrix_csn_',today(),'.rda'))

