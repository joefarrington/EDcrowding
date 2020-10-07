# About this file
# ==============

#bed moves data organization
# The bed moves information in bed_moves_emergency are entries of visits that go through ED.
# In this program, this information is prepared in order to fit into the learning model.
#
# 1. simplify room names
# 2. attach classification labels
#    a) TRUE or FALSE of admission
#    b) Finer classifications such as target wards
# 3. add age and sex
# 4. focusing on particular subgroups if desired


# load libraries
# ==============

library(dplyr)
library(tidyverse)
library(lubridate)
library(xgboost)
library(data.table)


# load data
# =========

#bed_moves = read.csv('F:/Saved/ENOCKUNG/ED project/bed_moves_emergency.csv')
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_2020-09-30.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-09-30.rda")

# temp fix to get pk_bed_moves
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_raw_all_2020-10-07.rda")

# demographic data
load("~/EDcrowding/predict-admission/data-raw/demog_2020-09-21.rda")


# process data
# ============

# join to get final admission status
bed_moves <- ED_bed_moves %>% 
  left_join(ED_csn_summ %>% select(arrival_dttm, csn, ED_last_status))

# create boolean for admission
bed_moves <- bed_moves %>% 
  mutate(adm = case_when(ED_last_status == "Discharged"~ FALSE,
                         TRUE ~ TRUE))

# attach pk_bed_moves
bed_moves <- bed_moves %>% 
  left_join(ED_bed_moves_raw %>% select(csn, pk_bed_moves))

### attach age and sex 

bed_moves <- bed_moves %>% 
  left_join(demog_raw)

bed_moves <- bed_moves %>% 
  mutate(age = year(arrival_dttm) - year(birthdate))


# select cols for matrix

matrix <- bed_moves %>% select(mrn, sex, csn, arrival_dttm, age, pk_bed_moves, admission,	discharge, department, room4, adm)

# add relevant arrival time information

matrix <- matrix %>% 
  mutate(weekend = ifelse(weekdays(arrival_dttm, abbreviate = TRUE) %in% c("Sat", "Sun"), 1, 0))

matrix <- matrix %>% 
  mutate(night = ifelse(hour(arrival_dttm) < 22 & hour(arrival_dttm) > 7, 0, 1))

save(matrix, file = paste0('EDcrowding/predict-admission/data-raw/matrix_',today(),'.rda'))

