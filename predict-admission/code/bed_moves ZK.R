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
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_2020-09-28.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-09-28.rda")

# demographic data
load("~/EDcrowding/predict-admission/data-raw/demog_2020-09-21.rda")


# process data
# ============

# join to get final admission status
bed_moves <- ED_bed_moves %>% 
  left_join(ED_csn_summ %>% select(csn, ED_last_status))

# create boolean for admission
bed_moves <- bed_moves %>% 
  mutate(adm = case_when(ED_last_status == "Discharged"~ FALSE,
                         TRUE ~ TRUE))

### attach age and sex 

bed_moves <- bed_moves %>% 
  left_join(demog_raw)

bed_moves <- bed_moves %>% 
  mutate(age = year(arrival_dttm) - year(birthdate))

# 
# 
# # change first stage of UCH EMERGENCY from <NA> to BEGIN
# bed_moves <- bed_moves %>% 
#   mutate(room = case_when(department == 'UCH EMERGENCY DEPT' & is.na(room) ~ "BEGIN",
#                    TRUE ~ room))


# select cols for matrix

matrix <- bed_moves %>% select(mrn, csn, admission,	discharge, department, room4, hl7_location,	sex ,age, adm, fk_bed_moves)
save(matrix, file = paste0('EDcrowding/predict-admission/data-raw/matrix_',today(),'.rda'))

