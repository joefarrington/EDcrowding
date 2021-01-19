# About this file
# ===============

# This file collects data from Star, which requires extracting data from non-materialised tables. 
# For flowsheets, this is very slow so the script itereates through a date range for flowsheets
# All patients are retrived (to make the SQL faster) so flowsheet data for non-ED patients, 
# and flowsheet data for ED patients after they leave ED are deleted

# For demog data, this is now coming from star_test

# For labs and flowsheets an additional step is required to create a foreign key to bed moves

# Note - this needs bed_moves and csn summary data already processed for the relevant motnhs
# And merges the new flowsheet and lab data into existing datasets that DO NO include
# any data for the relevant months



# Load libraries ----------------------------------------------------------


library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)


# Load data ---------------------------------------------------------------




# Set up connection
ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")




# Get and process flowsheet data ------------------------------------------------------


sqlQuery <- "select hv.encounter as csn, vo.observation_datetime, vo.unit, vo.value_as_real, vo.value_as_text,
vo.visit_observation_type_id
    from star_a.hospital_visit hv,
      star_a.visit_observation vo
  where hv.hospital_visit_id = vo.hospital_visit_id
    and hv.patient_class in ('EMERGENCY', 'INPATIENT')
    order by csn, vo.observation_datetime "

sqlQuery <- gsub('\n','',sqlQuery)
fs_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))


save(fs_raw, file = paste0('EDcrowding/predict-admission/data-raw/fs_raw_',today(),'.rda'))

# Get and process lab data ------------------------------------------------


# 
# lab_raw <- lab_raw %>% mutate(fk_bed_moves = as.character(fk_bed_moves)) %>%  bind_rows(lab_raw_SepOct_just_ED)
# 
# save(lab_raw, file = paste0('EDcrowding/predict-admission/data-raw/lab_raw_with_Star_',today(),'.rda'))

