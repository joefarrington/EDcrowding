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




# Get and process obs data ------------------------------------------------------

# added a time interval to reduce the number of rows returned

sqlQuery <- "select hv.encounter as csn, hv.admission_time, vo.observation_datetime, vo.unit, vo.value_as_real, vo.value_as_text,
vo.visit_observation_type_id, vot.id_in_application
    from star.hospital_visit hv,
      star.visit_observation vo,
      star.visit_observation_type vot
  where hv.hospital_visit_id = vo.hospital_visit_id
    and hv.patient_class in ('EMERGENCY', 'INPATIENT')
  and vo.observation_datetime < hv.admission_time + INTERVAL'2 days'  
  and vo.visit_observation_type_id = vot.visit_observation_type  "

sqlQuery <- gsub('\n','',sqlQuery)
obs_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))


save(obs_raw, file = paste0('EDcrowding/predict-admission/data-raw/obs_raw_',today(),'.rda'))



# # Get and process lab orders ------------------------------------------------
# 
# sqlQuery <- "select hv.encounter as csn, lo.lab_order_id, lo.order_datetime, lo.request_datetime, lo.lab_battery_id, lb.battery_code
#   from star.hospital_visit hv,
#   star.lab_order lo,
#   star.lab_battery lb
#   where hv.hospital_visit_id = lo.hospital_visit_id
#   and hv.patient_class in ('EMERGENCY', 'INPATIENT')
#   and lo.lab_battery_id = lb.lab_battery_id
#       order by csn, lo.order_datetime;"
# 
# sqlQuery <- gsub('\n','',sqlQuery)
# lab_orders_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))
# 
# lab_orders_raw = data.table(lab_orders_raw)
# 
# save(lab_orders_raw, file = paste0('EDcrowding/predict-admission/data-raw/lab_orders_raw_',today(),'.rda'))
# 
# 
# # Get and process lab results -------------------------------------------------------------
# 
# 
# 
# sqlQuery <- "  select hv.encounter as csn, lr.lab_order_id, lr.abnormal_flag, lr.comment, lr.units, lr.range_high, lr.range_low, lr.result_last_modified_time, lr.value_as_real, lr.value_as_text, ltd.test_lab_code
#     from star.hospital_visit hv,
#       star.lab_result lr,
#     star.lab_order lo,
#     star.lab_test_definition ltd
#   where hv.hospital_visit_id = lo.hospital_visit_id
#     and lr.lab_order_id = lo.lab_order_id
#     and ltd.lab_test_definition_id = lr.lab_test_definition_id
#     and hv.patient_class in ('EMERGENCY', 'INPATIENT')
#       order by csn, lr.result_last_modified_time"
# 
# sqlQuery <- gsub('\n','',sqlQuery)
# lab_results_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))
# 
# # add battery code for lab results
# 
# lab_results_raw = data.table(lab_results_raw)
# lab_results_raw = merge(lab_results_raw, lab_orders_raw[, .(csn, lab_order_id, battery_code)], by = c("csn", "lab_order_id"), all.x = TRUE)
# 
# 
# save(lab_results_raw, file = paste0('EDcrowding/predict-admission/data-raw/lab_results_raw_',today(),'.rda'))
# 
# 




