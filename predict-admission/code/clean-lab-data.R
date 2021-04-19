
# About this file ---------------------------------------------------------

# Takes flowsheet data and cleans it ready for creating design matrix
# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)
library(data.table)
library(readr)




# Load data ---------------------------------------------------------------

load("~/EDcrowding/predict-admission/data-raw/lab_orders_raw_2021-04-14.rda")
load("~/EDcrowding/predict-admission/data-raw/lab_results_raw_2021-04-14.rda")
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-04-13.rda")
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-04-13.rda")

summ[, left_ED := coalesce(first_outside_proper_admission, last_inside_discharge)]


# Process data ------------------------------------------------------------


# remove csns that are not included 
lab_orders_real <- data.table(lab_orders_raw)
lab_orders_real <- lab_orders_real[csn %in% summ$csn]
setkey(lab_orders_real, csn)

lab_results_real <- data.table(lab_results_raw)
lab_results_real <- lab_results_real[csn %in% summ$csn]
setkey(lab_results_real, csn)

# remove labs that are returned after ED

lab_orders_real <- merge(lab_orders_real, summ[,.(csn, first_ED_admission, left_ED)]) 
lab_orders_real <- lab_orders_real[request_datetime <= left_ED]

lab_results_real <- merge(lab_results_real, summ[,.(csn, first_ED_admission, left_ED)]) 
lab_results_real <- lab_results_real[result_last_modified_time <= left_ED]


# add elapsed time
lab_orders_real[, elapsed_mins := as.numeric(difftime(request_datetime, first_ED_admission, units = "mins"))]
lab_results_real[, elapsed_mins := as.numeric(difftime(result_last_modified_time, first_ED_admission, units = "mins"))]


# remove obs from prior to ED by more than 2 hours
lab_orders_real <- lab_orders_real[elapsed_mins >= -120]
lab_results_real <- lab_results_real[elapsed_mins >= -120]




# Transform data ----------------------------------------------------------

# create out of range values
lab_results_real <- lab_results_real %>% 
  mutate(oor_low = value_as_real < range_low,
         oor_high = value_as_real > range_high,
         abnormal = abnormal_flag == "A")


lab_results_real[, lab_results_real := case_when(is.na(value_as_real) & abnormal_flag == "A" ~ 1,
                                      TRUE ~ value_as_real
)]



# Save data ---------------------------------------------------------------


# create final dataset of orders
lab_orders_real <- lab_orders_real[, .(csn, request_datetime, battery_code, elapsed_mins)]

outFile = paste0("EDcrowding/predict-admission/data-raw/lab_orders_real_",today(),".rda")
save(lab_orders_real, file = outFile)

# create final dataset of results (real values)
lab_results_real <- lab_results_real[, .(csn, result_last_modified_time, abnormal_flag, value_as_real, test_lab_code, elapsed_mins, oor_low, oor_high, abnormal)]

outFile = paste0("EDcrowding/predict-admission/data-raw/lab_results_real_",today(),".rda")
save(lab_results_real, file = outFile)

