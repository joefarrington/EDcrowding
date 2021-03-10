
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

load("~/EDcrowding/predict-admission/data-raw/lab_raw_2021-03-01.rda")
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-03-03.rda")
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-03-03.rda")


# Process data ------------------------------------------------------------


# remove flowsheet csns that are not included 
lab_real <- data.table(lab_raw)
lab_real <- lab_real[csn %in% summ$csn]
setkey(lab_real, csn)

# remove labs that are returned after ED
lab_real <- merge(lab_real, summ[,.(csn, first_ED_admission, first_outside_proper_admission)]) 
lab_real <- lab_real[result_last_modified_time <= first_outside_proper_admission]

# add elapsed time
lab_real[, elapsed_mins := as.numeric(difftime(result_last_modified_time, first_ED_admission, units = "mins"))]

# remove obs from prior to ED by more than 2 hours
lab_real <- lab_real[elapsed_mins >= -120]




# Transform data ----------------------------------------------------------



# create out of range values
lab_real <- lab_real %>% 
  mutate(oor_low = value_as_real < range_low,
         oor_high = value_as_real > range_high,
         abnormal = abnormal_flag == "A")


lab_real[, value_as_real := case_when(is.na(value_as_real) & abnormal_flag == "A" ~ 1,
                                      TRUE ~ value_as_real
)]



# Save data ---------------------------------------------------------------


# create final dataset of results (real values)
lab_real <- lab_real[, .(csn, result_last_modified_time, abnormal_flag, value_as_real, test_lab_code, elapsed_mins, oor_low, oor_high, abnormal)]


outFile = paste0("EDcrowding/predict-admission/data-raw/lab_real_",today(),".rda")
save(lab_real, file = outFile)

