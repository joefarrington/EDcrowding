

# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)


# load data
# =========


load("~/EDcrowding/predict-admission/data-raw/lab_raw_2020-10-07.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-09-30.rda")


# transform data
# ==============

# remove blank rows
lab_raw <- lab_raw %>%
  filter(!is.na(result_text)) %>% 
  select(mrn, csn, fk_bed_moves, result_datetime, everything())


lab_real <- lab_raw %>% mutate(result_as_real = as.numeric(result_text))

lab_real <- lab_real %>%
  mutate(reference_range = case_when(reference_range == "" ~ NA_character_, 
 #                                    grepl("^<",reference_range) ~ gsub("^<", "0-", reference_range),
                                     TRUE ~ reference_range)) %>% 
  separate(reference_range, into = c("ref_low","ref_high"), sep = "-") %>% 
  mutate(ref_low = as.numeric(ref_low),
         ref_high = as.numeric(ref_high)) 

# create out of range values
lab_real <- lab_real %>% 
  mutate(oor_low = as.numeric(result_text) < ref_low,
         oor_high = as.numeric(result_text) > ref_high)

# two results that didn't convert to numeric can be derived from the result text
lab_real <- lab_real %>% 
  mutate(oor_high = case_when(result_text == ">90" & local_code == "GFR" ~ TRUE,
                              TRUE ~ oor_high),
         oor_low = case_when(result_text == "<0.6" & local_code == "CRP" ~ TRUE,
                             TRUE ~ oor_low))


# process data for ML
# ==================

# add elapsed time
lab_real <- lab_real %>% left_join(ED_csn_summ %>% select(mrn, csn, arrival_dttm)) %>% 
  mutate(elapsed_mins = as.numeric(difftime(result_datetime, arrival_dttm, units = "mins"))) %>% 
  select(-arrival_dttm)

# calculate number of results
lab_num_results <- lab_raw %>% 
  group_by(mrn, csn, fk_bed_moves, local_code) %>% 
  summarise(num_results = n())

# calculate wide matrix of number of results
lab_num_results_with_zero <- lab_num_results %>% 
  pivot_wider(names_from = local_code, values_from = num_results)

# replace NAs with zero (note this takes a long time so use separate file and run job)
lab_num_results_with_zero <- lab_num_results_with_zero %>%
  mutate_at(vars(colnames(lab_num_results_with_zero)[4:ncol(lab_num_results_with_zero)]), replace_na, 0)


# Save data
# =========

outFile = paste0("EDcrowding/predict-admission/data-raw/lab_real_",today(),".rda")
save(lab_real, file = outFile)

# note this is using local code not mapped_name
outFile = paste0("EDcrowding/predict-admission/data-raw/lab_num_results_",today(),".rda")
save(lab_num_results, file = outFile)

# note this is using local code
outFile = paste0("EDcrowding/predict-admission/data-raw/lab_num_results_with_zero_",today(),".rda")
save(lab_num_results_with_zero, file = outFile)
