

# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)


# load data
# =========


load("~/EDcrowding/predict-admission/data-raw/lab_2020-10-05.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-09-30.rda")

# # explore data
# # ============
# 

# exploring

# many different names for the same test
lab_raw %>% group_by(battery_code, local_code, mapped_name) %>% summarise(tot = n()) 

# local_code looks the most complete
lab_raw <- lab_raw %>% select(-battery_code, -mapped_name)



# transform data
# ==============

lab_raw <- lab_raw %>% mutate(result_as_real = as.numeric(result_text))


# two results have > 10K results therefore worth including
lab_raw %>% filter(is.na(result_as_real)) %>% count(result_text) %>% arrange(desc(n))

# all but 2 of the > 90 are GFR
lab_raw %>% filter(result_text == ">90") %>% count(local_code)

gfr_gt90 <- lab_raw %>% 
  select(-battery_code, -mapped_name, -reference_range) %>% 
  filter(result_text == ">90", local_code == "GFR") %>% 
  mutate(local_code = "GFR>90", result_as_real = 1)


lab_raw %>% filter(result_text == "Sample unsuitable for analysis due to haemolysis.") %>% count(local_code) %>% arrange(desc(n))

lab_raw %>% filter(grepl("haemolysis|haemolysed", result_text))

haemolysis_sample <- lab_raw %>% 
  select(-battery_code, -mapped_name, -reference_range) %>% 
  filter(grepl("haemolysis|haemolysed", result_text)) %>% 
  mutate(local_code = "HAEMOLYSIS_SAMPLE", result_as_real = 1)


lab_raw %>% filter(result_text == "<0.6") 

crp_lt0.6 <- lab_raw %>% 
  select(-battery_code, -mapped_name, -reference_range) %>% 
  filter(result_text == "<0.6", local_code == "CRP") %>% 
  mutate(local_code = "CRP<0.6", result_as_real = 1)   

negative_result <- lab_raw %>% 
  select(-battery_code, -mapped_name, -reference_range) %>% 
  filter(grepl("negative", tolower(result_text))) %>% 
  mutate(local_code = paste0(local_code, "_NEG"), result_as_real = 1)

positive_result <- lab_raw %>% 
  select(-battery_code, -mapped_name, -reference_range) %>% 
  filter(grepl("positive", tolower(result_text))) %>% 
  mutate(local_code = paste0(local_code, "_POS"), result_as_real = 1)


lab_real <- lab_raw %>% 
  select(-battery_code, -mapped_name, -reference_range) %>% 
  filter(!is.na(result_as_real)) %>%  # 138K rows of #145K
  bind_rows(gfr_gt90, haemolysis_sample, crp_lt0.6, negative_result, positive_result) %>% 
  arrange(mrn, csn, result_datetime, local_code)




# process data for ML
# ==================

# add elapsed time
lab_real <- lab_real %>% left_join(ED_csn_summ %>% select(mrn, csn, arrival_dttm)) %>% 
  mutate(elapsed_mins = as.numeric(difftime(result_datetime, arrival_dttm, units = "mins"))) %>% 
  select(-arrival_dttm)

# calculate number of results
lab_num_results <- lab_raw %>% 
  mutate(mapped_name = tolower(gsub(" ","_", mapped_name))) %>% 
  group_by(mrn, csn, fk_bed_moves, mapped_name) %>% 
  summarise(num_results = n())


# Save data
# =========

outFile = paste0("EDcrowding/predict-admission/data-raw/lab_real_",today(),".rda")
save(lab_real, file = outFile)

# note this is use mapped name not the abbreviation
outFile = paste0("EDcrowding/predict-admission/data-raw/lab_num_results_",today(),".rda")
save(lab_num_results, file = outFile)
