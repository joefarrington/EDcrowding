

# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)


# load data
# =========


load("~/EDcrowding/predict-admission/data-raw/lab_raw_with_Star_2020-11-05.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_2020-11-04.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-11-04.rda")
load("~/EDcrowding/flow-mapping/data-raw/Excluded_csns_all_2020-10-14.rda")

# remove lab csns that are not included in ED_bed_moves
lab_raw_excluded_csns <- lab_raw %>% 
  anti_join(excluded_csns, by = c("mrn", "csn" = "csn_old"))

# change csn to new csn by matching using a lookup from ED_bed_moves 
# this uses the bed_moves foreign key which will map the old csn to the new one if relevant
# using an inner join to exclude any csns which are not in ED_bed_moves
lab_raw_excluded_csns <- lab_raw_excluded_csns %>% 
  inner_join(ED_bed_moves %>% ungroup() %>% select(csn, csn_old, pk_bed_moves) %>% distinct() %>% rename(csn_new = csn),
            by = c("csn" = "csn_old", "fk_bed_moves" = "pk_bed_moves")) %>% 
  select(-csn) %>% 
  rename(csn = csn_new)

# add elapsed time
lab_raw_excluded_csns <- lab_raw_excluded_csns %>% 
  left_join(ED_csn_summ %>% 
               select(mrn, csn, arrival_dttm) %>% distinct()) %>% 
  mutate(elapsed_mins = as.numeric(difftime(result_datetime, arrival_dttm, units = "mins"))) %>% 
  select(-arrival_dttm)

# Transform data ----------------------------------------------------------


# remove blank rows
lab_raw_excluded_csns <- lab_raw_excluded_csns %>%
  filter(!is.na(result_text)) %>% 
  select(mrn, csn, fk_bed_moves, result_datetime, everything())


# some values are returned with a < or > before them
lab_raw_excluded_csns %>% mutate(temp = substring(result_text,1,1)) %>% filter(temp %in% c("<",">")) %>% 
  select(local_code, temp, result_text, reference_range) %>% distinct()

# mutate these to remove the < or >

lab_raw_excluded_csns <- lab_raw_excluded_csns %>% 
  mutate(result_text = case_when(substring(result_text,1,1) %in% c("<",">") ~ gsub("<|>","", result_text),
                                 TRUE ~ result_text))



lab_real <- lab_raw_excluded_csns %>% mutate(result_as_real = as.numeric(result_text))

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



# Process data for ML -----------------------------------------------------


# calculate number of results
lab_num_results <- lab_raw_excluded_csns %>% 
  group_by(mrn, csn, fk_bed_moves, local_code) %>% 
  summarise(num_results = n())

# calculate wide matrix of number of results - note this includes fk_bed_moves to enable linking to location
lab_num_results_with_zero <- lab_num_results %>% 
  pivot_wider(names_from = local_code, values_from = num_results)

# replace NAs with zero (note this takes a long time so use separate file and run job)
lab_num_results_with_zero <- lab_num_results_with_zero %>%
  mutate_at(vars(colnames(lab_num_results_with_zero)[4:ncol(lab_num_results_with_zero)]), replace_na, 0)


# calculate wide matrix of number of results at csn level
lab_num_results_with_zero_csn_level <- lab_num_results %>% 
  ungroup() %>% group_by(mrn, csn, local_code) %>%
  summarise(num_results = sum(num_results)) %>% 
  pivot_wider(names_from = local_code, values_from = num_results) 

# replace NAs with zero 
lab_num_results_with_zero_csn_level <- lab_num_results_with_zero_csn_level %>%   # need to fill in the NA values as zeroes for people without any flowsheet measurements
  mutate_at(vars(colnames(lab_num_results_with_zero_csn_level)[3:ncol(lab_num_results_with_zero_csn_level)]), replace_na, 0)




# Save data ---------------------------------------------------------------



# Save data
# =========

outFile = paste0("EDcrowding/predict-admission/data-raw/lab_raw_excluded_csns_",today(),".rda")
save(lab_raw_excluded_csns, file = outFile)

outFile = paste0("EDcrowding/predict-admission/data-raw/lab_real_",today(),".rda")
save(lab_real, file = outFile)

# note this is using local code not mapped_name
outFile = paste0("EDcrowding/predict-admission/data-raw/lab_num_results_",today(),".rda")
save(lab_num_results, file = outFile)

# note this is using local code
outFile = paste0("EDcrowding/predict-admission/data-raw/lab_num_results_with_zero_",today(),".rda")
save(lab_num_results_with_zero, file = outFile)

# note this is using local code
outFile = paste0("EDcrowding/predict-admission/data-raw/lab_num_results_with_zero_csn_level_",today(),".rda")
save(lab_num_results_with_zero_csn_level, file = outFile)



