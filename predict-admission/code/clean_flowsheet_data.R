
# About this file ---------------------------------------------------------

# Takes flowsheet data and cleans it ready for creating design matrix
# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)


# load data
# =========


load("~/EDcrowding/predict-admission/data-raw/flowsheet_raw_with_Star_2020-11-05.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_2020-11-04.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-11-04.rda")
load("~/EDcrowding/flow-mapping/data-raw/Excluded_csns_all_2020-10-14.rda")

# remove flowsheet rows that are not included in ED_bed_moves
flowsheet_raw_excluded_csns <- flowsheet_raw %>% 
  anti_join(excluded_csns, by = c("mrn", "csn" = "csn_old"))


# change csn to new csn by matching using a lookup from ED_bed_moves 
# this uses the bed_moves foreign key which will map the old csn to the new one if relevant
# using an inner join to exclude any csns which are not in ED_bed_moves
flowsheet_raw_excluded_csns <- flowsheet_raw_excluded_csns %>% 
  inner_join(ED_bed_moves %>% ungroup() %>% select(csn, csn_old, pk_bed_moves) %>% 
               mutate(pk_bed_moves = as.character(pk_bed_moves) ) %>% 
               distinct() %>% rename(csn_new = csn),
             by = c("csn" = "csn_old", "fk_bed_moves" = "pk_bed_moves")) %>% 
  select(-csn) %>% 
  rename(csn = csn_new)

# add elapsed time
flowsheet_raw_excluded_csns <- flowsheet_raw_excluded_csns %>% 
  left_join(ED_csn_summ %>% 
              select(mrn, csn, arrival_dttm) %>% distinct()) %>% 
  mutate(elapsed_mins = as.numeric(difftime(flowsheet_datetime, arrival_dttm, units = "mins"))) %>% 
  select(-arrival_dttm)

# tidy labels

abbrev= c("acvpu", "pain_nonverbal",  "bp", "temp", "o2_flowrate", "coma_score", "heart_rate", 
          "ideal_weight", "art_pressure_inv", "vent_inv_yes_no", "morphine_dose", "news", 
          "art_pressure_noninv", "o2_conc", "o2_delivery_method", "o2_sat", "pain_move", "pain_rest",
          "pip", "peep", "resp_assist", "resp_rate", "rass", 
           "tidal_vol", "vent_mode" , "vent_yes_no" 
           )
name_lookup <- bind_cols(unique(flowsheet_raw_excluded_csns$mapped_name)[order(unique(flowsheet_raw_excluded_csns$mapped_name))], 
                         abbrev)
colnames(name_lookup) <- c("mapped_name", "meas")

flowsheet_raw_excluded_csns <- flowsheet_raw_excluded_csns %>% 
  left_join(name_lookup) %>% select(-mapped_name)


# for now exclude all except main flowsheet values
flowsheet_raw_excluded_csns <- flowsheet_raw_excluded_csns %>% 
  filter(meas %in% c("acvpu", "bp", "temp",  "heart_rate",  "news", 
                                                    "o2_sat", "resp_assist", "resp_rate"))



# transform data
# ==============

# remove blank rows
flowsheet_raw_excluded_csns <- flowsheet_raw_excluded_csns %>%
  filter(!(is.na(result_as_real) & is.na(result_text))) %>% 
  select(mrn, csn, fk_bed_moves, flowsheet_datetime, everything())

# convert values that are text to numeric where this can be done simply
flowsheet_raw_excluded_csns <- flowsheet_raw_excluded_csns %>%
  mutate(result_as_real = case_when(!is.na(as.numeric(result_text)) ~ as.numeric(result_text), 
                                    TRUE ~ result_as_real))

flowsheet_raw_excluded_csns <- flowsheet_raw_excluded_csns %>% 
  mutate(result_as_real = case_when(meas == "acvpu" & result_text == "A" ~ 1,
                                                    meas == "acvpu" & result_text == "C" ~ 2,
                                                    meas == "acvpu" & result_text == "A" ~ 3,
                                                    meas == "acvpu" & result_text == "P" ~ 4,
                                                    meas == "acvpu" & result_text == "U" ~ 5,
                                                    TRUE ~ result_as_real
                                                    ))

# remove rows with no values
flowsheet_raw_excluded_csns <- flowsheet_raw_excluded_csns %>%
  mutate(keep_row = case_when(result_text == "\"\"" & is.na(result_as_real) ~ FALSE,
                       TRUE ~ TRUE)) %>% 
  filter(keep_row) %>% select(-keep_row)


# arrange rows
flowsheet_raw_excluded_csns <- flowsheet_raw_excluded_csns %>%
  arrange(mrn, csn, fk_bed_moves, flowsheet_datetime)
                               
# convert text-based numerical fields to numeric
bp <- flowsheet_raw_excluded_csns %>%
  filter(meas == "bp") %>% 
  select(mrn, csn, fk_bed_moves, elapsed_mins, flowsheet_datetime, result_text) %>% 
  separate(result_text, into = c("bp_sys","bp_dia"), sep = "/") %>% 
  mutate(bp_sys = as.numeric(bp_sys),
         bp_dia = as.numeric(bp_dia)) %>% 
    pivot_longer(!mrn:flowsheet_datetime, names_to = "meas", values_to = "result_as_real"
    )

flowsheet_real <- flowsheet_raw_excluded_csns %>% select(-result_text) %>% 
  filter(!meas %in% c("bp"), !is.na(result_as_real)) %>% 
  bind_rows(bp) %>% 
  arrange(mrn, csn, flowsheet_datetime, meas)

# process data for ML
# ==================

# calculate number of results
flowsheet_num_results <- flowsheet_raw_excluded_csns %>% 
  group_by(mrn, csn, fk_bed_moves, meas) %>% 
  summarise(num_results = n())

# calculate wide matrix of number of results - note this includes fk_bed_moves to enable linking to location
flowsheet_num_results_with_zero <- flowsheet_num_results %>% 
  pivot_wider(names_from = meas, values_from = num_results)


# replace NAs with zero 
flowsheet_num_results_with_zero <- flowsheet_num_results_with_zero %>%
  mutate_at(vars(colnames(flowsheet_num_results_with_zero)[4:ncol(flowsheet_num_results_with_zero)]), replace_na, 0)


# calculate wide matrix of number of results at csn level
flowsheet_num_results_with_zero_csn_level <- flowsheet_num_results %>% 
  ungroup() %>% group_by(mrn, csn, meas) %>%
  summarise(num_results = sum(num_results)) %>% 
  pivot_wider(names_from = meas, values_from = num_results) 

# replace NAs with zero 
flowsheet_num_results_with_zero_csn_level <- flowsheet_num_results_with_zero_csn_level %>%   # need to fill in the NA values as zeroes for people without any flowsheet measurements
  mutate_at(vars(colnames(flowsheet_num_results_with_zero_csn_level)[3:ncol(flowsheet_num_results_with_zero_csn_level)]), replace_na, 0)




# Save data
# =========
               

outFile = paste0("EDcrowding/predict-admission/data-raw/flowsheet_raw_excluded_csns_",today(),".rda")
save(flowsheet_raw_excluded_csns, file = outFile)

outFile = paste0("EDcrowding/predict-admission/data-raw/flowsheet_real_",today(),".rda")
save(flowsheet_real, file = outFile)

outFile = paste0("EDcrowding/predict-admission/data-raw/flowsheet_num_results_",today(),".rda")
save(flowsheet_num_results, file = outFile)

outFile = paste0("EDcrowding/predict-admission/data-raw/flowsheet_num_results_with_zero_",today(),".rda")
save(flowsheet_num_results_with_zero, file = outFile)

outFile = paste0("EDcrowding/predict-admission/data-raw/flowsheet_num_results_with_zero_csn_level_",today(),".rda")
save(flowsheet_num_results_with_zero_csn_level, file = outFile)
