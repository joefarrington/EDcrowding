
# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)


# load data
# =========


load("~/EDcrowding/predict-admission/data-raw/flowsheet_2020-09-28.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-10-12.rda")

# tidy labels

abbrev= c("acvpu", "pain_nonverbal",  "bp", "temp", "o2_flowrate", "coma_score", "heart_rate", 
          "ideal_weight", "art_pressure_inv", "vent_inv_yes_no", "morphine_dose", "news", 
          "art_pressure_noninv", "o2_conc", "o2_delivery_method", "o2_sat", "pain_move", "pain_rest",
          "pip", "peep", "resp_assist", "resp_rate", "rass", 
           "tidal_vol", "vent_mode" , "vent_yes_no" 
           )
name_lookup <- bind_cols(unique(flowsheet_raw$mapped_name)[order(unique(flowsheet_raw$mapped_name))], 
                         abbrev)
colnames(name_lookup) <- c("mapped_name", "meas")

flowsheet_raw <- flowsheet_raw %>% 
  left_join(name_lookup) %>% select(-mapped_name)


# transform data
# ==============

# remove blank rows
flowsheet_raw <- flowsheet_raw %>%
  filter(!(is.na(result_as_real) & is.na(result_text))) %>% 
  select(mrn, csn, fk_bed_moves, flowsheet_datetime, everything())

# convert values that are text to numeric where this can be done simply
flowsheet_raw <- flowsheet_raw %>%
  mutate(result_as_real = case_when(!is.na(as.numeric(result_text)) ~ as.numeric(result_text), 
                                    TRUE ~ result_as_real))

flowsheet_raw <- flowsheet_raw %>% 
  mutate(result_as_real = case_when(meas == "acvpu" & result_text == "A" ~ 1,
                                                    meas == "acvpu" & result_text == "C" ~ 2,
                                                    meas == "acvpu" & result_text == "A" ~ 3,
                                                    meas == "acvpu" & result_text == "P" ~ 4,
                                                    meas == "acvpu" & result_text == "U" ~ 5,
                                                    TRUE ~ result_as_real
                                                    ))

# remove rows with no values
flowsheet_raw <- flowsheet_raw %>%
  mutate(keep_row = case_when(result_text == "\"\"" & is.na(result_as_real) ~ FALSE,
                       TRUE ~ TRUE)) %>% 
  filter(keep_row) %>% select(-keep_row)

# add elapsed time of result to flowsheet raw
flowsheet_raw <- flowsheet_raw %>%
  left_join(ED_csn_summ %>% select(mrn, csn, arrival_dttm)) %>% 
  mutate(elapsed_mins = as.numeric(difftime(flowsheet_datetime, arrival_dttm, units = "mins"))) %>% 
  select(-arrival_dttm)

# arrange rows
flowsheet_raw <- flowsheet_raw %>%
  arrange(mrn, csn, fk_bed_moves, flowsheet_datetime)
                               
# convert text-based numerical fields to numeric
bp <- flowsheet_raw %>%
  filter(meas == "bp") %>% 
  select(mrn, csn, fk_bed_moves, elapsed_mins, flowsheet_datetime, result_text) %>% 
  separate(result_text, into = c("bp_sys","bp_dia"), sep = "/") %>% 
  mutate(bp_sys = as.numeric(bp_sys),
         bp_dia = as.numeric(bp_dia)) %>% 
    pivot_longer(!mrn:flowsheet_datetime, names_to = "meas", values_to = "result_as_real"
    )

flowsheet_real <- flowsheet_raw %>% select(-result_text) %>% 
  filter(!meas %in% c("bp"), !is.na(result_as_real)) %>% 
  bind_rows(bp) %>% 
  arrange(mrn, csn, flowsheet_datetime, meas)

# process data for ML
# ==================

# calculate number of results
flowsheet_num_results <- flowsheet_raw %>% 
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
               
outFile = paste0("EDcrowding/predict-admission/data-raw/flowsheet_real_",today(),".rda")
save(flowsheet_real, file = outFile)

outFile = paste0("EDcrowding/predict-admission/data-raw/flowsheet_num_results_",today(),".rda")
save(flowsheet_num_results, file = outFile)

outFile = paste0("EDcrowding/predict-admission/data-raw/flowsheet_num_results_with_zero_",today(),".rda")
save(flowsheet_num_results_with_zero, file = outFile)

outFile = paste0("EDcrowding/predict-admission/data-raw/flowsheet_num_results_with_zero_csn_level_",today(),".rda")
save(flowsheet_num_results_with_zero_csn_level, file = outFile)
