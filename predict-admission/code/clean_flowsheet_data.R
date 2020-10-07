
# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)


# load data
# =========


load("~/EDcrowding/predict-admission/data-raw/flowsheet_2020-09-28.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-09-30.rda")


# transform data
# ==============

# convert mapped names to underscores
flowsheet_raw <- flowsheet_raw %>%
  filter(!(is.na(result_as_real) & is.na(result_text))) %>% 
  mutate(mapped_name = tolower(gsub(" ","_", mapped_name))) %>% 
  select(mrn, csn, fk_bed_moves, flowsheet_datetime, everything())

# convert values that are text to numeric where this can be done simply
flowsheet_raw <- flowsheet_raw %>%
  mutate(result_as_real = case_when(!is.na(as.numeric(result_text)) ~ as.numeric(result_text), 
                                    TRUE ~ result_as_real))

flowsheet_raw <- flowsheet_raw %>% 
  mutate(result_as_real = case_when(mapped_name == "acvpu" & result_text == "A" ~ 1,
                                                    mapped_name == "acvpu" & result_text == "C" ~ 2,
                                                    mapped_name == "acvpu" & result_text == "A" ~ 3,
                                                    mapped_name == "acvpu" & result_text == "P" ~ 4,
                                                    mapped_name == "acvpu" & result_text == "U" ~ 5,
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
  filter(mapped_name == "blood_pressure") %>% 
  select(mrn, csn, fk_bed_moves, flowsheet_datetime, result_text) %>% 
  separate(result_text, into = c("bp_sys","bp_dia"), sep = "/") %>% 
  mutate(bp_sys = as.numeric(bp_sys),
         bp_dia = as.numeric(bp_dia)) %>% 
    pivot_longer(!mrn:flowsheet_datetime, names_to = "mapped_name", values_to = "result_as_real"
    )

respiratory_assist_status <- flowsheet_raw %>% 
  filter(mapped_name == "respiratory_assist_status", !is.na(result_text)) %>% 
  select(mrn, csn, fk_bed_moves, flowsheet_datetime, result_text) %>% 
  mutate(mapped_name = paste0("resp_assist_",tolower(gsub(" ","_", result_text))),
         result_as_real = 1) %>%  select(-result_text) 

oxygen_delivery_method <- flowsheet_raw %>% 
  filter(mapped_name == "oxygen_delivery_method", !is.na(result_text)) %>% 
  select(mrn, csn, fk_bed_moves, flowsheet_datetime, result_text) %>% 
  mutate(mapped_name = paste0("o2_delivery_",tolower(gsub(" ","_", result_text))),
         result_as_real = 1) %>%  select(-result_text) 

# only 343 rows have any capture of ventilation data
# assuming it's more relevant that it was captured than the values recorded
ventilation_data <- flowsheet_raw %>% 
  filter(mapped_name %in% c("invasive_ventilation_yes/no",
                            "ventilator_mode_",
                            "ventilator_yes/no"), !is.na(result_text)) %>% 
  mutate(mapped_name = "ventilation_data_captured",
         result_as_real = 1) %>%  select(-result_text) 

flowsheet_real <- flowsheet_raw %>% select(-result_text) %>% 
  filter(!mapped_name %in% c("blood_pressure","respiratory_assist_status",
                             "oxygen_delivery_method", "invasive_ventilation_yes/no",
                             "ventilator_mode_",
                             "ventilator_yes/no")) %>% 
  bind_rows(bp, ventilation_data, respiratory_assist_status, oxygen_delivery_method) %>% 
  arrange(mrn, csn, flowsheet_datetime, mapped_name)

# process data for ML
# ==================

# calculate number of results
flowsheet_num_results <- flowsheet_raw %>% 
  mutate(mapped_name = tolower(gsub(" ","_", mapped_name))) %>% 
  group_by(mrn, csn, fk_bed_moves, mapped_name) %>% 
  summarise(num_results = n())

# calculate wide matrix of number of results
flowsheet_num_results_with_zero <- flowsheet_num_results %>% 
  pivot_wider(names_from = mapped_name, values_from = num_results)

# replace NAs with zero
flowsheet_num_results_with_zero <- flowsheet_num_results_with_zero %>%
  mutate_at(vars(colnames(flowsheet_num_results_with_zero)[4:ncol(flowsheet_num_results_with_zero)]), replace_na, 0)



# Save data
# =========
               
outFile = paste0("EDcrowding/predict-admission/data-raw/flowsheet_real_",today(),".rda")
save(flowsheet_real, file = outFile)

outFile = paste0("EDcrowding/predict-admission/data-raw/flowsheet_num_results_",today(),".rda")
save(flowsheet_num_results, file = outFile)

outFile = paste0("EDcrowding/predict-admission/data-raw/flowsheet_num_results_with_zero_",today(),".rda")
save(flowsheet_num_results_with_zero, file = outFile)
