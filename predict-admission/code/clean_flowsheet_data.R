
# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)


# load data
# =========


load("~/EDcrowding/predict-admission/data-raw/flowsheet_2020-09-28.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-09-30.rda")

# # explore data
# # ============
# 
# text_types <- flowsheet_raw %>% filter(mapped_name != "BLOOD PRESSURE", !is.na(result_text)) %>% group_by(result_text) %>% summarise(tot = n()) %>% arrange(desc(tot))
# 
# # Looking at relationship between text and real 
# y <- flowsheet_raw %>% 
#   filter(mapped_name != "BLOOD PRESSURE", !is.na(result_as_real)) %>% 
#   group_by(mapped_name, result_text) %>% summarise(tot = n()) %>% arrange(mapped_name, desc(tot))
# 
# 
# z <- flowsheet_raw %>% 
#   filter(mapped_name != "BLOOD PRESSURE", !is.na(result_text)) %>% 
#   group_by(mapped_name, result_text, result_as_real) %>% summarise(tot = n()) %>% arrange(mapped_name, desc(tot))

# data transformations 
# =====================

# parse blood pressure


# RICHMOND AGITATION SEDATION SCORE, National early warning Score, PAIN SCORE AT REST, PAIN SCORE AT MOVEMENT have both text and real
# filter out OXYGEN DELIVERY METHOD where low values (free entry text containing ventilation info)
# INVASIVE VENTILATION YES/NO looks like it's recording whether the person is for that (rather than it actually happening)
# recode ACvPU ?
# respiratory assist status has useful values as text
# check whether the mask eg ETT is actually happening in ED or delayed update - but maybe the model could make use of this ?? 

# transform data
# ==============

flowsheet_raw <- flowsheet_raw %>%
  filter(!(is.na(result_as_real) & is.na(result_text))) %>% 
  mutate(mapped_name = tolower(gsub(" ","_", mapped_name))) %>% 
  select(mrn, csn, fk_bed_moves, flowsheet_datetime, everything())

flowsheet_raw <- flowsheet_raw %>%
  mutate(result_as_real = case_when(!is.na(as.numeric(result_text)) ~ as.numeric(result_text), 
                                    TRUE ~ result_as_real))

# use this to view the values in the remaining result_text rows
# n = flowsheet_raw %>% filter(is.na(result_as_real)) %>% distinct(mapped_name)
# 
# for (m in (unique(n$mapped_name))) {
#   print(m)
#   print(flowsheet_raw %>% filter(is.na(result_as_real), mapped_name == m) %>% count(result_text))
# }

flowsheet_raw <- flowsheet_raw %>%
  mutate(keep_row = case_when(result_text == "\"\"" & is.na(result_as_real) ~ FALSE,
                       TRUE ~ TRUE)) %>% 
  filter(keep_row) %>% select(-keep_row)
                               

# # use this to see which text values still need to be transformed now
# flowsheet_text <- flowsheet_clean %>% filter(!is.na(result_text), is.na(result_as_real)) 

bp <- flowsheet_raw %>%
  filter(mapped_name == "blood_pressure") %>% 
  select(mrn, csn, fk_bed_moves, flowsheet_datetime, result_text) %>% 
  separate(result_text, into = c("bp_sys","bp_dia"), sep = "/") %>% 
  mutate(bp_sys = as.numeric(bp_sys),
         bp_dia = as.numeric(bp_dia)) %>% 
    pivot_longer(!mrn:flowsheet_datetime, names_to = "mapped_name", values_to = "result_as_real"
    )

acvpu <- flowsheet_raw %>%
  filter(mapped_name == "acvpu", !is.na(result_text)) %>% 
  select(mrn, csn, fk_bed_moves, flowsheet_datetime, result_text) %>% 
  mutate(mapped_name = paste0("acvpu_",result_text),
         result_as_real = 1) %>%  select(-result_text) 

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
  filter(!mapped_name %in% c("blood_pressure","acvpu","respiratory_assist_status",
                             "oxygen_delivery_method", "invasive_ventilation_yes/no",
                             "ventilator_mode_",
                             "ventilator_yes/no")) %>% 
  bind_rows(bp, acvpu, ventilation_data, respiratory_assist_status, oxygen_delivery_method) %>% 
  arrange(mrn, csn, flowsheet_datetime, mapped_name)

# process data for ML
# ==================

# add elapsed time of result
flowsheet_real <- flowsheet_real %>%
  left_join(ED_csn_summ %>% select(mrn, csn, arrival_dttm)) %>% 
  mutate(elapsed_mins = as.numeric(difftime(flowsheet_datetime, arrival_dttm, units = "mins"))) %>% 
  select(-arrival_dttm)


# calculate number of results
flowsheet_num_results <- flowsheet_raw %>% 
  group_by(mrn, csn, fk_bed_moves, mapped_name) %>% 
  summarise(num_results = n())




# Save data
# =========
               
outFile = paste0("EDcrowding/predict-admission/data-raw/flowsheet_real_",today(),".rda")
save(flowsheet_real, file = outFile)

outFile = paste0("EDcrowding/predict-admission/data-raw/flowsheet_num_results_",today(),".rda")
save(flowsheet_num_results, file = outFile)
