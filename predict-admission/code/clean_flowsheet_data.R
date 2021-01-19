
# About this file ---------------------------------------------------------

# Takes flowsheet data and cleans it ready for creating design matrix
# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)


# load data
# =========


load("~/EDcrowding/predict-admission/data-raw/fs_raw_2021-01-19.rda")
load("~/EDcrowding/predict-admission/data-raw/ED_bed_moves_final_csns_2021-01-19.rda")
load("~/EDcrowding/predict-admission/data-raw/ED_csn_summ_final_csns_2021-01-19.rda")

fs_raw <- data.table(fs_raw)

# remove flowsheet csns that are not included 
fs_raw <- fs_raw[csn %in% ED_csn_summ$csn]

dt <- data.table(ED_csn_summ %>% select(csn, presentation_time, last_ED_discharge))

# add elapsed time
fs_raw <- merge(fs_raw, dt) 
fs_raw <- fs_raw[observation_datetime <= last_ED_discharge]
fs_raw[, elapsed_mins := as.numeric(difftime(observation_datetime, presentation_time, units = "mins"))]

# # tidy labels
# 
# abbrev= c("acvpu", "pain_nonverbal",  "bp", "temp", "o2_flowrate", "coma_score", "heart_rate", 
#           "ideal_weight", "art_pressure_inv", "vent_inv_yes_no", "morphine_dose", "news", 
#           "art_pressure_noninv", "o2_conc", "o2_delivery_method", "o2_sat", "pain_move", "pain_rest",
#           "pip", "peep", "resp_assist", "resp_rate", "rass", 
#            "tidal_vol", "vent_mode" , "vent_yes_no" 
#            )
# name_lookup <- bind_cols(unique(fs_raw$mapped_name)[order(unique(fs_raw$mapped_name))], 
#                          abbrev)
# colnames(name_lookup) <- c("mapped_name", "meas")
# 
# flowsheet_raw_excluded_csns <- flowsheet_raw_excluded_csns %>% 
#   left_join(name_lookup) %>% select(-mapped_name)


# for now exclude all except main flowsheet values

meas_ <- fs_raw[, .N, by = visit_observation_type_id]
meas_[, include := N > 60000]

fs_raw <- fs_raw[visit_observation_type_id %in% meas_[(include), visit_observation_type_id]]
setnames(fs_raw, "visit_observation_type_id", "meas")

fs_raw[, meas := as.character(meas)]
                                                      
# flowsheet_raw_excluded_csns <- flowsheet_raw_excluded_csns %>% 
#   filter(meas %in% c("acvpu", "bp", "temp",  "heart_rate",  "news", 
#                                                     "o2_sat", "resp_assist", "resp_rate"))

meas_[(include), visit_observation_type_id][1]
meas_[(include), visit_observation_type_id][2] # blood pressure
meas_[(include), visit_observation_type_id][3] # O2 sat?
meas_[(include), visit_observation_type_id][4] # O2 sat - measured in %
meas_[(include), visit_observation_type_id][5] 
meas_[(include), visit_observation_type_id][6]
meas_[(include), visit_observation_type_id][7] # resp assist type e.g. room air
meas_[(include), visit_observation_type_id][8] # ACVPU

# transform data
# ==============

# remove blank rows

fs_raw[is.na(value_as_real) & is.na(value_as_text)] # no rows



# convert acvpu
fs_raw[, meas := if_else(meas == "2273987929", "acvpu", meas)]
fs_raw[, value_as_real := case_when(meas == "acvpu" & value_as_text == "A" ~ 1,
                                 meas == "acvpu" & value_as_text == "C" ~ 2,
                                 meas == "acvpu" & value_as_text == "V" ~ 3,
                                 meas == "acvpu" & value_as_text == "P" ~ 4,
                                 meas == "acvpu" & value_as_text == "U" ~ 5,
                                 TRUE ~ value_as_real
                                                    )]
                               
# convert bp

bp <- as_tibble(fs_raw[meas == "2273987901"]) %>% select(-value_as_real, -meas) %>% 
  separate(value_as_text, into = c("bp_sys","bp_dia"), sep = "/") %>% 
  mutate(bp_sys = as.numeric(bp_sys),
         bp_dia = as.numeric(bp_dia)) %>% 
  pivot_longer(bp_sys:bp_dia, names_to = "meas", values_to = "value_as_real"
  )

bp <- bp %>% mutate(value_as_text = NA)

fs_raw <- bind_rows(fs_raw[meas != "2273987901"], bp)

# convert resp assist type

fs_raw[, meas := if_else(meas == "2273987935", "resp_assist", meas)]
fs_raw[, value_as_real := case_when(meas == "resp_assist" & value_as_text == "Supplemental Oxygen" ~ 1,
                                    meas == "resp_assist" & value_as_text == "Room air" ~ 0,
                                    TRUE ~ value_as_real)]

# create final dataset of results (real values)
fs_real <- fs_raw[, .(csn, observation_datetime, value_as_real, meas, presentation_time, last_ED_discharge, elapsed_mins)]


# create final dataset of number of results (count by csn)
fs_num <- fs_real[, .N, by = .(csn, meas)] %>% 
  pivot_wider(names_from = meas, values_from = N, values_fill = 0)



# Save data ---------------------------------------------------------------



outFile = paste0("EDcrowding/predict-admission/data-raw/fs_real_",today(),".rda")
save(fs_real, file = outFile)

outFile = paste0("EDcrowding/predict-admission/data-raw/fs_num_",today(),".rda")
save(fs_num, file = outFile)

