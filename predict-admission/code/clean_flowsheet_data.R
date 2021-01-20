
# About this file ---------------------------------------------------------

# Takes flowsheet data and cleans it ready for creating design matrix
# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)
library(data.table)


# load data
# =========


load("~/EDcrowding/predict-admission/data-raw/fs_real_2021-01-19.rda")
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-01-19.rda")
load("~/EDcrowding/predict-admission/data-raw/ED_csn_summ_final_csns_2021-01-19.rda")

fs_real <- data.table(fs_raw)

# remove flowsheet csns that are not included 
fs_real <- fs_real[csn %in% ED_csn_summ$csn]

dt <- data.table(ED_csn_summ %>% select(csn, first_ED_admission, last_ED_discharge))

# add elapsed time
fs_real <- merge(fs_real, dt) 
fs_real <- fs_real[observation_datetime <= last_ED_discharge]
fs_real[, elapsed_mins := as.numeric(difftime(observation_datetime, first_ED_admission, units = "mins"))]

# for now exclude all except main flowsheet values
# mapping of obs visit id not yet available
# look up types here:  https://docs.google.com/spreadsheets/d/1k5DqkOfUkPZnYaNRgM-GrM7OC2S4S2alIiyTC8-OqCw/edit#gid=1661666003


fs_real[, include := if_else(as.character(visit_observation_type_id) %in% 
                              c("2273987897", #O2 sat (ID in application = 10)
                                "2273987932", # news (ID = 28315)
                                "2273987924", # resp rate (ID = 9)
                                "2273987904", # heart rate (ID = 8)
                                "2273987915", # temp (ID = 6)))]
                                "2273987929", # acvpu
                                "2273987901", # bp
                                "2273987935" # resp assist
                                ), TRUE, FALSE)]


fs_real <- fs_real[(include)]
fs_real[, visit_observation_type_id := as.character(visit_observation_type_id)]

# tidy labels

# abbrev= c("acvpu", "pain_nonverbal",  "bp", "temp", "o2_flowrate", "coma_score", "heart_rate",
#           "ideal_weight", "art_pressure_inv", "vent_inv_yes_no", "morphine_dose", "news",
#           "art_pressure_noninv", "o2_conc", "o2_delivery_method", "o2_sat", "pain_move", "pain_rest",
#           "pip", "peep", "resp_assist", "resp_rate", "rass",
#           "tidal_vol", "vent_mode" , "vent_yes_no"
# )

abbrev= c("o2_sat", "bp", "heart_rate", "temp", "resp_rate", "acvpu", "news", "resp_assist")
      
name_lookup <- data.table(bind_cols(unique(fs_real$visit_observation_type_id)[order(unique(fs_real$visit_observation_type_id))],
                         abbrev))
colnames(name_lookup) <- c("visit_observation_type_id", "meas")

fs_real <- merge(fs_real, name_lookup, by = "visit_observation_type_id")
fs_real[, c("visit_observation_type_id", "include") := NULL]
                                                      


# Transform data ----------------------------------------------------------


# convert acvpu
fs_real[, value_as_real := case_when(meas == "acvpu" & value_as_text == "A" ~ 1,
                                 meas == "acvpu" & value_as_text == "C" ~ 2,
                                 meas == "acvpu" & value_as_text == "V" ~ 3,
                                 meas == "acvpu" & value_as_text == "P" ~ 4,
                                 meas == "acvpu" & value_as_text == "U" ~ 5,
                                 TRUE ~ value_as_real
                                                    )]
                               
# convert bp

bp <- as_tibble(fs_real[meas == "bp"]) %>% select(-value_as_real, -meas) %>% 
  separate(value_as_text, into = c("bp_sys","bp_dia"), sep = "/") %>% 
  mutate(bp_sys = as.numeric(bp_sys),
         bp_dia = as.numeric(bp_dia)) %>% 
  pivot_longer(bp_sys:bp_dia, names_to = "meas", values_to = "value_as_real"
  )

bp <- bp %>% mutate(value_as_text = NA)

fs_real <- bind_rows(fs_real[meas != "bp"], bp)

# convert resp assist type

fs_real[, value_as_real := case_when(meas == "resp_assist" & value_as_text == "Supplemental Oxygen" ~ 1,
                                    meas == "resp_assist" & value_as_text == "Room air" ~ 0,
                                    TRUE ~ value_as_real)]


# convert news
fs_real[meas == "news", value_as_real := as.numeric(value_as_text)]

# remove outliers
fs_real <- fs_real %>%
  mutate(value_as_real = case_when(value_as_real >115 & meas == "temp" ~ NA_real_,
                                    TRUE ~ value_as_real))
fs_real <- fs_real %>%
  filter(!is.na(value_as_real))

# Save data ---------------------------------------------------------------


# create final dataset of results (real values)
fs_real <- fs_real[, .(csn, observation_datetime, value_as_real, meas, first_ED_admission, last_ED_discharge, elapsed_mins)]


outFile = paste0("EDcrowding/predict-admission/data-raw/fs_real_",today(),".rda")
save(fs_real, file = outFile)

