
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


load("~/EDcrowding/predict-admission/data-raw/obs_raw_2021-01-25.rda")
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-01-25.rda")
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-01-25.rda")

obs_real <- data.table(obs_raw)

# remove flowsheet csns that are not included 
obs_real <- obs_real[csn %in% summ$csn]
setkey(obs_real, csn)

# remove obs that take place after ED
obs_real <- merge(obs_real, summ[,.(csn, first_ED_admission, last_ED_discharge)]) 
obs_real <- obs_real[observation_datetime <= last_ED_discharge]

# add elapsed time
obs_real[, elapsed_mins := as.numeric(difftime(observation_datetime, first_ED_admission, units = "mins"))]

# for now exclude all except main flowsheet values
# mapping of obs visit id not yet available
# look up types here:  https://docs.google.com/spreadsheets/d/1k5DqkOfUkPZnYaNRgM-GrM7OC2S4S2alIiyTC8-OqCw/edit#gid=1661666003


obs_real[, include := if_else(as.character(visit_observation_type_id) %in% 
                              c("2273987897", #O2 sat (ID in application = 10)
                                "2273987932", # news (ID = 28315)
                                "2273987924", # resp rate (ID = 9)
                                "2273987904", # heart rate (ID = 8)
                                "2273987915", # temp (ID = 6)))]
                                "2273987929", # acvpu
                                "2273987901", # bp
                                "2273987935" # resp assist
                                ), TRUE, FALSE)]


obs_real <- obs_real[(include)]
obs_real[, visit_observation_type_id := as.character(visit_observation_type_id)]

# tidy labels

# abbrev= c("acvpu", "pain_nonverbal",  "bp", "temp", "o2_flowrate", "coma_score", "heart_rate",
#           "ideal_weight", "art_pressure_inv", "vent_inv_yes_no", "morphine_dose", "news",
#           "art_pressure_noninv", "o2_conc", "o2_delivery_method", "o2_sat", "pain_move", "pain_rest",
#           "pip", "peep", "resp_assist", "resp_rate", "rass",
#           "tidal_vol", "vent_mode" , "vent_yes_no"
# )

abbrev= c("o2_sat", "bp", "heart_rate", "temp", "resp_rate", "acvpu", "news", "resp_assist")
      
name_lookup <- data.table(bind_cols(unique(obs_real$visit_observation_type_id)[order(unique(obs_real$visit_observation_type_id))],
                         abbrev))
colnames(name_lookup) <- c("visit_observation_type_id", "meas")

obs_real <- merge(obs_real, name_lookup, by = "visit_observation_type_id")
obs_real[, c("visit_observation_type_id", "include") := NULL]
                                                      


# Transform data ----------------------------------------------------------


# convert acvpu
obs_real[, value_as_real := case_when(meas == "acvpu" & value_as_text == "A" ~ 1,
                                 meas == "acvpu" & value_as_text == "C" ~ 2,
                                 meas == "acvpu" & value_as_text == "V" ~ 3,
                                 meas == "acvpu" & value_as_text == "P" ~ 4,
                                 meas == "acvpu" & value_as_text == "U" ~ 5,
                                 TRUE ~ value_as_real
                                                    )]
                               
# convert bp

bp <- as_tibble(obs_real[meas == "bp"]) %>% select(-value_as_real, -meas) %>% 
  separate(value_as_text, into = c("bp_sys","bp_dia"), sep = "/") %>% 
  mutate(bp_sys = as.numeric(bp_sys),
         bp_dia = as.numeric(bp_dia)) %>% 
  pivot_longer(bp_sys:bp_dia, names_to = "meas", values_to = "value_as_real"
  )

bp <- bp %>% mutate(value_as_text = NA)

obs_real <- bind_rows(obs_real[meas != "bp"], bp)

# convert resp assist type

obs_real[, value_as_real := case_when(meas == "resp_assist" & value_as_text == "Supplemental Oxygen" ~ 1,
                                    meas == "resp_assist" & value_as_text == "Room air" ~ 0,
                                    TRUE ~ value_as_real)]


# convert news
obs_real[meas == "news", value_as_real := as.numeric(value_as_text)]

# remove outliers
obs_real <- obs_real %>%
  mutate(value_as_real = case_when(value_as_real >115 & meas == "temp" ~ NA_real_,
                                    TRUE ~ value_as_real))
obs_real <- obs_real %>%
  filter(!is.na(value_as_real))

# Save data ---------------------------------------------------------------


# create final dataset of results (real values)
obs_real <- obs_real[, .(csn, observation_datetime, value_as_real, meas, first_ED_admission, last_ED_discharge, elapsed_mins)]


outFile = paste0("EDcrowding/predict-admission/data-raw/obs_real_",today(),".rda")
save(obs_real, file = outFile)

