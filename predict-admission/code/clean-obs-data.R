
# About this file ---------------------------------------------------------

# Takes flowsheet data and cleans it ready for creating design matrix
# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)
library(data.table)
library(readr)




# Load data ---------------------------------------------------------------

load("~/EDcrowding/predict-admission/data-raw/obs_raw_2021-03-01.rda")
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-03-01.rda")
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-03-01.rda")


# mapping of obs visit id not yet availablein Star
# look up mapping here:  https://docs.google.com/spreadsheets/d/1k5DqkOfUkPZnYaNRgM-GrM7OC2S4S2alIiyTC8-OqCw/edit#gid=1661666003

vo_mapping <- read_csv("~/Emap Mapping Spreadsheet - all questions.csv") %>% data.table()

vo_mapping = vo_mapping[,.(`Friendly name`, `epic id`)]
setnames(vo_mapping, "Friendly name", "obs_name")
setnames(vo_mapping, "epic id", "id_in_application")

vo_type <- read_delim("~/expdata visit_obervation_type.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE) %>% data.table()

setnames(vo_type, "visit_observation_type", "visit_observation_type_id")
vo_mapping = unique(merge(vo_mapping, vo_type[,.(visit_observation_type_id, id_in_application)], by = "id_in_application"))

# these 5 types have multiple mappings
vo_mapping[,.N, by = visit_observation_type_id][N>1]
vo_mapping = unique(vo_mapping[, obs_name := max(obs_name), by = visit_observation_type_id])
vo_mapping[, vo_mapping := gsub(" ", "", obs_name)]

# Process data ------------------------------------------------------------


# remove flowsheet csns that are not included 
obs_real <- data.table(obs_raw)
obs_real <- obs_real[csn %in% summ$csn]
setkey(obs_real, csn)

# remove obs that take place after ED
obs_real <- merge(obs_real, summ[,.(csn, first_ED_admission, last_ED_discharge)]) 
obs_real <- obs_real[observation_datetime <= last_ED_discharge]

# add elapsed time
obs_real[, elapsed_mins := as.numeric(difftime(observation_datetime, first_ED_admission, units = "mins"))]

# add obs description
obs_real[, visit_observation_type_id := as.character(visit_observation_type_id)]
vo_mapping[, visit_observation_type_id := as.character(visit_observation_type_id)]

obs_real = merge(obs_real, vo_mapping, by = "visit_observation_type_id", allow.cartesian=TRUE, all.x = TRUE)

# remove rows where there is no obs_name - ie not in mapping
obs_real = obs_real[(!is.na(obs_name))]

# remove observations whare are used less than 50 times
obs_real[, times_used := .N, by = obs_name]
obs_real = obs_real[times_used > 50]

# remove reported symptoms as this is free text and not widely used (only 1825 csns from Jan 2020 to end Feb 2021)
obs_real = obs_real[obs_name != "Reportedsymptomsonadmission"]

# Transform data ----------------------------------------------------------


# convert ACVPU
obs_real[, value_as_real := case_when(obs_name == "ACVPU" & value_as_text == "A" ~ 1,
                                 obs_name == "ACVPU" & value_as_text == "C" ~ 2,
                                 obs_name == "ACVPU" & value_as_text == "V" ~ 3,
                                 obs_name == "ACVPU" & value_as_text == "P" ~ 4,
                                 obs_name == "ACVPU" & value_as_text == "U" ~ 5,
                                 TRUE ~ value_as_real
                                                    )]
                               
# convert Bloodpressure

Bloodpressure <- as_tibble(obs_real[obs_name == "Bloodpressure"]) %>% select(-value_as_real, -obs_name) %>% 
  separate(value_as_text, into = c("Bloodpressure_sys","Bloodpressure_dia"), sep = "/") %>% 
  mutate(Bloodpressure_sys = as.numeric(Bloodpressure_sys),
         Bloodpressure_dia = as.numeric(Bloodpressure_dia)) %>% 
  pivot_longer(Bloodpressure_sys:Bloodpressure_dia, names_to = "obs_name", values_to = "value_as_real"
  )

Bloodpressure <- Bloodpressure %>% mutate(value_as_text = NA)

obs_real <- bind_rows(obs_real[obs_name != "Bloodpressure"], Bloodpressure)

# convert resp assist type

obs_real[, value_as_real := case_when(obs_name == "Roomairoroxygen" & value_as_text == "Supplemental Oxygen" ~ 1,
                                    obs_name == "Roomairoroxygen" & value_as_text == "Room air" ~ 0,
                                    TRUE ~ value_as_real)]


# convert text to numeric where straightfoward
obs_real[obs_name == "NEWSscore", value_as_real := as.numeric(value_as_text)]
obs_real[obs_name == "NEWS2score", value_as_real := as.numeric(value_as_text)]
obs_real[obs_name == "RASS", value_as_real := as.numeric(value_as_text)]
obs_real[obs_name == "Painscore-verbalatrest", value_as_real := as.numeric(value_as_text)]
obs_real[obs_name == "Painscore-verbalonmovement", value_as_real := as.numeric(value_as_text)]

# remove outliers
obs_real <- obs_real %>%
  mutate(value_as_real = case_when(value_as_real >115 & obs_name == "Temperature" ~ NA_real_,
                                    TRUE ~ value_as_real))

# # convert factors
# 
# for (obs_name_ in names(table(obs_real[is.na(value_as_real), obs_name]))) {
#   print(obs_name_)
#   print(table(obs_real[obs_name == obs_name_, value_as_text]))
#  # obs_real[obs_name == obs_name_, value_as_factor := factor(value_as_text)]
# }
# 
# obs_real[is.na(value_as_real), value_as_factor := factor(value_as_text)]
# 
# obs_real <- obs_real %>%
#   filter(!is.na(value_as_real))

# Save data ---------------------------------------------------------------


# create final dataset of results (real values)
obs_real <- obs_real[, .(csn, observation_datetime, value_as_real, obs_name, first_ED_admission, last_ED_discharge, elapsed_mins)]


outFile = paste0("EDcrowding/predict-admission/data-raw/obs_real_",today(),".rda")
save(obs_real, file = outFile)

