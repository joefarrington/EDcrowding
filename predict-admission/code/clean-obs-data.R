
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

load("~/EDcrowding/predict-admission/data-raw/obs_raw_2021-04-14.rda")
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-04-13.rda")
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-04-13.rda")

summ[, left_ED := coalesce(first_outside_proper_admission, last_inside_discharge)]


# mapping of obs visit id not yet availablein Star
# look up mapping here:  https://docs.google.com/spreadsheets/d/1k5DqkOfUkPZnYaNRgM-GrM7OC2S4S2alIiyTC8-OqCw/edit#gid=1661666003

vo_mapping <- read_csv("~/Emap Mapping Spreadsheet - all questions.csv") %>% data.table()

vo_mapping = vo_mapping[,.(`Friendly name`, `epic id`)]
setnames(vo_mapping, "Friendly name", "obs_name")
setnames(vo_mapping, "epic id", "id_in_application")

# these 5 types have multiple mappings
vo_mapping[,.N, by = id_in_application][N>1]
vo_mapping = unique(vo_mapping[, obs_name := max(obs_name), by = id_in_application])
vo_mapping[, obs_name := gsub(" ", "", obs_name)]

# Process data ------------------------------------------------------------


# remove flowsheet csns that are not included 
obs_real <- data.table(obs_raw)
obs_real <- obs_real[csn %in% summ$csn]
setkey(obs_real, csn)

# remove obs that take place after ED
obs_real <- merge(obs_real, summ[,.(csn, first_ED_admission, left_ED)]) 
obs_real <- obs_real[observation_datetime <= left_ED]

# add elapsed time
obs_real[, elapsed_mins := as.numeric(difftime(observation_datetime, first_ED_admission, units = "mins"))]

# remove obs from prior to ED by more than 2 hours

obs_real <- obs_real[elapsed_mins >= -120]


# add obs description
obs_real[, id_in_application := as.numeric(id_in_application)]

obs_real = merge(obs_real, vo_mapping, by = "id_in_application", allow.cartesian=TRUE, all.x = TRUE)

# remove rows where there is no obs_name - ie not in mapping
obs_real = obs_real[(!is.na(obs_name))]

# remove observations whare are used less than 500 times
obs_real[, times_used := .N, by = obs_name]
obs_real = obs_real[times_used > 500]

# remove reported symptoms as this is free text and not widely used (only 1825 csns from Jan 2020 to end Feb 2021)
obs_real = obs_real[obs_name != "Reportedsymptomsonadmission"]

# Transform data ----------------------------------------------------------

# Temperature measured in both celcius and farenheit
# Note that making a manual conversion will generate a different values to the original temp value
obs_real[obs_name == "Temperature", value_as_real := (value_as_real - 32) * 5/9]
obs_real[obs_name %in% c("Temperature", "Temp(inCelsius)"), num_temp_in_dttm := .N, by = .(csn, observation_datetime)]
obs_real[num_temp_in_dttm == 2 & obs_name == "Temperature", delete_row := TRUE]
obs_real = obs_real[is.na(delete_row)]
obs_real[obs_name == "Temperature", obs_name := "Temp(inCelsius)"]

# remove duplicate measurements (there are still a bunch with multiple temp measurements in one obs event)
obs_real[obs_name %in% c("Temperature", "Temp(inCelsius)"), num_temp_in_dttm := .N, by = .(csn, observation_datetime)]
obs_real[num_temp_in_dttm > 1]

# first delete cols id_in_application and visit_observation_type_id as these have diff values for celcius and f meas
obs_real[, id_in_application := NULL]
obs_real[, visit_observation_type_id := NULL]
obs_real = unique(obs_real)

# convert ACVPU to numeric
obs_real[, value_as_real := case_when(obs_name == "ACVPU" & value_as_text == "A" ~ 1,
                                 obs_name == "ACVPU" & value_as_text == "C" ~ 2,
                                 obs_name == "ACVPU" & value_as_text == "V" ~ 3,
                                 obs_name == "ACVPU" & value_as_text == "P" ~ 4,
                                 obs_name == "ACVPU" & value_as_text == "U" ~ 5,
                                 TRUE ~ value_as_real
                                                    )]
                               
# convert Bloodpressure to numeric

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
  mutate(value_as_real = case_when(value_as_real >46 & obs_name == "Temp(inCelsius)" ~ NA_real_,
                                    TRUE ~ value_as_real))


# Save data ---------------------------------------------------------------


# create final dataset of results (real values)
obs_real <- obs_real[, .(csn, observation_datetime, value_as_real, obs_name, elapsed_mins)]

# remove any punctuation that will make column names problematic
obs_real[, obs_name := gsub("\\(|\\)|\\-|\\>|\\?|\\/","", obs_name)]
obs_real[, obs_name := gsub("Currenttemperature>37.5orhistoryoffeverinthelast24hours","Fever", obs_name)]


outFile = paste0("EDcrowding/predict-admission/data-raw/obs_real_",today(),".rda")
save(obs_real, file = outFile)

