# About this file
# ==============

# Loads bed_moves adds age and gender and saves two versions
# - matrix_csn - one row per patient
# - matrix_loc - one row per patient and location



# Load libraries ----------------------------------------------------------



library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)

# Create functions --------------------------------------------------------

rpt <- function(dataframe) {
  print(dataframe %>% select(csn) %>% n_distinct())
}


# Load data ---------------------------------------------------------------



load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_2021-01-12.rda")
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-01-12.rda")

summ <- data.table(ED_csn_summ)
rpt(moves)
rpt(summ)


# Filter self-discharges (from ED only) -----------------------------------

# filter out self-discharges - only applies to those who discharged from ED

against_med <- ED_csn_summ %>% filter(discharge_disposition == "AGAINST MED", patient_class == "EMERGENCY") %>%
  mutate(against_med = TRUE)

ED_csn_summ <- ED_csn_summ %>% left_join(
  against_med %>% select(csn, against_med)
) %>% filter(is.na(against_med)) %>% select(-against_med)

# check number of csns at this point
rpt(ED_csn_summ) #
moves = moves[csn %in% ED_csn_summ$csn]
rpt(moves)

# Delete patients who died in ED ------------------------------------------

# delete patients who died on the day of being in ED
died <- ED_csn_summ %>% filter(discharge_destination == "Patient Died", patient_class == "EMERGENCY") %>%
  mutate(died_in_ED = TRUE)

ED_csn_summ <- ED_csn_summ %>% left_join(
  died %>% select(csn, died_in_ED)
) %>% filter(is.na(died_in_ED)) %>% select(-died_in_ED)


# check number of csns at this point
rpt(ED_csn_summ) #
moves = moves[csn %in% ED_csn_summ$csn]
rpt(moves)


# Add relevant transition time to ED_csn_summ -------------------------------------------
# Inside includes locations which have been 

ED_csn_summ <- ED_csn_summ %>% left_join(
  as_tibble(unique(moves[,.(csn, first_ED_admission, first_outside_ED_admission, first_outside_proper_admission, last_ED_discharge, last_inside_discharge)]))
)

rpt(ED_csn_summ)


# remove rows where total visit less than 5 minutes
ED_csn_summ %>% filter(difftime(last_ED_discharge, presentation_time , units = "mins") <5) %>% select(csn) %>% n_distinct() # 785
ED_csn_summ <- ED_csn_summ %>% filter(difftime(last_ED_discharge, presentation_time , units = "mins") >= 5)

# check number of csns at this point
rpt(ED_csn_summ) #
moves = moves[csn %in% ED_csn_summ$csn]
rpt(moves)


# Add bed move data -------------------------------------------------------


# truncate bed moves so that any rows with admission greater than the earliest ED discharge get deleted
bed_moves <- moves[!(outside)]
rpt(bed_moves)


# check number of csns at this point
bed_moves %>% select(csn) %>% n_distinct() #152429


# Save data ---------------------------------------------------------------

bed_moves <- bed_moves %>% arrange(csn, admission)
save(bed_moves, file = paste0('EDcrowding/predict-admission/data-raw/ED_bed_moves_final_csns_',today(),'.rda'))


# save final csns for future use with flowsheets and labs
save(ED_csn_summ, file = paste0('EDcrowding/predict-admission/data-raw/ED_csn_summ_final_csns_',today(),'.rda'))



