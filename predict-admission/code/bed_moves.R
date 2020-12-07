# About this file
# ==============

# Loads bed_moves adds age and gender and saves two versions
# - matrix_csn - one row per patient
# - matrix_loc - one row per patient and location


# load libraries
# ==============

library(dplyr)
library(tidyverse)
library(lubridate)


# load data
# =========

# get original set of csns that I was working with before

load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-11-04.rda")
ED_csn_summ_orig <- ED_csn_summ %>% filter(arrival_dttm < '2020-10-31') %>% select(csn_old) %>% distinct()
ED_csn_summ_orig <- ED_csn_summ_orig %>% rename(csn = csn_old)

# get new set of csns from the refactored STar with more variables

load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-12-02.rda")
ED_csn_summ_refactored <- ED_csn_summ %>%   
  mutate(adm = if_else(patient_class == "INPATIENT", TRUE, FALSE))

# subset of final csns includes those in original dataset that are also in the new one
final_csns <- ED_csn_summ_orig %>% inner_join(ED_csn_summ_refactored) %>% select(csn)

# create new csn_summ
ED_csn_summ <- ED_csn_summ_refactored %>% inner_join(final_csns)

# check number of csns at this point
ED_csn_summ %>% select(csn) %>% n_distinct() # 160123


# Filter self-discharges (from ED only) -----------------------------------

# filter out self-discharges - only applies to those who discharged from ED
against_med <- ED_csn_summ %>% filter(discharge_disposition == "AGAINST MED", patient_class == "EMERGENCY") %>%
  mutate(against_med = TRUE)

ED_csn_summ <- ED_csn_summ %>% left_join(
  against_med %>% select(csn, against_med)
) %>% filter(is.na(against_med)) %>% select(-against_med)


# check number of csns at this point
ED_csn_summ %>% select(csn) %>% n_distinct() #153241



# Delete patients who died in ED ------------------------------------------

# delete patients who died on the day of being in ED
died <- ED_csn_summ %>% filter(discharge_destination == "Patient Died", patient_class == "EMERGENCY") %>%
  mutate(died_in_ED = TRUE)

ED_csn_summ <- ED_csn_summ %>% left_join(
  died %>% select(csn, died_in_ED)
) %>% filter(is.na(died_in_ED)) %>% select(-died_in_ED)


# check number of csns at this point
ED_csn_summ %>% select(csn) %>% n_distinct() #153214


# Decide last ED admission time -------------------------------------------


# get stats
ED_csn_summ %>% filter(max_emerg_class > last_ED_discharge_time) %>% select(csn) %>% n_distinct() #6463
ED_csn_summ %>% filter(max_emerg_class < last_ED_discharge_time) %>% select(csn) %>% n_distinct() #8216
ED_csn_summ %>% filter(max_emerg_class == last_ED_discharge_time) %>% select(csn) %>% n_distinct() #138535

# work out the earliest of (a) class change to inpatient or (b) last ED discharge time
# for discharged patients, use last ED discharge time from bed move information
# for admitted patients, use earliest of last bed_move and change of patient class
ED_csn_summ <- ED_csn_summ %>%  mutate(ED_discharge_earliest = 
                                         case_when(adm ~ pmin(max_emerg_class, last_ED_discharge_time),
                                                   !adm ~ last_ED_discharge_time))



# remove rows where total visit less than 5 minutes
ED_csn_summ %>% filter(difftime(ED_discharge_earliest, presentation_time , units = "mins") <5) %>% select(csn) %>% n_distinct() # 785
ED_csn_summ <- ED_csn_summ %>% filter(difftime(ED_discharge_earliest, presentation_time , units = "mins") >= 5)

# check number of csns at this point
ED_csn_summ %>% select(csn) %>% n_distinct() #152429

# create record of final csns
final_csns <- ED_csn_summ %>% select(csn) 

# create final ED duration
ED_csn_summ <- ED_csn_summ %>% 
  mutate(ED_duration_final = difftime(ED_discharge_earliest, presentation_time , units = "mins"))


# Add bed move data -------------------------------------------------------


# load latest bed move data
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_2020-12-02.rda")


# join with ED_bed_moves (only rows in ED) and join with ED_csn_summ to get final admission status
bed_moves <- ED_bed_moves %>% inner_join(final_csns) %>% 
  filter(ED_row == 1) %>% 
  left_join(ED_csn_summ %>% select(csn, ED_discharge_earliest, presentation_time)) %>% 
  arrange(csn, admission)

# truncate bed moves so that any rows with admission greater than the earliest ED discharge get deleted
bed_moves <- bed_moves %>% filter(admission < ED_discharge_earliest)

# check number of csns
bed_moves %>% filter(admission < ED_discharge_earliest) %>% select(csn) %>% n_distinct()

# mutate any rows with discharge greater than the earliest ED discharge so that discharge gets truncated

bed_moves <- bed_moves %>% mutate(discharge = case_when(discharge > ED_discharge_earliest ~ ED_discharge_earliest, 
                               TRUE ~ discharge))


# check number of csns at this point
bed_moves %>% select(csn) %>% n_distinct() #152429
                                   
# 
# # generate vars for time in location
# bed_moves <- bed_moves %>%  
#   mutate(loc_admission = floor(as.numeric(difftime(admission, presentation_time, units = "mins"))),
#          loc_discharge = floor(as.numeric(difftime(discharge, presentation_time, units = "mins"))),
#          loc_duration = loc_discharge - loc_admission) %>% select(-admission, -discharge) %>% 
#   mutate(covid_loc_duration = case_when(covid_pathway ~ loc_duration,
#                                         TRUE ~ NA_real_))
# 

# select cols for matrix
# 
# matrix_loc <- bed_moves %>% select(mrn, sex, csn, csn_old, arrival_dttm, epoch, age, pk_bed_moves, room4,
#                                    loc_admission,	loc_discharge, loc_duration, adm) %>% ungroup()
# 
# save(matrix_loc, file = paste0('EDcrowding/predict-admission/data-raw/matrix_loc_',today(),'.rda'))

# 
# matrix_csn <- ED_csn_summ %>% 
#   select(csn, adm, sex, age, epoch,
#          presentation_time, arrival_method, max_emerg_class, last_ED_discharge_time) %>% 
#   left_join( 
#     bed_moves %>% select(csn, loc_duration, room4) %>% 
#       group_by(csn, room4) %>% 
#       summarise(loc_duration = sum(loc_duration)) %>% 
#       pivot_wider(names_from = room4, names_prefix = "loc_duration_", values_from = loc_duration)) %>% 
#   left_join(
#     bed_moves %>% select(csn, covid_loc_duration) %>% 
#       group_by(csn) %>% 
#       summarise(covid_duration = sum(covid_loc_duration)) 
# )
# 


# Save data ---------------------------------------------------------------

bed_moves <- bed_moves %>% arrange(csn, admission)
save(bed_moves, file = paste0('EDcrowding/predict-admission/data-raw/ED_bed_moves_final_csns_',today(),'.rda'))


# save final csns for future use with flowsheets and labs
save(final_csns, file = paste0('EDcrowding/predict-admission/data-raw/final_csns_',today(),'.rda'))
save(ED_csn_summ, file = paste0('EDcrowding/predict-admission/data-raw/ED_csn_summ_final_csns_',today(),'.rda'))



