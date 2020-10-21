# About this file
# ==============

# 


# load libraries
# ==============

library(dplyr)
library(tidyverse)
library(lubridate)





# function to create design matrix
# ================================
 

create_design_matrix <- function (bed_moves, csn_summ, flowsheet_real, lab_real, cutoff) {
  
  # bed moves
  # =========
  
  print("Processing bed moves")
  
  # select only rows where the admission began before the cutoff time(includes rows where discharge is later)
  bed_moves_cutoff <- bed_moves %>% 
    left_join(ED_csn_summ %>% select(csn, ED_duration_final)) %>% 
    filter(ED_duration_final >= 60/cutoff, 
           admission <= cutoff) %>% 
    select(-csn_old, -pk_bed_moves, -discharge_dttm, -ED_duration_final)
  
  # update discharge time of rows that exceed cutoff time
  bed_moves_cutoff <- bed_moves_cutoff  %>% 
    group_by(mrn, csn, arrival_dttm, room4) %>% 
    mutate(discharge = case_when(discharge > cutoff & discharge == max(discharge) ~ cutoff, 
                                 TRUE ~ discharge)) %>% ungroup() %>% 
    select(-arrival_dttm )
  
  # collate to csn level
  bed_moves_cutoff_csn <- bed_moves_cutoff  %>% 
    mutate(loc_duration = discharge - admission) %>% 
    group_by(mrn, csn, room4) %>% 
    summarise(loc_duration = sum(as.numeric(loc_duration))) %>% 
    pivot_wider(names_from = room4, names_prefix = "mins_", values_from = loc_duration, values_fill = 0)
  
  
  
  # flowsheet data
  # ==============
  
  print("Processing flowsheet numbers")
  
  
  # remove outliers
  flowsheet_real <- flowsheet_real %>% 
    mutate(result_as_real = case_when(result_as_real >115 & meas == "temp" ~ NA_real_,
                                      TRUE ~ result_as_real))
  
  
  flowsheet_real <- flowsheet_real %>% 
    filter(!is.na(result_as_real))
  
  # select for cutoff
  flowsheet_cutoff <- flowsheet_real %>% ungroup() %>% 
    filter(elapsed_mins < cutoff) %>% 
    left_join(ED_csn_summ %>% select(csn, ED_duration_final)) %>% 
    filter(ED_duration_final >= 60/cutoff)  %>% select(-ED_duration_final, -fk_bed_moves, -flowsheet_datetime)
  
  # add number of results for csn
  flowsheet_cutoff_csn <- flowsheet_cutoff %>% ungroup() %>% 
    group_by(mrn, csn) %>% 
    summarise(num_fs_results = n()) %>% 
    mutate(has_fs_results = 1)
  
  # add number of types of results for csn
  flowsheet_cutoff_csn <- flowsheet_cutoff_csn %>% 
    left_join(
      flowsheet_cutoff %>% 
        group_by(mrn, csn) %>% 
        summarise(num_fs_types = n_distinct(meas)) 
    )
  
  # add number of flowsheet events
  flowsheet_cutoff_csn <- flowsheet_cutoff_csn %>% 
    left_join(
      flowsheet_cutoff %>% 
        group_by(mrn, csn) %>% 
        summarise(num_fs_events = n_distinct(elapsed_mins)) 
    )
  
  # add number of each measurement 
  flowsheet_cutoff_csn <- flowsheet_cutoff_csn %>% 
    left_join(
      flowsheet_cutoff %>% 
        group_by(mrn, csn, meas) %>% 
        summarise(num_meas = n()) %>% 
        pivot_wider(names_from = meas, names_prefix = "fs_num_", values_from = num_meas, values_fill = 0)    
    )
  
  print("Processing flowsheet values")
  
  # flowsheet values
  # add min score for each measurement
  flowsheet_cutoff_csn_val <- flowsheet_cutoff %>% 
    group_by(mrn, csn, meas) %>% 
    summarise(min_meas = min(result_as_real, na.rm = TRUE))  %>% 
    pivot_wider(names_from = meas, names_prefix = "fs_min_", values_from = min_meas)    
  
  # add max score for each measurement
  flowsheet_cutoff_csn_val <- flowsheet_cutoff_csn_val %>% 
    left_join(
      flowsheet_cutoff %>% 
        group_by(mrn, csn, meas) %>% 
        summarise(max_meas = max(result_as_real, na.rm = TRUE))  %>% 
        pivot_wider(names_from = meas, names_prefix = "fs_max_", values_from = max_meas)    
    )
  
  # add latest score for each measurement
  flowsheet_cutoff_csn_val <- flowsheet_cutoff_csn_val %>% 
    left_join(
      flowsheet_cutoff %>% 
        group_by(mrn, csn, meas) %>% 
        filter(elapsed_mins == max(elapsed_mins)) %>% 
        summarise(latest_meas = max(result_as_real, na.rm = TRUE))  %>%  # using max allows for possibility of two measurements in same minute
        pivot_wider(names_from = meas, names_prefix = "fs_latest_", values_from = latest_meas)    
    )
  
  
  
  # lab data
  # ========
  
  print("Processing lab numbers")
  
  
  lab_real <- lab_real %>% 
    filter(!is.na(result_as_real))
  
  # select for cutoff
  lab_cutoff <- lab_real %>% ungroup() %>% 
    filter(elapsed_mins < cutoff) %>% 
    left_join(ED_csn_summ %>% select(csn, ED_duration_final)) %>% 
    filter(ED_duration_final >= 60/cutoff)  %>% 
    select(-ED_duration_final, -fk_bed_moves, -result_datetime, -mapped_name)
  
  # add number of results for csn
  lab_cutoff_csn <- lab_cutoff %>% ungroup() %>% 
    group_by(mrn, csn) %>% 
    summarise(num_lab_results = n()) %>% 
    mutate(has_lab_results = 1)
  
  # add number of types of results for csn
  lab_cutoff_csn <- lab_cutoff_csn %>% 
    left_join(
      lab_cutoff %>% 
        group_by(mrn, csn) %>% 
        summarise(num_lab_types = n_distinct(local_code)) 
    )
  
  # add number of lab result events
  lab_cutoff_csn <- lab_cutoff_csn %>% 
    left_join(
      lab_cutoff %>% 
        group_by(mrn, csn) %>% 
        summarise(num_lab_events = n_distinct(elapsed_mins)) 
    )
  
  # add number of each lab 
  lab_cutoff_csn <- lab_cutoff_csn %>% 
    left_join(
      lab_cutoff %>% 
        group_by(mrn, csn, local_code) %>% 
        summarise(num_lab = n()) %>% 
        pivot_wider(names_from = local_code, names_prefix = "l_num_", values_from = num_lab, values_fill = 0)    
    )
  
  print("Processing lab values")
  
  
  # lab values
  # add min score for each lab result  - use separate matrix so that NAs can be retained in this join
  lab_cutoff_csn_val <- lab_cutoff %>% 
    group_by(mrn, csn, local_code) %>% 
    summarise(min_lab = min(result_as_real, na.rm = TRUE))  %>% 
    pivot_wider(names_from = local_code, names_prefix = "l_min_", values_from = min_lab)    
  
  # add max score for each result
  lab_cutoff_csn_val <- lab_cutoff_csn_val %>% 
    left_join(
      lab_cutoff %>% 
        group_by(mrn, csn, local_code) %>% 
        summarise(max_lab = max(result_as_real, na.rm = TRUE))  %>% 
        pivot_wider(names_from = local_code, names_prefix = "l_max_", values_from = max_lab)       
    )
  
  # add latest score for each result
  lab_cutoff_csn_val <- lab_cutoff_csn_val %>% 
    left_join(
      lab_cutoff %>% 
        group_by(mrn, csn, local_code) %>%  
        filter(elapsed_mins == max(elapsed_mins)) %>% 
        summarise(latest_lab = max(result_as_real, na.rm = TRUE))  %>%  # using max allows for possibility of two measurements in same minute
        pivot_wider(names_from = local_code, names_prefix = "l_latest_", values_from = latest_lab)    
    )
  
  ## combine everything
  # ==================
  
  print("Combining together")
  
  
  matrix_cutoff <- csn_summ %>%
    filter(ED_duration_final >= 60/cutoff) %>% 
    left_join(bed_moves_cutoff_csn) %>% 
    left_join(flowsheet_cutoff_csn) %>% 
    left_join(lab_cutoff_csn)
  
  # need to fill in the NA values as zeroes for people without any flowsheet measurements
  # do this before adding the valued results as these need to remain as NA
  # ideally this would not be hard-coded - if you change number of locations, this needs to change
  matrix_cutoff <- matrix_cutoff %>% 
    mutate_at(vars(colnames(matrix_cutoff)[25:ncol(matrix_cutoff)]), replace_na, 0)
  
  matrix_cutoff <- matrix_cutoff %>% 
    left_join(flowsheet_cutoff_csn_val) %>% 
    left_join(lab_cutoff_csn_val) 
  
  return(matrix_cutoff)
  
  
}


# load data
# =========

load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_2020-10-14.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-10-14.rda")

# flowsheet data
load("~/EDcrowding/predict-admission/data-raw/flowsheet_real_2020-10-14.rda")

# lab data
load("~/EDcrowding/predict-admission/data-raw/lab_real_2020-10-14.rda")

# demographic data
load("~/EDcrowding/predict-admission/data-raw/demog_2020-09-21.rda")


# prepare csn level data
# =====================

# attach admission relevant variables

csn_summ <- ED_csn_summ %>% 
  select(mrn, csn, csn_old, arrival_dttm, num_ED_row_excl_OTF, adm, epoch, ED_duration_final) %>% 
  mutate(hour_of_arrival = hour(arrival_dttm),
         month = month(arrival_dttm), 
         year = year(arrival_dttm), 
         day_of_week = factor(weekdays(arrival_dttm, abbreviate = TRUE)),
         weekend = if_else(day_of_week %in% c("Sun", "Sat"), 1,0),
         night = ifelse(hour(arrival_dttm) < 22 & hour(arrival_dttm) > 7, 0, 1)) 


### attach age and sex 

csn_summ <- csn_summ %>% 
  left_join(demog_raw) %>% 
  mutate(age = year(arrival_dttm) - year(birthdate)) %>% 
  select(-arrival_dttm)


# select only ED rows
bed_moves <- ED_bed_moves %>% 
  filter(ED_row_excl_OTF == 1) %>% 
  select(mrn, csn, csn_old, arrival_dttm, admission, discharge, pk_bed_moves, room4)

bed_moves <- bed_moves %>% 
  mutate(room4 = case_when(room4 %in% c("Arrived", "Waiting", "WAITING ROOM", "TRIAGE") ~ "Waiting",
                           TRUE ~ room4))

# mutate time in location to elapsed time after arrival
bed_moves <- bed_moves %>% ungroup() %>% 
  mutate(admission = as.numeric(difftime(admission, arrival_dttm, units = "mins")),
         discharge = as.numeric(difftime(discharge, arrival_dttm, units = "mins")))


matrix_60 <- create_design_matrix(bed_moves, csn_summ, flowsheet_real, lab_real, 60)

outFile = paste0("EDcrowding/predict-admission/data-raw/matrix_60_",today(),".rda")
save(matrix_60, file = outFile)
