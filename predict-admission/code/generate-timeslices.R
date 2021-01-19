# About this file
# ==============

# Generates timeslices for ML



# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)


# Create functions --------------------------------------------------------

rpt <- function(dataframe) {
  print(dataframe %>% select(csn) %>% n_distinct())
}



# function to create design matrix
# ================================
 

create_timeslice <- function (bed_moves, csn_summ, fs_real, lab_real, cutoff) {
  
  # locations
  
  print("Processing bed moves")
  
  # select only rows where the admission began before the cutoff time(includes rows where discharge is later)
  loc_cutoff <- loc[discharge_e <= cutoff]
  
  # count number of rows
  loc_cutoff[, num_loc := .N, by = csn]
  
  # add indicator of location visited at csn level
  loc_cutoff_csn <- loc_cutoff[(!outside), .N, by = .(csn, location)] %>% 
    pivot_wider(names_from = location, values_from = N, values_fill = 0)

  
  
  # flowsheet data
  
  print("Processing flowsheet numbers")
  
  
  # # remove outliers
  # fs_real <- fs_real %>% 
  #   mutate(result_as_real = case_when(result_as_real >115 & meas == "temp" ~ NA_real_,
  #                                     TRUE ~ result_as_real))
  # 
  # 
  # fs_real <- fs_real %>% 
  #   filter(!is.na(result_as_real))
  
  # select for cutoff
  fs_cutoff <- fs_real[elapsed_mins < cutoff]
  
  # add number of flowsheet measurements up to cutoff
  fs_cutoff[, num_fs := .N, by = csn]
  
  # add indicator of fs meas at csn level
  fs_cutoff_csn2 <-fs_cutoff[, .N, by = .(csn, meas)] %>% pivot_wider(names_from = meas, values_from = N, values_fill = 0)
  merge(unique(fs_cutoff_csn2, fs_cutoff[, .(csn, num_fs)]))
  fs_cutoff_csn[, has_fs := if_else(num_fs >0, 1, 0)]
  
  # add number of types of results for csn
  fs_cutoff_csn <- fs_cutoff_csn %>% 
    left_join(
      fs_cutoff %>% 
        group_by(mrn, csn) %>% 
        summarise(num_fs_types = n_distinct(meas)) 
    )
  
  # add number of flowsheet events
  fs_cutoff_csn <- fs_cutoff_csn %>% 
    left_join(
      fs_cutoff %>% 
        group_by(mrn, csn) %>% 
        summarise(num_fs_events = n_distinct(elapsed_mins)) 
    )
  
  # add number of each measurement 
  fs_cutoff_csn <- fs_cutoff_csn %>% 
    left_join(
      fs_cutoff %>% 
        group_by(mrn, csn, meas) %>% 
        summarise(num_meas = n()) %>% 
        pivot_wider(names_from = meas, names_prefix = "fs_num_", values_from = num_meas, values_fill = 0)    
    )
  
  # add o2 sat categories to show when O2 sat dropped below 90 or 94
    fs_cutoff_csn <- fs_cutoff_csn %>% 
    left_join(
      fs_cutoff  %>% filter(meas == "o2_sat") %>% 
        select(csn, meas, result_as_real) %>% distinct() %>% 
        mutate(o2_sat_lt90 = case_when(result_as_real < 90 ~ 1, 
                                       TRUE ~ 0),
               o2_sat_lt94 = case_when(result_as_real < 95 ~ 1, 
                                       TRUE ~ 0)) %>% 
        select(-result_as_real, -meas) %>% 
        group_by(csn) %>% distinct() %>% 
        mutate(fs_o2_sat_lt90 = sum(o2_sat_lt90) > 0,
               fs_o2_sat_lt94 = sum(o2_sat_lt94) > 0) %>% 
        select(-starts_with("o2_sat")) %>% distinct()
    )
    
    # add news categories
  
    fs_cutoff_csn <- fs_cutoff_csn %>% 
      left_join(
        fs_cutoff  %>% filter(meas == "news") %>% 
          select(csn, meas, result_as_real) %>% distinct() %>% 
          mutate(news_medium = case_when(result_as_real < 7 & result_as_real > 4 ~ 1, 
                                         TRUE ~ 0),
                 news_high = case_when(result_as_real >= 7 ~ 1, 
                                       TRUE ~ 0)) %>% 
          select(-result_as_real, -meas) %>% 
          group_by(csn) %>% distinct() %>% 
          mutate(fs_news_medium = sum(news_medium) > 0,
                 fs_news_high = sum(news_high) > 0) %>% 
          select(-starts_with("news_"))  %>% distinct()
      )
  
  print("Processing flowsheet values")

  
  # flowsheet values
  # add min score for each measurement
  fs_cutoff_csn_val <- fs_cutoff %>% 
    group_by(mrn, csn, meas) %>% 
    summarise(min_meas = min(result_as_real, na.rm = TRUE))  %>% 
    pivot_wider(names_from = meas, names_prefix = "fs_min_", values_from = min_meas)    
  
  # add max score for each measurement
  fs_cutoff_csn_val <- fs_cutoff_csn_val %>% 
    left_join(
      fs_cutoff %>% 
        group_by(mrn, csn, meas) %>% 
        summarise(max_meas = max(result_as_real, na.rm = TRUE))  %>% 
        pivot_wider(names_from = meas, names_prefix = "fs_max_", values_from = max_meas)    
    )
  
  # add latest score for each measurement
  fs_cutoff_csn_val <- fs_cutoff_csn_val %>% 
    left_join(
      fs_cutoff %>% 
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
    filter(ED_duration_final >= cutoff/60)  %>% 
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
    filter(ED_duration_final/60 >= cutoff/60) %>% distinct() %>% 
    left_join(loc_cutoff_csn) %>% 
    left_join(fs_cutoff_csn) %>% 
    left_join(lab_cutoff_csn)
  
  # need to fill in the NA values as zeroes for people without any flowsheet measurements
  # do this before adding the valued results as these need to remain as NA
  # ideally this would not be hard-coded - if you change number of locations, this needs to change
  matrix_cutoff <- matrix_cutoff %>% 
    mutate_at(vars(colnames(matrix_cutoff)[25:ncol(matrix_cutoff)]), replace_na, 0)
  
  matrix_cutoff <- matrix_cutoff %>% 
    left_join(fs_cutoff_csn_val) %>% 
    left_join(lab_cutoff_csn_val) 
  
  return(matrix_cutoff)
  
  
}


# load data
# =========

load("~/EDcrowding/predict-admission/data-raw/ED_csn_summ_final_csns_2020-01-19.rda")
load("~/EDcrowding/predict-admission/data-raw/ED_bed_moves_final_csns_2020-01-19.rda")

# flowsheet data
load("~/EDcrowding/predict-admission/data-raw/fs_real_2020-01-19.rda")

# lab data
load("~/EDcrowding/predict-admission/data-raw/lab_real_2020-11-05.rda")

# prior visit info
load("~/EDcrowding/flow-mapping/data-raw/visits_all_2021-01-06.rda")



# Prepare csn level data --------------------------------------------------


# attach admission relevant variables

summ <- data.table(ED_csn_summ %>% 
  select(csn, age, sex, presentation_time, adm, epoch, last_ED_discharge, min_I, first_ED_admission) %>% 
  mutate(hour = hour(presentation_time),
         month = month(presentation_time), 
         quarter = factor((month %/% 3)+1),
         year = year(presentation_time), 
         day = factor(weekdays(presentation_time, abbreviate = TRUE)),
         weekend = if_else(day %in% c("Sun", "Sat"), 1,0),
         night = ifelse(hour(presentation_time) < 22 & hour(presentation_time) > 7, 0, 1),
         time = factor((hour %/% 4)+1),
         gt70 = factor(age >= 70),
         inpatient = if_else(min_I < first_ED_admission, 1, 0)))

rpt(summ)

### add prior visits

summ <- merge(summ, visits %>% select(csn, days_since_last_visit, num_prior_adm_after_ED, num_prior_ED_visits, prop_adm_from_ED))


# Prepare location data --------------------------------------------------

loc <- moves[, .(csn, admission, discharge, location, first_ED_admission, last_inside_discharge, visited_CDU, outside)]
loc <- merge(loc, summ[,.(csn, presentation_time)])
loc[, admission_e := as.numeric(difftime(admission, presentation_time, units = "mins"))]
loc[, discharge_e := as.numeric(difftime(discharge, presentation_time, units = "mins"))]

# remove rows after the patient leaves ED or other inside location (where inside includes CDU and possibly others)
loc <- loc[discharge <= last_inside_discharge]
rpt(loc)

#matrix_60 <- create_timeslice(bed_moves, csn_summ, fs_real, lab_real, 60)
matrix_120 <- create_timeslice(loc, summ, fs_real, 120)

# useful function to explore data 
#skimr::skim(matrix_60)

outFile = paste0("EDcrowding/predict-admission/data-raw/matrix_120_",today(),".rda")
save(matrix_120, file = outFile)
