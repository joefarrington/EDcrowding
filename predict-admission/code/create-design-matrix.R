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
  bed_moves_cutoff <- bed_moves %>% ungroup() %>% 
    left_join(ED_csn_summ %>% select(csn, ED_duration_final)) %>% 
    filter(ED_duration_final >= cutoff/60, 
           admission <= cutoff) %>% 
    select(-csn_old, -pk_bed_moves, -discharge_dttm, -ED_duration_final) %>% distinct()
  
  # count number of rows
  bed_moves_cutoff <- bed_moves_cutoff  %>%
    group_by(mrn, csn, arrival_dttm) %>% 
    mutate(num_ED_rows = n())
    
  
  # update discharge time of rows that exceed cutoff time
  bed_moves_cutoff <- bed_moves_cutoff  %>% ungroup() %>% 
    group_by(mrn, csn, arrival_dttm, room4) %>%
    mutate(discharge = case_when(discharge > cutoff & discharge == max(discharge) ~ cutoff,
                                 TRUE ~ discharge)) %>% ungroup() %>%
    select(-arrival_dttm )

  # collate to csn level
  bed_moves_cutoff_csn <- bed_moves_cutoff  %>%
    mutate(loc_duration = discharge - admission) %>%
    group_by(mrn, csn, room4, num_ED_rows) %>%
    summarise(loc_duration = sum(as.numeric(loc_duration))) %>%
    pivot_wider(names_from = room4, names_prefix = "mins_", values_from = loc_duration, values_fill = 0)
  
  # add indicator of location visited
  bed_moves_cutoff_csn <-   bed_moves_cutoff_csn %>% 
    left_join(
      bed_moves_cutoff  %>%
        ungroup() %>% select(mrn, csn, room4) %>% 
        group_by(mrn, csn, room4) %>% 
        mutate(visited = n()) %>%
        pivot_wider(names_from = room4, names_prefix = "visited_", values_from = visited, values_fn = sum, values_fill = 0)
    )

  
  
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
    filter(ED_duration_final >= cutoff/60)  %>% select(-ED_duration_final, -fk_bed_moves, -flowsheet_datetime)
  
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
  
  # add o2 sat categories to show when O2 sat dropped below 90 or 94
    flowsheet_cutoff_csn <- flowsheet_cutoff_csn %>% 
    left_join(
      flowsheet_cutoff  %>% filter(meas == "o2_sat") %>% 
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
  
    flowsheet_cutoff_csn <- flowsheet_cutoff_csn %>% 
      left_join(
        flowsheet_cutoff  %>% filter(meas == "news") %>% 
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
    filter(ED_duration_final >= cutoff/60) %>% distinct() %>% 
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

load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_2020-11-04.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-11-04.rda")

# flowsheet data
load("~/EDcrowding/predict-admission/data-raw/flowsheet_real_2020-11-05.rda")

# lab data
load("~/EDcrowding/predict-admission/data-raw/lab_real_2020-11-05.rda")

# demographic data
load("~/EDcrowding/predict-admission/data-raw/demog_2020-11-23.rda")

# all prior bed moves data to get prior visit info
load("~/EDcrowding/flow-mapping/data-raw/visit_summ_all_flow_and_star_2020-11-09.rda")



# prepare csn level data
# =====================

# attach admission relevant variables

csn_summ <- ED_csn_summ %>% 
  select(mrn, csn, csn_old, arrival_dttm, num_ED_row_excl_OTF, adm, epoch, ED_duration_final) %>% 
  mutate(hour_of_arrival = hour(arrival_dttm),
         month = month(arrival_dttm), 
         quarter = factor((month %/% 3)+1),
         year = year(arrival_dttm), 
         day_of_week = factor(weekdays(arrival_dttm, abbreviate = TRUE)),
         weekend = if_else(day_of_week %in% c("Sun", "Sat"), 1,0),
         night = ifelse(hour(arrival_dttm) < 22 & hour(arrival_dttm) > 7, 0, 1),
         time_of_day = factor((hour_of_arrival %/% 4)+1))


### attach age and sex 

csn_summ <- csn_summ %>% 
  left_join(demog_raw %>% select(mrn, sex, date_of_birth) %>% distinct()) %>% 
  # clean records with birthdate of 1900-01-01
  mutate(age = case_when(date_of_birth <= "1900-01-01" ~ NA_real_,
                         TRUE ~ year(arrival_dttm) - year(date_of_birth)),
         gt70 = factor(age >= 70)) 



### add prior visits

print("Processing prior visits")

vs <- vs %>% rename(csn_old = csn)

csn_summ <- csn_summ %>%  
  left_join(vs)

csn_summ <- csn_summ %>%  
  mutate(num_prior_visits = visit_num -1)

csn_summ <- csn_summ %>% 
  select(-arrival_dttm, -date_of_birth, -num_ED_row_excl_OTF, -visit_num)


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


#matrix_60 <- create_design_matrix(bed_moves, csn_summ, flowsheet_real, lab_real, 60)
matrix_120 <- create_design_matrix(bed_moves, csn_summ, flowsheet_real, lab_real, 120)

# useful function to explore data 
#skimr::skim(matrix_60)

outFile = paste0("EDcrowding/predict-admission/data-raw/matrix_120_",today(),".rda")
save(matrix_120, file = outFile)
