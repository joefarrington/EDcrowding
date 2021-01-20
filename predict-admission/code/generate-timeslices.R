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
 

create_timeslice <- function (bed_moves, csn_summ, fs_real, cutoff, nextcutoff) {
  
  # locations - note this section will not include patients who have no location data up to cutoff + margin
  
  print("Processing bed moves")
  
  # select only patients with duration longer than cutoff 
  loc_cutoff <- loc[duration > cutoff]
  
  # select locations that began before the cutoff (up to midpoint before next cutoff)
  loc_cutoff <- loc_cutoff[admission_e < cutoff + (nextcutoff - cutoff)/2]
  
  # count number of location rows up to ED - note will include any pre- ED locations
  loc_count <- loc_cutoff[, .N, by = csn]
  setnames(loc_count, "N", "num_loc")
  
  # add indicator of location visited at csn level
  loc_cutoff_csn <- merge(loc_count, data.table(
    loc_cutoff[(!outside), .N, by = .(csn, location)] %>% 
    pivot_wider(names_from = location, names_prefix = "num_", values_from = N, values_fill = 0)
  ), all.x = TRUE)
  
  # rename CDU
  setnames(loc_cutoff_csn, "num_UCHT00CDU", "num_CDU")

  
  
  # flowsheet data - this will create counts of all flowsheet data prior to cutoff + margin
  
  print("Processing flowsheet numbers")
  
  # select for cutoff
  fs_cutoff <- fs_real[elapsed_mins < cutoff + (nextcutoff - cutoff)/2]
  
  # add number of flowsheet measurements up to cutoff
  fs_cutoff[, num_fs := .N, by = csn]
  
  # generate counts of fs meas by csn
  fs_cutoff_csn <- fs_cutoff[, .N, by = .(csn, meas)] %>% 
    pivot_wider(names_from = meas, names_prefix = "num_", values_from = N, values_fill = 0)
  fs_cutoff_csn <- data.table(merge(fs_cutoff_csn, unique(fs_cutoff[, .(csn, num_fs)])))
  
  # add number of types of results by csn
  fs_cutoff_csn <- merge(fs_cutoff_csn, fs_cutoff[, .(fs_types = uniqueN(meas)), by = csn])
  
  # blood pressure has two measurements, so delete one
  fs_cutoff_csn[, fs_types := fs_types -1]
  fs_cutoff_csn[, num_fs := num_fs - num_bp_dia]
  fs_cutoff_csn[, has_fs := if_else(num_fs > 0, 1, 0)]
  
  # add number of flowsheet events per csn
  fs_cutoff_csn <- merge(fs_cutoff_csn, fs_cutoff[, .(fs_events = uniqueN(elapsed_mins)), by = csn])
  
  # add count of times when O2 sat dropped below 90 or 95

  sat_lt90 <- fs_cutoff[meas == "o2_sat" & value_as_real < 90, .N, by = .(csn)] 
  setnames(sat_lt90, "N", "num_o2sat_lt90")
  sat_lt95 <- fs_cutoff[meas == "o2_sat" & value_as_real < 95, .N, by = .(csn)] 
  setnames(sat_lt95, "N", "num_o2sat_lt95")
  
  fs_cutoff_csn <- merge(fs_cutoff_csn, sat_lt90, all.x = TRUE, by = "csn")
  fs_cutoff_csn <- merge(fs_cutoff_csn, sat_lt95, all.x = TRUE, by = "csn")
    
  # add count of times when news score was medium or high
  
  news_med <- fs_cutoff[meas == "news" & value_as_real < 7 & value_as_real > 4, .N, by = .(csn)] 
  setnames(news_med, "N", "num_newsmed")
  news_high <- fs_cutoff[meas == "news" & value_as_real >= 7, .N, by = .(csn)] 
  setnames(news_high, "N", "num_newshigh")
  
  fs_cutoff_csn <- merge(fs_cutoff_csn, news_med, all.x = TRUE, by = "csn")
  fs_cutoff_csn <- merge(fs_cutoff_csn, news_high, all.x = TRUE, by = "csn")
  
  print("Processing flowsheet values")
  # Note - this uses a different table due to handling of NA values later on

  # add min for each measurement
  fs_cutoff_csn_val <- data.table(
    fs_cutoff[,  min(value_as_real, na.rm = TRUE), by = .(csn, meas)] %>% 
    pivot_wider(names_from = meas, names_prefix = "fs_min_", values_from = V1)
  )
  
  # add max score for each measurement
  fs_cutoff_csn_val <- merge(fs_cutoff_csn_val, data.table(
    fs_cutoff[,  max(value_as_real, na.rm = TRUE), by = .(csn, meas)] %>% 
      pivot_wider(names_from = meas, names_prefix = "fs_max_", values_from = V1)
  ))
  
  # add latest score for each measurement
  fs_cutoff_csn_val <- merge(fs_cutoff_csn_val, data.table(
      fs_cutoff %>% 
        group_by(csn, meas) %>% 
        filter(elapsed_mins == max(elapsed_mins)) %>% 
        # using max allows for possibility of two measurements in same minute
        summarise(latest_meas = max(value_as_real, na.rm = TRUE))  %>%  
        pivot_wider(names_from = meas, names_prefix = "fs_latest_", values_from = latest_meas)    
    ))

  
  ## combine everything
  
  print("Combining together")
  
  # just use csn from summ to start with - add the other summ fields (which may have genuine NAs) later
  matrix_cutoff <- merge(summ[duration > cutoff, .(csn, adm)], loc_cutoff_csn, all.x = TRUE) 
  matrix_cutoff <- merge(matrix_cutoff, fs_cutoff_csn, all.x = TRUE)
  
  # need to fill in the NA values as zeroes for people without any flowsheet measurements
  # do this before adding the valued results as these need to remain as NA
  # ideally this would not be hard-coded - if you change number of locations, this needs to change
  
  # replace all counts with NA as 0
  matrix_cutoff[is.na(matrix_cutoff)] <- 0
  
  # add other info where there may be genuine NAs
  matrix_cutoff <- merge(matrix_cutoff, summ[duration > cutoff], by = c("csn", "adm")) 
  matrix_cutoff <- merge(matrix_cutoff, fs_cutoff_csn_val, all.x = TRUE) 
  
  return(matrix_cutoff)
  
  
}


# load data
# =========

load("~/EDcrowding/predict-admission/data-raw/ED_csn_summ_final_csns_2021-01-19.rda")
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-01-19.rda")

# flowsheet data
load("~/EDcrowding/predict-admission/data-raw/fs_real_2021-01-20.rda")

# lab data
#load("~/EDcrowding/predict-admission/data-raw/lab_real_2020-11-05.rda")

# prior visit info
load("~/EDcrowding/flow-mapping/data-raw/visits_all_2021-01-06.rda")



# Prepare csn level data --------------------------------------------------


# attach admission relevant variables

summ <- data.table(ED_csn_summ %>% 
  select(csn, age, sex, presentation_time, adm, epoch, arrival_method, last_inside_discharge, min_I, first_ED_admission) %>% 
  mutate(hour = hour(presentation_time),
         month = month(presentation_time), 
         quarter = factor((month %/% 3)+1),
         year = year(presentation_time), 
         day = factor(weekdays(presentation_time, abbreviate = TRUE)),
         weekend = if_else(day %in% c("Sun", "Sat"), 1,0),
         night = ifelse(hour(presentation_time) < 22 & hour(presentation_time) > 7, 0, 1),
         time = factor((hour %/% 4)+1),
         gt70 = factor(age >= 70),
         # was an inpatient, as part of this visit, before ED
         inpatient = if_else(min_I < first_ED_admission | is.na(min_I), 1, 0),
         # include a feature to capture time delay between presentation and arrival at ED;
         beforeED = as.numeric(difftime(first_ED_admission, presentation_time, units = "mins")),
         # NB - using last inside discharge for duration - need to check this? 
         duration = as.numeric(difftime(last_inside_discharge, presentation_time, units = "mins"))))

rpt(summ)


# simply arrival method
summ[, arrival_method := if_else(!arrival_method %in% c("Ambulance",
                                                        "Walk-in",
                                                        "Public Trans",
                                                        "Amb no medic"), "Other", arrival_method) ]
### add prior visits

summ <- merge(summ, visits %>% select(csn, days_since_last_visit, num_prior_adm_after_ED, num_prior_ED_visits, prop_adm_from_ED))

# correcting error in visits processing - small number have negative number of days since last visit
summ[, days_since_last_visit := if_else(days_since_last_visit <0, NA_real_, days_since_last_visit) ]

summ[, c("presentation_time", "last_inside_discharge", "min_I", "first_ED_admission") := NULL]

# Prepare location data --------------------------------------------------

loc <- moves[, .(csn, admission, discharge, location, visited_CDU, outside)]
loc <- merge(loc, summ[,.(csn, presentation_time, duration)])
loc[, admission_e := as.numeric(difftime(admission, presentation_time, units = "mins"))]
loc[, discharge_e := as.numeric(difftime(discharge, presentation_time, units = "mins"))]

# remove rows after the patient leaves ED or other inside location (where inside includes CDU and possibly others)
loc <- loc[discharge <= last_inside_discharge]
rpt(loc)

loc[, c("admission", "discharge", "presentation_time") := NULL]


#matrix_60 <- create_timeslice(bed_moves, csn_summ, fs_real, lab_real, 60)
dm15 <- create_timeslice(loc, summ, fs_real, 15, 30)

# useful function to explore data 
#skimr::skim(matrix_60)

outFile = paste0("EDcrowding/predict-admission/data-raw/dm15_",today(),".rda")
save(dm15, file = outFile)
