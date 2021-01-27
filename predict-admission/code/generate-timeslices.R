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
 

create_timeslice <- function (moves, dm, obs_real, cutoff, nextcutoff) {
  
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
  
  loc_cutoff_csn[, has_loc := if_else(num_loc > 0, 1, 0)]
  
  
  # rename CDU
  
  if (sum(grepl("UCHT00CDU", colnames(loc))) > 0) {
    setnames(loc_cutoff_csn, "num_UCHT00CDU", "num_CDU")
  }
  
  # observation data - this will create counts of all observation data prior to cutoff + margin
  
  print("Processing observation numbers")
  
  # select for cutoff
  obs_cutoff <- obs_real[elapsed_mins < cutoff + (nextcutoff - cutoff)/2]
  
  # add number of observation measurements up to cutoff
  obs_cutoff[, num_obs := .N, by = csn]
  
  # generate counts of fs meas by csn
  obs_cutoff_csn <- obs_cutoff[, .N, by = .(csn, meas)] %>% 
    pivot_wider(names_from = meas, names_prefix = "num_", values_from = N, values_fill = 0)
  obs_cutoff_csn <- data.table(merge(obs_cutoff_csn, unique(obs_cutoff[, .(csn, num_obs)])))
  
  # add number of types of results by csn
  obs_cutoff_csn <- merge(obs_cutoff_csn, obs_cutoff[, .(obs_types = uniqueN(meas)), by = csn])
  
  # blood pressure has two measurements, so delete one
  obs_cutoff_csn[, obs_types := obs_types -1]
  obs_cutoff_csn[, num_obs := num_obs - num_bp_dia]
  obs_cutoff_csn[, has_obs := if_else(num_obs > 0, 1, 0)]
  
  # add number of observation events per csn
  obs_cutoff_csn <- merge(obs_cutoff_csn, obs_cutoff[, .(obs_events = uniqueN(elapsed_mins)), by = csn])
  
  # add count of times when O2 sat dropped below 90 or 95

  sat_lt90 <- obs_cutoff[meas == "o2_sat" & value_as_real < 90, .N, by = .(csn)] 
  setnames(sat_lt90, "N", "num_o2sat_lt90")
  sat_lt95 <- obs_cutoff[meas == "o2_sat" & value_as_real < 95, .N, by = .(csn)] 
  setnames(sat_lt95, "N", "num_o2sat_lt95")
  
  obs_cutoff_csn <- merge(obs_cutoff_csn, sat_lt90, all.x = TRUE, by = "csn")
  obs_cutoff_csn <- merge(obs_cutoff_csn, sat_lt95, all.x = TRUE, by = "csn")
    
  # add count of times when news score was medium or high
  
  news_med <- obs_cutoff[meas == "news" & value_as_real < 7 & value_as_real > 4, .N, by = .(csn)] 
  setnames(news_med, "N", "num_newsmed")
  news_high <- obs_cutoff[meas == "news" & value_as_real >= 7, .N, by = .(csn)] 
  setnames(news_high, "N", "num_newshigh")
  
  obs_cutoff_csn <- merge(obs_cutoff_csn, news_med, all.x = TRUE, by = "csn")
  obs_cutoff_csn <- merge(obs_cutoff_csn, news_high, all.x = TRUE, by = "csn")
  
  print("Processing observation values")
  # Note - this uses a different table due to handling of NA values later on

  # add min for each measurement
  obs_cutoff_csn_val <- data.table(
    obs_cutoff[,  min(value_as_real, na.rm = TRUE), by = .(csn, meas)] %>% 
    pivot_wider(names_from = meas, names_prefix = "obs_min_", values_from = V1)
  )
  
  # add max score for each measurement
  obs_cutoff_csn_val <- merge(obs_cutoff_csn_val, data.table(
    obs_cutoff[,  max(value_as_real, na.rm = TRUE), by = .(csn, meas)] %>% 
      pivot_wider(names_from = meas, names_prefix = "obs_max_", values_from = V1)
  ))
  
  # add latest score for each measurement
  obs_cutoff_csn_val <- merge(obs_cutoff_csn_val, data.table(
      obs_cutoff %>% 
        group_by(csn, meas) %>% 
        filter(elapsed_mins == max(elapsed_mins)) %>% 
        # using max allows for possibility of two measurements in same minute
        summarise(latest_meas = max(value_as_real, na.rm = TRUE))  %>%  
        pivot_wider(names_from = meas, names_prefix = "obs_latest_", values_from = latest_meas)    
    ))

  
  ## combine everything
  
  print("Combining together")
  
  # just use csn from summ to start with - add the other summ fields (which may have genuine NAs) later
  matrix_cutoff <- merge(dm[duration > cutoff, .(csn, adm)], loc_cutoff_csn, all.x = TRUE) 
  matrix_cutoff <- merge(matrix_cutoff, obs_cutoff_csn, all.x = TRUE)
  
  # need to fill in the NA values as zeroes for people without any observation measurements
  # do this before adding the valued results as these need to remain as NA
  # ideally this would not be hard-coded - if you change number of locations, this needs to change
  
  # replace all counts with NA as 0
  matrix_cutoff[is.na(matrix_cutoff)] <- 0
  
  # add other info where there may be genuine NAs
  matrix_cutoff <- merge(matrix_cutoff, dm[duration > cutoff], by = c("csn", "adm")) 
  matrix_cutoff <- merge(matrix_cutoff, obs_cutoff_csn_val, all.x = TRUE) 
  matrix_cutoff[, duration := NULL]
  
  return(matrix_cutoff)
  
  
}


# load data
# =========

load("~/EDcrowding/flow-mapping/data-raw/moves_2021-01-27.rda")
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-01-27.rda")


# observation data
load("~/EDcrowding/predict-admission/data-raw/obs_real_2021-01-25.rda")

# lab data
#load("~/EDcrowding/predict-admission/data-raw/lab_real_2020-11-05.rda")

# prior visit info
load("~/EDcrowding/flow-mapping/data-raw/visits_all_2021-01-25.rda")



# Prepare csn level data --------------------------------------------------


# attach admission relevant variables

dm <- summ[,.(csn, age, sex, presentation_time, adm, epoch, arrival_method, last_inside_discharge, min_I, first_ED_admission)]
dm[, hour := hour(presentation_time)]
dm[, tod := factor((hour %/% 4)+1)]
dm[, hour := factor(hour)] # do this after calculating tod
dm[, month := month(presentation_time)]
dm[, quarter := factor(case_when( month <= 3 ~ 1,
                                   month <= 6 ~ 2, 
                                   month <= 9 ~ 3, 
                                   month <= 12 ~ 4))]
dm[, month := factor(month)] # do this after calculating quarter
dm[, year := factor(year(presentation_time))]
dm[, day :=  factor(weekdays(presentation_time, abbreviate = TRUE))]
dm[, weekend := factor(if_else(day %in% c("Sun", "Sat"), 1,0))]
dm[, night := factor(ifelse(hour(presentation_time) < 22 & hour(presentation_time) > 7, 0, 1))]
dm[, gt70 := factor(age >= 70)]
dm[, sex := factor(sex)]

# was an inpatient, as part of this visit, before ED
dm[, inpatient := if_else(min_I < first_ED_admission, 1, 0)]
dm[, inpatient := if_else(is.na(inpatient), 0, inpatient)]
dm[, inpatient := factor(inpatient)]

# include a feature to capture time delay between presentation and arrival at ED;
dm[, beforeED := as.numeric(difftime(first_ED_admission, presentation_time, units = "mins"))]
# small number have negative time for before ED
dm[, beforeED := if_else(beforeED <0, 0, beforeED) ]

# NB - using last inside discharge for duration - includes time spent in CDU
dm[, duration := as.numeric(difftime(last_inside_discharge, presentation_time, units = "mins"))]

rpt(dm)


# simplify arrival method
dm[, arrival_method := gsub(" |-", "", arrival_method)]
dm[, arrival_method := factor(if_else(!arrival_method %in% c("Ambulance",
                                                        "Walkin",
                                                        "PublicTrans",
                                                        "Ambnomedic"), "Other", arrival_method)) ]
### add prior visits

dm <- merge(dm, visits %>% select(csn, days_since_last_visit, num_prior_adm_after_ED, num_prior_ED_visits, prop_adm_from_ED), all.x = TRUE)

# correcting error in visits processing - small number have negative number of days since last visit
dm[, days_since_last_visit := if_else(days_since_last_visit <0, NA_real_, days_since_last_visit) ]

# prepare outcome variable
dm[, adm := if_else(adm %in% c("direct_adm", "indirect_adm"), 1, 0)]

# Prepare location data --------------------------------------------------

loc <- moves[, .(csn, admission, discharge, location, visited_CDU, outside)]
loc <- merge(loc, dm[,.(csn, presentation_time,last_inside_discharge, duration)])

# For now, filter out dates where no observation data available
# for chart of this
# obs_real[, .N, by = date(observation_datetime)] %>% ggplot(aes(x = date, y = N)) + geom_line()

loc <- loc[date(presentation_time) > '2020-04-01']
dm <- dm[date(presentation_time) > '2020-04-01']

loc[, admission_e := as.numeric(difftime(admission, presentation_time, units = "mins"))]
loc[, discharge_e := as.numeric(difftime(discharge, presentation_time, units = "mins"))]

# remove rows after the patient leaves ED or other inside location (where inside includes CDU and possibly others)
loc <- loc[discharge <= last_inside_discharge | is.na(last_inside_discharge)]
rpt(loc)

loc[, c("admission", "discharge", "presentation_time") := NULL]
dm[, c("presentation_time", "last_inside_discharge", "min_I", "first_ED_admission") := NULL]



# -----------



# Create timeslices -------------------------------------------------------


timeslices <- c(0, 15, 30, 60, 90, 120, 150, 180, 210, 240, 300, 360, 24*60)

for (i in seq(1, length(timeslices) -1, 1)) {
  print(paste0("Processing timeslice ", timeslices[i]))
  name_ <- paste0("dm", timeslices[i])
  ts <- create_timeslice(loc, dm, obs_real, timeslices[i], timeslices[i+1])
  assign(name_, ts)
}



# useful function to explore data 
skimr::skim(dm15)


save(dm0, file = paste0("EDcrowding/predict-admission/data-raw/dm0_",today(),".rda"))
save(dm15, file = paste0("EDcrowding/predict-admission/data-raw/dm15_",today(),".rda"))
save(dm30, file = paste0("EDcrowding/predict-admission/data-raw/dm30_",today(),".rda"))
save(dm60, file = paste0("EDcrowding/predict-admission/data-raw/dm60_",today(),".rda"))
save(dm90, file = paste0("EDcrowding/predict-admission/data-raw/dm90_",today(),".rda"))
save(dm120, file = paste0("EDcrowding/predict-admission/data-raw/dm120_",today(),".rda"))
save(dm150, file = paste0("EDcrowding/predict-admission/data-raw/dm150_",today(),".rda"))
save(dm180, file = paste0("EDcrowding/predict-admission/data-raw/dm180_",today(),".rda"))
save(dm210, file = paste0("EDcrowding/predict-admission/data-raw/dm210_",today(),".rda"))
save(dm240, file = paste0("EDcrowding/predict-admission/data-raw/dm240_",today(),".rda"))
save(dm300, file = paste0("EDcrowding/predict-admission/data-raw/dm300_",today(),".rda"))
save(dm360, file = paste0("EDcrowding/predict-admission/data-raw/dm360_",today(),".rda"))








