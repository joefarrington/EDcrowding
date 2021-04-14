# About this file
# ==============

# Generates timeslices for ML. A subset of cases are held out for final testing



# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)
library(tidymodels)


# Create functions --------------------------------------------------------

rpt <- function(dataframe) {
  print(dataframe %>% select(csn) %>% n_distinct())
}


# function to create design matrix

create_timeslice <- function (moves, dm, obs_real, lab_real, cutoff, nextcutoff) {
  
  # locations - note this section will not include patients who have no location data up to cutoff + margin
  
  print("Processing bed moves")
  
  # select only patients with duration longer than cutoff 
  loc_cutoff <- loc[duration > cutoff]
  
  # select locations that began before the cutoff (up to midpoint before next cutoff)
  loc_cutoff <- loc_cutoff[admission_e < cutoff + (nextcutoff - cutoff)/2]
  
  # count number of location rows up to ED - note will include any pre- ED locations
  loc_count <- loc_cutoff[, .N, by = csn]
  setnames(loc_count, "N", "l_num")
  
  loc_count <- merge(loc_count, loc_cutoff[(admission_e <= cutoff & discharge_e > cutoff), location, by = csn],
                     all.x = TRUE)
  setnames(loc_count, "location", "l_current")
  loc_count[, l_current := factor(l_current)]
  
  # add indicator of location visited at csn level
  loc_cutoff_csn <- merge(loc_count, data.table(
    loc_cutoff[(!outside), .N > 0, by = .(csn, location)]  %>% 
    pivot_wider(names_from = location, names_prefix = "l_visited_", values_from = V1, values_fill = 0)
  ), all.x = TRUE)
  
  
  # rename CDU
  
  if (sum(grepl("UCHT00CDU", colnames(loc))) > 0) {
    setnames(loc_cutoff_csn, "num_UCHT00CDU", "num_CDU")
  }
  
  # observation data - this will create counts of all observation data prior to cutoff + margin
  
  print("Processing observation numbers")
  
  # select for cutoff
  obs_cutoff <- obs_real[elapsed_mins < cutoff + (nextcutoff - cutoff)/2]
  
  # add number of observation measurements up to cutoff
  obs_cutoff[, o_num_meas := .N, by = csn]
  
  # add number of types of results by csn
  obs_cutoff_csn <- merge(unique(obs_cutoff[, .(csn, o_num_meas)]), 
                          obs_cutoff[, .(o_num_types = uniqueN(obs_name)), by = csn], by = "csn")
  
  # add number of observation events per csn
  obs_cutoff_csn <- merge(obs_cutoff_csn, obs_cutoff[, .(o_num_events = uniqueN(elapsed_mins)), by = csn])

  # blood pressure has two measurements per event, so delete one type
  obs_cutoff_csn[, o_num_types := o_num_types -1]
  # obs_cutoff_csn[, o_num := o_num - o_num_Bloodpressure_sys]
  obs_cutoff_csn[, o_has := 1] # this will be 1 for all csns currently; zeros added later


  # add count of times when O2 sat dropped below 90 or 95

  sat_lt90 <- obs_cutoff[obs_name == "Oxygensaturation" & value_as_real < 90, .N, by = .(csn)]
  setnames(sat_lt90, "N", "o_num_o2sat_lt90")
  sat_lt95 <- obs_cutoff[obs_name == "Oxygensaturation" & value_as_real < 95, .N, by = .(csn)]
  setnames(sat_lt95, "N", "o_num_o2sat_lt95")

  obs_cutoff_csn <- merge(obs_cutoff_csn, sat_lt90, all.x = TRUE, by = "csn")
  obs_cutoff_csn <- merge(obs_cutoff_csn, sat_lt95, all.x = TRUE, by = "csn")
    
  # add count of times when news score was medium or high
  
  news_med <- obs_cutoff[obs_name == "NEWSscore" & value_as_real < 7 & value_as_real > 4, .N, by = .(csn)] 
  setnames(news_med, "N", "o_num_news_med")
  news_high <- obs_cutoff[obs_name == "NEWSscore" & value_as_real >= 7, .N, by = .(csn)] 
  setnames(news_high, "N", "o_num_news_high")
  
  obs_cutoff_csn <- merge(obs_cutoff_csn, news_med, all.x = TRUE, by = "csn")
  obs_cutoff_csn <- merge(obs_cutoff_csn, news_high, all.x = TRUE, by = "csn")
  
  # add count of times ACVPU not equal to A
  ACVPU_notA <- obs_cutoff[obs_name == "ACVPU" & value_as_real > 1, .N, by = .(csn)] 
  setnames(ACVPU_notA, "N", "o_num_ACVPU_notA")
  
  obs_cutoff_csn <- merge(obs_cutoff_csn, ACVPU_notA, all.x = TRUE, by = "csn")
  
  # add count of times GCS <= 8
  GCS_lt9 <- obs_cutoff[obs_name == "GCStotal" & value_as_real < 9, .N, by = .(csn)] 
  setnames(GCS_lt9, "N", "o_num_GCS_lt9")
  
  obs_cutoff_csn <- merge(obs_cutoff_csn, GCS_lt9, all.x = TRUE, by = "csn")
  
  # generate counts of each observation by csn
  obs_cutoff_csn_w <- obs_cutoff[, .N, by = .(csn, obs_name)] %>%
    pivot_wider(names_from = obs_name, names_prefix = "o_num_", values_from = N, values_fill = 0)
  
  obs_cutoff_csn_w <- obs_cutoff_csn_w %>% select(-o_num_Bloodpressure_dia)
  obs_cutoff_csn_w <- obs_cutoff_csn_w %>% rename(o_num_Bloodpressure = o_num_Bloodpressure_sys)
  
  obs_cutoff_csn <- data.table(merge(obs_cutoff_csn, obs_cutoff_csn_w))
  
  # add valued obs data
  obs_cutoff_csn_val <- data.table(
    obs_cutoff %>% 
      filter(obs_name %in% c("TempinCelsius", "Heartrate","MAPnoninvasive", "Respiratoryrate", "FiO2" )) %>% 
      group_by(csn, obs_name) %>%
      filter(elapsed_mins == max(elapsed_mins), !is.na(value_as_real)) %>%
      # using max allows for possibility of two measurements in same minute
      summarise(latest_value = max(value_as_real, na.rm = TRUE))  %>%
      pivot_wider(names_from = obs_name, names_prefix = "p_latest_", values_from = latest_value)
  )
    
  # 
  # add lab data
  
  print("Processing lab numbers")
  
  # select for cutoff
  lab_orders_cutoff <- lab_orders_real[elapsed_mins < cutoff + (nextcutoff - cutoff)/2]
  lab_results_cutoff <- lab_results_real[elapsed_mins < cutoff + (nextcutoff - cutoff)/2]
  
  # # add number of lab orders up to cutoff
  # lab_cutoff[, p_num_orders := .N, by = csn]

  # add number of types of orders by csn
  lab_cutoff_csn <- lab_orders_cutoff[, .(p_num_battery = uniqueN(battery_code)), by = csn]
  
  # add number of lab results that are out of range high and low
  lab_cutoff_csn <- merge(lab_cutoff_csn, lab_results_cutoff[(oor_high), .(p_num_oor_high =.N), by = csn])
  lab_cutoff_csn <- merge(lab_cutoff_csn, lab_results_cutoff[(oor_low), .(p_num_oor_low =.N), by = csn])
  lab_cutoff_csn <- merge(lab_cutoff_csn, lab_results_cutoff[(abnormal), .(p_num_abnormal =.N), by = csn])
  
  # add whether each cluster was requested
  
  lab_cutoff_csn_battery = lab_orders_cutoff[, (N =.N > 0), by = .(csn, battery_code)] %>% 
       pivot_wider(names_from = battery_code, names_prefix = "p_req_battery_", values_from = V1, values_fill = 0)
  
  lab_cutoff_csn <- data.table(merge(lab_cutoff_csn, lab_cutoff_csn_battery))
  
  # add score for each lab test in APACHE
  lab_cutoff_csn_val <- data.table(
    lab_results_cutoff %>%
      filter(test_lab_code %in% c("K", "NA", "CREA", "HCTU", "WCC")) %>% 
      group_by(csn, test_lab_code) %>%
      filter(elapsed_mins == max(elapsed_mins), !is.na(value_as_real)) %>%
      # using max allows for possibility of two measurements in same minute
      summarise(latest_value = max(value_as_real, na.rm = TRUE))  %>%
      pivot_wider(names_from = test_lab_code, names_prefix = "p_latest_", values_from = latest_value)
  )
  

  
  ## combine everything
  
  print("Combining together")
  
  # just use csn from summ to start with - add the other summ fields (which may have genuine NAs) later
  matrix_cutoff <- merge(dm[duration > cutoff, .(csn, adm)], loc_cutoff_csn, all.x = TRUE) 
  matrix_cutoff <- merge(matrix_cutoff, obs_cutoff_csn, all.x = TRUE)
  matrix_cutoff <- merge(matrix_cutoff, lab_cutoff_csn, all.x = TRUE)
  
  
  # need to fill in the NA values as zeroes for people without any observation measurements
  # do this before adding the valued results as these need to remain as NA
  # ideally this would not be hard-coded - if you change number of locations, this needs to change
  
  # replace all counts with NA as 0
  matrix_cutoff[is.na(matrix_cutoff)] <- 0
  
  # add other info where there may be genuine NAs
  matrix_cutoff <- merge(matrix_cutoff, dm[duration > cutoff], by = c("csn", "adm")) 
  matrix_cutoff <- merge(matrix_cutoff, obs_cutoff_csn_val, by = "csn", all.x = TRUE) 
  matrix_cutoff <- merge(matrix_cutoff, lab_cutoff_csn_val, by = "csn", all.x = TRUE)
  matrix_cutoff[, duration := NULL]
  
  return(matrix_cutoff)
  
  
}


# load data
# =========

file_date = '2021-04-13'

load(paste0("~/EDcrowding/flow-mapping/data-raw/moves_", file_date,".rda"))
load(paste0("~/EDcrowding/flow-mapping/data-raw/summ_", file_date,".rda"))
load(paste0("~/EDcrowding/flow-mapping/data-raw/visits_all_", file_date,".rda"))



# observation data
file_date = '2021-04-14'
load(paste0("~/EDcrowding/predict-admission/data-raw/obs_real_", file_date,".rda"))


# lab data
file_date = '2021-04-14'
load(paste0("~/EDcrowding/predict-admission/data-raw/lab_orders_real_", file_date,".rda"))
load(paste0("~/EDcrowding/predict-admission/data-raw/lab_results_real_", file_date,".rda"))

# keep only lab orders that are used more than 50 times
battery_count = lab_orders_real[, .N, by = battery_code]
lab_orders_real = lab_orders_real[battery_code %in% battery_count[N>50, battery_code]]



# Create admission details--------------------------------------------------


dm <- summ[,.(csn, age, sex, presentation_time, adm, arrival_method, first_outside_proper_admission, last_ED_discharge, min_I, first_ED_admission)]

dm[, tod := factor((hour(presentation_time) %/% 4)+1)]
dm[, quarter := factor(case_when( month(presentation_time) <= 3 ~ 1,
                                  month(presentation_time) <= 6 ~ 2, 
                                  month(presentation_time) <= 9 ~ 3, 
                                  month(presentation_time) <= 12 ~ 4))]
#dm[, year := factor(year(presentation_time))]
dm[, weekend := factor(if_else(weekdays(presentation_time, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0))]
# the lab closes at 10 pm 
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

# NB - using first inside admission for duration - includes time spent in CDU
# NB - first ED admission exludes any time as an inpatient prior to arriving in ED

dm[, duration := case_when(is.na(first_outside_proper_admission) ~ as.numeric(difftime(last_ED_discharge, first_ED_admission, units = "mins")),
                               TRUE ~ as.numeric(difftime(first_outside_proper_admission, first_ED_admission, units = "mins")))]
print("Rows in design matrix initially")
rpt(dm)


# simplify arrival method
dm[, arrival := gsub(" |-", "", arrival_method)]
dm[, arrival := factor(if_else(!arrival %in% c("Ambulance",
                                                        "Walkin",
                                                        "PublicTrans",
                                                        "Ambnomedic"), "Other", arrival)) ]
dm[, arrival_method := NULL]

### add prior visits - for now excluding features that will have missing values

dm <- merge(dm, visits %>% select(csn, 
                                  # days_since_last_visit, 
                                  num_prior_adm_after_ED, num_prior_ED_visits, 
                                  # prop_adm_from_ED
                                  ), all.x = TRUE)

# # correcting error in visits processing - small number have negative number of days since last visit
# dm[, days_since_last_visit := if_else(days_since_last_visit <0, NA_real_, days_since_last_visit) ]

# prepare outcome variable
dm[, adm := if_else(adm %in% c("direct_adm", "indirect_adm"), 1, 0)]



       
# Train test split --------------------------------------

dm[, in_set := case_when(presentation_time < '2019-11-19 00:00:00' ~ "Train",
                         presentation_time < '2019-12-13 00:00:00' ~ "Val",
                         presentation_time < '2020-03-19 00:00:00' ~ "Test",
                         presentation_time < '2020-12-01 00:00:00' ~ "Train",
                         presentation_time < '2020-12-29 00:00:00' ~ "Val",
                         TRUE ~ "Test",)]


dm[, epoch := case_when(date(presentation_time) >= '2020-03-19' ~ "Post",
                        TRUE ~ "Pre")]

# For now, only process from start of Covid only
print("Rows in design matrix before COVID")
rpt(dm[epoch == "Pre"])
print("Rows in design matrix after COVID")
rpt(dm[epoch == "Post"])

# # to show implications of this cut off
# summ[, adm := if_else(adm %in% c("direct_adm", "indirect_adm"), 1, 0)]
# summ[, .N, by = list(date(presentation_time), adm)] %>% ggplot(aes(x = date, y = N, fill = as.character(adm))) +
#   geom_bar(stat = "identity") +
#   labs(title = "Number of admissions and discharges by day since beginning of Epic",
#        fill = "Admitted (1 = True)") +
#   theme(legend.position = "bottom")


setkey(dm, csn)



# Prepare location data --------------------------------------------------

loc <- moves[csn %in% dm$csn, .(csn, admission, discharge, location, visited_CDU, outside)]
loc <- merge(loc, dm[,.(csn, first_ED_admission, first_outside_proper_admission, duration)])

# remove rows where admission occurs to locations before arrival at ED
loc <- loc[admission >= first_ED_admission]


loc[, admission_e := as.numeric(difftime(admission, first_ED_admission, units = "mins"))]
loc[, discharge_e := as.numeric(difftime(discharge, first_ED_admission, units = "mins"))]

# remove rows after the patient leaves ED or other inside location (where inside includes CDU and possibly others)
loc <- loc[discharge <= first_outside_proper_admission | is.na(first_outside_proper_admission)]
print("Rows in loc")
rpt(loc)

loc[, c("admission", "discharge", "first_ED_admission", "first_outside_proper_admission") := NULL]
dm[, c("presentation_time", "first_outside_proper_admission", "min_I", "first_ED_admission", "last_ED_discharge") := NULL]

cols = colnames(copy(dm)[, c("csn", "adm", "duration", "in_set") := NULL])
cols_ = paste("a", cols, sep="_")
setnames(dm, cols, cols_)

setkey(loc, csn)

# Create timeslices -------------------------------------------------------


timeslices <- c(0, 15, 30, 60, 90, 120, 180, 240, 300, 360, 480, 480+4*60)

for (i in seq(1, length(timeslices) -1, 1)) {
  print(paste0("Processing timeslice ", timeslices[i]))
  filenum <- case_when(nchar(as.character(timeslices[i])) == 1 ~ paste0("00", timeslices[i]),
                       nchar(as.character(timeslices[i])) == 2 ~ paste0("0", timeslices[i]),
                      TRUE ~ as.character(timeslices[i]))
  name_ <- paste0("dm", filenum)
  ts <- create_timeslice(loc, dm, obs_real, lab_real, timeslices[i], timeslices[i+1])
  assign(name_, ts)
}



save(dm000, file = paste0("EDcrowding/predict-admission/data-raw/dm000_",today(),".rda"))
save(dm015, file = paste0("EDcrowding/predict-admission/data-raw/dm015_",today(),".rda"))
save(dm030, file = paste0("EDcrowding/predict-admission/data-raw/dm030_",today(),".rda"))
save(dm060, file = paste0("EDcrowding/predict-admission/data-raw/dm060_",today(),".rda"))
save(dm090, file = paste0("EDcrowding/predict-admission/data-raw/dm090_",today(),".rda"))
save(dm120, file = paste0("EDcrowding/predict-admission/data-raw/dm120_",today(),".rda"))
save(dm180, file = paste0("EDcrowding/predict-admission/data-raw/dm180_",today(),".rda"))
save(dm240, file = paste0("EDcrowding/predict-admission/data-raw/dm240_",today(),".rda"))
save(dm300, file = paste0("EDcrowding/predict-admission/data-raw/dm300_",today(),".rda"))
save(dm360, file = paste0("EDcrowding/predict-admission/data-raw/dm360_",today(),".rda"))
save(dm480, file = paste0("EDcrowding/predict-admission/data-raw/dm480_",today(),".rda"))

dm[, row_id := seq_len(nrow(dm))]
save(dm, file = paste0("EDcrowding/predict-admission/data-raw/dm_",today(),".rda"))






