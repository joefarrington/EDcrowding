# ===============
# About this file
# ===============
# Loads data on ED patients locations
# Formats a matrix for input into a ML model to predict location
# Matrix holds number of patients in any location in each hour
#


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


load("~/EDcrowding/flow-mapping/data-raw/edgedf_2021-03-03.rda")
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-03-03.rda")
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-03-03.rda")

# temporarily for old data
load("~/EDcrowding/data-prep-for-ML/data-raw/edgedf_2021-03-03.rda")
load("~/EDcrowding/data-prep-for-ML/data-raw/moves_2021-03-03.rda")
load("~/EDcrowding/data-prep-for-ML/data-raw/summ_2021-03-03.rda")

# Set parameters ----------------------------------------------------------


matrix_start_date <- as.POSIXct("2019-05-01 00:00:00")
matrix_end_date <- as.POSIXct('2020-03-19 00:00:00')
covid_start <- as.POSIXct('2020-03-19 00:00:00')


# Pre-process data ----------------------------------------------------

summ = summ[admission_time >= matrix_start_date]
summ = summ[admission_time < matrix_end_date]

# # to see implications of this cut point as a chart
# summ[, adm := if_else(adm %in% c("direct_adm", "indirect_adm"), 1, 0)]
# summ[presentation_time > '2020-02-01' & presentation_time < '2020-06-01', .N, by = list(date(presentation_time), adm)] %>% 
#   ggplot(aes(x = date, y = N, fill = as.character(adm))) +
#   scale_x_date(date_breaks = "weeks" , date_labels = "%m/%d") +
#   geom_bar(stat = "identity") +
#   labs(title = paste0("Number of admissions and discharges from 1 Feb to 31 May 2020 showing Covid cut point of ",date(covid_start)),
#        fill = "Admitted (1 = True)") +
#   theme(legend.position = "bottom") +
#   geom_vline(xintercept = date(covid_start), linetype = "dashed")
# rpt(summ)


# remove minority locations
edgedf[, from := case_when(from == "OTF POOL" ~ "OTF",
                           from %in% c("PAEDS", "SAA") ~ "Other",
                           from == "UCHT00CDU" ~ "CDU",
                           from == "WR_POOL" ~ "Waiting",
                           TRUE ~ from)]

edgedf[, to := case_when(to == "OTF POOL" ~ "OTF",
                           to %in% c("PAEDS", "SAA") ~ "Other",
                           to == "UCHT00CDU" ~ "CDU",
                           to == "WR_POOL" ~ "Waiting",
                         
                           TRUE ~ to)]

moves[, location := case_when(location == "OTF POOL" ~ "OTF",
                              location %in% c("PAEDS", "SAA") ~ "Other",
                              location == "DIAGNOSTICS" & admission > date(covid_start) ~ "Other",
                              location == "UCHT00CDU" ~ "CDU",
                              location == "WR_POOL" ~ "Waiting",
                              
                              TRUE ~ location)]


# # I considered merging wtih summ to get first outside ED exit; this is to avoid double counting of cases where there is more than one exit
# # but then we lose people with first outside admission prior to ED - commenting this out for now
# rpt(moves[num_ED_exit > 1]) 
# edgedf = merge(edgedf, summ[,.(csn, adm, first_outside_proper_admission)], by = "csn")
# edgedf[, drop_row := (adm %in% c("indirect_adm", "direct_adm") & dttm > first_outside_proper_admission)]
# edgedf = edgedf[!(drop_row)]

# Create adjacency matrix for the two epochs ------------------------------

edgedf = edgedf[csn %in% summ$csn]
rpt(edgedf)


# identify moves where patient is admitted from ED
edgedf[from_dept %in% c("ED", "UCHT00CDU") & !(to_dept %in% c("ED", "UCHT00CDU") | is.na(to_dept)), adm := "Admitted"]

# identify moves where patient is discharged from ED
edgedf[from_dept %in% c("ED", "UCHT00CDU") & is.na(to_dept), adm := "Discharged"]

# update "to" column with admitted or discharged
edgedf[from_dept %in% c("ED", "UCHT00CDU"), to_new := case_when(!is.na(adm) ~ adm,
                                                                TRUE ~  to)]
# check number of admission and discharges matches summ
# note that this won't exactly match summ$adm because some visits involve > 1 admission
rpt(edgedf[from_dept %in% c("ED", "UCHT00CDU") & to_new == "Discharged"])
rpt(edgedf[from_dept %in% c("ED", "UCHT00CDU") & to_new == "Admitted"])


# split into before and after covid
edgedf_before_covid <- edgedf[csn %in% summ[presentation_time < covid_start, csn]]
rpt(edgedf_before_covid)

edgedf_after_covid <- edgedf[csn %in% summ[presentation_time >= covid_start, csn]]
rpt(edgedf_after_covid)

#  after covid, Diagnostics becomes a very infrequently used location
edgedf_after_covid[, from := case_when(from == "DIAGNOSTICS" ~ "Other",
                                         TRUE ~ from)]
edgedf_after_covid[, to_new := case_when(to_new == "DIAGNOSTICS" ~ "Other",
                         TRUE ~ to_new)]

# create adjacency matrices

before_covid_adj_matrix <- edgedf_before_covid[from_dept %in% c("ED", "UCHT00CDU")] %>%
  group_by(from, to_new) %>%
  summarise(tot = n()) %>%
  pivot_wider(names_from = to_new, values_from = tot) %>%
  column_to_rownames("from") %>%
  replace(is.na(.), 0) 

before_covid_adj_matrix <- before_covid_adj_matrix %>% 
  # reorder colnames
  select(colnames(before_covid_adj_matrix %>% select(-Admitted, -Discharged))[order(colnames(before_covid_adj_matrix %>% select(-Admitted, -Discharged)))], 
         Admitted, Discharged)


after_covid_adj_matrix <- edgedf_after_covid[from_dept %in% c("ED", "UCHT00CDU")] %>%
  group_by(from, to_new) %>%
  summarise(tot = n()) %>%
  pivot_wider(names_from = to_new, values_from = tot) %>%
  column_to_rownames("from") %>%
  replace(is.na(.), 0) 

after_covid_adj_matrix <- after_covid_adj_matrix %>% 
  # reorder colnames
  select(colnames(after_covid_adj_matrix %>% select(-Admitted, -Discharged))[order(colnames(after_covid_adj_matrix %>% select(-Admitted, -Discharged)))], 
         Admitted, Discharged)

outFile = paste0("EDcrowding/data-prep-for-ML/data-output/before_covid_adj_matrix_",today(),".csv")
write.csv(before_covid_adj_matrix, file = outFile, row.names = TRUE)

outFile = paste0("EDcrowding/data-prep-for-ML/data-output/after_covid_adj_matrix_",today(),".csv")
write.csv(after_covid_adj_matrix, file = outFile, row.names = TRUE)

# Process data by time periods ------------------------------------------------------------


# create a series with all the required time periods by hour

get_nums_by_dttm <- function(date_range, moves, edgdf) {
  
  num_in_location <- data.table()
  moved_from_location <- data.table()
  
  
  
  for (i in 2:length(date_range)) {
    if (i %% 500 == 0) {
      print(paste("Processed",i,"datetimes"))
    }
    
    # number in ED location at the timepoint
    num = moves[admission <= date_range[i] & discharge >= date_range[i] & department %in% c("ED", "UCHT00CDU"), .N, by = location]
    # if noone is in the location, we still need a row to represent the timeslot
    if (nrow(num) == 0) {
      num = data.table(DateTime = date_range[i])
    } else {
      num$DateTime = date_range[i] 
    }
    num_in_location <- bind_rows(num_in_location, num)
    
    # numbers leaving ED location within the hour up to the timepoint
    moved = moves[discharge > date_range[i-1] & discharge <= date_range[i] & department %in% c("ED", "UCHT00CDU"), .N, by = location]
    
    # if noone moved, we still need a row to represent the timeslot
    if (nrow(moved) == 0) {
      moved = data.table(DateTime = date_range[i])
    } else {
      moved$DateTime = date_range[i] 
    }
    moved_from_location <- bind_rows(moved_from_location, moved)
  }
  
  adm_during_hour = data.table()
  for (i in 2:length(date_range)) {
    adm = data.table(edgedf[from_dept %in% c("ED", "UCHT00CDU") & !(to_dept %in% c("ED", "UCHT00CDU") | is.na(to_dept)) & dttm > date_range[i-1] & dttm <= date_range[i], .N])
    adm$DateTime = date_range[i] 
    # adm$i = i
    # adm$first_outside_prop = nrow(summ[first_outside_proper_admission > date_range[i-1] & first_outside_proper_admission <= date_range[i]])
    # adm$first_inside_exit = nrow(summ[first_outside_proper_admission > date_range[i-1] & first_outside_proper_admission <= date_range[i]])
    adm_during_hour = bind_rows(adm_during_hour, adm)
  }
  
  setnames(adm_during_hour, "V1", "adm")
  
  # pivot to wide matrix and add columns
  
  num_in_location[, time_of_day := hour(DateTime)]
  num_in_location[, month := month(DateTime)]
  num_in_location[, day_of_week := wday(DateTime)]
  
  moved_from_location[, time_of_day := hour(DateTime)]
  moved_from_location[, month := month(DateTime)]
  moved_from_location[, day_of_week := wday(DateTime)]
  
  moved_w <- moved_from_location %>% pivot_wider(names_from = location, values_from = N, values_fill = 0) %>% 
    left_join(adm_during_hour)
  
  # NA rows are created if noone moved during a time slot; these become NA column after pivot
  if (sum(grepl("NA", colnames(moved_w)))>0) {
    moved_w <- moved_w %>% select(-`NA`)
  }

  
  num_w <- num_in_location %>% pivot_wider(names_from = location, values_from = N, values_fill = 0)  %>% 
    left_join(adm_during_hour)
  # NA rows are created if noone was in any location during a time slot; these become NA column after pivot
  
  if (sum(grepl("NA", colnames(num_w)))>0) {
    num_w <- num_w %>% select(-`NA`)
  }
  
  moved_w <- moved_w %>% 
    # reorder colnames
    select(DateTime:day_of_week, colnames(moved_w)[5:(ncol(moved_w)-1)][order(colnames(moved_w)[5:(ncol(moved_w)-1)])],
           adm)
  
  num_w <- num_w %>% 
    # reorder colnames
    select(DateTime:day_of_week, colnames(num_w)[5:(ncol(num_w)-1)][order(colnames(num_w)[5:(ncol(num_w)-1)])],
           adm)
  
  
  
  output <- list(moved_w, num_w)
  
  return(output)
  
}


# before Covid started
date_range <- seq(matrix_start_date - hours(1), covid_start, by = "hours")
before_covid <- get_nums_by_dttm(date_range, moves, edgedf_before_covid)

outFile = paste0("EDcrowding/data-prep-for-ML/data-output/before_covid_moved_from_location_",today(),".csv")
write.csv(before_covid[[1]], file = outFile, row.names = FALSE)

outFile = paste0("EDcrowding/data-prep-for-ML/data-output/before_covid_num_in_location_",today(),".csv")
write.csv(before_covid[[2]], file = outFile, row.names = FALSE)


# after Covid started
date_range <- seq(covid_start - hours(1), matrix_end_date, by = "hours")
after_covid <- get_nums_by_dttm(date_range, moves, edgedf_after_covid)


outFile = paste0("EDcrowding/data-prep-for-ML/data-output/after_covid_moved_from_location_",today(),".csv")
write.csv(after_covid[[1]], file = outFile, row.names = FALSE)

outFile = paste0("EDcrowding/data-prep-for-ML/data-output/after_covid_num_in_location_",today(),".csv")
write.csv(after_covid[[2]], file = outFile, row.names = FALSE)





# Code for debugging ------------------------------------------------------

# 
# 
# library(readr)
# 
# num_in_location_2021_02_16 <- read_csv("~/EDcrowding/data-prep-for-ML/data-output/num_in_location_2021-02-16.csv")
# moved_from_location_2021_02_16 <- read_csv("~/EDcrowding/data-prep-for-ML/data-output/moved_from_location_2021-02-16.csv")
# 
# num_in_location <- as.data.table(num_in_location_2021_02_16)
# moved_from_location <- as.data.table(moved_from_location_2021_02_16)
# 
# setkey(num_in_location, DateTime)
# setkey(moved_from_location, DateTime)
# 
# 
# num_in_location[, .N, by = DateTime][N>1] # one date is repeated - at time of hour change
# moved_from_location[, .N, by = DateTime][N>1] # one date is repeated - at time of hour change
# 
# x = merge(num_in_location, moved_from_location, by = "DateTime", all.x = TRUE)
# x[is.na(time_of_day.y)] # some datetimes seem to be missing at 2019-06-11 00:00:00 through to 14:00:00 and 2019-06-19 03:00:00
# 
# x = merge(moved_from_location, num_in_location, by = "DateTime", all.x = TRUE)
