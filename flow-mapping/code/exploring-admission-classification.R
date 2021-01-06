
# About this file ---------------------------------------------------------

# Explores implications of the admission classification

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)

# Create function  --------------------------------------------------------

rpt <- function(dataframe) {
  print(dataframe %>% select(csn) %>% n_distinct())
}


# Load data ---------------------------------------------------------------
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_2021-01-06.rda")
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-01-06.rda")
load("~/EDcrowding/flow-mapping/data-raw/all_patient_class_2021-01-06.rda")
rpt(moves)


# Why are so many inpatients discharged?  ---------------------------------

fast_dis_inpatient <- data.table(ED_csn_summ %>% filter(patient_class == "INPATIENT", adm == "fast_dis"))
setkey(fast_dis_inpatient, csn)
moves_1 <- moves[fast_dis_inpatient, nomatch = 0]
all_patient_class %>% filter(csn == "1004891335")
moves_1[csn == "1004891335"]

loc = moves_1 %>% select(csn, first_dept, final_location) %>% distinct()
loc[,.N, by = first_dept] # 3010 began in ED
loc[,.N, by = final_location] # 2470 ended in OTF

moves_1[final_location %in% c("EAU", "T01ECU")] # note that these should not be considered as discharges - need to fix - but these are relatively few

csn_dttm = moves_1[final_location == "OTF POOL" & location == "OTF POOL"] %>% 
  select(csn, presentation_time, first_dept, min_E, max_E, min_I, max_E, admission, discharge) %>% 
  mutate(match_dttm = min_I == max_E) %>% 
  mutate(time_OTF_to_class_change = difftime(admission, max_E, units = "mins"))
