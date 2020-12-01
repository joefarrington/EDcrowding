
# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)


# new csn from Star
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_raw_all_2020-11-30.rda")

# old csn from Star and Flow inc CDU
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_CDUinED_2020-11-16.rda")
ED_csn_summ_CDU <- ED_csn_summ
ED_csn_summ_CDU <- ED_csn_summ_CDU %>% rename(adm_CDU = adm)

# old csn from Star and Flow exc CDU
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-11-04.rda")

paed_location <- ED_bed_moves %>% filter(room3 %in% c("PAEDS TRIAGE", "PAEDS")) %>% 
  select(csn) %>% distinct() %>% mutate(paed = TRUE)


# Comparing this with old ED_csn_summ
compare <- ED_csn_summ_raw %>% 
  filter(date(admission_time) <= "2020-10-31" ) %>% 
  select(mrn,csn, patient_class, admission_time) %>%  
  left_join(ED_csn_summ %>% select(csn_old, adm), by = c("csn" = "csn_old")) %>% 
  left_join(ED_csn_summ_CDU %>% select(csn_old, adm_CDU), by = c("csn" = "csn_old")) 

compare <- compare %>% left(paed_location)

compare %>% filter(is.na(adm), paed)


compare <- compare %>% 
  mutate(adm_star = if_else(patient_class == "INPATIENT", TRUE, FALSE))

compare <- compare %>% 
  mutate(consensus_discharged = adm + adm_CDU + adm_star == 0,
         consensus_admitted = adm + adm_CDU + adm_star == 3)

compare <- compare %>% 
  mutate(consensus = if_else(consensus_discharged |  consensus_admitted , TRUE, FALSE), 
         CDU_case = if_else(adm & !adm_CDU, TRUE, FALSE))


# there could be various reasons for a mismatch in whether admitted or not
# 1. the visit wasn't in my original dataset(is.na(adm))
# 2. this is a CDU visit which appears to be classed as not admitted in refactored Star
# 3. this is an overlap row around the time of beginning of September 2020 e.g. csn 1022163865 which had 2 entries in the old ED_csn_summ
# 4. this visit ends in OTF which Star has no INPATIENT entry for
# 5. Some were admitted near end of October and stayed in location meaning empty discharge from next location thus not showing up in bed_moves
# 6. Some have odd arrival rows e.g. 1020805305 has virtual arrival location in July 2020

compare2 <- compare %>% 
  filter(!is.na(adm), !consensus, !CDU_case)

compare2 <- compare2 %>% 
  mutate(CDU_case = if_else(adm & !adm_CDU, TRUE, FALSE))

b = bed_moves %>% inner_join(compare2  %>% select(csn, patient_class))

cz = "1021092450"
bed_moves %>% filter(csn == cz)
ED_bed_moves %>% filter(csn == cz)
ED_csn_summ %>%  filter(csn == cz)
ED_csn_summ_raw %>%  filter(csn == cz) %>% select(age)
compare2 %>%  filter(csn == cz)



# Looking at admissions that were missing in the original 

compare %>% filter(is.na(adm), is.na(paed), admission_time < "2020-08-31") %>% mutate(date_ = date(admission_time)) %>% 
  group_by(date_) %>% summarise(tot = n())  %>% 
  ggplot(aes(x = date_, y = tot)) + geom_bar(stat = "identity") +
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 week",
               date_labels = "%Y-%b") +
  theme_classic() +
  labs(title = "Number of csns missed in previous analysis by date and not otherwise accounted for", 
       x = "Date", y = "Number of missed csns")



# Looking at other 

load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-11-30.rda")

ED_csn_summ %>% filter(!is.na(arrival_method)) %>%
  group_by(arrival_method) %>% summarise(tot = n()) %>% 
  ggplot(aes(x = arrival_method, y = tot)) + geom_bar(stat = "identity") +
  labs(title = "Arrival method", x= NULL, y = "Num visits") +
  theme_classic() + coord_flip()


ED_csn_summ %>% filter(!is.na(discharge_disposition)) %>%
  group_by(discharge_disposition) %>% summarise(tot = n()) %>% 
  ggplot(aes(x = discharge_disposition, y = tot)) + geom_bar(stat = "identity") +
  labs(title = "Discharge disposition", x= NULL, y = "Num visits") +
  theme_classic() + coord_flip()


ED_csn_summ %>% filter(!is.na(discharge_destination)) %>%
  group_by(discharge_destination) %>% summarise(tot = n()) %>% 
  ggplot(aes(x = discharge_destination, y = tot)) + geom_bar(stat = "identity") +
  labs(title = "Arrival method", x= NULL, y = "Num visits") +
  theme_classic() + coord_flip()


