
# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)


# Comparing with number of visits in old data -------------------------------------------------



# new csn from Star - ED_csn_summ_new
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-12-01.rda")
ED_csn_summ_new <- ED_csn_summ %>% 
  filter(date(admission_time) <= "2020-10-31" )

ED_csn_summ_new %>% select(csn) %>% n_distinct()

# old csn from Star and Flow inc CDU
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_CDUinED_2020-11-16.rda")
ED_csn_summ_CDU <- ED_csn_summ
ED_csn_summ_CDU <- ED_csn_summ_CDU %>% rename(adm_CDU = adm) %>% 
  filter(date(arrival_dttm) <= "2020-10-31" ) 

# old csn from Star and Flow exc CDU
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-11-04.rda")
ED_csn_summ_ex_CDU <- ED_csn_summ  %>% 
  filter(date(arrival_dttm) <= "2020-10-31" )

# new bed moves to get adults who went to paeds
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_2020-12-01.rda")
paed_location <- ED_bed_moves %>% filter(room3 %in% c("PAEDS TRIAGE", "PAEDS")) %>% 
  select(csn) %>% distinct() %>% mutate(paed = TRUE)


# Comparing this with old ED_csn_summ
compare <- ED_csn_summ_new %>% 
  select(mrn,csn, patient_class, admission_time) %>%  
  left_join(ED_csn_summ %>% rename(csn_new = csn) %>% select(csn_old, csn_new, adm), by = c("csn" = "csn_old")) %>% distinct() 

ED_csn_summ_new %>% select(csn) %>% n_distinct()
compare %>% select(csn) %>% n_distinct()

compare2 = compare %>% 
  left_join(ED_csn_summ_CDU %>% rename(csn_new2 = csn)%>% select(csn_old, csn_new2, adm_CDU), by = c("csn" = "csn_old")) %>% distinct()

compare2 %>% select(csn) %>% n_distinct()

compare3 <- compare2 %>% left_join(paed_location)

compare3 <- compare3 %>% 
  mutate(adm_star = if_else(patient_class == "INPATIENT", TRUE, FALSE))

# look at cases missed last time

compare3 %>% filter(is.na(adm)) %>% select(csn) %>% n_distinct() #3233 missed
compare3 %>% filter(is.na(adm), paed) %>% select(csn) %>% n_distinct() # of which 1946 were adults in paeds

compare3 %>% filter(is.na(adm), is.na(paed)) %>% group_by(patient_class) %>% summarise(n()) #37% were admitted

missing_cases = compare3 %>% filter(is.na(adm), is.na(paed)) %>% distinct()
missing_cases %>% select(csn) %>% n_distinct() # 1135



# were they in all bed moves from flow? 
load("~/EDcrowding/flow-mapping/data-raw/bed_moves_raw_all_flow_2020-11-09.rda")
missing_cases = missing_cases %>% left_join(bed_moves_all %>% select(csn) %>% distinct() %>% mutate(in_flow = TRUE))
missing_cases %>% filter(admission_time < '2020-09-01', is.na(in_flow)) %>% select(csn) %>% n_distinct()
# 13 were not picked up in flow

missing_cases %>% filter(admission_time < '2020-09-01', in_star) %>% select(csn) %>% n_distinct()
# 747 were elimiated from flow for some reason
load("~/EDcrowding/flow-mapping/data-raw/Excluded_csns_all_CDUinED_2020-11-16.rda")
missing_cases %>% filter(admission_time < '2020-09-01', in_flow) %>% select(csn) %>% left_join(excluded_csns)

# were they in all bed moves from star
load("~/EDcrowding/flow-mapping/data-raw/bed_moves_raw_all_star_from_Sep_2020-11-09.rda")
bed_moves_all_star <- bed_moves_all_star %>% filter(admission < '2020-10-31')
missing_cases = missing_cases %>% left_join(bed_moves_all_star %>% select(csn) %>% distinct() %>% mutate(in_star = TRUE))
missing_cases %>% filter(admission_time >= '2020-09-01', is.na(in_star)) 
# 7 were not picked up in Star

missing_cases %>% filter(admission_time >= '2020-09-01', in_star) %>% select(csn) %>% n_distinct()
# 368 were eliminated from star for some reason
load("~/EDcrowding/flow-mapping/data-raw/Excluded_csns_all_CDUinED_2020-11-16.rda")
missing_cases %>% filter(admission_time >= '2020-09-01', in_star) %>% select(csn) %>% left_join(excluded_csns)

# were they in flow or star - note that this dataset is somewhat filtered to exclude certain rows
load("~/EDcrowding/flow-mapping/data-raw/bed_moves_raw_all_flow_and_star_2020-11-10.rda")
missing_cases = missing_cases %>% left_join(bed_moves_raw_all %>% ungroup() %>% select(csn) %>% distinct() %>% mutate(in_flow_or_star = TRUE))
sum(is.na(missing_cases$in_flow_or_star)) # 93 were not in either flow or star

# # checking against the new bedmoves from Star_test
# load("~/EDcrowding/flow-mapping/data-raw/bed_moves_2020-11-30.rda")
# bed_moves_raw_all %>% inner_join(missing_cases %>% filter(is.na(missing_cases$in_flow_or_star)))


# there could be various reasons for a mismatch in cases
# 1. these were adults who visited paeds
# 2. they were not picked up in the original SQL queries (93 were in neither flow or star)
# 3. I eliminated them in my initial processing eg becuase admission and discharge times were the same


# look at consensus re admission ------------------------------------------



compare4 <- compare3 %>% filter(!is.na(adm)) %>% 
  mutate(consensus_discharged = adm + adm_CDU + adm_star == 0,
         consensus_admitted = adm + adm_CDU + adm_star == 3)

compare4 <- compare4 %>% 
  mutate(consensus = if_else(consensus_discharged |  consensus_admitted , TRUE, FALSE), 
         CDU_case = if_else(adm & !adm_CDU, TRUE, FALSE))

compare4 %>% filter(consensus) %>% select(csn) %>% n_distinct()
compare4 %>% filter(!consensus, CDU_case) %>% select(csn) %>% n_distinct()
compare4 %>% filter(!consensus, !CDU_case) %>% select(csn) %>% n_distinct()


# there could be various reasons for a mismatch in whether admitted or not
# 1. the visit wasn't in my original dataset(is.na(adm))
# 2. this is a CDU visit which appears to be classed as not admitted in refactored Star
# 3. this is an overlap row around the time of beginning of September 2020 e.g. csn 1022163865 which had 2 entries in the old ED_csn_summ
# 4. this visit ends in OTF which Star has no INPATIENT entry for
# 5. Some were admitted near end of October and stayed in location meaning empty discharge from next location thus not showing up in bed_moves
# 6. Some have odd arrival rows e.g. 1020805305 has virtual arrival location in July 2020

compare5 <- compare4 %>% 
  filter(!consensus, !CDU_case)

compare5 %>% select(csn) %>% n_distinct()

# b = bed_moves %>% inner_join(compare5  %>% select(csn, patient_class))
# 
# cz = "1021092450"
# bed_moves %>% filter(csn == cz)
# ED_bed_moves %>% filter(csn == cz)
# ED_csn_summ %>%  filter(csn == cz)
# ED_csn_summ_new %>%  filter(csn == cz) %>% select(age)
# compare2 %>%  filter(csn == cz)



# # Looking at admissions that were missing in the original 
# 
# compare %>% filter(is.na(adm), is.na(paed), admission_time < "2020-08-31") %>% mutate(date_ = date(admission_time)) %>% 
#   group_by(date_) %>% summarise(tot = n())  %>% 
#   ggplot(aes(x = date_, y = tot)) + geom_bar(stat = "identity") +
#   scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 week",
#                date_labels = "%Y-%b") +
#   theme_classic() +
#   labs(title = "Number of csns missed in previous analysis by date and not otherwise accounted for", 
#        x = "Date", y = "Number of missed csns")
# 
# 

# Looking at other new variables

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


# Looking at distribution of duration from presentation to admission

load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-12-01.rda")

ED_csn_summ = ED_csn_summ %>% mutate(time_to_admission = difftime(admission_time, presentation_time, units = "hours")) 
ED_csn_summ %>% filter(time_to_admission <0) #25 less than zero
ED_csn_summ %>% filter(time_to_admission >3) #333 greater than four hours

ED_csn_summ %>% filter(time_to_admission > 0, time_to_admission <3,
                       arrival_method %in% c("Amb no medic", "Ambulance", "Public Trans", "Walk-in")) %>% 
  ggplot(aes(x = as.numeric(time_to_admission)*60)) + geom_histogram(binwidth = 15) + 
  labs(title = "ED patients time from presentation to admission" ,
       subtitle = "Excluding where presentation is after admission (25 cases) and more than four hours before admission (333 cases)", 
       x = "Time from presentation to ED admission (mins)") +
  facet_wrap(.~arrival_method) +
  scale_x_continuous(breaks = seq(0,180, 15))

# Look at missingness of presentation 

load("~/EDcrowding/flow-mapping/data-raw/csn_summ_2020-11-30.rda")
csn_summ %>% filter(arrival_method %in% c("Amb no medic", "Ambulance", "Public Trans", "Walk-in")) %>% 
  group_by(arrival_method) %>% summarise(num_missing = sum(is.na(presentation_time)),
                                         tot = n(),
                                         prop_missing = sum(is.na(presentation_time))/tot)


