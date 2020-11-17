# About this file
# ===============

# created this file to get all bed moves data, in order to look up information about prior visits
# it collects all bed moves records from flow, and recent records from Star
# for each visit the number of previous visits and the time since the last visit is calculated

# load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)

# Create functions
# ===============

save_chart = function(chart_title, g, width = 1077, height = 659) {
  png(paste0("EDcrowding/flow-mapping/media/", chart_title, ".png"), width = width, height = height) 
  print(g)
  dev.off()
}


# hl7_location seems to have more complete info than room and bed for some rows
# but locations are grouped into one string; this function splits them

split_location <- function(hl7_location, n) {
  s <- unlist(strsplit(hl7_location, "\\^"))
  return(s[n])
}
split_location <- Vectorize(split_location)


# load data
# ==========


# Get data from Star ------------------------------------------------------


ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")

# sqlQuery <- "select  * from
#   flow.bed_moves
# "
# sqlQuery <- gsub('\n','',sqlQuery)
# bed_moves_all <- as_tibble(dbGetQuery(ctn, sqlQuery))
# save(bed_moves_all, file = paste0('EDcrowding/flow-mapping/data-raw/bed_moves_raw_all_flow_',today(),'.rda'))

load("~/EDcrowding/flow-mapping/data-raw/bed_moves_raw_all_flow_2020-11-09.rda")

# sqlQuery <- "select  * from
#     star.bed_moves
#     where admission > '2020-09-01'
# "
# sqlQuery <- gsub('\n','',sqlQuery)
# bed_moves_all_star <- as_tibble(dbGetQuery(ctn, sqlQuery))
# save(bed_moves_all_star, file = paste0('EDcrowding/flow-mapping/data-raw/bed_moves_raw_all_star_from_Sep_',today(),'.rda'))

load("~/EDcrowding/flow-mapping/data-raw/bed_moves_raw_all_star_from_Sep_2020-11-09.rda")


# Create merged dataset ---------------------------------------------------


     
bed_moves_raw_all <- bed_moves_all %>% filter(admission <= today()) %>% 
  bind_rows(bed_moves_all_star  %>% filter(admission <= today())) %>% distinct()


bed_moves_raw_all <- bed_moves_raw_all %>% 
  arrange(mrn, admission)


bed_moves_raw_all <- bed_moves_raw_all %>% group_by(mrn) %>% 
  mutate(next_department = lead(department))


bed_moves_raw_all <- bed_moves_raw_all %>% group_by(mrn) %>% 
  mutate(next_csn = lead(csn))
 

bed_moves_raw_all <- bed_moves_raw_all %>% 
  group_by(mrn) %>% 
  mutate(next_admission = lead(admission))

save(bed_moves_raw_all, file = paste0('EDcrowding/flow-mapping/data-raw/bed_moves_raw_all_flow_and_star_',today(),'.rda'))


# Explore AEDU ------------------------------------------------------------

# this returns 4.5K rows; looks like next admission is not always instantaneous and can be before discharge from ED
ED_to_AEDU <- bed_moves_raw_all %>% filter(department == "UCH EMERGENCY DEPT", next_department == "UCH T00 AMBULATORY ECU") %>% 
  filter(csn != next_csn) %>% 
  select(mrn, csn, next_csn, discharge) %>% rename(ED_discharge = discharge)

ED_to_AEDU <- ED_to_AEDU %>% 
  # note this is a one_many join as it will pick up all rows for the next csn
  left_join(bed_moves_raw_all %>% select(-next_csn) %>%  rename(next_csn = csn))

ED_to_AEDU <- ED_to_AEDU %>% 
  mutate(time_ED_to_AEDU = difftime(admission, ED_discharge,units = "hours"),
         duration_row = difftime(discharge, admission,units = "hours")) %>% select(mrn, csn, next_csn, ED_discharge, admission, time_ED_to_AEDU, everything())


save(ED_to_AEDU, file = paste0('EDcrowding/flow-mapping/data-raw/ED_to_AEDU_',today(),'.rda'))


# note there are a lot of negative times (ie arrived in AEDU before leaving ED) because OTF rows for the old csn are left hanging
# but in each case the admission dttms are correct so we can ignore this

save_chart("Boxplot with time from ED discharge to arrival in AEDU (hours)",
  ED_to_AEDU %>% filter(department == "UCH T00 AMBULATORY ECU") %>%
    ggplot(aes(x = as.numeric(time_ED_to_AEDU))) + geom_boxplot() +
    theme_classic() +
    labs(x = "Time from ED discharge to arrival in AEDU (hours)", 
         title = "All patients with next destination after ED being AEDU (with a new csn)",
         subtitle = "Negative times are due to OTF rows not being updated with the correct ED discharge times")
  
)

save_chart("Histogram with time from ED discharge to arrival in AEDU (hours) ",
  ED_to_AEDU %>% filter(department == "UCH T00 AMBULATORY ECU", time_ED_to_AEDU >= 0, time_ED_to_AEDU < 24) %>%
    ggplot(aes(x = as.numeric(time_ED_to_AEDU))) + geom_histogram(binwidth = 1) +
    theme_classic() +
    labs(x = "Time from ED discharge (hours)", 
         title = "All patients with next destination after ED being AEDU (with a new csn), arriving less than 24 hours after ED discharge",
         subtitle =  "Based on this 3 hours might be a reasonable cutoff to assume patient went straight to AEDU"
    )
)

# how many patients went within 3 hours? 
ED_to_AEDU %>% filter(department == "UCH T00 AMBULATORY ECU", time_ED_to_AEDU < 3)


# Exploring admissions from AEDU -------------------------------------------


all_AEDU = bed_moves_raw_all %>% filter(department == "UCH T00 AMBULATORY ECU") %>% 
  arrange(mrn, admission) %>% 
  mutate(time_to_next_admission = difftime(next_admission, discharge ,units = "days"),
         duration_row = difftime(discharge, admission, units = "days"))


save_chart("Boxplot with time to next UCLH admission after discharge from AEDU (days) ",
           all_AEDU  %>%
             ggplot(aes(x = as.numeric(time_to_next_admission))) + geom_boxplot() +
             theme_classic() +
             labs(x = "Time from AEDU discharge to next admission (days)", 
                  title = "All patients with an later uCLH visit after time in AEDU",
                  subtitle = "1793 of 12015 patients had no further visit"
             )
)

save_chart("Boxplot showing duration in AEDU (days) ",
           all_AEDU  %>%
             ggplot(aes(x = as.numeric(duration_row))) + geom_boxplot() +
             theme_classic() +
             labs(x = "Time between admission and discharge from AEDU (days)", 
                  title = "All patients visiting AEDU (total = 12,015)",
                  subtitle = "Some records are marked with admission times after discharge; hence negative times"
             )
)

all_AEDU %>% filter(time_to_next_admission <1) %>% group_by(next_department) %>% summarise(tot = n()) %>% arrange(desc(tot))


# Get list of csns that changed -------------------------------------------

# this bit looks for a match on discharge of one row exactly matching admission on the next
# note - 6 of these are in ED_to_AEDU 

csn_changed <- bed_moves_raw_all %>% filter(discharge == next_admission, csn != next_csn) %>% select(admission, discharge, department, next_department, csn, next_csn, next_admission)

# only keep the ones that involve ED
csn_changed <- csn_changed %>% 
  filter(department == "UCH EMERGENCY DEPT" | next_department == "UCH EMERGENCY DEPT") 

# don't need to worry about the 6 that go to AEDU, as these are covered elsewhere
csn_changed <- csn_changed %>% 
  filter(next_department !=  "UCH T00 AMBULATORY ECU")


# save the rest

save(csn_changed, file = paste0('EDcrowding/flow-mapping/data-raw/csn_changed_',today(),'.rda'))


# Explore CDU -------------------------------------------------------------

all_CDU = bed_moves_raw_all %>% filter(department == "UCH T00 CLIN DECISION") %>% 
  arrange(mrn, admission) %>% 
  mutate(time_to_next_admission = difftime(next_admission, discharge ,units = "days"),
         duration_row = difftime(discharge, admission, units = "days"))

# note that many repeat rows due to going to more than one CDU location

all_CDU %>% filter(!is.na(next_department)) %>% group_by(next_department) %>%
  summarise(tot = n()) %>% arrange(desc(tot))

save(all_CDU, file = paste0('EDcrowding/flow-mapping/data-raw/all_CDU_',today(),'.rda'))

all_CDU %>% ggplot(aes(x = duration_row)) + geom_boxplot()

all_CDU %>% filter(time_to_next_admission < 1, next_department != "UCH T00 CLIN DECISION") %>% mutate(next_department = case_when(is.na(next_department) ~ "No further move",
                                               TRUE ~ next_department)) %>% 
  group_by(next_department) %>% summarise(tot = n()) %>% 
  filter(tot > 10) %>% 
  ggplot(aes(x = next_department, y = tot)) + geom_bar(stat = "identity") + coord_flip() +
  theme_classic() +
  labs(x = "Destination after CDU, where move takes place within 24 hours",
       title = "Destination after CDU showing all destinations receiving more than 10 patients"
  )

# group by csn to get total CDU time
all_CDU %>% group_by(csn) %>% summarise(tot_CDU_time = sum(duration_row)) %>% 
  ggplot(aes(x = tot_CDU_time)) + geom_boxplot() +
  theme_classic() +
  labs(x = "Total time spent in CDU (days)",
       title = "Total time spent in CDU for 2,498 visits")

#  how long do they spend in ED before moving to CDU? (Note there are 105 NA values here)
all_CDU %>% select(csn) %>% distinct() %>% 
  left_join(ED_csn_summ, by = c("csn" = "csn_old")) %>% select(csn, ED_duration_final) %>% distinct() %>% 
  ggplot(aes(x = ED_duration_final)) + geom_histogram(binwidth = 1) +
  theme_classic() +
  labs(x = "Total time spent in ED (hours)",
       title = "Total time spent in ED (excluding CDU) spent by people who then move to CDU (hours)")  

# just visits less than 1 day
all_CDU %>% group_by(csn) %>% summarise(tot_CDU_time = sum(duration_row)) %>% 
  filter(tot_CDU_time < 1) %>% 
  ggplot(aes(x = tot_CDU_time*24)) + geom_histogram(binwidth = 1) +
  theme_classic() +
  labs(x = "Total time spent in CDU (hours)",
       title = "Total time spent in CDU for visits with total duration less than one day")  +
  scale_x_continuous(breaks = seq(0, 30, 5), limits = c(-1,30))


# adding in ED time
all_CDU %>% group_by(csn) %>% 
  mutate(duration_row = difftime(discharge, admission, units = "hours")) %>% 
  summarise(tot_CDU_time = sum(duration_row)) %>% 
  filter(tot_CDU_time < 24) %>% left_join(ED_csn_summ %>% select(csn, ED_duration_final)) %>% 
  mutate(tot_ED_CDU_time = tot_CDU_time + ED_duration_final)  %>% 
  ggplot(aes(x = tot_ED_CDU_time)) + geom_histogram(binwidth = 1) +
  theme_classic() +
  labs(x = "Total time spent in ED and CDU (hours)",
       title = "Total time spent in ED and CDU for visits with total CDU duration less than one day") +
  scale_x_continuous(breaks = seq(0, 30, 5), limits = c(-1,30))

CDU_to_ED <- all_CDU %>% filter(next_department == "UCH EMERGENCY DEPT")

CDU_to_ED %>% mutate(same_csn = csn == next_csn) %>% 
  filter(time_to_next_admission < 1) %>% ggplot(aes(x = as.numeric(time_to_next_admission)*24, y = same_csn)) + geom_boxplot() +
theme_classic() +
labs(x = "Time to next ED admission (hours)",
     title = "Time from CDU discharge to next ED admission (hours)"
     )


CDU_to_ED %>% mutate(same_csn = csn == next_csn) %>% filter(time_to_next_admission < 1, !same_csn)

CDU_to_ED %>% 
 filter(time_to_next_admission < 1) %>% 
 ggplot(aes(x = as.numeric(time_to_next_admission)*24)) + geom_boxplot() +
 theme_classic() +
 labs(x = "Time from CDU discharge to next ED admission (hours)",
      title = "Time from CDU discharge to next ED admission (hours)",
      
      subtitle = "Only where time less than 24 hours"
 )

# Looking at CDU when it's included within ED
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_CDUinED_2020-11-16.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_CDUinED_2020-11-16.rda")

ED_csn_summ %>%
  ggplot(aes(x = ED_duration_final, y = adm, fill = adm, col = adm)) + geom_boxplot(alpha = .8) +
  labs(x = "Total time spent in ED (hours)",
       y = "Admitted", 
       title = "Boxplot of total time in ED when CDU is included")  +
  theme(legend.position = "None") +
  scale_x_continuous(breaks = c(seq(0,50,2)))

# same chart with it's excluded
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-11-04.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_2020-11-04.rda")

ED_csn_summ %>%
  ggplot(aes(x = ED_duration_final, y = adm, fill = adm, col = adm)) + geom_boxplot(alpha = .8) +
  labs(x = "Total time spent in ED (hours)",
       y = "Admitted", 
       title = "Boxplot of total time in ED when CDU is excluded")  +
  theme(legend.position = "None") +
  scale_x_continuous(breaks = c(seq(0,50,2)))


# Looking at SAA ----------------------------------------------------------

# how long do people spend in total? 

load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-11-04.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_2020-11-04.rda")

ED_bed_moves %>% filter(room4 == "SAA") %>% select(csn) %>% distinct() %>% 
  left_join(ED_csn_summ %>%  select(csn, adm, ED_duration_final)) %>% 
  ggplot(aes(x = ED_duration_final)) + geom_histogram(binwidth = 1) +
  theme_classic() +
  labs(x = "Total time spent in ED (hours)",
       title = "Total time spent in ED spent by people who spent some time in SAA (hours)")  


# how long do people spend in SAA? 

ED_bed_moves %>% filter(room4 == "SAA")  %>% select(csn, admission, discharge) %>% 
  mutate(duration = difftime(discharge, admission, units = "hours")) %>% 
  group_by(csn) %>% summarise(tot_duration = sum(duration)) %>% 
  ggplot(aes(x = tot_duration)) + geom_histogram(binwidth = 1) +
  theme_classic() +
  labs(x = "Total time spent in SAA (hours)",
       title = "Total time spent in SAA")  

# does it differ by whether admitted

ED_bed_moves %>% filter(room4 == "SAA") %>% select(csn) %>% distinct() %>% 
  left_join(ED_csn_summ %>%  select(csn, adm, ED_duration_final)) %>% 
  ggplot(aes(x = ED_duration_final, fill = adm)) + geom_histogram(binwidth = 1) +
  theme_classic() +
  facet_wrap(~adm) +
  labs(x = "Total time spent in ED (hours)",
       fill = "Admitted", 
       title = "Total time in ED for those who visited SAA, comparing admitted and discharged patients")  

# Looking at quick readmissions to ED -------------------------------------

quick_readmission =ED_csn_summ %>% select(csn_old, ED_discharge_dttm, adm, num_ED_rows) %>% 
  left_join(bed_moves_raw_all, by = c("csn_old" = "csn", "ED_discharge_dttm" = "discharge")) %>% 
  filter(!adm, csn_old != next_csn, num_ED_rows > 1) %>% mutate(time_to_next_admission = difftime(next_admission, ED_discharge_dttm, units = "hours")) %>% 
  filter(time_to_next_admission < 24, next_department == "UCH EMERGENCY DEPT")

quick_readmission %>% ggplot(aes(x = time_to_next_admission)) + geom_histogram(binwidth = 1)  +
  theme_classic() +
  labs(title = "Histogram of instances I'm treating as discharges that re-appear in the ED department within 24 hours",
       x = "Time from ED discharge to next ED admission (hours)"
  )

# these people have negative time to next admission 
# two rows created at about the same time when the person was admitted
b = bed_moves_raw_all %>% filter(mrn == "41200132")  %>% mutate(time_to_next_admission = difftime(next_admission, discharge, units = "hours"))
b = bed_moves_raw_all %>% filter(mrn == "21065106")  %>% mutate(time_to_next_admission = difftime(next_admission, discharge, units = "hours"))

# seems to have a couple of blank rows but could be due to merging around the time of early Sept
b = bed_moves_raw_all %>% filter(mrn == "40965281") %>% mutate(time_to_next_admission = difftime(next_admission, discharge, units = "hours"))

# this person has an extra null row between admissions
b = bed_moves_raw_all %>% filter(mrn == "21250548") %>% mutate(time_to_next_admission = difftime(next_admission, discharge, units = "hours"))

# 44 people have csn change
q0 = quick_readmission %>% filter(time_to_next_admission == 0, csn_old != next_csn) 
q0 = quick_readmission %>% filter(time_to_next_admission == 0, csn_old != next_csn, !csn_old %in% csn_changed$csn) # goes to zero when this added
# 208 were discharged, but immediately back again 
# some seem due to have next admission row with discharge dttm being null - will ignore these


b0 = quick_readmission %>% filter(time_to_next_admission == 0, csn_old != next_csn) %>% select(mrn) %>% 
  left_join(bed_moves_raw_all) %>% mutate(time_to_next_admission = difftime(next_admission, discharge, units = "hours")) %>% 
  arrange(mrn, admission) # 208 rows


q1 = quick_readmission %>% filter(time_to_next_admission > 0, time_to_next_admission < 1)



b1 = quick_readmission %>% filter(time_to_next_admission > 0, time_to_next_admission < 1) %>% select(mrn) %>% 
  left_join(bed_moves_raw_all) # 208 rows

# Create visit summary dataset  -------------------------------------------


visit_summ <- bed_moves_raw_all %>% 
  group_by(mrn, csn) %>% 
  mutate(arrival_dttm = min(admission, na.rm = TRUE)) 

# visit_summ <- visit_summ %>% 
#   mutate(discharge_dttm = max(discharge, na.rm = TRUE))

vs <- visit_summ %>% ungroup() %>% 
  select(mrn, csn, arrival_dttm, discharge_dttm) %>% distinct() %>% filter(!is.na(arrival_dttm)) %>% arrange(mrn, arrival_dttm)

vs <- vs %>% group_by(mrn) %>% 
  mutate(visit_num = 1:n())

vs <- vs %>% 
  mutate(days_since_last_visit = as.numeric(difftime(arrival_dttm, lag(arrival_dttm), units = "days")))


save(vs, file = paste0('EDcrowding/flow-mapping/data-raw/visit_summ_all_flow_and_star_',today(),'.rda'))



# Explore care site locations ---------------------------------------------

bed_moves_raw_all <- bed_moves_raw_all %>% 
  mutate(
    room1 = split_location(hl7_location, 1))

# load care site lookup
load("~/EDcrowding/flow-mapping/data-raw/care_sites_2020-11-10.rda")

care_sites <- care_sites %>% 
  mutate(
    room1 = split_location(care_site_name, 1),
    room2 = split_location(care_site_name, 2))

# explore rows that are not known care sites
all_depts <- bed_moves_raw_all %>% group_by(department) %>% summarise(tot = n())
all_depts[all_depts %in% care_sites$room1]



