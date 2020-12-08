# Load libraries
# ==============
library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)

# Set up connection
ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")





split_location <- function(hl7_location, n) {
  s <- unlist(strsplit(hl7_location, "\\^"))
  return(s[n])
}
split_location <- Vectorize(split_location)



load("~/EDcrowding/flow-mapping/data-raw/missing_ED_2020-12-08.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-12-08.rda")
load("~/EDcrowding/flow-mapping/data-raw/OTF_arrival_csn_all_2020-12-08.rda")
load("~/EDcrowding/flow-mapping/data-raw/long_ED_csn_all_2020-12-08.rda")
load("~/EDcrowding/flow-mapping/data-raw/long_ED_csn_24_all_2020-12-08.rda")

load("~/EDcrowding/flow-mapping/data-raw/csn_summ_2020-12-08.rda")
load("~/EDcrowding/flow-mapping/data-raw/patient_class_2020-12-08.rda")


# patient class history

# patient class change information 

sqlQuery <- "select encounter as csn, patient_class, valid_from, valid_until from star_test.hospital_visit_audit"

sqlQuery <- gsub('\n','',sqlQuery)
all_patient_class <- as_tibble(dbGetQuery(ctn, sqlQuery))




# Missing ED csns ---------------------------------------------------------

missing_ED = missing_ED_bed_moves %>% select(csn) %>% distinct()


missing_ED <- missing_ED %>% inner_join(csn_summ)

all_patient_class_ =  all_patient_class %>% inner_join(missing_ED_bed_moves %>% select(csn) %>% distinct()) %>% 
  filter(patient_class %in% c("EMERGENCY")) %>% 
  group_by(csn) %>% summarise(earliest_emerg_class = min(valid_from))

missing_ED <- missing_ED %>% 
  left_join(
    all_patient_class_ 
  )

missing_ED <- missing_ED %>% 
  left_join(patient_class %>% select(csn, max_emerg_class))



# some cans seems to have different mrns ??  so need to exclude mrn in this join


missing_ED_bed_moves <- missing_ED_bed_moves %>% select(-hospital_visit_id) %>% 
  mutate(department = split_location(location_string, 1),
         room = split_location(location_string, 2))

missing_ED_bed_moves <- missing_ED_bed_moves %>% inner_join(missing_ED)
missing_ED_bed_moves %>% select(csn) %>% n_distinct()

missing_ED_bed_moves <- missing_ED_bed_moves  %>% 
  select(-arrival_method, -discharge_destination, -discharge_disposition, -discharge_time) 

missing_ED_bed_moves <- missing_ED_bed_moves %>% 
  mutate(in_ED_class = if_else(admission < max_emerg_class, TRUE, FALSE))

missing_ED_bed_moves %>% filter(in_ED_class) %>% 
  group_by(department) %>% summarise(n())

# find first row

arrival_row = missing_ED_bed_moves %>% group_by(csn) %>% summarise(min_admission = min(admission))

missing_ED_bed_moves <- missing_ED_bed_moves %>% 
  left_join(arrival_row)

missing_ED_bed_moves <- missing_ED_bed_moves %>% 
  mutate(arrival_row = if_else(admission == min_admission, TRUE, FALSE))

missing_ED_bed_moves %>% filter(arrival_row) %>% 
  group_by(department) %>% summarise(tot = n()) %>% arrange(desc(tot))

m =missing_ED_bed_moves %>% filter(arrival_row) %>% 
  mutate(min_admission_before_admission_time = if_else(min_admission < admission_time, TRUE, FALSE),
         min_admission_at_admission_time = if_else(min_admission == admission_time, TRUE, FALSE), 
         min_admission_before_pres_time = if_else(min_admission < presentation_time, TRUE, FALSE), 
         has_earliest_emerg_class_before_admission_time = if_else(earliest_emerg_class < admission_time, TRUE, FALSE), 
         has_max_emerg_class_before_admission_time = if_else(max_emerg_class < admission_time, TRUE, FALSE), 
         has_earliest_emerg_class_before_min_admission = if_else(earliest_emerg_class < min_admission, TRUE, FALSE), 
    admission_time_to_earliest_emerg_classs = difftime(earliest_emerg_class , admission_time, units = "mins"),
         admission_time_to_max_emerg_class = difftime(max_emerg_class, admission_time, units = "mins"),
         min_admission_to_earliest_emerg_class = difftime(earliest_emerg_class, min_admission, units = "mins"),
        min_admission_to_max_emerg_class = difftime(max_emerg_class, min_admission, units = "mins"))

m %>% group_by(in_ED_class) %>% summarise(n())
m %>% group_by(min_admission_before_admission_time) %>% summarise(n())
m %>% group_by(min_admission_at_admission_time) %>% summarise(n())

m %>% group_by(min_admission_before_pres_time) %>% summarise(n())
m %>% group_by(has_earliest_emerg_class_before_admission_time) %>% summarise(n())
m %>% group_by(has_max_emerg_class_before_admission_time) %>% summarise(n())
m %>% group_by(has_earliest_emerg_class_before_min_admission) %>% summarise(n())

# to create table in my google sheet
m1 =m %>% group_by(department, patient_class) %>% summarise(tot = n()) %>% 
  pivot_wider(names_from = patient_class, values_from = tot, values_fill = 0) %>% arrange(desc(INPATIENT))

# get a sense of the order of each timestamp

m_long = m %>% select(csn, min_admission, admission_time, earliest_emerg_class, max_emerg_class) %>% 
  pivot_longer(min_admission:max_emerg_class, names_to = "timestamp", values_to = "time") %>% 
  arrange(csn, time)



