
# About the preprocessing steps -------------------------------------------

# retrieve data
# - deal with missing admission time
# - deal with missing discharge time
# - deal with outlier ages
# - delete patients who died in ED or on the day of being in ED as these were not clinically admitted or discharged


# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)


# Create functions --------------------------------------------------------

split_location <- function(hl7_location, n) {
  s <- unlist(strsplit(hl7_location, "\\^"))
  return(s[n])
}
split_location <- Vectorize(split_location)


# Set up connection
ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")


# Get data ---------------------------------------------------------

# hopital visit summary

sqlQuery <- "select m.mrn, hv.encounter as csn, hv.patient_class, hv.presentation_time, hv.admission_time,   hv.discharge_time, hv.arrival_method, hv.discharge_destination, hv.discharge_disposition
    from star_test.hospital_visit hv,
      star_test.mrn m
  where hv.mrn_id = m.mrn_id
    order by mrn, csn, admission_time
"
sqlQuery <- gsub('\n','',sqlQuery)
csn_summ <- as_tibble(dbGetQuery(ctn, sqlQuery))

# demographics

sqlQuery <- "select mrn, 
 date_of_birth, 
 date_of_death, 
 alive, 
 sex
 from star_test.core_demographic d,
  star_test.mrn m
  where m.mrn_id = d.mrn_id
"

sqlQuery <- gsub('\n','',sqlQuery)
demog_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))

# bed moves

sqlQuery <- "select m.mrn, hv.encounter as csn, lv.hospital_visit_id, l.location_string, 
lv.admission_time as admission, lv.discharge_time as discharge 
from star_test.hospital_visit hv, 
star_test.mrn m,    
star_test.location_visit lv,
star_test.location l 
where hv.mrn_id = m.mrn_id 
and hv.patient_class not in ('OUTPATIENT', 'NEW_BORN', 'DAY_CASE', 'SURGICAL_ADMISSION') 
and lv.hospital_visit_id = hv.hospital_visit_id 
and l.location_id = lv.location_id 
order by mrn, csn, admission"

sqlQuery <- gsub('\n','',sqlQuery)
bed_moves <- as_tibble(dbGetQuery(ctn, sqlQuery))

# patient class change information 

sqlQuery <- "select encounter as csn, hospital_visit_id, max(valid_until) as max_emerg_class from star_test.hospital_visit_audit
  where patient_class = 'EMERGENCY'
  group by encounter, hospital_visit_id"

sqlQuery <- gsub('\n','',sqlQuery)
patient_class <- as_tibble(dbGetQuery(ctn, sqlQuery))




# Find visits involving ED ------------------------------------------------------

csn_summ %>% select(csn) %>% n_distinct() # total csns all classees


# identify csns which had patient class emergency at some point
ED_csn_summ_raw <- csn_summ %>% left_join(patient_class) %>% filter(!is.na(max_emerg_class)) %>% 
  mutate(hospital_visit_id = as.character(hospital_visit_id))

ED_bed_moves_raw <- ED_csn_summ_raw %>% select(csn) %>% left_join(bed_moves) %>% 
  mutate(hospital_visit_id = as.character(hospital_visit_id))

ED_csn_summ_raw %>% select(csn) %>% n_distinct()
ED_bed_moves_raw %>% select(csn) %>% n_distinct()

# find csns with NA in admission row - these are all outpatients with no bed move info
NA_in_admission_csn <- ED_bed_moves_raw %>% filter(is.na(admission)) %>% select(csn)

ED_csn_summ_raw <- ED_csn_summ_raw %>% anti_join(NA_in_admission_csn)
ED_bed_moves_raw <- ED_bed_moves_raw  %>% anti_join(NA_in_admission_csn)

ED_csn_summ_raw %>% select(csn) %>% n_distinct()
ED_bed_moves_raw %>% select(csn) %>% n_distinct()


# add room and dept information to ED

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(department = split_location(location_string, 1),
         room = split_location(location_string, 2))

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(ED_row = case_when(department == "ED" | department == "UCHT00CDU" ~ 1,
                                        TRUE ~ 0))

# find csns with no ED location information

ED_csn_summ_raw <- ED_csn_summ_raw %>% 
  left_join(
    ED_bed_moves_raw %>% filter(ED_row == 1) %>% group_by(csn) %>% summarise(num_ED_rows = n())
  )

missing_ED <- ED_csn_summ_raw %>% filter(is.na(num_ED_rows)) %>% select(csn) 
missing_ED_bed_moves <- missing_ED %>% inner_join(bed_moves)

ED_csn_summ_raw <- ED_csn_summ_raw %>% anti_join(missing_ED)
ED_bed_moves_raw <- ED_bed_moves_raw  %>%  inner_join(ED_csn_summ_raw %>% select(csn))

ED_csn_summ_raw %>% select(csn) %>% n_distinct()
ED_bed_moves_raw %>% select(csn) %>% n_distinct()


# select csns that began before the beginning of epic

ED_csn_summ_raw <- ED_csn_summ_raw %>% filter(admission_time > "2019-03-31")

ED_bed_moves_raw <- ED_bed_moves_raw %>%  inner_join(ED_csn_summ_raw %>% select(csn))


ED_csn_summ_raw %>% select(csn) %>% n_distinct()
ED_bed_moves_raw %>% select(csn) %>% n_distinct()

# add demographic information  --------------------------------------------


ED_csn_summ_raw <- ED_csn_summ_raw %>% 
  left_join(demog_raw %>% filter(!is.na(mrn))) %>% 
  # clean records with birthdate of 1900-01-01
  mutate(age = case_when(date_of_birth <= "1915-01-01" ~ NA_integer_,
                         TRUE ~ as.integer(as.period(interval(start = date_of_birth, end = admission_time))$year)))

ED_csn_summ_raw %>% select(csn) %>% n_distinct()

# delete under 18s

ED_csn_summ_raw <- ED_csn_summ_raw %>% 
  filter(age >= 18)
ED_bed_moves_raw <- ED_bed_moves_raw %>% inner_join(ED_csn_summ_raw %>% select(csn))

ED_csn_summ_raw %>% select(csn) %>% n_distinct()
ED_bed_moves_raw %>% select(csn) %>% n_distinct()


# Deal with missing admission presentation and discharge times -------------------------



# deal with missing admission time in ED_csn_summ_raw

missing_adm_time <- ED_csn_summ_raw %>% filter(is.na(admission_time)) %>% select(csn)  # zero rows
# missing_adm_time <- missing_adm_time %>% 
#   left_join(
#     ED_bed_moves_raw %>% filter(csn %in% missing_adm_time$csn) %>% 
#       select(csn, admission) %>% group_by(csn) %>% summarise(admission_time_ = min(admission, na.rm = TRUE))
#   )
# 
# ED_csn_summ_raw <- ED_csn_summ_raw %>% 
#   left_join(missing_adm_time) %>% 
#   mutate(admission_time = case_when(is.na(admission_time) ~ admission_time_,
#                                     TRUE ~ admission_time)) %>% select(-admission_time_)
# 
# ED_csn_summ_raw %>% select(csn) %>% n_distinct()
# ED_bed_moves_raw %>% select(csn) %>% n_distinct()


# deal with missing presentation time
ED_csn_summ_raw %>% filter(is.na(presentation_time)) %>% select(csn) %>% n_distinct() # 213 missing

ED_csn_summ_raw <- ED_csn_summ_raw %>% 
  mutate(presentation_time = if_else(is.na(presentation_time), admission_time, presentation_time))

# get latest ED discharge from bed moves
ED_discharge =  ED_bed_moves_raw %>% 
      filter(ED_row ==1) %>% 
      group_by(csn) %>% 
      summarise(last_ED_discharge_time = max(discharge, na.rm = TRUE))

ED_csn_summ_raw <- ED_csn_summ_raw %>% left_join(ED_discharge)

# # filter out the mistmatches, allowing a 5 min difference bewteen the timestamps
# ED_discharge_mismatch = ED_discharge %>% 
#   select(csn, max_emerg_class, last_ED_discharge_time) %>% 
#   mutate(greater = max_emerg_class > last_ED_discharge_time) %>% 
#   filter(floor_date(max_emerg_class,"15 mins") != floor_date(last_ED_discharge_time, "15 mins"))
# 
# # this number have mismatch because last row is OTF and class has been changed before that row - these are false admissions 
# ED_discharge_mismatch %>% filter(!greater) %>% 
#   left_join(ED_bed_moves_raw %>% select(csn, location_string, discharge), 
#             by = c("csn", "last_ED_discharge_time" = "discharge")) %>% filter(location_string == "ED^UCHED OTF POOL^OTF") %>% select(csn) %>% n_distinct()
# 
# # they are also very confused records for the most part - see examples here
# b  = ED_discharge_mismatch %>% filter(!greater) %>% 
#   left_join(ED_bed_moves_raw %>% select(csn, location_string, discharge), 
#             by = c("csn", "last_ED_discharge_time" = "discharge")) %>% filter(location_string == "ED^UCHED OTF POOL^OTF") %>% 
#   select(csn, max_emerg_class, last_ED_discharge_time) %>% distinct() %>% 
#   inner_join(ED_bed_moves_raw) %>% arrange(csn, admission)
#                                                                   
# # find the last location - not there are 62 double counted because bed_move rows have same discharge time
# ED_discharge_mismatch = ED_discharge_mismatch %>% 
#   left_join(ED_bed_moves_raw %>% select(csn, location_string, discharge), 
#                                                             by = c("csn", "last_ED_discharge_time" = "discharge"))
# 
# # this number of csns have a mismatch because their last discharge row is OTF
# ED_discharge_mismatch %>% filter(location_string == "ED^UCHED OTF POOL^OTF") %>% select(csn) %>% n_distinct()




# deal with missing discharge time - some of these will be patients still in
missing_dis_time <- ED_csn_summ_raw %>% filter(is.na(discharge_time)) %>% 
  select(csn, admission_time, last_ED_discharge_time, num_ED_rows)

# calculate total number of bed moves
missing_dis_time <- missing_dis_time %>% 
  left_join(
    ED_bed_moves_raw %>% inner_join(missing_dis_time %>% select(csn)) %>% 
      group_by(csn) %>% summarise(num_rows = n())
  )


# where the number of ED rows = total number of bed moves rows, AND the patient presented more 
# than 48 hours ago, we can assume the person only visited ED

# so calculate a new discharge time for this person
missing_dis_time <- missing_dis_time %>% 
  mutate(new_discharge_time = case_when(num_ED_rows == num_rows & 
                                             admission_time < Sys.Date() - 2 ~ last_ED_discharge_time))

# and update ED_csn_summ_raw
ED_csn_summ_raw <- ED_csn_summ_raw %>% 
  left_join(missing_dis_time %>% select(csn, new_discharge_time))

# 
# # delete any ED rows without any kind of discharge time (ie only one row per patient) where arrival was > 48 hours ago
# missing_dis_time <- missing_dis_time %>% 
#   mutate(delete = case_when(admission_time < Sys.Date() - 2 & num_ED_rows == 1 ~ TRUE))
# 
# sum(missing_dis_time$delete, na.rm = TRUE)
# 
# ED_csn_summ_raw <- ED_csn_summ_raw %>% left_join(
#   missing_dis_time %>% select(csn, delete) 
# ) %>% filter(is.na(delete)) %>% select(-delete)
# 
# ED_bed_moves_raw <- ED_bed_moves_raw  %>% left_join(
#   missing_dis_time %>% select(csn, delete) 
# ) %>% filter(is.na(delete)) %>% select(-delete)
# 

ED_csn_summ_raw %>% select(csn) %>% n_distinct()
ED_bed_moves_raw %>% select(csn) %>% n_distinct()



# Create visit history ----------------------------------------------------

visit_summ <- csn_summ %>% filter(patient_class %in% c("EMERGENCY", "INPATIENT")) %>% 
  group_by(mrn, patient_class) %>% 
  summarise(num_visits = n()) 

had_emergency_visit <- csn_summ %>% left_join(patient_class) %>% filter(!is.na(max_emerg_class)) 

visits <- csn_summ %>% select(mrn, csn, patient_class, admission_time, discharge_time) %>%  
  filter(patient_class %in% c("EMERGENCY", "INPATIENT"))  %>% 
  left_join(patient_class) %>% 
  mutate(type = case_when(is.na(max_emerg_class) & patient_class == "INPATIENT" ~ "planned_inpatient",
                          !is.na(max_emerg_class) & patient_class == "INPATIENT" ~ "emergency_inpatient",
                          patient_class == "EMERGENCY" ~ "emergency_discharge"))

visits <- visits %>% 
  group_by(mrn) %>% 
  mutate(days_since_last_visit = as.numeric(difftime(admission_time, lag(discharge_time), units = "days")))

# this tots up all visits including current one
visits <- visits %>% 
  group_by(mrn) %>% 
  mutate(num_adm_after_ED = cumsum(type == "emergency_inpatient"),
         num_ED_visits = cumsum(type %in% c("emergency_inpatient", "emergency_discharge")))

# this elimintes the current one to create sum of number of ED visits, and number adm
visits <- visits %>% 
  ungroup() %>% 
  mutate(num_prior_adm_after_ED = case_when(type == "emergency_inpatient" ~ num_adm_after_ED -1,
                                TRUE ~ num_adm_after_ED - 0),
         num_prior_ED_visits = case_when(type %in% c("emergency_inpatient", "emergency_discharge") ~ num_ED_visits -1,
                                         TRUE ~ num_ED_visits -0))  %>% 
  select(-num_adm_after_ED, -num_ED_visits)

# this generates the proportion of prior hospitalisations from ED

visits <- visits %>% 
  mutate(prop_adm_from_ED = case_when(num_prior_ED_visits != 0 ~ num_prior_adm_after_ED/ num_prior_ED_visits,
                                      TRUE ~ NA_real_))



# Save data ---------------------------------------------------------------

# save bed_moves for later use

outFile = paste0("EDcrowding/flow-mapping/data-raw/bed_moves_",today(),".rda")
save(bed_moves, file = outFile)
rm(outFile)

# save ED_bed_moves for later use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_raw_all_",today(),".rda")
save(ED_bed_moves_raw, file = outFile)
rm(outFile)

# save csn_summ for later use

outFile = paste0("EDcrowding/flow-mapping/data-raw/csn_summ_",today(),".rda")
save(csn_summ, file = outFile)
rm(outFile)

# save ED_csn_summ for future use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_csn_summ_raw_all_",today(),".rda")
save(ED_csn_summ_raw, file = outFile)
rm(outFile)

# save demog for later use
outFile = paste0("EDcrowding/flow-mapping/data-raw/demog_all_",today(),".rda")
save(demog_raw, file = outFile)
rm(outFile)

# save patient class for later use
outFile = paste0("EDcrowding/flow-mapping/data-raw/patient_class_",today(),".rda")
save(patient_class, file = outFile)
rm(outFile)

# save visits for later use
outFile = paste0("EDcrowding/flow-mapping/data-raw/visits_all_",today(),".rda")
save(visits, file = outFile)


# save missing bed moves data
outFile = paste0("EDcrowding/flow-mapping/data-raw/missing_ED_",today(),".rda")
save(missing_ED_bed_moves, file = outFile)
