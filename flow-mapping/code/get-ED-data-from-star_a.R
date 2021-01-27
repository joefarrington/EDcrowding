
# About the preprocessing steps -------------------------------------------

# retrieve data
# - deal with missing admission time
# - deal with missing discharge time
# - deal with outlier ages



# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)


# Create functions --------------------------------------------------------

rpt <- function(dataframe) {
  print(dataframe %>% select(csn) %>% n_distinct())
}


split_location <- function(hl7_location, n) {
  s <- unlist(strsplit(hl7_location, "\\^"))
  return(s[n])
}
split_location <- Vectorize(split_location)

# clean room data
# function removes bay and chair numbers

clean_room_names <- function(department, room) {
  if (department == "ED" && !is.na(room)) {
    room = gsub("UCHED ", "", room)
    room = gsub("UCH ED ", "", room)
    room = gsub("UCH ", "", room)
    room = gsub("^ED ","",room)  
    room = gsub("CHAIR [0-9]{2}", "", room)
    room = gsub("[0-9]{3}", "", room)
    room = gsub("[0-9]{2}", "", room)
    room = gsub("MAJ-CHAIR","MAJORS CH",room)
    room = gsub("MAJCH","MAJORS CH",room)
    room = gsub("SPECIALTY ASSESSMENT AREA","SAA",room)
    room = gsub("RAT-CHAIR","RAT",room)
    room = gsub("RATBED","RAT",room)
    room = gsub("SDEC","SDEC",room)
    room = gsub("  "," ",room)
    room = gsub(" $","",room)
    room = gsub("^ ","",room)  
    room = gsub("OTF POOL","OTF",room)  
    room = gsub(" ","_",room)  
  }
  else if (grepl("UCHT00CDU",department)) {
    room = "CDU"
  }
  return(room)
}
clean_room_names <- Vectorize(clean_room_names)

# function to group room names
# NB RAT COVID MAJORS could be both - need to check which to prioritise
group_room_names <- function(room) {
  room_ <- case_when(
    length(grep("UTC", room)) >0 ~ "UTC",
    length(grep("MAJ", room)) >0 ~ "MAJORS",
    length(grep("RAT", room)) >0 ~ "RAT",
    length(grep("TRIAGE", room)) >0 ~ "TRIAGE",
    length(grep("SPECIALTY ASSESSMENT AREA", room)) >0 ~ "SAA",
    length(grep("SDEC", room)) >0 ~ "SDEC",
    room %in% c( "null", "WR POOL") ~ "Waiting",
    TRUE ~ room)
  return(room_)
}

group_room_names <- Vectorize(group_room_names)


# Database connection -----------------------------------------------------


# Set up connection
ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")


# Get  data ---------------------------------------------------------

# hopital visit summary

sqlQuery <- "select m.mrn, hv.encounter as csn, hv.patient_class, hv.presentation_time, hv.admission_time,   hv.discharge_time, hv.arrival_method, hv.discharge_destination, hv.discharge_disposition
    from star_test.hospital_visit hv,
      star_test.mrn m
  where hv.mrn_id = m.mrn_id
    order by mrn, csn, admission_time
"
sqlQuery <- gsub('\n','',sqlQuery)
csn_summ <- as_tibble(dbGetQuery(ctn, sqlQuery))

rpt(csn_summ) #3,387,833


# patient class history

sqlQuery <- "select encounter as csn, patient_class, valid_from, valid_until from star_test.hospital_visit_audit"
sqlQuery <- gsub('\n','',sqlQuery)
all_patient_class <- as_tibble(dbGetQuery(ctn, sqlQuery))

#all_patient_class <- all_patient_class %>% arrange(csn, valid_from)
rpt(all_patient_class) #2,275,160 csns have class history


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

# location data

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

# patient class change history 

sqlQuery <- "select encounter as csn, hospital_visit_id, max(valid_until) as max_emerg_class 
from star_test.hospital_visit_audit
  where patient_class = 'EMERGENCY'
  group by encounter, hospital_visit_id"

sqlQuery <- gsub('\n','',sqlQuery)
patient_class <- as_tibble(dbGetQuery(ctn, sqlQuery))

print(paste0("Total number of visits in Star up to ", Sys.Date()))
print(rpt(csn_summ))


# Create subset of visits involving ED ------------------------------------------------------

# identify csns which had patient class emergency at some point

csn_summ <- csn_summ %>% left_join(patient_class) 
rpt(csn_summ %>% filter(!is.na(max_emerg_class))) # has emergency class with 'valid until'

# split location string to get department information 

bed_moves <- bed_moves %>% mutate(department = split_location(location_string, 1))

visited_ED_csn <- bed_moves %>% filter(department == "ED") %>% select(csn) %>% distinct() %>% 
  mutate(visited_ED = TRUE)

csn_summ <- csn_summ %>% left_join(visited_ED_csn) 
rpt(csn_summ %>% filter(visited_ED)) # visited ED at some point

# deal with missing admission times; infer these from location data
missing_admission_time <- visited_ED_csn %>% 
  left_join(csn_summ %>% select(csn, admission_time)) %>% filter(is.na(admission_time))

missing_admission_time <- missing_admission_time %>% select(csn) %>% 
  left_join(bed_moves) %>% group_by(csn) %>% summarise(new_admission_time = min(admission, na.rm = TRUE))

csn_summ <- csn_summ %>% left_join(missing_admission_time) %>% 
  mutate(admission_time = case_when(is.na(admission_time) & !is.na(new_admission_time) ~ new_admission_time,
                                    TRUE ~ admission_time))

# those with no emergency class AND no ED visits are the ones to remove
print("No record of 'Emergency' patient class AND did not visit ED at any point: ")
print(rpt(csn_summ %>% filter(is.na(max_emerg_class) & is.na(visited_ED))))

ED_csn_summ_raw <- csn_summ %>% 
  anti_join(csn_summ %>% filter(is.na(max_emerg_class) & is.na(visited_ED)) %>% select(csn)) %>% 
  mutate(hospital_visit_id = as.character(hospital_visit_id))

print("Leaving this number of csns:")
print(rpt(ED_csn_summ_raw))

# add max and min I and E timestamps
ED_csn_summ_raw <-  ED_csn_summ_raw %>%
  left_join(
    all_patient_class %>% filter(patient_class == "EMERGENCY") %>% group_by(csn) %>%
      summarise(max_E = max(valid_until),
                min_E = min(valid_from))
  )  %>%
  left_join(
    all_patient_class %>% filter(patient_class == "INPATIENT") %>% group_by(csn) %>%
      summarise(max_I = max(valid_until),
                min_I = min(valid_from))
  )


# Create dataset of location moves that involve ED -----------------------------

ED_bed_moves_raw <- ED_csn_summ_raw %>% select(csn) %>% left_join(bed_moves) %>% 
  mutate(hospital_visit_id = as.character(hospital_visit_id))

rpt(ED_csn_summ_raw)
rpt(ED_bed_moves_raw)


# Remove csns with NA in admission row - these are all outpatients with no bed move info
NA_in_admission_csn <- ED_bed_moves_raw %>% filter(is.na(admission)) %>% select(csn)

print("Missing admission data for location  - these are all outpatients with no bed move info")
print(rpt(NA_in_admission_csn))

ED_csn_summ_raw <- ED_csn_summ_raw %>% anti_join(NA_in_admission_csn)
ED_bed_moves_raw <- ED_bed_moves_raw  %>% anti_join(NA_in_admission_csn)

print("Has no missing admission time for locations")
print(rpt(ED_csn_summ_raw)) # has admission time
rpt(ED_bed_moves_raw) # has admission time


# split location string to get room information 

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(room = split_location(location_string, 2))

# Update department when it's null (was the case for T01ECU in December 2020)

ED_bed_moves_raw <- ED_bed_moves_raw %>% mutate(T01ECU = grepl("^T01ECU", room)) 

ED_bed_moves_raw <- ED_bed_moves_raw %>%
  mutate(department = case_when(T01ECU ~ "T01ECU",
                                TRUE ~ department))
ED_bed_moves_raw <- ED_bed_moves_raw %>% select(-T01ECU)

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(ED_row = case_when(department == "ED" | department == "UCHT00CDU" ~ 1,
                                        TRUE ~ 0))

# check no other null departments
print("Has null department - this should be zero")
print(rpt(ED_bed_moves_raw %>% filter(department == "null")))

# find csns with no ED location information

ED_csn_summ_raw <- ED_csn_summ_raw %>% 
  left_join(
    ED_bed_moves_raw %>% filter(ED_row == 1) %>% group_by(csn) %>% summarise(num_ED_rows = n())
  )

missing_ED <- ED_csn_summ_raw %>% filter(is.na(num_ED_rows)) %>% select(csn) 
missing_ED_bed_moves <- missing_ED %>% inner_join(bed_moves)

# print("Has no ED location data")
# rpt(missing_ED)

ED_csn_summ_raw <- ED_csn_summ_raw %>% anti_join(missing_ED)
ED_bed_moves_raw <- ED_bed_moves_raw  %>%  inner_join(ED_csn_summ_raw %>% select(csn))

print("Has ED location data")
print(rpt(ED_csn_summ_raw)) # has location info
print(rpt(ED_bed_moves_raw)) # has location info


# select csns that began before the beginning of epic

ED_csn_summ_raw <- ED_csn_summ_raw %>% filter(admission_time > "2019-03-31")

ED_bed_moves_raw <- ED_bed_moves_raw %>%  inner_join(ED_csn_summ_raw %>% select(csn))

print("Admitted on or after 1 April 2019")
print(rpt(ED_csn_summ_raw)) 
print(rpt(ED_bed_moves_raw)) # since beginning of epic



# Infer missing admission, presentation and discharge times where possible -------------------------

missing_adm_time <- ED_csn_summ_raw %>% filter(is.na(admission_time)) %>% select(csn)  # zero rows

# deal with missing presentation time
print("Number with missing presentation time (for information only)")
rpt(ED_csn_summ_raw %>% filter(is.na(presentation_time)))

ED_csn_summ_raw <- ED_csn_summ_raw %>% 
  mutate(presentation_time = if_else(is.na(presentation_time), admission_time, presentation_time))

# get latest ED discharge from bed moves
ED_discharge =  ED_bed_moves_raw %>% 
      filter(ED_row ==1) %>% 
      group_by(csn) %>% 
      summarise(last_ED_discharge_time = max(discharge, na.rm = TRUE))

ED_csn_summ_raw <- ED_csn_summ_raw %>% left_join(ED_discharge)

# deal with missing discharge time 
# some of these will be patients still in hospital - need to keep these but exclude others
missing_dis_time <- ED_csn_summ_raw %>% filter(is.na(discharge_time)) %>% 
  select(csn, admission_time, last_ED_discharge_time, num_ED_rows)

# calculate total number of bed moves for those with missing discharge times
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

# update discharge time in ED_csn_summ where new discharge time is available

ED_csn_summ_raw <- ED_csn_summ_raw %>% 
  mutate(discharge_time = 
           case_when(is.na(discharge_time) & !is.na(new_discharge_time) ~ new_discharge_time,
                     !is.na(discharge_time) ~ discharge_time,
                     TRUE ~ NA_POSIXct_))

rpt(ED_csn_summ_raw)
rpt(ED_bed_moves_raw)


# Remove csns with zero duration  -----------------------------------

# first find visits with only one location row where admission == discharge; 
oneED_adm_equal_dis <- ED_bed_moves_raw %>% 
  left_join(ED_csn_summ_raw %>% select(csn, num_ED_rows)) %>% 
  filter(admission == discharge, num_ED_rows == 1) %>% select(csn) %>% distinct()

# where there is only one row these csns also need to be deleted from ED_csn_summ_raw
ED_csn_summ_raw = ED_csn_summ_raw %>% 
  anti_join(oneED_adm_equal_dis)

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  anti_join(oneED_adm_equal_dis)

rm(oneED_adm_equal_dis)

# remove other rows where admission == discharge; 
csn_to_keep = ED_bed_moves_raw %>% filter(admission != discharge) %>% select(csn) %>% distinct()
other_adm_equal_dis = ED_csn_summ_raw %>% anti_join(csn_to_keep)

ED_csn_summ_raw = ED_csn_summ_raw %>% 
  inner_join(csn_to_keep)

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  inner_join(csn_to_keep)

rm(other_adm_equal_dis, csn_to_keep)

# finally, get rid of the duplicate rows for the other csns

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(rows_to_delete = case_when(!is.na(discharge) & admission == discharge ~ TRUE,
                                    TRUE ~ FALSE)) %>% 
  filter(!rows_to_delete) %>% select(-rows_to_delete)

print("After removing csns with zero duration and location data with zero duration")
print(rpt(ED_csn_summ_raw))
print(paste0("ED_bed_moves_raw: ",ED_bed_moves_raw %>% select(csn) %>% n_distinct()))


# Removes csns with mismatched timestamps ------------------------------------------


# remove csns where admission is later than discharge
admission_later_csns <- ED_bed_moves_raw %>% filter(admission > discharge) %>% select(csn) %>% distinct() # none to remove
print("Admission later than discharge csns (should be zero but check")
print(rpt(admission_later_csns))

# where discharge on bed moves is null but csn has a discharge time, update this

ED_bed_moves_raw = ED_bed_moves_raw %>% left_join(ED_csn_summ_raw %>% select(csn, discharge_time)) %>% 
  mutate(discharge = case_when(is.na(discharge) & !is.na(discharge_time) ~ discharge_time,
                               TRUE ~ discharge))

# NB - the above will create some rows with admission > discharge; most are less than one minute
rpt(ED_bed_moves_raw %>% filter(difftime(discharge, admission, units = "mins") <0)) # 297
rpt(ED_bed_moves_raw %>% filter(difftime(discharge, admission, units = "mins") < -1)) #34
rpt(ED_bed_moves_raw %>% filter(difftime(discharge, admission, units = "mins") < -10)) #28
rpt(ED_bed_moves_raw %>% filter(difftime(discharge, admission, units = "mins") < -100)) #21

# remove csns where admission to next location is later than discharge from current location by more than 1 minute
admission_later_csns <- ED_bed_moves_raw %>% filter(difftime(discharge, admission, units = "mins") < -1) %>% 
  select(csn) %>% distinct() # none to remove
print(paste0("Admission later csns: "))

ED_csn_summ_raw = ED_csn_summ_raw %>% 
  anti_join(admission_later_csns)

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  anti_join(admission_later_csns)

rm(admission_later_csns)


# find cases where there is still a mismatch of admission and discharge times

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  group_by(csn) %>% 
  mutate(next_csn = lead(csn), 
         next_admission = lead(admission),
         next_discharge = lead(discharge))  %>% 
  ungroup()

lead_row_mismatch_csn <- ED_bed_moves_raw %>% 
  filter(discharge != next_admission) %>% select(csn) %>% distinct()
rpt(lead_row_mismatch_csn) # Mismatch timestamps in moves between locations

ED_csn_summ_raw <- ED_csn_summ_raw %>% 
  anti_join(lead_row_mismatch_csn  %>% select(csn))

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  anti_join(lead_row_mismatch_csn  %>% select(csn))

print("After removing where admission to next location is later than discharge from current location by more than 1 minute")
print("Or there is another type of gap or mismatch")
print(rpt(ED_csn_summ_raw))
print(rpt(ED_bed_moves_raw))


# Simplify room names -----------------------------------------------------


# create room3 (see wiki for more information)

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(room3 = clean_room_names(department, room))

# create temp mapping to speed up creation of room4 
room_mapping <- ED_bed_moves_raw %>% filter(ED_row ==1) %>% group_by(room3) %>% summarise(tot = n()) %>% 
  mutate(room4 = group_room_names(room3)) %>% select(-tot)

# Create room4 (see wiki for more information)
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  left_join(room_mapping)


# Exclude csns with implausible length of time in ED ----------------------

# calc row durations
ED_bed_moves_raw <- ED_bed_moves_raw %>% ungroup() %>% 
  mutate(duration_row = difftime(discharge, admission, units = "hours"))


# indicate whether row is OTF location
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(OTF_row = case_when(room == "UCHED OTF POOL" ~ 1,
                             TRUE ~ 0)) 


long_ED_csn <-  ED_bed_moves_raw %>% ungroup() %>% 
  filter(ED_row == 1 & OTF_row !=1 & room3 != "TAF" & # exclude TAF in this as peopel have long durations there
           duration_row > 48) 

# long_ED_csn_24 <-  ED_bed_moves_raw %>% ungroup() %>% 
#   filter(ED_row == 1 & OTF_row !=1 & room3 != "TAF" & # exclude TAF in this as peopel have long durations there
#            duration_row > 24) 

long_ED_csn = long_ED_csn %>% left_join(ED_csn_summ_raw %>% select(csn, max_E)) %>% 
  select(csn, room3, admission, discharge, max_E, duration_row)

# long_ED_csn_24 = long_ED_csn_24 %>% left_join(ED_csn_summ_raw %>% select(csn, max_E)) %>% 
#   select(csn, room3, admission, discharge, max_E, duration_row)

# # only 5 have as max_E as late as the discharge from this location
long_ED_csn %>% filter(discharge <= max_E)
# long_ED_csn_24 %>% filter(discharge <= max_E)

ED_csn_summ_raw <- ED_csn_summ_raw %>% 
  anti_join(long_ED_csn %>% filter(discharge <= max_E) %>% select(csn))

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  anti_join(long_ED_csn %>% filter(discharge <= max_E) %>% select(csn))


print("After removing long_ED_csn:")
print(rpt(ED_csn_summ_raw))
print(rpt(ED_bed_moves_raw))



# add demographic information and delete under 18s --------------------------------------------

ED_csn_summ_raw <- ED_csn_summ_raw %>% 
  left_join(demog_raw %>% filter(!is.na(mrn))) %>% 
  # clean records with birthdate of 1900-01-01
  mutate(age = case_when(date_of_birth <= "1915-01-01" ~ NA_integer_,
                         TRUE ~ as.integer(as.period(interval(start = date_of_birth, end = admission_time))$year)))

rpt(ED_csn_summ_raw)

# delete under 18s

ED_csn_summ_raw <- ED_csn_summ_raw %>% 
  filter(age >= 18)
ED_bed_moves_raw <- ED_bed_moves_raw %>% inner_join(ED_csn_summ_raw %>% select(csn))

print("Age >= 18")
print(rpt(ED_csn_summ_raw)) 
print(rpt(ED_bed_moves_raw))


# Remove self discharges --------------------------------------------------

# filter out self-discharges - only applies to those who discharged from ED

against_med <- ED_csn_summ_raw %>% filter(discharge_disposition == "AGAINST MED", patient_class == "EMERGENCY") %>%
  mutate(against_med = TRUE)

ED_csn_summ_raw <- ED_csn_summ_raw %>% left_join(
  against_med %>% select(csn, against_med)
) %>% filter(is.na(against_med)) %>% select(-against_med)
ED_bed_moves_raw <- ED_bed_moves_raw %>% inner_join(ED_csn_summ_raw %>% select(csn))

print("Not a self-discharge")
print(rpt(ED_csn_summ_raw)) 
print(rpt(ED_bed_moves_raw))


# Remove patients who died in ED ------------------------------------------

# delete patients who died on the day of being in ED
died <- ED_csn_summ_raw %>% filter(discharge_destination == "Patient Died", patient_class == "EMERGENCY") %>%
  mutate(died_in_ED = TRUE)

ED_csn_summ_raw <- ED_csn_summ_raw %>% left_join(
  died %>% select(csn, died_in_ED)
) %>% filter(is.na(died_in_ED)) %>% select(-died_in_ED)
ED_bed_moves_raw <- ED_bed_moves_raw %>% inner_join(ED_csn_summ_raw %>% select(csn))

print("Did not die in ED")
print(rpt(ED_csn_summ_raw)) 
print(rpt(ED_bed_moves_raw))


ED_csn_summ_raw <- ED_csn_summ_raw %>%  
  mutate(epoch = case_when(presentation_time < '2020-03-31' ~ "Pre_Covid",
                           presentation_time < '2020-05-31' ~ 'Surge1',
                           presentation_time < '2020-12-24' ~ 'Post_Surge1',
                           TRUE ~ 'Surge2')) %>% 
  mutate(epoch = factor(epoch, levels = c("Pre_Covid", "Surge1", "Post_Surge1", "Surge2")))




ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  select(-discharge_time, -next_admission, -next_discharge) 



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

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_raw_",today(),".rda")
save(ED_bed_moves_raw, file = outFile)
rm(outFile)

# save csn_summ for later use

outFile = paste0("EDcrowding/flow-mapping/data-raw/csn_summ_",today(),".rda")
save(csn_summ, file = outFile)
rm(outFile)

# save ED_csn_summ for future use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_csn_summ_raw_",today(),".rda")
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

# save all_patient_class for later use
outFile = paste0("EDcrowding/flow-mapping/data-raw/all_patient_class_",today(),".rda")
save(all_patient_class, file = outFile)
rm(outFile)

# save visits for later use
outFile = paste0("EDcrowding/flow-mapping/data-raw/visits_all_",today(),".rda")
save(visits, file = outFile)


# # save missing bed moves data
# outFile = paste0("EDcrowding/flow-mapping/data-raw/missing_ED_",today(),".rda")
# save(missing_ED_bed_moves, file = outFile)
