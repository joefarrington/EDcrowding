
# About this file ---------------------------------------------------------

# Retrives all patient data of possible relevance
# Filters only those of relevance, exposing numbers included/excluded at each stage



# Set up connection -------------------------------------------------------


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



# Create function ---------------------------------------------------------

rpt <- function(dataframe) {
  print(dataframe %>% select(csn) %>% n_distinct())
}


# Retrieve patient class data ---------------------------------------------

# hopital visit summary

sqlQuery <- "select m.mrn, hv.encounter as csn, hv.patient_class, hv.presentation_time, hv.admission_time,   hv.discharge_time, hv.arrival_method, hv.discharge_destination, hv.discharge_disposition
    from star_test.hospital_visit hv,
      star_test.mrn m
  where hv.mrn_id = m.mrn_id
    order by mrn, csn, admission_time
"
sqlQuery <- gsub('\n','',sqlQuery)
csn_summ <- as_tibble(dbGetQuery(ctn, sqlQuery))

rpt(csn_summ) #3,372,548


# patient class history

sqlQuery <- "select encounter as csn, patient_class, valid_from, valid_until from star_test.hospital_visit_audit"
sqlQuery <- gsub('\n','',sqlQuery)
all_patient_class <- as_tibble(dbGetQuery(ctn, sqlQuery))

all_patient_class <- all_patient_class %>% arrange(csn, valid_from)
rpt(all_patient_class) #2,263,677 csns have class history

csn_summ <-  csn_summ %>% 
  left_join(
  all_patient_class %>% filter(patient_class == "EMERGENCY") %>% group_by(csn) %>% 
    summarise(max_emerg_class = max(valid_until),
              min_emerg_class = min(valid_from))
)  %>% 
  left_join(
    all_patient_class %>% filter(patient_class == "INPATIENT") %>% group_by(csn) %>% 
      summarise(max_inpatient_class = max(valid_until),
                min_inpatient_class = min(valid_from))
  )


rpt(csn_summ) #3,372,548


# Checking integrity of patient class -------------------------------------


csn_summ %>% filter(max_emerg_class < min_emerg_class) # 43 
csn_summ %>% filter(max_inpatient_class < min_inpatient_class) # 83

csn_summ %>% filter(min_inpatient_class < min_emerg_class) # 120
csn_summ <- csn_summ %>% 
  mutate(inpatient_before_ED = case_when(min_inpatient_class < min_emerg_class ~ TRUE))
b = all_bed_moves %>% inner_join(csn_summ %>% filter(inpatient_before_ED))
# however, some of these are initially inpatients for tiny lengths of time
# and not in in patient location - may need to include them 


csn_summ %>% filter(min_inpatient_class < max_emerg_class) # 351
csn_summ <- csn_summ %>% 
  mutate(inpatient_before_leaving_ED = case_when(min_inpatient_class < max_emerg_class ~ TRUE))
b1 = all_bed_moves %>% inner_join(csn_summ %>% filter(inpatient_before_leaving_ED))
# some of these have overlapping rows in ED  and should probably be deleted
# others have min inpatient class set while in OTF

csn_summ %>% 
  left_join(
    all_patient_class %>% filter(!is.na(patient_class)) %>% 
      filter(!patient_class %in% c("EMERGENCY", "INPATIENT")) %>% select(csn) %>% distinct() %>% 
      mutate(has_other_class = TRUE)
  ) %>% filter(has_other_class, patient_class %in% c("EMERGENCY")) # only 1 patient






# Get location data -------------------------------------------------------



# all bed moves
sqlQuery <- "select m.mrn, hv.encounter as csn, lv.hospital_visit_id, l.location_string, 
lv.admission_time as admission, lv.discharge_time as discharge 
from star_test.hospital_visit hv, 
star_test.mrn m,    
star_test.location_visit lv,
star_test.location l 
where hv.mrn_id = m.mrn_id 
and lv.hospital_visit_id = hv.hospital_visit_id 
and l.location_id = lv.location_id 
order by mrn, csn, admission"

sqlQuery <- gsub('\n','',sqlQuery)
all_bed_moves <- as_tibble(dbGetQuery(ctn, sqlQuery))

all_bed_moves <- all_bed_moves %>% arrange(csn, admission)

rpt(all_bed_moves) # 3,219,996




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






