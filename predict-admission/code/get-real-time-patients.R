# About the script  -------------------------------------------

# Gets all patients currently in ED



# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)


# Create functions --------------------------------------------------------

rpt <- function(dataframe) {
  print(dataframe %>% select(csn) %>% n_distinct())
}




split_location <- function(hl7_location, n) {
  s <- unlist(strsplit(hl7_location, "\\^"))
  return(s[n])
}
split_location <- Vectorize(split_location)

clean_room_names <- function(department, room) {
  if (department == "ED" && !is.na(room)) {
    room = gsub("UCHED ", "", room)
    room = gsub("UCH ED ", "", room)
    room = gsub("UCH ", "", room)
    room = gsub("^ED ","",room)  
    room = gsub("CHAIR [0-9]{2}", "", room)
    room = gsub(" CH ED", "", room)
    room = gsub(" CHAIR", "", room)
    room = gsub("[0-9]{3}", "", room)
    room = gsub("[0-9]{2}", "", room)
    room = gsub("MAJ-CHAIR","MAJORS CH",room)
    room = gsub("MAJCH","MAJORS CH",room)
    room = gsub("SPECIALTY ASSESSMENT AREA","SAA",room)
    room = gsub("RAT-CHAIR","RAT",room)
    room = gsub("RATBED","RAT",room)
    #    room = gsub("^UTC [A-z]+ ROOM","UTC",room)
    room = gsub("  "," ",room)
    room = gsub(" $","",room)
    room = gsub("^ ","",room) 
    room = gsub("SDEC CH","SDEC",room)
  }
  else if (grepl("UCHT00CDU",department)) {
    room = "CDU"
  }
  return(room)
}
clean_room_names <- Vectorize(clean_room_names)



# Set up connection
ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")


# Get  data ---------------------------------------------------------

# all patients with emergency class now

sqlQuery <- "select m.mrn, hv.encounter as csn, hv.hospital_visit_id, hv.patient_class, hv.presentation_time, hv.admission_time,  hv.arrival_method
    from star_test.hospital_visit hv,
      star_test.mrn m
  where hv.mrn_id = m.mrn_id
  and patient_class = 'EMERGENCY'
  and hv.discharge_time is NULL
  and hv.admission_time is not NULL
  and hv.admission_time > NOW() - INTERVAL '1 DAY' 
    order by admission_time desc
"
sqlQuery <- gsub('\n','',sqlQuery)
class_e_now <- as_tibble(dbGetQuery(ctn, sqlQuery))
rpt(class_e_now)
class_e_now <- class_e_now %>% mutate(time_since_arrival = difftime(Sys.time(), presentation_time, units = "mins"))
# note - this generated on additional patient who was not in ED

# all patients in ED now

sqlQuery <- "select m.mrn, hv.encounter as csn, hv.hospital_visit_id, hv.patient_class, hv.presentation_time, hv.admission_time,  hv.arrival_method, lv.admission_time as admission, lv.discharge_time as discharge, l.location_string
    from star_test.hospital_visit hv,
      star_test.mrn m,    
      star_test.location_visit lv,
      star_test.location l 
  where hv.mrn_id = m.mrn_id
  and hv.discharge_time is NULL
  and hv.admission_time is not NULL
  and lv.hospital_visit_id = hv.hospital_visit_id 
  and l.location_id = lv.location_id 
  and hv.admission_time > NOW() - INTERVAL '1 DAY' 
  and left(l.location_string, 3) ='ED^'
  and lv.discharge_time is null
    order by csn, admission_time desc;"
sqlQuery <- gsub('\n','',sqlQuery)
in_ED_now <- as_tibble(dbGetQuery(ctn, sqlQuery))
rpt(in_ED_now)
in_ED_now %>% filter(!csn %in% class_e_now$csn)
in_ED_now <- in_ED_now %>% mutate(time_since_arrival = difftime(Sys.time(), presentation_time, units = "mins"))



sqlQuery <- "   select m.mrn, hv.encounter as csn, hv.hospital_visit_id, hv.patient_class, hv.presentation_time, hv.admission_time,  hv.arrival_method, lv.admission_time as admission, lv.discharge_time as discharge, l.location_string    
    from star_test.hospital_visit hv,
      star_test.mrn m,    
      star_test.location_visit lv,
      star_test.location l
      where lv.hospital_visit_id in 
            (
            select hv.hospital_visit_id
            from star_test.hospital_visit hv,
              star_test.location_visit lv,
              star_test.location l 
          where hv.discharge_time is NULL
          and hv.admission_time is not NULL
          and lv.hospital_visit_id = hv.hospital_visit_id 
          and l.location_id = lv.location_id 
          and hv.admission_time > NOW() - INTERVAL '1 DAY' 
          and left(l.location_string, 3) ='ED^'
          and lv.discharge_time is null
            )
      and l.location_id = lv.location_id 
      and lv.hospital_visit_id = hv.hospital_visit_id 
      and hv.mrn_id = m.mrn_id;
"
sqlQuery <- gsub('\n','',sqlQuery)
moves_now <- as_tibble(dbGetQuery(ctn, sqlQuery))
moves_now %>% filter(!csn %in% class_e_now$csn)



# Chart current location ---------------------------------------------------

moves_now <- moves_now %>% 
  mutate(room = split_location(location_string, 2)) 

moves_now <- moves_now %>% 
  mutate(department = split_location(location_string, 1)) 


moves_now <- moves_now %>% 
  mutate(room4 = clean_room_names(department, room))

moves_now %>% filter(is.na(discharge)) %>% group_by(room4) %>% summarise(n()) 

timeslices = c(15, 60, 120, 180, 240)
moves_now %>% filter(is.na(discharge)) %>% left_join(in_ED_now %>% select(csn, time_since_arrival)) %>% 
  ggplot(aes(x = as.numeric(time_since_arrival), y = room4)) + geom_point() +
  labs(y = "Current location", x = "Minutes since arrival") +
  geom_vline(xintercept = timeslices) +
  scale_x_continuous(breaks = c(timeslices, seq(240, 24*60, 120))) + theme(panel.grid.minor = element_blank())
  
  

# Save data ---------------------------------------------------------------

time_now = gsub(" ", "-", Sys.time())

outFile = paste0("EDcrowding/predict-admission/data-raw/real-time/moves_now_",time_now,".rda")
save(moves_now, file = outFile)
rm(outFile)

outFile = paste0("EDcrowding/predict-admission/data-raw/real-time/in_ED_now_",time_now,".rda")
save(in_ED_now, file = outFile)
rm(outFile)

outFile = paste0("EDcrowding/predict-admission/data-raw/real-time/class_e_now_",time_now,".rda")
save(class_e_now, file = outFile)
rm(outFile)