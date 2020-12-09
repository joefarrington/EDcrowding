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


# Set up connection
ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")



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

# 
# get_node <- function(dept, room) {
#   if (dept == "ED") {
#     node <- room
#   }
#   else {
#     node <- dept
#   }
# }



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
    #    room = gsub("^UTC [A-z]+ ROOM","UTC",room)
    room = gsub("  "," ",room)
    room = gsub(" $","",room)
    room = gsub("^ ","",room)  
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

get_edgelist <- function(sites) {
  
  edgelist <- tribble(
    ~csn,
    ~from,
    ~to,
    ~dttm)
  
  from_node = as.character(sites$location[1])
  
  for (j in 1:nrow(sites)) {
    if (j != nrow(sites)) {
      
      to_node <- as.character(sites$location[j+1])
      
      # new code to remove unwanted rows flipping between Arrived, Waiting and back to Arrived, added 24/11
      if(to_node == "Arrived" && j > 1) {
        to_node <- from_node
      }
      
      # in any of the following cases, ignore the to node and move to next row
      if (!(to_node %in% c("OTF", "DIAGNOSTICS", "WAITING ROOM")) && !is.na(to_node)) {
        if (from_node != to_node) {
          
          edgelist <- edgelist %>% add_row(tibble_row(
            csn = sites$csn[j],
            from = from_node,
            to = to_node,
            dttm = sites$discharge[j]
          ))
        }
        from_node <- to_node
      }
    }
    else {
      # if (!(to_node %in% c("OTF", "DIAGNOSTICS", "WAITING ROOM")) && !is.na(to_node)) {
      # 
      #   edgelist <- edgelist %>% add_row(tibble_row(
      #     csn = sites$csn[j],
      #     from = to_node,
      #     to = "Discharged",
      #     dttm = sites$discharge_dttm[j]
      #   ))
      # }
      # else {
      
      # added new code here - need to check it works
      
      edgelist <- edgelist %>% add_row(tibble_row(
        csn = sites$csn[j],
        from = from_node,
        to = sites$patient_class[j],
        dttm = sites$discharge[j]
      ))
      #      }
    }
  }
  return(edgelist)
}


# for  group of encounters, this function generates an edge list of 
# all moves between locations, with a timestamp for each move
# locations are provided to it in a tibble called locations
# locations needs 5 attributes:  csn, location, time of admission to location, 
# time of discharge from location


get_care_site_flows <- function(locations) {
  
  # filter only those locations with more than one room move
  locations_gt1 <- locations %>% 
    group_by(csn) %>% 
    summarise("num_loc_changes" = n_distinct(location)) %>% 
    filter(num_loc_changes > 1)
  
  # then create edge list for all changes in location
  
  edgedf <- tribble(
    ~csn,
    ~from,
    ~to,
    ~dttm)
  
  for (i in (1:nrow(locations_gt1))) {
    
    if (i%%100 == 0) {
      print(paste("Processed",i,"locations"))
    }
    visit_csn = locations_gt1$csn[i]
    num_locations = locations_gt1$num_loc_changes[i]
    
    #get all visit detail records for this visit occurence
    sites <- locations %>% filter(csn == visit_csn) %>% arrange(admission)
    
    # create edgelist if the visit involved change of site
    edgelist <- get_edgelist(sites)
    if (nrow(edgelist) > 0) {
      edgedf <- edgedf %>% add_row(edgelist)
    }
  }
  return(edgedf)
}


# load data
# ==========

# bed moves - get all data including outpatient

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

# # save bed_moves for later use - couldn't do this - took too long
# 
# outFile = paste0("EDcrowding/flow-mapping/data-raw/all_bed_moves_",today(),".rda")
# save(all_bed_moves, file = outFile)
# rm(outFile)


load("~/EDcrowding/flow-mapping/data-raw/bed_moves_2020-12-08.rda")


# process data ------------------------------------------------------------



bed_moves <- bed_moves %>% 
  mutate(department = split_location(location_string, 1))

bed_moves <- bed_moves %>% 
  mutate(room = split_location(location_string, 2))


bed_moves <- bed_moves %>% 
  mutate(CDU = if_else(department == "UCHT00CDU" , TRUE, FALSE),
         EDU = if_else(department %in% c("AECU", "T01ECU") , TRUE, FALSE)) # apparently there has been a name change since Dec?? 

bed_moves <- bed_moves %>% 
  mutate(SAA = if_else(grepl("SAA|SPECIALTY ASSESSMENT AREA", room), TRUE, FALSE),
         SDEC = if_else(grepl("SDEC", room), TRUE, FALSE))

bed_moves %>% select(csn) %>% n_distinct()

bm_summ = bed_moves %>% group_by(csn) %>% 
  summarise(in_CDU = sum(CDU) >0 ,
            in_EDU = sum(EDU) > 0,
            in_SAA = sum(SAA) > 0,
            in_SDEC = sum(SDEC) > 0)

bm_summ %>% select(csn) %>% n_distinct()

load("~/EDcrowding/flow-mapping/data-raw/csn_summ_2020-12-08.rda")
bm_summ = bm_summ %>% left_join(csn_summ %>% 
                                  mutate(patient_class = case_when(patient_class == "EMERGENCY" ~ "EMERGENCY_class",
                                                                   patient_class == "INPATIENT" ~ "INPATIENT_class",
                                                                   TRUE ~ patient_class)))


# Most of the EDU cases are not in my subset of ED patients because they don't have patient class of emergency or inpatient

all_bed_moves <- all_bed_moves %>% 
  mutate(department = split_location(location_string, 1))

all_bm_summ = all_bed_moves %>% filter(department %in% c("AECU", "T01ECU")) %>% select(csn) %>% distinct() %>% 
  mutate(in_EDU = TRUE) %>% left_join(csn_summ)



# looking at AEDU and these name changes
a = all_bm_summ %>% arrange(admission_time) %>% left_join(all_bed_moves)

a %>% filter(department %in% c("AECU", "T01ECU")) %>% group_by(department, date(admission_time)) %>% summarise(tot =n()) %>% 
  ggplot(aes(x = `date(admission_time)`, y = tot, fill = department)) + geom_bar(stat = "identity")

# Get summary information on each -----------------------------------------

bm_summ %>% filter(in_SAA) %>% group_by(patient_class) %>% summarise(n())
bm_summ %>% filter(in_CDU) %>% group_by(patient_class) %>% summarise(n())
bm_summ %>% filter(in_SDEC) %>% group_by(patient_class) %>% summarise(n())
bm_summ %>% filter(in_EDU) %>% group_by(patient_class) %>% summarise(n())

all_bm_summ %>% filter(in_EDU)  %>% group_by(patient_class) %>% summarise(n())


# Create bed move data for each type --------------------------------------

# set location to room level for SAA and SDEC

saa_bed_moves = bm_summ %>% filter(in_SAA) %>% select(csn, patient_class) %>% left_join(bed_moves) %>% 
  mutate(room3 = clean_room_names(department, room)) %>% 
  mutate(location = group_room_names(room3))

sdec_bed_moves = bm_summ %>% filter(in_SDEC) %>% select(csn, patient_class) %>% left_join(bed_moves)  %>% 
  mutate(room3 = clean_room_names(department, room)) %>% 
  mutate(location = group_room_names(room3))

# set location to dept level for CDU and AEDU

cdu_bed_moves = bm_summ %>% filter(in_CDU) %>% select(csn, patient_class) %>% left_join(bed_moves) %>% 
  mutate(location = department)

edu_bed_moves = bm_summ %>% filter(in_EDU) %>% select(csn, patient_class) %>% left_join(bed_moves) %>% 
  mutate(location = department)



# Create edgelist for each ------------------------------------------------

# SAA - create edgelist
edgedf_saa <- get_care_site_flows(saa_bed_moves)

edgedf_saa %>% select(csn) %>% n_distinct()
saa_bed_moves %>% select(csn) %>% n_distinct() # 2 only had one location

# to subset with earliest SAA - 

edgedf_saa <- edgedf_saa %>% left_join(
  edgedf_saa %>% filter(to == "SAA") %>% group_by(csn) %>% summarise(earliest = min(dttm))
)  %>% group_by(csn) %>% mutate(before = if_else(earliest < dttm, FALSE, TRUE)) %>% 
  select(-earliest)

# SDEC - create edgelist
edgedf_sdec <- get_care_site_flows(sdec_bed_moves)

edgedf_sdec %>% select(csn) %>% n_distinct()
sdec_bed_moves %>% select(csn) %>% n_distinct() # same

edgedf_sdec <- edgedf_sdec %>% left_join(
  edgedf_sdec %>% filter(to == "SDEC") %>% group_by(csn) %>% summarise(earliest = min(dttm))
)  %>% group_by(csn) %>% mutate(before = if_else(earliest < dttm, FALSE, TRUE)) %>% 
  select(-earliest)

# CDU - create edgelist

edgedf_cdu <- get_care_site_flows(cdu_bed_moves)

edgedf_cdu %>% select(csn) %>% n_distinct()
cdu_bed_moves %>% select(csn) %>% n_distinct() # 3 only had one location - therefore discharged from CDU

edgedf_cdu <- edgedf_cdu %>% left_join(
  edgedf_cdu %>% filter(to == "UCHT00CDU") %>% group_by(csn) %>% summarise(earliest = min(dttm))
)  %>% group_by(csn) %>% mutate(before = if_else(earliest < dttm, FALSE, TRUE)) %>% 
  select(-earliest)


# EDU - create edgelist

edgedf_edu <- get_care_site_flows(edu_bed_moves)

## NB edgelist nto working for edu - need to check this

edgedf_edu %>% select(csn) %>% n_distinct()
edu_bed_moves %>% select(csn) %>% n_distinct() # 18 had only one location

edgedf_edu <- edgedf_edu %>% left_join(
  edgedf_edu %>% filter(to %in% c("AECU", "T01ECU")) %>% group_by(csn) %>% summarise(earliest = min(dttm))
)  %>% group_by(csn) %>% mutate(before = if_else(earliest < dttm, FALSE, TRUE)) %>% 
  select(-earliest)


# Looking at destinations after each --------------------------------------

# SAA - look at destinations

edgedf_saa %>% filter(from == "SAA") %>% group_by(to) %>% summarise(tot = n()) %>% arrange(desc(tot))
edgedf_saa %>% filter(from == "SAA") %>% select(csn) %>% n_distinct()

edgedf_saa <- edgedf_saa %>% left_join(
  # get earliest location after this location that was not ED - 
  # note - need to join with edgedf to work out if before or after otherwies if came from other wards than ED, this doesn't pick them up
  saa_bed_moves %>% left_join(
    edgedf_saa %>% select(csn, dttm, before), by = c("csn", "discharge" = "dttm")
  ) %>% 
    filter(!before, department != "ED") %>%  group_by(csn) %>% summarise(min_not_ED = min(admission))
) %>%  mutate(first_not_ED = case_when(is.na(min_not_ED) ~ NA,
                                      min_not_ED == dttm ~ TRUE,
                                      TRUE ~ FALSE)) %>% select(-min_not_ED)

# next location after admission 

saa_summ <- bm_summ %>% filter(in_SAA) %>% left_join(
  edgedf_saa %>% filter(first_not_ED) %>% left_join(saa_bed_moves %>% select(csn, admission, department), 
                                                    by = c("csn", "dttm" = "admission")) %>% 
    select(csn, dttm, department) %>% 
    rename(dttm_left_ED = dttm , moved_to = department)
) %>% mutate(time_to_admit = difftime(dttm_left_ED, presentation_time, units = "hours"))

# one patient has two ED visis - is a huge outlier
saa_summ %>% filter(!is.na(time_to_admit), time_to_admit < 60) %>% ggplot(aes(x = time_to_admit)) + geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks =  seq(0,60,2), limits = c(0,60)) +
  labs(title = "Time from presentation at ED to admission for SAA patients were later admitted to a ward", 
       x = "Elapsed time (hours)")

saa_summ %>% filter(!is.na(time_to_admit)) %>% group_by(moved_to) %>% summarise(count = n()) %>% 
  ggplot(aes(x = moved_to, y = count)) + geom_bar(stat = "identity") +
  labs(title = "Next destination for SAA patients were later admitted", 
       x = "First location after ED")

# SDEC - look at destinations

edgedf_sdec %>% filter(from == "SDEC") %>% group_by(to) %>% summarise(tot = n()) %>% arrange(desc(tot))
edgedf_sdec %>% filter(from == "SDEC") %>% select(csn) %>% n_distinct()

edgedf_sdec <- edgedf_sdec %>% left_join(
  # get earliest location after this location that was not ED - 
  # note - need to join with edgedf to work out if before or after otherwies if came from other wards than ED, this doesn't pick them up
  sdec_bed_moves %>% left_join(
    edgedf_sdec %>% select(csn, dttm, before), by = c("csn", "discharge" = "dttm")
  ) %>% 
    filter(!before, department != "ED") %>%  group_by(csn) %>% summarise(min_not_ED = min(admission))
) %>% 
  mutate(first_not_ED = case_when(is.na(min_not_ED) ~ NA,
                                      min_not_ED == dttm ~ TRUE,
                                      TRUE ~ FALSE)) %>% select(-min_not_ED)

# next location after admission 

sdec_summ <- bm_summ %>% filter(in_SDEC) %>% left_join(
  edgedf_sdec %>% filter(first_not_ED) %>% left_join(sdec_bed_moves %>% select(csn, admission, department), 
                                                    by = c("csn", "dttm" = "admission")) %>% 
    select(csn, dttm, department) %>% 
    rename(dttm_left_ED = dttm , moved_to = department)
) %>% mutate(time_to_admit = difftime(dttm_left_ED, presentation_time))


sdec_summ %>% filter(!is.na(time_to_admit), time_to_admit < 60) %>% ggplot(aes(x = time_to_admit)) + geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks =  seq(0,60,2), limits = c(0,60)) +
  labs(title = "Time from presentation at ED to admission for SDEC patients who were later admitted to a ward", 
       x = "Elapsed time (hours)")

sdec_summ %>% filter(!is.na(time_to_admit)) %>% group_by(moved_to) %>% summarise(count = n()) %>% 
  ggplot(aes(x = moved_to, y = count)) + geom_bar(stat = "identity") +
  labs(title = "Next destination for SDEC patients were later admitted", 
       x = "First location after ED")

# to look at those that returned to ED or have null - seomthing weird going on with them !!
# the recent ones have null look like they haven't been discharged yet ?? 
s = sdec_summ %>% filter(moved_to == "ED") %>% select(csn) %>% left_join(sdec_bed_moves)

# CDU

edgedf_cdu %>% filter(from == "UCHT00CDU") %>% group_by(to) %>% summarise(tot = n()) %>% arrange(desc(tot))
edgedf_cdu %>% filter(from == "UCHT00CDU") %>% select(csn) %>% n_distinct()

edgedf_cdu <- edgedf_cdu %>% left_join(
  # get earliest location after this location that was not ED - 
  # note - need to join with edgedf to work out if before or after otherwies if came from other wards than ED, this doesn't pick them up
  cdu_bed_moves %>% left_join(
    edgedf_cdu %>% select(csn, dttm, before), by = c("csn", "discharge" = "dttm")
  ) %>% 
    filter(!before, department != "UCHT00CDU") %>%  group_by(csn) %>% summarise(min_not_ED = min(admission))
) %>%  mutate(first_not_ED = case_when(is.na(min_not_ED) ~ NA,
                                       min_not_ED == dttm ~ TRUE,
                                       TRUE ~ FALSE)) %>% select(-min_not_ED)

# next location after admission 

cdu_summ <- bm_summ %>% filter(in_CDU) %>% left_join(
  edgedf_cdu %>% filter(first_not_ED) %>% left_join(cdu_bed_moves %>% select(csn, admission, department), 
                                                    by = c("csn", "dttm" = "admission")) %>% 
    select(csn, dttm, department) %>% 
    rename(dttm_left_ED = dttm , moved_to = department)
) %>% mutate(time_to_admit = difftime(dttm_left_ED, presentation_time))



# Save data so far --------------------------------------------------------


outFile = paste0("EDcrowding/flow-mapping/data-raw/saa_bed_moves_",today(),".rda")
save(saa_bed_moves, file = outFile)

outFile = paste0("EDcrowding/flow-mapping/data-raw/cdu_bed_moves_",today(),".rda")
save(cdu_bed_moves, file = outFile)

outFile = paste0("EDcrowding/flow-mapping/data-raw/sdec_bed_moves_",today(),".rda")
save(sdec_bed_moves, file = outFile)

outFile = paste0("EDcrowding/flow-mapping/data-raw/edu_bed_moves_",today(),".rda")
save(edu_bed_moves, file = outFile)



outFile = paste0("EDcrowding/flow-mapping/data-raw/edgedf_saa_",today(),".rda")
save(edgedf_saa, file = outFile)

outFile = paste0("EDcrowding/flow-mapping/data-raw/edgedf_cdu_",today(),".rda")
save(edgedf_cdu, file = outFile)

outFile = paste0("EDcrowding/flow-mapping/data-raw/edgedf_sdec_",today(),".rda")
save(edgedf_sdec, file = outFile)





outFile = paste0("EDcrowding/flow-mapping/data-raw/bm_summ_",today(),".rda")
save(bm_summ, file = outFile)

# Explore AEDU ------------------------------------------------------------
### NOTE THAT THIS IS OLD CODE _ BUT KEEPING IT HERE
# this returns 4.5K rows; looks like next admission is not always instantaneous and can be before discharge from ED
ED_to_AEDU <- bed_moves %>% filter(department == "UCH EMERGENCY DEPT", next_department == "UCH T00 AMBULATORY ECU") %>% 
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


