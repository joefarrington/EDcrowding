# About this script
# =================

# This script reads data from EMAP Star using the view called
# bed_moves. It identified admissions that involved ED at some point
# and extracts all bed_moves for those admissions, including 
# onward destinations from ED. 
# 
# Ward names are shortened and grouped to focus only on onward
# wards of interest (AMU, ICU, T07 and HDRU, T08/09) with others
# grouped into Tower Other and Outside Tower
#
# Within ED, room names are cleaned so that all bay numbers
# and chairs are combined 
#
# Treatment rooms in the UTC (plaster, opthamology, treatment) are
# grouped 
#
# Various strange encounters are removed 
# and so are any encounters that involve pediatrics in ED
#
# Some additional processing is done to include durations 
# in locations with very low numbers (e.g. diagnostics, waiting room)
# into the previous node
#
# The final dataset has the following fields for department info
# - department (as original)
# - dept2 - department simplified by function called clean_ward_names
# - dept3 - dept2 simplified into "Still in ED" or "Admitted"
# Note: for any encounters where the last row is OTF, dept3 shows these
# as Admitted, even though department and dept2 show ED
#
# The final dataset has the following fields for room info
# - room (as original)
# - room2 - as room, but if this is NA, adds room info derived 
#   from the hl7_location field 
# - room3 - simplified by the function clean_room_names which
#   removes bay and room numbers
# - room4 - room3 simplified by the function group_room_names
#   which collapses room into the major categories of TRIAGE, 
#   RAT, UTC etc not distinguishing between COVID and non-COVID
# - room4_new - which inserts the previous room when the next one
#   is not used much (diagnostics, waiting room)



## Currently two problems
# duplicate rows are being created in ED_bed_moves
# duration rows should be in hours - currently in minutes

# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)


# Create functions
# ===============

# hl7_location seems to have more complete info than room and bed for some rows
# but locations are grouped into one string; this function splits them

split_location <- function(hl7_location, n) {
  s <- unlist(strsplit(hl7_location, "\\^"))
  return(s[n])
}
split_location <- Vectorize(split_location)

# shorten ward names
clean_wardnames5 <- function(x) {
  # If any ward has a name in brackets use the text in brackets as ward name e.g. "UCH T07 SOUTH (T07S)" becomes T07S
  if (grepl("\\(",x))  {
    start <- regexpr("\\(",x)[[1]]
    end <- regexpr("\\)",x)[[1]]
    x <- substr(x,start+1,end-1)
  }
  # Any West Moreland St location becomes Outside Tower
  else if (grepl("WMS",x)) {
    x <- "Outside Tower"
  } 
  # Any EGA location becomes  Outside Tower
  else if (grepl("EGA",x)) {
    x <- "Outside Tower"
  }
  # Any MCC location becomes  Outside Tower
  else if (grepl("MCC",x)) {
    x <- "Outside Tower"
  }
  # Any NHNN location becomes Outside Tower
  else if (grepl("NHNN",x)) {
    x <- "Outside Tower"
  }
  # Any RNTNE location becomes Outside Tower
  else if (grepl("RNTNE",x)) {
    x <- "Outside Tower"
  }
  # Anything with OFF in location becomes OFF
  else if (grepl("OFF",x)) {
    x <- "OFF"
  }
  # Anything with OUT in location becomes Outside Tower
  else if (grepl("OUT",x)) {
    x <- "Outside Tower"
  }
  # Anything with CRF in location becomes CRF
  else if (grepl("CRF",x)) {
    x <- "CRF"
  }
  # Anything with HARLEY in location becomes HARLEY
  else if (grepl("HARLEY",x)) {
    x <- "HARLEY"
  }
  # Anything with LOUNGE in location becomes LOUNGE
  else if (grepl("LOUNGE",x)) {
    x <- "LOUNGE"
  }
  x <- gsub("UCH ","",x) #remove UCH from any name
  x <- gsub("THEATRE SUITE","THR",x)
  x <- gsub(" CRITICAL CARE","",x)
  x <- gsub(" WARD","",x)
  x <- gsub(" SURG ","",x)
  x <- gsub(" SURGICAL ITU","SURG ITU",x)
  
  if (grepl("EMERGENCY DEPT",x)) {
    x <- "ED"
  } 
  if (grepl("EMERGENCY AU",x)) {
    x <- "AMU/EAU"
  }
  if (grepl("ACUTE MEDICAL",x)) {
    x <- "AMU/EAU"
  }
  if (grepl("T00",x)) {
    x <- "AMU/EAU"
  }
  if (grepl("P03 CV|P03 THR",x)) {
    x <- "ICU"
  }
  if (grepl("T03 INTENSIVE CARE",x)) {
    x <- "ICU"
  }
  if (grepl("HDRU",x)) {
    x <- "HDRU"
  }
  if (grepl("T07",x)) { #leave as is for now
    x <- "T07"
  }
  else if (grepl("T08|T09", x)) {
    x <- "T08/09" # leave as is for now
  }
  else if (grepl("T02", x)) {
    x <- "T02" # leave as is for now
  }
  else if (grepl("T[0-9][0-9]",x)) {
    x <- "Tower"
  }
  
  return(x)
}

clean_wardnames5 <- Vectorize(clean_wardnames5)

# clean room data
# function removes bay and chair numbers
clean_room_names <- function(dept, room) {
  if (dept == "ED" && !is.na(room)) {
    room = gsub("UCHED ","",room)
    room = gsub("CHAIR [0-9]{2}", "", room)
    room = gsub("[0-9]{2}", "", room)
    room = gsub("[0-9]{2}", "", room)
    room = gsub("MAJ-CHAIR","MAJORS",room)
    room = gsub("RAT-CHAIR","RAT",room)
    room = gsub("RATBED","RAT",room)
#    room = gsub("^UTC [A-z]+ ROOM","UTC",room)
    room = gsub("  "," ",room)
    room = gsub(" $","",room)
  }
  return(room)
}
clean_room_names <- Vectorize(clean_room_names)

# function to group room names
# NB RAT COVID MAJORS could be both - need to check which to prioritise
group_room_names <- function(room) {
  room_ <- case_when(
    length(grep("UTC", room)) >0 ~ "UTC",
    length(grep("RAT", room)) >0 ~ "RAT",
    length(grep("MAJORS", room)) >0 ~ "MAJORS",
    length(grep("PAEDS TRIAGE", room)) >0 ~ "PAEDS TRIAGE",
    length(grep("TRIAGE", room)) >0 ~ "TRIAGE",
    TRUE ~ room)
  return(room_)
}

group_room_names <- Vectorize(group_room_names)


# simple function to return whether location denotes admission
calc_admission <- function(dept2, room2, discharge, discharge_dttm, pre_OTF_ED_discharge_dttm) {
  if (dept2 != "ED") {
    return("Admitted")
  }
  else if (room2 == "OTF" && !is.na(pre_OTF_ED_discharge_dttm) 
           # added this to make sure there are later rows - ie patient was eventually admitted
           && discharge < discharge_dttm)
    return("Admitted")
  else {
    return("Still in ED")
  }
}

calc_admission <- Vectorize(calc_admission)



# simple function to return whether location involves pediatrics in ED
calc_pediatric <- function(room) {
  if (length(grep("PAEDS", room)) >0 ) {
    return(TRUE)
  }
  else
    return(FALSE)
}

calc_pediatric <- Vectorize(calc_pediatric)


# Load bed_move data
# ==================

ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")

## EITHER get bed moves from Star

sqlQuery <- "SELECT *
  FROM star.bed_moves bm"
sqlQuery <- gsub('\n','',sqlQuery)

bed_moves <- as_tibble(dbGetQuery(ctn, sqlQuery))

# remove rows where admission == discharge; 
# these are rows where a patient spent no time in a location
# to see the number: 
bed_moves %>% filter(admission == discharge) %>% n_distinct() # 209

# to remove these rows
bed_moves <- bed_moves %>% filter(admission != discharge)

# join to get encounter_id for each encounter
# encounter_id is used as key on patient_fact

sqlQuery <- "SELECT *
  FROM star.encounter 
"
sqlQuery <- gsub('\n','',sqlQuery)
encounter <- as_tibble(dbGetQuery(ctn, sqlQuery))

bed_moves <- bed_moves %>% left_join(encounter %>% select(encounter_id, encounter), 
                                     by = c("csn" = "encounter"))

# derive admission and discharge dates
bed_moves <- bed_moves %>% group_by(mrn, csn, encounter_id) %>% 
  mutate(arrival_dttm = min(admission), discharge_dttm = max(discharge)) %>% 
  arrange(mrn, csn, admission)

# considering how to handle OTF rows, the following shows that 87 + 5 NA rows do not have OTF as last ED location
bed_moves %>% filter(room == "OTF") %>% select(csn, room) %>% 
  left_join(bed_moves %>% 
            filter(department == "UCH EMERGENCY DEPT") %>% 
            group_by(csn) %>% 
            filter(discharge == max(discharge)) %>%
            select(csn, last_room = room)) %>% 
  group_by(last_room != "OTF") %>% 
  summarise(n())

# identify ED rows - NOTE I considered excluding OTF rows here but decided against due to the above
bed_moves <- bed_moves %>% 
#  mutate(ED_row = case_when(department == "UCH EMERGENCY DEPT" & room != "OTF" ~ 1,
  mutate(ED_row = case_when(department == "UCH EMERGENCY DEPT" ~ 1,
                            TRUE ~ 0))

# identify admission rows (first for each encounter)
bed_moves <- bed_moves %>% 
  mutate(arrival_row = ifelse(admission == arrival_dttm, TRUE, FALSE))

# save data for future loading
outFile = paste0("EDcrowding/flow-mapping/data-raw/bed_moves_",today(),".rda")
save(bed_moves, file = outFile)
rm(outFile)

# OR LOAD EXISTING FILE

inFile = paste0("EDcrowding/flow-mapping/data-raw/bed_moves_2020-07-27.rda")
load(inFile)

# Select only encounters involving ED
# ===================================

# select only encounters with one or more bed moves involving ED
ED_csn <- bed_moves %>% 
  filter(date(admission) == "2020-02-04") %>% 
  select(csn, ED_row) %>% 
  group_by(csn) %>% 
  summarise(num_ED_rows = sum(ED_row)) %>% 
  filter(num_ED_rows > 1) 

# get subset of bed_moves
ED_bed_moves <- bed_moves %>% 
  filter(csn %in% ED_csn$csn)

# calc row durations
ED_bed_moves <- ED_bed_moves %>% 
  mutate(duration_row = difftime(discharge, admission, units = "hours"))

# get arrival, discharge and ED discharge time; this will not be correct for OTF as last row
ED_csn_summ <- ED_bed_moves %>% 
  filter(ED_row == 1) %>%
  group_by(mrn, csn, encounter_id, arrival_dttm, discharge_dttm) %>% 
  summarise(ED_discharge_dttm = max(discharge), 
            ED_duration = difftime(ED_discharge_dttm, arrival_dttm, units = "mins")) %>% 
  select(mrn, csn, encounter_id, arrival_dttm, ED_discharge_dttm, ED_duration)

# arrange by arrival time
ED_bed_moves <- ED_bed_moves %>% 
  arrange(arrival_dttm, admission)


# identify CSNs involving OTF and their last location in ED (which isn't always OTF)
OTF_csn <- ED_bed_moves %>% 
  filter(ED_row == 1, room == "OTF") %>% select(csn) %>% 
  mutate(OTF_csn = 1) %>% 
  left_join(ED_bed_moves %>% 
              filter(ED_row == 1) %>% 
              group_by(csn) %>% 
              filter(discharge == max(discharge)) %>%
              select(csn, last_room = room)) %>% 
  ungroup()

# create a small version of bed_moves to use for OTF calculations - where the last row is OTF
OTF_bed_moves <- OTF_csn %>% filter(last_room == "OTF") %>% select(csn) %>% 
  left_join(ED_bed_moves)

# calculate ED duration for csns where OTF is the last row
OTF_ED_duration <- OTF_bed_moves %>% 
  filter(ED_row == 1, room != "OTF") %>%
  group_by(mrn, csn, encounter_id, arrival_dttm, discharge_dttm) %>% 
  summarise(pre_OTF_ED_discharge_dttm = max(discharge)) %>% 
  mutate(pre_OTF_ED_duration = difftime(pre_OTF_ED_discharge_dttm, arrival_dttm, units = "mins")) %>% 
  select(mrn, csn, encounter_id, arrival_dttm, pre_OTF_ED_discharge_dttm, pre_OTF_ED_duration)

# update ED duration
ED_csn_summ <- ED_csn_summ %>% left_join(OTF_ED_duration)

# update ED_bed_moves with the new information
ED_bed_moves <- ED_bed_moves %>% left_join(ED_csn_summ)

# change ED_row flag where last row is OTF
ED_bed_moves <- ED_bed_moves %>% 
  mutate(ED_row = case_when(ED_row == 1 & discharge > pre_OTF_ED_discharge_dttm ~ 0,
                            TRUE ~ ED_row))

# Data cleaning for encounters involving ED
# =========================================

# some bed move rows have no department information
ED_bed_moves <- ED_bed_moves %>% filter(!is.na(department))

# hl7_location seems to have more complete info in some cases where room and bed are NA
ED_bed_moves <- ED_bed_moves %>% 
  mutate(room2 = case_when(is.na(room) & hl7_location != "ED^null^null" ~
                             split_location(hl7_location, 2),
                           TRUE ~ room))

# where there is still no room information, assume patient is Waiting
ED_bed_moves <- ED_bed_moves %>% 
  mutate(room2 = case_when(is.na(room2) & hl7_location == "ED^null^null" ~ "Waiting",
                           TRUE ~ room2))

# shorten ward name
ED_bed_moves <- ED_bed_moves %>% 
  mutate(dept2 = clean_wardnames5(department))

# use d to see which room names the function clean_wardnames5 has grouped: 
# d <- ED_bed_moves %>% group_by(department, dept2) %>% summarise(total = n()) 

# clean room names
ED_bed_moves <- ED_bed_moves %>% 
  mutate(room3 = clean_room_names(dept2, room2))

# use e to see which room names the function clean_room_names has grouped: 
# e <- ED_bed_moves %>% filter(dept2 == "ED") %>% 
#   group_by(dept2, room2, room3) %>% summarise(total = n()) 

# to see mapping from room to room3 (note room 3 has more values because we used the HL7 field)
# g <- ED_bed_moves %>% 
#   filter(department == "UCH EMERGENCY DEPT") %>% 
#   group_by(room, room3) %>% 
#   summarise(tot = n()) %>% 
#   select(room, room3, tot) %>%
#   pivot_wider(names_from = room3, values_from = tot)

# simple version of department (Admitted or still in ED) used in edge list
ED_bed_moves <- ED_bed_moves %>% 
  mutate(dept3 = calc_admission(dept2, room2, discharge, discharge_dttm, pre_OTF_ED_discharge_dttm))

# Create grouped room name column (incorporates COVID and non-COVID locations into one category)
ED_bed_moves <- ED_bed_moves %>% 
  mutate(room4 = group_room_names(room3))

# to see mapping from room3 to room4 (note this function prioritises RAT over MAJORS but that only affects 28 rows)
# h <- ED_bed_moves %>% 
#   filter(department == "UCH EMERGENCY DEPT") %>% 
#   group_by(room3, room4) %>% 
#   summarise(tot = n()) %>% 
#   select(room3, room4, tot) %>%
#   pivot_wider(names_from = room4, values_from = tot)
# 
# # save data on mappings for reference
# 
# outFile = paste0("EDcrowding/flow-mapping/data-output/room_mappings_",today(),".rda")
# save(h, file = outFile)
# rm(outFile)

# denote whether location involves pediatrics
ED_bed_moves <- ED_bed_moves %>% 
  mutate(pediatric_row = calc_pediatric(room3))


# # look at outliers
# ggplot(ED_bed_moves %>% filter(ED_row == 1), # one clear outlier
#        aes(x=1, y = duration_row)) + 
#         geom_boxplot()
# 
# # without that one, majority of long durations rows are OTF (off the floor)
# ggplot(ED_bed_moves %>% filter(ED_row == 1, duration_row < 400), aes(x=room3, y = duration_row)) + 
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))

# remove one outlier
ED_bed_moves <- ED_bed_moves %>% filter(duration_row < 400) %>% 
  arrange(mrn, csn, admission)


# Handle unwanted and weird encounters
# ====================================

# work out csns to ignore if involve pediatrics 
pediatric_csn <- ED_bed_moves %>% group_by(mrn, csn, arrival_dttm) %>% 
  summarise(tot = sum(pediatric_row)) %>% 
  filter(tot > 0) %>% 
  select(mrn, csn) %>% distinct() # 4752

# some csns have rows where the discharge time is earlier than the admission time
odd_csn <- ED_bed_moves %>% filter(discharge < admission) %>% select(csn)
# to see the number of csns removed: 
odd_csn %>% select(csn) %>% n_distinct() #118


# identify visits where someone goes to ED from another location
elsewhere_to_ED_csn <- ED_bed_moves %>% 
  group_by(csn) %>% 
  select(csn, ED_row, arrival_row) %>% 
  mutate(check = case_when(ED_row == 1 & !arrival_row & lag(ED_row) == 0 ~ "B",
                           TRUE ~ "A")) %>% 
  filter(check == "B") %>% select(csn) # 175

# still 99 csns with more than one day:
long_ED_csn <-  ED_bed_moves %>% 
  filter(!csn %in% elsewhere_to_ED_csn$csn,
         as.numeric(ED_duration, units = "days") > 1) %>%
  select(csn, ED_duration, pre_OTF_ED_duration) %>% distinct()

# remove those where pre_OTF_ED_duration is shorter than a day
long_ED_csn <- long_ED_csn %>% 
  filter(as.numeric(pre_OTF_ED_duration, units = "days") > 1)
# reduces this down to 7 csns

# before, I had encounters that appeared to be stuck in OTF and TAF - this code no longer needed
# stuck_in_otf_taf_csn <- ED_bed_moves %>% 
#   filter(csn %in% long_ED_csn$csn) %>%
#   group_by(csn) %>% filter(ED_row == 1) %>%
#   filter(admission == max(admission), room4 %in% c("OTF", "TAF")) %>% select(csn)
# # Note that doing two separate filters in the above code - picked up 5 more this way

# One is genuinely weird - spent 2 months in ED but only one row
ED_bed_moves %>% filter(csn == "1018734672")

# this one spent 163 in OTF and then went elsewhere in ED so I didn't pick them up
ED_bed_moves %>% filter(csn == "1019997966")

# several have weird arrival rows
odd_arrival_rows <- ED_bed_moves %>% filter(arrival_row, 
                            hl7_location %in% c("ED^UCHED WR POOL^WR",
                                                "ED^UCHED OTF POOL^OTF")) %>% 
  select(csn) %>% distinct()

# create clean version of ED bed moves

ED_bed_moves <- ED_bed_moves %>% 
  filter(
    !csn %in% pediatric_csn$csn,
    !csn %in% odd_csn$csn,
    !csn %in% odd_arrival_rows$csn,
    !csn %in% elsewhere_to_ED_csn$csn,
    !csn == "1019997966",  
    !csn == "1018734672")


# create clean version of ED_csn_summ

ED_bed_moves <- ED_bed_moves %>% 
  mutate(ED_duration_final = case_when(!is.na(pre_OTF_ED_duration) ~ pre_OTF_ED_duration,
         TRUE ~ ED_duration),
         ED_discharge_dttm_final = case_when(!is.na(pre_OTF_ED_discharge_dttm) ~ pre_OTF_ED_discharge_dttm,
                                       TRUE ~ ED_discharge_dttm))

ED_csn_summ <- ED_bed_moves %>% 
  group_by(mrn, csn, encounter_id, arrival_dttm, discharge_dttm, 
           ED_discharge_dttm, pre_OTF_ED_discharge_dttm, ED_discharge_dttm_final,
           ED_duration, pre_OTF_ED_duration, ED_duration_final) %>% 
  summarise(num_ED_rows = sum(ED_row)) %>% ungroup() %>% 
  filter(num_ED_rows > 0) %>% 
  
  # create breach detail
  mutate(seen4hrs = ifelse(ED_duration < hours(4), "Seen in 4 hours", "Breach")) %>% 
  
  # join with ED bed_moves (ED rows only) to get last ED location
  left_join(ED_bed_moves %>% 
              filter(ED_row == 1) %>%
              group_by(mrn, csn, encounter_id, arrival_dttm, discharge_dttm) %>% 
              filter(discharge == ED_discharge_dttm_final) %>% 
              mutate(ED_last_loc = room4) %>% 
              select(csn, ED_last_loc))



ED_csn_summ <- ED_csn_summ %>%  
  # calculate whether admitted
  mutate(ED_last_status = if_else(discharge_dttm == ED_discharge_dttm, "Discharged", "Admitted"))


# Process OTF into other nodes
# ============================
# add time in diagnostics and Waiting room into duration of previous node

ED_bed_moves <- ED_bed_moves %>% 
  group_by(csn) %>%
  # add a column to contain the discharge time of the next row
  mutate(next_location = lead(room4),
         next_dttm = lead(discharge)) %>% 
  # use the date for the next row as the discharge time when next row is diagnostics or Waiting room
  mutate(discharge_new = case_when(next_location %in% c("DIAGNOSTICS", "WAITING ROOM") ~ next_dttm,
                                   TRUE ~ discharge)) %>% 
  mutate(duration_row_new = difftime(discharge_new, admission, units = "hours"))   

ED_bed_moves <- ED_bed_moves %>% 
  group_by(csn) %>%
  # add a column to contain the previous location
  mutate(prev_location = lag(room4)) %>% 
  mutate(room4_new = case_when(room4 %in% c("DIAGNOSTICS", "WAITING ROOM") ~ prev_location,
                           TRUE ~ room4))

# # a few CSNs have multiple iterations through OTF and Waiting room
# ED_bed_moves %>% filter(room4_new %in% c("DIAGNOSTICS", "WAITING ROOM")) %>% select(csn) %>% distinct()
# 
# ED_bed_moves <- ED_bed_moves %>% 
#   group_by(csn) %>%
#   mutate(prev_location = lag(room4_new)) %>% 
#   mutate(room4_new = case_when(room4_new %in% c("DIAGNOSTICS", "WAITING ROOM") ~ prev_location,
#                                TRUE ~ room4_new))


# Save data
# =========

# save ED_bed_moves for later use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_2_",today(),".rda")
save(ED_bed_moves, file = outFile)
rm(outFile)

# OR load saved ED_bed_moves

inFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_","2020-07-27",".rda")
load(inFile)


# save ED_csn_summ for future use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_csn_summ_2_",today(),".rda")
save(ED_csn_summ, file = outFile)
rm(outFile)

# OR LOAD saved data
inFile = paste0("EDcrowding/flow-mapping/data-raw/ED_csn_summ_","2020-07-27",".rda")
load(inFile)


