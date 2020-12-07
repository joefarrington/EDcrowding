# About this script
# =================
# 
# The script processes the raw data from ED_bed_moves_raw 
# 
# Ward and room names are cleaned as shown in the wiki:
# https://github.com/zmek/EDcrowding/wiki/Data-Dictionary:-ED_bed_moves
#

# Load libraries
# ==============

library(dplyr)
library(tidyverse)
library(lubridate)


# Create functions
# ===============


# shorten ward names

clean_wardnames7 <- function(x) {
  if (grepl("ED|UCHT00CDU",x) ) {
    x <- "ED"
  } 
  else if (grepl("EMERGENCY AU",x)) {
    x <- "EAU"
  }
  else if (grepl("ACUTE MEDICAL",x)) {
    x <- "AMU"
  }
  else if (grepl("T07",x)) { 
    x <- "T07"
  }
  else if (grepl("T06",x)) { 
    x <- "T06"
  }
  else if (grepl("T09",x)) { 
    x <- "T09"
  }
  else if (grepl("T08",x)) { 
    x <- "T08"
  }
  else if (grepl("T10",x)) { 
    x <- "T10"
  }
  else if (grepl("P03|T03",x)) {
    x <- "ICU/Theatres"
  }
  else if (grepl("T[0-9][0-9]",x)) {
    x <- "Tower Other"
  }
  # Any NHNN location becomes Outside Tower
  else {
    x <- "Outside Tower"
  }
  
  return(x)
}

clean_wardnames7 <- Vectorize(clean_wardnames7)


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

get_covid_pathway <- function(room) {
  if (grepl("^COVID", room)) {
    return(TRUE)
  } 
  else if (grepl("NON COVID", room)) {
    return(FALSE) 
  } 
  else {
    return(NA)
  }
  
}

get_covid_pathway <- Vectorize(get_covid_pathway)

# # Load bed_move data
# # ==================

file_label <- "all_" # note - updated the file names of the flow datasets at the end of this file (if dates have changed)
inFile <- paste0("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_raw_",file_label,"2020-12-02.rda")
load(inFile)

inFile <- paste0("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_raw_",file_label,"2020-12-02.rda")
load(inFile)


# Basic checks
# ============


# remove rows where admission == discharge; 
oneED_adm_equal_dis <- ED_bed_moves_raw %>% 
  left_join(ED_csn_summ_raw %>% select(csn, num_ED_rows)) %>% 
  filter(admission == discharge, num_ED_rows == 1) %>% select(csn) %>% distinct()

# where there is only one row these csns also need to be deleted from ED_csn_summ_raw
ED_csn_summ_raw = ED_csn_summ_raw %>% 
  anti_join(oneED_adm_equal_dis)

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  anti_join(oneED_adm_equal_dis)

rm(oneED_adm_equal_dis)

print("After removing oneED_adm_equal_dis")
print(paste0("ED_csn_sum_raw: ",ED_csn_summ_raw %>% select(csn) %>% n_distinct()))
print(paste0("ED_bed_moves_raw: ",ED_bed_moves_raw %>% select(csn) %>% n_distinct()))

# remove other rows where admission == discharge; 
# find rows - there are many
csn_to_keep = ED_bed_moves_raw %>% filter(admission != discharge) %>% select(csn) %>% distinct()
other_adm_equal_dis = ED_csn_summ_raw %>% anti_join(csn_to_keep)

ED_csn_summ_raw = ED_csn_summ_raw %>% 
  anti_join(other_adm_equal_dis)

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  anti_join(other_adm_equal_dis)

rm(other_adm_equal_dis, csn_to_keep)

# finally, get rid of the duplicate rows for the other csns
ED_bed_moves_raw <- ED_bed_moves_raw %>% filter(admission != discharge)

print("After removing other_adm_equal_dis")
print(paste0("ED_csn_sum_raw: ",ED_csn_summ_raw %>% select(csn) %>% n_distinct()))
print(paste0("ED_bed_moves_raw: ",ED_bed_moves_raw %>% select(csn) %>% n_distinct()))

# remove csns where admission is later than discharge
admission_later_csns <- ED_bed_moves_raw %>% filter(admission > discharge) %>% select(csn) %>% distinct() # none to remove
print(paste0("Admission later csns: ", nrow(admission_later_csns)))

# identify arrival rows (first row for each encounter)

ED_bed_moves_raw <- ED_bed_moves_raw %>%
  left_join(ED_csn_summ_raw %>% select(csn, presentation_time)) %>% 
  select(mrn, csn, presentation_time, everything())

# now using presentation time to identify arrival row - which has no saved seconds
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(arrival_row = ifelse(floor_date(admission, "minute") == floor_date(presentation_time), TRUE, FALSE))

# identify csns where arrival row not picked up this way. This could be because
# 1. patient came to ED from somewhere else thus has presentation time much earlier than arrival in ED 
# 2. csn was reused later eg see csn 1020805305 who came to ED 10 days after presentation at Virtual clinic or  csn 1023337830 (from oncology)
# 3. arrival time and presentation time are not within same minute
# note - some peopley may end up with mulitple arrival rows if they go within 1 min of presentation time to another location

no_arrival_row = ED_csn_summ_raw %>% anti_join(ED_bed_moves_raw %>% filter(arrival_row)  %>% select(csn, arrival_row))

# get earliest time of arrival to ED
no_arrival_row <- no_arrival_row %>% 
  left_join(ED_bed_moves_raw %>% filter(ED_row == 1) %>% 
              group_by(csn) %>% summarise(first_ED_arrival = min(admission)))

# set this to be the arrival row in ED
ED_bed_moves_raw <- ED_bed_moves_raw %>% ungroup() %>% 
  left_join(
    no_arrival_row  %>% select(csn, first_ED_arrival)
  ) %>% 
  mutate(arrival_row = case_when(first_ED_arrival == admission ~ TRUE,
                                 TRUE ~ arrival_row))

rm(no_arrival_row)

# check every visit has an arrival row 
ED_bed_moves_raw %>% filter(arrival_row) %>% select(csn) %>% n_distinct()

# delete rows in bed_moves where the movement is prior to the first ED arrival
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
   mutate(first_ED_arrival = if_else(is.na(first_ED_arrival), presentation_time, first_ED_arrival)) %>% 
   filter(admission >= first_ED_arrival)

print("After deleting ED rows where movement is prior to first ED arrival")
print(paste0("ED_csn_sum_raw: ",ED_csn_summ_raw %>% select(csn) %>% n_distinct()))
print(paste0("ED_bed_moves_raw: ",ED_bed_moves_raw %>% select(csn) %>% n_distinct()))


# calc row durations
ED_bed_moves_raw <- ED_bed_moves_raw %>% ungroup() %>% 
  mutate(duration_row = difftime(discharge, admission, units = "hours"))



# indicate whether row is OTF location
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(OTF_row = case_when(room == "UCHED OTF POOL" ~ 1,
                            TRUE ~ 0)) 

# a few csns have OTF in the arrival row 

print(Sys.time() - timer)
print("Remove OTF arrival row csns")
timer <- Sys.time()


OTF_arrival_csn <- ED_bed_moves_raw %>% 
  filter(OTF_row ==1, arrival_row) %>% select(csn) %>% distinct()

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  anti_join(OTF_arrival_csn)

ED_csn_summ_raw <- ED_csn_summ_raw  %>% 
  anti_join(OTF_arrival_csn)

rm(OTF_arrival_csn)


print("After removing OTF_arrival_csn")
print(paste0("ED_csn_sum_raw: ",ED_csn_summ_raw %>% select(csn) %>% n_distinct()))
print(paste0("ED_bed_moves_raw: ",ED_bed_moves_raw %>% select(csn) %>% n_distinct()))



# Simplify room names -----------------------------------------------------


# create room3 (see wiki for more information)

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(room3 = clean_room_names(department, room))

# create temp mapping to speed up creation of room4 
room_mapping <- ED_bed_moves %>% filter(ED_row ==1) %>% group_by(room3) %>% summarise(tot = n()) %>% 
  mutate(room4 = group_room_names(room3)) %>% select(-tot)

# Create room4 (see wiki for more information)
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  left_join(room_mapping)

# create covid_pathway 

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(covid_pathway = case_when(presentation_time > '2020-06-01' & ED_row == 1 ~ 
                                     get_covid_pathway(room),
                                   TRUE ~ NA))


# Final data cleaning
# ===================


# identify visits where someone goes to ED from another location
elsewhere_to_ED_csn <- ED_bed_moves_raw %>% ungroup() %>% 
  select(mrn, csn, #csn_old, 
         ED_row, arrival_row) %>% 
  group_by(csn) %>% 
  mutate(check = case_when(ED_row == 1 & !arrival_row & lag(ED_row) == 0 ~ "B",
                           TRUE ~ "A")) %>% 
  filter(check == "B") %>% 
  select(csn) %>% distinct() 

print("number of elsewhere to ED csns:")
print(elsewhere_to_ED_csn %>% select(csn) %>% n_distinct()) #0Jan/Feb; 0 in Mar/Apr; 4 in May/Jun/Jul; 0 in August

ED_csn_summ_raw <- ED_csn_summ_raw %>% 
  anti_join(elsewhere_to_ED_csn)

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  anti_join(elsewhere_to_ED_csn)


print("After removing elsewhere_to_csn")
print(paste0("ED_csn_sum_raw: ",ED_csn_summ_raw %>% select(csn) %>% n_distinct()))
print(paste0("ED_bed_moves_raw: ",ED_bed_moves_raw %>% select(csn) %>% n_distinct()))


# look at csns with duration of more than 48 hours, I can now see that most have max_emerg_class earlier than the discharge
# therefore

long_ED_csn <-  ED_bed_moves_raw %>% ungroup() %>% 
  filter(ED_row == 1, OTF_row !=1, room3 != "TAF", # exclude TAF in this as peopel have long durations there
         duration_row > 48)

long_ED_csn = long_ED_csn %>% left_join(ED_csn_summ_raw %>% select(csn, max_emerg_class)) %>% 
  select(csn, room3, admission, discharge, max_emerg_class, duration_row)

# # only 5 have as max_emerg_class as late as the discharge from this location
# long_ED_csn %>% filter(discharge <= max_emerg_class)


ED_csn_summ_raw <- ED_csn_summ_raw %>% 
  anti_join(long_ED_csn %>% filter(discharge <= max_emerg_class) %>% select(csn))

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  anti_join(long_ED_csn %>% filter(discharge <= max_emerg_class) %>% select(csn))


print("After removing long_ED_csn")
print(paste0("ED_csn_sum_raw: ",ED_csn_summ_raw %>% select(csn) %>% n_distinct()))
print(paste0("ED_bed_moves_raw: ",ED_bed_moves_raw %>% select(csn) %>% n_distinct()))



# Final data for ML -------------------------------------------------------



# calculate whether admitted
ED_csn_summ <- ED_csn_summ_raw %>%  
  mutate(adm = if_else(patient_class == "INPATIENT", TRUE, FALSE))


ED_csn_summ <- ED_csn_summ %>%  
  mutate(epoch = case_when(presentation_time < '2020-03-31' ~ "Pre_Covid",
                           presentation_time < '2020-05-31' ~ 'Surge1',
                           TRUE ~ 'Post_Surge1')) %>% 
  mutate(epoch = factor(epoch, levels = c("Pre_Covid", "Surge1", "Post_Surge1")))


ED_bed_moves <- ED_bed_moves_raw %>% 
  select(-presentation_time) 




# Save data
# =========

print("Saving data")

# save ED_bed_moves for later use


outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_",file_label,today(),".rda")
save(ED_bed_moves, file = outFile)
rm(outFile)

# save ED_csn_summ for future use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_csn_summ_",file_label,today(),".rda")
save(ED_csn_summ, file = outFile)
rm(outFile)


