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

# hl7_location seems to have more complete info than room and bed for some rows
# but locations are grouped into one string; this function splits them

split_location <- function(hl7_location, n) {
  s <- unlist(strsplit(hl7_location, "\\^"))
  return(s[n])
}
split_location <- Vectorize(split_location)

# shorten ward names

clean_wardnames7 <- function(x, include_CDU) {
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
    TRUE ~ room)
  return(room_)
}

group_room_names <- Vectorize(group_room_names)



# # Load bed_move data
# # ==================
timer <- Sys.time()
print("Loading data")
print(Sys.time() - timer)
timer <- Sys.time()


file_label <- "all_" # note - updated the file names of the flow datasets at the end of this file (if dates have changed)
inFile <- paste0("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_raw_",file_label,"2020-11-30.rda")
load(inFile)

inFile <- paste0("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_raw_",file_label,"2020-11-30.rda")
load(inFile)

# # Name changes (SQL doesn't do capitals)
# ED_bed_moves_raw <- ED_bed_moves_raw %>% 
#   rename(ED_arrival_dttm = arrival_dttm,
#          ED_discharge_dttm = discharge_dttm)
# 
# # add pk_bed_moves as system time plus rownumber
# key_start = as.numeric(Sys.time())
# ED_bed_moves_raw <- ED_bed_moves_raw %>% ungroup() %>% 
#   mutate(pk_bed_moves = paste0(key_start,row_number()))

# Basic checks
# ============

print(Sys.time() - timer)
print("Basic checks")
timer <- Sys.time()

# remove rows where admission == discharge; 
oneED_adm_equal_dis <- ED_bed_moves_raw %>% filter(admission == discharge, num_ED_rows == 1) %>% select(csn) %>% distinct()

# where there is only one row these csns also need to be deleted from ED_csn_summ_raw
ED_csn_summ_raw = ED_csn_summ_raw %>% 
  anti_join(oneED_adm_equal_dis)

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  anti_join(oneED_adm_equal_dis)

rm(oneED_adm_equal_dis)


# remove other rows where admission == discharge; 
ED_bed_moves_raw <- ED_bed_moves_raw %>% filter(admission != discharge) 


# # remove csns where admission is later than discharge
# admission_later_csns <- ED_bed_moves_raw %>% filter(admission > discharge) %>% select(mrn, csn) %>% distinct()
# ED_bed_moves_raw <- ED_bed_moves_raw %>% filter(!csn %in% admission_later_csns$csn)
# # to see the number of csns to remove: 
# print("number of csns removed because admission later than discharge:")
# print(admission_later_csns %>% select(csn) %>% n_distinct()) 

# duplicate_admission_csns <- ED_bed_moves_raw %>% group_by(mrn, csn, admission) %>% summarise(tot = n()) %>% filter(tot >1) %>% select(mrn, csn) %>% distinct()
# ED_bed_moves_raw <- ED_bed_moves_raw %>% filter(!csn %in% duplicate_admission_csns$csn)
# print("number of csns removed because more than one row with same admission time:")
# print(duplicate_admission_csns %>% select(csn) %>% n_distinct()) 


print(Sys.time() - timer)
print("Arrival and discharge dttms")
timer <- Sys.time()

# get arrival and discharge dttms
ED_bed_moves_raw <- ED_bed_moves_raw %>%
  left_join(ED_csn_summ_raw %>% select(mrn, csn, presentation_time, admission_time, last_ED_discharge_time, discharge_time)) %>% 
  select(mrn, csn, presentation_time, admission_time, last_ED_discharge_time, discharge_time, everything())


# identify arrival rows (first row for each encounter)

print(Sys.time() - timer)
print("Arrival rows")
timer <- Sys.time()

# now using presentation time to identify arrival row - which has no saved seconds
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(arrival_row = ifelse(floor_date(admission, "minute") == floor_date(presentation_time), TRUE, FALSE))

# identify csns where arrival row not picked up this way. This could be because
# 1. patient came to ED from somewhere else thus has presentation time much earlier than arrival in ED 
# 2. csn was reused later eg see csn 1020805305 who came to ED 10 days after presentation at Virtual clinic or  csn 1023337830 (from oncology)
# 3. arrival time and presentation time are not within same minute

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

# delete rows in bed_moves where the movement is prior to the first ED arrival
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
   mutate(first_ED_arrival = if_else(is.na(first_ED_arrival), presentation_time, first_ED_arrival)) %>% 
   filter(admission >= first_ED_arrival)

# calc row durations
print(Sys.time() - timer)
print("Row durations")
timer <- Sys.time()

ED_bed_moves_raw <- ED_bed_moves_raw %>% ungroup() %>% 
  mutate(duration_row = difftime(discharge, admission, units = "hours"))


# # indicate whether row is ED location
# 
# print(Sys.time() - timer)
# print("ED location")
# timer <- Sys.time()
# 
# if (include_CDU == 1) {
#   ED_bed_moves_raw <- ED_bed_moves_raw %>% 
#     mutate(ED_row = case_when(department %in% c("UCH EMERGENCY DEPT", "UCH T00 CLIN DECISION") ~ 1 ,
#                               TRUE ~ 0))
#   print("Including CDU within ED")
#   } else {
#   ED_bed_moves_raw <- ED_bed_moves_raw %>% 
#     mutate(ED_row = case_when(department %in% c("UCH EMERGENCY DEPT") ~ 1 ,
#                               TRUE ~ 0))
#   print("CDU will not be included within ED")
#   
# }


# # set arrival row where there are multiple ED visits in same csn
# 
# print(Sys.time() - timer)
# print("Handle multiple visits")
# timer <- Sys.time()
# 
# ED_bed_moves_raw <- ED_bed_moves_raw %>% 
#   group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
#   mutate(arrival_row = case_when(ED_row == 1 & is.na(lag(department != "UCH EMERGENCY DEPT")) ~ TRUE,
#                                  ED_row == 1 & lag(department != "UCH EMERGENCY DEPT") ~ TRUE,
#                                   ED_row == 1 & lag(department == "UCH EMERGENCY DEPT") ~ arrival_row,
#                                   ED_row != 1  ~ arrival_row))
# 
# # create dataset of csns with multiple visits to ED
# multiple_ED_bed_moves <- ED_bed_moves_raw %>% filter(arrival_row) %>% 
#   group_by(csn) %>% summarise(tot = n()) %>% 
#   filter(tot > 1) %>% select(csn) %>% left_join(ED_bed_moves_raw) %>% 
#   arrange(mrn, csn, admission)
# 
# # count the number of these
# num_multiple_ED_visits <- multiple_ED_bed_moves %>% 
#   select(csn) %>% distinct() %>%  n_distinct()
# print(paste("Number of multiple visits to process: ",num_multiple_ED_visits))
# 
# # save old csn for joining with other tables later
# ED_bed_moves_raw <- ED_bed_moves_raw %>% 
#   mutate(csn_old = csn)
# 
# # while this number is greater than 0, calculate a new csn, arrival and
# # discharge time for the csns with multiple visits to ED 
# # and update ED_bed_moves_raw with these
# 
# #i <- 0
# while (num_multiple_ED_visits >0 ) {
# #  i <- i + 1
#   mult_visit_csn <- multiple_ED_bed_moves %>% filter(arrival_row) %>% 
#     group_by(csn) %>% summarise(latest_ED_arrival = max(admission)) %>% 
#    select(csn, latest_ED_arrival) %>% 
#     mutate(csn_new = as.character(as.numeric(csn)*10)) %>% 
#     select(csn, csn_new, latest_ED_arrival)
#   
#   multiple_ED_bed_moves <- multiple_ED_bed_moves %>% 
#     left_join(mult_visit_csn, by = "csn") %>% 
#     mutate(csn_new = case_when(!is.na(latest_ED_arrival) & admission >= latest_ED_arrival ~ csn_new,
#                            TRUE ~ csn),
#            arrival_dttm_new = case_when(!is.na(latest_ED_arrival) & admission >= latest_ED_arrival ~ latest_ED_arrival,
#                            TRUE ~ arrival_dttm))
#   
#   multiple_ED_bed_moves <- multiple_ED_bed_moves %>% ungroup() %>% 
#     group_by(mrn, csn_new, arrival_dttm_new) %>% 
#       mutate(discharge_dttm_new = max(discharge))
# 
#   ED_bed_moves_raw <- ED_bed_moves_raw %>% left_join(multiple_ED_bed_moves) %>% 
#     mutate(csn = case_when(!is.na(csn_new) ~ csn_new,
#                            TRUE ~ csn),
#            arrival_dttm = case_when(!is.na(arrival_dttm_new) ~ arrival_dttm_new,
#                                     TRUE ~ arrival_dttm),
#            discharge_dttm = case_when(!is.na(discharge_dttm_new) ~ discharge_dttm_new,
#                                       TRUE ~ discharge_dttm)) %>% 
#     select(-csn_new, - arrival_dttm_new, -discharge_dttm_new, -latest_ED_arrival)
#   
#   multiple_ED_bed_moves <- ED_bed_moves_raw %>% filter(arrival_row) %>% 
#     group_by(csn) %>% summarise(tot = n()) %>% 
#     filter(tot > 1) %>% select(csn) %>% left_join(ED_bed_moves_raw) %>% 
#     arrange(mrn, csn, admission)
#   
#   num_multiple_ED_visits <- multiple_ED_bed_moves %>% 
#     select(csn) %>% distinct() %>%  n_distinct()
#   
#   print(paste("Number of multiple visits to process: ",num_multiple_ED_visits))
#            
# }
# # clean up - don't worry if you get warning messages here
# rm(mult_visit_csn, multiple_ED_bed_moves)
# 
# # get rid of any csn which no longer involve ED
# # NB - on 26.8.20 I corrected this to include csns with only one ED row
# ED_bed_moves_raw <- ED_bed_moves_raw %>% group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
#   mutate(num_ed_rows = sum(ED_row == 1)) %>% filter(num_ed_rows > 0)

print(Sys.time() - timer)
print("Get OTF location")
timer <- Sys.time()

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

# # Calculating discharge dttm
# # =====================
# 
# print(Sys.time() - timer)
# print("Calculating discharge dttm(1)")
# timer <- Sys.time()
# 
# # OTF is not considered to be a ED location
# # considering how to handle OTF rows, the following shows how many rows do not have OTF as last ED location
# # 
# # ED_bed_moves_raw %>% filter(room == "OTF") %>% select(csn, room) %>% 
# #   left_join(ED_bed_moves_raw %>% 
# #               filter(department == "UCH EMERGENCY DEPT") %>% 
# #               group_by(csn) %>% 
# #               filter(discharge == max(discharge)) %>%
# #               select(csn, last_room = room)) %>% 
# #   group_by(last_room != "OTF") %>% 
# #   summarise(n())
# 
# # calculate ED discharge time; this will not be correct for OTF as last row
# 
# 
# 
# ED_bed_moves_raw <- ED_bed_moves_raw %>%
#   group_by(mrn, csn, arrival_dttm, discharge_dttm, ED_row) %>% 
#   mutate(ED_discharge_dttm = 
#            case_when(ED_row == 1 ~ max(discharge))) %>% 
#   ungroup() %>% 
#   group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
#   mutate(ED_discharge_dttm = 
#            case_when(is.na(ED_discharge_dttm) ~ min(ED_discharge_dttm, na.rm = TRUE),
#                      TRUE ~ ED_discharge_dttm)) 
# # 
# # ED_bed_moves_raw <- ED_bed_moves_raw %>%
# #   mutate(ED_duration = difftime(ED_discharge_dttm, arrival_dttm, units = "hours"))
# 
# 
# # do this again but filter out OTF rows to get a ED duration excluding these
# # note this may not be wholly correct for the csns which involve OTF and then a return to ED
# 
# print(Sys.time() - timer)
# print("Calculating discharge dttm(2)")
# timer <- Sys.time()
# 
# 
# ED_bed_moves_raw <- ED_bed_moves_raw %>%
#   group_by(mrn, csn, arrival_dttm, discharge_dttm, ED_row, OTF_row) %>%
#   mutate(ED_discharge_dttm_excl_OTF = 
#            case_when(OTF_row == 0 & ED_row == 1 ~ max(discharge),
#                      OTF_row == 1 & ED_row == 1 ~ NA_POSIXct_,
#                      OTF_row == 0 & ED_row == 0 ~ NA_POSIXct_,
#                      OTF_row == 1 & ED_row == 1 ~ NA_POSIXct_)) %>% 
#   ungroup() %>% 
#   group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
#   mutate(ED_discharge_dttm_excl_OTF = 
#            case_when(is.na(ED_discharge_dttm_excl_OTF)~ min(ED_discharge_dttm_excl_OTF, na.rm = TRUE),
#                      TRUE ~ ED_discharge_dttm_excl_OTF)) 
# # ED_bed_moves_raw <- ED_bed_moves_raw %>%
# #   mutate(ED_duration_excl_OTF =
# #            difftime(ED_discharge_dttm_excl_OTF, arrival_dttm, units = "hours"))
# 
# # create final version of ED_duration - select the pre OTF duration when there is an OTF row last, but not otherwise
# 
# print(Sys.time() - timer)
# print("Calculating discharge dttm(3)")
# timer <- Sys.time()
# 
# ED_bed_moves_raw <- ED_bed_moves_raw %>% 
#   mutate(
#     # ED_duration_final =
#     #        case_when(sum(OTF_row) > 0 ~ max(ED_duration_excl_OTF, na.rm = TRUE),
#     #                  TRUE ~ max(ED_duration, na.rm = TRUE)),
#          ED_discharge_dttm_final =
#          case_when(sum(OTF_row) > 0 ~ max(ED_discharge_dttm_excl_OTF, na.rm = TRUE),
#                      TRUE ~ max(ED_discharge_dttm, na.rm = TRUE)))
# 
# # use row durations to calc the row that denotes admission beyond ED
# # in most cases, this will be the OTF row
# 
# print(Sys.time() - timer)
# print("Getting admission row")
# timer <- Sys.time()
# 
# # ED_bed_moves_raw <- ED_bed_moves_raw %>%
# #   mutate(admission_row = case_when(admission >= ED_discharge_dttm_excl_OTF &
# #                                    lag(admission) < ED_discharge_dttm_excl_OTF ~ TRUE,
# #                                  TRUE ~ FALSE))
# 
# ED_bed_moves_raw <- ED_bed_moves_raw %>%
#   mutate(admission_row = case_when(
#     ED_row != 1 & lag(OTF_row) != 1 & admission == ED_discharge_dttm ~ TRUE,
#     ED_row == 1 & OTF_row == 1 & admission == ED_discharge_dttm_excl_OTF ~ TRUE,
#     TRUE ~ FALSE
#     )
#   )
# 

# Simplify room and dept names
# ============================

# print(Sys.time() - timer)
# print("Calculating room2")
# timer <- Sys.time()
# 
# # create room2 (see wiki for more information)
# ED_bed_moves_raw <- ED_bed_moves_raw %>% 
#   mutate(room2 = case_when(is.na(room) & hl7_location != "ED^null^null" ~
#                              split_location(hl7_location, 2),
#                            TRUE ~ room))
# 
# print(Sys.time() - timer)
# print("Calculating room2a")
# timer <- Sys.time()
# 
# 
# # create room2a (see wiki for more information)
# ED_bed_moves_raw <- ED_bed_moves_raw %>% 
#   mutate(room2a = case_when(is.na(room2) & hl7_location == "ED^null^null" & arrival_row ~ "Arrived",
#                            is.na(room2) & hl7_location == "ED^null^null" & !arrival_row ~ "Waiting",
#                            TRUE ~ room2))
# 
# print(Sys.time() - timer)
# print("Calculating dept2")
# timer <- Sys.time()
# 
# # create dept2 (see wiki for more information)
# ED_bed_moves_raw <- ED_bed_moves_raw %>% 
#   mutate(dept2 = clean_wardnames7(department, include_CDU))

# create room3 (see wiki for more information)

print(Sys.time() - timer)
print("Calculating room3")
timer <- Sys.time()

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(room3 = clean_room_names(department, room))

# use e to see which room names the function clean_room_names has grouped: 
# e <- ED_bed_moves_raw %>% filter(dept2 == "ED") %>% 
#   group_by(dept2, room2, room3) %>% summarise(total = n()) 

# to see mapping from room to room3 (note room3 has more values because we used the HL7 field)
# g <- ED_bed_moves_raw %>% 
#   filter(department == "UCH EMERGENCY DEPT") %>% 
#   group_by(room, room3) %>% 
#   summarise(tot = n()) %>% 
#   select(room, room3, tot) %>%
#   pivot_wider(names_from = room3, values_from = tot)

print(Sys.time() - timer)
print("Calculating dept3")
timer <- Sys.time()

# # create dept3 (see wiki for more information)
ED_bed_moves_raw <- ED_bed_moves_raw %>%
  mutate(dept3 = ifelse(admission >= last_ED_discharge_time, "Admitted", "Still in ED" ))

print(Sys.time() - timer)
print("Calculating room4")
timer <- Sys.time()

# Create room4 (see wiki for more information)
ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  mutate(room4 = group_room_names(room3))


# ED_bed_moves_raw <- ED_bed_moves_raw %>% 
#   # create new ED_row flag where last row is OTF
#   mutate(ED_row_excl_OTF = case_when(ED_row == 1 & discharge > ED_discharge_dttm_final ~ 0,
#                             TRUE ~ ED_row))

# print(Sys.time() - timer)
# print("Calculating room6")
# timer <- Sys.time()
# 
# # create room6 (see wiki for more information)
# ED_bed_moves_raw <- ED_bed_moves_raw %>% 
#   group_by(mrn, csn, arrival_dttm, discharge_dttm) %>% 
#   # udpate OTF to either side location when these are the same
#   mutate(room6 = case_when(OTF_row == 1 & ED_row_excl_OTF ==1 & lead(room5) == lag(room5) ~ lead(room5),
#                                 TRUE ~ room5)) 

# print(Sys.time() - timer)
# print("Calculating room7")
# timer <- Sys.time()
# 
# # create room7 (see wiki for more information)
# ED_bed_moves_raw <-ED_bed_moves_raw %>% group_by(mrn, csn, arrival_dttm, discharge_dttm, room4) %>% 
#   mutate(sum_triage = sum(room4 == "TRIAGE")) %>% 
#   mutate(room7 = case_when(sum_triage > 1 & room4 == "TRIAGE" & admission != min(admission) ~ paste0(room4, " Return"),
#                            TRUE ~ room4)) %>% 
#   select(-sum_triage) %>% ungroup() %>% 
#   group_by(mrn, csn, arrival_dttm, discharge_dttm)

# Final data cleaning
# ===================

print(Sys.time() - timer)
print("Final data cleaning")
timer <- Sys.time()

# # look at outliers
# ggplot(ED_bed_moves_raw %>% filter(ED_row == 1), # one clear outlier
#        aes(x=1, y = duration_row)) +
#         geom_boxplot()
# 
# # without that one, majority of long durations rows are OTF (off the floor)
# ggplot(ED_bed_moves_raw %>% filter(ED_row == 1, duration_row < 400), aes(x=room3, y = duration_row)) +
#   geom_boxplot() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))
# 
# # remove one outlier
# ED_bed_moves_raw <- ED_bed_moves_raw %>% filter(duration_row < 400) %>% 
#   arrange(mrn, csn, admission)



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


# look at csns with duration of more than one day:
long_ED_csn <-  ED_bed_moves_raw %>% ungroup() %>% 
  filter(ED_row == 1, OTF_row !=1, 
         duration_row > 24) %>% select(csn) %>% distinct()

# to see the number of csns identified:
print("number of long ED csns:")
print(long_ED_csn %>% select(csn) %>% n_distinct())
#print(long_ED_csn$csn)


# # several have weird arrival rows
# odd_arrival_rows <- ED_bed_moves_raw %>% 
#   filter(arrival_row,
#          location_string %in% c("ED^UCHED WR POOL^WR")) %>%
#   select(csn) %>% distinct()


# # create dataset of excluded csns
# excluded_csns <- admission_later_csns %>% 
#   bind_rows(elsewhere_to_ED_csn %>% select(mrn, csn, csn_old))  %>% 
#   bind_rows(long_ED_csn %>% select(mrn, csn, csn_old)) %>% 
# #  bind_rows(odd_arrival_rows %>% select(mrn, csn, csn_old)) %>% 
# #  bind_rows(duplicate_admission_csns %>% select(mrn, csn, csn_old = csn)) %>% 
#   distinct() %>% 
#   mutate(reason = case_when(csn %in% admission_later_csns$csn ~ "Admission later than discharge",
#                             csn %in% elsewhere_to_ED_csn$csn & csn %in% long_ED_csn$csn ~ "Elsewhere to ED and long ED",
#                             csn %in% elsewhere_to_ED_csn$csn ~ "Elsewhere to ED",
#                             csn %in% long_ED_csn$csn ~ "Long ED",
#                             csn %in% duplicate_admission_csns$csn ~ "Duplicate admission dttm",
#                             TRUE ~ "Odd arrival row"))
# 

# # a few CSNs have multiple iterations through OTF and Waiting room - use this to check
# ED_bed_moves_raw %>% filter(room5 %in% c("DIAGNOSTICS", "WAITING ROOM")) %>% select(csn) %>% distinct()
# 
# ED_bed_moves <- ED_bed_moves %>% 
#   group_by(csn) %>%
#   mutate(prev_location = lag(room4_new)) %>% 
#   mutate(room4_new = case_when(room4_new %in% c("DIAGNOSTICS", "WAITING ROOM") ~ prev_location,
#                                TRUE ~ room4_new))


# create clean version of ED bed moves

ED_bed_moves_raw <- ED_bed_moves_raw %>% 
  anti_join(long_ED_csn) %>% anti_join(elsewhere_to_ED_csn) 

ED_csn_summ_raw <- ED_csn_summ_raw %>% 
  anti_join(long_ED_csn) %>% anti_join(elsewhere_to_ED_csn) 

# Create summary dataset
# =========================

# print("Creating summary dataset")
# 
# # create summary (one row per csn)
# ED_csn_summ <- ED_bed_moves %>% 
#   group_by(mrn, csn, csn_old, arrival_dttm, discharge_dttm, 
#            ED_discharge_dttm, ED_discharge_dttm_final,
#            ED_discharge_dttm_excl_OTF, 
#            # ED_duration_excl_OTF,
#            # ED_duration, ED_duration_final
#            ) %>% 
#   summarise(num_ED_rows = sum(ED_row),
#             num_ED_row_excl_OTF = sum(ED_row_excl_OTF)) %>% ungroup() %>% 
#   filter(num_ED_rows > 0) 
#   
#   # create breach detail
# ED_csn_summ <- ED_csn_summ %>%  
#   mutate(ED_duration_final = difftime(ED_discharge_dttm_final, arrival_dttm, units = "hours"),
#          seen4hrs = ifelse(ED_duration_final < hours(4), "Seen in 4 hours", "Breach"))  


# calculate whether admitted
ED_csn_summ <- ED_csn_summ_raw %>%  
  mutate(adm = if_else(patient_class == "INPATIENT", FALSE, TRUE))


ED_csn_summ <- ED_csn_summ %>%  
  mutate(epoch = case_when(presentation_time < '2020-03-31' ~ "Pre_Covid",
                           presentation_time < '2020-05-31' ~ 'Surge1',
                           TRUE ~ 'Post_Surge1')) %>% 
  mutate(epoch = factor(epoch, levels = c("Pre_Covid", "Surge1", "Post_Surge1")))

# reorder columns
ED_bed_moves <- ED_bed_moves_raw %>% 
  select(-presentation_time, -admission_time,  -discharge_time) 




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


