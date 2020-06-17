# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)

# hl7_location seems to have more complete info than room and bed for some rows
# split location
split_location <- function(hl7_location, n) {
  s <- unlist(strsplit(hl7_location, "\\^"))
  return(s[n])
}
split_location <- Vectorize(split_location)



# clean room data
clean_room_names <- function(x) {
  # x = gsub("UCHED ","",x)
  # x = gsub(" BY[0-9]{2}", "", x)
  # x = gsub("BY[0-9]{2} ", "", x)
  # x = gsub(" SR[0-9]{2}", "", x)
  # x = gsub("SR[0-9]{2} ", "", x)
  # x = gsub(" CB[0-9]{2}", "", x)
  # if (length( grep("^T[0-9]{2}",x)) == 0) {
  #   x = gsub("[0-9]", "", x)
  # }
  x = gsub("CHAIR [0-9]{2}", "CHAIR", x)
  x = gsub("[0-9]{2}", "BED", x)
  x = gsub("MAJ-CHAIR","MAJORS CHAIR",x)
  x = gsub("RAT-CHAIR","RAT CHAIR",x)
  x = gsub("UCHED RATBED","RAT BED",x)
  x = gsub("^UTC [A-z]+ ROOM","UTC O/P/T ROOM",x)
  x = gsub(" $","",x)
  return(x)
}
clean_room_names <- Vectorize(clean_room_names)

# Load bed_move data
# ==================

ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")

## Either get bed moves from Star

# First retrieve bed_moves

sqlQuery <- "SELECT *
  FROM star.bed_moves bm"
sqlQuery <- gsub('\n','',sqlQuery)

bed_moves <- as_tibble(dbGetQuery(ctn, sqlQuery))

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
  mutate(admission_dttm = min(admission), discharge_dttm = max(discharge)) %>% 
  arrange(mrn, csn, admission)

# identify ED rows
bed_moves <- bed_moves %>% group_by(mrn, csn, encounter_id) %>% 
  mutate(ED_row = ifelse(department == "UCH EMERGENCY DEPT", 1, 0))

# save data for future loading
setwd("//uclcmddprafss21/Home/zelking1/Documents/EDcrowding/flow-mapping")
outFile = paste0("/EDcrowding/flow-mapping/data-raw/bed_moves_",today(),".rda")
save(bed_moves, file = outFile)
rm(outFile)

## Or if data already saved
inFile = paste0("EDcrowding/flow-mapping/data-raw/bed_moves_","2020-06-15",".rda")
load(inFile)
rm(inFile)

bed_moves <- bed_moves %>% mutate(admission_row = ifelse(admission == admission_dttm, TRUE, FALSE))
bed_moves <- bed_moves %>% group_by(mrn, csn, encounter_id) %>% 
  mutate(ED_row = ifelse(department == "UCH EMERGENCY DEPT", 1, 0))

ED_bed_moves <- ED_bed_moves %>% mutate(duration_row = difftime(discharge, admission, units = "hours"))


# Select only encounters involving ED
# ===================================

# EITHER generate from bed_moves
# Create summary of all encounters  
csn_summ <- bed_moves %>% group_by(mrn, csn, encounter_id, admission_dttm, discharge_dttm) %>% 
  summarise(duration = difftime(max(discharge),min(admission), units = "hours"),
            nUm_ED_rows = sum(ED_row)) %>% ungroup()

# denote whether encounter was before or after covid changes
csn_summ <- csn_summ %>% mutate(covid = ifelse(admission_dttm < "2020-03-20 00:00:00", "Before", "After"))

# select only encounters with one or more bed_moves involving ED
ED_csn_summ <- csn_summ %>% filter(nUm_ED_rows > 0) 

# save data for future use
outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_csn_summ_",today(),".rda")
save(ED_csn_summ, file = outFile)

# OR LOAD saved data
inFile = paste0("EDcrowding/flow-mapping/data-raw/ED_csn_summ_","2020-06-17",".rda")
load(inFile)

# Data cleaning for encounters involving ED
# =========================================


# get bed_moves data for those encounters
ED_bed_moves <- left_join(ED_csn_summ, bed_moves)

# hl7_location seems to have more complete info in some cases where room and bed are NA
ED_bed_moves <- ED_bed_moves %>% mutate(room2 = case_when(is.na(room) & hl7_location != "ED^null^null" ~ split_location(hl7_location, 2),
                                                          TRUE ~ room))

# clean room names
ED_bed_moves <- ED_bed_moves %>% mutate(room3 = clean_room_names(room2))
# use this to see which room names the function clean_room_names has grouped: 
e <- ED_bed_moves %>% filter(department == "UCH EMERGENCY DEPT") %>%  group_by(room3, room) %>% summarise(total = n()) 


# Replace NAs with None for use in edge list
ED_bed_moves <- ED_bed_moves %>% mutate(room3 = ifelse(is.na(room3), "None", room3))

# looking at COVID locations - looks like first demaraction of COVID locations happened on 20/3
ED_bed_moves %>% filter(room2 %in% c( "COVID UTC", "NON COVID UTC", "NON COVID UTC CHAIR")) %>% summarise(min(admission_dttm))
# ED_bed_moves <- ED_bed_moves %>% mutate(covid = ifelse(admission_dttm < "2020-03-20 12:00:00", "Before", "After"))
ED_bed_moves %>% group_by(covid) %>% summarise(n())

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_",today(),".rda")
save(ED_bed_moves, file = outFile)
rm(outFile)

# OR load saved ED_bed_moves

inFile = paste0("EDcrowding/flow-mapping/data-raw/ED_bed_moves_","2020-06-15",".rda")
load(inFile)




# CREATE CHARTS
# =============

# Plot of durations in ED - by room
png("media/Durations in ED.png", width = 1077, height = 659)
ED_bed_moves %>% filter(department == "UCH EMERGENCY DEPT", as.numeric(duration_row) < 20, as.numeric(duration_row) >= 0, !is.na(covid)) %>%   
  ggplot( aes(x=reorder(room2,as.numeric(duration_row), .fun='median'), y=as.numeric(duration_row), fill=room2)) + 
  geom_boxplot() +
  labs(title = "Time spent by patients in ED locations before and after COVID-19 (20.3.20)",
       subtitle = "Source: Star",
       x = "",
       y = "Duration (hours)"
       # fill = "Ventilation support"
       ) +
  coord_flip() +
  theme_classic()  + 
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0)) +
  facet_wrap(~fct_rev(covid))
dev.off()

# Plot of durations in ED - by clustered room
png("media/Durations in ED.png", width = 1077, height = 659)
ED_bed_moves %>% filter(department == "UCH EMERGENCY DEPT", as.numeric(duration_row) < 20, as.numeric(duration_row) >= 0, !is.na(covid)) %>%   
  ggplot( aes(x=reorder(room3,as.numeric(duration_row), .fun='median'), y=as.numeric(duration_row), fill=room3)) + 
  geom_boxplot() +
  labs(title = "Time spent by patients in ED locations before and after COVID-19 (20.3.20)",
       subtitle = "Source: Star",
       x = "",
       y = "Duration (hours)"
       # fill = "Ventilation support"
  ) +
  coord_flip() +
  theme_classic()  + 
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0)) +
  facet_wrap(~fct_rev(covid))
dev.off()



# Plot of numbers in ED

png("media/Numbers in ED by day.png", width = 1077, height = 659)
ED_pats %>% filter(!is.na(covid)) %>%
  group_by(date(admission_dttm), covid) %>% summarise(num_pats = n()) %>%  
  ggplot(aes(x=`date(admission_dttm)`, y=num_pats, fill = fct_rev(covid))) + 
  geom_bar(stat = "identity") +
  labs(title = "Number of patients in ED by day",
       subtitle = "Source: Star",
       x = "",
       y = "Number of patients",
       fill = "Before / after Covid-19"
  ) +
  theme_classic()  + 
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0)) 
dev.off()

# Plot of day of week in ED

# below I have divided by 12 as that is approx the number of weeks between 1/1/20 and 20/3/20, and similarly between 20/3/20 and today
difftime("2020-03-22","2020-01-01", units = "weeks")
difftime("2020-06-15","2020-03-22", units = "weeks")

png("media/Mean numbers in ED by day of week.png", width = 1077, height = 659)
ED_pats %>% filter(!is.na(covid)) %>%
  group_by(covid, weekdays(admission_dttm)) %>% summarise(mean_pats = n()/12) %>%  
  ggplot(aes(x=factor(`weekdays(admission_dttm)`, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")), 
             y=mean_pats, fill = `weekdays(admission_dttm)`)) + 
  geom_bar(stat = "identity") +
  labs(title = "Mean number of patients admitted to ED by day of week, before and after COVID-19 (20.3.20)",
       subtitle = "Source: Star",
       x = "",
       y = "Number of patients"
       # fill = "Ventilation support"
  ) +
  theme_classic()  + 
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0)) +
  facet_wrap(~fct_rev(covid))
dev.off()

# Plot of day of week in ED

png("media/Mean in ED by time of day.png", width = 1077, height = 659)
ED_pats %>% filter(!is.na(covid)) %>%
  group_by(hour(admission_dttm), covid) %>% summarise(num_pats = n()) %>%  
  ggplot(aes(x=`hour(admission_dttm)`, 
             y=num_pats, fill = `hour(admission_dttm)`)) + 
  geom_bar(stat = "identity") +
  labs(title = "Mean number of patients in ED by time of admission, before and after COVID-19 (20.3.20)",
       subtitle = "Source: Star",
       x = "",
       y = "Number of patients"
       # fill = "Ventilation support"
  ) +
  theme_classic()  + 
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0)) +
  facet_wrap(~fct_rev(covid))
dev.off()