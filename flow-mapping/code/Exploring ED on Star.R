
# clean room data
clean_room_names <- function(x) {
  x = gsub("UCHED ","",x)
  x = gsub(" BY[0-9]{2}", "", x)
  x = gsub("BY[0-9]{2} ", "", x)
  x = gsub(" SR[0-9]{2}", "", x)
  x = gsub("SR[0-9]{2} ", "", x)
  x = gsub(" CB[0-9]{2}", "", x)
  if (length( grep("^T[0-9]{2}",x)) == 0) {
    x = gsub("[0-9]", "", x)
  }
  x = gsub(" $","",x)
  return(x)
}
clean_room_names <- Vectorize(clean_room_names)

# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)

ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")

# load saved bed_moves data - all

setwd("//uclcmddprafss21/Home/zelking1/Documents/EDcrowding/flow-mapping")
inFile = paste0("data-raw/bed_moves_","2020-06-15",".rda")
load(inFile)

# load saved bed_moves data - just ED

setwd("//uclcmddprafss21/Home/zelking1/Documents/EDcrowding/flow-mapping")
inFile = paste0("data-raw/ED_bed_moves_","2020-06-15",".rda")
load(inFile)

ED_bed_moves <- ED_bed_moves %>% mutate(room2 = clean_room_names(room))
ED_bed_moves <- ED_bed_moves %>% mutate(room2 = ifelse(is.na(room2), "None", room2))
ED_bed_moves <- ED_bed_moves %>% mutate(duration_row = difftime(discharge, admission, units = "hours"))
ED_bed_moves %>% filter(department == "UCH EMERGENCY DEPT") %>% group_by(room2) %>% summarise(n())

# looking at COVID locations - looks like reorg for COVID happened on 20/3
ED_bed_moves %>% filter(room2 %in% c( "COVID UTC", "NON COVID UTC", "NON COVID UTC CHAIR")) %>% summarise(min(admission_dttm))
# ED_bed_moves <- ED_bed_moves %>% mutate(covid = ifelse(admission_dttm < "2020-03-20 12:00:00", "Before", "After"))
ED_bed_moves %>% group_by(covid) %>% summarise(n())
ED_bed_moves %>% group_by(room2) %>% summarise(n())


# load ED pats
inFile = paste0("data-raw/ED_pats_","2020-06-15",".rda")
load(inFile)


# load flowsheets
# ===============




# get ED discharge time
ED_pats 
ED_discharge_time <- ED_bed_moves %>% filter(ED_row == 1) %>% group_by(encounter_id) %>% summarise(ED_discharge_dttm = max(discharge))
ED_pats <- ED_pats %>% left_join(ED_discharge_time)
rm(ED_discharge_time)


# get triage end time
ED_triage_end_time <-  ED_bed_moves %>% filter(ED_row == 1, room2 %in% c("ADULT TRIAGE", "PAEDS TRIAGE")) %>% group_by(encounter_id) %>% summarise(ED_triage_end_time = max(discharge))
ED_pats <- ED_pats %>% left_join(ED_triage_end_time)


# get vital signs for ED patients
load("~/EDcrowding/flow-mapping/data-raw/flowsheets2020-06-15.rda") #flowsheets
ED_vital_signs <- ED_pats %>% left_join(flowsheets)
ED_vital_signs <- ED_vital_signs %>% mutate(taken_in_ED = ifelse(flowsheet_datetime < ED_discharge_dttm, "ED", "Other"))
ED_vital_signs <- ED_vital_signs %>% mutate(taken_in_triage = ifelse(flowsheet_datetime < ED_triage_end_time, "Triage", "Other"))

vitals_in_ED <- ED_vital_signs %>% filter(!is.na(mapped_name), !is.na(taken_in_ED)) %>%  
  group_by(mapped_name, taken_in_ED)  %>% summarise(tot = n()) %>%
  pivot_wider(names_from = taken_in_ED, values_from = tot)

vitals_in_triage <- ED_vital_signs %>% filter(!is.na(mapped_name), !is.na(taken_in_triage)) %>%  
  group_by(mapped_name, taken_in_triage)  %>% summarise(tot = n()) %>%
  pivot_wider(names_from = taken_in_triage, values_from = tot)

# Plot of durations in ED
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