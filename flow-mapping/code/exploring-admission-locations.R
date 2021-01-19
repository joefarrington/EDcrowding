
# About this file ---------------------------------------------------------

# Explores implications of the admission classification

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)

# Create function  --------------------------------------------------------

rpt <- function(dataframe) {
  print(dataframe %>% select(csn) %>% n_distinct())
}


# Load data ---------------------------------------------------------------
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_2021-01-12.rda")
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-01-12.rda")
rpt(moves)



# Reproducing previous code to produce location frequency chart by month ------------------------------------

loc_summ <- moves[, .N, by = location]

room_summ <- as_tibble(loc_summ[N>205, location]) %>% 
  rename(location = value) %>% inner_join(moves) %>% 
  left_join(ED_csn_summ %>% select(csn, presentation_time)) %>% 
  mutate(year_month = paste0(year(presentation_time), month(presentation_time))) %>% 
  group_by(location, year_month) %>% summarise(tot = n()) 

  

room_summ <- room_summ %>% mutate(year_month = factor(year_month, levels = 
                                                        c(  "20194",
                                                            "20195",
                                                            "20196",
                                                            "20197",
                                                            "20198",
                                                            "20199",
                                                            "201910",
                                                            "201911",
                                                            "201912",
                                                            "20201",
                                                            "20202",
                                                            "20203",
                                                            "20204",
                                                            "20205",
                                                            "20206",
                                                            "20207",
                                                            "20208",
                                                            "20209",
                                                            "202010",
                                                            "202011",
                                                            "202012"
                                                        ), labels = 
                                                        c(  "201904",
                                                            "201905",
                                                            "201906",
                                                            "201907",
                                                            "201908",
                                                            "201909",
                                                            "201910",
                                                            "201911",
                                                            "201912",
                                                            "202001",
                                                            "202002",
                                                            "202003",
                                                            "202004",
                                                            "202005",
                                                            "202006",
                                                            "202007",
                                                            "202008",
                                                            "202009",
                                                            "202010",
                                                            "202011",
                                                            "202012"
                                                        )))


png("EDcrowding/flow-mapping/media/Use of locations by month to Dec 20.png", width = 1200, height = 1200)


room_summ %>% filter(!location %in% c("BSNQ", "CCAC", "DFNQ", "NHNNTHR", "SINQ", "SPEV", "W01THR", "WSSS", "WSU3", "LVNB", "Waiting")) %>% 
  ggplot(aes(x = year_month, y = tot)) + geom_bar(stat = "identity") +theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(location~., switch = "y", scales = "free")+
  theme(strip.text.y.left = element_text(angle = 0)) +
  
  labs(title = "Use of locations by month since beginning of Epic", x = "Month", y = "Total patient numbers") +
  theme(        axis.text.y=element_blank(),
                axis.ticks.y=element_blank()
  ) 
dev.off()

# by quarter

room_summQ <-as_tibble(loc_summ[N>205, location]) %>% 
  rename(location = value) %>% inner_join(moves) %>% 
  left_join(ED_csn_summ %>% select(csn, presentation_time)) %>% 
  mutate(year_Q = paste0(year(presentation_time), "Q", quarter(presentation_time))) %>% 
  group_by(location, year_Q) %>% summarise(tot = n()) 





png("EDcrowding/flow-mapping/media/Use of locations by month.png", width = 1200, height = 1200)


room_summQ %>% filter(!location %in% c("BSNQ", "CCAC", "DFNQ", "NHNNTHR", "SINQ", "SPEV", "W01THR", "WSSS", "WSU3", "LVNB", "Waiting")) %>% 
  ggplot(aes(x = year_Q, y = tot)) + geom_bar(stat = "identity") +  theme_classic() +theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(location~., switch = "y")+
  theme(strip.text.y.left = element_text(angle = 0)) +
  
  labs(title = "Use of ED locations by quarter since beginning of Epic", x = "Month", y = "Total patient numbers") +
  theme(        axis.text.y=element_blank(),
                axis.ticks.y=element_blank()
  ) 
dev.off()


# Checking on EAU dropping out of use -------------------------------------

moves[, visited_T09 := sum(department %in% c("T09C", "T09N", "T09S"), na.rm = TRUE) > 0, by = csn]

# Save summary as data table -------------------------------------------------

# get data table verison of ED_Csn_summ
summ = data.table(ED_csn_summ)
setkey(summ, csn)
summ = summ[unique(moves[,.(csn, first_outside_ED_admission, first_outside_proper_admission, last_ED_discharge, last_inside_discharge)])]

# get first proper location to save to summ
m = moves[admission == first_outside_proper_admission, .(csn, location)] 
setnames(m, "location", "first_proper_location")
summ = merge(summ, m, all.x = TRUE)

# get first department (note that this has to be a separate join to pick up all rows)
m1 = unique(moves[, .(csn, first_dept, num_ED_exit)])
summ = merge(summ, m1, all.x = TRUE)

# NB some csns have no first proper location because, although they are inpatients, 
# they originated outside ED and didn't go anywhere after ED
rpt(summ[is.na(first_proper_location) & adm %in% c("direct_adm", "direct_adm")]) == 
  rpt(summ[first_dept != "ED" & adm %in% c("direct_adm", "direct_adm") & num_ED_exit == 0])




# Looking at first proper admission location ------------------------------

# all proper admissions
moves[admission == first_outside_proper_admission, .N, by = location] %>% 
  ggplot(aes(x = first_proper_location, y = N)) + geom_bar(stat = "identity")

# including only admissions after observation units
moves[visited_obs & admission == first_outside_proper_admission, .N, by = location] %>% 
  ggplot(aes(x = location, y = N)) + geom_bar(stat = "identity")

# including only admissions after same day treatment units
moves[visited_same_day & admission == first_outside_proper_admission, .N, by = location] %>% 
  ggplot(aes(x = location, y = N)) + geom_bar(stat = "identity")


# calculate first at obs or same day units and add to summ
moves[,SAA := if_else(location == "SAA", 1,0)]
moves[,SDEC := if_else(location == "SDEC", 1,0)]

moves[,visited_SAA := sum(SAA > 0, na.rm = TRUE) > 0, by = csn]
moves[,visited_SDEC := sum(SDEC > 0, na.rm = TRUE) > 0, by = csn]

moves[SAA == 1, first_SAA_admission := if_else(admission == min(admission, na.rm = TRUE), TRUE, FALSE), by = csn]
first_SAA = unique(moves[(first_SAA_admission), list(csn, admission)])
setnames(first_SAA, "admission", "first_SAA")
summ = merge(summ, first_SAA, all.x = TRUE)

moves[SDEC == 1, first_SDEC_admission := if_else(admission == min(admission, na.rm = TRUE), TRUE, FALSE), by = csn]
first_SDEC = unique(moves[(first_SDEC_admission), list(csn, admission)])
setnames(first_SDEC, "admission", "first_SDEC")
summ = merge(summ, first_SDEC, all.x = TRUE)

moves[department == "UCHT00CDU", first_CDU_admission := if_else(admission == min(admission, na.rm = TRUE), TRUE, FALSE), by = csn]
first_CDU = unique(moves[(first_CDU_admission), list(csn, admission)])
setnames(first_CDU, "admission", "first_CDU")
summ = merge(summ, first_CDU, all.x = TRUE)

moves[department %in% c("T01ECU", "AECU"), first_EDU_admission := if_else(admission == min(admission, na.rm = TRUE), TRUE, FALSE), by = csn]
first_EDU = unique(moves[(first_EDU_admission), list(csn, admission)])
setnames(first_EDU, "admission", "first_EDU")
summ = merge(summ, first_EDU, all.x = TRUE)

moves[obs == 1, first_obs_admission := if_else(admission == min(admission, na.rm = TRUE), TRUE, FALSE), by = csn]
first_obs = unique(moves[(first_obs_admission), list(csn, admission)])
setnames(first_obs, "admission", "first_obs")
summ = merge(summ, first_obs, all.x = TRUE)

moves[same_day == 1, first_same_day_admission := if_else(admission == min(admission, na.rm = TRUE), TRUE, FALSE), by = csn]
first_same_day = unique(moves[(first_same_day_admission), list(csn, admission)])
setnames(first_same_day, "admission", "first_same_day")
summ = merge(summ, first_same_day, all.x = TRUE)


# calculate times to admission to final location 
summ[,time_to_admit_from_ED_pres := as.numeric(difftime(first_outside_proper_admission, 
                                                        presentation_time, 
                                                        units = "hours"))]
summ[,time_to_admit_from_first_SAA := as.numeric(difftime(first_outside_proper_admission,
                                                          first_SAA, 
                                                        units = "hours"))]
summ[,time_to_admit_from_first_SDEC := as.numeric(difftime(first_outside_proper_admission,
                                                           first_SDEC, 
                                                          units = "hours"))]
summ[,time_to_admit_from_first_CDU := as.numeric(difftime(first_outside_proper_admission,
                                                          first_CDU, 
                                                          units = "hours"))]
summ[,time_to_admit_from_first_EDU := as.numeric(difftime(first_outside_proper_admission, 
                                                          first_EDU, 
                                                          units = "hours"))]
summ[,time_to_admit_from_first_obs := as.numeric(difftime(first_outside_proper_admission, 
                                                          first_obs, 
                                                          units = "hours"))]
summ[,time_to_admit_from_first_same_day := as.numeric(difftime(first_outside_proper_admission, 
                                                               first_same_day, 
                                                          units = "hours"))]

# select only visits that began in ED - this gets 25,501 rows including all direct admissions (not via obs units)
chart_data = as_tibble(summ[first_dept == "ED" & time_to_admit_from_ED_pres < 48]) %>% 
  select(csn, adm, starts_with("time_to_admit_from")) 

# proportion admitted via each route
prop_SAA = chart_data %>% filter(!is.na(time_to_admit_from_first_SAA)) %>% n_distinct() / length(moves[(visited_SAA), unique(csn)])
prop_SDEC = chart_data %>% filter(!is.na(time_to_admit_from_first_SDEC)) %>% n_distinct() / length(moves[(visited_SDEC), unique(csn)])
prop_CDU = chart_data %>% filter(!is.na(time_to_admit_from_first_CDU)) %>% n_distinct() / length(moves[(visited_CDU), unique(csn)])
prop_EDU = chart_data %>% filter(!is.na(time_to_admit_from_first_EDU)) %>% n_distinct() / length(moves[(visited_EDU), unique(csn)])
prop_obs = chart_data %>% filter(!is.na(time_to_admit_from_first_obs)) %>% n_distinct() / length(moves[(visited_obs), unique(csn)])
prop_same_day = chart_data %>% filter(!is.na(time_to_admit_from_first_same_day)) %>% n_distinct() / length(moves[(visited_same_day), unique(csn)])
  

chart_data_long = chart_data %>% 
  pivot_longer(starts_with("time_to_admit_from"), names_to = "time_to_admit_from") %>% 
  mutate(time_to_admit_from = gsub("time_to_admit_from_first_", "first_arrival_at_", time_to_admit_from))  %>% 
  mutate(time_to_admit_from = gsub("time_to_admit_from_ED_pres", "presentation_at_ED", time_to_admit_from))


# Draw charts -------------------------------------------------------------

subtitle_text = "First ward excludes CDU, AECU (also known as EDU), T01ECU and any ED unit"

# histogram all visits, time to admit from ED presentation
chart_data_long %>% filter(time_to_admit_from == "presentation_at_ED") %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(binwidth = 1, col = "white", fill = "#00BFC4") +
  scale_x_continuous(breaks = seq(0,48,2)) +
  labs(x = "Hours from presentation at ED to arrival at first ward",
       y = "Number of visits",
       title = "Distribution of time from ED presentation to admission at first ward (histogram)",
       subtitle = subtitle_text)

# boxplot all visits, time to admit from ED presentation
chart_data_long %>% filter(time_to_admit_from == "presentation_at_ED") %>% 
  ggplot(aes(x = value)) + 
  geom_boxplot() +
  scale_x_continuous(breaks = seq(0,48,2)) +
  
  labs(x = "Hours from presentation at ED to arrival at first ward",
       title = "Distribution of time from ED presentation to admission at first ward (boxplot)",
       subtitle = subtitle_text) +
  theme(axis.text.y=element_blank())


# Intermediate desination time to admit from ED presentation --------



# charts from time admitted to unit to time to final destination

chart_data %>% filter(!is.na(time_to_admit_from_first_SAA)) %>% 
  pivot_longer(starts_with("time_to_admit_from"), names_to = "time_to_admit_from") %>% 
  mutate(time_to_admit_from = gsub("time_to_admit_from_ED_pres", "presentation_at_ED", time_to_admit_from)) %>% 
  filter(time_to_admit_from == "presentation_at_ED") %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(binwidth = 1, col = "white", fill = "#00BFC4") +
  labs(x = "Hours from presentation at ED to arrival at first ward",
       y = "Number of visits",
       title = "Distribution of time from presentation at ED to admission at first ward for patients who visited SAA",
       subtitle = subtitle_text)

chart_data %>% filter(!is.na(time_to_admit_from_first_SDEC)) %>% 
  pivot_longer(starts_with("time_to_admit_from"), names_to = "time_to_admit_from") %>% 
  mutate(time_to_admit_from = gsub("time_to_admit_from_ED_pres", "presentation_at_ED", time_to_admit_from)) %>% 
  filter(time_to_admit_from == "presentation_at_ED") %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(binwidth = 1, col = "white", fill = "#00BFC4") +
  labs(x = "Hours from presentation at ED to arrival at first ward",
       y = "Number of visits",
       title = "Distribution of time from presentation at ED to admission at first ward for patients who visited SDEC",
       subtitle = subtitle_text)

chart_data %>% filter(!is.na(time_to_admit_from_first_CDU)) %>% 
  pivot_longer(starts_with("time_to_admit_from"), names_to = "time_to_admit_from") %>% 
  mutate(time_to_admit_from = gsub("time_to_admit_from_ED_pres", "presentation_at_ED", time_to_admit_from)) %>% 
  filter(time_to_admit_from == "presentation_at_ED") %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(binwidth = 1, col = "white", fill = "#00BFC4") +
  labs(x = "Hours from presentation at ED to arrival at first ward",
       y = "Number of visits",
       title = "Distribution of time from presentation at ED to admission at first ward for patients who visited CDU",
       subtitle = subtitle_text)

chart_data %>% filter(!is.na(time_to_admit_from_first_EDU)) %>% 
  pivot_longer(starts_with("time_to_admit_from"), names_to = "time_to_admit_from") %>% 
  mutate(time_to_admit_from = gsub("time_to_admit_from_ED_pres", "presentation_at_ED", time_to_admit_from)) %>% 
  filter(time_to_admit_from == "presentation_at_ED") %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(binwidth = 1, col = "white", fill = "#00BFC4") +
  labs(x = "Hours from presentation at ED to arrival at first ward",
       y = "Number of visits",
       title = "Distribution of time from presentation at ED to admission at first ward for patients who visited T01ECU",
       subtitle = subtitle_text)



# histogram by type of intermediate desination time to admit from ED presentation
chart_data_long %>% filter(time_to_admit_from == "first_arrival_at_SAA", !is.na(value), 
                           # exluding patients who previously visited a ward
                           value > 0) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(binwidth = 1, col = "white", fill = "#00BFC4") +
  labs(x = "Hours from arrival at SAA to arrival at first ward",
       y = "Number of visits",
       title = "Distribution of time from arrival at SAA to admission at first ward",
       subtitle = "Excludes one patient who was inpatient, went home and then presented at ED later as part of same hospital visit") 

chart_data_long %>% filter(time_to_admit_from == "first_arrival_at_SDEC", !is.na(value), 
                           # one SDEC patient was sent home in middle of visit 
                           value > 0) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(binwidth = 1, col = "white", fill = "#00BFC4") +
  labs(x = "Hours from arrival at SDEC to arrival at first ward",
       y = "Number of visits",
       title = "Distribution of time from arrival at SDEC to admission at first ward",
       subtitle = "Excludes one patient who was inpatient, went home and then presented at ED later as part of same hospital visit") 

chart_data_long %>% filter(time_to_admit_from == "first_arrival_at_CDU", !is.na(value), 
                           # one CDU patient was sent to EAU first 
                           value > 0) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(binwidth = 1, col = "white", fill = "#00BFC4") +
  labs(x = "Hours from arrival at CDU to arrival at first ward",
       y = "Number of visits",
       title = "Distribution of time from arrival at CDU to admission at first ward",
       subtitle = "Excludes one patient who went to EAU before CDU") 

chart_data_long %>% filter(time_to_admit_from == "first_arrival_at_EDU", !is.na(value), 
                           # many T01ECU patients are sent here after T01
                           value > 0) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(binwidth = 1, col = "white", fill = "#00BFC4") +
  labs(x = "Hours from arrival at EDU to arrival at first ward",
       y = "Number of visits",
       title = "Distribution of time from arrival at T01ECU to admission at first ward",
       subtitle = "Excludes 34 patients (approx 1/3) who were admitted to a ward before arrival at T01ECU") 

rpt(chart_data_long %>% filter(time_to_admit_from == "first_arrival_at_EDU", !is.na(value), 
                           value < 0))

