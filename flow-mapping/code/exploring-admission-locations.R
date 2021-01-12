
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
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-01-12.rda")
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

moves[visited_T09 := sum(department %in% c("T09C", "T09N", "T09S"), na.rm = TRUE) > 0, by = csn]

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


summ[,time_to_admit_from_ED_pres := as.numeric(difftime(first_outside_proper_admission, 
                                                       presentation_time, 
                                                       units = "hours"))]


chart_data = summ[first_dept == "ED" & time_to_admit_from_ED_pres < 48]

# histogram all time to admit
chart_data %>% ggplot(aes(x = time_to_admit_from_ED_pres)) + 
  geom_histogram(binwidth = 1)

# histogram time to admit for CDU
visited_CDU = moves[(visited_CDU)]
merge(moves[(visited_CDU),unique(csn)], chart_data) %>% ggplot(aes(x = time_to_admit_from_ED_pres)) + 
  geom_histogram(binwidth = 1)



