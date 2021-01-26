# About this file ---------------------------------------------------------

# Explores different ways to divide visits into timeslices, comparing
# 1. all patients with durations that exceed the timeslice
# 2. randomly selecting visits

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
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-01-25.rda")
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-01-25.rda")
rpt(moves)

## Having to delete this one row because it has a NA_dttm in last_ED_discharge_time 
summ [, adm := factor(adm, levels = c("direct_dis", "indirect_dis", "indirect_adm", "direct_adm"))]
m = unique(moves[, .(csn, last_ED_discharge)])
summ = summ[m, on = "csn"]


# Find out how many patients in each timeslice ----------------------------

summ[,ED_duration := difftime(last_ED_discharge, presentation_time, units = "mins")]
summ[,timeslice := case_when(ED_duration < 15 ~ 0,
                             ED_duration >= 15 & ED_duration < 60 ~ 15,
                             ED_duration >= 60 & ED_duration < 90 ~ 90,
                             ED_duration >= 90 & ED_duration < 120 ~ 90,
                             ED_duration >= 120 & ED_duration < 180 ~ 120,
                             ED_duration >= 180 & ED_duration < 240 ~ 180,
                             ED_duration >= 240 & ED_duration < 300 ~ 240,
                             ED_duration >= 300 & ED_duration < 360 ~ 300,
                             ED_duration >= 360 ~ 360)
                             ]

summ[ED_duration < 24*60] %>% ggplot(aes(x = ED_duration)) + geom_histogram()
summ[,.N, by = list(adm, timeslice)] %>% 
  ggplot(aes(x = as.factor(timeslice), y = N, fill = adm)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#F8766D" , "#FFB2B2","#99E4E7","#00BFC4", guide = NULL, name = NULL)) +
#  scale_x_continuous(breaks = unique(summ$timeslice)) +
  theme(panel.grid.minor = element_blank()) + 
  labs(y = "Number of visits", 
       x = "Timeslice",
       title = "Number of visits by timeslice: mutually exclusive timeslices") 
  

summ[adm %in% c("direct_adm", "indirect_adm"),.N, by = list(adm, timeslice)] %>% 
  ggplot(aes(x = as.factor(timeslice), y = N, fill = adm)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#99E4E7","#00BFC4", guide = NULL, name = NULL)) +
#  scale_x_continuous(breaks = unique(summ$timeslice)) +
  theme(panel.grid.minor = element_blank()) + 
  labs(y = "Number of visits", 
       x = "Timeslice",
       title = "Number of visits by timeslice: mutually exclusive timeslices - admitted patients only")


# to get times to admission by timeslice
chart_data = summ[adm %in% c("direct_adm", "indirect_adm") & ED_duration < 24*60, list(.N, 
                                                                          as.numeric(quantile(ED_duration - timeslice, .25)),
                                                                          as.numeric(median(ED_duration - timeslice)), 
                                                                          as.numeric(quantile(ED_duration - timeslice, .75))), by = timeslice] 
setnames(chart_data, "V2", "Q25")
setnames(chart_data, "V3", "Median")
setnames(chart_data, "V4", "Q75")

chart_data = chart_data %>% pivot_longer(N:Q75, names_to = "quartile")

chart_data %>% 
  mutate(quartile = factor(quartile, levels = c("Q25", "Median", "Q75"))) %>% 
  filter(quartile != "N") %>% 
  ggplot(aes(x = as.factor(timeslice), y = value, col = quartile, group = quartile)) + geom_point() + geom_line() +
  theme(panel.grid.minor = element_blank()) + 
  labs(y = "Elapsed time to onward admission \nafter beginning of timeslice", 
       x = "Timeslice",
       title = "Time to admission: mutually exclusive timeslices - admitted patients only") +
  theme(legend.position = "bottom")
  

# Inclusive timeslices ----------------------------------------------------

summ[,timeslice0 := 1]
summ[,timeslice15 := if_else(ED_duration > 15, 1, 0)]
summ[,timeslice60 := if_else(ED_duration > 60, 1, 0)]
summ[,timeslice120 := if_else(ED_duration > 120, 1, 0)]
summ[,timeslice180 := if_else(ED_duration > 180, 1, 0)]
summ[,timeslice240 := if_else(ED_duration > 240, 1, 0)]
summ[,timeslice300 := if_else(ED_duration > 300, 1, 0)]
summ[,timeslice360 := if_else(ED_duration > 360, 1, 0)]

summ[ED_duration < 24*60] %>% ggplot(aes(x = ED_duration)) + geom_histogram()
summ[,.N, by = list(adm, timeslice)] %>% 
  ggplot(aes(x = as.factor(timeslice), y = N, fill = adm)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#F8766D" , "#FFB2B2","#99E4E7","#00BFC4", guide = NULL, name = NULL)) +
  #  scale_x_continuous(breaks = unique(summ$timeslice)) +
  theme(panel.grid.minor = element_blank()) + 
  labs(y = "Number of visits", 
       x = "Timeslice",
       title = "Number of visits by timeslice: mutually exclusive timeslices") 


# generate long version of this for chart
summ_timeslice_long = summ[ED_duration < 24*60] %>% select(csn, adm, ED_duration, starts_with("timeslice")) %>% select(-timeslice) %>% 
  pivot_longer(c(timeslice15:timeslice360, timeslice0), names_to = "timeslice", values_to = "value") %>% 
  mutate(timeslice = gsub("timeslice", "", timeslice)) 

summ_timeslice_long = data.table(summ_timeslice_long)

# Number in each timeslice
summ_timeslice_long %>% 
  group_by(adm, timeslice) %>% summarise(N = sum(value)) %>% 
  ggplot(aes(x = factor(timeslice, levels = c(0, 15, 60, 120, 180,  240, 300, 360)), y = N, fill = adm)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#F8766D" , "#FFB2B2","#99E4E7","#00BFC4", guide = NULL, name = NULL)) +
  #  scale_x_continuous(breaks = unique(summ$timeslice)) +
  theme(panel.grid.minor = element_blank()) + 
  labs(y = "Number of visits", 
       x = "Timeslice",
       title = "Number of visits by timeslice: inclusive timeslices") 



# to get times to admission by timeslice
summ_timeslice_long[, timeslice := as.numeric(timeslice)]
chart_data2 = summ_timeslice_long[adm %in% c("direct_adm", "indirect_adm") & value > 0, list(.N, 
                                                                                       as.numeric(quantile(ED_duration - timeslice, .25)),
                                                                                       as.numeric(median(ED_duration - timeslice)), 
                                                                                       as.numeric(quantile(ED_duration - timeslice, .75))), by = timeslice] 
setnames(chart_data2, "V2", "Q25")
setnames(chart_data2, "V3", "Median")
setnames(chart_data2, "V4", "Q75")

chart_data2 = chart_data2 %>% pivot_longer(N:Q75, names_to = "quartile")

chart_data2 %>% 
  mutate(quartile = factor(quartile, levels = c("Q25", "Median", "Q75"))) %>% 
  filter(quartile != "N") %>% 
  ggplot(aes(x = as.factor(timeslice), y = value, col = quartile, group = quartile)) + geom_point() + geom_line() +
  theme(panel.grid.minor = element_blank()) + 
  labs(y = "Elapsed time to onward admission \nafter beginning of timeslice", 
       x = "Timeslice",
       title = "Time to admission: inclusive timeslices - admitted patients only") +
  theme(legend.position = "bottom")


# Generate chart of random timestamp --------------------------------------


# pick a random date
summ = summ[date(presentation_time) == '2020-06-21' ]

# pick a random time to sample
sample_time <- summ$presentation_time[20] - hours(5)
summ[presentation_time < sample_time &
       ED_discharge_earliest > sample_time] # all patients who presented before that time who were still in ED at that time


summ[presentation_time < sample_time &
       ED_discharge_earliest > sample_time] %>% pivot_longer(presentation_time:ED_discharge_earliest, names_to = "time") %>% 
  ggplot(aes(y = as.character(csn), x = value, col = time, group = csn)) + geom_point() +
  scale_x_datetime(date_breaks = "hours" , date_labels = "%H") +
  geom_line() +
  # geom_vline(xintercept = sample_time) +
  # geom_vline(xintercept = sample_time - minutes(15), linetype = "dashed") +
  # geom_vline(xintercept = sample_time - minutes(60), linetype = "dashed") +
  # geom_vline(xintercept = sample_time - minutes(120), linetype = "dashed") +
  labs(y = "csn", x = "Time of day") +
  theme(legend.position = "none")



# generate a set of random date times-----------------------------------------------------

# from here: https://stackoverflow.com/questions/14720983/efficiently-generate-a-random-sample-of-times-and-dates-between-two-dates
# use a uniform distribution to generate a random set of seconds after a start date

getRandomTimestamp <- function(N, st="2019-04-09 01:01:01 BST", et="2020-08-31 01:01:01 BST") {
  st <- as.POSIXct(st)
  et <- as.POSIXct(et)
  dt <- as.numeric(difftime(et,st,unit="sec"))
  ev <- sort(runif(N, 0, dt))
  rt <- st + ev
}



# Look at options for sampling ---------------------------------------------------

# 1. try completely random
# how many hours in total available data?
difftime("2020-12-17 01:01:01 GMT", "2019-04-08 01:01:01 BST", units = "hours")
# approx 15K hours

set.seed(42)
date_sample = getDates(7500) # one sample time per 2 hour slot 

# uneven coverage seen in chart
data.table(date_sample) %>% ggplot(aes(x = date_sample)) + geom_histogram(bins = 7500/12)

# 2. try four hour slots
date_slots = seq(as.POSIXct("2019-04-08 01:01:01 BST"), as.POSIXct("2020-12-17 01:01:01 GMT"), by = "4 hour")

set.seed(42)
date_sample2 = getRandomTimestamp(1, date_slots[1], date_slots[2])

for (i in 3:length(date_slots)) {
  date_sample2 = c(date_sample2, getRandomTimestamp(1, date_slots[i-1], date_slots[i]))
}

data.table(date_sample2) %>% ggplot(aes(x = date_sample2)) + geom_histogram()



# Generating timeslices ---------------------------------------------------

# 1. Try completely random

# filtering out any csns who spend more than 48 hours between presentation and last ED discharge
summ_temp = summ[difftime(last_ED_discharge, presentation_time, units = "hours") < 24]
rpt(summ_temp)

timeslices_sampled = data.table(summ_temp[presentation_time < date_sample[1] &
                                            last_ED_discharge > date_sample[1], csn])
timeslices_sampled[, sample_dttm := date_sample[1]]
setnames(timeslices_sampled, "V1", "csn")


for (i in 2:length(date_sample)) {
  if (i %% 1000 == 0) {
    print(paste0( "Processed ", i, " rows"))
  }
  d = data.table(summ_temp[presentation_time < date_sample[i] &
                             last_ED_discharge > date_sample[i], csn])
  d[, sample_dttm := date_sample[i]]
  setnames(d, "V1", "csn")
  
  timeslices_sampled = rbind(timeslices_sampled, d)
}

# looking at distribution of csns across timeslices when doing completely random picks
timeslices_sampled[, .N, by = csn] %>% ggplot(aes(x = N, col = "white")) + geom_histogram(binwidth = 1, alpha = .5)

# found out how many csns were missed
rpt(summ[!csn %in% timeslices_sampled$csn])

#2. Using four hour slots

timeslices_sampled2 = data.table(summ_temp[presentation_time < date_sample2[1] &
                                            last_ED_discharge > date_sample2[1], csn])
timeslices_sampled2[, sample_dttm := date_sample2[1]]
setnames(timeslices_sampled2, "V1", "csn")


for (i in 2:length(date_sample2)) {
  if (i %% 1000 == 0) {
    print(paste0( "Processed ", i, " rows"))
  }
  d = data.table(summ_temp[presentation_time < date_sample2[i] &
                             last_ED_discharge > date_sample2[i], csn])
  d[, sample_dttm := date_sample2[i]]
  setnames(d, "V1", "csn")
  
  timeslices_sampled2 = rbind(timeslices_sampled2, d)
}

# looking at distribution of csns across timeslices when doing completely random picks
timeslices_sampled2[, .N, by = csn] %>% ggplot(aes(x = N, col = "white")) + geom_histogram(binwidth = 1, alpha = .5)

# found out how many csns were missed
rpt(summ[csn %in% timeslices_sampled2$csn])


# Assign sampled csns to timeslice ----------------------------------------

summ_s <- merge(timeslices_sampled2, summ[, .(csn, adm, presentation_time, last_ED_discharge, ED_duration)], by = "csn")
summ_s[, time_since_sample_dttm := difftime(sample_dttm, presentation_time, units = "mins")]

summ_s[,timeslice := case_when(time_since_sample_dttm < 15 ~ 0,
                             time_since_sample_dttm >= 15 & time_since_sample_dttm < 60 ~ 15,
                             time_since_sample_dttm >= 60 & time_since_sample_dttm < 120 ~ 60,
                             time_since_sample_dttm >= 120 & time_since_sample_dttm < 180 ~ 120,
                             time_since_sample_dttm >= 180 & time_since_sample_dttm < 240 ~ 180,
                             time_since_sample_dttm >= 240 & time_since_sample_dttm < 300 ~ 240,
                             time_since_sample_dttm >= 300 & time_since_sample_dttm < 360 ~ 300,
                             time_since_sample_dttm >= 360 ~ 360)]



summ_s[,.N, by = list(adm, timeslice)] %>% 
  ggplot(aes(x = as.factor(timeslice), y = N, fill = adm)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#F8766D" , "#FFB2B2","#99E4E7","#00BFC4", guide = NULL, name = NULL)) +
  #  scale_x_continuous(breaks = unique(summ$timeslice)) +
  theme(panel.grid.minor = element_blank()) + 
  labs(y = "Number of visits", 
       x = "Timeslice",
       title = "Number of visits by timeslice: randomly sampled timeslices") 


summ_s[adm %in% c("direct_adm", "indirect_adm"),.N, by = list(adm, timeslice)] %>% 
  ggplot(aes(x = as.factor(timeslice), y = N, fill = adm)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#99E4E7","#00BFC4", guide = NULL, name = NULL)) +
  #  scale_x_continuous(breaks = unique(summ$timeslice)) +
  theme(panel.grid.minor = element_blank()) + 
  labs(y = "Number of visits", 
       x = "Timeslice",
       title = "Number of visits by timeslice: randomly sampled timeslices - admitted patients only")


# to get times to admission from the beginning of the timeslice
chart_data_s = summ_s[adm %in% c("direct_adm", "indirect_adm") & ED_duration < 24*60, list(.N, 
                                                                                       as.numeric(quantile(ED_duration - timeslice, .25)),
                                                                                       as.numeric(median(ED_duration - timeslice)), 
                                                                                       as.numeric(quantile(ED_duration - timeslice, .75))), by = timeslice] 
setnames(chart_data_s, "V2", "Q25")
setnames(chart_data_s, "V3", "Median")
setnames(chart_data_s, "V4", "Q75")

chart_data_s = chart_data_s %>% pivot_longer(N:Q75, names_to = "quartile")

chart_data_s %>% 
  mutate(quartile = factor(quartile, levels = c("Q25", "Median", "Q75"))) %>% 
  filter(quartile != "N") %>% 
  ggplot(aes(x = as.factor(timeslice), y = value, col = quartile, group = quartile)) + geom_point() + geom_line() +
  theme(panel.grid.minor = element_blank()) + 
  labs(y = "Elapsed time to onward admission \nafter beginning of timeslice", 
       x = "Timeslice",
       title = "Time to admission after earliest cut point of timeslice: mutually exclusive timeslices - admitted patients only") +
  theme(legend.position = "bottom")

# to get times to admission from the sampling time

chart_data_s2 = summ_s[adm %in% c("direct_adm", "indirect_adm") & ED_duration < 24*60, list(.N, 
                                                                                           as.numeric(quantile(ED_duration - time_since_sample_dttm, .25)),
                                                                                           as.numeric(median(ED_duration - time_since_sample_dttm)), 
                                                                                           as.numeric(quantile(ED_duration - time_since_sample_dttm, .75))), by = timeslice] 
setnames(chart_data_s2, "V2", "Q25")
setnames(chart_data_s2, "V3", "Median")
setnames(chart_data_s2, "V4", "Q75")

chart_data_s2 = chart_data_s2 %>% pivot_longer(N:Q75, names_to = "quartile")

chart_data_s2 %>% 
  mutate(quartile = factor(quartile, levels = c("Q25", "Median", "Q75"))) %>% 
  filter(quartile != "N") %>% 
  ggplot(aes(x = as.factor(timeslice), y = value, col = quartile, group = quartile)) + geom_point() + geom_line() +
  theme(panel.grid.minor = element_blank()) + 
  labs(y = "Elapsed time to onward admission \nafter the sampling moment", 
       x = "Timeslice",
       title = "Time to admission after sampling moment: mutually exclusive timeslices - admitted patients only") +
  theme(legend.position = "bottom")


# Analysis of timeslices versus presentation time -------------------------

# load timeslices first

timeslices <- c(0, 15, 30, 60, 90, 120, 150, 180, 210, 240, 300, 360, 24*60)

chart_data <- dm0[, .N, by = has_loc]
chart_data[, timeslice := 0]

for (i in seq(2, length(timeslices) -1, 1)) {
  name_ <- paste0("dm", timeslices[i])
  dt <- get(name_)
  cd <- dt[, .N, by = has_loc]
  cd[, timeslice := timeslices[i]]
  chart_data <- bind_rows(chart_data, cd)
}
  
chart_data %>% ggplot(aes(x = timeslice, y = N, fill = factor(has_loc, labels = c("No", "Yes")))) + geom_bar(stat = "identity") +
  scale_x_continuous(breaks = timeslices) +
  theme(panel.grid.minor = element_blank()) + 
  labs(y = "Number of visits", 
       x = "Timeslice (elapsed time from presentation)",
       title = "Number of visits by timeslice; colour shows whether visit has location in ED (including null location)", 
       subtitle = "Visits from 1 April 2020 to 25 Jan 2020", 
       fill = "Has location data") +
  theme(legend.position = "bottom")



