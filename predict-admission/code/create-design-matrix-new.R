


# Libraries --------------------------------------------------------------


library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)


# Load data ----------------------------------------------------------------


load("~/EDcrowding/predict-admission/data-raw/ED_csn_summ_final_csns_2020-12-07.rda")

summ = data.table(ED_csn_summ %>% select(csn, presentation_time, ED_discharge_earliest))

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

getDates <- function(N, st="2019-04-09 01:01:01 BST", et="2020-08-31 01:01:01 BST") {
  st <- as.POSIXct(st)
  et <- as.POSIXct(et)
  dt <- as.numeric(difftime(et,st,unit="sec"))
  ev <- sort(runif(N, 0, dt))
  rt <- st + ev
}



# Generate sampled csns---------------------------------------------------

# how many hours in total available data?
difftime("2020-08-31 01:01:01 BST", "2019-04-01 01:01:01 BST", units = "hours")

set.seed(42)
date_sample = getDates(3108)

# filtering out any csns who spend more than 48 hours between presentation and last ED discharge
summ_temp = summ[difftime(last_ED_discharge, presentation_time, units = "hours") < 48]
rpt(summ_temp)

timeslices_sampled = data.table(summ_temp[presentation_time < date_sample[1] &
                last_ED_discharge > date_sample[1], csn])
timeslices_sampled[, sample_dttm := date_sample[1]]
setnames(timeslices_sampled, "V1", "csn")


for (i in 2:length(date_sample)) {
  d = data.table(summ_temp[presentation_time < date_sample[i] &
                             last_ED_discharge > date_sample[i], csn])
  d[, sample_dttm := date_sample[i]]
  setnames(d, "V1", "csn")
  
  timeslices_sampled = rbind(timeslices_sampled, d)
}

# looking at distribution of csns across timeslices when doing completely random picks
timeslices_sampled[, .N, by = csn] %>% ggplot(aes(x = N, col = "white")) + geom_histogram(binwidth = 1, alpha = .5)
order(x, csn)

# found out how many csns were missed
timeslices_sampled[summ_temp[,.(csn, presentation_time)], on = "csn"] %>% filter(is.na(sample_dttm)) %>% nrow() # ALOT!
