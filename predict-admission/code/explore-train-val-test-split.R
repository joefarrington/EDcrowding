
# About this script -------------------------------------------------------

# Use this for charts comparing the allocation of visits between training set and test set. 
# Shows that proportions of train, val and test don't change between timeslices
# in a material way



# Functions ---------------------------------------------------------------


rpt <- function(dataframe) {
  print(dataframe %>% select(csn) %>% n_distinct())
}

# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)



# Load data ----------------------------------------------------------------

# newest data
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-03-24.rda")
summ_new = summ
rpt(summ_new)
max(summ_new$presentation_time)

load("~/EDcrowding/flow-mapping/data-raw/summ_2021-01-25.rda")
summ_old = summ
summ_old = summ_old[presentation_time > '2019-05-01 00:00:00']
min(summ_old$presentation_time)

summ_old = summ_old[!csn %in% summ_new$csn]
rpt(summ_old)


summ = bind_rows(summ_old, summ_new)
rpt(summ)

# Mark pre and post COVID -------------------------------------------------

summ[, COVID := case_when(presentation_time < '2020-03-19 00:00:00' ~ "Pre",
                         TRUE ~ "Post")]

# Do train-test-val split  -----------------------------------------------------------------

summ[, set_ := case_when(presentation_time < '2019-11-19 00:00:00' ~ "Train",
                         presentation_time < '2019-12-13 00:00:00' ~ "Val",
                         presentation_time < '2020-03-19 00:00:00' ~ "Test",
                         presentation_time < '2020-12-01 00:00:00' ~ "Train",
                         presentation_time < '2020-12-29 00:00:00' ~ "Val",
                         TRUE ~ "Test",)]

summ[, set_ := factor(set_, levels = c("Train", "Val", "Test"))]

summ[, adm := if_else(adm %in% c("direct_dis", "indirect_dis"), "Discharged", "Admitted")]

# number of arrivals
summ[, .N, by = .(date(presentation_time), set_)] %>% ggplot(aes(x = date, y = N, fill = set_)) + 
  geom_bar(stat = "identity") +
  labs(title = "Number of visits by day",
       x = "Date", 
       y = "Number of visits",
       fill = "Set") +
  geom_vline(xintercept = c(date("2019-11-19"), date("2019-12-13"), date("2020-03-19"), date("2020-12-01"), date("2020-12-29"))) 


# Raw nuumbers 
summ[, .N, by = .(COVID, set_)] %>% ggplot(aes(x = fct_rev(COVID), y = N, fill = set_)) + geom_bar(stat = "identity") +
  labs(title = "Numbers in each set during each period",
       x = "COVID period", 
       y = "Number of visits",
       fill = "Set") 

# Raw nuumbers to get counts on the plot
summ %>%  ggplot(aes(x = fct_rev(COVID), fill = set_)) + geom_bar(stat = "count") +
  labs(title = "Number of visits in each set during each period",
       x = "COVID period", 
       y = "Number of visits",
       fill = "Set") + 
  stat_count(geom = "text", colour = "black", size = 3.5,
             aes(label = ..count..),position=position_stack(vjust=0.5))
  





# Proportions 
summ[, .N, by = .(COVID, set_)] %>% ggplot(aes(x = fct_rev(COVID), y = N, fill = set_)) + geom_bar(stat = "identity", position = "fill") +
  labs(title = "Proportions in each set during each period",
       x = "COVID period", 
       y = "Number of visits",
       fill = "Set") 



# number of admissions with lines
summ[, .N, by = .(date(presentation_time), adm)] %>% ggplot(aes(x = date, y = N, fill = fct_rev(adm))) + 
  geom_bar(stat = "identity") +
  geom_vline(xintercept = c(date("2019-11-19"), date("2019-12-13"), date("2020-03-19"), date("2020-12-01"), date("2020-12-29"))) +
  labs(title = "Number of admissions and discharges by day",
       x = "Date", 
       y = "Number of visits",
       fill = "Disposition") 

# proportion of admissions with lines
summ[, .N, by = .(date(presentation_time), adm)] %>% ggplot(aes(x = date, y = N, fill = fct_rev(adm))) + 
  geom_bar(stat = "identity", position = "fill") +
  geom_vline(xintercept = c(date("2019-11-19"), date("2019-12-13"), date("2020-03-19"), date("2020-12-01"), date("2020-12-29"))) +
  labs(title = "Proportion of admissions and discharges by day",
       x = "Date", 
       y = "Number of visits",
       fill = "Disposition") 



# I first did this - but note this does not stratify using the dependent variable
# use the following code to divide into relevant group - train, val or teset

test = data.table(row_id = sample(nrow(dm), 0.2 * nrow(dm)), set_ = "test")
rest = base::setdiff(seq_len(nrow(dm)), test$row_id)

val = data.table(row_id = sample(rest, 1/8 * length(rest)), set_ = "val")
train = data.table(row_id = setdiff(seq_len(nrow(dm)), c(val$row_id, test$row_id)), set_ = "train")

row_ids = bind_rows(bind_rows(test, val), train)

dm[, row_id := seq_len(nrow(dm))]
dm = merge(dm, row_ids, by = "row_id")

# So instead I used a tidy models split


# Generate timeslices -----------------------------------------------------

# generate timeslices using generate-timeslices.R

# process slices using beginning of run-ML.R



timeslices <- c("000", "015", "030", "060", "090", "120", "180", "240", "300", "360", "480")

adm_summ <- data.table()
set_summ <- data.table()
adm_set_summ <- data.table()

for (ts_ in timeslices) {
  name_ <- paste0("dm", ts_, "p")
  ts = get(name_)
  num_adm <- ts[, .N, by = .(adm)]
  num_adm[, model := ts_]
  
  num_adm_set <- ts[, .N, by = .(adm, set_)]
  num_adm_set[, model := ts_]
  
  num_set <- ts[, .N, by = .(set_)]
  num_set[, model := ts_]
  
  adm_summ <- bind_rows(adm_summ, num_adm)
  set_summ <- bind_rows(set_summ, num_set)
  adm_set_summ <- bind_rows(adm_set_summ, num_adm_set)
  
}

set_summ[, set_ := factor(set_, levels = c("train", "val", "test"))]
adm_set_summ[, set_ := factor(set_, levels = c("train", "val", "test"))]

# look at class balance as timeslices progress
adm_summ %>% ggplot(aes(x = model, y = N, fill = adm)) + geom_bar(stat = "identity") + 
  labs(title = "Numbers admitted / not admitted in each timeslice", 
       fill = "Admitted (1 = TRUE)",
       x = "Timeslice") +
  theme(legend.position = "bottom") 

# same chart with proportions

adm_summ[, perc := N/sum(N), by = .(model)]
adm_summ[, label := paste(round(perc*100, 1), "%")]

adm_summ %>% ggplot(aes(x = model, y = N, fill = adm)) + geom_bar(stat = "identity") + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Proportions admitted / not admitted in each timeslice (using prior split into train, val and test)", 
       fill = "Set",
       x = "Timeslice") +
  theme(legend.position = "bottom")


# look at class balance as timeslices progress with train-test-val split
adm_set_summ %>% ggplot(aes(x = model, y = N, fill = adm)) + geom_bar(stat = "identity", position = "fill") + 
  labs(title = "Proportion admitted / not admitted in each timeslice", 
       fill = "Admitted (1 = TRUE)",
       x = "Timeslice") +
  theme(legend.position = "bottom")  +
  facet_wrap(. ~ set_)



# look at train, val, test props within timeslice

set_summ[, perc := N/sum(N), by = .(model)]
set_summ[, label := paste(round(perc*100, 1), "%")]

set_summ %>% ggplot(aes(x = model, y = N, fill = set_)) + geom_bar(stat = "identity") + 
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Proportions in train, validation and test sets in each timeslice (using prior split into train, val and test)", 
       fill = "Set",
       x = "Timeslice") +
  theme(legend.position = "bottom") 
  

