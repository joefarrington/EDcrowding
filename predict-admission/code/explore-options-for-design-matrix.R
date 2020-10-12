

# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)


# load data
# ============


load("~/EDcrowding/predict-admission/data-raw/matrix_2020-10-07.rda")

load("~/EDcrowding/predict-admission/data-raw/flowsheet_2020-09-28.rda")
load("~/EDcrowding/predict-admission/data-raw/flowsheet_real_2020-10-08.rda")
load("~/EDcrowding/predict-admission/data-raw/flowsheet_num_results_2020-10-08.rda")
load("~/EDcrowding/predict-admission/data-raw/flowsheet_num_results_with_zero_2020-10-08.rda")

load("~/EDcrowding/predict-admission/data-raw/lab_real_2020-10-07.rda")
load("~/EDcrowding/predict-admission/data-raw/lab_num_results_2020-10-07.rda")
load("~/EDcrowding/predict-admission/data-raw/lab_num_results_with_zero_2020-10-07.rda")

load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-09-30.rda")


ED_csn_mult_rows <- ED_csn_summ %>% filter(num_ED_rows > 4) %>% select(csn)
df <- ED_csn_mult_rows[sample(nrow(ED_csn_mult_rows), 5), ] 

df <- df %>% left_join(matrix)

# add total number of meas for encounter
df <- df %>% left_join(flowsheet_num_results_with_zero_csn_level) # use this to get csn level

# use this to get by location

df <- df %>% left_join(flowsheet_num_results_with_zero %>% rename(pk_bed_moves = fk_bed_moves))
df <- df %>% left_join(lab_num_results_with_zero %>% rename(pk_bed_moves = fk_bed_moves))








# exploring overall distribution of admission
ED_csn_summ %>% filter(date(arrival_dttm) < "2020-03-18", date(arrival_dttm) > "2019-05-01") %>% group_by(date(arrival_dttm)) %>% summarize(num_admitted = n()) %>% 
  ggplot(aes(num_admitted)) + geom_histogram(binwidth = 25, fill="#69b3a2", color="#e9ecef", alpha=0.9)

ED_csn_summ %>% filter(date(arrival_dttm) > "2020-05-01") %>% group_by(date(arrival_dttm)) %>% summarize(num_admitted = n()) %>% 
  ggplot(aes(num_admitted)) + geom_histogram(binwidth = 25, fill="#69b3a2", color="#e9ecef", alpha=0.9)
