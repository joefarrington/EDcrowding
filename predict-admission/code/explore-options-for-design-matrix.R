

# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)


# load data
# ============


load("~/EDcrowding/predict-admission/data-raw/matrix_loc_2020-10-12.rda")
load("~/EDcrowding/predict-admission/data-raw/matrix_csn_2020-10-12.rda")

load("~/EDcrowding/predict-admission/data-raw/flowsheet_2020-09-28.rda")
load("~/EDcrowding/predict-admission/data-raw/flowsheet_real_2020-10-08.rda")
load("~/EDcrowding/predict-admission/data-raw/flowsheet_num_results_2020-10-08.rda")
load("~/EDcrowding/predict-admission/data-raw/flowsheet_num_results_with_zero_2020-10-08.rda")

load("~/EDcrowding/predict-admission/data-raw/lab_real_2020-10-07.rda")
load("~/EDcrowding/predict-admission/data-raw/lab_num_results_2020-10-07.rda")
load("~/EDcrowding/predict-admission/data-raw/lab_num_results_with_zero_2020-10-07.rda")

load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-10-12.rda")


ED_csn_mult_rows <- ED_csn_summ %>% filter(num_ED_rows > 4) %>% select(csn)
df <- ED_csn_mult_rows[sample(nrow(ED_csn_mult_rows), 5), ] 


# to get results summed across enoucnter
df_csn <- df %>% left_join(matrix_csn)
df_csn <- df_csn %>% left_join(flowsheet_num_results_with_zero_csn_level) # use this to get csn level
df_csn <- df_csn %>% left_join(lab_num_results_with_zero_csn_level) # use this to get csn level


# use this to get by location
# but there will be NAs for each row without a flowsheet measurement

df_loc <- df %>% left_join(matrix_loc)
df_loc <- df_loc %>% left_join(flowsheet_num_results_with_zero %>% rename(pk_bed_moves = fk_bed_moves))
df_loc <- df_loc %>% left_join(lab_num_results_with_zero %>% rename(pk_bed_moves = fk_bed_moves))



lab_real %>% filter(elapsed_mins <= 90) %>% select(csn, local_code, result_as_real) %>% 
  group_by(csn, local_code) %>% summarise(num = n()) %>% 
  pivot_wider(names_from = local_code, values_from = num) 

