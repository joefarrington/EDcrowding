# library(tidymodels)
# library(discrim)
library(dplyr)
# library(DBI)
# library(lubridate)
# library(xgboost)
# library(parsnip)
# library(caret)
# library(polynom)

load("~/EDcrowding/predict-admission/data-raw/matrix_2020-09-28.rda")
load("~/EDcrowding/predict-admission/data-raw/flowsheet_2020-09-28.rda")

# Data cleaning of flowsheets
text_types <- flowsheet_raw %>% filter(mapped_name != "BLOOD PRESSURE", !is.na(result_text)) %>% group_by(result_text) %>% summarise(tot = n()) %>% arrange(desc(tot))
# can check whether result_as_real and text show the same; parse blood pressure
# could maybe also check whether the mask eg ETT is actually happening in ED or delayed update - but maybe the model could make use of this ?? 

# Enoch selects latest flowsheet measurement for the relevant row 
# I can use fk_bed_moves to group measurements together and find the latest one
max_meas <- flowsheet_raw %>% group_by(csn, fk_bed_moves) %>% 
  filter(flowsheet_datetime == max(flowsheet_datetime))

# this works but I need pk_bed_moves in the matrix file
# will have to join to get that for now
load("~/EDcrowding/predict-admission/data-raw/bed_moves_2020-09-28.rda")
matrix <- matrix %>% left_join(
  bed_moves_raw %>% select(csn, admission, discharge, pk_bed_moves)
)



# get unique mapped_name's
mapped_names = unique(flowsheet$mapped_name)

# iteratively add new column for each mapped_name
for (i in 1:length(mapped_names)){
  # filter out rows that are about mapped_names[i]
  flow_part = flowsheet %>% filter(mapped_name == mapped_names[i])
  # take only the csn, the datetime logged, and the result
  flow_part = flow_part[,c(2,3,7)]
  # rename columns to csn, datetime, and the third column is named by mapped_names[i]
  colnames(flow_part)=c('csn','datetime',mapped_names[i])
  
  #attach flow_part to the matrix by csn and create new column with the time difference between discharge and 
  #lab datetime (NA if there is no datetime or if difference is negative, time difference only when it is positive)
  matrix_copy = matrix_copy %>% left_join(flow_part,'csn') %>%
    mutate(diff = ifelse(is.na(dm_particular$datetime),factor(NA),
                         ifelse(difftime(discharge,datetime)>=0,difftime(discharge,datetime),factor(NA))))
  
  #grouping by csn, admission, and discharge (unique rows on matrix), arrange them in descending order of time diff,
  # and select the first entry (smallest time difference)
  matrix_copy = matrix_copy %>%
    group_by(csn,admission,discharge) %>% arrange(diff,.group_by=TRUE) %>% filter(row_number()==1) %>% ungroup()
  
  # erase unnecessary columns
  matrix_copy$diff = NULL
  matrix_copy$datetime = NULL
}

write.csv(matrix_copy,'F:/Saved/ENOCKUNG/ED project/design_matrix.csv')