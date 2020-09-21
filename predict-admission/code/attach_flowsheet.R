library(tidymodels)
library(discrim)
library(dplyr)
library(DBI)
library(lubridate)
library(xgboost)
library(parsnip)
library(caret)
library(polynom)

#------------------------------------------------------------
#------------------------------------------------------------
#flowsheet
# flowsheet contains certain readings for patients
# In this program, each row of the matrix from matrix.csv is attached with 
# the most recent reading, if one exists.
#
# 1. reading matrix from matrix.csv
# 2. query flowsheet
# 3. for each type of reading, create new column and under that column 
#    reading for the csn that has a reading and NA otherwise
# 4. save to design_matrix.csv
#------------------------------------------------------------
#------------------------------------------------------------


# query flowsheet
ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = 'UCLVLDDDTAEPS02',
                      port = 5432,
                      user = 'enockung',
                      password = 'emapu8Q15bz7',
                      dbname = "uds")

sqlQuery <- "SELECT *
  FROM flow.flowsheets"
sqlQuery <- gsub('\n','',sqlQuery)

flowsheet <- as_tibble(dbGetQuery(ctn, sqlQuery))

# read matrix
matrix = read.csv('F:/Saved/ENOCKUNG/ED project/matrix.csv')
matrix$X=NULL
matrix$X.x=NULL

matrix$admission = strptime(matrix$admission,format = '%d/%m/%Y %H:%M')
matrix$discharge = strptime(matrix$discharge,format = '%d/%m/%Y %H:%M')
matrix_copy = matrix
# remove rows where mapped_name == NA
flowsheet = flowsheet %>% filter(!is.na(mapped_name))

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