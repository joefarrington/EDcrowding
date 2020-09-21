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
#lab
# labs contains certain readings for patients
# In this program, each row of the matrix from design_matrix.csv is attached with 
# the most recent reading, if one exists.
#
# 1. reading matrix from matrix.csv
# 2. query labs
# 3. for each type of reading, create new column and under that column 
#    reading for the csn that has a reading and NA otherwise
#    note: because of how long the process of this took, I saved a prelim version of this to lab_before_edit.csv.
#          only labs columns are there
# 4. read lab_before_edit and attach everything else from design_matrix
# 5. save to dm_with_demog_flowsheet_labs.csv
#------------------------------------------------------------
#------------------------------------------------------------

ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = 'UCLVLDDDTAEPS02',
                      port = 5432,
                      user = 'enockung',
                      password = 'emapu8Q15bz7',
                      dbname = "uds")

sqlQuery <- "SELECT *
  FROM flow.labs"
sqlQuery <- gsub('\n','',sqlQuery)

labs <- as_tibble(dbGetQuery(ctn, sqlQuery))

# reading design_matrix containing patient location, age, sex, and flowsheet
dm = read.csv('F:/Saved/ENOCKUNG/ED project/design_matrix.csv')

dm_csn = unique(dm$csn)

# pull out csn, admission, discharge
dm_particular = dm[,c(3,4,5)]
dm_particular$admission = strptime(dm_particular$admission,format = '%d/%m/%Y %H:%M')
dm_particular$discharge = strptime(dm_particular$discharge,format = '%d/%m/%Y %H:%M')
dm_particular$csn = as.character(dm$csn)

# for each lab
lab_code = unique(labs$local_code)
df_labs = data.frame(csn = as.character(dm$csn))

for (i in 1:length(lab_code)){
  # for each new lab, create data frame with csn and reading under one column with name of lab_code
  
  lab_particular = labs %>% filter(local_code == lab_code[i])
  lab_particular = lab_particular[,c(2,3,7)]
  colnames(lab_particular) = c('csn','datetime',lab_code[i])
  
  dm_particular = dm_particular %>% left_join(lab_particular,by='csn')
  
  dm_particular = dm_particular %>% mutate(diff = ifelse(is.na(dm_particular$datetime),factor(NA),
                                                         ifelse(difftime(discharge,datetime)>=0,difftime(discharge,datetime),factor(NA))))
  dm_particular = dm_particular %>%
    group_by(csn,admission,discharge) %>% arrange(diff,.group_by=TRUE) %>% filter(row_number()==1) %>% ungroup()
  
  dm_particular$datetime = NULL
  dm_particular$diff = NULL
  
}

write.csv(dm_particular, 'F:/Saved/ENOCKUNG/ED project/lab_before_edit.csv')

## combining newly organized labs with the dm

dm_labs = read.csv('F:/Saved/ENOCKUNG/ED project/lab_before_edit.csv')
dm_labs$X = NULL
dm$X = NULL

dm_combine = dm %>% left_join(dm_labs,by = c('csn','admission','discharge'))

dm_combine = dm_combine %>% relocate(ADM, .after = last_col())
write.csv(dm_combine,'F:/Saved/ENOCKUNG/ED project/dm_with_demog_flowsheet_labs.csv')
