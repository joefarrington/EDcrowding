# About this file
# ==============

#bed moves data organization
# The bed moves information in bed_moves_emergency are entries of visits that go through ED.
# In this program, this information is prepared in order to fit into the learning model.
#
# 1. simplify room names
# 2. attach classification labels
#    a) TRUE or FALSE of admission
#    b) Finer classifications such as target wards
# 3. add age and sex
# 4. focusing on particular subgroups if desired


# load libraries
# ==============

library(dplyr)
library(tidyverse)
library(lubridate)
library(xgboost)
library(data.table)


# load data
# =========

#bed_moves = read.csv('F:/Saved/ENOCKUNG/ED project/bed_moves_emergency.csv')
load("~/EDcrowding/predict-admission/data-raw/bed_moves_2020-09-21.rda")

adm_zk <- bed_moves_raw %>% group_by(csn) %>% 
  mutate(discharge_dttm = max(discharge)) %>% 
  mutate(admitted = ifelse(discharge_dttm > ed_discharge_dttm, TRUE, FALSE)) %>% select(csn, admitted) %>% distinct()




# join class label with bed_moves based on same csn
bed_move_final = bed_moves1 %>% left_join(adm)

# fix room == RAT (just something weird that came up)
bed_move_final[(bed_move_final$department=='UCH EMERGENCY DEPT' & grepl('RAT',bed_move_final$room)),] = 'RAT'
bed_move_final = bed_move_final %>% filter(admission!='RAT')


### attach age and sex ( if not already added )
### age calculated from birthday


bed_moves = bed_move_final
csn = unique(bed_moves$csn)

# change first stage of UCH EMERGENCY from <NA> to BEGIN
bed_moves$room[(bed_moves$department=='UCH EMERGENCY DEPT' & is.na(bed_moves$room))]='BEGIN'

# add age and sex
demog = read.csv('F:/UserProfiles/ENOCKUNG/ED project/demog.csv')

bed_moves1 = bed_moves %>% left_join(demog,by='mrn')
bed_moves1$X.y=NULL

# use birthdate to calculate age at admission
age = c()

for (i in 1:nrow(bed_moves1)){
  bday=as.Date(bed_moves1$birthdate[i],format='%Y-%m-%d')
  adm_day=as.Date(bed_moves1$admission[i], format = '%d/%m/%Y')
  
  age = c(age,floor(as.numeric(difftime(adm_day,bday))/365))
}

# combine everything together
bed_moves1 = data.frame(bed_moves1,'age'=age)
bed_moves1 = bed_moves1[,c(1,2,3,4,5,6,7,8,11,12,9)]


### if you want to focus on particular groups, such as a particular age group,
### example: age >60. Then simply do a bed_moves1 %>% filter(age>60)

### filter the rows that you need here before saving to excel
write.csv(bed_moves1,'F:/UserProfiles/ENOCKUNG/ED project/matrix.csv')

