#------------------------------------------------------------
#------------------------------------------------------------
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
#------------------------------------------------------------
#------------------------------------------------------------

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)
library(xgboost)

bed_moves = read.csv("~/EDcrowding/predict-admission/data-raw/bed_moves_emergency.csv")

# remove unnecessary columns
bed_moves$bed = NULL
bed_moves$X = NULL

# attach encounters
encounter = read.csv('F:/Saved/ENOCKUNG/ED project/encounter.csv')

bed_moves <- bed_moves %>% left_join(encounter %>% select(encounter_id, encounter), 
                                     by = c("csn" = "encounter"))

# # simplify room names
# # ZK note - the intention of this code was to simply room names but it  actually overwrites all rows UTC, MAJ etc in the room field !!
# bed_moves[(bed_moves$department=='UCH EMERGENCY DEPT' & grepl('UTC',bed_moves$room)),]='UTC'
# bed_moves[(bed_moves$department=='UCH EMERGENCY DEPT' & grepl('MAJ',bed_moves$room)),]='MAJ'
# bed_moves[(bed_moves$department=='UCH EMERGENCY DEPT' & grepl('RESUS',bed_moves$room)),]='RESUS'
# bed_moves[(bed_moves$department=='UCH EMERGENCY DEPT' & grepl('PAEDS',bed_moves$room)),]='PAEDS'
# bed_moves[(bed_moves$department=='UCH EMERGENCY DEPT' & grepl('TAF',bed_moves$room)),]='TAF'
# 
# bed_moves[(bed_moves$department=='UCH EMERGENCY DEPT' & is.na(bed_moves$room) & grepl('UTC',bed_moves$hl7_location)),]='UTC'
# bed_moves[(bed_moves$department=='UCH EMERGENCY DEPT' & is.na(bed_moves$room) & grepl('MAJ',bed_moves$hl7_location)),]='MAJ'
# bed_moves[(bed_moves$department=='UCH EMERGENCY DEPT' & is.na(bed_moves$room) & grepl('RESUS',bed_moves$hl7_location)),]='RESUS'
# bed_moves[(bed_moves$department=='UCH EMERGENCY DEPT' & is.na(bed_moves$room) & grepl('PAEDS',bed_moves$hl7_location)),]='PAEDS'
# bed_moves[(bed_moves$department=='UCH EMERGENCY DEPT' & is.na(bed_moves$room) & grepl('TAF',bed_moves$hl7_location)),]='TAF'
# 
# # remove some weird rows
# # no wonder the rows are weird given the above!!
# bed_moves1 = bed_moves %>% filter(!is.na(admission) & admission!='RAT',admission!='UTC' & admission!='MAJ' & admission!='RESUS' & admission!='TAF' & admission!='PAEDS')
# 
# write.csv(bed_moves1,'F:/UserProfiles/ENOCKUNG/ED project/bed_moves_test.csv')

# for each csn, get column for admission
csn = unique(bed_moves1$csn)

classify_csn=c()
classify_adm=c()

for (i in 1:100){
  print(i)
  classify_csn=c(classify_csn,csn[i])
  
  ## check if there are non emergency departments with a later admission time than the emergency dept
  emergency_adm_dates = strptime(bed_moves1[(bed_moves1$csn==csn[i] & bed_moves1$department=='UCH EMERGENCY DEPT'),]$admission,
                                 format = '%d/%m/%Y %H:%M')  # get all admission dates for stages in emergency dept
  # ZK note - dates are currently being read in the wrong format
  
  ## get all non emergency dept stages with admission time after the latest emergency date
  section = bed_moves1[(bed_moves1$csn==csn[i] & 
                          bed_moves1$department!='UCH EMERGENCY DEPT' & 
                          difftime(strptime(bed_moves1$admission,format = '%d/%m/%Y %H:%M'),max(emergency_adm_dates))>0),]
  
  # ## check if a move to CDU should be counted as admission (TRUE or FALSE)
  # count_CDU = TRUE
  # if (count_CDU==TRUE){
  #   section = section %>% filter(grep1('CDU',section$department))
  # }
  
  # EITHER
  # 1. admission is TRUE or FALSE
  # 2. admission is divided into finer classifications (require further definition. here I just put in next stop 
  # after emergency)
  
  classification_type = 1
  
  if (classification_type==1){
    if (nrow(section)!=0){
      classify_adm = c(classify_adm,TRUE)
    } else{
      classify_adm = c(classify_adm,FALSE)
    }
  } 
  
  # ZK - really not sure this next section is ever activated so commenting it out
  # else if(classification_type==2){
  #   # classification is based on its next stop
  #   if (nrow(section)!=0)
  #     {
  #     section = section[order(section$admission),]
  #     next_stop = section[1,]
  #     } 
  #   else
  #     {
  #     # if no stops after then next_stop = 'discharge'
  #     next_stop = 'discharge'
  #     }
  #   
  #   # define the class label based on next_stop
  #   class_labels = c('A','B') # can add more class labels
  #   ### input instructions for next_stop to obtain the variable 'label'
  #   ### for example, to achieve the same as admission TRUE or FALSE, do below:
  #   if (next_stop=='discharge'){
  #     classify_adm = c(classify_adm,'B')
  #   } else {
  #     classify_adm = c(classify_adm,'A')
  #   }
  # }
}
# combine the csn with its eventual class label
adm = data.frame('csn' = classify_csn,'ADM' = classify_adm)

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
demog = read.csv('~/EDcrowding/predict-admission/data-raw/demog.csv')

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

