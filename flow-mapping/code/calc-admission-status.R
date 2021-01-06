
# About this file ---------------------------------------------------------

# Creates a classification of admitted or not 

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
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_2021-01-06.rda")
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-01-06.rda")
rpt(moves)

class_e <- data.table(ED_csn_summ %>% filter(patient_class == "EMERGENCY") %>% select(csn))
rpt(class_e) # number with patient class of emergency
setkey(class_e, csn)


class_i <- data.table(ED_csn_summ %>% filter(patient_class == "INPATIENT") %>% select(csn))
rpt(class_i) # number with patient class of inpatient
setkey(class_i, csn)


# Checking moves to locations outside ED -----------------------------------------

class_e_outside_ED = merge(moves[moves[num_ED_exit > 0, unique(csn)], nomatch = 0], class_e)
rpt(class_e_outside_ED) #

class_e_outside_ED_short = merge(moves[moves[ED_exit & ED_exit_short2, unique(csn)], nomatch = 0], class_e)
rpt(class_e_outside_ED_short)

class_e_no_ED_exit = merge(moves[moves[num_ED_exit == 0, unique(csn)], nomatch = 0], class_e)
rpt(class_e_no_ED_exit) 


class_i_outside_ED = merge(moves[moves[num_ED_exit > 0, unique(csn)], nomatch = 0], class_i)
rpt(class_i_outside_ED) # 

class_i_outside_ED_short = merge(moves[moves[ED_exit & ED_exit_short2, unique(csn)], nomatch = 0], class_i)
rpt(class_i_outside_ED_short)

class_i_no_ED_exit = merge(moves[moves[num_ED_exit == 0, unique(csn)], nomatch = 0], class_i)
rpt(class_i_no_ED_exit) 



# Patient class of emergency - calculating statuses ------------- --------

# only has patient class of emergency and ends in OTF 
class_e_ends_in_OTF = merge(moves[final_location == "OTF POOL"], class_e)
rpt(class_e_ends_in_OTF)

# # check no overlap between these
# rpt(class_e_ends_in_OTF[class_e_outside_ED, nomatch = 0]) 
# 
# # check wither any go outside of ED
# rpt(class_e_ends_in_OTF[num_ED_exit >0]) # none
# rpt(class_e_ends_in_OTF[num_ED_exit ==0])

# check class_e that does not end in OTF
class_e_via_OTF = merge(moves[moves[location == "OTF POOL" & final_location != "OTF POOL" , unique(csn)], nomatch = 0], class_e)
rpt(class_e_via_OTF)


rpt(class_e_via_OTF[num_ED_exit >0]) # 13 of the via OTFs did exit ED
rpt(class_e_via_OTF[num_ED_exit >0 & final_dept == "ED"]) # of whom 3 returned
rpt(class_e_via_OTF[num_ED_exit ==0]) # 177 of the via OTFs didn't leave ED

# CHECK count never marked as OTF - NB this uses an anti-join
class_e_never_OTF = merge(moves[!moves[location == "OTF POOL" , unique(csn)]], class_e)
rpt(class_e_never_OTF)

# Comparing e class who never left ED with number with OTF in final row
sum(class_e_no_ED_exit[,unique(csn)] %in% class_e_ends_in_OTF[,unique(csn)]) # ends in OTF
sum(!class_e_no_ED_exit[,unique(csn)] %in% class_e_ends_in_OTF[,unique(csn)])

class_e_no_ED_exit_no_OTF_end = merge(moves[moves[final_location != "OTF POOL" & num_ED_exit == 0, unique(csn)]], class_e)
rpt(class_e_no_ED_exit_no_OTF_end)

# of which some may have been maked as OTF at some point
class_e_no_ED_exit_via_OTF = merge(moves[moves[final_location != "OTF POOL" & location == "OTF POOL" & num_ED_exit == 0, unique(csn)]], class_e)
rpt(class_e_no_ED_exit_via_OTF)

# or never marked as OTF 
class_e_no_ED_exit_never_OTF = merge(class_e_no_ED_exit[class_e_never_OTF[num_ED_exit == 0, unique(csn)]], class_e)
rpt(class_e_no_ED_exit_never_OTF)

class_i_no_ED_exit_OTF_end = merge(moves[moves[final_location == "OTF POOL" & num_ED_exit == 0, unique(csn)]], class_i)
rpt(class_i_no_ED_exit_OTF_end)


# Patient class of inpatient - OTF analysis ------------- --------

# has class inpatient and ends in OTF 
class_i_ends_in_OTF = merge(moves[final_location == "OTF POOL"], class_i)
rpt(class_i_ends_in_OTF)

# check whether any go outside of ED
rpt(class_i_ends_in_OTF[num_ED_exit ==0])
rpt(class_i_ends_in_OTF[num_ED_exit >0]) # 8


# check class_i that does not end in OTF
class_i_via_OTF = merge(moves[moves[location == "OTF POOL" & final_location != "OTF POOL", unique(csn)], nomatch = 0], class_i)
rpt(class_i_via_OTF)


rpt(class_i_via_OTF[num_ED_exit >0]) # 
rpt(class_i_via_OTF[num_ED_exit >0 & final_dept == "ED"]) # of whom 30 returned
rpt(class_i_via_OTF[num_ED_exit ==0]) # 

# CHECK count never marked as OTF - NB this uses an anti-join
class_i_never_OTF = merge(moves[!moves[location == "OTF POOL" , unique(csn)]], class_i)
rpt(class_i_never_OTF)


# Comparing inpatients who never left ED with number with OTF in final row
sum(class_i_no_ED_exit[,unique(csn)] %in% class_i_ends_in_OTF[,unique(csn)]) # ends in OTF
sum(!class_i_no_ED_exit[,unique(csn)] %in% class_i_ends_in_OTF[,unique(csn)])

class_i_no_ED_exit_no_OTF_end = merge(moves[moves[final_location != "OTF POOL" & num_ED_exit == 0, unique(csn)]], class_i)
rpt(class_i_no_ED_exit_no_OTF_end)

# of which some may have been maked as OTF at some point
class_i_no_ED_exit_via_OTF = merge(moves[moves[final_location != "OTF POOL" & location == "OTF POOL" & num_ED_exit == 0, unique(csn)]], class_i)
rpt(class_i_no_ED_exit_via_OTF)

# or never market as OTF 
class_i_no_ED_exit_never_OTF = merge(class_i_no_ED_exit[class_i_never_OTF[num_ED_exit == 0, unique(csn)]], class_i)
rpt(class_i_no_ED_exit_never_OTF)

class_i_no_ED_exit_OTF_end = merge(moves[moves[final_location == "OTF POOL" & num_ED_exit == 0, unique(csn)]], class_i)
rpt(class_i_no_ED_exit_OTF_end)



# Patient class of emergency - TABLE: looking at locations visited ------------- --------

rpt(class_e_outside_ED) 
rpt(class_e_outside_ED_short)
rpt(class_e_no_ED_exit)


# For table
rpt(class_e_ends_in_OTF) #491
rpt(class_e_no_ED_exit_via_OTF) # 177
rpt(class_e_no_ED_exit_never_OTF) # 141701

# Patient class of inpatient - TABLE: looking at locations visited ------------- --------

rpt(class_i_outside_ED) 
rpt(class_i_outside_ED_short)
rpt(class_i_no_ED_exit)

rpt(class_i_ends_in_OTF[num_ED_exit ==0]) # 2469
rpt(class_i_no_ED_exit_via_OTF) # 17
rpt(class_i_no_ED_exit_never_OTF) # 549


# Patient class of emergency - TABLE: OTF analysis ------------- --------

rpt(class_e_ends_in_OTF) #491
rpt(class_e_via_OTF) # 190
rpt(class_e_never_OTF) #141705

rpt(class_e_ends_in_OTF[num_ED_exit ==0]) #491
rpt(class_e_ends_in_OTF[num_ED_exit >0]) # 0
rpt(class_e_via_OTF[num_ED_exit >0 & final_dept != "ED"]) # 10 
rpt(class_e_via_OTF[num_ED_exit >0 & final_dept == "ED"]) # 3 
rpt(class_e_via_OTF[num_ED_exit ==0]) # 177 
rpt(class_e_never_OTF) #141705


# Patient class of inpatient - TABLE: OTF analysis ------------- --------

rpt(class_i_ends_in_OTF) #2477
rpt(class_i_via_OTF) # 24283 
rpt(class_i_never_OTF) #4385

rpt(class_i_ends_in_OTF[num_ED_exit ==0]) #2469
rpt(class_i_ends_in_OTF[num_ED_exit >0]) # 8
rpt(class_i_via_OTF[num_ED_exit >0 & final_dept != "ED"]) # 24236
rpt(class_i_via_OTF[num_ED_exit >0 & final_dept == "ED"]) # 30
rpt(class_i_via_OTF[num_ED_exit ==0]) # 17
rpt(class_i_never_OTF) #4385





# Compare patient class dates ---------------------------------------------

rpt(class_i[min_I < min_E]) # 32 were somewhere else before, according to dates
rpt(moves[first_dept != "ED"]) # 26 had somewhere other than ED as first dept

# check overlap between these
merge(class_i[min_I < min_E], moves[first_dept != "ED"])
# odd that there is no overlap; 
# one person had min_I starting when they left ED (ie min_I == max_E)
# this person started in NCNQ but had no location at their min_E time and their max_E time doesn't look right (NB very recent patient; may change)
class_i[csn == "1024617258",list(csn, min_E, max_E, min_I, max_I)]
moves[csn == "1024617258"]



# Via ED locations --------------------------------------------------------

# SAA

rpt(merge(moves[moves[location == "SAA" , unique(csn)]], class_e))
rpt(merge(moves[!moves[location == "SAA" , unique(csn)]], class_e))

class_e_SAA_to_ED = merge(moves[moves[location == "SAA"  & final_dept == "ED", unique(csn)]], class_e)
rpt(class_e_SAA_to_ED)

class_e_SAA_admitted = merge(moves[moves[location == "SAA"  & final_dept != "ED", unique(csn)]], class_e)
rpt(class_e_SAA_admitted)

rpt(merge(moves[moves[location == "SAA" , unique(csn)]], class_i))
rpt(merge(moves[!moves[location == "SAA" , unique(csn)]], class_i))

class_i_SAA_to_ED = merge(moves[moves[location == "SAA"  & final_dept == "ED", unique(csn)]], class_i)
rpt(class_i_SAA_to_ED)

class_i_SAA_admitted = merge(moves[moves[location == "SAA"  & final_dept != "ED", unique(csn)]], class_i)
rpt(class_i_SAA_admitted)

# SDEC

rpt(merge(moves[moves[location == "SDEC" , unique(csn)]], class_e))
rpt(merge(moves[!moves[location == "SDEC" , unique(csn)]], class_e))

class_e_SDEC_to_ED = merge(moves[moves[location == "SDEC"  & final_dept == "ED", unique(csn)]], class_e)
rpt(class_e_SDEC_to_ED)

class_e_SDEC_admitted = merge(moves[moves[location == "SDEC"  & final_dept != "ED", unique(csn)]], class_e)
rpt(class_e_SDEC_admitted)

rpt(merge(moves[moves[location == "SDEC" , unique(csn)]], class_i))
rpt(merge(moves[!moves[location == "SDEC" , unique(csn)]], class_i))

class_i_SDEC_to_ED = merge(moves[moves[location == "SDEC"  & final_dept == "ED", unique(csn)]], class_i)
rpt(class_i_SDEC_to_ED)

class_i_SDEC_admitted = merge(moves[moves[location == "SDEC"  & final_dept != "ED", unique(csn)]], class_i)
rpt(class_i_SDEC_admitted)

# Via observation units ------------------------

# CDU

class_e_to_CDU = merge(moves[moves[num_to_CDU > 0, unique(csn)]], class_e)
rpt(class_e_to_CDU)
rpt(merge(moves[moves[num_to_CDU == 0, unique(csn)]], class_e))

class_e_to_CDU_to_ED = merge(moves[moves[num_to_CDU > 0 & final_dept == "ED", unique(csn)]], class_e)
rpt(class_e_to_CDU_to_ED)

class_i_to_CDU = merge(moves[moves[num_to_CDU > 0, unique(csn)]], class_i)
rpt(class_i_to_CDU)

class_i_to_CDU_to_ED = merge(moves[moves[num_to_CDU > 0 & final_dept == "ED", unique(csn)]], class_i)
rpt(class_i_to_CDU_to_ED)

class_i_to_CDU_to_discharge =  merge(moves[moves[num_to_CDU > 0 & final_dept == "UCHT00CDU", unique(csn)]], class_i)
rpt(class_i_to_CDU_to_discharge)

class_i_to_CDU_admitted = merge(moves[moves[num_to_CDU > 0 & final_dept != "ED" & final_dept != "UCHT00CDU", unique(csn)]], class_i)
rpt(class_i_to_CDU_admitted)

rpt(merge(moves[moves[num_to_CDU == 0, unique(csn)]], class_i))


# EDU
class_e_to_EDU = merge(moves[moves[num_to_EDU > 0, unique(csn)]], class_e)
rpt(class_e_to_EDU)

class_e_to_EDU_to_ED = merge(moves[moves[num_to_EDU > 0 & final_dept == "ED", unique(csn)]], class_e)
rpt(class_e_to_EDU_to_ED)

rpt(merge(moves[moves[num_to_EDU == 0, unique(csn)]], class_e))


class_i_to_EDU = merge(moves[moves[num_to_EDU > 0, unique(csn)]], class_i)
rpt(class_i_to_EDU)

class_i_to_EDU_to_ED = merge(moves[moves[num_to_EDU > 0 & final_dept == "ED", unique(csn)]], class_i)
rpt(class_i_to_EDU_to_ED)

class_i_to_EDU_to_discharge =  merge(moves[moves[num_to_EDU > 0 & final_dept == "T01ECU", unique(csn)]], class_i)
rpt(class_i_to_EDU_to_discharge)

class_i_to_EDU_admitted = merge(moves[moves[num_to_EDU > 0 & final_dept != "ED" & final_dept != "T01ECU", unique(csn)]], class_i)
rpt(class_i_to_EDU_admitted)

rpt(merge(moves[moves[num_to_EDU == 0, unique(csn)]], class_i))


# EAU
class_e_to_EAU = merge(moves[moves[num_to_EAU > 0, unique(csn)]], class_e)
rpt(class_e_to_EAU)

class_e_to_EAU_to_ED = merge(moves[moves[num_to_EAU > 0 & final_dept == "ED", unique(csn)]], class_e)
rpt(class_e_to_EAU_to_ED)

class_e_to_EAU_to_discharge =  merge(moves[moves[num_to_EAU > 0 & final_dept == "EAU", unique(csn)]], class_e)
rpt(class_e_to_EAU_to_discharge)

class_e_to_EAU_admitted = merge(moves[moves[num_to_EAU > 0 & final_dept != "ED" & final_dept != "EAU", unique(csn)]], class_e)
rpt(class_e_to_EAU_admitted)

rpt(merge(moves[moves[num_to_EAU == 0, unique(csn)]], class_e))


class_i_to_EAU = merge(moves[moves[num_to_EAU > 0, unique(csn)]], class_i)
rpt(class_i_to_EAU)

class_i_to_EAU_to_ED = merge(moves[moves[num_to_EAU > 0 & final_dept == "ED", unique(csn)]], class_i)
rpt(class_i_to_EAU_to_ED)

class_i_to_EAU_to_discharge =  merge(moves[moves[num_to_EAU > 0 & final_dept == "EAU", unique(csn)]], class_i)
rpt(class_i_to_EAU_to_discharge)

class_i_to_EAU_admitted = merge(moves[moves[num_to_EAU > 0 & final_dept != "ED" & final_dept != "EAU", unique(csn)]], class_i)
rpt(class_i_to_EAU_admitted)

rpt(merge(moves[moves[num_to_EAU == 0, unique(csn)]], class_i))


# T01
class_e_to_T01 = merge(moves[moves[num_to_T01 > 0, unique(csn)]], class_e)
rpt(class_e_to_T01)

class_e_to_T01_to_ED = merge(moves[moves[num_to_T01 > 0 & final_dept == "ED", unique(csn)]], class_e)
rpt(class_e_to_T01_to_ED)

class_i_to_T01 = merge(moves[moves[num_to_T01 > 0, unique(csn)]], class_i)
rpt(class_i_to_T01)

rpt(merge(moves[moves[num_to_T01 == 0, unique(csn)]], class_e))


class_i_to_T01_to_ED = merge(moves[moves[num_to_T01 > 0 & final_dept == "ED", unique(csn)]], class_i)
rpt(class_i_to_T01_to_ED)

class_i_to_T01_to_discharge =  merge(moves[moves[num_to_T01 > 0 & final_dept == "T01", unique(csn)]], class_i)
rpt(class_i_to_T01_to_discharge)

class_i_to_T01_admitted = merge(moves[moves[num_to_T01 > 0 & final_dept != "ED" & final_dept != "T01", unique(csn)]], class_i)
rpt(class_i_to_T01_admitted)

rpt(merge(moves[moves[num_to_T01 == 0, unique(csn)]], class_i))



# Grouped exits -----------------------------------------------------------

# Observation units

class_e_via_obs = merge(moves[moves[num_via_obs > 0, unique(csn)]], class_e)
rpt(class_e_via_obs)
rpt(merge(moves[moves[num_via_obs == 0, unique(csn)]], class_e))

class_i_via_obs = merge(moves[moves[num_via_obs > 0, unique(csn)]], class_i)
rpt(class_i_via_obs)

class_i_via_obs_to_ED = merge(moves[moves[num_via_obs > 0 & final_dept == "ED", unique(csn)]], class_i)
rpt(class_i_via_obs_to_ED)

class_i_via_obs_to_discharge =  merge(moves[moves[num_via_obs > 0 & final_dept == "UCHT00CDU", unique(csn)]], class_i)
rpt(class_i_via_obs_to_discharge)

class_i_via_obs_admitted = merge(moves[moves[num_via_obs > 0 & final_dept != "ED" & final_dept != "UCHT00CDU", unique(csn)]], class_i)
rpt(class_i_via_obs_admitted)

rpt(merge(moves[moves[num_via_obs == 0, unique(csn)]], class_i))



# Day pathway

class_e_via_day_path = merge(moves[moves[num_via_day_path > 0, unique(csn)]], class_e)
rpt(class_e_via_day_path)
rpt(merge(moves[moves[num_via_day_path == 0, unique(csn)]], class_e))

class_e_via_day_path_to_ED = merge(moves[moves[num_via_day_path > 0 & final_dept == "ED", unique(csn)]], class_e)
rpt(class_e_via_day_path_to_ED)

class_i_via_day_path = merge(moves[moves[num_via_day_path > 0, unique(csn)]], class_i)
rpt(class_i_via_day_path)

class_i_via_day_path_to_ED = merge(moves[moves[num_via_day_path > 0 & final_dept == "ED", unique(csn)]], class_i)
rpt(class_i_via_day_path_to_ED)

class_i_via_day_path_to_discharge =  merge(moves[moves[num_via_day_path > 0 & final_dept == "UCHT00CDU", unique(csn)]], class_i)
rpt(class_i_via_day_path_to_discharge)

class_i_via_day_path_admitted = merge(moves[moves[num_via_day_path > 0 & final_dept != "ED" & final_dept != "UCHT00CDU", unique(csn)]], class_i)
rpt(class_i_via_day_path_admitted)

rpt(merge(moves[moves[num_via_day_path == 0, unique(csn)]], class_i))


# Putting it together -----------------------------------------------------

# NB - the condition final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU") means that people who end after an admission to somwhere else
# are considered discharged which is not correct - NEED TO FIX

# class e who were discharged
rpt(merge(moves[moves[num_via_day_path == 0 & num_via_obs ==0 & final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU"), unique(csn)]], class_e)) #fast discharge
rpt(merge(moves[moves[num_via_day_path == 0 & num_via_obs !=0 & final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU"), unique(csn)]], class_e)) #slow discharge
rpt(merge(moves[moves[num_via_day_path != 0 & num_via_obs ==0 & final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU"), unique(csn)]], class_e)) #slow discharge

# check whether anyone visited both
class_e_via_both = (merge(moves[moves[num_via_day_path != 0 & num_via_obs !=0 , unique(csn)]], class_e))
rpt(class_e_via_both[final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU") ]) #slow discharge
rpt(class_e_via_both[!final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU") ]) #slow admission

# looking at those who were admitted
class_e_via_obs_admitted = merge(moves[moves[num_via_day_path == 0 & num_via_obs !=0 & !final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU"), unique(csn)]], class_e)
rpt(class_e_via_obs_admitted) # slow admission
class_e_via_day_path_admitted = merge(moves[moves[num_via_day_path != 0 & num_via_obs ==0 & !final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU"), unique(csn)]], class_e)
rpt(class_e_via_day_path_admitted) # slow admission
class_e_admitted_not_via = merge(moves[moves[num_via_day_path == 0 & num_via_obs ==0 & !final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU"), unique(csn)]], class_e)
rpt(class_e_admitted_not_via) # fast_admission



# class i who were discharged
rpt(merge(moves[moves[num_via_day_path == 0 & num_via_obs ==0 & final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU"), unique(csn)]], class_i))
rpt(merge(moves[moves[num_via_day_path == 0 & num_via_obs !=0 & final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU"), unique(csn)]], class_i))
rpt(merge(moves[moves[num_via_day_path != 0 & num_via_obs ==0 & final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU"), unique(csn)]], class_i))

# check whether anyone visited both
class_i_via_both = (merge(moves[moves[num_via_day_path != 0 & num_via_obs !=0 , unique(csn)]], class_i))
rpt(class_i_via_both[final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU") ])
rpt(class_i_via_both[!final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU") ])

# looking at those who were admitted
class_i_via_obs_admitted = merge(moves[moves[num_via_day_path == 0 & num_via_obs !=0 & !final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU"), unique(csn)]], class_i)
rpt(class_i_via_obs_admitted)
class_i_via_day_path_admitted = merge(moves[moves[num_via_day_path != 0 & num_via_obs ==0 & !final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU"), unique(csn)]], class_i)
rpt(class_i_via_day_path_admitted)
class_i_admitted_not_via = merge(moves[moves[num_via_day_path == 0 & num_via_obs ==0 & !final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU"), unique(csn)]], class_i)
rpt(class_i_admitted_not_via)


# Assign final classification ---------------------------------------------

fast_adm <- moves[num_via_day_path == 0 & num_via_obs ==0 & !final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU"), unique(csn)]
slow_adm <- moves[(num_via_day_path != 0 & num_via_obs ==0 & !final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU")) |
                  (num_via_day_path == 0 & num_via_obs !=0 & !final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU")) |
                  (num_via_day_path != 0 & num_via_obs !=0 & !final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU")), unique(csn)]
slow_dis <- moves[(num_via_day_path == 0 & num_via_obs !=0 & final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU")) |
                  (num_via_day_path != 0 & num_via_obs ==0 & final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU")) |
                  (num_via_day_path != 0 & num_via_obs !=0 & final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU")), unique(csn)]
fast_dis <- moves[num_via_day_path == 0 & num_via_obs ==0 & final_dept %in% c("ED", "UCHT00CDU", "EAU", "T01ECU"), unique(csn)]

ED_csn_summ <- ED_csn_summ %>% 
  mutate(adm = case_when(csn %in% fast_adm ~ "fast_adm",
                         csn %in% slow_adm ~ "slow_adm",
                         csn %in% slow_dis ~ "slow_dis",
                         csn %in% fast_dis ~ "fast_dis"))

# save ED_csn_summ for future use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_csn_summ_",today(),".rda")
save(ED_csn_summ, file = outFile)
rm(outFile)
