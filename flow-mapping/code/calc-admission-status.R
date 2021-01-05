
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
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-12-17.rda")
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-01-04.rda")
rpt(moves)

moves <- moves[order(csn, admission)]

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



# Patient class of emergency - OTF analysis ------------- --------

# only has patient class of emergency and ends in OTF 
class_e_ends_in_OTF = merge(moves[final_location == "OTF POOL"], class_e)
rpt(class_e_ends_in_OTF)

# check no overlap between these
rpt(class_e_ends_in_OTF[class_e_outside_ED, nomatch = 0]) 

# check wither any go outside of ED
rpt(class_e_ends_in_OTF[num_ED_exit >0]) # none
rpt(class_e_ends_in_OTF[num_ED_exit ==0])

# check class_e that does not end in OTF
class_e_via_OTF = merge(moves[moves[location == "OTF POOL" & final_location != "OTF POOL" , unique(csn)], nomatch = 0], class_e)
rpt(class_e_via_OTF)


rpt(class_e_via_OTF[num_ED_exit >0]) # 12 of the via OTFs did exit ED
rpt(class_e_via_OTF[num_ED_exit >0 & final_dept == "ED"]) # of whom three returned
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

# or never market as OTF 
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
rpt(merge(moves[moves[num_to_CDU == 0, unique(csn)]], class_i))

class_i_to_CDU_to_ED = merge(moves[moves[num_to_CDU > 0 & final_dept == "ED", unique(csn)]], class_i)
rpt(class_i_to_CDU_to_ED)

class_i_to_CDU_to_discharge =  merge(moves[moves[num_to_CDU > 0 & final_dept == "UCHT00CDU", unique(csn)]], class_i)
rpt(class_i_to_CDU_to_discharge)

class_i_to_CDU_admitted = merge(moves[moves[num_to_CDU > 0 & final_dept != "ED" & final_dept != "UCHT00CDU", unique(csn)]], class_i)
rpt(class_i_to_CDU_admitted)


# EDU
class_e_to_EDU = merge(moves[moves[num_to_EDU > 0, unique(csn)]], class_e)
rpt(class_e_to_EDU)
rpt(merge(moves[moves[num_to_EDU == 0, unique(csn)]], class_e))

class_e_to_EDU_to_ED = merge(moves[moves[num_to_EDU > 0 & final_dept == "ED", unique(csn)]], class_e)
rpt(class_e_to_EDU_to_ED)

class_i_to_EDU = merge(moves[moves[num_to_EDU > 0, unique(csn)]], class_i)
rpt(class_i_to_EDU)
rpt(merge(moves[moves[num_to_EDU == 0, unique(csn)]], class_i))

class_i_to_EDU_to_ED = merge(moves[moves[num_to_EDU > 0 & final_dept == "ED", unique(csn)]], class_i)
rpt(class_i_to_EDU_to_ED)

class_i_to_EDU_to_discharge =  merge(moves[moves[num_to_EDU > 0 & final_dept == "T01ECU", unique(csn)]], class_i)
rpt(class_i_to_EDU_to_discharge)

class_i_to_EDU_admitted = merge(moves[moves[num_to_EDU > 0 & final_dept != "ED" & final_dept != "T01ECU", unique(csn)]], class_i)
rpt(class_i_to_EDU_admitted)

# EAU
class_e_to_EAU = merge(moves[moves[num_to_EAU > 0, unique(csn)]], class_e)
rpt(class_e_to_EAU)
rpt(merge(moves[moves[num_to_EAU == 0, unique(csn)]], class_e))

class_e_to_EAU_to_ED = merge(moves[moves[num_to_EAU > 0 & final_dept == "ED", unique(csn)]], class_e)
rpt(class_e_to_EAU_to_ED)

class_e_to_EAU_to_discharge =  merge(moves[moves[num_to_EAU > 0 & final_dept == "EAU", unique(csn)]], class_e)
rpt(class_e_to_EAU_to_discharge)

class_e_to_EAU_admitted = merge(moves[moves[num_to_EAU > 0 & final_dept != "ED" & final_dept != "EAU", unique(csn)]], class_e)
rpt(class_e_to_EAU_admitted)

class_i_to_EAU = merge(moves[moves[num_to_EAU > 0, unique(csn)]], class_i)
rpt(class_i_to_EAU)
rpt(merge(moves[moves[num_to_EAU == 0, unique(csn)]], class_i))

class_i_to_EAU_to_ED = merge(moves[moves[num_to_EAU > 0 & final_dept == "ED", unique(csn)]], class_i)
rpt(class_i_to_EAU_to_ED)

class_i_to_EAU_to_discharge =  merge(moves[moves[num_to_EAU > 0 & final_dept == "T01ECU", unique(csn)]], class_i)
rpt(class_i_to_EAU_to_discharge)

class_i_to_EAU_admitted = merge(moves[moves[num_to_EAU > 0 & final_dept != "ED" & final_dept != "T01ECU", unique(csn)]], class_i)
rpt(class_i_to_EAU_admitted)

# T01
class_e_to_T01 = merge(moves[moves[num_to_T01 > 0, unique(csn)]], class_e)
rpt(class_e_to_T01)
rpt(merge(moves[moves[num_to_T01 == 0, unique(csn)]], class_e))

class_e_to_T01_to_ED = merge(moves[moves[num_to_T01 > 0 & final_dept == "ED", unique(csn)]], class_e)
rpt(class_e_to_T01_to_ED)

class_i_to_T01 = merge(moves[moves[num_to_T01 > 0, unique(csn)]], class_i)
rpt(class_i_to_T01)
rpt(merge(moves[moves[num_to_T01 == 0, unique(csn)]], class_i))

class_i_to_T01_to_ED = merge(moves[moves[num_to_T01 > 0 & final_dept == "ED", unique(csn)]], class_i)
rpt(class_i_to_T01_to_ED)

class_i_to_T01_to_discharge =  merge(moves[moves[num_to_T01 > 0 & final_dept == "T01", unique(csn)]], class_i)
rpt(class_i_to_T01_to_discharge)

class_i_to_T01_admitted = merge(moves[moves[num_to_T01 > 0 & final_dept != "ED" & final_dept != "T01", unique(csn)]], class_i)
rpt(class_i_to_T01_admitted)




