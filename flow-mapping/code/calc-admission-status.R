
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

class_e <- data.table(ED_csn_summ %>% filter(patient_class == "EMERGENCY"))
rpt(class_e) # number with patient class of emergency
setkey(class_e, csn)


class_i <- data.table(ED_csn_summ %>% filter(patient_class == "INPATIENT"))
rpt(class_i) # number with patient class of inpatient
setkey(class_i, csn)


# Checking moves to locations outside ED -----------------------------------------

class_e_outside_ED = merge(moves[moves[num_ED_exit > 0, unique(csn)], nomatch = 0], class_e)
rpt(class_e_outside_ED) # only class emergency with moves outside - 17

### START here - problem with ED_exit_short 

moves[, ED_exit_short2 := if_else(ED_exit & csn == lead_csn & shift(row_duration, 1, NA, "lead") < 15, TRUE, FALSE)]

class_e_outside_ED_short = merge(moves[moves[ED_exit & ED_exit_short2, unique(csn)], nomatch = 0], class_e)

class_i_outside_ED = merge(moves[moves[num_ED_exit > 0, unique(csn)], nomatch = 0], class_i)
rpt(class_i_outside_ED) # 


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
class_e_via_OTF = merge(moves[moves[location == "OTF POOL" & final_location != "OTF POOL", unique(csn)], nomatch = 0], class_e)
rpt(class_e_via_OTF)


rpt(class_e_via_OTF[num_ED_exit >0]) # 12 of the via OTFs did exit ED
rpt(class_e_via_OTF[num_ED_exit ==0]) # 177 of the via OTFs didn't leave ED

# CHECK count never marked as OTF - NB this uses an anti-join
class_e_never_OTF = merge(moves[!moves[location == "OTF POOL" , unique(csn)]], class_e)
rpt(class_e_never_OTF)


# Patient class of inpatient - OTF analysis ------------- --------

# only has patient class of emergency and ends in OTF 
class_i_ends_in_OTF = merge(moves[final_location == "OTF POOL"], class_i)
rpt(class_i_ends_in_OTF)

# check whether any go outside of ED
rpt(class_i_ends_in_OTF[num_ED_exit ==0])
rpt(class_i_ends_in_OTF[num_ED_exit >0]) # 8


# check class_i that does not end in OTF
class_i_via_OTF = merge(moves[moves[location == "OTF POOL" & final_location != "OTF POOL", unique(csn)], nomatch = 0], class_i)
rpt(class_i_via_OTF)


rpt(class_i_via_OTF[num_ED_exit >0]) # 
rpt(class_i_via_OTF[num_ED_exit ==0]) # 

# CHECK count never marked as OTF - NB this uses an anti-join
class_i_never_OTF = merge(moves[!moves[location == "OTF POOL" , unique(csn)]], class_i)
rpt(class_i_never_OTF)




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



# With patient class of inpatient - OTF as final location ------------------------



#  has patient class of inptatient but ends in OTF 
class_i_ends_in_OTF = merge(moves[final_location == "OTF POOL"], class_i)
rpt(class_i_ends_in_OTF)

# some of these do go to another location 
rpt(class_i_ends_in_OTF[num_ED_exit > 0]) #8 
# of which some go to CDU then back to ED
rpt(class_i_ends_in_OTF[num_ED_exit > 0 & num_to_CDU > 0]) #4

# but most do not leave ED - these need to be marked as dicharged
rpt(class_i_ends_in_OTF[num_ED_exit == 0])

# With patient class of inpatient - went via OTF to somewhere else------------------------

# marked as OTF and went to another location
class_i_via_OTF = merge(moves[moves[location == "OTF POOL" & final_location != "OTF POOL", unique(csn)], nomatch = 0], class_i)
rpt(class_i_via_OTF)

rpt(class_i_via_OTF[num_ED_exit >0])
rpt(class_i_via_OTF[num_ED_exit ==0]) # 17 didn't leave ED
