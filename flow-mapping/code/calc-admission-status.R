
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
load("~/EDcrowding/flow-mapping/data-raw/moves_2021-01-11.rda")
rpt(moves)

class_e <- data.table(ED_csn_summ %>% filter(patient_class == "EMERGENCY") %>% select(csn))
rpt(class_e) # number with patient class of emergency
setkey(class_e, csn)


class_i <- data.table(ED_csn_summ %>% filter(patient_class == "INPATIENT") %>% select(csn))
rpt(class_i) # number with patient class of inpatient
setkey(class_i, csn)


# Checking moves to locations visited_outside ED -----------------------------------------

class_e_visited_outside_ED = merge(moves[moves[num_ED_exit > 0, unique(csn)], nomatch = 0], class_e)
rpt(class_e_visited_outside_ED) #

class_e_visited_outside_ED_short = merge(moves[moves[ED_exit & ED_exit_short2, unique(csn)], nomatch = 0], class_e)
rpt(class_e_visited_outside_ED_short)

class_e_no_ED_exit = merge(moves[moves[num_ED_exit == 0, unique(csn)], nomatch = 0], class_e)
rpt(class_e_no_ED_exit) 


class_i_visited_outside_ED = merge(moves[moves[num_ED_exit > 0, unique(csn)], nomatch = 0], class_i)
rpt(class_i_visited_outside_ED) # 

class_i_visited_outside_ED_short = merge(moves[moves[ED_exit & ED_exit_short2, unique(csn)], nomatch = 0], class_i)
rpt(class_i_visited_outside_ED_short)

class_i_no_ED_exit = merge(moves[moves[num_ED_exit == 0, unique(csn)], nomatch = 0], class_i)
rpt(class_i_no_ED_exit) 



# Patient class of emergency - calculating statuses ------------- --------

# only has patient class of emergency and ends in OTF 
class_e_ends_in_OTF = merge(moves[final_location == "OTF POOL"], class_e)
rpt(class_e_ends_in_OTF)

# # check no overlap between these
# rpt(class_e_ends_in_OTF[class_e_visited_outside_ED, nomatch = 0]) 
# 
# # check wither any go visited_outside of ED
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

# check whether any go visited_outside of ED
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

rpt(class_e_visited_outside_ED) 
rpt(class_e_visited_outside_ED_short)
rpt(class_e_no_ED_exit)


# For table
rpt(class_e_ends_in_OTF) #491
rpt(class_e_no_ED_exit_via_OTF) # 177
rpt(class_e_no_ED_exit_never_OTF) # 141701

# Patient class of inpatient - TABLE: looking at locations visited ------------- --------

rpt(class_i_visited_outside_ED) 
rpt(class_i_visited_outside_ED_short)
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
# NB these use final location which may not in all cases be accurate 
# as patients may leave ED and come back


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

class_e_to_CDU = merge(moves[moves[(visited_CDU), unique(csn)]], class_e)
rpt(class_e_to_CDU)
rpt(merge(moves[moves[(!visited_CDU), unique(csn)]], class_e))

class_e_to_CDU_to_ED = merge(moves[moves[(visited_CDU) & final_dept == "ED", unique(csn)]], class_e)
rpt(class_e_to_CDU_to_ED)

class_i_to_CDU = merge(moves[moves[(visited_CDU), unique(csn)]], class_i)
rpt(class_i_to_CDU)

class_i_to_CDU_to_ED = merge(moves[moves[(visited_CDU) & final_dept == "ED", unique(csn)]], class_i)
rpt(class_i_to_CDU_to_ED)

class_i_to_CDU_to_discharge =  merge(moves[moves[(visited_CDU) & final_dept == "UCHT00CDU", unique(csn)]], class_i)
rpt(class_i_to_CDU_to_discharge)

class_i_to_CDU_admitted = merge(moves[moves[(visited_CDU) & final_dept != "ED" & final_dept != "UCHT00CDU", unique(csn)]], class_i)
rpt(class_i_to_CDU_admitted)

rpt(merge(moves[moves[(!visited_CDU), unique(csn)]], class_i))


# EDU
class_e_to_EDU = merge(moves[moves[(visited_EDU), unique(csn)]], class_e)
rpt(class_e_to_EDU)

class_e_to_EDU_to_ED = merge(moves[moves[(visited_EDU) & final_dept == "ED", unique(csn)]], class_e)
rpt(class_e_to_EDU_to_ED)

rpt(merge(moves[moves[(!visited_EDU), unique(csn)]], class_e))


class_i_to_EDU = merge(moves[moves[(visited_EDU), unique(csn)]], class_i)
rpt(class_i_to_EDU)

class_i_to_EDU_to_ED = merge(moves[moves[(visited_EDU) & final_dept == "ED", unique(csn)]], class_i)
rpt(class_i_to_EDU_to_ED)

class_i_to_EDU_to_discharge =  merge(moves[moves[(visited_EDU) & final_dept %in% c("T01ECU", "AECU"), unique(csn)]], class_i)
rpt(class_i_to_EDU_to_discharge)

class_i_to_EDU_admitted = merge(moves[moves[(visited_EDU) & final_dept != "ED" & !final_dept %in% c("T01ECU", "AECU"), unique(csn)]], class_i)
rpt(class_i_to_EDU_admitted)

rpt(merge(moves[moves[(!visited_EDU), unique(csn)]], class_i))


# EAU
class_e_to_EAU = merge(moves[moves[(visited_EAU), unique(csn)]], class_e)
rpt(class_e_to_EAU)

class_e_to_EAU_to_ED = merge(moves[moves[(visited_EAU) & final_dept == "ED", unique(csn)]], class_e)
rpt(class_e_to_EAU_to_ED)

class_e_to_EAU_to_discharge =  merge(moves[moves[(visited_EAU) & final_dept == "EAU", unique(csn)]], class_e)
rpt(class_e_to_EAU_to_discharge)

class_e_to_EAU_admitted = merge(moves[moves[(visited_EAU) & final_dept != "ED" & final_dept != "EAU", unique(csn)]], class_e)
rpt(class_e_to_EAU_admitted)

rpt(merge(moves[moves[(!visited_EAU), unique(csn)]], class_e))


class_i_to_EAU = merge(moves[moves[(visited_EAU), unique(csn)]], class_i)
rpt(class_i_to_EAU)

class_i_to_EAU_to_ED = merge(moves[moves[(visited_EAU) & final_dept == "ED", unique(csn)]], class_i)
rpt(class_i_to_EAU_to_ED)

class_i_to_EAU_to_discharge =  merge(moves[moves[(visited_EAU) & final_dept == "EAU", unique(csn)]], class_i)
rpt(class_i_to_EAU_to_discharge)

class_i_to_EAU_admitted = merge(moves[moves[(visited_EAU) & final_dept != "ED" & final_dept != "EAU", unique(csn)]], class_i)
rpt(class_i_to_EAU_admitted)

rpt(merge(moves[moves[(!visited_EAU), unique(csn)]], class_i))


# T01
class_e_to_T01 = merge(moves[moves[(visited_T01), unique(csn)]], class_e)
rpt(class_e_to_T01)

class_e_to_T01_to_ED = merge(moves[moves[(visited_T01) & final_dept == "ED", unique(csn)]], class_e)
rpt(class_e_to_T01_to_ED)

class_i_to_T01 = merge(moves[moves[(visited_T01), unique(csn)]], class_i)
rpt(class_i_to_T01)

rpt(merge(moves[moves[(!visited_T01), unique(csn)]], class_e))


class_i_to_T01_to_ED = merge(moves[moves[(visited_T01) & final_dept == "ED", unique(csn)]], class_i)
rpt(class_i_to_T01_to_ED)

class_i_to_T01_to_discharge =  merge(moves[moves[(visited_T01) & final_dept == "T01", unique(csn)]], class_i)
rpt(class_i_to_T01_to_discharge)

class_i_to_T01_admitted = merge(moves[moves[(visited_T01) & final_dept != "ED" & final_dept != "T01", unique(csn)]], class_i)
rpt(class_i_to_T01_admitted)

rpt(merge(moves[moves[(!visited_T01), unique(csn)]], class_i))



# Grouped exits -----------------------------------------------------------

# Observation units

class_e_via_obs = merge(moves[moves[(visited_obs), unique(csn)]], class_e)
rpt(class_e_via_obs)
rpt(merge(moves[moves[num_via_obs == 0, unique(csn)]], class_e))

class_i_via_obs = merge(moves[moves[(visited_obs), unique(csn)]], class_i)
rpt(class_i_via_obs)

class_i_via_obs_to_ED = merge(moves[moves[(visited_obs) & final_dept == "ED", unique(csn)]], class_i)
rpt(class_i_via_obs_to_ED)

class_i_via_obs_to_discharge =  merge(moves[moves[(visited_obs) & final_dept == "UCHT00CDU", unique(csn)]], class_i)
rpt(class_i_via_obs_to_discharge)

class_i_via_obs_admitted = merge(moves[moves[(visited_obs) & final_dept != "ED" & final_dept != "UCHT00CDU", unique(csn)]], class_i)
rpt(class_i_via_obs_admitted)

rpt(merge(moves[moves[num_via_obs == 0, unique(csn)]], class_i))



# Day pathway

class_e_via_day_path = merge(moves[moves[(visited_same_day), unique(csn)]], class_e)
rpt(class_e_via_day_path)
rpt(merge(moves[moves[(!visited_same_day), unique(csn)]], class_e))

class_i_via_day_path = merge(moves[moves[(visited_same_day), unique(csn)]], class_i)
rpt(class_i_via_day_path)

rpt(merge(moves[moves[(!visited_same_day), unique(csn)]], class_i))


# Day pathway

class_e_via_acute = merge(moves[moves[(visited_acute), unique(csn)]], class_e)
rpt(class_e_via_acute)
rpt(merge(moves[moves[(!visited_acute), unique(csn)]], class_e))

class_i_via_acute = merge(moves[moves[(visited_acute), unique(csn)]], class_i)
rpt(class_i_via_acute)

rpt(merge(moves[moves[(!visited_acute), unique(csn)]], class_i))


# Putting it together -----------------------------------------------------

# NB - the condition visited_outside means that people who end after an admission to somwhere else
# are considered discharged which is not correct - NEED TO FIX

# class e who were discharged
rpt(merge(moves[moves[(!visited_same_day) & !visited_obs & !visited_outside, unique(csn)]], class_e)) #fast discharge
rpt(merge(moves[moves[(!visited_same_day) & visited_obs & !visited_outside, unique(csn)]], class_e)) #slow discharge
rpt(merge(moves[moves[(visited_same_day) & !visited_obs & !visited_outside, unique(csn)]], class_e)) #slow discharge
rpt(merge(moves[moves[visited_same_day & visited_obs & !visited_outside, unique(csn)]], class_e)) #slow discharge

rpt(merge(moves[moves[visited_same_day & visited_obs & visited_outside, unique(csn)]], class_e)) #slow admission
rpt(merge(moves[moves[(!visited_same_day) & visited_obs & visited_outside, unique(csn)]], class_e)) #slow admission
rpt(merge(moves[moves[(visited_same_day) & !visited_obs & visited_outside, unique(csn)]], class_e)) #slow admission
rpt(merge(moves[moves[(!visited_same_day) & !visited_obs & visited_outside, unique(csn)]], class_e)) #fast admission




# class i who were discharged
rpt(merge(moves[moves[(!visited_same_day) & !visited_obs & !visited_outside, unique(csn)]], class_i)) #fast discharge
rpt(merge(moves[moves[(!visited_same_day) & visited_obs & !visited_outside, unique(csn)]], class_i)) #slow discharge
rpt(merge(moves[moves[(visited_same_day) & !visited_obs & !visited_outside, unique(csn)]], class_i)) #slow discharge
rpt(merge(moves[moves[visited_same_day & visited_obs & !visited_outside, unique(csn)]], class_i)) #slow discharge

rpt(merge(moves[moves[visited_same_day & visited_obs & visited_outside, unique(csn)]], class_i)) #slow admission
rpt(merge(moves[moves[(!visited_same_day) & visited_obs & visited_outside, unique(csn)]], class_i)) #slow admission
rpt(merge(moves[moves[(visited_same_day) & !visited_obs & visited_outside, unique(csn)]], class_i)) #slow admission
rpt(merge(moves[moves[(!visited_same_day) & !visited_obs & visited_outside, unique(csn)]], class_i)) #fast admission




# Assign final classification ---------------------------------------------

direct_adm <- moves[(!visited_same_day) & !visited_obs & visited_outside, unique(csn)]
indirect_adm <- moves[(visited_same_day & !visited_obs & visited_outside) |
                  ((!visited_same_day) & visited_obs & visited_outside) |
                  (visited_same_day & visited_obs & visited_outside), unique(csn)]
indirect_dis <- moves[((!visited_same_day) & visited_obs & !visited_outside) |
                  (visited_same_day & !visited_obs & !visited_outside) |
                  (visited_same_day & visited_obs & !visited_outside), unique(csn)]
direct_dis <- moves[(!visited_same_day) & !visited_obs & !visited_outside, unique(csn)]

ED_csn_summ <- ED_csn_summ %>% 
  mutate(adm = case_when(csn %in% direct_adm ~ "direct_adm",
                         csn %in% indirect_adm ~ "indirect_adm",
                         csn %in% indirect_dis ~ "indirect_dis",
                         csn %in% direct_dis ~ "direct_dis"))

# save ED_csn_summ for future use

outFile = paste0("EDcrowding/flow-mapping/data-raw/ED_csn_summ_",today(),".rda")
save(ED_csn_summ, file = outFile)
rm(outFile)



# Looking at last ED discharge time ----------------------------------------


moves = moves[data.table(ED_csn_summ %>% select(csn, last_ED_discharge_time)), nomatch = 0]
moves[, before := admission < last_ED_discharge_time]
moves[, during := admission <= last_ED_discharge_time & discharge >=last_ED_discharge_time]
moves[, after := admission > last_ED_discharge_time]

moves[(!outside), last_inside := if_else(discharge == max(discharge, na.rm = TRUE), TRUE, FALSE), by = csn]
moves[ED == 1, last_ED := if_else(discharge == max(discharge, na.rm = TRUE), TRUE, FALSE), by = csn]

last_inside = moves[(last_inside), list(csn, discharge)]
setnames(last_inside, "discharge", "last_inside_discharge")

last_ED = moves[(last_ED), list(csn, discharge)]
setnames(last_ED, "discharge", "last_ED_discharge")


# Note- getting 100 duplicate rows due to admission and discharge dates being the same - ignoring for now
e = ED_csn_summ %>% left_join(last_inside %>% distinct()) %>% select(csn, adm, min_I, min_E, presentation_time, last_inside_discharge)
e = e %>% left_join(last_ED)
e = e %>% mutate(time_to_discharge_from_ED_obs_same_day = difftime(last_inside_discharge, presentation_time, units = "hours"))
e = e %>% mutate(time_to_discharge_from_ED = difftime(last_ED_discharge, presentation_time, units = "hours"))
e = e %>% mutate(time_to_inpatient_class = difftime(min_I, presentation_time, units = "hours"))
e = e %>% mutate(has_inpatient_class = if_else(is.na(min_I), 0, 1))
e = e %>% mutate(has_emerg_class = if_else(is.na(min_E), 0, 1))


e = e %>% mutate(adm = factor(adm, levels = c("direct_dis", "indirect_dis", "indirect_adm", "direct_adm")))

# Bar chart showing inpatient class
e %>% 
  ggplot(aes(fill = adm, x = adm, y = has_inpatient_class)) + geom_bar(stat = "identity")  +
  scale_fill_manual(values = c("#F8766D" , "#FFB2B2","#99E4E7","#00BFC4")) +
  labs(title = "Number of visits which include class of inpatient",
       y = "Has inpatient class") +
  theme(legend.position = "bottom")  +
  theme(axis.text.x=element_blank())


e %>% filter(time_to_inpatient_class < days(1)) %>% 
  ggplot(aes(fill = adm, col = adm, x = as.numeric(time_to_inpatient_class))) + geom_boxplot(alpha = .5) + 
  scale_fill_manual(values = c("#F8766D" , "#FFB2B2","#99E4E7","#00BFC4")) +
  labs(title = "Time from presentation to change to inpatient class",
       subtitle = "Note that some patients are inpatients before they present at ED", 
       y = NULL,
       x = "Hours from presentation (capped at 1 day)"
  ) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("#F8766D" , "#FFB2B2","#99E4E7","#00BFC4", guide = NULL, name = NULL)) +
  theme(axis.text.y=element_blank())

# e %>% 
#   ggplot(aes(fill = adm, x = adm, y = has_emerg_class)) + geom_bar(stat = "identity")  +
#   scale_fill_manual(values = c("#F8766D" , "#FFB2B2","#99E4E7","#00BFC4")) +
#   labs(title = "Number of visits which include class of emergency",
#        y = "Has emergency class", 
#        fill = "Admission class: ") +
#   theme(legend.position = "bottom")


chart_data = e %>% pivot_longer(starts_with("time_to_"), names_to = "time_to") 

e %>% filter(time_to_discharge_from_ED_obs_same_day < days(1)) %>% 
  ggplot(aes(fill = adm, col = adm, x = as.numeric(time_to_discharge_from_ED_obs_same_day))) + geom_boxplot(alpha = .5) + 
  scale_fill_manual(values = c("#F8766D" , "#FFB2B2","#99E4E7","#00BFC4")) +
  labs(title = "Length of stay in ED, including time in observation or same day emergency care",
       y = NULL,
       x = "Hours from presentation (capped at 1 day)"
       ) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("#F8766D" , "#FFB2B2","#99E4E7","#00BFC4", guide = NULL, name = NULL)) +
  theme(axis.text.y=element_blank())
  
e %>% filter(time_to_discharge_from_ED < days(1)) %>% 
  ggplot(aes(fill = adm, col = adm, x = as.numeric(time_to_discharge_from_ED))) + geom_boxplot(alpha = .5) + 
  scale_fill_manual(values = c("#F8766D" , "#FFB2B2","#99E4E7","#00BFC4")) +
  labs(title = "Length of stay in ED only",
       y = NULL,
       x = "Hours from presentation (capped at 1 day)"
  ) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("#F8766D" , "#FFB2B2","#99E4E7","#00BFC4", guide = NULL, name = NULL)) +
  theme(axis.text.y=element_blank())

