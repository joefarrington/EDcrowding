
# About this file ---------------------------------------------------------

# Explores implications of the admission classification

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
load("~/EDcrowding/flow-mapping/data-raw/all_patient_class_2021-01-06.rda")
rpt(moves)


# Why are so many inpatients discharged?  ---------------------------------

fast_dis_inpatient <- data.table(ED_csn_summ %>% filter(patient_class == "INPATIENT", adm == "fast_dis"))
setkey(fast_dis_inpatient, csn)
moves_1 <- moves[fast_dis_inpatient, nomatch = 0]
all_patient_class %>% filter(csn == "1004891335")
moves_1[csn == "1004891335"]

loc = moves_1 %>% select(csn, first_dept, final_location) %>% distinct()
loc[,.N, by = first_dept] # 3010 began in ED
loc[,.N, by = final_location] # 2470 ended in OTF

moves_1[final_location %in% c("EAU", "T01ECU")] # note that these should not be considered as discharges - need to fix - but these are relatively few

csn_dttm = moves_1[final_location == "OTF POOL" & location == "OTF POOL"] %>% 
  select(csn, presentation_time, first_dept, min_E, max_E, min_I, max_E, admission, discharge) %>% 
  mutate(match_dttm = min_I == max_E) %>% 
  mutate(time_OTF_to_class_change = difftime(admission, max_E, units = "mins"))


# Looking at last ED discharge time ----------------------------------------


moves = moves[data.table(ED_csn_summ %>% select(csn, last_ED_discharge_time)), nomatch = 0]

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



