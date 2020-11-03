

# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)




# load data
# ============

load("~/EDcrowding/predict-admission/data-raw/matrix_csn_2020-10-14.rda")
#load("~/EDcrowding/predict-admission/data-raw/matrix_loc_2020-10-14.rda")

load("~/EDcrowding/predict-admission/data-raw/flowsheet_raw_excluded_csns_2020-10-14.rda")
load("~/EDcrowding/predict-admission/data-raw/flowsheet_real_2020-10-14.rda")
load("~/EDcrowding/predict-admission/data-raw/flowsheet_num_results_2020-10-14.rda")
load("~/EDcrowding/predict-admission/data-raw/flowsheet_num_results_with_zero_2020-10-14.rda")
load("~/EDcrowding/predict-admission/data-raw/flowsheet_num_results_with_zero_csn_level_2020-10-14.rda")

load("~/EDcrowding/predict-admission/data-raw/lab_raw_excluded_csns_2020-10-14.rda")
load("~/EDcrowding/predict-admission/data-raw/lab_real_2020-10-14.rda")
load("~/EDcrowding/predict-admission/data-raw/lab_num_results_2020-10-14.rda")
load("~/EDcrowding/predict-admission/data-raw/lab_num_results_with_zero_2020-10-14.rda")
load("~/EDcrowding/predict-admission/data-raw/lab_num_results_with_zero_csn_level_2020-10-14.rda")


load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-10-14.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_all_2020-10-14.rda")
load("~/EDcrowding/predict-admission/data-raw/demog_2020-09-21.rda")



ED_bed_moves <- ED_bed_moves %>%  ungroup() %>% 
  filter(ED_row_excl_OTF ==1) %>% 
  left_join(ED_csn_summ %>% select(csn, adm))


# explore overall distribution
# =============================

# ggplot two colours"#F8766D" "#00BFC4"

chart_title = "Distribution of age"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659) 
ED_csn_summ %>% 
  left_join(demog_raw) %>% 
  mutate(age = year(arrival_dttm) - year(birthdate))  %>% 
  ggplot(aes(age)) + geom_histogram(binwidth = 1) + 
  theme_classic() +
  labs(title = chart_title, x = "Age",
       y = "Number of encounters") +
  facet_wrap(~epoch) 

dev.off()


chart_title = "Distribution of age - under 18"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659) 
ED_csn_summ %>% 
  left_join(demog_raw) %>% 
  mutate(age = year(arrival_dttm) - year(birthdate)) %>% 
  filter(age < 18) %>% 
  ggplot(aes(age)) + geom_histogram(binwidth = 1) + 
  theme_classic() +
  labs(title = chart_title, x = "Age",
       y = "Number of encounters") +
  facet_wrap(~epoch) 

dev.off()


chart_title = "Distribution of duration in ED (visits of less than 12 hours)"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659) 

# exploring overall distribution of elapsed time
ED_csn_summ %>% mutate(duration = as.numeric(difftime(ED_discharge_dttm_excl_OTF, arrival_dttm, units = "hours"))) %>% 
  filter(duration < 12) %>% 
  ggplot(aes(duration)) + geom_histogram( binwidth = .5, fill="#898989", color="#ffffff", alpha=0.9)  +
  scale_x_continuous(breaks = seq(0,12,2)) +
  theme_classic() +
  labs(title = chart_title, x = "Duration (hours)",
       y = "Number of encounters",
       subtitle = "~2K encounters (of ~150K) have total duration of more than 12 hours") +
  facet_wrap(~ epoch)

dev.off()


# exploring overall distribution of elapsed time - with colour showing whether admitted
chart_title = "Distribution of duration in ED (visits of less than 12 hours) with colour for whether admitted"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659) 

ED_csn_summ %>% mutate(duration = as.numeric(difftime(ED_discharge_dttm_excl_OTF, arrival_dttm, units = "hours"))) %>% 
  filter(duration < 12) %>% 
  ggplot(aes(x = duration)) +   
  geom_histogram(data=subset(ED_csn_summ %>% mutate(duration = as.numeric(difftime(ED_discharge_dttm_excl_OTF, arrival_dttm, units = "hours"))) %>% 
                               filter(duration < 12),!adm), binwidth = .5,  fill = "#F8766D")   +
  geom_histogram(data=subset(ED_csn_summ %>% mutate(duration = as.numeric(difftime(ED_discharge_dttm_excl_OTF, arrival_dttm, units = "hours"))) %>% 
                               filter(duration < 12) ,adm), binwidth = .5, fill = "#00BFC4") +

  scale_x_continuous(breaks = seq(0,12,2)) +
  theme_classic() +
  labs(title = chart_title, x = "Duration (hours)",
       y = "Number of encounters",
       subtitle = "~2K encounters (of ~150K) have total duration of more than 12 hours") +
  facet_wrap(~ epoch)

dev.off()


# number of locations 
chart_title = "Distribution of number of ED locations"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659) 

ED_csn_summ %>% 
  ggplot(aes(num_ED_row_excl_OTF)) + geom_histogram(binwidth = 1, fill="#898989", color="#ffffff", alpha=0.9)  +
  theme_classic() +
  scale_x_continuous(breaks = seq(1,12,1)) +
  labs(title = chart_title, x = "Number of ED locations visited (includes repeat visits)",
       y = "Number of encounters") +
  facet_wrap(~ epoch)

dev.off()


# number of locations - with colour showing whether admitted
chart_title = "Distribution of number of ED locations - with colour for whether admitted"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659) 

ED_csn_summ %>% 
  ggplot(aes(num_ED_row_excl_OTF))  +   
  geom_histogram(data=subset(ED_csn_summ %>% mutate(duration = as.numeric(difftime(ED_discharge_dttm_excl_OTF, arrival_dttm, units = "hours"))) %>% 
                               filter(duration < 12),!adm), binwidth = .5,  fill = "#F8766D")   +
  geom_histogram(data=subset(ED_csn_summ %>% mutate(duration = as.numeric(difftime(ED_discharge_dttm_excl_OTF, arrival_dttm, units = "hours"))) %>% 
                               filter(duration < 12) ,adm), binwidth = .5, fill = "#00BFC4")  +
  theme_classic() +
  scale_x_continuous(breaks = seq(1,12,1)) +
  labs(title = chart_title, x = "Number of ED locations visited (includes repeat visits)",
       y = "Number of encounters") +
  facet_wrap(~ epoch)

dev.off()


chart_title = "Distribution of duration in Triage (where less than 10 hours)"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659) 

# exploring overall distribution of  time in triage
ED_bed_moves %>% filter(room4 == "TRIAGE") %>% 
  mutate(duration = as.numeric(difftime(discharge, admission, units = "hours"))) %>% 
  filter(duration < 10) %>% 
  ggplot(aes(duration)) + geom_histogram(binwidth = .5, fill="#898989", color="#ffffff")  +
  theme_classic() +
  scale_x_continuous(breaks = seq(0,10,1)) +
  labs(title = chart_title, x = "Duration (hours)",
       y = "Number of encounters",
       subtitle = "46 encounters (of ~150K) have Triage duration of more than 10 hours") +
  facet_wrap(~ epoch)

dev.off()


# exploring overall distribution of  time in triage - with colour

chart_title = "Distribution of duration in Triage (where less than 10 hours) - with colour showing whether admitted"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659) 


ED_bed_moves %>% filter(room4 == "TRIAGE") %>% 
  mutate(duration = as.numeric(difftime(discharge, admission, units = "hours"))) %>% 
  filter(duration < 10) %>% 
  ggplot(aes(duration)) + 
  geom_histogram(data = subset(ED_bed_moves %>% filter(room4 == "TRIAGE") %>% 
                    mutate(duration = as.numeric(difftime(discharge, admission, units = "hours"))) %>% 
                    filter(duration < 10), !adm), binwidth = .5,  fill = "#F8766D")   +
  geom_histogram(data = subset(ED_bed_moves %>% filter(room4 == "TRIAGE") %>% 
                                 mutate(duration = as.numeric(difftime(discharge, admission, units = "hours"))) %>% 
                                 filter(duration < 10), adm), binwidth = .5,  fill = "#00BFC4")   +
  theme_classic() +
  scale_x_continuous(breaks = seq(0,10,1)) +
  labs(title = chart_title, x = "Duration (hours)",
       y = "Number of encounters",
       subtitle = "46 encounters (of ~150K) have Triage duration of more than 10 hours") +
  facet_wrap(~ epoch)

dev.off()


# exploring overall distribution of elapsed time


chart_title = "Boxplot of duration in each location (for durations less than 100 hours)"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659) 

ED_bed_moves %>% 
  filter(ED_row_excl_OTF == 1) %>% 
  mutate(duration = duration_mins/60 + duration_hours + 24*duration_days) %>% 
  filter(duration < 100) %>% 
  ggplot(aes(room4, duration)) + geom_boxplot()  +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = chart_title, x = "Location",
       y = "Duration (hours)") +
  facet_wrap(~ epoch)

dev.off()


# exploring overall distribution of admission
chart_title = "Distribution of number admitted by day"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659) 

ED_csn_summ %>% 
  group_by(date(arrival_dttm), epoch) %>% summarize(num_admitted = n()) %>% 
  ggplot(aes(num_admitted)) + geom_histogram(binwidth = 25,  fill="#898989", color="#ffffff") +
  theme_classic() +
  labs(title = chart_title, x = "Number admitted",
       y = "Count of encounters") +
  facet_wrap(~ epoch)


dev.off()


# explore general factors 
# ======================

hlines <- matrix_csn %>% group_by(epoch) %>% summarise(prop_admitted = (sum(adm)/n()))

# effect of night arrival

chart_title = "Exploring relationship between admission and night time arrival"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659) 

matrix_csn %>% ungroup() %>%  
  mutate(night = ifelse(hour(arrival_dttm) < 22 & hour(arrival_dttm) > 7, 0, 1)) %>%
  mutate(night = night==1) %>% 
  group_by(adm, night, epoch) %>% summarise(num = n()) %>% 
  ggplot(aes(adm, num, fill = night)) + geom_bar(stat = "identity", position = "fill") +
  theme_classic() +
  theme(legend.position = "bottom") +
  
  labs(x = "Admitted", y = "Proportion of patients arriving at night", fill = "Nightime (10 pm to 7 am) arrival to ED",
       title = chart_title) +
  facet_wrap(~ epoch) +
  scale_fill_manual(values = c("#6dd3f8", "#a0f86d"))

dev.off()

chart_title = "Exploring relationship between admission and gender"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659)

# gender
matrix_csn %>% ungroup() %>%  
  mutate(female = sex=="FEMALE") %>% 
  group_by(adm, female, epoch) %>% summarise(num = n()) %>% 
  ggplot(aes(adm, num, fill = female)) + geom_bar(stat = "identity", position = "fill") +
  theme(panel.grid.minor = element_line(colour="black", size=0.5)) +
  scale_y_continuous(minor_breaks = seq(0 , 1, .25), breaks = seq(0 , 1, .25)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "Admitted", y = "Proportion of patients who are female", fill = "Female",
       title = chart_title) +
  facet_wrap(~ epoch) +
  scale_fill_manual(values = c("#6dd3f8", "#a0f86d"))


dev.off()

# effect of time of day arrival


chart_title = "Exploring relationship between admission and hour of arrival"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659)

matrix_csn %>%  ungroup() %>% 
  group_by(hour = hour(arrival_dttm), adm, epoch) %>%
  summarise(tot = n()) %>%
  ggplot(aes(hour, tot, fill = adm)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "Hour of arrival", y = "Proportion of patients", fill = "Admitted",
       title = chart_title,
       subtitle = "Dashed line shows proportion admitted over whole epoch") +
  facet_wrap(~ epoch) + 
  geom_hline(data = hlines, aes(yintercept = prop_admitted), linetype = "dashed")

dev.off()

# day of week

chart_title = "Exploring relationship between admission and day of week of arrival"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659)

matrix_csn %>% ungroup() %>% 
  mutate(day_of_week = 
           factor(weekdays(arrival_dttm, abbreviate = TRUE), 
                  levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>% 
  group_by(day_of_week, adm, epoch) %>% 
  summarise(tot = n()) %>% 
  ggplot(aes(day_of_week, tot, fill = adm)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "Admitted", y = "Proportion of patients", fill = "Admitted",
       title = chart_title,
       subtitle = "Dashed line shows proportion admitted over whole epoch") +
  facet_wrap(~ epoch) + 
  geom_hline(data = hlines, aes(yintercept = prop_admitted), linetype = "dashed")

dev.off()


# explore flowsheet data
# ======================

# has flowsheet results 

chart_title = "Exploring relationship between admission and presence of at least one flowsheet record"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659)

matrix_csn %>% ungroup() %>%  
  mutate(has_results = csn %in% (flowsheet_num_results_with_zero$csn)) %>% 
  group_by(adm, has_results, epoch) %>% summarise(num = n()) %>% 
  ggplot(aes(has_results, num, fill = adm)) + geom_bar(stat = "identity", position = "fill") +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "Has at least one flowsheet record", y = "Proportion of patients", fill = "Admitted",
       title = chart_title) +
  facet_wrap(~ epoch) 

dev.off()

# total number of flowsheet measurements
# need to left join with matrix to include the patients without any measurements

chart_title = "Boxplot of total number of flowsheet measurements while in ED"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659)

matrix_csn %>% ungroup() %>% 
  left_join(
    flowsheet_num_results %>% group_by(mrn, csn) %>% summarise(tot = sum(num_results))
  ) %>% 
  mutate(tot = replace_na(tot, 0)) %>% 
  ggplot(aes(adm, tot, fill = adm, col = adm)) +
  geom_boxplot(alpha = 0.4) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "Total number of measurements", color = NULL, fill = NULL, x = "Admitted",
       title = chart_title)+
  facet_wrap(~ epoch) 

dev.off()


chart_title = "Boxplot of number of different types of flowsheet measurement while in ED"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659)

# look at total number of different types of measurements
matrix_csn %>% ungroup()  %>% 
  left_join(
    flowsheet_num_results %>% ungroup() %>% group_by(mrn, csn) %>% summarise(tot = n_distinct(meas))
  ) %>% mutate(tot = replace_na(tot, 0)) %>% 
  ggplot(aes(adm, tot, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "Number of different measurements", color = NULL, fill = NULL, x = "Admitted",
       title = chart_title)+
  facet_wrap(~ epoch) 

dev.off()


# look at total number of flowsheet events


chart_title = "Boxplot of number of different flowsheet events while in ED"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659)

matrix_csn %>% ungroup()  %>% 
  left_join(
    flowsheet_raw_excluded_csns %>% ungroup() %>% group_by(mrn, csn) %>% summarise(tot = n_distinct(elapsed_mins))
  ) %>% mutate(tot = replace_na(tot, 0)) %>% 
  ggplot(aes(adm, tot, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "Number of different flowsheet events", color = NULL, fill = NULL, x = "Admitted",
       title = chart_title)+
  facet_wrap(~ epoch) 

dev.off()


# explore lab data
# ================


# has lab results

chart_title = "Exploring relationship between admission and presence of at least one lab result"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659)
           
matrix_csn %>% ungroup() %>%  
  mutate(has_results = csn %in% (lab_num_results_with_zero$csn)) %>% 
  group_by(adm, has_results, epoch) %>% summarise(num = n()) %>% 
  ggplot(aes(has_results, num, fill = adm)) + geom_bar(stat = "identity", position = "fill") +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "Has at least one lab result", y = "Proportion of patients", fill = "Admitted",
       title = chart_title)+
  facet_wrap(~ epoch) 

dev.off()




# total number of lab results

chart_title = "Boxplot of total number of lab results while in ED"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659)

matrix_csn %>% ungroup()  %>% 
  left_join(
    lab_num_results %>% group_by(mrn, csn) %>% summarise(tot = sum(num_results))
  ) %>% 
  mutate(tot = replace_na(tot, 0)) %>% 
  ggplot(aes(adm, tot, fill = adm, col = adm)) +
  geom_boxplot(alpha = 0.4) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "Total number of lab results", color = NULL, fill = NULL, x = "Admitted",
       title = chart_title)+
  facet_wrap(~ epoch) 


dev.off()



# total number of different types of lab results

chart_title = "Boxplot of number of different types of lab result while in ED"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659)

matrix_csn %>% ungroup() %>% 
  left_join(
    lab_num_results %>% ungroup() %>% group_by(mrn, csn) %>% summarise(tot = n_distinct(local_code))
  ) %>% mutate(tot = replace_na(tot, 0)) %>% 
  ggplot(aes(adm, tot, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "Number of different lab results", color = NULL, fill = NULL, x = "Admitted",
       title = chart_title)+
  facet_wrap(~ epoch) 

dev.off()

# total number of lab events

chart_title = "Boxplot of number of different lab events while in ED"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659)

matrix_csn %>% ungroup()  %>% 
  left_join(
    lab_raw_excluded_csns %>% ungroup() %>% group_by(mrn, csn) %>% summarise(tot = n_distinct(elapsed_mins))
  ) %>% mutate(tot = replace_na(tot, 0)) %>% 
  ggplot(aes(adm, tot, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "Number of different lab result events", color = NULL, fill = NULL, x = "Admitted",
       title = chart_title)+
  facet_wrap(~ epoch) 

dev.off()

# # elapsed time and lab events - within a patient, what is the elapsed time after their first lab result?
# # if anumber of lab results come close together, arguably that is one event
# # other patients might have them spread out
# 
# lab_raw_excluded_csns %>% select(mrn, csn, elapsed_mins) %>% ungroup() %>% distinct() %>% 
#   group_by(mrn, csn) %>% mutate(first_lab_time = min(elapsed_mins),
#                                                       mean_lab_time = mean(elapsed_mins, na.rm = TRUE)) %>% 
#   select(-elapsed_mins) %>% 
#   left_join(
#     matrix_csn %>% ungroup() %>% select(mrn, csn, adm)
#   ) %>%
#   distinct() %>% 
#   ggplot(aes(x = adm, y = first_lab_time)) + geom_boxplot()


# more detail on flowsheets
# =========================


# # find flowsheet measurements that are worth looking at
# flowsheet_num_results %>% group_by(meas) %>% summarise(tot = sum(num_results)) %>% 
#   mutate(m2 = fct_reorder(meas, tot)) %>% 
#   ggplot(aes(x = m2, y = tot)) + geom_bar(stat = "identity") + coord_flip() +
#   labs(y = "Number of measurements", color = NULL, fill = NULL, x = "Measurement")

# seems to be a clear split above and below 500
common <- flowsheet_num_results %>% group_by(meas) %>% summarise(tot = sum(num_results)) %>%
  filter(tot > 150000) %>% ungroup() %>% select(meas)

# uncommon <- flowsheet_num_results %>% group_by(meas) %>% summarise(tot = sum(num_results)) %>%
#   filter(tot <= 500) %>% ungroup() %>% select(meas)

# chart of number of measurements for those where > 500 measurements taken in total
# note - I have corrected this one - was not aggregating to csn level - but others may also be incorrect

matrix_flowsheet_num_results_with_zero <- matrix_csn %>% ungroup() %>% 
  left_join(flowsheet_num_results_with_zero_csn_level %>% ungroup()
) %>%   # need to fill in the NA values as zeroes for people without any flowsheet measurements
  mutate_at(vars(acvpu:morphine_dose), replace_na, 0)



chart_title = "Boxplots of number of flowsheet measurements for the 8 most commonly used measurements"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659)

matrix_flowsheet_num_results_with_zero %>%
  select(csn, adm, common$meas, epoch) %>% 
  pivot_longer(acvpu:temp, names_to = "meas", values_to = "num_results") %>% 
  ggplot(aes(adm, num_results, fill = adm, col = adm)) +
  geom_boxplot(alpha = 0.4) +
  theme(legend.position = "none") +
  facet_wrap(meas~epoch, scales = "fixed", ncol = 6) +
  labs(y = NULL, color = NULL, x = "Admitted", title = chart_title,
       subtitle = "Includes the 8 measurements used more than 150K times between April 2019 and August 2020")

dev.off()




# # chart of number of measurements for those where < 500 measurements taken in total
# 
# png("EDcrowding/predict-admission/media/Existence of unusual measurement recorded while in ED.png")
# 
# # this includes everyone but is not very useful
# matrix_flowsheet_num_results_with_zero %>%
#   select(csn, sex, adm, uncommon$meas) %>% 
#   mutate_at(vars(uncommon$meas), ~ replace(., which(.!=0), 1)) %>% 
#   pivot_longer(art_pressure_inv:vent_yes_no, names_to = "meas", values_to = "has_results") %>% 
#   group_by(meas, adm, has_results) %>% summarise(num = n()) %>% 
#   mutate(has_results= has_results == 1) %>% 
#   ggplot(aes(adm, num, fill = has_results)) +
#   geom_bar(stat = "identity", position = "fill") +
#   theme_classic() +
#   labs(x = "Admitted", y = "Proportion of patients", fill = "Has lab results") +
#   facet_wrap(~meas, scales = "free_y", ncol = 4) +
#   labs(y = NULL, color = NULL, x = "Admitted", title = "Number of measurements recorded while in ED, by type of measuremen",
#        subtitle = "Includes the 12 least frequently used measurements only")
# 
# dev.off()
  

# numerical flowsheet values
# ==========================



# note - doing this join led to me identifying 273 CSNs that exist on flowsheet_real that don't exist on matrix_csn
# this is after allowing for the new csns created by the multiple ED visit patients
# some are deleted by having an admission row later than a departure row - but how many

flowsheet_real %>% 
  filter(meas %in% common$meas) %>% 
  left_join(
    matrix_csn
  ) %>%
select(csn) %>% n_distinct()

# most common flowsheet values - some definite outliers that look like data entry errors if you compare within patient
flowsheet_real %>% filter(meas == "temp", csn == "1012852078")
flowsheet_real %>% filter(meas == "temp", csn == "1010988945")


chart_title = "Boxplots of value of flowsheet measurements for the 8 most commonly used measurements"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"), width = 1077, height = 659)

flowsheet_real %>% 
  filter(meas %in% c(common$meas, "bp_sys", "bp_dia"), meas != "acvpu") %>% 
  left_join(
    matrix_csn
  ) %>%
  filter(!is.na(epoch)) %>% 
  ggplot(aes(adm, result_as_real, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(meas~epoch, scales = "free_y", ncol = 6) +
  theme(legend.position = "none") +
  labs(y = NULL, color = NULL, x = "Admitted", title = chart_title,
       subtitle = "Includes the 8 measurements used more than 500 times between April 2019 and August 2020; excludes ACVPU as this is ordinal")

dev.off()

# # looking at acvpu
# 
# chart_title = "Exploring the relationship between ACVPU scores and admission"
# png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"))
# 
# flowsheet_real %>% 
#   filter(meas== "acvpu") %>% 
#   left_join(
#     matrix_csn,
#     by = c("mrn", "csn" = "csn_old")
#   ) %>%
#   filter(!is.na(epoch)) %>% 
#   mutate(acvpu = factor(result_as_real, levels = c(1,2,3,4,5) , labels = c("A", "C", "V", "P", "U"))) %>% 
#   group_by(acvpu, adm) %>% summarise(num = n()) %>% 
#   ggplot(aes(adm, num, fill = acvpu)) +
#   geom_bar(stat = "identity") +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   labs(y = "Number of AVPU scores recorded", fill = "ACVPU score", x = "Admitted", title = chart_title,
#        subtitle = "ACVPU: A = Alert, C = Confusion, V = Voice (not used), P = Pain, U = Unresponsive")
# 
# dev.off()


# # looking at acvpu without A
# 
# chart_title = "Exploring the relationship between ACPU scores and admission (excluding A)"
# png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"))
# 
# flowsheet_real %>% 
#   filter(meas== "acvpu", result_as_real !=1) %>% 
#   left_join(
#     matrix_csn,
#     by = c("mrn", "csn" = "csn_old")
#   ) %>% 
#   mutate(acvpu = factor(result_as_real, levels = c(1,2,3,4,5) , labels = c("A", "C", "V", "P", "U"))) %>% 
#   group_by(acvpu, adm) %>% summarise(num = n()) %>% 
#   ggplot(aes(adm, num, fill = acvpu)) +
#   geom_bar(stat = "identity", position = "fill") +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   labs(y = "Proportion of patients receiving score", fill = "ACVPU score", x = "Admitted", title = chart_title,
#        subtitle = "ACVPU: A = Alert, C = Confusion, V = Voice (not used), P = Pain, U = Unresponsive")
# 
# dev.off()



# more detail on lab results
# ==========================

# find labs that are worth looking at
l <- lab_num_results %>% group_by(local_code) %>% summarise(tot = sum(num_results)) %>% arrange(desc(tot))

# seems to be a clear split above and below 500
common_labs <- lab_num_results %>% group_by(local_code) %>% summarise(tot = sum(num_results)) %>%
  filter(tot > 12000) %>% ungroup() %>% select(local_code)

uncommon_labs <- lab_num_results %>% group_by(local_code) %>% summarise(tot = sum(num_results)) %>%
  filter(tot <= 12000) %>% ungroup() %>% select(local_code)


# chart of number of measurements for those where > 500 measurements taken in total
matrix_lab_num_results_with_zero <- matrix_csn %>% ungroup() %>%
  left_join(lab_num_results_with_zero %>%
              ungroup() %>%
              select(mrn, csn, common_labs$local_code, -fk_bed_moves)
  ) %>%   # need to fill in the NA values as zeroes for people without any flowsheet measurements
  mutate_at(vars(ALB:WCC), replace_na, 0)


png("EDcrowding/predict-admission/media/Number of labs recorded while in ED for 33 most common lab tests - Part 1.png", width = 1077, height = 659)

matrix_lab_num_results_with_zero %>%
  select(csn, adm, common_labs$local_code) %>% 
  pivot_longer(ALB:WCC, names_to = "local_code", values_to = "num_results") %>% 
  filter(local_code %in% common_labs$local_code[1:16]) %>% 
  ggplot(aes(adm, num_results, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  theme(legend.position = "none") +
  facet_wrap(~local_code, scales = "free_y", ncol = 4) +
  labs(y = NULL, color = NULL, x = "Admitted", title = "Number of lab results recorded while in ED (Part 1 of 2)",
       subtitle = "Includes the 33 most frequently used lab results only (total number of lab tests is 388)")

dev.off()

png("EDcrowding/predict-admission/media/Number of labs recorded while in ED for 33 most common lab tests - Part 2.png", width = 1077, height = 659)

matrix_lab_num_results_with_zero %>%
  select(csn, adm, common_labs$local_code) %>% 
  pivot_longer(ALB:WCC, names_to = "local_code", values_to = "num_results") %>% 
  filter(local_code %in% common_labs$local_code[17:33]) %>% 
  ggplot(aes(adm, num_results, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  theme(legend.position = "none") +
  facet_wrap(~local_code, scales = "free_y", ncol = 4) +
  labs(y = NULL, color = NULL, x = "Admitted", title = "Number of lab results recorded while in ED (Part 2 of 2)",
       subtitle = "Includes the 33 most frequently used lab results only (total number of lab tests is 388)")

dev.off()

## values of lab tests - without COVID epoch

png("EDcrowding/predict-admission/media/Values of labs recorded while in ED for 33 most common lab tests without Covid epoch - Part 1.png", width = 1077, height = 659)


lab_real %>% 
  filter(local_code %in% common_labs$local_code[1:18]) %>% 
  left_join(
    matrix_csn %>% ungroup()
  ) %>%
  filter(!is.na(adm)) %>% 
  ggplot(aes(adm, result_as_real, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~local_code, scales = "free_y", ncol = 6) +
  theme(legend.position = "none") +
  labs(y = NULL, color = NULL, x = "Admitted", title = "Values of lab results recorded while in ED (Part 1)",
       subtitle = "Includes the 33 most frequently used lab results only (total number of lab tests is 388)")
dev.off()


png("EDcrowding/predict-admission/media/Values of labs recorded while in ED for 33 most common lab tests without Covid epoch - Part 2.png", width = 1077, height = 659)


lab_real %>% 
  filter(local_code %in% common_labs$local_code[19:32]) %>% 
  left_join(
    matrix_csn %>% ungroup()
  ) %>%
  filter(!is.na(adm)) %>% 
  ggplot(aes(adm, result_as_real, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~local_code, scales = "free_y", ncol = 6) +
  theme(legend.position = "none") +
  labs(y = NULL, color = NULL, x = "Admitted", title = "Values of lab results recorded while in ED (Part 2)",
       subtitle = "Includes the 33 most frequently used lab results only (total number of lab tests is 388)")
dev.off()




## values of lab tests - breaking down by COVID epoch

png("EDcrowding/predict-admission/media/Values of labs recorded while in ED for 33 most common lab tests - Part 1.png", width = 1077, height = 659)


lab_real %>% 
  filter(local_code %in% common_labs$local_code[1:8]) %>% 
  left_join(
    matrix_csn %>% ungroup()
  ) %>%
  filter(!is.na(adm)) %>% 
  ggplot(aes(adm, result_as_real, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(local_code~epoch, scales = "free_y", ncol = 3) +
  theme(legend.position = "none") +
  labs(y = NULL, color = NULL, x = "Admitted", title = "Values of lab results recorded while in ED (Part 1)",
       subtitle = "Includes the 33 most frequently used lab results only (total number of lab tests is 388)")
dev.off()


png("EDcrowding/predict-admission/media/Values of labs recorded while in ED for 33 most common lab tests - Part 2.png", width = 1077, height = 659)

lab_real %>% ungroup() %>% 
  filter(local_code %in% common_labs$local_code[9:16]) %>% 
  left_join(
    matrix_csn %>% ungroup()
  ) %>%
  filter(!is.na(adm)) %>% 
  ggplot(aes(adm, result_as_real, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(local_code~epoch, scales = "free_y", ncol = 3) +
  theme(legend.position = "none") +
  labs(y = NULL, color = NULL, x = "Admitted", title = "Values of lab results recorded while in ED (Part 2)",
       subtitle = "Includes the 33 most frequently used lab results only (total number of lab tests is 388)")
dev.off()



png("EDcrowding/predict-admission/media/Values of labs recorded while in ED for 33 most common lab tests - Part 3.png", width = 1077, height = 659)

lab_real %>% ungroup() %>% 
  filter(local_code %in% common_labs$local_code[17:24]) %>% 
  left_join(
    matrix_csn %>% ungroup()
  ) %>%
  filter(!is.na(adm)) %>% 
  ggplot(aes(adm, result_as_real, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(local_code~epoch, scales = "free_y", ncol = 3) +
  theme(legend.position = "none") +
  labs(y = NULL, color = NULL, x = "Admitted", title = "Values of lab results recorded while in ED (Part 3)",
       subtitle = "Includes the 33 most frequently used lab results only (total number of lab tests is 388)")
dev.off()



png("EDcrowding/predict-admission/media/Values of labs recorded while in ED for 33 most common lab tests - Part 4.png", width = 1077, height = 659)

lab_real %>% ungroup() %>% 
  filter(local_code %in% common_labs$local_code[25:32]) %>% 
  left_join(
    matrix_csn %>% ungroup()
  ) %>%
  filter(!is.na(adm)) %>% 
  ggplot(aes(adm, result_as_real, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(local_code~epoch, scales = "free_y", ncol = 3) +
  theme(legend.position = "none") +
  labs(y = NULL, color = NULL, x = "Admitted", title = "Values of lab results recorded while in ED (Part 4)",
       subtitle = "Includes the 33 most frequently used lab results only (total number of lab tests is 388)")
dev.off()



lab_real_common <- lab_real %>% ungroup() %>% 
  filter(local_code %in% common_labs$local_code, !is.na(ref_low)) %>% 
  left_join(
    matrix_csn %>% ungroup()
  ) %>%
  filter(!is.na(adm))

## OUt of range lab tests

png("EDcrowding/predict-admission/media/Out of range lab results.png", width = 1077, height = 659)

lab_real_common %>% group_by(local_code, adm) %>% 
  summarise(out_of_range_high = sum(oor_high, na.rm = TRUE),
            out_of_range_low = sum(oor_low, na.rm = TRUE),
            in_range = sum(!oor_low & !oor_high, na.rm = TRUE)) %>% 
  pivot_longer(out_of_range_high:in_range, names_to = "range", values_to = "num") %>% 
  mutate(range = factor(range, levels = c("out_of_range_low", "in_range", "out_of_range_high"))) %>% 
  ggplot(aes(x = adm, y = num, fill = fct_rev(range))) + geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(breaks = c(0,0.5,1)) +
  facet_wrap(~local_code, scales = "fixed", ncol = 6) +
  theme(legend.position = "none") +
  labs(y = "Proportion of patients", fill = "Proportion that are:", x = "Admitted", title = "Proportion of lab results in and out of range",
       subtitle = "Includes the most frequently used lab results only (total number of lab tests is 388)") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("out_of_range_low" = "blue", "out_of_range_high" = "red", "in_range" = "grey"))
  

dev.off()


png("EDcrowding/predict-admission/media/Out of range lab results by epoch.png", width = 1077, height = 659)

lab_real_common %>% 
  filter(local_code %in% c("ALB", "ALP", "ALT", "AMY", "CREA", "CRP",
                           "HBGL", "HCTU", "LY", "MO", "NA", "NE")) %>% 
  group_by(local_code, adm, epoch) %>% 
  summarise(out_of_range_high = sum(oor_high, na.rm = TRUE),
            out_of_range_low = sum(oor_low, na.rm = TRUE),
            in_range = sum(!oor_low & !oor_high, na.rm = TRUE)) %>% 
  pivot_longer(out_of_range_high:in_range, names_to = "range", values_to = "num") %>% 
  ggplot(aes(x = adm, y = num, fill = fct_rev(range))) + geom_bar(stat = "identity", position = "fill") +
  
  facet_wrap(local_code~epoch, scales = "fixed", ncol = 3) +
  labs(y = "Proportion of patients", fill = "Proportion that are:", x = "Admitted", title = "Proportion of lab results in and out of range",
       subtitle = "Includes the 32 most frequently used lab results only (total number of lab tests is 388)") +
  theme(legend.position = "bottom")


dev.off()


