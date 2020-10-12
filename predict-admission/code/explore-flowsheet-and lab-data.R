

# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)



# load data
# ============


load("~/EDcrowding/predict-admission/data-raw/matrix_2020-10-07.rda")

load("~/EDcrowding/predict-admission/data-raw/flowsheet_real_2020-10-08.rda")
load("~/EDcrowding/predict-admission/data-raw/flowsheet_num_results_2020-10-08.rda")
load("~/EDcrowding/predict-admission/data-raw/flowsheet_num_results_with_zero_2020-10-08.rda")

load("~/EDcrowding/predict-admission/data-raw/lab_real_2020-10-07.rda")
load("~/EDcrowding/predict-admission/data-raw/lab_num_results_2020-10-07.rda")
load("~/EDcrowding/predict-admission/data-raw/lab_num_results_with_zero_2020-10-07.rda")




# explore general factors 
# ======================



# effect of night arrival

chart_title = "Exploring relationship between admission and night time arrival"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"))

matrix %>% ungroup() %>%  select(csn, night, adm) %>% distinct() %>% 
  mutate(night = night==1) %>% 
  group_by(adm, night) %>% summarise(num = n()) %>% 
  ggplot(aes(adm, num, fill = night)) + geom_bar(stat = "identity", position = "fill") +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "Admitted", y = "Proportion of patients", fill = "Nightime (10 pm to 7 am) arrival to ED",
       title = chart_title)

dev.off()

chart_title = "Exploring relationship between admission and gender"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"))

# gender
matrix %>% ungroup() %>%  select(csn, sex, adm) %>% distinct() %>% 
  mutate(female = sex=="FEMALE") %>% 
  group_by(adm, female) %>% summarise(num = n()) %>% 
  ggplot(aes(adm, num, fill = female)) + geom_bar(stat = "identity", position = "fill") +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "Admitted", y = "Proportion of patients", fill = "Female",
       title = chart_title)


dev.off()

# effect of time of day arrival


chart_title = "Exploring relationship between admission and hour of arrival"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"))

matrix %>%
  ungroup() %>% select(csn, arrival_dttm, adm) %>% distinct() %>% 
  group_by(hour = hour(arrival_dttm), adm) %>%
  summarise(tot = n()) %>%
  ggplot(aes(hour, tot, fill = adm)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "Hour of arrival", y = "Proportion of patients", fill = "Admitted",
       title = chart_title)

dev.off()

# day of week

chart_title = "Exploring relationship between admission and day of week of arrival"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"))

matrix %>% ungroup() %>%  select(csn, arrival_dttm, adm) %>% distinct() %>% 
  mutate(day_of_week = 
           factor(weekdays(arrival_dttm), 
                  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>% 
  group_by(day_of_week, adm) %>% 
  summarise(tot = n()) %>% 
  ggplot(aes(day_of_week, tot, fill = adm)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "Admitted", y = "Proportion of patients", fill = "Day of week of arrival to ED",
       title = chart_title)

dev.off()


# explore flowsheet data
# ======================

# has flowsheet results 

chart_title = "Exploring relationship between admission and presence of at least one flowsheet record"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"))

matrix %>% ungroup() %>%  select(csn, adm) %>% distinct() %>% 
  mutate(has_results = csn %in% (flowsheet_num_results_with_zero$csn)) %>% group_by(adm, has_results) %>% summarise(num = n()) %>% 
  ggplot(aes(adm, num, fill = has_results)) + geom_bar(stat = "identity", position = "fill") +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "Admitted", y = "Proportion of patients", fill = "Has at least one flowsheet record",
       title = chart_title)

dev.off()


# has lab results

chart_title = "Exploring relationship between admission and \npresence of at least one lab result"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"))
           
matrix %>% ungroup() %>%  select(csn, adm) %>% distinct() %>% 
  mutate(has_results = csn %in% (lab_num_results_with_zero$csn)) %>% group_by(adm, has_results) %>% summarise(num = n()) %>% 
  ggplot(aes(adm, num, fill = has_results)) + geom_bar(stat = "identity", position = "fill") +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(x = "Admitted", y = "Proportion of patients", fill = "Has at least one lab result",
       title = chart_title)

dev.off()


# look at total number of measurements
# need to left join with matrix to include the patients without any measurements

chart_title = "Boxplot of total number of flowsheet measurements while in ED"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"))

matrix %>% ungroup() %>% select(csn, sex, adm) %>% distinct() %>% 
  left_join(
    flowsheet_num_results %>% group_by(csn) %>% summarise(tot = sum(num_results))
    ) %>% 
  mutate(tot = replace_na(tot, 0)) %>% 
  ggplot(aes(adm, tot, fill = adm, col = adm)) +
  geom_boxplot(alpha = 0.4) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "Total number of measurements", color = NULL, fill = NULL, x = "Admitted",
       title = chart_title)

dev.off()

# look at total number of lab results

chart_title = "Boxplot of total number of lab results while in ED"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"))

matrix %>% ungroup() %>% select(csn, sex, adm) %>% distinct() %>% 
  left_join(
    lab_num_results %>% group_by(csn) %>% summarise(tot = sum(num_results))
  ) %>% 
  mutate(tot = replace_na(tot, 0)) %>% 
  ggplot(aes(adm, tot, fill = adm, col = adm)) +
  geom_boxplot(alpha = 0.4) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "Total number of lab results", color = NULL, fill = NULL, x = "Admitted",
       title = chart_title)


dev.off()

chart_title = "Boxplot of number of different types of flowsheet measurement while in ED"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"))

# look at total number of different types of measurements
matrix %>% ungroup() %>% select(csn, sex, adm) %>% distinct() %>% 
  left_join(
    flowsheet_num_results %>% ungroup() %>% group_by(csn) %>% summarise(tot = n_distinct(meas)) 
  ) %>% mutate(tot = replace_na(tot, 0)) %>% 
  ggplot(aes(adm, tot, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "Number of different measurements", color = NULL, fill = NULL, x = "Admitted",
       title = chart_title)
dev.off()


# look at total number of different types of lab results

chart_title = "Boxplot of number of different types of lab result while in ED"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"))

matrix %>% ungroup() %>% select(csn, sex, adm) %>% distinct() %>% 
  left_join(
    lab_num_results %>% ungroup() %>% group_by(csn) %>% summarise(tot = n_distinct(local_code)) 
  ) %>% mutate(tot = replace_na(tot, 0)) %>% 
  ggplot(aes(adm, tot, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "Number of different lab results", color = NULL, fill = NULL, x = "Admitted",
       title = chart_title)

dev.off()

# more detail on flowsheets
# =========================


# # find flowsheet measurements that are worth looking at
# flowsheet_num_results %>% group_by(meas) %>% summarise(tot = sum(num_results)) %>% 
#   mutate(m2 = fct_reorder(meas, tot)) %>% 
#   ggplot(aes(x = m2, y = tot)) + geom_bar(stat = "identity") + coord_flip() +
#   labs(y = "Number of measurements", color = NULL, fill = NULL, x = "Measurement")

# seems to be a clear split above and below 500
common <- flowsheet_num_results %>% group_by(meas) %>% summarise(tot = sum(num_results)) %>%
  filter(tot > 500) %>% ungroup() %>% select(meas)

uncommon <- flowsheet_num_results %>% group_by(meas) %>% summarise(tot = sum(num_results)) %>%
  filter(tot <= 500) %>% ungroup() %>% select(meas)

# chart of number of measurements for those where > 500 measurements taken in total
matrix_flowsheet_num_results_with_zero <- matrix %>% ungroup() %>% select(mrn, csn, adm, sex) %>% distinct() %>% 
  left_join(flowsheet_num_results_with_zero %>% ungroup() %>% select(-fk_bed_moves)
) %>%   # need to fill in the NA values as zeroes for people without any flowsheet measurements
  mutate_at(vars(acvpu:morphine_dose), replace_na, 0)



chart_title = "Boxplot of number of flowsheet measurements recorded while in ED for the 16 most commonly used measurements"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"))

matrix_flowsheet_num_results_with_zero %>%
  select(csn, sex, adm, common$meas) %>% 
  pivot_longer(acvpu:temp, names_to = "meas", values_to = "num_results") %>% 
  ggplot(aes(adm, num_results, fill = adm, col = adm)) +
  geom_boxplot(alpha = 0.4) +
  theme(legend.position = "none") +
  facet_wrap(~meas, scales = "free_y", ncol = 4) +
  labs(y = NULL, color = NULL, x = "Admitted", title = chart_title,
       subtitle = "Includes the 16 measurements used more than 500 times between April 2019 and August 2020")

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

flowsheet_real %>% 
  ungroup() %>% 
  count(meas) %>% arrange(n)



png("EDcrowding/predict-admission/media/Value of measurements recorded while in ED and whether admitted.png")

# note - doing this join led to me identifying 428 CSNs that exist on flowsheet_real that don't exist on matrix
# some are solved by the split of csn where 2 ED visits in same csn - but how many 
# some are deleted by having an admission row later than a departure row - but how many

flowsheet_real %>% 
  filter(meas %in% common$meas) %>% 
  left_join(
    matrix %>% select(csn, age, sex, adm, weekend, night) %>% distinct()
  ) %>%
select(csn) %>% n_distinct()

# continuing with the join

chart_title = "Boxplot of value of flowsheet measurements recorded while in ED for the 16 most commonly used measurements"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"))

flowsheet_real %>% 
  filter(meas %in% c(common$meas, "bp_sys", "bp_dia"), meas != "acvpu") %>% 
  left_join(
    matrix %>% select(csn, age, sex, adm, weekend, night) %>% distinct()
  ) %>%
  filter(!is.na(adm)) %>% 
  ggplot(aes(adm, result_as_real, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~meas, scales = "free_y", ncol = 4) +
  theme(legend.position = "none") +
  labs(y = NULL, color = NULL, x = "Admitted", title = chart_title,
       subtitle = "Includes the 16 measurements used more than 500 times between April 2019 and August 2020; excludes ACVPU as this is ordinal")

dev.off()

# looking at acvpu

chart_title = "Exploring the relationship between ACVPU scores and admission"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"))

flowsheet_real %>% 
  filter(meas== "acvpu") %>% 
  left_join(
    matrix %>% select(csn, age, sex, adm, weekend, night) %>% distinct()
  ) %>%
  filter(!is.na(adm)) %>% 
  mutate(acvpu = factor(result_as_real, levels = c(1,2,3,4,5) , labels = c("A", "C", "V", "P", "U"))) %>% 
  group_by(acvpu, adm) %>% summarise(num = n()) %>% 
  ggplot(aes(adm, num, fill = acvpu)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(y = "Number of AVPU scores recorded", fill = "ACVPU score", x = "Admitted", title = chart_title,
       subtitle = "ACVPU: A = Alert, C = Confusion, V = Voice (not used), P = Pain, U = Unresponsive")

dev.off()


# looking at acvpu without A

chart_title = "Exploring the relationship between ACPU scores and admission (excluding A)"
png(paste0("EDcrowding/predict-admission/media/", chart_title, ".png"))

flowsheet_real %>% 
  filter(meas== "acvpu", result_as_real !=1) %>% 
  left_join(
    matrix %>% select(csn, age, sex, adm, weekend, night) %>% distinct()
  ) %>%
  filter(!is.na(adm)) %>% 
  mutate(acvpu = factor(result_as_real, levels = c(1,2,3,4,5) , labels = c("A", "C", "V", "P", "U"))) %>% 
  group_by(acvpu, adm) %>% summarise(num = n()) %>% 
  ggplot(aes(adm, num, fill = acvpu)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(y = "Proportion of patients receiving score", fill = "ACVPU score", x = "Admitted", title = chart_title,
       subtitle = "ACVPU: A = Alert, C = Confusion, V = Voice (not used), P = Pain, U = Unresponsive")

dev.off()



# more detail on lab results
# ==========================

# find labs that are worth looking at
l <- lab_num_results %>% group_by(local_code) %>% summarise(tot = sum(num_results)) %>% arrange(desc(tot))

# seems to be a clear split above and below 500
common_labs <- lab_num_results %>% group_by(local_code) %>% summarise(tot = sum(num_results)) %>%
  filter(tot > 10000) %>% ungroup() %>% select(local_code)

uncommon_labs <- lab_num_results %>% group_by(local_code) %>% summarise(tot = sum(num_results)) %>%
  filter(tot <= 10000) %>% ungroup() %>% select(local_code)

# chart of number of measurements for those where > 500 measurements taken in total
matrix_lab_num_results_with_zero <- matrix %>% ungroup() %>% select(csn, adm) %>% distinct() %>% 
  left_join(lab_num_results_with_zero %>%
              ungroup() %>%
              select(csn, common_labs$local_code, -fk_bed_moves)
  ) %>%   # need to fill in the NA values as zeroes for people without any flowsheet measurements
  mutate_at(vars(ALB:WCC), replace_na, 0)


png("EDcrowding/predict-admission/media/Number of labs recorded while in ED for 33 most common lab tests - Part 1.png")

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

png("EDcrowding/predict-admission/media/Number of labs recorded while in ED for 33 most common lab tests - Part 2.png")

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

## values of lab tests

png("EDcrowding/predict-admission/media/Values of labs recorded while in ED for 33 most common lab tests - Part 1.png")


lab_real %>% 
  filter(local_code %in% common_labs$local_code[1:16]) %>% 
  left_join(
    matrix %>% select(csn, age, sex, adm, weekend, night) %>% distinct()
  ) %>%
  filter(!is.na(adm)) %>% 
  ggplot(aes(adm, result_as_real, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~local_code, scales = "free_y", ncol = 4) +
  theme(legend.position = "none") +
  labs(y = NULL, color = NULL, x = "Admitted", title = "Values of lab results recorded while in ED (Part 1 of 2)",
       subtitle = "Includes the 33 most frequently used lab results only (total number of lab tests is 388)")
dev.off()


png("EDcrowding/predict-admission/media/Values of labs recorded while in ED for 33 most common lab tests - Part 2.png")

lab_real %>% ungroup() %>% 
  filter(local_code %in% common_labs$local_code[17:33]) %>% 
  left_join(
    matrix %>% select(csn, age, sex, adm, weekend, night) %>% distinct()
  ) %>%
  filter(!is.na(adm)) %>% 
  ggplot(aes(adm, result_as_real, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~local_code, scales = "free_y", ncol = 4) +
  theme(legend.position = "none") +
  labs(y = NULL, color = NULL, x = "Admitted", title = "Values of lab results recorded while in ED (Part 2 of 2)",
       subtitle = "Includes the 33 most frequently used lab results only (total number of lab tests is 388)")
dev.off()

lab_real_common <- lab_real %>% ungroup() %>% 
  filter(local_code %in% common_labs$local_code) %>% 
  left_join(
    matrix %>% select(csn, adm) %>% distinct()
  ) %>%
  filter(!is.na(adm))

## OUt of range lab tests
r1 = lab_real_common %>% filter(oor_high) %>% group_by(local_code, adm) %>% summarise(out_of_range_high = n()) # this works
r2 = lab_real_common %>% filter(oor_low) %>% group_by(local_code) %>% summarise(out_of_range_low = n()) # this works

oor_df = r1 %>% full_join(r2) 

png("EDcrowding/predict-admission/media/Out of range lab results.png")

oor_df  %>%
  pivot_longer(out_of_range_high:out_of_range_low, names_to = "range", values_to = "num") %>% 
  ggplot(aes(adm, num, fill = range)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~local_code, scales = "free_y", ncol = 4) +
  theme(legend.position = "bottom") +
  labs(y = NULL, color = NULL, x = "Admitted", title = "Out of range lab results", 
       fill = "Proportion that are:",
       subtitle = "Includes the most frequently used lab results that have reference ranges")

dev.off()



# but this doesn't work
l = lab_real %>% group_by(local_code) %>% 
  summarise(total_high = sum(isTRUE(oor_high)),
          total_not_high = sum(isFALSE(oor_high)))


z = lab_real %>%  mutate(oor_high2 = ifelse(oor_high, 1, 0),
                    oor_low2 = ifelse(oor_low, 1, 0)) 

z %>% ungroup() %>% select(local_code, oor_low2) %>% 
  group_by(local_code) %>% summarise(
                                     b = sum(oor_low2))


