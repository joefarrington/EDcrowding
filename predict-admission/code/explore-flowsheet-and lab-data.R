

# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)



# load data
# ============


load("~/EDcrowding/predict-admission/data-raw/matrix_2020-10-07.rda")

load("~/EDcrowding/predict-admission/data-raw/flowsheet_real_2020-10-07.rda")
load("~/EDcrowding/predict-admission/data-raw/flowsheet_num_results_2020-10-07.rda")
load("~/EDcrowding/predict-admission/data-raw/flowsheet_num_results_with_zero_2020-10-07.rda")

load("~/EDcrowding/predict-admission/data-raw/lab_real_2020-10-07.rda")
load("~/EDcrowding/predict-admission/data-raw/lab_num_results_2020-10-07.rda")
load("~/EDcrowding/predict-admission/data-raw/lab_num_results_with_zero_2020-10-07.rda")




# explore general factors 
# ======================

# something like this
matrix %>% 
  ungroup() %>% 
  mutate(weekend = if_else(weekend == 0, "Weekday", "Weekend"), 
         night = if_else(night == 0, "Daytime", "Nightime")) %>% 
  select(csn, weekend, night, sex, adm) %>% 
  pivot_longer(weekend:sex, names_to = "var", values_to = "tot") %>% 
  ggplot(aes(var, tot, fill = adm, color = adm)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.4) +
  labs(y = NULL, color = NULL, fill = NULL, x = "Hour of arrival")


# OR

# effect of weekend arrival
weekend <- matrix %>% 
  ungroup() %>% 
  group_by(weekend, adm) %>% 
  summarise(tot = n()) %>% 
  rename(prop = weekend) %>% 
  mutate(prop = as.character(prop)) %>%
  mutate(var = "Weekend arrival")


# effect of might arrival

night <- matrix %>% 
  ungroup() %>% 
  group_by(night, adm) %>% 
  summarise(tot = n()) %>% 
  rename(prop = night) %>% 
  mutate(prop = as.character(prop))  %>%
  mutate(var = "Nightime arrival")


sex <- matrix %>% 
  filter(sex != "UNKNOWN") %>% 
  ungroup() %>% 
  group_by(sex, adm) %>% 
  summarise(tot = n()) %>% 
  rename(prop = sex) %>% 
  mutate(prop = if_else(prop == "FEMALE", "1", "0"))  %>%
  mutate(var = "Female patient")


# then to plot
weekend %>% bind_rows(night, sex) %>% 
  ggplot(aes(var, tot, fill = prop)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.4) +
  labs(y = NULL, color = NULL, fill = NULL, x = "Proportion of arrivals")

# effect of time of day arrival


matrix %>% 
  ungroup() %>% 
  group_by(hour = hour(arrival_dttm), adm) %>% 
  summarise(tot = n()) %>% 
  ggplot(aes(hour, tot, fill = adm, color = adm)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.4) +
  labs(y = NULL, color = NULL, fill = NULL, x = "Hour of arrival")

# explore flowsheet data
# ======================

# how many people don't have measurements at all? 

matrix %>% select(csn) %>% anti_join(flowsheet_num_results_with_zero %>% select(csn)) %>% n_distinct() 

matrix %>% select(csn) %>% left_join(flowsheet_num_results_with_zero %>% select(csn)) %>% n_distinct() 


# could do same for labs



# look at total number of measurements
# need to left join with matrix to include the patients without any measurements

png("EDcrowding/predict-admission/media/Number of measurements while in ED.png")

p1 <- matrix %>% ungroup() %>% select(csn, sex, adm) %>% distinct() %>% 
  left_join(
  flowsheet_raw %>% ungroup() %>% 
    group_by(csn) %>% 
    summarise(tot = n()) 
  ) %>% mutate(tot = replace_na(tot, 0)) %>% 
  ggplot(aes(sex, tot, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  labs(y = "Total number of measurements", color = NULL, fill = "Admitted", x = NULL,
       title = "Total number of measurements while in ED")

p1
dev.off()

png("EDcrowding/predict-admission/media/Number of different types of measurement while in ED.png")

# look at total number of different types of measurements
p2 <- matrix %>% ungroup() %>% select(csn, sex, adm) %>% distinct() %>% 
  left_join(
    flowsheet_raw %>% ungroup() %>% 
      group_by(csn) %>% summarise(tot = n_distinct(mapped_name)) 
  ) %>% mutate(tot = replace_na(tot, 0)) %>% 
  filter(sex != "UNKNOWN") %>% 
  ggplot(aes(sex, tot, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  labs(y = "Number of different measurements", color = NULL, fill = NULL, x = NULL,
       title = "Number of different types of measurement while in ED")
p2
dev.off()


# find flowsheet measurements that are worth looking at
flowsheet_num_results %>% group_by(mapped_name) %>% summarise(tot = sum(num_results)) %>% 
  mutate(m2 = fct_reorder(mapped_name, tot)) %>% 
  ggplot(aes(x = m2, y = tot)) + geom_bar(stat = "identity") + coord_flip() +
  labs(y = "Number of measurements", color = NULL, fill = NULL, x = "Measurement")

# create a dataframe of num_results which is wide
flow_num_results_wide <- flowsheet_num_results %>% 
  ungroup() %>% 
  group_by(mrn, csn, mapped_name) %>% # need to exclude bed_moves
  summarise(tot = sum(num_results)) %>%
  pivot_wider(names_from = mapped_name, values_from = tot, values_fill = 0)

# seems to be a clear split above and below 500
common <- flowsheet_num_results %>% group_by(mapped_name) %>% summarise(tot = sum(num_results)) %>%
  filter(tot > 500) %>% ungroup() %>% select(mapped_name)

uncommon <- flowsheet_num_results %>% group_by(mapped_name) %>% summarise(tot = sum(num_results)) %>%
  filter(tot <= 500) %>% ungroup() %>% select(mapped_name)

# chart of number of measurements for those where > 500 measurements taken in total
df2 <- matrix %>% ungroup() %>% select(mrn, csn, adm, sex) %>% distinct() %>% 
  left_join(flow_num_results_wide
) %>%   # need to fill in the NA values as zeroes for people without any flowsheet measurements
  mutate_at(vars(acvpu:morphine_pca_total_hourly_dose), replace_na, 0)




# matrix %>% ungroup() %>% select(csn, sex, adm) %>% distinct() %>% 
#   left_join(flowsheet_num_results %>% ungroup() %>% select(-mrn, -fk_bed_moves) %>% 
#               filter(mapped_name %in% common$mapped_name) %>% 
#               pivot_wider(names_from = mapped_name, values_from = num_results, values_fill = 0)) %>% 
#   pivot_longer(acvpu:`non-invasive_mean_arterial_pressure`, names_to = "mapped_name", values_to = "num_results") %>% 
#   

png("EDcrowding/predict-admission/media/Number of measurements recorded while in ED for 16 most common types, by type of measurement.png")

df2 %>%
  select(csn, sex, adm, common$mapped_name) %>% 
  filter(sex != "UNKNOWN") %>% 
  pivot_longer(acvpu:respiratory_rate, names_to = "mapped_name", values_to = "num_results") %>% 
  ggplot(aes(sex, num_results, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~mapped_name, scales = "free_y", ncol = 4) +
  labs(y = NULL, color = NULL, fill = "Admitted", title = "Number of measurements recorded while in ED, by type of measuremen",
       subtitle = "Includes the 16 most frequently used measurements only")

dev.off()

# chart of number of measurements for those where < 500 measurements taken in total

# trying to get this to be able to show the number of people in total, who don't have these measurements
# could try using flowsheet_num_results_wide but not including the non-measured people
# but that is still 121K rows instead of 3K rows

# # tried this approach to show distribution of measurements but doesn't work
# df2 %>% 
#   select(mrn, csn, sex, adm, uncommon$mapped_name)  %>% 
#   mutate_at(vars(uncommon$mapped_name), ~ replace(., which(.!=0), 1)) %>% 
#   pivot_longer(uncommon$mapped_name, names_to = "mapped_name", values_to = "has_meas") %>% 
#   group_by(mapped_name, has_meas, adm) %>% summarise(tot = n()) %>% 
#   ggplot(aes(as.character(has_meas), tot, fill = adm, color = adm)) +
#   geom_boxplot(alpha = 0.4) +
#   facet_wrap(~mapped_name, scales = "free_y", ncol = 4) +
#   labs(y = NULL, color = NULL, fill = "Admitted", title = "Had this measurement recorded at least once while in ED",
#        subtitle = "Includes the 10 least frequently used measurements only")

# note df2a only has 3000 rows - ie only includes those with these measurements
df2a <- flowsheet_num_results %>% 
  filter(!mapped_name %in% common$mapped_name) %>% ungroup() %>% 
  group_by(mrn, csn, mapped_name) %>% # need to exclude bed_moves
  summarise(tot = n()) %>% 
  pivot_wider(names_from = mapped_name, values_from = tot, values_fill = 0)  %>% 
  pivot_longer(tidal_volume:morphine_pca_total_hourly_dose, names_to = "mapped_name", values_to = "has_meas") %>% 
  left_join(
    matrix %>% select(adm, sex) %>% distinct()
  )


  
  
png("EDcrowding/predict-admission/media/Existence of unusual measurement recorded while in ED.png")


df2a %>%
  filter(sex != "UNKNOWN") %>% 
  ggplot(aes(sex, has_meas, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~mapped_name, scales = "free_y", ncol = 4) +
  labs(y = NULL, color = NULL, fill = "Admitted", title = "Had this measurement recorded at least once while in ED",
       subtitle = "Includes the 10 least frequently used measurements only")

dev.off()


# add elapsed time of result to flowsheet raw
flowsheet_raw <- flowsheet_raw %>%
  left_join(ED_csn_summ %>% select(mrn, csn, arrival_dttm)) %>% 
  mutate(elapsed_mins = as.numeric(difftime(flowsheet_datetime, arrival_dttm, units = "mins"))) %>% 
  select(-arrival_dttm)

flowsheet_raw <- flowsheet_raw %>%
  mutate(mapped_name = tolower(gsub(" ","_", mapped_name))) 
  

flowsheet_num_results_1hr <- flowsheet_raw %>% 
  filter(elapsed_mins <= 60) %>% 
  group_by(mrn, csn, fk_bed_moves, mapped_name) %>% 
  summarise(num_results = n())


flowsheet_num_results_1hr_with_zero <- flowsheet_num_results_1hr %>% 
  pivot_wider(names_from = mapped_name, values_from = num_results) 

# replace NAs with zero
flowsheet_num_results_1hr_with_zero <- flowsheet_num_results_1hr_with_zero %>%
  mutate_at(vars(colnames(flowsheet_num_results_1hr_with_zero)[4:ncol(flowsheet_num_results_1hr_with_zero)]), replace_na, 0)


df3 <- matrix %>% ungroup() %>% select(csn, sex, adm) %>% distinct() %>% 
  left_join(
    flowsheet_num_results_1hr_with_zero
  )

png("EDcrowding/predict-admission/media/Number of measurements recorded in first hour of ED and whether admitted.png")

df3 %>%
  filter(sex != "UNKNOWN") %>% 
  pivot_longer(blood_pressure:tidal_volume, names_to = "mapped_name", values_to = "num_results") %>% 
  ggplot(aes(sex, num_results, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~mapped_name, scales = "free_y", ncol = 4) +
  labs(y = NULL, color = NULL, fill = "Admitted", title = "Number of measurements recorded in first hour")


dev.off()


# numerical flowsheet values

flowsheet_real %>% 
  ungroup() %>% 
  filter(!grepl("o2_delivery", mapped_name),
         !grepl("morphine_pca", mapped_name),
         !grepl("inspiratory", mapped_name),
         !grepl("expiratory", mapped_name),
         !grepl("tidal_volume", mapped_name),
         !grepl("non_verbal", mapped_name)) %>% 
  count(mapped_name) %>% arrange(n)


df <- flowsheet_real %>% 
  ungroup() %>% 
  filter(!grepl("o2_delivery", mapped_name),
         !grepl("morphine_pca", mapped_name),
         !grepl("inspiratory", mapped_name),
         !grepl("expiratory", mapped_name),
         !grepl("tidal_volume", mapped_name),
         !grepl("non_verbal", mapped_name)) %>% 
  left_join(
  matrix %>% select(age, sex, adm, weekend, night) %>% distinct()
)

png("EDcrowding/predict-admission/media/Value of measurements recorded while in ED and whether admitted.png")


df %>%
  filter(sex != "UNKNOWN") %>% 
  ggplot(aes(sex, result_as_real, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~mapped_name, scales = "free_y", ncol = 4) +
  labs(y = NULL, color = NULL, fill = "Admitted", title = "Value of measurements recorded while in ED and whether admitted")

dev.off()

df %>%
  ggplot(aes(sex, age, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  labs(y = NULL, color = NULL, fill = NULL)


# explore lab data
# ================


# try looking at number of results overall

lab_num_results_with_zero <- lab_num_results %>% 
  pivot_wider(names_from = mapped_name, values_from = num_results)


# replace NAs with zero
lab_num_results_with_zero <- lab_num_results_with_zero %>%
  mutate_at(vars(colnames(lab_num_results_with_zero)[4:ncol(lab_num_results_with_zero)]), replace_na, 0)

df4 <- lab_num_results_with_zero %>% 
  
  left_join(
    matrix %>% select(age, sex, adm, weekend, night) %>% distinct()
  )


include <- lab_num_results_with_zero %>% 
  pivot_longer(colnames(lab_num_results_with_zero)[4:ncol(lab_num_results_with_zero)], names_to = "mapped_name", values_to = "num_results") %>% 
  ungroup() %>%
  count(mapped_name) %>% filter(n > 1000) %>% select(mapped_name)


df4 <- lab_num_results_with_zero %>%
  pivot_longer(colnames(lab_num_results_with_zero)[4:ncol(lab_num_results_with_zero)], names_to = "mapped_name", values_to = "num_results") %>% 
  filter(mapped_name %in% include$mapped_name) %>% 
  left_join(
  matrix %>% select(age, sex, adm, weekend, night) %>% distinct()
)  

png("EDcrowding/predict-admission/media/Number of lab results returned while ED and whether admitted.png")

df4 %>%
  filter(sex != "UNKNOWN") %>% 
  ggplot(aes(sex, num_results, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~mapped_name, scales = "free_y", ncol = 4) +
  labs(y = NULL, color = NULL, fill = "Admitted", title = "Number of lab results while in ED")


dev.off()
