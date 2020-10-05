

# load libraries
# =============

library(dplyr)
library(lubridate)
library(tidyverse)



# load data
# ============


load("~/EDcrowding/predict-admission/data-raw/matrix_2020-09-28.rda")
load("~/EDcrowding/predict-admission/data-raw/flowsheet_real_2020-10-05.rda")
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-09-30.rda")
load("~/EDcrowding/predict-admission/data-raw/flowsheet_num_results_2020-10-05.rda")
load("~/EDcrowding/predict-admission/data-raw/lab_num_results_2020-10-05.rda")


matrix <- matrix %>% 
  mutate(weekend = ifelse(weekdays(arrival_dttm, abbreviate = TRUE) %in% c("Sat", "Sun"), 1, 0))

matrix <- matrix %>% 
  mutate(night = ifelse(hour(arrival_dttm) < 22 & hour(arrival_dttm) > 7, 0, 1))


# explore general factors
# ======================

# effect of weekend arrival
matrix %>% 
  ungroup() %>% 
  group_by(weekend, adm) %>% 
  summarise(tot = n()) %>% 
  ggplot(aes(as.character(weekend), tot, fill = adm, color = adm)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.4) +
  labs(y = NULL, color = NULL, fill = NULL, x = "Weekend arrival")

# effect of time of day arrival

matrix %>% 
  ungroup() %>% 
  group_by(night, adm) %>% 
  summarise(tot = n()) %>% 
  ggplot(aes(as.character(night), tot, fill = adm, color = adm)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.4) +
  labs(y = NULL, color = NULL, fill = NULL, x = "Nightime arrival")

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


# try looking at number of results overall

# 
# flowsheet_num_results <- flowsheet_num_results %>% 
#   left_join(ED_csn_summ %>% select(csn, arrival_dttm)) %>% 
#   filter(date(arrival_dttm) <= index_date, date(arrival_dttm) >= prior_date)

df2 <- flowsheet_num_results %>% left_join(
  matrix %>% select(age, sex, adm, weekend, night)
)

df2 <- df2 %>% distinct()


png("EDcrowding/predict-admission/media/Number of measurements recorded while in ED and whether admitted.png")

df2 %>%
  filter(sex != "UNKNOWN") %>% 
  ggplot(aes(sex, num_results, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~mapped_name, scales = "free_y", ncol = 4) +
  labs(y = NULL, color = NULL, fill = "Admitted", title = "Number of measurements recorded while in ED")

dev.off()


# add elapsed time of result to flowsheet raw
flowsheet_raw <- flowsheet_raw %>%
  left_join(ED_csn_summ %>% select(mrn, csn, arrival_dttm)) %>% 
  mutate(elapsed_mins = as.numeric(difftime(flowsheet_datetime, arrival_dttm, units = "mins"))) %>% 
  select(-arrival_dttm)


flowsheet_num_results_1hr <- flowsheet_raw %>% 
  filter(elapsed_mins <= 60) %>% 
  group_by(mrn, csn, fk_bed_moves, mapped_name) %>% 
  summarise(num_results = n())

df3 <- flowsheet_num_results_1hr %>% left_join(
  matrix %>% select(age, sex, adm, weekend, night)
) %>% distinct()

png("EDcrowding/predict-admission/media/Number of measurements recorded in firrt hour of ED and whether admitted.png")

df3 %>%
  filter(sex != "UNKNOWN") %>% 
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

include <- lab_num_results %>% filter(!is.na(mapped_name)) %>% ungroup() %>%
  count(mapped_name) %>% filter(n > 1000) %>% select(mapped_name)


df4 <- lab_num_results %>%
  filter(mapped_name %in% include$mapped_name) %>% 
  left_join(
  matrix %>% select(age, sex, adm, weekend, night)
)  %>% distinct()

png("EDcrowding/predict-admission/media/Number of lab results returned while ED and whether admitted.png")

df4 %>%
  filter(sex != "UNKNOWN") %>% 
  ggplot(aes(sex, num_results, fill = adm, color = adm)) +
  geom_boxplot(alpha = 0.4) +
  facet_wrap(~mapped_name, scales = "free_y", ncol = 4) +
  labs(y = NULL, color = NULL, fill = "Admitted", title = "Number of lab results while in ED")


dev.off()
