# Load libraries ----------------------------------------------------------


library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)



# Load data ---------------------------------------------------------------


file_date = '2021-05-17'

load(paste0("~/EDcrowding/flow-mapping/data-raw/summ_", file_date,".rda"))

summ[, in_set := case_when(first_ED_admission < '2019-11-19 00:00:00' ~ "Train",
                           first_ED_admission < '2019-12-13 00:00:00' ~ "Val",
                           first_ED_admission < '2020-03-19 00:00:00' ~ "Test",
                           first_ED_admission < '2020-12-01 00:00:00' ~ "Train",
                           first_ED_admission < '2020-12-29 00:00:00' ~ "Val",
                           first_ED_admission < '2021-05-01 00:00:00' ~ "Test",
                           TRUE ~ "After")]

summ[, epoch := case_when(date(first_ED_admission) < '2020-03-19' ~ "Pre",
                      date(first_ED_admission) < '2021-05-01' ~ "Post",
                      TRUE ~ "After")]

summ[, left_ED := coalesce(first_outside_proper_admission, last_inside_discharge)]


# Load previous version of file -------------------------------------------


tta_prob_file = "~/EDcrowding/real-time/data-raw/tta_prob.rda"

if (file.exists(tta_prob_file)) {
  load(tta_prob_file)
  save(tta_prob, file = paste0("~/EDcrowding/real-time/data-raw/poisson_means_archived_on_",Sys.Date(),".rda"))
}

# tta_prob_old = tta_prob

# Generate time points  ------------------------------

start_of_set = as.POSIXct('2019-05-01', tz = "UTC")
end_of_set = as.POSIXct('2021-05-01', tz = "UTC")
next_dt = start_of_set

time_pts = POSIXct()
# each sample to be more than 12 hours and less than 24 hours after the previous one
while (next_dt < end_of_set) {
  next_pt <- next_dt + c(hours(6), hours(12), hours(16), hours(22))
  time_pts <- c(time_pts, next_pt)
  next_dt = next_dt + days(1)
}




# Summarise number not in who get admitted later --------------------------


# get all later-admitted patients in ED at that point in time
in_ED_at_time_pt_all = data.table()

for (i in 1:length(time_pts)) {
  
  if (i %% 100 == 0) {
    print(paste("Processed ", i, " dates"))
  }
  
  in_ED_at_time_pt = summ[first_ED_admission < time_pts[i] &  first_outside_proper_admission > time_pts[i] 
                          & adm %in% c("indirect_adm", "direct_adm"), 
                          .(csn, epoch, in_set, first_ED_admission = with_tz(first_ED_admission, tzone = "UTC"), 
                            first_outside_proper_admission = with_tz(first_outside_proper_admission, tzone = "UTC"))]
  
  in_ED_at_time_pt[, time_pt := with_tz(time_pts[i], tzone = "UTC")]
  
  in_ED_at_time_pt_all = bind_rows(in_ED_at_time_pt_all, in_ED_at_time_pt)
}

in_ED_at_time_pt_all[, ED_duration := difftime(first_outside_proper_admission, first_ED_admission, units = "mins")]
in_ED_at_time_pt_all[, ED_duration_so_far := difftime(time_pt, first_ED_admission, units = "mins")]

in_ED_at_time_pt_all[,timeslice := case_when(ED_duration_so_far < 15 ~ 0,
                                             ED_duration_so_far >= 15 & ED_duration_so_far < 30 ~ 15,
                                             ED_duration_so_far >= 30 & ED_duration_so_far < 60 ~ 30,
                                             ED_duration_so_far >= 60 & ED_duration_so_far < 90 ~ 60,
                                             ED_duration_so_far >= 90 & ED_duration_so_far < 120 ~ 90,
                                             ED_duration_so_far >= 120 & ED_duration_so_far < 180 ~ 120,
                                             ED_duration_so_far >= 180 & ED_duration_so_far < 240 ~ 180,
                                             ED_duration_so_far >= 240 & ED_duration_so_far < 300 ~ 240,
                                             ED_duration_so_far >= 300 & ED_duration_so_far < 360 ~ 300,
                                             ED_duration_so_far >= 360 & ED_duration_so_far < 480 ~ 360,
                                             ED_duration_so_far >= 480 & ED_duration_so_far < 720 ~ 480,
                                             ED_duration_so_far >= 720 ~ 720)
                     ]


# get time to admission after report time

in_ED_at_time_pt_all[, tta_after_ts_start := as.numeric(ED_duration - as.numeric(gsub("task","", timeslice)))]
in_ED_at_time_pt_all[, tta_hr := ceiling(tta_after_ts_start/60)]

# cap maximum hours to admission at 24 
in_ED_at_time_pt_all[, tta_hr := if_else(tta_hr > 24, 24, tta_hr)]

# or do not factor times to admissions greater than 24 hours into the calculation??

# add in other variables which may affect time to admission

# in_ED_at_time_pt_all[, weekend:= if_else(weekdays(time_pt, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0)]
in_ED_at_time_pt_all[, time_of_report:= factor(paste0(hour(time_pt), ":", substr(time_pt, 15, 16)),
                                               levels = c("6:00", "12:00", "16:00", "22:00"))]



# resummarise number in timeslice
in_ED_at_time_pt_all[, num_ts := .N, by = .(epoch, in_set, time_of_report, timeslice)]

# create data table counting how many people had which time to admission (in hours)
# this is incomplete because not all time to admission hours will be observed empirically 
# eg zero hours to admission might be quite unlikely
tta_prob_incomplete = in_ED_at_time_pt_all[, .(num_with_tta_in_hr = .N), by = .(epoch, in_set, time_of_report, timeslice, tta_hr)]

col_order = c("epoch", "in_set", "time_of_report", "timeslice", "tta_hr")
setorderv(tta_prob_incomplete, col_order)

# get all permutations of variables which may affect time to admission 
permutations = unique(in_ED_at_time_pt_all[, .N, by = .(epoch, in_set, time_of_report, timeslice, tta_hr)][, .(epoch, in_set, time_of_report, timeslice)])

# create a vector of hours until admission from 0 to 24
tta_hr_vector = seq(0,24, 1)

# create a matrix combining these two (permutations + vector of hours) 
combined = data.table(matrix(nrow = nrow(permutations), ncol = length(tta_hr_vector), data = rep(tta_hr_vector, 25), byrow = TRUE))
target_rows = bind_cols(permutations, combined) %>% pivot_longer(V1:V25, values_to = "tta_hr") %>% select(-name)

# merge to fill all the missing values of tta in the final version of tta_prob
tta_prob = data.table(target_rows %>% full_join(tta_prob_incomplete))
tta_prob[is.na(tta_prob)] <- 0 

# add number in each timeslice by merging with in_ED_at_time_pt_all
tta_prob = merge(tta_prob, unique(in_ED_at_time_pt_all[, .(epoch, in_set, time_of_report, timeslice, num_ts)]))
tta_prob[, prob := num_with_tta_in_hr/num_ts]
tta_prob[, cdf := cumsum(prob), by = .(epoch, in_set, time_of_report, timeslice)]


# check
tta_prob[epoch == "Post" & in_set == "Train"] %>% ggplot(aes(x = tta_hr, y = cdf)) + geom_point() + facet_grid(time_of_report~timeslice)

tta_prob_old = tta_prob


# Calculate for combined dataset ------------------------------------------

pre_post_train = in_ED_at_time_pt_all[first_ED_admission < '2020-12-01 00:00:00']

pre_post_train[, epoch := "Pre + Post"]
pre_post_train[, in_set := "Train"]

# resummarise number in timeslice
pre_post_train[, num_ts := .N, by = .(epoch, in_set, time_of_report, timeslice)]
pre_post_train_incomplete = pre_post_train[, .(num_with_tta_in_hr = .N), by = .(epoch, in_set, time_of_report, timeslice, tta_hr)]
setorderv(pre_post_train_incomplete, col_order)

# create a matrix combining these two (permutations + vector of hours) 
permutations = unique(pre_post_train[, .N, by = .(epoch, in_set, time_of_report, timeslice, tta_hr)][, .(epoch, in_set, time_of_report, timeslice)])

combined = data.table(matrix(nrow = nrow(permutations), ncol = length(tta_hr_vector), data = rep(tta_hr_vector, 25), byrow = TRUE))
target_rows = bind_cols(permutations, combined) %>% pivot_longer(V1:V25, values_to = "tta_hr") %>% select(-name)

# merge to fill all the missing values of tta in the final version of tta_prob
tta_prob_pre_post_train = data.table(target_rows %>% full_join(pre_post_train_incomplete))
tta_prob_pre_post_train[is.na(tta_prob_pre_post_train)] <- 0 

# add number in each timeslice by merging with in_ED_at_time_pt_all
tta_prob_pre_post_train = merge(tta_prob_pre_post_train, unique(pre_post_train[, .(epoch, in_set, time_of_report, timeslice, num_ts)]))
tta_prob_pre_post_train[, prob := num_with_tta_in_hr/num_ts]
tta_prob_pre_post_train[, cdf := cumsum(prob), by = .(epoch, in_set, time_of_report, timeslice)]


# check
tta_prob_pre_post_train[epoch == "Pre + Post" & in_set == "Train"] %>% ggplot(aes(x = tta_hr, y = cdf)) + geom_point() + facet_grid(time_of_report~timeslice)


# Save to file ------------------------------------------------------------

# validation set is same as post Covid validation set

tta_prob_pre_post_val = tta_prob[epoch == "Post" & in_set == "Val"]
tta_prob_pre_post_val[, epoch := "Pre + Post"]
tta_prob = bind_rows(tta_prob, tta_prob_pre_post_train, tta_prob_pre_post_val)

save(tta_prob, file = tta_prob_file)
