# work out distribution of not yet arrived

# summary of csns to get minimum and maxium
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-04-13.rda")
summ = summ[!is.na(discharge_time)]
summ[, left_ED := coalesce(first_outside_proper_admission, last_inside_discharge)]

summ[, in_set := case_when(presentation_time < '2019-11-19 00:00:00' ~ "Train",
                         presentation_time < '2019-12-13 00:00:00' ~ "Val",
                         presentation_time < '2020-03-19 00:00:00' ~ "Test",
                         presentation_time < '2020-12-01 00:00:00' ~ "Train",
                         presentation_time < '2020-12-29 00:00:00' ~ "Val",
                         TRUE ~ "Test",)]

# Generate time points  ------------------------------

start_of_set = as.POSIXct('2019-05-01', tz = "UTC")
end_of_set = as.POSIXct('2021-04-13', tz = "UTC")
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
                          .(csn, in_set, first_ED_admission = with_tz(first_ED_admission, tzone = "UTC"), 
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
                                             ED_duration_so_far >= 480 ~ 480)
                     ]


# get time to admission after report time

in_ED_at_time_pt_all[, tta_after_ts_start := as.numeric(ED_duration - as.numeric(gsub("task","", timeslice)))]
in_ED_at_time_pt_all[, tta_hr := ceiling(tta_after_ts_start/60)]

# commenting out for now 
# in_ED_at_time_pt_all = in_ED_at_time_pt_all[tta_hr <= 24]

# add in other variables which may affect time to admission
in_ED_at_time_pt_all[, epoch := case_when(date(time_pt) >= '2020-03-19' ~ "Post",
                                          TRUE ~ "Pre")]
in_ED_at_time_pt_all[, weekend:= if_else(weekdays(time_pt, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0)]
in_ED_at_time_pt_all[, time_of_report:= factor(paste0(hour(time_pt), ":", substr(time_pt, 15, 16)),
                                               levels = c("6:00", "12:00", "16:00", "22:00"))]

# # generate number of visits in timeslice in total, also taking into account epoch, time of report, weekend etc. 
# in_ED_at_time_pt_all[, num_ts := .N, by = .(epoch, in_set, weekend, time_of_report, timeslice)]
# # generate cumulative probability of being admitted within a number of hours after timeslice
# tta_prob = in_ED_at_time_pt_all[, .(num_with_tta_in_hr = .N), by = .(epoch, in_set, weekend, time_of_report, timeslice, num_ts, tta_hr)]
# 
# tta_prob[, prob := num_with_tta_in_hr/num_ts]
# tta_prob[, cdf := cumsum(prob), by = .(epoch, in_set, weekend, time_of_report, timeslice)]
# 
# tta_prob[in_set == "Train" & epoch == "Pre" ] %>% 
#   mutate(weekend = factor(weekend, levels = c(0,1), labels = c("Weekday", "Weekend")), 
#          in_set = factor(in_set, levels = c("Train", "Val", "Test"))) %>% 
#   ggplot(aes(x = tta_hr, y = prob, col = weekend)) + geom_line() + 
#   facet_grid(time_of_report  ~ timeslice) +
#   labs(title = "Probability distribution for time to admission after beginning of timeslice (up to 24 hours)",
#        subtitle = "Pre COVID data", 
#        x = "Time to admission (hrs)",
#        y = "Probability")

## looks like not much variation by weekend; trying without
# resummarise number in timeslice
in_ED_at_time_pt_all[, num_ts := .N, by = .(epoch, in_set, time_of_report, timeslice)]




# Save to file ------------------------------------------------------------



tta_prob_file = "~/EDcrowding/real-time/data-raw/tta_prob.rda"

if (file.exists(tta_prob_file)) {
  load(tta_prob_file)
  save(tta_prob, file = paste0("~/EDcrowding/real-time/data-raw/poisson_means_archived_on_",Sys.Date(),".rda"))
}

tta_prob = in_ED_at_time_pt_all[, .(num_with_tta_in_hr = .N), by = .(epoch, in_set, time_of_report, timeslice, num_ts, tta_hr)]

col_order = c("epoch", "in_set", "time_of_report", "timeslice", "tta_hr")
setorderv(tta_prob, col_order)
tta_prob[, prob := num_with_tta_in_hr/num_ts]
tta_prob[, cdf := cumsum(prob), by = .(epoch, in_set, time_of_report, timeslice)]

save(tta_prob, file = tta_prob_file)




