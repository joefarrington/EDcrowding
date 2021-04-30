# Load libraries ----------------------------------------------------------


library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)
library(polynom)

# Create functions --------------------------------------------------------


poly_prod = function(df){
  
  # polynomial(vector) is a polynomial t
  y = polynomial(c(1,0))# 
  for (n in 1:nrow(df)){
    y = y*polynomial(c(1-df$prob.1[n],df$prob.1[n]))
  }
  return(coef(y))
}


# Get probability distribution for number admitted at each time point of interest  

get_prob_dist = function(time_window, time_pts, summ, preds_all_ts_, tta_prob) {
  
  distr_coll = data.table()
  adm_coll = data.table()
  noone_in_ED = POSIXct()
  
  for (i in (1:length(time_pts))) {
    
    in_ED = summ[first_ED_admission < time_pts[i] & left_ED > time_pts[i], 
                 .(csn, first_ED_admission, adm, 
                   left_ED,
                   elapsed = difftime(time_pts[i], first_ED_admission, units = "mins"),
                   remaining = difftime(left_ED, time_pts[i], units = "mins"))]
    
    in_ED[, adm_in_time_window := case_when(remaining < time_window*60 & adm %in% c("direct_adm", "indirect_adm") ~ TRUE,
                                            adm %in% c("direct_adm", "indirect_adm") ~ FALSE,
                                            TRUE ~ NA)]
    
    in_ED[, timeslice := case_when(elapsed < 15 ~ "task000",
                                   elapsed < 30 ~ "task015",
                                   elapsed < 60 ~ "task030",
                                   elapsed < 90 ~ "task060",
                                   elapsed < 120 ~ "task090",
                                   elapsed < 180 ~ "task120",
                                   elapsed < 240 ~ "task180",
                                   elapsed < 300 ~ "task240",
                                   elapsed < 360 ~ "task300",
                                   elapsed < 480 ~ "task360",
                                   TRUE ~ "task480")]
    
    if (nrow(in_ED) == 0) {
      
      noone_in_ED = c(noone_in_ED, time_pts[i])
      
    } else {
      
      if(is.na(time_window)) {
        
        df = merge(in_ED, preds_all_ts_[,.(csn, truth, prob.1, timeslice)], 
                            by = c("csn", "timeslice"), all.x = TRUE)
      } else {
        
        df = merge(in_ED, preds_all_ts_[,.(csn, truth, prob.1, timeslice)], 
                            by = c("csn", "timeslice"), all.x = TRUE)
        df = merge(df, tta_prob[tta_hr == time_window, .(timeslice, prob_adm_in_time_window = cdf)], 
                   by = "timeslice")
        df[, truth := adm_in_time_window]
        df[, prob.1 := prob.1 * prob_adm_in_time_window]
      }
    }

    # make an array from 0 admissions to max admissions (ie all patients admitted)
    num_adm_ = seq(0,nrow(df), 1)
    # the probabilities of each of these numbers being admitted
    probs = poly_prod(df) 

    distr = bind_cols(sample_time = time_pts[i], num_adm_pred = num_adm_, 
                      probs = probs, cdf = cumsum(probs))
    distr_coll = bind_rows(distr_coll, distr)
    
    num_adm = bind_cols(sample_time = time_pts[i], num_in_ED = nrow(in_ED), 
                        num_adm = sum(df$truth == 1, na.rm = TRUE))
    adm_coll = bind_rows(adm_coll, num_adm)
  }
  
  return(list(distr_coll, adm_coll, noone_in_ED))
  
}


# Load data ---------------------------------------------------------------

# summary of csns to get minimum and maxium
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-04-13.rda")
# load("~/summ_2021-04-13.rda")
summ = summ[!is.na(discharge_time)]
summ[, left_ED := coalesce(first_outside_proper_admission, last_inside_discharge)]


summ[, in_set := case_when(presentation_time < '2019-11-19 00:00:00' ~ "Train",
                           presentation_time < '2019-12-13 00:00:00' ~ "Val",
                           presentation_time < '2020-03-19 00:00:00' ~ "Test",
                           presentation_time < '2020-12-01 00:00:00' ~ "Train",
                           presentation_time < '2020-12-29 00:00:00' ~ "Val",
                           TRUE ~ "Test",)]




# load predictions (output from ML) - this loads predictions identified by row_id
preds_file <- paste0("~/EDcrowding/predict-admission/data-output/xgb_preds_",today(),".rda")
load(preds_file)

# preds_file <- paste0("~/EDcrowding/predict-admission/data-output/xgb_preds_2021-04-21.rda")


# load timeslice data in order to match row_ids to csns

timeslices <- c("000", "015", "030", "060", "090", "120", "180", "240", "300", "360", "480")
file_date = '2021-04-19'

preds_all_ts_ <- data.table()

for (ts_ in timeslices) {
  
  # load timeslice 
  inFile = paste0("~/EDcrowding/predict-admission/data-raw/dm", ts_, "_", file_date, ".rda")
  load(inFile)
  
  # assign name dt to loaded file temporarily
  name_ts <- paste0("dm", ts_)
  dt = get(name_ts)
  name_tsk <- paste0("task", ts_)
  dt = dt[a_epoch == "Post"]
  dt[, row_id := seq_len(nrow(dt))]
  
  
  # bind columns to add csns to predictions
  dt_ = bind_cols(dt[, list(csn, adm, row_id)],preds[timeslice == name_tsk & tsk_ids == "all"])
  # check that row ids match
  if (nrow(dt_[row_id...3 != row_id...4]) > 0) {
    print("Row match error")
  }
  
  # bind all preds into one file
  preds_all_ts_ = bind_rows(preds_all_ts_, dt_)
  
}


# Generate time points ------------------------------

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



# Get probability distribution for time to admission for each timeslice ----------------------------

summ[adm %in% c("direct_adm", "indirect_adm"), ED_duration := difftime(left_ED, first_ED_admission, units = "mins")]

summ[,task000 := 1]
summ[,task015 := if_else(ED_duration > 15, 1, 0)]
summ[,task030 := if_else(ED_duration > 30, 1, 0)]
summ[,task060 := if_else(ED_duration > 60, 1, 0)]
summ[,task090 := if_else(ED_duration > 90, 1, 0)]
summ[,task120 := if_else(ED_duration > 120, 1, 0)]
summ[,task180 := if_else(ED_duration > 180, 1, 0)]
summ[,task240 := if_else(ED_duration > 240, 1, 0)]
summ[,task300 := if_else(ED_duration > 300, 1, 0)]
summ[,task360 := if_else(ED_duration > 360, 1, 0)]
summ[,task480 := if_else(ED_duration > 480, 1, 0)]





# get time to admission after beginning of each timeslice
tta = data.table(summ %>% filter(!is.na(ED_duration)) %>% 
                   select(csn, task000:task480, ED_duration) %>% 
                   pivot_longer(task000:task480, names_to = "timeslice", values_to = "in_timeslice"))
tta = tta[in_timeslice ==1]
tta[, tta_after_ts_start := as.numeric(ED_duration - as.numeric(gsub("task","", timeslice)))]

# cut this to get whole number of hours until admission (was cutting at floor, now cutting at ceiling)
tta[, tta_hr := ceiling(tta_after_ts_start/60)]

# cut the distribution at 24 hours _ NB not sure this is right but this will increase the probability at earlier points, only very marginal
tta = tta[tta_hr <= 24]

# generate number of visits in timeslice in total
tta[, num_ts := sum(in_timeslice), by = timeslice]

# generate cumulative probability of being admitted within a number of hours after timeslice
tta_prob_old_way = data.table(tta %>% filter(tta_hr >= 0) %>% 
                        group_by(timeslice, num_ts, tta_hr) %>% 
                        summarise(num_with_tta_in_hr = n()))
tta_prob_old_way[, prob := num_with_tta_in_hr/num_ts]
tta_prob_old_way[, cdf := cumsum(prob), by = timeslice]

# plot tta after timeslice
tta_prob_old_way[tta_hr < 24] %>% 
  mutate(in_timeslice = as.numeric(gsub("timeslice", "", timeslice))) %>% 
  ggplot(aes(x = tta_hr, y = prob)) + geom_line() + facet_grid(.~timeslice) +
  labs(title = "Probability distribution for time to admission after beginning of timeslice (up to 24 hours)",
       x = "Time to admission (hrs)",
       y = "Probability")


# Different approach to generate tta - separately by time of day and whether weekday/weekend  -----------------------------------------

# get all later-admitted patients in ED at that point in time
in_ED_at_time_pt_all = data.table()

for (i in 1:length(time_pts)) {
  
  if (i %% 100 == 0) {
    print(paste("Processed ", i, " dates"))
  }
  
  in_ED_at_time_pt = summ[first_ED_admission < time_pts[i] &  first_outside_proper_admission > time_pts[i] 
                          & adm %in% c("indirect_adm", "direct_adm"), 
                          .(csn, in_set, first_ED_admission = with_tz(first_ED_admission, tzone = "UTC"), 
                            first_outside_proper_admission = with_tz(first_outside_proper_admission, tzone = "UTC"), 
                            ED_duration)]
  
  in_ED_at_time_pt[, time_pt := with_tz(time_pts[i], tzone = "UTC")]
  
  in_ED_at_time_pt_all = bind_rows(in_ED_at_time_pt_all, in_ED_at_time_pt)
}

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
in_ED_at_time_pt_all = in_ED_at_time_pt_all[tta_hr <= 24]

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
tta_prob_2 = in_ED_at_time_pt_all[, .(num_with_tta_in_hr = .N), by = .(epoch, in_set, time_of_report, timeslice, num_ts, tta_hr)]
tta_prob_2[, prob := num_with_tta_in_hr/num_ts]
tta_prob_2[, cdf := cumsum(prob), by = .(epoch, in_set, time_of_report, timeslice)]

tta_prob_file = paste0("EDcrowding/real-time/data-raw/tta_prob.rda")
save(tta_prob_2, file = tta_prob_file)

# epoch_ = "Post"
# tta_prob_2[in_set == "Train" & epoch == epoch_ ] %>% 
#   ggplot(aes(x = tta_hr, y = prob)) + geom_line() + 
#   facet_grid(time_of_report  ~ timeslice) +
#   labs(title = "Probability distribution for time to admission after beginning of timeslice (up to 24 hours)",
#        subtitle = paste(epoch_, "COVID data"), 
#        x = "Time to admission (hrs)",
#        y = "Probability")
# 
# tta_prob_2[timeslice %in% c(300, 360, 480) & in_set == "Train"] %>% 
#   mutate( timeslice = case_when(nchar(as.character(timeslice)) == 1 ~ paste0("00", timeslice),
#                                      nchar(as.character(timeslice)) == 2 ~ paste0("0", timeslice),
#                                      TRUE ~ paste0("", timeslice)),
#           epoch = factor(epoch, levels = c("Pre", "Post"))) %>% 
#             ggplot(aes(x = tta_hr, y = prob, col = time_of_report)) + geom_line() +
#   facet_grid(epoch ~ timeslice ) +
#   labs(title = "Probability of number of hours to admission for last three timeslices at different times of day",
#        subtitle = "Pre and post Covid",
#        x = "Hours to admission", 
#        y = "Probability")
#   
# 
# # get expected values
# expected_tta = tta_prob_2[in_set == "Train", .SD[which.max(prob)], by = list(epoch, time_of_report, timeslice)]
# expected_tta %>%  mutate( timeslice = case_when(nchar(as.character(timeslice)) == 1 ~ paste0("00", timeslice),
#                                         nchar(as.character(timeslice)) == 2 ~ paste0("0", timeslice),
#                                         TRUE ~ paste0("", timeslice)),
#                           epoch = factor(epoch, levels = c("Pre", "Post"))) %>% 
#   ggplot(aes(x = timeslice, y = tta_hr, col = time_of_report, group = time_of_report)) + geom_line() +
#   scale_y_continuous(breaks = seq(0, 10, 1)) +
#   facet_grid(epoch  ~ time_of_report) +
#   labs(title = "Expected time to admission from beginning of timeslice",
#        subtitle = "Training set only", 
#        y = "Expected number of hours to admission",
#        x = "Timeslice") +
#   theme(legend.position = "bottom")
