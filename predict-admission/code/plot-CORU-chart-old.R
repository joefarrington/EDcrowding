
# Evaluate a distribution using the CORU method -----------------------------------------------

# Take input of predictions of admission numbers from the indiv level ML
# and examines the distribution of probabilities

# The input data is a set of probabilities of N patients being admitted in the next 4 hours
# where N ranges from 0 to 25

# First step is to generate a random sample of timestamps

# Then, for each sample,
# - retrieve the cumulative probabilities and the actual number admitted

# Then for each sampled timestamps in turn, do the following:
# - for each of 20 points along the cdf from 0 to 1, starting at 0.05, referred to as alpha, do the following:
# - - find the number of admissions that have a cumulative probability of less than alpha (this is the model's lower number of admissions at that value of alpha)
# - - add one to this (to get the model's upper number of admissions at that value of alpha)
# - - record the model's cdf at the model's lower number of admissions 
# - - record the model's cdf at the model's higher number of admissions (this and the one above lie either side of each alpha)
# - - record whether the actual number of admissions for that timestamp was less than the model's lower at that value of alpha (True or False)
# - - record whether the actual number of admissions for that timestamp was more than the model's lower at that value of alpha (True or False)

# Then take the mean for all sample timestamps, by doing the following:
# - for each of 20 values of alpha
# - - calculate the mean model's cdf at the model's lower number of admissions below that value of alpha 
# - - calculate the mean model's cdf at the model's higher number of admissions above that value of alpha 
# - - calculate the proportion of days when the actual number of admissions for that timestamp was less than the model's lower limit at that level of alpha
# - - calculate the proportion of days when the actual number of admissions for that timestamp was less than the model's upper limit at that level of alpha


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

get_random_dttm <- function(dttm_start, dttm_end) {
  dt <- as.numeric(difftime(dttm_end, dttm_start,unit="sec"))
  increment <- runif(1, 0, dt)
  return(dttm_start + increment)
}


# Get probability distribution for number admitted at each time point of interest  

get_prob_dist = function(time_window, time_pts, summ, preds_all_ts, tta_prob) {
  
  distr_coll = data.table()
  adm_coll = data.table()
  noone_in_ED = POSIXct()
  
  for (i in (1:length(time_pts))) {
    
    in_ED = summ[first_ED_admission < time_pts[i] & left_ED > time_pts[i], 
                 .(csn, first_ED_admission, 
                   left_ED,
                   elapsed = difftime(time_pts[i], first_ED_admission, units = "mins"),
                   elapsed_time_to_adm = difftime(left_ED, time_pts[i], units = "mins"))]
    
    in_ED[, admtime_window := elapsed_time_to_adm < time_window*60]
    
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
    
    if (nrow(in_ED) != 0) {
      df = setorder(merge(in_ED, preds_all_ts[,.(csn, truth, response, prob.1, prob.0, timeslice)], 
                          by = c("csn", "timeslice")), prob.1)
      df = merge(df, tta_prob[tta_hr == time_window, .(timeslice, prob_ts_in_time_window = cdf)], 
                 by = "timeslice")
      
      
      # # should it be like this - calculating the probabilities of each within four hours: 
      # 
      # for (timeslice_ in unique(in_ED$timeslice)) {
      #   df = setorder(merge(in_ED[timeslice == timeslice_], preds_all_ts[,.(csn, truth, response, prob.1, prob.0, timeslice)], 
      #                       by = c("csn", "timeslice")), prob.1)
      #   df = merge(df, tta_prob[tta_hr == time_window, .(timeslice, prob_ts_in_time_window = cdf)], 
      #              by = "timeslice")
      #   
      #   # make an array from 0 admissions to max admissions (ie all patients admitted)
      #   num_adm = seq(0,nrow(df), 1)
      #   # the probabilities of each of these numbers being admitted
      #   pgf = poly_prod(df) 
      #   
      #   # the probabilities of each of these numbers being admitted within four hours
      #   pgftime_window = poly_prod(df[, prob.1 := prob.1*prob_ts_in_time_window]) 
      #   
      # }
      # 
      
      
      # for all patients irrespective of timeslice - a calc of likely number of patients
      
      # make an array from 0 admissions to max admissions (ie all patients admitted)
      num_adm_ = seq(0,nrow(df), 1)
      # the probabilities of each of these numbers being admitted
      pgf = poly_prod(df) 
      # the probabilities of each of these numbers being admitted within four hours
      pgftime_window = poly_prod(df[, prob.1 := prob.1*prob_ts_in_time_window]) 
      
      distr = bind_cols(sample_time = time_pts[i], num_adm_pred = num_adm_, 
                        probs = pgf, cdf = cumsum(pgf), 
                        probtime_window = pgftime_window, cdftime_window = cumsum(pgftime_window))
      distr_coll = bind_rows(distr_coll, distr)
      
      num_adm = bind_cols(sample_time = time_pts[i], num_in_ED = nrow(in_ED), 
                          num_adm = sum(df$truth == 1), 
                          num_admtime_window = sum((df$truth == 1)*df$admtime_window, na.rm = TRUE))
      adm_coll = bind_rows(adm_coll, num_adm)
    }
    else {
      noone_in_ED = c(noone_in_ED, time_pts[i])
    }
    
  }
  
  return(list(distr_coll, adm_coll, noone_in_ED))
  
}

# Analyse distributions  

get_adm_by_cutoff = function(distr_coll, adm_coll) {
  
  
  cutoff_cdf_at_mult_days = tibble()
  cutoff_cdf_at_mult_days_time_windowhrs = tibble()
  
  
  for (i in 1:nrow(adm_coll)) {
    
    distr = as_tibble(distr_coll[sample_time == adm_coll$sample_time[i]])
    
    actual_adm = adm_coll[sample_time == adm_coll$sample_time[i], num_adm]
    actual_admtime_window = adm_coll[sample_time == adm_coll$sample_time[i], num_admtime_window]
    
    alpha_increments = 0.05
    
    # all admissions
    cutoff_cdf_at = as_tibble(seq(0,1,alpha_increments)) %>% rename(cutoff = value)
    cutoff_cdf_at$date = adm_coll$sample_time[i]
    cutoff_cdf_at$actual_adm = actual_adm
    cutoff_cdf_at$model_lower_num_adm = NA
    cutoff_cdf_at$model_lower_cdf = NA
    cutoff_cdf_at$model_upper_num_adm = NA
    cutoff_cdf_at$model_upper_cdf = NA
    
    # admissions in time_window hours
    cutoff_cdf_at_time_windowhrs = as_tibble(seq(0,1,alpha_increments)) %>% rename(cutoff = value)
    cutoff_cdf_at_time_windowhrs$date = adm_coll$sample_time[i]
    cutoff_cdf_at_time_windowhrs$actual_adm = actual_admtime_window
    cutoff_cdf_at_time_windowhrs$model_lower_num_adm = NA
    cutoff_cdf_at_time_windowhrs$model_lower_cdf = NA
    cutoff_cdf_at_time_windowhrs$model_upper_num_adm = NA
    cutoff_cdf_at_time_windowhrs$model_upper_cdf = NA
    
    
    for (j in 1:nrow(cutoff_cdf_at)) {
      
      cutoff_cdf_at$model_lower_num_adm[j] = 
        nrow(distr %>% filter(cdf < cutoff_cdf_at$cutoff[j]))
      
      if (j != nrow(cutoff_cdf_at)) {
        
        cutoff_cdf_at$model_upper_num_adm[j] = cutoff_cdf_at$model_lower_num_adm[j] + 1 # This is correct
        
      }      else {
        cutoff_cdf_at$model_upper_num_adm[j] = 
          nrow(distr %>% filter(cdf < cutoff_cdf_at$cutoff[j]))
      }
      
      # Enoch also looks up the cdf at the model threshold 
      # (although note that Enoch adds 1 to i to get the upper threshold, returning the next row in the cdf
      # rather than setting the higher band at the next alpha threshhold for the cdf
      
      if (cutoff_cdf_at$model_lower_num_adm[j] != 0) {
        cutoff_cdf_at$model_lower_cdf[j] = distr$cdf[cutoff_cdf_at$model_lower_num_adm[j]] 
      } else {
        cutoff_cdf_at$model_lower_cdf[j] = 0
      }
      
      cutoff_cdf_at$model_upper_cdf[j] = distr$cdf[cutoff_cdf_at$model_upper_num_adm[j]]
      
    }
    
    for (j in 1:nrow(cutoff_cdf_at_time_windowhrs)) {
      
      cutoff_cdf_at_time_windowhrs$model_lower_num_adm[j] = 
        nrow(distr %>% filter(cdf < cutoff_cdf_at_time_windowhrs$cutoff[j]))
      
      if (j != nrow(cutoff_cdf_at_time_windowhrs)) {
        
        cutoff_cdf_at_time_windowhrs$model_upper_num_adm[j] = cutoff_cdf_at_time_windowhrs$model_lower_num_adm[j] + 1 # This is correct
        
      }      else {
        cutoff_cdf_at_time_windowhrs$model_upper_num_adm[j] = 
          nrow(distr %>% filter(cdf < cutoff_cdf_at_time_windowhrs$cutoff[j]))
      }
      
      # Enoch also looks up the cdf at the model threshold 
      # (although note that Enoch adds 1 to i to get the upper threshold, returning the next row in the cdf
      # rather than setting the higher band at the next alpha threshhold for the cdf
      
      if (cutoff_cdf_at_time_windowhrs$model_lower_num_adm[j] != 0) {
        cutoff_cdf_at_time_windowhrs$model_lower_cdf[j] = distr$cdf[cutoff_cdf_at_time_windowhrs$model_lower_num_adm[j]] 
      }
      cutoff_cdf_at_time_windowhrs$model_upper_cdf[j] = distr$cdf[cutoff_cdf_at_time_windowhrs$model_upper_num_adm[j]]
      
    }
    
    cutoff_cdf_at_mult_days <- cutoff_cdf_at_mult_days %>% bind_rows(cutoff_cdf_at)
    cutoff_cdf_at_mult_days_time_windowhrs <- cutoff_cdf_at_mult_days_time_windowhrs %>% bind_rows(cutoff_cdf_at_time_windowhrs)
  }
  
  # Get number of days where the actual admission is less than the model threshold limits
  cutoff_cdf_at_mult_days <- cutoff_cdf_at_mult_days %>% 
    mutate(actual_less_than_lower = ifelse(actual_adm < model_lower_num_adm, TRUE, FALSE),
           actual_less_than_upper = ifelse(actual_adm < model_upper_num_adm, TRUE, FALSE))
  
  cutoff_cdf_at_mult_days_time_windowhrs <- cutoff_cdf_at_mult_days_time_windowhrs %>% 
    mutate(actual_less_than_lower = ifelse(actual_adm < model_lower_num_adm, TRUE, FALSE),
           actual_less_than_upper = ifelse(actual_adm < model_upper_num_adm, TRUE, FALSE))
  
  return(list(cutoff_cdf_at_mult_days, cutoff_cdf_at_mult_days_time_windowhrs))
}



# Load data ---------------------------------------------------------------

# summary  to get minimum and maxium


load("~/EDcrowding/flow-mapping/data-raw/summ_2021-03-16.rda")
summ = summ[!is.na(discharge_time)]

# added this while using only part of the dataset - need to change it later
summ = summ[date(presentation_time) >= '2020-03-19']

summ[, left_ED := coalesce(first_outside_proper_admission, last_inside_discharge)]


# load predictions (output from ML)
preds_file <- paste0("~/EDcrowding/predict-admission/data-output/xgb_preds_",today(),".rda")
load(preds_file)

# load("~/EDcrowding/predict-admission/data-output/xgb_preds_2021-03-23.rda")


timeslices <- c("000", "015", "030", "060", "090", "120", "180", "240", "300", "360", "480")

file_date = '2021-03-16'

preds_all_ts <- data.table()

for (ts_ in timeslices) {
  
  # load timeslice 
  inFile = paste0("~/EDcrowding/predict-admission/data-raw/dm", ts_, "_", file_date, ".rda")
  load(inFile)
  
  name_ts <- paste0("dm", ts_)
  dt = get(name_ts)
  
  name_tsk <- paste0("task", ts_)
  
  dt[, row_id := seq_len(nrow(dt))]
  
  dt_ = bind_cols(dt[, list(csn, adm, row_id)],preds[timeslice == name_tsk & tsk_ids == "all"])
  # check that row ids match
  if (nrow(dt_[row_id...3 != row_id...4]) > 0) {
    print("Row match error")
  }
  
  preds_all_ts = bind_rows(preds_all_ts, dt_)
  
}



# Get a sample of time points of interest ---------------------------------



set.seed(17L)
time_pts <- get_random_dttm(min(summ$presentation_time, na.rm = TRUE), min(summ$presentation_time, na.rm = TRUE) + hours(12))
last_pt <- time_pts

while (last_pt + hours(12) < max(summ$presentation_time, na.rm = TRUE)) {
  next_pt <- get_random_dttm(last_pt + hours(12), last_pt + hours(24))
  time_pts <- c(time_pts, next_pt)
  last_pt <- next_pt
  
}

# To get probability distribution for time to admission for each timeslice ----------------------------

# now using left_ED and first_ED_admission to tighten the distribution
# summ[adm %in% c("direct_adm", "indirect_adm"),ED_duration := difftime(last_ED_discharge, presentation_time, units = "mins")]
summ[adm %in% c("direct_adm", "indirect_adm"),ED_duration := difftime(left_ED, first_ED_admission, units = "mins")]

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
tta = data.table(summ %>% filter(!is.na(ED_duration), adm %in% c("direct_adm", "indirect_adm")) %>% 
                   select(csn, task000:task480, ED_duration) %>% 
                   pivot_longer(task000:task480, names_to = "timeslice"))
tta = tta[value ==1]
tta[, tta := case_when(value ==1 ~ as.numeric(ED_duration - as.numeric(gsub("task","", timeslice))),
                       TRUE ~ NA_real_)]

# cut this to get whole number of hours until admission (was cutting at floor, now cutting at ceiling)
tta[, tta_hr := ceiling(tta/60)]

# cut the distribution at 24 hours _ NB not sure this is right but this will increase the probability at earlier points, only very marginal
tta = tta[tta_hr <= 24]

# generate number of visits in timslice in total
tta[, num_ts := sum(value), by = timeslice]

# generate cumulative probability of being admitted within a number of hours after timeslice
tta_prob = data.table(tta %>% filter(tta_hr >= 0) %>% 
                        group_by(timeslice, num_ts, tta_hr) %>% 
                        summarise(num_with_tta_in_hr = n()))
tta_prob[, prob := num_with_tta_in_hr/num_ts]
tta_prob[, cdf := cumsum(prob), by = timeslice]

# # # plot tta after timeslice
# tta_prob[tta_hr < 24] %>%
#   mutate(timeslice = as.numeric(gsub("task", "", timeslice))) %>%
#   ggplot(aes(x = tta_hr, y = prob)) + geom_line() + facet_grid(.~timeslice) +
#   labs(title = "Probability distribution for time to admission after beginning of timeslice (up to 24 hours)",
#        x = "Time to admission (hrs)",
#        y = "Probability")
# # 
# 
# # plot cdf by timeslice
# tta_prob[tta_hr < 48] %>% 
#   mutate(timeslice = as.numeric(gsub("timeslice", "", timeslice))) %>% 
#   ggplot(aes(x = tta_hr, y = cdf)) + geom_line() + facet_grid(timeslice~.) +
#   geom_vline(xintercept = 4, colour = "red") +
#   labs(title = "Cumulative probability distribution for time to admission after beginning of timeslice (up to 48 hours)",
#        x = "Time to admission (hrs)",
#        y = "Probability")
# 
# 
# # to see probs as a wider array
# 
# tta_prob %>% 
#   select(timeslice, tta_hr, prob) %>% 
#   mutate(timeslice = as.numeric(gsub("timeslice", "", timeslice))) %>% 
#   pivot_wider(names_from = timeslice, values_from = prob)
# 
# tta_prob[, timeslice := as.numeric(gsub("timeslice", "", timeslice))]
# setorder(tta_prob, timeslice)
# 
# tta_prob[tta_hr > 24, sum(prob), by = timeslice]
# tta_prob[tta_hr > 48, sum(prob), by = timeslice]



# Processing --------------------------------------------------------------

prob_dist2 = get_prob_dist(2, time_pts, summ, preds_all_ts, tta_prob)
prob_dist3 = get_prob_dist(3, time_pts, summ, preds_all_ts, tta_prob)
prob_dist4 = get_prob_dist(4, time_pts, summ, preds_all_ts, tta_prob)
prob_dist6 = get_prob_dist(6, time_pts, summ, preds_all_ts, tta_prob)
prob_dist9 = get_prob_dist(9, time_pts, summ, preds_all_ts, tta_prob)
prob_dist12 = get_prob_dist(12, time_pts, summ, preds_all_ts, tta_prob)


# plot of sampled time points
prob_dist4[[2]] %>% pivot_longer(num_in_ED:num_adm) %>%  
  ggplot(aes(x = sample_time, y = value, col = name, group = sample_time)) + geom_line() + geom_point() +
  theme(legend.position = "bottom") +
  labs(title = "Showing range of sample points over time with number in ED and number admitted",
       y = "Number of patients",
       x = "sampled time")

cutoff_cdf2 = get_adm_by_cutoff(prob_dist2[[1]], prob_dist2[[2]])
cutoff_cdf3 = get_adm_by_cutoff(prob_dist3[[1]], prob_dist3[[2]])
cutoff_cdf4 = get_adm_by_cutoff(prob_dist4[[1]], prob_dist4[[2]])
cutoff_cdf6 = get_adm_by_cutoff(prob_dist6[[1]], prob_dist6[[2]])
cutoff_cdf9 = get_adm_by_cutoff(prob_dist9[[1]], prob_dist9[[2]])
cutoff_cdf12 = get_adm_by_cutoff(prob_dist12[[1]], prob_dist12[[2]])


# normalise across all days
cutoff_cdf_normalised <- cutoff_cdf3[[1]] %>% 
  group_by(cutoff) %>% summarise(model_lower_limits = mean(model_lower_cdf, na.rm = TRUE),
                                 actual_less_than_lower_limit = mean(actual_less_than_lower, na.rm = TRUE),
                                 model_upper_limits = mean(model_upper_cdf, na.rm = TRUE),
                                 actual_less_than_upper_limit = mean(actual_less_than_upper, na.rm = TRUE),
                                 
                                 )

cutoff_cdf_normalised_3hrs <- cutoff_cdf3[[2]] %>% 
  group_by(cutoff) %>% summarise(model_lower_limits = mean(model_lower_cdf, na.rm = TRUE),
                                 actual_less_than_lower_limit = mean(actual_less_than_lower, na.rm = TRUE),
                                 model_upper_limits = mean(model_upper_cdf, na.rm = TRUE),
                                 actual_less_than_upper_limit = mean(actual_less_than_upper, na.rm = TRUE),
  )                        

cutoff_cdf_normalised_4hrs <- cutoff_cdf4[[2]] %>% 
  group_by(cutoff) %>% summarise(model_lower_limits = mean(model_lower_cdf, na.rm = TRUE),
                                 actual_less_than_lower_limit = mean(actual_less_than_lower, na.rm = TRUE),
                                 model_upper_limits = mean(model_upper_cdf, na.rm = TRUE),
                                 actual_less_than_upper_limit = mean(actual_less_than_upper, na.rm = TRUE),
                                 
  )


cutoff_cdf_normalised_6hrs <- cutoff_cdf6[[2]] %>% 
  group_by(cutoff) %>% summarise(model_lower_limits = mean(model_lower_cdf, na.rm = TRUE),
                                 actual_less_than_lower_limit = mean(actual_less_than_lower, na.rm = TRUE),
                                 model_upper_limits = mean(model_upper_cdf, na.rm = TRUE),
                                 actual_less_than_upper_limit = mean(actual_less_than_upper, na.rm = TRUE),
                                 
  )

cutoff_cdf_normalised_9hrs <- cutoff_cdf9[[2]] %>% 
  group_by(cutoff) %>% summarise(model_lower_limits = mean(model_lower_cdf, na.rm = TRUE),
                                 actual_less_than_lower_limit = mean(actual_less_than_lower, na.rm = TRUE),
                                 model_upper_limits = mean(model_upper_cdf, na.rm = TRUE),
                                 actual_less_than_upper_limit = mean(actual_less_than_upper, na.rm = TRUE),
                                 
  )


cutoff_cdf_normalised_12hrs <- cutoff_cdf12[[2]] %>% 
  group_by(cutoff) %>% summarise(model_lower_limits = mean(model_lower_cdf, na.rm = TRUE),
                                 actual_less_than_lower_limit = mean(actual_less_than_lower, na.rm = TRUE),
                                 model_upper_limits = mean(model_upper_cdf, na.rm = TRUE),
                                 actual_less_than_upper_limit = mean(actual_less_than_upper, na.rm = TRUE),
                                 
  )


# Create charts -----------------------------------------------------------

plot_cdf = function(cutoff_cdf_normalised, time_window) {
  cutoff_cdf_normalised %>% 
    pivot_longer(model_lower_limits:actual_less_than_upper_limit, 
                 names_to = "model", 
                 values_to = "proportion") %>% 
    mutate(model = factor(model, levels = c("model_lower_limits", "actual_less_than_lower_limit", "model_upper_limits", "actual_less_than_upper_limit"))) %>% 
    ggplot(aes(x = cutoff, y = proportion, col = model)) + geom_point() + geom_line(size = 1)  +
    theme_classic(base_size = 18) +
    scale_x_continuous(breaks = seq(0, 1, .1)) +
    labs(title = paste0("Admission in next ",time_window, " hours"), 
         y = "Proportion of x <= X on cdf",
         # subtitle = "At each time point of interest, probability of admission is calculated for each patient in ED. These probabilities are converted into a cumulative probability distribution.\n
         # 20 equally spaced points on the cdf are chosen (shown on the X axis); these are the probability that the number of admissions is less than or equal to a number x \n
         # The distribution generated by the model is used to retrieve what x (the number of admissions) would be at each of these 20 points on the cdf",
         x = "X",
         col = "model/actual")  +
    theme(legend.position = "none") +
    # scale_color_manual(values = c("#F8766D" , "#FFB2B2","#00BFC4","#99E4E7", guide = NULL, name = NULL)) 
    scale_color_manual(values = c("darkblue" , "chartreuse4","cadetblue4","deeppink", guide = NULL, name = NULL)) 
  
  
}

# main plot
cutoff_cdf_normalised %>% pivot_longer(model_lower_limits:actual_less_than_upper_limit, 
                                       names_to = "model", 
                                       values_to = "proportion") %>% 
  mutate(model = factor(model, levels = c("model_lower_limits", "actual_less_than_lower_limit", "model_upper_limits", "actual_less_than_upper_limit"))) %>% 
  ggplot(aes(x = cutoff, y = proportion, col = model)) + geom_point() + geom_line(size = 1)  +
  theme_classic(base_size = 18) +
  scale_x_continuous(breaks = seq(0, 1, .05)) +
  labs(title = paste0("Admission overall: evaluation - based on ", length(time_pts), " randomly sampled time points of interest"), 
       y = "Proportion of instances <= X on cdf",
       # subtitle = "At each time point of interest, probability of admission is calculated for each patient in ED. These probabilities are converted into a cumulative probability distribution.\n
       # 20 equally spaced points on the cdf are chosen (shown on the X axis); these are the probability that the number of admissions is less than or equal to a number x \n
       # The distribution generated by the model is used to retrieve what x (the number of admissions) would be at each of these 20 points on the cdf",
       x = "X",
       col = "model/actual")  +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("darkblue" , "chartreuse4","cadetblue4","deeppink", guide = NULL, name = NULL)) 



# other plots
p3 = plot_cdf(cutoff_cdf_normalised_3hrs, 3)

p6 = plot_cdf(cutoff_cdf_normalised_6hrs, 6)
p9 = plot_cdf(cutoff_cdf_normalised_9hrs, 9)
p12 = plot_cdf(cutoff_cdf_normalised_12hrs, 12)

library("gridExtra")
grid.arrange(p3, p6, p9, p12,
             ncol = 2, nrow = 2)



outFile = paste0("EDcrowding/predict-admission/data-output/predicted_distribution_",today(),".csv")
write.csv(prob_dist3[[1]], file = outFile, row.names = FALSE)


outFile = paste0("EDcrowding/predict-admission/data-output/actual_admissions_",today(),".csv")
write.csv(prob_dist3[[2]], file = outFile, row.names = FALSE)

# Exploring poor predictions ----------------------------------------------

# using three hour distribution

distr_coll = prob_dist3[[1]]
adm_coll = prob_dist3[[2]]
cutoff_cdf_at_mult_days = data.table(cutoff_cdf3[[1]])

c = data.table(cutoff_cdf_at_mult_days %>% group_by(date) %>% summarise(num_rows_less_than_lower = sum(actual_less_than_lower)))
c[, prop_less_than_lower := num_rows_less_than_lower/20]
setnames(c, "date", "sample_time")

c = merge(c, adm_coll, by = "sample_time", all.x = TRUE)

c %>% ggplot(aes(x = num_adm, y = prop_less_than_lower)) + geom_point()

# look at individual time points
cutoff_cdf_at_mult_days[sample_time == c$sample_time[333]] 

cutoff_cdf_at_mult_days_3hrs = data.table(cutoff_cdf3[[2]])
c3 = data.table(cutoff_cdf_at_mult_days_3hrs %>% group_by(date) %>% summarise(num_rows_less_than_lower = sum(actual_less_than_lower)))
c3[, prop_less_than_lower := num_rows_less_than_lower/20]
setnames(c3, "date", "sample_time")
c3 = merge(c3, adm_coll, by = "sample_time", all.x = TRUE)
c3 %>% ggplot(aes(x = num_adm, y = prop_less_than_lower)) + geom_point() +
  labs(title = "Proportion of alpha values where actual admissions in three hours is less than model lower bound",
       x = "Number of people in ED at time point of interest")
