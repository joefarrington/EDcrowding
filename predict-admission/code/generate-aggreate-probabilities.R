
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




# Load data ---------------------------------------------------------------

# summary  to get minimum and maxium


load("~/EDcrowding/flow-mapping/data-raw/summ_2021-01-27.rda")
summ = summ[!is.na(discharge_time)]

# added this while using only part of the dataset - need to change it later
summ = summ[date(presentation_time) > '2020-04-01']


# load predictions (output from ML)
preds_file <- paste0("~/EDcrowding/predict-admission/data-output/preds_",today(),".rda")
load(preds_file)

timeslices <- c("000", "015", "030", "060", "120", "180", "240", "300", "360")

file_date <- "2021-02-08"

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

preds_all_ts[, timeslice := gsub("task", "timeslice", timeslice)]


# Get a sample of time points of interest ---------------------------------



set.seed(17L)
time_pts <- get_random_dttm(min(summ$presentation_time, na.rm = TRUE), min(summ$presentation_time, na.rm = TRUE) + hours(12))
last_pt <- time_pts

while (last_pt + hours(12) < max(summ$presentation_time, na.rm = TRUE)) {
  next_pt <- get_random_dttm(last_pt, last_pt + hours(12))
  time_pts <- c(time_pts, next_pt)
  last_pt <- next_pt
  
}


# Get probability distribution for all patients together -------------------------------------

distr_coll = data.table()
adm_coll = data.table()

for (i in (1:length(time_pts))) {
  
  in_ED = summ[presentation_time < time_pts[i] &
          last_ED_discharge > time_pts[i], .(csn, presentation_time, elapsed = difftime(time_pts[i], presentation_time, units = "mins"))]
  
  in_ED[, timeslice := case_when(elapsed < 15 ~ "timeslice000",
                                 elapsed < 30 ~ "timeslice015",
                                 elapsed < 60 ~ "timeslice030",
                                 elapsed < 120 ~ "timeslice060",
                                 elapsed < 180 ~ "timeslice120",
                                 elapsed < 240 ~ "timeslice180",
                                 elapsed < 300 ~ "timeslice240",
                                 elapsed < 360 ~ "timeslice300",
                                 TRUE ~ "timeslice360")]
  
  df = setorder(merge(in_ED, preds_all_ts[,.(csn, truth, response, prob.1, prob.0, timeslice)], by = c("csn", "timeslice")), prob.1)
  
  # for all patients irrespective of timeslice - a calc of likely number of patients
  
  num_adm = seq(0,nrow(df), 1)# make an array from 0 admissions to max admissions (ie all patients admitted)
  pgf = poly_prod(df) # the probabilities of each of these
  
  distr = bind_cols(sample_time = time_pts[i], num_adm_pred = num_adm, probs = pgf, cdf = cumsum(pgf))
  
  distr_coll = bind_rows(distr_coll, distr)
  
  adm = sum(df$truth == 1)
  num_adm = bind_cols(sample_time = time_pts[i], num_adm = adm)
  
  adm_coll = bind_rows(adm_coll, num_adm)

}

# Create chart showing distributions --------------------------------------


cutoff_cdf_at_mult_days = tibble()


for (i in 1:nrow(adm_coll)) {
  
  distr = as_tibble(distr_coll[sample_time == adm_coll$sample_time[i]])
  
  actual_adm = adm_coll[sample_time == adm_coll$sample_time[i], num_adm]

  alpha_increments = 0.05
  cutoff_cdf_at = as_tibble(seq(alpha_increments,1,alpha_increments)) %>% rename(cutoff = value)
  cutoff_cdf_at$date = adm_coll$sample_time[i]
  cutoff_cdf_at$actual_adm = actual_adm
  cutoff_cdf_at$model_lower_num_adm = NA
  cutoff_cdf_at$model_lower_cdf = NA
  cutoff_cdf_at$model_upper_num_adm = NA
  cutoff_cdf_at$model_upper_cdf = NA
  
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
    }
    cutoff_cdf_at$model_upper_cdf[j] = distr$cdf[cutoff_cdf_at$model_upper_num_adm[j]]
    
  }
  
  cutoff_cdf_at_mult_days <- cutoff_cdf_at_mult_days %>% bind_rows(cutoff_cdf_at)
}


# to check the calculations of model_lower_cdf and model_upper_cdf,
# they should sum to the number of days covered by the model
# in today's meeting, Martin said he was apportioning the propabilities for each day 
# into a line from 0 to 1 - so its sum should be the number of days in the test
cutoff_cdf_at_mult_days %>% group_by(cutoff) %>% summarise(tot = sum(model_lower_cdf, na.rm = TRUE)) %>%  arrange(desc(tot))

# Enoch's code counts the number of days where the actual admission is less than the model threshold limits
cutoff_cdf_at_mult_days <- cutoff_cdf_at_mult_days %>% 
  mutate(actual_less_than_lower = ifelse(actual_adm < model_lower_num_adm, TRUE, FALSE),
         actual_less_than_upper = ifelse(actual_adm < model_upper_num_adm, TRUE, FALSE))


# he then divides by the number of days but he's doing this across all days
# therefore I will create a grouped version across all days

cutoff_cdf_normalised <- cutoff_cdf_at_mult_days %>% 
  group_by(cutoff) %>% summarise(actual_less_than_lower_limit = mean(actual_less_than_lower, na.rm = TRUE),
                                 actual_less_than_upper_limit = mean(actual_less_than_upper, na.rm = TRUE),
                                 model_upper_limits = mean(model_upper_cdf, na.rm = TRUE),
                                 model_lower_limits = mean(model_lower_cdf, na.rm = TRUE))


cutoff_cdf_normalised %>% pivot_longer(actual_less_than_lower_limit:model_lower_limits, 
                                       names_to = "model", 
                                       values_to = "proportion") %>% 
  ggplot(aes(x = cutoff, y = proportion, col = model)) + geom_point() + geom_line()  + theme_classic() +
  scale_x_continuous(breaks = seq(0, 1, .05)) +
  labs(title = paste0("Admission probability distribution evaluation - based on ", length(time_pts), " randomly sampled time points of interest"), 
       y = "Proportion of instances <= X on cdf",
       subtitle = "At each time point of interest, probability of admission is calculated for each patient in ED. These probabilities are converted into a cumulative probability distribution.\n
       20 equally spaced points on the cdf are chosen (shown on the X axis); these are the probability that the number of admissions is less than or equal to a number x \n
       The distribution generated by the model is used to retrieve what x (the number of admissions) would be at each of these 20 points on the cdf",
       x = "X")  +
  theme(legend.position = "bottom")




  
                


# To get probability distribution for time to admission for each timeslice ----------------------------
# each of the probs in the resulting tta_prob is a prob that can be put into a Bernouilli trial
# to get the number of successes (patients) needing a bed in that number of hours
# should this be a cumulative 



summ[adm %in% c("direct_adm", "indirect_adm"),ED_duration := difftime(last_ED_discharge, presentation_time, units = "mins")]

summ[,timeslice000 := 1]
summ[,timeslice015 := if_else(ED_duration > 15, 1, 0)]
summ[,timeslice030 := if_else(ED_duration > 30, 1, 0)]
summ[,timeslice060 := if_else(ED_duration > 60, 1, 0)]
summ[,timeslice120 := if_else(ED_duration > 120, 1, 0)]
summ[,timeslice180 := if_else(ED_duration > 180, 1, 0)]
summ[,timeslice240 := if_else(ED_duration > 240, 1, 0)]
summ[,timeslice300 := if_else(ED_duration > 300, 1, 0)]
summ[,timeslice360 := if_else(ED_duration > 360, 1, 0)]

# get time to admission after beginning of each timeslice
tta = data.table(summ %>% filter(!is.na(ED_duration)) %>% select(csn, timeslice000:timeslice360, ED_duration) %>% 
  pivot_longer(timeslice000:timeslice360, names_to = "in_timeslice"))
tta = tta[value ==1]
tta[, tta := case_when(value ==1 ~ as.numeric(ED_duration - as.numeric(gsub("timeslice","", in_timeslice))),
                       TRUE ~ NA_real_)]

# cut this to get whole number of hours until admission (cut at floor)
tta[, tta_hr := floor(tta/60)]

# generate number of visits in timslice in total
tta[, num_ts := sum(value), by = in_timeslice]

# generate propability of being admitted 
tta_prob = data.table(tta %>% filter(tta_hr >= 0) %>% 
                        group_by(in_timeslice, num_ts, tta_hr) %>% 
                        summarise(num_with_tta_in_hr = n()))
tta_prob[, prob := num_with_tta_in_hr/num_ts]

# plot tta after timeslice
tta_prob[tta_hr < 24] %>% 
  mutate(in_timeslice = as.numeric(gsub("timeslice", "", in_timeslice))) %>% 
  ggplot(aes(x = tta_hr, y = prob.1)) + geom_line() + facet_grid(.~in_timeslice) +
  labs(title = "Probability distribution for time to admission after beginning of timeslice (up to 24 hours)",
       x = "Time to admission (hrs)",
       y = "Probability")

# to see probs as a wider array
               
tta_prob %>% 
  select(in_timeslice, tta_hr, prob) %>% 
  mutate(in_timeslice = as.numeric(gsub("timeslice", "", in_timeslice))) %>% 
  pivot_wider(names_from = in_timeslice, values_from = prob)

tta_prob[, in_timeslice := as.numeric(gsub("timeslice", "", in_timeslice))]
setorder(tta_prob, in_timeslice)

tta_prob[tta_hr > 24, sum(prob), by = in_timeslice]
tta_prob[tta_hr > 48, sum(prob), by = in_timeslice]


# Looking at 360 timeslice ------------------------------------------------


summ[, los := difftime(last_ED_discharge, presentation_time, units = "hours")]
summ[los > 6 & los < 8, .N, by = adm2]
summ[los >= 48, .N]

summ[,adm2 := adm %in% c("direct_adm", "indirect_adm")]
summ[los > 6 & los < 48] %>% ggplot(aes(x = los - 6)) + geom_histogram(binwidth = 1) +
  labs(title = "Length of stay for patients with duration > 6 hours (by whether admitted) - up to 48 hours of total length of stay",
       x = "Length of stay beyond 6 hours (hours)") +
  facet_grid(adm2 ~ .) +
  geom_vline(xintercept = 1.5)


summ[los > 6 & los < 24*30] %>% ggplot(aes(x = los - 6)) + geom_histogram(binwidth = 1) +
  labs(title = "Length of stay for patients with duration > 6 hours (by whether admitted) - up to 30 days of total length of stay",
       x = "Length of stay beyond 6 hours (hours)") +
  facet_grid(adm2 ~ .)


# Now taking timeslices into account -------------------------------------------------

# cutting tta_prob for now at 48 hours
tta_prob = tta_prob[tta_hr <= 48]
setnames(tta_prob, "prob", "prob.1")

# either use a probability generating function
distry_all = data.table()

for (ts_ in timeslices) {
  name_ts = paste0("timeslice", ts_)
  
  pgfy = poly_prod(tta_prob[in_timeslice == name_ts])
  num_hr = seq(0, nrow(tta_prob[in_timeslice == name_ts]), 1)
  
  distry = bind_cols(timeslice = name_ts, num_hr = num_hr, prob = pgfy, cdf = cumsum(pgfy))
  distry_all = bind_rows(distry_all, distry)
}

distry_all %>%  ggplot(aes(x = num_hr, y = prob)) + geom_point() + facet_grid((timeslice ~ .))

# or something much simpler 
distry_4hr = tta_prob[tta_hr <=4, sum(prob.1), by = in_timeslice]

distrx_coll = data.table()

for (i in (1:length(time_pts))) {
  
  in_ED = summ[presentation_time < time_pts[i] &
                 last_ED_discharge > time_pts[i], .(csn, presentation_time, elapsed = difftime(time_pts[i], presentation_time, units = "mins"))]
  
  in_ED[, timeslice := case_when(elapsed < 15 ~ "timeslice000",
                                 elapsed < 30 ~ "timeslice015",
                                 elapsed < 60 ~ "timeslice030",
                                 elapsed < 120 ~ "timeslice060",
                                 elapsed < 180 ~ "timeslice120",
                                 elapsed < 240 ~ "timeslice180",
                                 elapsed < 300 ~ "timeslice240",
                                 elapsed < 360 ~ "timeslice300",
                                 TRUE ~ "timeslice360")]
  
  for (ts_ in timeslices) {
    name_ts = paste0("timeslice", ts_)
    
    if (nrow(in_ED[timeslice == name_ts]) > 0) {
      
      dfx = setorder(merge(in_ED[timeslice == name_ts], preds_all_ts[,.(csn, truth, response, prob.1, prob.0, timeslice)], by = c("csn", "timeslice")), prob.1)
      
      num_adm = seq(0,nrow(dfx), 1) # number of patients in timeslice at sample time
      pgfx = poly_prod(dfx) # the probabilities of each of these being admitted
      bernoulli_prob = distry_4hr[in_timeslice == name_ts, V1] # the probability of this being within four hours
      
      for (j in 1:length(num_adm)) {
        
        for (k in 0: num_adm[j]) {
          prob_in4 = dbinom(k, num_adm[j], bernoulli_prob)
          
          row = bind_cols(sample_time = time_pts[i], timeslice = name_ts, num_adm_pred = num_adm[j], prob_num_adm = pgfx[j], 
                          of_which_adm_in4_pred = k, of_which_prob_adm_in4 = prob_in4)
          
          distr = bind_rows(distr, row)
          
        } 
      }
    }
  }
}


# predictions for in 4 hours
distr = data.table(distr)
distr[, prob_num_adm_in4 := prob_num_adm * of_which_prob_adm_in4]

distr[, sum(prob_num_adm_in4), by = of_which_adm_in4_pred]
