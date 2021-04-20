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
      
      for (ts_ in unique(in_ED$timeslice)) {
        
        name_tsk <- paste0("task", ts_)
        tsk = get(name_tsk)
        tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
        tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
        
        ts = dm120[csn %in% in_ED$csn]
        name_ts <- paste0("dm", ts_, "p")
        
        

        # create task
        tsk = TaskClassif$new(id = name_ts, backend = ts ,target="adm") 
        tsk$col_roles$name = "csn"
        tsk$col_roles$feature = setdiff(tsk$col_roles$feature, "csn")
        tsk$positive = "1" # tell mlr3 which is the positive class
        name_tsk <- paste0("task", ts_)
        assign(name_tsk, tsk)
        
        pred_values = learner120$predict(tsk)
        
      }
      
      if(is.na(time_window)) {
        
        df = merge(in_ED, preds_all_ts[,.(csn, truth, prob.1, timeslice)], 
                            by = c("csn", "timeslice"), all.x = TRUE)
      } else {
        
        df = merge(in_ED, preds_all_ts[,.(csn, truth, prob.1, timeslice)], 
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
summ = summ[!is.na(discharge_time)]
summ[, left_ED := coalesce(first_outside_proper_admission, last_inside_discharge)]

# added this while using only part of the dataset - need to change it later
summ = summ[date(presentation_time) >= '2020-03-19']

timeslices <- c("000", "015", "030", "060", "090", "120", "180", "240", "300", "360", "480")
file_date = '2021-04-19'
model_date = '2021-04-19'

preds_all_ts <- data.table()

for (ts_ in timeslices) {
  
  name_ts <- paste0("dm", ts_, "p")
  ts = get(name_ts)
  
  # create task
  tsk = TaskClassif$new(id = name_ts, backend = ts ,target="adm") 
  tsk$col_roles$name = "csn"
  tsk$col_roles$feature = setdiff(tsk$col_roles$feature, "csn")
  tsk$positive = "1" # tell mlr3 which is the positive class
  name_tsk <- paste0("task", ts_)
  assign(name_tsk, tsk)
  
}
  
for (ts_ in timeslices) {
  
  # load timeslice 
  inFile = paste0("~/EDcrowding/predict-admission/data-raw/dm", ts_, "_", file_date, ".rda")
  load(inFile)
  
  # assign name dt to loaded file temporarily
  name_ts <- paste0("dm", ts_)
  dt = get(name_ts)
  name_tsk <- paste0("task", ts_)
  dt[, row_id := seq_len(nrow(dt))]
  
  inFile = paste0("~/EDcrowding/predict-admission/data-output/learner_", name_tsk, "_", model_date, ".rda")
  load(inFile)
  
  assign(paste0("learner", ts_), learner)
}


# Generate sample of time points of interest ------------------------------

set.seed(17L)
#  using validation set for post COVID
time_pts <- get_random_dttm(as.POSIXct('2020-12-01 00:00:00') + hours(12), as.POSIXct('2020-12-01 00:00:00') + hours(24))
last_pt <- time_pts

# each sample to be more than 12 hours and less than 24 hours after the previous one
while (last_pt + hours(12) < as.POSIXct('2020-12-29 00:00:00')) {
  next_pt <- get_random_dttm(last_pt + hours(12), last_pt + hours(24))
  time_pts <- c(time_pts, next_pt)
  last_pt <- next_pt
}


# Get probability distribution for time to admission for each timeslice ----------------------------

# now using left_ED and first_ED_admission to tighten the distribution
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
tta_prob = data.table(tta %>% filter(tta_hr >= 0) %>% 
                        group_by(timeslice, num_ts, tta_hr) %>% 
                        summarise(num_with_tta_in_hr = n()))
tta_prob[, prob := num_with_tta_in_hr/num_ts]
tta_prob[, cdf := cumsum(prob), by = timeslice]


# Create chart ------------------------------------------------------------


time_window = 3

prob_dist = get_prob_dist(NA, time_pts, summ, preds_all_ts, tta_prob)

# collect all predicted distributions for each time point of interest
distr_coll = prob_dist[[1]]

# now treat the full set of cdfs (all time points) as a single discrete variable
distr_coll[, upper_M_discrete_value := cdf]
distr_coll[, lower_M_discrete_value := lag(cdf), by = sample_time]
distr_coll[num_adm_pred == 0, lower_M_discrete_value := 0]

# outFile = paste0("EDcrowding/predict-admission/data-output/predicted_distribution_",today(),".csv")
# write.csv(distr_coll, file = outFile, row.names = FALSE)


# for the array of low cdf values (now considered a discrete distribution) work out its cdf

lower_M = distr_coll[, .(value = lower_M_discrete_value), probs]
setorder(lower_M, value)
lower_M[, cum_weight := cumsum(probs)]
lower_M[, cum_weight_normed := cumsum(probs)/length(time_pts)]
lower_M[, dist := "model lower"]

# same for high cdf values

upper_M = distr_coll[, .(value = upper_M_discrete_value), probs]
setorder(upper_M, value)
upper_M[, cum_weight := cumsum(probs)]
upper_M[, cum_weight_normed := cumsum(probs)/length(time_pts)]
upper_M[, dist := "model upper"]

# same for mid point

mid_M = distr_coll[, .(value = (upper_M_discrete_value+lower_M_discrete_value)/2, probs)]
setorder(mid_M, value)
mid_M[, cum_weight := cumsum(probs)]
mid_M[, cum_weight_normed := cumsum(probs)/length(time_pts)]
mid_M[, dist := "model mid"]


# compare the observed values against their predicted distribution 
# and find their position on the cdf; combine this into a distribution

adm_coll = prob_dist[[2]]


adm_coll = merge(adm_coll, 
                 distr_coll[, .(sample_time, num_adm = num_adm_pred, 
                                lower_E = lower_M_discrete_value, 
                                upper_E = upper_M_discrete_value)], 
                 by = c("sample_time", "num_adm"))

# To check the cdf of thess
# p = adm_coll %>% ggplot() + 
#   stat_ecdf(aes(x = lower_E), geom = "point") +
#   stat_ecdf(aes(x = upper_E), geom = "point") +
#   labs(title = "Cumulative distribution functions for E (upper and lower)",
#        x = "cdf value associated with number of patients admitted in a given instance",
#        y = "cdf of these values") 



# for the array of low cdf values (now considered a discrete distribution) work out its cdf

lower_E = adm_coll[, .(value = lower_E)]
setorder(lower_E, value)
lower_E_prob = lower_E[, .N, by = value]
lower_E_prob[, cum_weight := N/length(time_pts)]
lower_E_prob[, cum_weight_normed := cumsum(cum_weight)]
lower_E_prob[, dist := "actual lower"]

# same for high cdf values

upper_E = adm_coll[, .(value = upper_E)]
setorder(upper_E, value)
upper_E_prob = upper_E[, .N, by = value]
upper_E_prob[, cum_weight := N/length(time_pts)]
upper_E_prob[, cum_weight_normed := cumsum(cum_weight)]
upper_E_prob[, dist := "actual upper"]

# same for mid point

mid_E = adm_coll[, .(value = (upper_E+lower_E)/2)]
setorder(mid_E, value)
mid_E_prob = mid_E[, .N, by = value]
mid_E_prob[, cum_weight := N/length(time_pts)]
mid_E_prob[, cum_weight_normed := cumsum(cum_weight)]
mid_E_prob[, dist := "acutal mid"]


plot_data = bind_rows(lower_M, mid_M, upper_M, lower_E_prob, mid_E_prob, upper_E_prob)
plot_data %>% ggplot(aes(x = value, y = cum_weight_normed, colour = dist)) + geom_point() +
  labs(title = "Proportion of demand values <= threshold X on predicted cdf",
       x = "X",
       y = NULL) +
  theme_classic() +
  scale_color_manual(values = c("deeppink" , "chartreuse4" ,  "lightblue","black","black", "black", guide = NULL, name = NULL)) +
  theme(legend.position = "none") 



