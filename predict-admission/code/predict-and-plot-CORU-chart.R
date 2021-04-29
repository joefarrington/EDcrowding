# Load libraries ----------------------------------------------------------


library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)
library(polynom)

# for mlr3
library(mlr3)
library(mlr3learners)
library(mlr3proba)

# for shapley plot

# library(GGally)
# library(precrec)
# library(paradox)
# library(mlr3tuning)
library(mlr3fselect)
library(mlr3misc)

# Create functions --------------------------------------------------------


poly_prod = function(df){
  
  # polynomial(vector) is a polynomial t
  y = polynomial(c(1,0))# 
  for (n in 1:nrow(df)){
    y = y*polynomial(c(1-df$prob.1[n],df$prob.1[n]))
  }
  return(coef(y))
}



one_hot <- function(dt, cols="auto", dropCols=TRUE, dropUnusedLevels=FALSE){
  
  if(cols[1] == "auto") cols <- colnames(dt)[which(sapply(dt, function(x) is.factor(x) & !is.ordered(x)))]
  
  if(length(cols) == 0) {
    return(dt) }
  else {
    
    # Build tempDT containing and ID column and 'cols' columns
    tempDT <- dt[, cols, with=FALSE]
    tempDT[, ID := .I]
    setcolorder(tempDT, unique(c("ID", colnames(tempDT))))
    for(col in cols) set(tempDT, j=col, value=factor(paste(col, tempDT[[col]], sep="_"), levels=paste(col, levels(tempDT[[col]]), sep="_")))
    
    # One-hot-encode
    if(dropUnusedLevels == TRUE){
      newCols <- dcast(melt(tempDT, id = 'ID', value.factor = T), ID ~ value, drop = T, fun = length)
    } else{
      newCols <- dcast(melt(tempDT, id = 'ID', value.factor = T), ID ~ value, drop = F, fun = length)
    }
    
    # Combine binarized columns with the original dataset
    result <- cbind(dt, newCols[, !"ID"])
    
    # If dropCols = TRUE, remove the original factor columns
    if(dropCols == TRUE){
      result <- result[, !cols, with=FALSE]
    }
    
    return(result)
  }
}



# Make predictions --------------------------------------------------------

make_predictions = function(time_pts, summ) {
  
  in_ED_all = data.table()
  
  # get relevant timeslice for all in ED at that time point of interest
  
  for (i in (1:length(time_pts))) {
    
    in_ED = summ[first_ED_admission < time_pts[i] & left_ED > time_pts[i], 
                 .(csn, first_ED_admission, adm, 
                   left_ED,
                   elapsed = difftime(time_pts[i], first_ED_admission, units = "mins"),
                   remaining = difftime(left_ED, time_pts[i], units = "mins"))]
    
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
    
    in_ED[, time_pt := time_pts[i]]
    
    in_ED_all = bind_rows(in_ED_all, in_ED)
  }
  
  # Get data to provide to model 
  
  timeslices <- gsub("task", "", unique(in_ED_all$timeslice))
  file_date = '2021-04-19'
  model_date = '2021-04-19'
  model_features = "alop"
  use_dataset = "Post"
  
  preds_all_ts <- data.table()
  
  for (ts_ in timeslices) {
    
    print(ts_)
    
    # load timeslice 
    inFile = paste0("~/EDcrowding/predict-admission/data-raw/dm", ts_, "_", file_date, ".rda")
    load(inFile)
    
    name_ts <- paste0("dm", ts_)
    dt = get(name_ts)
    
    #  select dataset (pre or post covid or both if use_dataset is null)
    if (!is.null(use_dataset)) {
      dt = dt[a_epoch == use_dataset]
      dt[, a_epoch := NULL]
    }
    
    # select csns of interest
    dt = dt[csn %in% in_ED_all[timeslice == paste0("task", ts_), csn]]
    csns <- dt$csn
    dt[, row_id := seq_len(nrow(dt))]
    
    # create vectors identifying test ids
    assign(paste0("task", ts_, "_ids"), dt$row_id)
    
    # remove train-val-test label and row_id so not included in features
    dt[, in_set := NULL]
    dt[, row_id := NULL]
    
    # remove features not wanted in model
    if (model_features != "alop") {
      if (!grepl("p", model_features)) {
        dt[, colnames(dt)[grep("^p_", colnames(dt))] := NULL]
      }
      if (!grepl("a", model_features)) {
        dt[, colnames(dt)[grep("^a_", colnames(dt))] := NULL]
      }
      if (!grepl("l", model_features)) {
        dt[, colnames(dt)[grep("^l_", colnames(dt))] := NULL]
      }
      if (!grepl("o", model_features)) {
        dt[, colnames(dt)[grep("^o_", colnames(dt))] := NULL]
      }
    }
    
    # encode factors
    ts <- one_hot(cols = "auto", dt = as.data.table(dt),  dropUnusedLevels=TRUE)
    ts[,adm:=as.factor(adm)] 
    
    name_tsk <- paste0("task", ts_)
    
    
    # add other features that might be missing
    inFile = paste0("~/EDcrowding/predict-admission/data-output/features_", name_tsk, "_", file_date, ".rda")
    load(inFile)
    
    ts_cols = colnames(ts)
    missing_features = feature_list[!feature_list %in% ts_cols] # load this from file later
    
    missing_cols <- data.table(matrix(0, nrow = nrow(ts), ncol = length(missing_features)))
    ts = bind_cols(ts, missing_cols)
    colnames(ts) = c(ts_cols, missing_features)
    
    # # assign to named data table
    # name_tsp <- paste0("dm", ts_, "p")
    # assign(name_tsp, ts)
    # 
    # create task
    tsk = TaskClassif$new(id = name_ts, backend = ts, target="adm") 
    tsk$col_roles$name = "csn"
    tsk$col_roles$feature = setdiff(tsk$col_roles$feature, "csn")
    tsk$positive = "1" # tell mlr3 which is the positive class
    assign(name_tsk, tsk)
    
    
    # load learner
    inFile = paste0("~/EDcrowding/predict-admission/data-output/learner_", name_tsk, "_", model_date, ".rda")
    load(inFile)
    
    # get predictions
    pred_values = as.data.table(learner$predict(tsk, row_ids = dt$row_id))
    pred_values$timeslice = name_tsk
    pred_values$csn = csns
    
    preds_all_ts <- bind_rows(preds_all_ts, pred_values)
    
  }
  
  return(list(in_ED_all, preds_all_ts))
  
}


# Get probability distribution from predictions  -----------------------------



get_prob_dist = function(time_window, time_pts, in_ED_all, preds_all_ts, tta_prob) {
  
  distr_coll = data.table()
  adm_coll = data.table()
  noone_in_ED = POSIXct()
  
  if(!is.na(time_window)) {
    in_ED_all[, adm_in_time_window := case_when(remaining < time_window*60 & adm %in% c("direct_adm", "indirect_adm") ~ TRUE,
                                                adm %in% c("direct_adm", "indirect_adm") ~ FALSE,
                                                TRUE ~ NA)]
  }

  for (i in (1:length(time_pts))) {
      
    in_ED = in_ED_all[time_pt == time_pts[i]]

    if (nrow(in_ED) == 0) {
      noone_in_ED = c(noone_in_ED, time_pts[i])
      
    } else {
      
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



# Get prob dist with not-yet-arrived --------------------------------------



get_prob_dist_inc_not_yet_arrived = function(time_window, time_pts, in_ED_all, preds_all_ts, tta_prob, summ) {
  
  distr_coll = data.table()
  adm_coll = data.table()
  noone_in_ED = POSIXct()
  
  if(!is.na(time_window)) {
    in_ED_all[, adm_in_time_window := case_when(remaining < time_window*60 & adm %in% c("direct_adm", "indirect_adm") ~ TRUE,
                                                adm %in% c("direct_adm", "indirect_adm") ~ FALSE,
                                                TRUE ~ NA)]
  }
  
  for (i in (1:length(time_pts))) {
    
    in_ED = in_ED_all[time_pt == time_pts[i]]
    
    if (nrow(in_ED) == 0) {
      noone_in_ED = c(noone_in_ED, time_pts[i])
      
      # need to add something here
      
    } else {
        
      df = merge(in_ED, preds_all_ts[,.(csn, truth, prob.1, timeslice)], 
                 by = c("csn", "timeslice"), all.x = TRUE)
      df = merge(df, tta_prob[tta_hr == time_window, .(timeslice, prob_adm_in_time_window = cdf)], 
                 by = "timeslice")
      df[, truth := adm_in_time_window]
      df[, prob.1 := prob.1 * prob_adm_in_time_window]
    }
    
    # 
    # For the pre-COVID dataset, 
    # [1] "Max admissions"
    # [1] "In 2 hours:  17"
    # [1] "In 3 hours:  22"
    # [1] "In 4 hours:  27"
    # [1] "In 6 hours:  36"
    # [1] "In 8 hours:  45"
    # [1] "In 12 hours:  47"
    # 
    # For the post-COVID dataset
    # [1] "Max admissions"
    # [1] "In 2 hours:  14"
    # [1] "In 3 hours:  20"
    # [1] "In 4 hours:  26"
    # [1] "In 6 hours:  33"
    # [1] "In 8 hours:  38"
    # [1] "In 12 hours:  39"
    
    # > not_in_ED_yet_all[, mean(N), by = time_window]
    # time_window          V1
    # 1:           2  0.06873185
    # 2:           3  0.28073572
    # 3:           4  0.89545015
    # 4:           6  3.18005808
    # 5:           8  5.83059051
    # 6:          12 11.73959342
    # 
    # make an array from 0 admissions to max possible number of admissions (ie all patients admitted)
    num_adm_ = seq(0, nrow(df) + 20, 1)
    # the probabilities of each of these numbers being admitted
    probs_in_ED = poly_prod(df)
    probs_in_ED = c(probs_in_ED, rep(0, nrow(df) + 20-length(probs_in_ED)))
    probs_not_yet_arrived = dpois(num_adm_, lambda = 5.83059051)
    probs = rowSums(outer(probs_not_yet_arrived, probs_in_ED, "*"))
    
    
    distr = bind_cols(sample_time = time_pts[i], num_adm_pred = num_adm_, 
                      probs = probs, cdf = cumsum(probs))
    distr_coll = bind_rows(distr_coll, distr)
    
    num_adm_all = summ[adm %in% c("indirect_adm", "direct_adm") & 
                       first_outside_proper_admission > time_pts[i] &
                         first_outside_proper_admission < time_pts[i] + hours(time_window), .N]
    
    num_adm = bind_cols(sample_time = time_pts[i], num_in_ED = nrow(in_ED), 
                        num_adm = num_adm_all)
    adm_coll = bind_rows(adm_coll, num_adm)
  }
  
  return(list(distr_coll, adm_coll, noone_in_ED))
  
}

# Plot chart --------------------------------------------------------------


plot_CORU_chart = function(time_window, time_pts, prob_dist, subtitle) {
  
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
  mid_E_prob[, dist := "actual mid"]
  
  
  plot_data = bind_rows(lower_M, mid_M, upper_M, lower_E_prob, mid_E_prob, upper_E_prob)
  p = plot_data %>% ggplot(aes(x = value, y = cum_weight_normed, colour = dist)) + geom_point() +
    labs(title = "Proportion of demand values <= threshold X on predicted cdf",
         # subtitle = subtitle,
         x = "X",
         y = NULL) +
    theme_classic() +
    scale_color_manual(values = c("deeppink" , "chartreuse4" ,  "lightblue","black","black", "black", guide = NULL, name = NULL)) +
    theme(legend.position = "none") 
  
  return(p)
}


# Plot Czado chart --------------------------------------------------------



czado_nonrandomized_pit_buckets = function(predicted_distribution, n_buckets) {
  
  quantiles = seq(0,1,1/n_buckets)  
  
  cumulative_output = czado_nonrandomized_pit_cumulative(predicted_distribution$upper_E, 
                                                         predicted_distribution$lower_E,
                                                         quantiles)
  return(diff(cumulative_output))
}

czado_nonrandomized_pit_cumulative = function(upper = adm_coll$upper_E, 
                                              lower = adm_coll$lower_E, 
                                              quantiles){
  
  output = as.numeric()
  for (u in quantiles) { 
    
    # Eq(2) and Eq(3) from Czado et al (2009)
    # implemented in Python as:
    # output.append(((u - lower) / (upper - lower)).clip(0, 1).mean()
    
    ratio = ((u - lower) / (upper - lower))
    
    # R doesn't seem to have a clip function
    ratio = case_when(ratio < 0 ~ 0, 
                      ratio > 1 ~ 1,
                      TRUE ~ ratio)
    
    output = c(output, mean(ratio))
    
    
  } 
  
  return(output)
}


# Load data ---------------------------------------------------------------

# summary of csns to get minimum and maxium
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-04-13.rda")
summ = summ[!is.na(discharge_time)]
summ[, left_ED := coalesce(first_outside_proper_admission, last_inside_discharge)]

# added this while using only part of the dataset - need to change it later
summ = summ[date(presentation_time) >= '2020-03-19']

# load file for poisson distributions - generated empirically 
poisson_file <- paste0("~/EDcrowding/flow-mapping/data-output/poisson_means.rda")
load(poisson_file)


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


# CORU plots for training data ------------------------------------------------------------

start_of_set = with_tz(as.POSIXct('2020-12-01'), tz = "UTC")
end_of_set = with_tz(as.POSIXct('2020-12-29'), tz = "UTC")
next_dt = start_of_set

time_pts = POSIXct()
# each sample to be more than 12 hours and less than 24 hours after the previous one
while (next_dt < end_of_set) {
  next_pt <- next_dt + c(hours(6), hours(12), hours(15) + minutes(30), hours(22))
  time_pts <- c(time_pts, next_pt)
  next_dt = next_dt + days(1)
}


preds_train = make_predictions(time_pts, summ)
# make_predictions returns two datasets: in_ED_all and preds_all_ts
# input these to chart

time_window = NA
prob_dist_train_NA = get_prob_dist(time_window, time_pts_train, preds_train[[1]], preds_train[[2]], tta_prob)
p_train_NA = plot_CORU_chart(time_window, time_pts_train, prob_dist_train_NA, 
                        paste0("Post COVID training set; random sample of ", length(time_pts_train), " time points"))

# plots for various time windows
for (time_window in c(2, 3, 4, 6, 8, 12)) {
  prob_dist_train = get_prob_dist(time_window, time_pts_train, preds_train[[1]], preds_train[[2]], tta_prob)
  p_train = plot_CORU_chart(time_window, time_pts_train, prob_dist_train, 
                          paste0(time_window, " hour time window"))
  p_train = p_train + labs(title =  paste0(time_window, " hour time window"),
                           subtitle = paste0("Post COVID training set; ", length(time_pts_train), " time points"))
  plot_name = paste0("p_train_", get("time_window"))
  assign(plot_name, p_train)
}

library(gridExtra)
grid.arrange(p_train_2, p_train_3, p_train_4, p_train_6, p_train_8, p_train_12, ncol = 3, nrow = 2)





# Validation data - no time window ----------------------------------------

set.seed(17L)

start_of_set = with_tz(as.POSIXct('2020-12-01'), tz = "UTC")
end_of_set = with_tz(as.POSIXct('2020-12-29'), tz = "UTC")
#  find the first random date
time_pts_val <- get_random_dttm(start_of_set + hours(4), 
                            start_of_set + hours(8)) 
last_pt <- time_pts_val 

# each sample to be more than 12 hours and less than 24 hours after the previous one
while (last_pt + hours(12) < end_of_set) {
  next_pt <- get_random_dttm(last_pt + hours(4), last_pt + hours(8))
  time_pts_val <- c(time_pts_val, next_pt)
  last_pt <- next_pt
}

preds_val = make_predictions(time_pts_val, summ)
# preds returns two datasets: in_ED_all and preds_ts_all
# input these to chart

time_window = NA
prob_dist = get_prob_dist(time_window, time_pts_val, preds_val[[1]], preds_val[[2]], tta_prob)
p_val_NA = plot_CORU_chart(time_window, prob_dist, 
                        paste0("Post COVID validation set; random sample of ", length(time_pts_val), " time points"))



# plot comparing training and validation set
library(gridExtra)
grid.arrange(p_train_NA, p_val_NA, ncol = 2, nrow = 1)

# plots for various time windows
for (time_window in c(2, 3, 4, 6, 8, 12)) {
  prob_dist_val = get_prob_dist(time_window, time_pts_val, preds_val[[1]], preds_val[[2]], tta_prob)
  p_val = plot_CORU_chart(time_window, time_pts_val, prob_dist_val, 
                     paste0(time_window, " hour time window"))
  p_val = p_val + labs(title =  paste0(time_window, " hour time window"),
                       subtitle = paste0("Post COVID validation set; ", length(time_pts_val), " time points"))
  
  plot_name = paste0("p_val_", get("time_window"))
  assign(plot_name, p_val)
}

grid.arrange(p_val_2, p_val_3, p_val_4, p_val_6, p_val_8, p_val_12, ncol = 3, nrow = 2)

# Other exploration -------------------------------------------------------

plot_Czado_chart = function(time_pts, prob_dist, subtitle) {
  
  
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
  mid_E_prob[, dist := "actual mid"]
  
  output_c = czado_nonrandomized_pit_cumulative(adm_coll$upper_E, adm_coll$lower_E, quantiles)
  
  plotdata = bind_cols(expected = quantiles, output_c = output_c, distribution = "observed") %>% 
    bind_rows(bind_cols(expected = quantiles, output_c = quantiles, distribution = "expected"))
  czado_cum = plotdata %>% ggplot(aes(x = expected, y = output_c, col = distribution)) + geom_line() + geom_point() +
    labs(x = "Expected fraction in quantile", 
         y = "Observed fraction in quantile",
         title = "Calibration plot for post-COVID XGBoost model",
         subtitle = subtitle) +
    theme_classic() +
    scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0,1)) +
    theme(legend.position = "none")
  
 
  czado_pit = bind_cols(rel_freq = czado_nonrandomized_pit_buckets(adm_coll, 20), 
                        # pit = as.character(sprintf("%.2f", quantiles[1:length(quantiles)-1]))) %>%
                        pit = quantiles[1:length(quantiles)-1]) %>%
    ggplot(aes(x = pit+0.025, y = rel_freq)) + geom_bar(stat = "identity", fill = "#00BFC4") +
    labs(x = "Probability integral transform", 
         y = "Relative frequency",
         title = "Calibration plot for post-COVID XGBoost model",
         subtitle = subtitle) +
    theme_classic() +
    geom_hline(yintercept = 0.05, linetype = "dashed", col = "#F8766D" ) +
    scale_x_continuous(breaks = seq(0, 1, 0.05), limits = c(0,1)) +
    theme(legend.position = "none")
  
  return(list(czado_cum, czado_pit))
  
  
}

cz = plot_Czado_chart(time_pts_train, prob_dist_train, subtitle = "Training set")
grid.arrange(cz[[1]], cz[[2]], ncol = 2, nrow = 1)

cz = plot_Czado_chart(time_pts_val, prob_dist_val, subtitle = "Validation set")
grid.arrange(cz[[1]], cz[[2]], ncol = 2, nrow = 1)



# Including people not yet arrived ----------------------------------------


prob_dist_inc = get_prob_dist_inc_not_yet_arrived(time_window, time_pts_val, preds_val[[1]], preds_val[[2]], tta_prob, summ)
p_val_8_inc = plot_chart(time_window, time_pts_val, prob_dist_inc, 
                           paste0("Post COVID validation set; random sample of ", length(time_pts_val), " time points"))
