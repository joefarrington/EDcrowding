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

get_random_dttm <- function(dttm_start, dttm_end) {
  dt <- as.numeric(difftime(dttm_end, dttm_start,unit="sec"))
  increment <- runif(1, 0, dt)
  return(dttm_start + increment)
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


get_prob_dist = function(time_window, time_pts, in_ED_all, preds_all_ts, tta_prob) {
  
  distr_coll = data.table()
  adm_coll = data.table()
  noone_in_ED = POSIXct()
  
  in_ED_all[, adm_in_time_window := case_when(remaining < time_window*60 & adm %in% c("direct_adm", "indirect_adm") ~ TRUE,
                                              adm %in% c("direct_adm", "indirect_adm") ~ FALSE,
                                              TRUE ~ NA)]
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



# Load data ---------------------------------------------------------------

# summary of csns to get minimum and maxium
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-04-13.rda")
summ = summ[!is.na(discharge_time)]
summ[, left_ED := coalesce(first_outside_proper_admission, last_inside_discharge)]

# added this while using only part of the dataset - need to change it later
summ = summ[date(presentation_time) >= '2020-03-19']

# Generate sample of time points of interest ------------------------------

set.seed(17L)
#  using validation set for post COVID
time_pts <- get_random_dttm(as.POSIXct('2020-03-19 00:00:00') + hours(12), as.POSIXct('2020-12-01 00:00:00') + hours(24))
last_pt <- time_pts

# each sample to be more than 12 hours and less than 24 hours after the previous one
while (last_pt + hours(12) < as.POSIXct('2020-12-29 00:00:00')) {
  next_pt <- get_random_dttm(last_pt + hours(12), last_pt + hours(24))
  time_pts <- c(time_pts, next_pt)
  last_pt <- next_pt
}



# Get everyone in ED ------------------------------------------------------



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



# Get data to provide to model --------------------------------------------

timeslices <- gsub("task", "", unique(in_ED_all$timeslice))
file_date = '2021-04-19'
model_date = '2021-04-19'
model_features = "alop"
use_dataset = "Post"

preds_all_ts <- data.table()

for (ts_ in timeslices) {
  
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

prob_dist = get_prob_dist(NA, time_pts, in_ED_all, preds_all_ts, tta_prob)

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



