# Load libraries ----------------------------------------------------------


library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)
library(polynom)
library(readr)

# for mlr3
library(mlr3)
library(mlr3learners)
library(mlr3proba)
library(mlr3fselect)
library(mlr3misc)


# Set programme instructions ----------------------------------------------



file_date = '2021-05-17'
dm_file_date = '2021-05-19'
model_date = '2021-05-24' # alop models are currently 24.5 for Pre, Post | 26.5 for Pre + Pots | aop models currently 25.5
model_features = "alop"
use_dataset = "Post"
tsk_ids = "Val"


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

make_predictions = function(time_pts, summ, dm_file_date, model_date,  model_features,  use_dataset) {
  
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
                                   elapsed < 720 ~ "task480",
                                   TRUE ~ "task720")]
    
    in_ED[, time_pt := time_pts[i]]
    
    in_ED_all = bind_rows(in_ED_all, in_ED)
  }
  
  # Get data to provide to model 
  
  timeslices <- gsub("task", "", unique(in_ED_all$timeslice))

  preds_all_ts <- data.table()
  
  for (ts_ in timeslices) {
    
    print(ts_)
    
    # load timeslice 
    inFile = paste0("~/EDcrowding/predict-admission/data-raw/dm", ts_, "_", dm_file_date, ".rda")
    load(inFile)
    
    name_ts <- paste0("dm", ts_)
    dt = get(name_ts)
    
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
    
    
    # Pre + Post learners needs two flags a_epoch_Pre and a_epoch_Post
    # therefore set a_epoch to be a factor
    dt$a_epoch = factor(dt$a_epoch, levels = c("Pre", "Post"))
    
    
    # encode factors - note, if set dropUnusedLevels to false, no need to bring in additional features
    # this is only possible because validation set has been created at same time as training set 
    # therefore all values present in eaach factor will be present here
    ts <- one_hot(cols = "auto", dt = as.data.table(dt),  dropUnusedLevels=FALSE)
    ts[,adm:=as.factor(adm)] 
    
    name_tsk <- paste0("task", ts_)

    
    
    # # add other features used in training the model that might be missing from this one-hot process
    # 
    # features_file <- paste0("~/EDcrowding/predict-admission/data-output/xgb_",model_features,
    #                         # "_", gsub(" +", "", use_dataset),
    #                         "_features_",name_tsk,"_",model_date, ".rda")
    # load(features_file)
    # 
    # ts_cols = colnames(ts)
    # missing_features = feature_list[!feature_list %in% ts_cols] 
    # 
    # missing_cols <- data.table(matrix(0, nrow = nrow(ts), ncol = length(missing_features)))
    # ts = bind_cols(ts, missing_cols)
    # colnames(ts) = c(ts_cols, missing_features)
    
    # # create task
    # tsk = TaskClassif$new(id = name_ts, backend = ts, target="adm") 
    # tsk$col_roles$name = "csn"
    # tsk$col_roles$feature = setdiff(tsk$col_roles$feature, "csn")
    # tsk$positive = "1" # tell mlr3 which is the positive class
    # assign(name_tsk, tsk)
    # 
    
    
    # load learner
    
    learner_file <- paste0("~/EDcrowding/predict-admission/data-output/xgb_",model_features,
                            "_", gsub(" +", "", use_dataset),
                            "_learner_",name_tsk,"_",model_date, ".rda")
    load(learner_file)
    
    # get predictions - if doing real-time- predictions
    pred_values = as.data.table(predict(learner, ts, predict_type = "prob"))
    setnames(pred_values, c("1", "0"), c("prob.1", "prob.0"))
    pred_values$timeslice = name_tsk
    pred_values$csn = csns
    
    preds_all_ts <- bind_rows(preds_all_ts, pred_values)
    
  }
  
  return(list(in_ED_all, preds_all_ts))
  
}



# Get probability distribution from predictions  -----------------------------



get_prob_dist = function(time_pts, in_ED_all, preds_all_ts, tta_prob, poisson_not_yet_arrived, preds_nya_ngboost) {
  
  distr_coll = data.table()
  pt_estimates_coll = data.table()
  
  for (i in (1:length(time_pts))) {
    
    print(i)
    
    # get patients in ED at this time and set characteristics of time point

    in_ED = in_ED_all[time_pt == time_pts[i]]
    num_adm_ = seq(0,nrow(in_ED), 1)
    get_report = case_when(as.numeric(substr(time_pts[i], 12,13)) == 6 ~ "6:00",
                           TRUE ~ substr(time_pts[i], 12,16))
    is_weekend = ifelse(weekdays(time_pts[i], abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0)

    if (nrow(in_ED) == 0) {
      
      # if there is noone in ED
      # only need to do time varying poission for patients not yet arrived
      
      for (time_window in c(4, 8)) {
        
        # if at end of time points, skip time window calcs
        
        if (!(i == length(time_pts) |
            i == length(time_pts)-1 & time_window == 8)) { 
          
          time_window_ = time_window
          
          # using empirically derived poisson mean, 
          # generate probs of each number of not-yet-arrived admissions
          # assuming that a maximum of 20 people can arrive after the time point of interest
          # and be admitted before the end of the time window after that
          probs_not_yet_arrived = 
            dpois(seq(0, 20 ,1),
                  lambda = poisson_not_yet_arrived[time_window == time_window_ &
                                                     weekend == is_weekend &
                                                     time_of_report == get_report,
                                                   poisson_mean])
          # save this distribution
          distr = data.table(bind_cols(time_of_report = time_pts[i],
                                       num_adm_pred = seq(0, 20 ,1),
                                       probs = probs_not_yet_arrived, cdf = cumsum(probs_not_yet_arrived),
                                       time_window = time_window,
                                       inc_nya = TRUE,
                                       dist = paste("Empirical poisson")))
          # derive point estimates from this distribution
          pt_estimates_coll_ = 
            data.table(bind_cols(time_of_report = time_pts[i],
                                  num_in_ED = 0, 
                                 # retrieve true number of admissions within time window of people
                                 # who arrived after the time point of interest
                                  truth = nrow(summ[first_ED_admission > time_pts[i] &
                                                      adm %in% c("direct_adm", "indirect_adm") &
                                                      left_ED <= time_pts[i] + hours(time_window)]),
                                 # use the distribution to get an expected value and 5th and 9th quantile on cdf
                                  expected_value = distr[, .SD[which.max(probs)], by = time_of_report][, num_adm_pred],
                                 quantile5 = ifelse(nrow(distr[cdf<0.05]) == 0, min(distr$num_adm_pred), distr[cdf<0.05, max(num_adm_pred)]),
                                 quantile95 = ifelse(nrow(distr[cdf<0.95]) == 0, min(distr$num_adm_pred), distr[cdf<0.95, max(num_adm_pred)]),
                                 time_window = time_window,
                                 inc_nya = TRUE,
                                 dist = paste("Empirical poisson")))

          distr_coll = bind_rows(distr_coll, distr)
          pt_estimates_coll = bind_rows(pt_estimates_coll, pt_estimates_coll_)
          
          # using poisson mean from ngboost predictions
          # generate probs of each number of not-yet-arrived admissions
          # using same method above
          
          probs_not_yet_arrived = 
            dpois(seq(0, 20 ,1),
                  lambda = preds_nya_ngboost[time_window == time_window_ 
                                             & DateTime == DateTime[i], 
                                             poisson_mean])
          
          distr = data.table(bind_cols(time_of_report = time_pts[i],
                                       num_adm_pred = seq(0, 20 ,1),
                                       probs = probs_not_yet_arrived, cdf = cumsum(probs_not_yet_arrived),
                                       time_window = time_window,
                                       inc_nya = TRUE,
                                       dist = paste("NGBoost poisson")))

          pt_estimates_coll_ = 
            data.table(bind_cols(time_of_report = time_pts[i],
                                 num_in_ED = 0, 
                                  truth = nrow(summ[first_ED_admission > time_pts[i] &
                                                      adm %in% c("direct_adm", "indirect_adm") &
                                                      left_ED <= time_pts[i] + hours(time_window)]),
                                  expected_value = distr[, .SD[which.max(probs)], by = time_of_report][, num_adm_pred],
                                 quantile5 = ifelse(nrow(distr[cdf<0.05]) == 0, min(distr$num_adm_pred), distr[cdf<0.05, max(num_adm_pred)]),
                                 quantile95 = ifelse(nrow(distr[cdf<0.95]) == 0, min(distr$num_adm_pred), distr[cdf<0.95, max(num_adm_pred)]),
                                 time_window = time_window,
                                 inc_nya = TRUE,
                                 dist = paste("NGBoost poisson")))

          distr_coll = bind_rows(distr_coll, distr)
          pt_estimates_coll = bind_rows(pt_estimates_coll, pt_estimates_coll_)
        }

      }
      

      
    } else {
      
      # since there are patients in ED
      # we first derive a probability distribution for each possible number of admission
      
      # get the probability of admission for each individual
      df = merge(in_ED, preds_all_ts[,.(csn, prob.1, timeslice)], 
                 by = c("csn", "timeslice"), all.x = TRUE)
      
      # use the generating function to create a distribution from these probs
      probs_in_ED = poly_prod(df) 
      
      # save the true number of admissions of these patients
      df[, truth := case_when(adm %in% c("direct_adm", "indirect_adm") ~ 1,
                              TRUE ~ 0)]
      
      # save the distribution
      distr = data.table(bind_cols(time_of_report = time_pts[i],
                             num_adm_pred = num_adm_,
                             probs = probs_in_ED, cdf = cumsum(probs_in_ED),
                             time_window = NA,
                             inc_nya = FALSE,
                             dist = paste("Only patients in ED")))
      
      
      # derive point estimates from this distribution
      pt_estimates_coll_ = 
        data.table(bind_cols(time_of_report = time_pts[i],
                   num_in_ED = nrow(df), 
                   truth = sum(df$truth),
                   # use the distribution to get an expected value and 5th and 9th quantile on cdf
                   expected_value = distr[, .SD[which.max(probs)], by = time_of_report][, num_adm_pred],
                   quantile5 = ifelse(nrow(distr[cdf<0.05]) == 0, min(distr$num_adm_pred), distr[cdf<0.05, max(num_adm_pred)]),
                   quantile95 = ifelse(nrow(distr[cdf<0.95]) == 0, min(distr$num_adm_pred), distr[cdf<0.95, max(num_adm_pred)]),
                   time_window = NA,
                   inc_nya = FALSE,
                   dist = paste("Only patients in ED")))

      distr_coll = bind_rows(distr_coll, distr)
      pt_estimates_coll = bind_rows(pt_estimates_coll, pt_estimates_coll_)
      
      
      for (time_window in c(4, 8)) {
        
        time_window_ = time_window
        
        # if at end of time points, skip time window calcs
        
        if (!(i == length(time_pts) |
              i == length(time_pts)-1 & time_window == 8)) {
          
          # use empirically derived data to get probability of admission within time window
          df_ = merge(df[, .(prob.1, csn, timeslice = as.numeric(gsub("task", "", timeslice)), adm, left_ED)], 
                      tta_prob[tta_hr == time_window & time_of_report == get_report, 
                               .(timeslice, prob_adm_in_time_window = cdf)], 
                      by = "timeslice", all.x = TRUE)
          
          # then get distribution using generating function
          # but this time using joint probability of admission and admission within time window
          # where empirical data has zero prob, impute a small value to avoid NA errors
          df_[prob_adm_in_time_window == 0, prob_adm_in_time_window := 0.0001]
          df_[, prob.1 := prob.1 * prob_adm_in_time_window]
          probs_in_ED = poly_prod(df_) 
          
          # save the true number of admissions of these patients within the time window
          df_[, truth := case_when(adm %in% c("direct_adm", "indirect_adm") & 
                                     difftime(left_ED, time_pts[i], units = "hours") <= time_window ~ 1,
                                   TRUE ~ 0)]
          
          # save the distribution 
          distr = data.table(bind_cols(time_of_report = time_pts[i],
                                       num_adm_pred = num_adm_,
                                       probs = probs_in_ED, cdf = cumsum(probs_in_ED),
                                       time_window = time_window_,
                                       inc_nya = FALSE,
                                       dist = paste("Only patients in ED")))
          
          # derive point estimates from this distribution
          pt_estimates_coll_ = 
            data.table(bind_cols(time_of_report = time_pts[i],
                                 num_in_ED = nrow(df_), 
                                  truth = sum(df_$truth),
                                 expected_value = distr[, .SD[which.max(probs)], by = time_of_report][, num_adm_pred],
                                 quantile5 = ifelse(nrow(distr[cdf<0.05]) == 0, min(distr$num_adm_pred), distr[cdf<0.05, max(num_adm_pred)]),
                                 quantile95 = ifelse(nrow(distr[cdf<0.95]) == 0, min(distr$num_adm_pred), distr[cdf<0.95, max(num_adm_pred)]),
                                 time_window = time_window_,
                                 inc_nya = FALSE,
                                 dist = paste("Only patients in ED")))
          
          distr_coll = bind_rows(distr_coll, distr)
          pt_estimates_coll = bind_rows(pt_estimates_coll, pt_estimates_coll_)
          
          # -----
          # for interest, create a distribution with true numbers of not-yet-arrived
          # and combine with projected probability distribution
          true_nya = nrow(summ[first_ED_admission > time_pts[i] &
                                 adm %in% c("direct_adm", "indirect_adm") &
                                 left_ED <= time_pts[i] + hours(time_window)])
          
          num_adm_true_nya = seq(0, true_nya + nrow(in_ED), 1)
          probs_true_nya = c(rep(0, true_nya), probs_in_ED)
          
          # save the distribution 
          distr = data.table(bind_cols(time_of_report = time_pts[i],
                                       num_adm_pred = num_adm_true_nya,
                                       probs = probs_true_nya, cdf = cumsum(probs_true_nya),
                                       time_window = time_window_,
                                       inc_nya = TRUE,
                                       dist = paste("Patients in ED + true nya")))          
          # derive point estimates from this distribution
          pt_estimates_coll_ = 
            data.table(bind_cols(time_of_report = time_pts[i],
                                 num_in_ED = nrow(df_), 
                                 truth = sum(df_$truth) + true_nya,
                                 expected_value = distr[, .SD[which.max(probs)], by = time_of_report][, num_adm_pred],
                                 quantile5 = ifelse(nrow(distr[cdf<0.05]) == 0, min(distr$num_adm_pred), distr[cdf<0.05, max(num_adm_pred)]),
                                 quantile95 = ifelse(nrow(distr[cdf<0.95]) == 0, min(distr$num_adm_pred), distr[cdf<0.95, max(num_adm_pred)]),
                                 time_window = time_window_,
                                 inc_nya = TRUE,
                                 dist = paste("Patients in ED + true nya")))  
          
          distr_coll = bind_rows(distr_coll, distr)
          pt_estimates_coll = bind_rows(pt_estimates_coll, pt_estimates_coll_)
          
          # -----------
          # now create distributions including patients not yet arrived
          # first using empirically derived poisson mean as before
          
          # generate probs of each number of not-yet-arrived admissions
          time_window_ = time_window
          probs_not_yet_arrived = dpois(c(num_adm_ , seq(max(num_adm_) + 1, max(num_adm_+20),1)),
                                        lambda = poisson_not_yet_arrived[time_window == time_window_ &
                                                                           weekend == is_weekend &
                                                                           time_of_report == get_report,
                                                                         poisson_mean])
          
          # the random variable (number of admissions) is a combination of 
          # admissions from those in ED (indexed by k), and those not yet arrived (indexed by j)
          # assuming that a maximum of 20 people can arrive after the time point of interest
          # and be admitted before the end of the time window after that
          # then for each pair of values of each of these two random variables 
          # calculate the probability of the combination, and sum the random variables value
          # note that the distributions starts at zero so subject 1 from each index
          dist_nya = data.table()
          
          for (k in 1:length(probs_in_ED)) {
            for (j in 1:length(probs_not_yet_arrived)) {
              
              tot_adm_ = k-1 + j-1
              prob_tot_ = probs_in_ED[k] * probs_not_yet_arrived [j]
              row = data.table(num_adm_pred = tot_adm_, prob_tot = prob_tot_)
              
              dist_nya = bind_rows(dist_nya, row)
            }
          }
          
          # then sum the probabilities wherever the sum of the pairs is the same
          dist_nya = dist_nya[, .(probs = sum(prob_tot)), by = num_adm_pred]
          
          # then save the distribution as before
          distr = data.table(bind_cols(time_of_report = time_pts[i],
                                       num_adm_pred = seq(0, nrow(dist_nya)-1, 1),
                                       probs = dist_nya$probs, cdf = cumsum(dist_nya$probs),
                                       time_window = time_window_,
                                       inc_nya = TRUE,
                                       dist = paste("Empirical poisson")))
          
          # and the point estimates
          pt_estimates_coll_ =
            data.table(bind_cols(time_of_report = time_pts[i],
                                 num_in_ED = nrow(df_), 
                                 # truth is sum of those in ED and those not yet arrived 
                                 # who were admitted within time window
                                  truth = sum(df_$truth) + 
                                   nrow(summ[first_ED_admission > time_pts[i] &
                                                      adm %in% c("direct_adm", "indirect_adm") &
                                                      left_ED <= time_pts[i] + hours(time_window)]),
                                  expected_value = distr[, .SD[which.max(probs)], by = time_of_report][, num_adm_pred],
                                 quantile5 = ifelse(nrow(distr[cdf<0.05]) == 0, min(distr$num_adm_pred), distr[cdf<0.05, max(num_adm_pred)]),
                                 quantile95 = ifelse(nrow(distr[cdf<0.95]) == 0, min(distr$num_adm_pred), distr[cdf<0.95, max(num_adm_pred)]),
                                 time_window = time_window_,
                                 inc_nya = TRUE,
                                 dist = paste("Empirical poisson")))
          
          distr_coll = bind_rows(distr_coll, distr)
          pt_estimates_coll = bind_rows(pt_estimates_coll, pt_estimates_coll_)
          
          # --- 
          # for interest, create a distribution with true numbers of patients in ED
          
          probs_not_yet_arrived = 
            dpois(seq(0, 20 ,1),
                  lambda = poisson_not_yet_arrived[time_window == time_window_ &
                                                     weekend == is_weekend &
                                                     time_of_report == get_report,
                                                   poisson_mean])
          
          true_adm_from_in_ED = sum(df$truth)
          num_adm_true_in_ED = seq(sum(df$truth), 20 + sum(df$truth) ,1)
          # save the distribution 
          distr = data.table(bind_cols(time_of_report = time_pts[i],
                                       num_adm_pred = num_adm_true_in_ED,
                                       probs = probs_not_yet_arrived, cdf = cumsum(probs_not_yet_arrived),
                                       time_window = time_window_,
                                       inc_nya = TRUE,
                                       dist = paste("Empirical poisson for nya + true adm from ED")))   
          
          # derive point estimates from this distribution
          pt_estimates_coll_ = 
            data.table(bind_cols(time_of_report = time_pts[i],
                                 num_in_ED = nrow(df_), 
                                 truth = true_adm_from_in_ED + 
                                   nrow(summ[first_ED_admission > time_pts[i] &
                                               adm %in% c("direct_adm", "indirect_adm") &
                                               left_ED <= time_pts[i] + hours(time_window)]),
                                 expected_value = distr[, .SD[which.max(probs)], by = time_of_report][, num_adm_pred],
                                 quantile5 = ifelse(nrow(distr[cdf<0.05]) == 0, min(distr$num_adm_pred), distr[cdf<0.05, max(num_adm_pred)]),
                                 quantile95 = ifelse(nrow(distr[cdf<0.95]) == 0, min(distr$num_adm_pred), distr[cdf<0.95, max(num_adm_pred)]),
                                 time_window = time_window,
                                 inc_nya = TRUE,
                                 dist = paste("Empirical poisson for nya + true adm from ED")))             
          distr_coll = bind_rows(distr_coll, distr)
          pt_estimates_coll = bind_rows(pt_estimates_coll, pt_estimates_coll_)
          
          # ---- 
          # and second using poisson mean from ngboost predictions
          
          probs_not_yet_arrived = dpois(seq(0, 20 ,1),
                                        lambda = preds_nya_ngboost[time_window == time_window_ & DateTime == DateTime[i], poisson_mean])
          
          dist_nya = data.table()
          
          for (k in 1:length(probs_in_ED)) {
            for (j in 1:length(probs_not_yet_arrived)) {
              
              tot_adm_ = k-1 + j-1
              prob_tot_ = probs_in_ED[k] * probs_not_yet_arrived [j]
              row = data.table(num_adm_pred = tot_adm_, prob_tot = prob_tot_)
              
              dist_nya = bind_rows(dist_nya, row)
            }
          }
          
          dist_nya = dist_nya[, .(probs = sum(prob_tot)), by = num_adm_pred]
          
          distr = data.table(bind_cols(time_of_report = time_pts[i],
                                       num_adm_pred = seq(0, nrow(dist_nya)-1, 1),
                                       probs = dist_nya$probs, cdf = cumsum(dist_nya$probs),
                                       time_window = time_window_,
                                       inc_nya = TRUE,
                                       dist = paste("NGBoost poisson")))

          
          pt_estimates_coll_ = 
            data.table(bind_cols(time_of_report = time_pts[i],
                                 num_in_ED = nrow(df_), 
                                 # truth is sum of those in ED and those not yet arrived
                                  truth = sum(df_$truth) + 
                                   nrow(summ[first_ED_admission > time_pts[i] &
                                                      adm %in% c("direct_adm", "indirect_adm") &
                                                      left_ED <= time_pts[i] + hours(time_window)]),
                                  expected_value = distr[, .SD[which.max(probs)], by = time_of_report][, num_adm_pred],
                                 quantile5 = ifelse(nrow(distr[cdf<0.05]) == 0, min(distr$num_adm_pred), distr[cdf<0.05, max(num_adm_pred)]),
                                 quantile95 = ifelse(nrow(distr[cdf<0.95]) == 0, min(distr$num_adm_pred), distr[cdf<0.95, max(num_adm_pred)]),
                                 time_window = time_window_,
                                 inc_nya = TRUE,
                                 dist = paste("NGBoost poisson")))
          
          distr_coll = bind_rows(distr_coll, distr)
          pt_estimates_coll = bind_rows(pt_estimates_coll, pt_estimates_coll_)
          
          
          
          
        }
      }
    }
    

  }
  
  
  return(list(distr_coll, pt_estimates_coll))
  
}




# Load data ---------------------------------------------------------------


load(paste0("~/EDcrowding/flow-mapping/data-raw/summ_", file_date,".rda"))

summ[, in_set := case_when(first_ED_admission < '2019-11-19 00:00:00' ~ "Train",
                         first_ED_admission < '2019-12-13 00:00:00' ~ "Val",
                         first_ED_admission < '2020-03-19 00:00:00' ~ "Test",
                         first_ED_admission < '2020-12-01 00:00:00' ~ "Train",
                         first_ED_admission < '2020-12-29 00:00:00' ~ "Val",
                         first_ED_admission < '2021-05-01 00:00:00' ~ "Test",
                         TRUE ~ "After")]

summ[, left_ED := coalesce(first_outside_proper_admission, last_inside_discharge)]




# Create set of time points to evaluate over ------------------------------


if (tsk_ids == "Val") {
  if (use_dataset == "Pre") {
    # pre-Covid validation used for Pre only
    start_of_set = with_tz(as.POSIXct('2019-11-19 00:00:00'), tz = "UTC")
    end_of_set = with_tz(as.POSIXct('2019-12-13 00:00:00'), tz = "UTC")
    
    ngboost_8hr <- read_csv("~/EDcrowding/predict-admission/model-output/not_yet_arrived_pre_covid_8hr.csv")
    ngboost_4hr <- read_csv("~/EDcrowding/predict-admission/model-output/not_yet_arrived_pre_covid_4hr.csv")
    
  } else {
    # post-Covid validation used for Post and combined models 
    start_of_set = with_tz(as.POSIXct('2020-12-01 00:00:00'), tz = "UTC")
    end_of_set = with_tz(as.POSIXct('2020-12-29 00:00:00'), tz = "UTC")
    
    ngboost_8hr <- read_csv("~/EDcrowding/predict-admission/model-output/not_yet_arrived_post_covid_8hr.csv")
    ngboost_4hr <- read_csv("~/EDcrowding/predict-admission/model-output/not_yet_arrived_post_covid_4hr.csv")
  }
}

next_dt = start_of_set

time_pts = POSIXct()
while (next_dt < end_of_set) {
  next_pt <- next_dt + c(hours(6), hours(12), hours(16), hours(22))
  time_pts <- c(time_pts, next_pt)
  next_dt = next_dt + days(1)
}


preds_nya_ngboost = data.table(
  bind_rows(ngboost_8hr %>% 
              mutate(time_window = 8, poisson_mean = pred_poisson_mean_8hr) %>% 
               select(-pred_poisson_mean_8hr), 
            ngboost_4hr %>% 
               mutate(time_window = 4, poisson_mean = pred_poisson_mean_4hr) %>% 
               select(-pred_poisson_mean_4hr)))


# Get aggregate predictions ---------------------------------------------------------

# load poisson means for not-yet-arrived
poisson_file = "~/EDcrowding/real-time/data-raw/poisson_not_yet_arrived.rda"
load(poisson_file)

# load probabilities of time to admit
tta_hr_file = "EDcrowding/real-time/data-raw/tta_prob.rda"
load(tta_hr_file)

# individual predictions

preds = make_predictions(time_pts, summ, dm_file_date, model_date,  model_features,  use_dataset)

# # for debugging
# in_ED_all = preds[[1]]
# preds_all_ts = preds[[2]]
# tta_prob = tta_prob[epoch == use_dataset & in_set == "Train"]
# poisson_not_yet_arrived = poisson_not_yet_arrived[epoch == use_dataset & in_set == "Train"]

# aggregate predictions
prob_dist = get_prob_dist(time_pts, in_ED_all = preds[[1]], preds_all_ts = preds[[2]], 
                          tta_prob[epoch == use_dataset & in_set == "Train"], 
                          poisson_not_yet_arrived[epoch == use_dataset & in_set == "Train"], 
                          preds_nya_ngboost)


prob_dist_file = paste0("~/EDcrowding/predict-admission/model-output/model_eval_xgb_", model_features, "_", use_dataset, "_", 
                        tsk_ids, "_", Sys.Date(),".rda")

save(prob_dist, file = prob_dist_file)






