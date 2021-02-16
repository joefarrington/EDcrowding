
# About this script -------------------------------------------------------

# Timeslice dataset are loaded and encoded into factors
# Each has a ML model trained on it with a binary outcome of admitted or discharged
# The steps are

# - prepare datasets using one-hot encoding, noting which visits should be in train or validation sets
# - train a basic model wihout tuning and score on training and validation sets
# - tune scale pos weight for imbalanced samples (this is included but not used)
# - tune number of boosting rounds  
# - tune tree characteristics: max_depth and min_child_weight, once crudely and then more refined a second time
# - tune gamma 
# - recalibrate number of boosting rounds
# - tune samples and colsamples_bytree, once crudely and then more refined a second time

# For each step the timeslices are handled in loops which iterate through each timeslice in turn



# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(data.table)
library(skimr)
library(lubridate)

# for mlr3
library(mlr3)
library(mlr3learners)
library(mlr3proba)

# for shapley plot

library(GGally)
library(precrec)
library(paradox)
library(mlr3tuning)
library(mlr3fselect)
library(mlr3misc)



# Create function ---------------------------------------------------------

# from https://stackoverflow.com/questions/39905820/how-to-one-hot-encode-factor-variables-with-data-table
one_hot <- function(dt, cols="auto", dropCols=TRUE, dropUnusedLevels=FALSE){

  # Automatically get the unordered factor columns
  if(cols[1] == "auto") cols <- colnames(dt)[which(sapply(dt, function(x) is.factor(x) & !is.ordered(x)))]
  
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

# set up parameters
train_learner <- function(learner, tsk, tsk_train_ids, 
                          # initialise params at default values
                          eval_metric = "logloss",
                          nrounds = 1,
                          max_depth = 6, 
                          min_child_weight = 1, 
                          gamma = 0,
                          subsample = 1,
                          colsample_bytree = 1,
                          eta = 0.3, 
                          scale_pos_weight = 1,
                          alpha = 0,
                          lambda = 1,
                          early_stopping_rounds = 10
                          ) {
  
  learner$param_set$values = insert_named(
    learner$param_set$values,
    list(
      "eval_metric" = eval_metric,
      "nrounds" = nrounds,
      "max_depth" = max_depth,
      "min_child_weight" = min_child_weight,
      "gamma" = gamma, 
      "subsample" = subsample,
      "colsample_bytree" = colsample_bytree,
      "eta" = eta,
      "scale_pos_weight" = scale_pos_weight,
      "alpha" = alpha,
      "lambda" = lambda
      
    )
  )
  
  # train learner on training set
  set.seed(17L)
  learner$train(tsk, row_ids = tsk_train_ids)
  
  return(learner)
}

tune_learner <- function(name_tsk, tsk, learner, tsk_val_ids, tuning_round, scores, 
                         # initialise params at default values
                         eval_metric = "logloss",
                         nrounds = 1,
                         max_depth = 6, 
                         min_child_weight = 1, 
                         gamma = 0,
                         subsample = 1,
                         colsample_bytree = 1,
                         eta = 0.3, 
                         scale_pos_weight = 1,
                         alpha = 0,
                         lambda = 1,
                         early_stopping_rounds = 10) {
  
  learner$param_set$values = insert_named(
    learner$param_set$values,
    list(
      "eval_metric" = eval_metric,
      "nrounds" = nrounds,
      "max_depth" = max_depth,
      "min_child_weight" = min_child_weight,
      "gamma" = gamma, 
      "subsample" = subsample,
      "colsample_bytree" = colsample_bytree,
      "eta" = eta,
      "scale_pos_weight" = scale_pos_weight,
      "alpha" = alpha,
      "lambda" = lambda
      
    )
  )
  
  set.seed(17L)
  rr = resample(tsk, learner, rsmp("cv"), store_models = TRUE)
  
  score = data.table(tsk_ids = "train",
                     tsk = name_tsk,
                     tuning_round = tuning_round,
                     logloss = rr$aggregate(msr("classif.logloss")),
                     auc = rr$aggregate(msr("classif.auc")),
                     acc = rr$aggregate(msr("classif.acc")),
                     tp = rr$aggregate(msr("classif.tp")),
                     fp = rr$aggregate(msr("classif.fp")),
                     fn = rr$aggregate(msr("classif.fn")),
                     tn = rr$aggregate(msr("classif.tn")),
                     eval_metric = eval_metric,
                     nrounds = nrounds,
                     max_depth = max_depth,
                     min_child_weight = min_child_weight,
                     gamma = gamma, 
                     subsample = subsample,
                     colsample_bytree = colsample_bytree,
                     eta = eta,
                     scale_pos_weight = scale_pos_weight,
                     alpha = alpha,
                     lambda = lambda,
                     dttm = now()
  )
  
  scores <- bind_rows(scores, score)
}

save_results <- function(name_tsk, tsk, learner, tsk_val_ids, tuning_round, scores, preds,
                         # initialise params at default values
                         eval_metric = "logloss",
                         nrounds = 1,
                         max_depth = 6, 
                         min_child_weight = 1, 
                         gamma = 0,
                         subsample = 1,
                         colsample_bytree = 1,
                         eta = 0.3, 
                         scale_pos_weight = 1,
                         alpha = 0,
                         lambda = 1,
                         early_stopping_rounds = 10) {
  
  learner$param_set$values = insert_named(
    learner$param_set$values,
    list(
      "eval_metric" = eval_metric,
      "nrounds" = nrounds,
      "max_depth" = max_depth,
      "min_child_weight" = min_child_weight,
      "gamma" = gamma, 
      "subsample" = subsample,
      "colsample_bytree" = colsample_bytree,
      "eta" = eta,
      "scale_pos_weight" = scale_pos_weight,
      "alpha" = alpha,
      "lambda" = lambda
    )
  )
  
  # train learner on training set
  set.seed(17L)
  learner$train(tsk, row_ids = tsk_train_ids)
  
  # get predictions
  pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
  
  score = data.table(tsk_ids = "val",
                     tsk = name_tsk,
                     tuning_round = tuning_round,
                     logloss = pred_val$score(msr("classif.logloss")),
                     auc = pred_val$score(msr("classif.auc")),
                     acc = pred_val$score(msr("classif.acc")),
                     tp = pred_val$score(msr("classif.tp")),
                     fp = pred_val$score(msr("classif.fp")),
                     fn = pred_val$score(msr("classif.fn")),
                     tn = pred_val$score(msr("classif.tn")),
                     eval_metric = eval_metric,
                     nrounds = nrounds,
                     max_depth = max_depth,
                     min_child_weight = min_child_weight,
                     gamma = gamma, 
                     subsample = subsample,
                     colsample_bytree = colsample_bytree,
                     eta = eta,
                     scale_pos_weight = scale_pos_weight,
                     alpha = alpha,
                     lambda = lambda,
                     dttm = now()
                    ) 
  
  scores <- bind_rows(scores, score)
  
  pred <- as.data.table(pred_val)
  pred[, model := name_tsk]
  pred[, tuning_round := tuning_round]
  
  preds <- bind_rows(preds, pred)


  return(list(scores, preds))
}

# Set programme instructions ----------------------------------------------

scores_file <- paste0("~/EDcrowding/predict-admission/data-output/scores_",today(),".rda")

if (file.exists(scores_file)) {
  load(scores_file)
} else {
  scores <- data.table()
}

preds_file <- paste0("~/EDcrowding/predict-admission/data-output/preds_",today(),".rda")

if (file.exists(preds_file)) {
  load(preds_file)
} else {
  preds <- data.table()
}




file_date <- "2021-02-08"

base_model = FALSE
check_eval_metric = FALSE
tune_nr = TRUE
tune_spw = FALSE
tune_trees = FALSE
tune_trees2 = FALSE
tune_gamma = FALSE
recal_nr = FALSE
tune_samples = TRUE


# Load data and encode factors --------------------------------------------------------------

timeslices <- c("000", "015", "030", "060", "120", "180", "240", "300", "360")

for (ts_ in timeslices) {
  
  # load timeslice 
  inFile = paste0("~/EDcrowding/predict-admission/data-raw/dm", ts_, "_", file_date, ".rda")
  load(inFile)
  
  name_ts <- paste0("dm", ts_)
  dt = get(name_ts)
  
  dt[, row_id := seq_len(nrow(dt))]
  
  # create vectors identifying test, val and training ids
  assign(paste0("task", ts_, "_test_ids"), dt[in_set == "test", row_id])
  assign(paste0("task", ts_, "_val_ids"), dt[in_set == "val", row_id])
  assign(paste0("task", ts_, "_train_ids"), dt[in_set == "train", row_id])
  
  # remove train-val-test label and row_id so not included in features
  dt[, in_set := NULL]
  dt[, row_id := NULL]
  
  # encode factors
  ts <- one_hot(cols = "auto", dt = as.data.table(dt),  dropUnusedLevels=TRUE)
  ts[,adm:=as.factor(adm)] 
  
  # assign to named data table
  name_tsp <- paste0("dm", ts_, "p")
  assign(name_tsp, ts)
  
}


# Set up ML ------------------------------------------------------------

# create task
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

# create learner
learner = lrn("classif.xgboost", predict_type = "prob")

# Train without and then with cross validation and check against validation set, no tuning -----------------------------------

if (base_model) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    learner <- train_learner(learner, tsk, tsk_train_ids)
    scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, tuning_round = "base", scores)
    scores <- save_results(name_tsk, tsk, learner, tsk_val_ids, tuning_round = "base", scores)
    
  } 
  
  save(scores, file = scores_file)
  
}


# Examine eval_metric -----------------------------------

if (check_eval_metric) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    for(eval_metric in c("logloss", "auc", "error")) {
      learner <- train_learner(learner, tsk, tsk_train_ids, eval_metric = eval_metric)
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, tuning_round = "eval_metric", eval_metric = eval_metric, scores)
    }
    
    scores %>% 
      pivot_longer(logloss:tn) %>% filter(name %in% c("auc")) %>% 
      ggplot(aes(x = tsk, y = value, colour = tsk_ids, group = tsk_ids)) + geom_line() + facet_grid(. ~ eval_metric)
    
#    scores <- save_results(name_tsk, tsk, learner, tsk_val_ids, tuning_round = "base", scores)
    
  } 
  
  save(scores, file = scores_file)
  
}


# Tuning nrounds ----------------------------------------------------------


if (tune_nr) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    # # first round - test wide range - returns 50 as best for all timeslices
    # # for(nrounds in c(1, 50, 100, 200)) {    
    # for(nrounds in c(30, 45, 60, 75)) {
    #   learner <- train_learner(learner, tsk, tsk_train_ids, nrounds = nrounds)
    #   scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, tuning_round = "nrounds", nrounds = nrounds, scores)
    # }
    # 
    # save(scores, file = scores_file)

    scores_pred_list <- save_results(name_tsk, tsk, learner, tsk_val_ids, tuning_round = "nrounds", 
             #             nrounds = as.numeric(scores[tsk_ids == "train" & tsk == name_tsk & tuning_round == "nrounds", .SD[which.min(logloss)], by = list(tsk)][,.(nrounds)]), 
                          nrounds = 30,
                          scores, preds)
    
    scores <- scores_pred_list[[1]]
    preds <- scores_pred_list[[2]]
    preds[, dttm := now()]
    
    save(scores, file = scores_file)
    save(preds, file = preds_file)
  
  } 
  
  # print ("Best results:")
  # setindex(scores, tsk_ids)
  # scores[tsk_ids == "train" & tuning_round == "nrounds", .SD[which.min(logloss)], by = list(tsk)][,.(tsk, nrounds)]
  # 
  # scores_n <- scores[tuning_round == "nrounds"]
  # scores_n[tsk_ids == "train", .SD[which.min(logloss)], by = list(tsk)][,.(tsk, nrounds)]
  # 
  # scores_n[tsk_ids == "train"] %>% 
  #   pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
  #   ggplot(aes(x = nrounds, y = value)) + geom_line() + facet_grid(. ~ tsk) +
  #   labs(y = "logloss", title = "Results of tuningnumber of rounds of XGBoost for each timeslice - logloss scores")
  # 
  # scores_n[tsk_ids == "train"] %>% 
  #   pivot_longer(logloss:tn) %>% filter(name %in% c("auc")) %>%
  #   ggplot(aes(x = nrounds, y = value)) + geom_line() + facet_grid(. ~ tsk) +
  #   labs(y = "auc", title = "Results of tuningnumber of rounds of XGBoost for each timeslice - auc scores")
  
  # looking at model output
  
  s = scores[tuning_round == "nrounds" & tsk_ids == "train"]
  setorder(s, nrounds)
  s[tuning_round == "nrounds" & tsk_ids == "train" & tsk == "task060"] %>% 
    mutate(actual = tp + fn, predicted = tp + fp) %>% 
    pivot_longer(c(tp:fn, actual, predicted)) %>% 
    
    ggplot(aes(x = nrounds, y = value, col = name, group = name)) + geom_line() + geom_point() +
    labs(title = "Scores for 60 minute timeslice over various values of nrounds", 
         col = "score")
  
  
  
  s[tuning_round == "nrounds" & tsk_ids == "train" & tsk == "task060"] %>% 
    mutate(actual = tp + fn, predicted = tp + fp) %>% 
    
    ggplot(aes(x = nrounds)) + 
    geom_line(aes(y = actual), colour = "red", linetype = "dashed") +
    geom_line(aes(y = predicted), colour = "blue", linetype = "dashed") + 
    geom_line(aes(y = tp), colour = "blue") +
    geom_line(aes(y = tp), colour = "blue") 
  
  }
