
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

tune_learner <- function(name_tsk, tsk, learner, tsk_train_ids, tuning_round, scores, model_features,
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
  
  score = data.table(
                     timeslice = name_tsk,
                     tsk_ids = "train",
                     model_features = model_features,
                     tuning_round = tuning_round,
                     logloss = rr$aggregate(msr("classif.logloss")),
                     bbrier = rr$aggregate(msr("classif.bbrier")), # binary brier score
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

update_learner <- function(learner, 
                         eval_metric = NA,
                         nrounds = NA,
                         max_depth = NA, 
                         min_child_weight = NA, 
                         gamma = NA,
                         subsample = NA,
                         colsample_bytree = NA,
                         eta = NA, 
                         scale_pos_weight = NA,
                         alpha = NA,
                         lambda = NA,
                         early_stopping_rounds = NA) {
  
  if (!is.na(eval_metric)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("eval_metric" = eval_metric))
  }
  
  if (!is.na(nrounds)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("nrounds" = nrounds))
  }
  
  if (!is.na(max_depth)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("max_depth" = max_depth))
  }
  
  if (!is.na(min_child_weight)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("min_child_weight" = min_child_weight))
  }
  
  if (!is.na(gamma)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("gamma" = gamma))
  }
  
  if (!is.na(subsample)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("subsample" = subsample))
  }
  
  if (!is.na(colsample_bytree)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("colsample_bytree" = colsample_bytree))
  }
  
  if (!is.na(eta)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("eta" = eta))
  }
  
  if (!is.na(scale_pos_weight)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("scale_pos_weight" = scale_pos_weight))
  }
  
  if (!is.na(alpha)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("alpha" = alpha))
  }
  
  if (!is.na(lambda)) {
    learner$param_set$values = insert_named(learner$param_set$values, list("lambda" = lambda))
  }
  
  return(learner)
}

get_preds <- function(name_tsk, tsk, learner, train_or_val_ids, tsk_ids, tuning_round, param_value, preds, model_features) {
  pred_values = learner$predict(tsk, row_ids = train_or_val_ids)
  
  pred <- as.data.table(pred_values)
  pred[, model_features := model_features]
  pred[, timeslice := name_tsk]
  pred[, tsk_ids := tsk_ids]
  pred[, tuning_round := tuning_round]
  pred[, param_value := param_value]
  pred[, dttm := now()]
  
  preds <- bind_rows(preds, pred)
  return(preds)
  
}

get_imps <- function(name_tsk, learner, tsk_ids, tuning_round, param_value, imps, model_features) {
  
  imp <- as.data.table(learner$importance())
  setnames(imp, "V1", "importance")
  imp[, model_features := model_features]
  imp[, feature := names(learner$importance())]
  imp[, timeslice := name_tsk]
  imp[, tsk_ids := tsk_ids]
  imp[, tuning_round := tuning_round]
  imp[, param_value := param_value]
  imp[, dttm := now()]
  
  imps <- bind_rows(imps, imp)
  return(imps)
  
}


save_results <- function(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids, tuning_round, scores, model_features) {
  
  # train learner on training set
  set.seed(17L)
  learner$train(tsk, row_ids = tsk_train_ids)
  
  # get predictions
  pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
  
  score = data.table(
                     timeslice = name_tsk,
                     tsk_ids = tsk_ids,
                     model_features = model_features,
                     tuning_round = tuning_round,
                     logloss = pred_val$score(msr("classif.logloss")),
                     bbrier = pred_val$score(msr("classif.bbrier")), # binary brier score
                     auc = pred_val$score(msr("classif.auc")),
                     acc = pred_val$score(msr("classif.acc")),
                     tp = pred_val$score(msr("classif.tp")),
                     fp = pred_val$score(msr("classif.fp")),
                     fn = pred_val$score(msr("classif.fn")),
                     tn = pred_val$score(msr("classif.tn")),
                     eval_metric = learner$param_set$values$eval_metric,
                     nrounds = learner$param_set$values$nrounds,
                     max_depth = learner$param_set$values$max_depth,
                     min_child_weight = learner$param_set$values$min_child_weight,
                     gamma = learner$param_set$values$gamma, 
                     subsample = learner$param_set$values$subsample,
                     colsample_bytree = learner$param_set$values$colsample_bytree,
                     eta = learner$param_set$values$eta,
                     scale_pos_weight = learner$param_set$values$scale_pos_weight,
                     alpha = learner$param_set$values$alpha,
                     lambda = learner$param_set$values$lambda,
                     dttm = now()
  ) 
  
  scores <- bind_rows(scores, score)
  
  return(scores)
}

# Load saved data ----------------------------------------------

scores_file <- paste0("~/EDcrowding/predict-admission/data-output/xgb_scores_",today(),".rda")

if (file.exists(scores_file)) {
  load(scores_file)
} else {
  scores <- data.table()
}

preds_file <- paste0("~/EDcrowding/predict-admission/data-output/xgb_preds_",today(),".rda")

if (file.exists(preds_file)) {
  load(preds_file)
} else {
  preds <- data.table()
}


imps_file <- paste0("~/EDcrowding/predict-admission/data-output/xgb_imps_",today(),".rda")

if (file.exists(imps_file)) {
  load(imps_file)
} else {
  imps <- data.table()
}


# Set programme instructions ----------------------------------------------

# choose features to include - a - admission features; l = location; o = observation; p = pathology
model_features = "alop"

base_model = FALSE
check_eval_metric =  FALSE
tune_nr = FALSE
tune_spw = FALSE
tune_trees = FALSE
tune_gamma = FALSE
recal_nr = FALSE
tune_samples = FALSE
tune_alpha = TRUE
final_preds = FALSE


# Load data and encode factors --------------------------------------------------------------

#timeslices <- c("000")
timeslices <- c("000", "015", "030", "060",  "090", "120", "180", "240", "300", "360", "480")

file_date <- "2021-03-16"


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
    scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, tuning_round = "base", scores, model_features)
    scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val", tuning_round = "base", scores, model_features)
    
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
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, tuning_round = "eval_metric", eval_metric = eval_metric, scores, model_features)
    }
    
    scores %>% 
      pivot_longer(logloss:tn) %>% filter(name %in% c("auc")) %>% 
      ggplot(aes(x = tsk, y = value, colour = tsk_ids, group = tsk_ids)) + geom_line() + facet_grid(. ~ eval_metric)
    
    #    scores <- save_results(name_tsk, tsk, learner, tsk_val_ids, tuning_round = "base", scores, model_features)
    
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
    
    for(nrounds in c(5, 10, 15, 30, 40, 50, 60)) {

            # get scores on training set using cross-validation
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
                             tuning_round = "nrounds", nrounds = nrounds, 
                             scores, model_features)
      
      # train on full training set and save results on validation set
      scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val", 
                             tuning_round = "nrounds", 
                             scores, model_features)
      
      save(scores, file = scores_file) 
    }
  } 
  
  best_param_val = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & 
                                       tuning_round == "nrounds",
                                     .SD[which.min(logloss)], by = list(timeslice)][,.(nrounds)])
  print ("Best results:")
  scores <- data.table(scores)
  print(scores[tsk_ids == "val" & tuning_round == "nrounds" & model_features == model_features
               , .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, nrounds)])

}


# Tune tree depth and max child weight ------------------------------------


if (tune_trees) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    nrounds = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & 
                                  tuning_round == "nrounds",
                                .SD[which.min(logloss)], by = list(timeslice)][,.(nrounds)])
    
    # tune max_depth
    for(max_depth in c(3, 6, 9)) { # first round

      # get scores on training set using cross-validation
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids,
                             tuning_round = "max_depth",
                             nrounds = nrounds,
                             max_depth = max_depth,
                             scores, model_features)

      # train on full training set and save results on validation set
      scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                             tuning_round = "max_depth", scores, model_features)

      save(scores, file = scores_file)

    }

    # tune min_child_weight
    for(min_child_weight in c(3, 4, 5)) { # first round

      # get scores on training set using cross-validation
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids,
                             nrounds = nrounds,
                             tuning_round = "min_child_weight",
                             min_child_weight = min_child_weight,
                             scores, model_features)

      scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                             tuning_round = "min_child_weight", scores, model_features)

      save(scores, file = scores_file)
    }
    
    
    # get best params
    best_param_val_md = as.numeric(scores[tsk_ids == "train" & timeslice == name_tsk & model_features == model_features & 
                                            tuning_round == "max_depth",
                                       .SD[which.min(logloss)], by = list(timeslice)][,.(max_depth)])
    
    best_param_val_mcw = as.numeric(scores[tsk_ids == "train" & timeslice == name_tsk & model_features == model_features & 
                                             tuning_round == "min_child_weight",
                                          .SD[which.min(logloss)], by = list(timeslice)][,.(min_child_weight)])
    
    # tune in combination
    for(max_depth in c(best_param_val_md-1, best_param_val_md, best_param_val_md+1)) { # second round 
      for (min_child_weight in c(best_param_val_mcw-1, best_param_val_mcw, best_param_val_mcw+1)) {
        # get scores on training set using cross-validation
        scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
                               tuning_round = "tune_trees", 
                               nrounds = nrounds,
                               max_depth = max_depth,
                               min_child_weight = min_child_weight,
                               scores, model_features)
        
        # train on full training set and save results on validation set
        scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                               tuning_round = "tune_trees", scores, model_features)
        
        save(scores, file = scores_file) 
        
      }
    }
  } 
  
  scores[tsk_ids == "val" & tuning_round == "max_depth" & model_features == model_features,
         .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, max_depth)]

  scores[tsk_ids == "val" & tuning_round == "min_child_weight" & model_features == model_features,
         .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, min_child_weight)]

  scores[tsk_ids == "val" & tuning_round == "max_depth" & model_features == model_features] %>%
    pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
    ggplot(aes(x = max_depth, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
    labs(y = "logloss", title = "Results of tuning max_depth of XGBoost for each timeslice - logloss scores")

  scores[tsk_ids == "val" & tuning_round == "min_child_weight" & model_features == model_features] %>%
    pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
    ggplot(aes(x = min_child_weight, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
    labs(y = "logloss", title = "Results of tuning min_child_weight of XGBoost for each timeslice - logloss scores")
  
}


# Tune gamma --------------------------------------------------------------

if (tune_gamma) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    nrounds = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & 
                                  tuning_round == "tune_trees",
                                .SD[which.min(logloss)], by = list(timeslice)][,.(nrounds)])
    
    max_depth = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & 
                                  nrounds == nrounds & tuning_round == "tune_trees",
                                .SD[which.min(logloss)], by = list(timeslice)][,.(max_depth)])
    
    
    min_child_weight = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & 
                                           nrounds == nrounds & tuning_round == "tune_trees",
                                  .SD[which.min(logloss)], by = list(timeslice)][,.(min_child_weight)])
    
    # tune gamma
    for(gamma in c(0, 0.1, 0.2, 0.3, 0.4)) { 

      # get scores on training set using cross-validation
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
                             tuning_round = "gamma", 
                             nrounds = nrounds,
                             max_depth = max_depth, 
                             min_child_weight = min_child_weight, 
                             gamma = gamma, 
                             scores, model_features)
      
      # train on full training set and save results on validation set
      scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                             tuning_round = "gamma", scores, model_features)
    }
    
    save(scores, file = scores_file) # aggregate scores of 10 resamplings

  } 
# 
#   scores[tsk_ids == "val" & tuning_round == "gamma" & model_features == model_features,
#          .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, gamma)]
# 
#   scores[tsk_ids == "train" & tuning_round == "gamma" & model_features == model_features] %>%
#     pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
#     ggplot(aes(x = gamma, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
#     labs(y = "logloss", title = "Results of tuning gamma of XGBoost for each timeslice - logloss scores")

}

# Recalibrate nrounds -----------------------------------------------------


if (recal_nr) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    max_depth = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & 
                                    tuning_round == "gamma",
                                  .SD[which.min(logloss)], by = list(timeslice)][,.(max_depth)])
    
    
    min_child_weight = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & 
                                           tuning_round == "gamma",
                                         .SD[which.min(logloss)], by = list(timeslice)][,.(min_child_weight)])
    
    
    gamma = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & 
                                  tuning_round == "gamma",
                                .SD[which.min(logloss)], by = list(timeslice)][,.(gamma)])
    
    # tune nrounds again
    for(nrounds in c(15, 20, 30, 40, 50, 60)) { 
      
      # get scores on training set using cross-validation
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
                             tuning_round = "recal_nr", 
                             nrounds = nrounds,
                             max_depth = max_depth, 
                             min_child_weight = min_child_weight, 
                             gamma = gamma, 
                             scores, model_features)
      
      
      # train on full training set and save results on validation set
      scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                             tuning_round = "recal_nr", scores, model_features)
    }
    
    save(scores, file = scores_file) 
    
  } 
  
  # scores[tsk_ids == "train" & tuning_round == "recal_nr" & model_features == model_features,
  #        .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, nrounds)]
  # 
  # 
  # scores[tsk_ids == "train" & tuning_round == "recal_nr" & model_features == model_features] %>%
  #   pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
  #   ggplot(aes(x = nrounds, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
  #   labs(y = "logloss", title = "Results of recalibrating nrounds of XGBoost for each timeslice - logloss scores")
  
}


# Tune subsamples and col samples -----------------------------------------

if (tune_samples) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    
    nrounds = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & 
                                    tuning_round == "recal_nr",
                                  .SD[which.min(logloss)], by = list(timeslice)][,.(nrounds)])
    
    max_depth = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & 
                                    tuning_round == "recal_nr",
                                  .SD[which.min(logloss)], by = list(timeslice)][,.(max_depth)])
    
    min_child_weight = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & 
                                           tuning_round == "recal_nr",
                                         .SD[which.min(logloss)], by = list(timeslice)][,.(min_child_weight)])
    
    gamma = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & 
                                tuning_round == "recal_nr",
                              .SD[which.min(logloss)], by = list(timeslice)][,.(gamma)])
    
    # tune subsample 
    for(subsample in c(0.6, 0.7, 0.8, 0.9)) { 
      
      # get scores on training set using cross-validation
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
                             tuning_round = "subsample", 
                             nrounds = nrounds,
                             max_depth = max_depth, 
                             min_child_weight = min_child_weight, 
                             gamma = gamma, 
                             subsample = subsample, 
                             scores, model_features)
    
      # train on full training set and save results on validation set
      scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                             tuning_round = "subsample", scores, model_features)
    }
    
    save(scores, file = scores_file) 
    
    # tune colsample 
    for(colsample_bytree in c(0.6, 0.7, 0.8, 0.9)) { 

      # get scores on training set using cross-validation
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
                             tuning_round = "colsample_bytree", 
                             nrounds = nrounds,
                             max_depth = max_depth, 
                             min_child_weight = min_child_weight, 
                             gamma = gamma, 
                             colsample_bytree = colsample_bytree, 
                             scores, model_features)
      
      
      # train on full training set and save results on validation set
      scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                             tuning_round = "colsample_bytree", scores, model_features)
    }
    
    save(scores, file = scores_file) 
    
    # get best param values
    best_param_val_sub = as.numeric(scores[tsk_ids == "train" & timeslice == name_tsk & model_features == model_features & 
                                         tuning_round == "subsample",
                                       .SD[which.min(logloss)], by = list(timeslice)][,.(subsample)])
    
    best_param_val_col = as.numeric(scores[tsk_ids == "train" & timeslice == name_tsk & model_features == model_features & 
                                             tuning_round == "colsample_bytree",
                                           .SD[which.min(logloss)], by = list(timeslice)][,.(colsample_bytree)])
    
    
    
    # tune in combination
    
    for(subsample in c(best_param_val_sub - .1, best_param_val_sub, best_param_val_sub + .1)) { 
      for (colsample_bytree in c(best_param_val_col - .1, best_param_val_col, best_param_val_col + .1)) {
        
        # get scores on training set using cross-validation
        scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
                               tuning_round = "tune_samples", 
                               nrounds = nrounds,
                               max_depth = max_depth, 
                               min_child_weight = min_child_weight, 
                               gamma = gamma, 
                               subsample = subsample, 
                               colsample_bytree = colsample_bytree,
                               scores, model_features)
        
        # train on full training set and save results on validation set
        scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                               tuning_round = "tune_samples", scores, model_features)
        
      }

    }
    
    save(scores, file = scores_file) 
    
  } 

  scores[tsk_ids == "val" & tuning_round == "tune_samples" & model_features == model_features & subsample != 1,
         .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, subsample, colsample_bytree)]


  scores[tsk_ids == "val" & tuning_round == "subsample" & model_features == model_features] %>%
    pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
    ggplot(aes(x = subsample, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
    labs(y = "logloss", title = "Results of recalibrating subsample of XGBoost for each timeslice - logloss scores")

  scores[tsk_ids == "train" & tuning_round == "colsample_bytree" & model_features == model_features] %>%
    pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
    ggplot(aes(x = colsample_bytree, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
    labs(y = "logloss", title = "Results of recalibrating colsample_bytree of XGBoost for each timeslice - logloss scores")
  
}


# Tune alpha --------------------------------------------------------------

if (tune_alpha) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    params = scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & 
                           tuning_round == "tune_samples",
                         .SD[which.min(logloss)], by = list(timeslice)]
    
    # tune gamma
    # for(alpha in c(.005, 1, 10, 100)) { 
    for(alpha in c(0)) { 
        
      
      # get scores on training set using cross-validation
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
                             tuning_round = "alpha", 
                             nrounds = params$nrounds,
                             max_depth = params$max_depth, 
                             min_child_weight = params$min_child_weight, 
                             gamma = params$gamma, 
                             subsample = params$subsample,
                             colsample_bytree = params$colsample_bytree,
                             alpha = alpha,
                             scores, model_features)
      
      # train on full training set and save results on validation set
      scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                             tuning_round = "alpha", scores, model_features)
    }
    
    save(scores, file = scores_file) # aggregate scores of 10 resamplings
    
  } 

  scores[tsk_ids == "val" & tuning_round == "alpha" & model_features == model_features,
         .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, alpha)]

  scores[tsk_ids == "val" & tuning_round == "alpha" & model_features == model_features & alpha <= 10] %>%
    pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
    ggplot(aes(x = alpha, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
    labs(y = "logloss", title = "Results of tuning alpha of XGBoost for each timeslice - logloss scores")

}

# Looking at improvement on validation set with tuning --------------------

# s = data.table(scores[tsk_ids == "val"  & model_features == model_features]  %>%
#   pivot_longer(logloss) %>% select(timeslice, name, value, tuning_round, dttm))
#   
# 
# s[, .SD[which.max(dttm)], by = list(timeslice, tuning_round)] %>% 
#   ggplot(aes(x = factor(tuning_round, levels = c("base", "nrounds", "tune_trees", "gamma")), y = value)) +
#   geom_point() + facet_grid(. ~ timeslice) + 
#   theme(axis.text.x=element_text(angle=45,hjust=1)) 
#   

# Save preds from final model ---------------------------------------------



if (final_preds) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    params = scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & 
                      tuning_round == "alpha",
                    .SD[which.min(logloss)], by = list(timeslice)]
    
    learner <- update_learner(learner, 
                              nrounds = params$nrounds,
                              max_depth = params$max_depth, 
                              min_child_weight = params$min_child_weight, 
                              gamma = params$gamma,
                              subsample = params$subsample,
                              colsample_bytree = params$colsample_bytree,
                              eta = params$eta, 
                              scale_pos_weight = params$scale_pos_weight,
                              alpha = params$alpha,
                              lambda = params$lambda,
                              early_stopping_rounds = params$early_stopping_rounds)
    
    set.seed(17L)
    
    # train on full training set and save results on validation set
    scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                           tuning_round = "alpha", scores, model_features)

    name_ts <- paste0("dm", ts_)
    dt = get(name_ts)
    
    dt[, row_id := seq_len(nrow(dt))]
    
    # get predictions on validation set
    preds <- get_preds(name_tsk, tsk, learner, train_or_val_ids = dt$row_id, tsk_ids = "all", tuning_round = "final_preds", 
                       param_value = "final_preds",
                       preds, model_features)   
    
    
    save(preds, file = preds_file)
    
    imps <- get_imps(name_tsk, learner, tsk_ids = "all", tuning_round = "final_preds", 
                       param_value = "final_preds",
                       imps, model_features)   
    
    
    save(imps, file = imps_file)
    
  } 
  
  
  
}
