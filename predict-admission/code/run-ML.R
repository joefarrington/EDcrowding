
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



# set date of file to include

file_date <- "2021-05-19"


# choose features to include - a - admission features; l = location; o = observation; p = pathology
model_features = "alop"
use_dataset = "Pre + Post" # combined format is "Pre + Post"

base_model = TRUE
# check_eval_metric =  FALSE # keep eval_metric as log_loss
tune_nr = TRUE
tune_trees = TRUE
# tune_gamma = FALSE # no longer tuning gamma; treat it as zero since in earlier versions tuning showed no variation
recal_nr = FALSE # have now capped nrounds at 30 so will skip this step (24.5.21)
tune_samples = TRUE
# tune_alpha = FALSE # not necessary; we are achieving regularisation in others ways
reduce_lr = FALSE # doesn't make a discernible improvement to metrics, so will skip this step (25.5.21)
final_preds = TRUE # NB - check which prior round final_preds is looking for



# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)

# for mlr3
library(mlr3)
library(mlr3learners)
library(mlr3proba)
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

tune_learner <- function(name_tsk, tsk, learner, tsk_train_ids, tuning_round, scores, model_features, dataset,
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
  rr = resample(tsk, learner, rsmp("cv"), store_models = FALSE)
  
  score = data.table(dataset = dataset,
                     timeslice = name_tsk,
                     tsk_ids = "train",
                     model_features = model_features,
                     tuning_round = tuning_round,
                     logloss = rr$aggregate(msr("classif.logloss")),
                     bbrier = rr$aggregate(msr("classif.bbrier")), # binary brier score
                     prauc = rr$aggregate(msr("classif.prauc")),
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

get_preds <- function(name_tsk, tsk, learner, train_or_val_ids, tsk_ids, tuning_round, param_value, preds, model_features, dataset) {
  pred_values = learner$predict(tsk, row_ids = train_or_val_ids)

  pred <- as.data.table(pred_values)
  pred[, dataset := dataset]
  pred[, model_features := model_features]
  pred[, timeslice := name_tsk]
  pred[, tsk_ids := tsk_ids]
  pred[, tuning_round := tuning_round]
  pred[, param_value := param_value]
  pred[, dttm := now()]

  preds <- bind_rows(preds, pred)
  return(preds)

}

get_imps <- function(name_tsk, learner, tsk_ids, tuning_round, param_value, imps, model_features, dataset) {
  
  imp <- as.data.table(learner$importance())
  setnames(imp, "V1", "importance")
  imp[, dataset := dataset]
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


save_results <- function(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids, tuning_round, scores, model_features, dataset) {
  
  # train learner on training set
  set.seed(17L)
  learner$train(tsk, row_ids = tsk_train_ids)
  
  # get predictions
  pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
  
  score = data.table(dataset = dataset,
                     timeslice = name_tsk,
                     tsk_ids = tsk_ids,
                     model_features = model_features,
                     tuning_round = tuning_round,
                     logloss = pred_val$score(msr("classif.logloss")),
                     bbrier = pred_val$score(msr("classif.bbrier")), # binary brier score
                     prauc = pred_val$score(msr("classif.prauc")),
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

scores_file <- paste0("~/EDcrowding/predict-admission/data-output/xgb_",model_features, "_scores_",today(),".rda")

if (file.exists(scores_file)) {
  load(scores_file)
} else {
  scores <- data.table()
}

# preds_file <- paste0("~/EDcrowding/predict-admission/data-output/xgb_",model_features, "_preds_",today(),".rda")
# 
# if (file.exists(preds_file)) {
#   load(preds_file)
# } else {
#   preds <- data.table()
# }
# 

imps_file <- paste0("~/EDcrowding/predict-admission/data-output/xgb_",model_features, "_", use_dataset, "_imps_",today(),".rda")

if (file.exists(imps_file)) {
  load(imps_file)
} else {
  imps <- data.table()
}



# Load data and encode factors --------------------------------------------------------------

#timeslices <- c("000")
timeslices <- c("000", "015", "030", "060",  "090", "120", "180", "240", "300", "360", "480", "720")



for (ts_ in timeslices) {
  
  # load timeslice 
  inFile = paste0("~/EDcrowding/predict-admission/data-raw/dm", ts_, "_", file_date, ".rda")
  load(inFile)
  
  name_ts <- paste0("dm", ts_)
  dt = get(name_ts)

  #  select dataset (pre or post covid or both if use_dataset is null)
  if (use_dataset != "Pre + Post") {
    
    dt = dt[a_epoch == use_dataset & in_set %in% c("Train", "Val")]
    dt[, a_epoch := NULL]
    
    dt[, row_id := seq_len(nrow(dt))]
    assign(paste0("task", ts_, "_val_ids"), dt[in_set == "Val", row_id])
    assign(paste0("task", ts_, "_train_ids"), dt[in_set == "Train", row_id])
    
  } else {
    
    # in the combined dataset, the pre Covid test set is included as part of the training set
    dt = dt[(a_epoch %in% c("Pre", "Post") & in_set %in% c("Train", "Val")) | (a_epoch == "Pre" & in_set == "Test") ]
    dt[, a_epoch := as.factor(a_epoch)]
    
    dt[, row_id := seq_len(nrow(dt))]
    # in the combined dataset, validation set is post-Covid validation set only, and rest is used for training
    assign(paste0("task", ts_, "_val_ids"), dt[in_set == "Val"  & a_epoch == "Post", row_id])
    assign(paste0("task", ts_, "_train_ids"), dt[!(in_set == "Val" & a_epoch == "Post"), row_id])
  }
  
  
  
  # create vectors identifying test, val and training ids
  # assign(paste0("task", ts_, "_test_ids"), dt[in_set == "Test", row_id])

  
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
  ts <- one_hot(cols = "auto", dt = dt,  dropUnusedLevels=TRUE)
  ts[,adm:=as.factor(adm)] 
  
  
  # remove train-val-test label and row_id so not included in features
  ts[, in_set := NULL]
  
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

# Base model -----------------------------------

if (base_model) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    learner <- train_learner(learner, tsk, tsk_train_ids)
    scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, tuning_round = "base", scores, model_features, use_dataset)
    scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val", 
                           tuning_round = "base", scores, model_features, use_dataset)
    
  } 
  
  save(scores, file = scores_file)
  
}


# # Examine eval_metric -----------------------------------
# 
# if (check_eval_metric) {
#   
#   for (ts_ in timeslices) {
#     name_tsk <- paste0("task", ts_)
#     tsk = get(name_tsk)
#     tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
#     tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
#     
#     for(eval_metric in c("logloss", "auc", "error")) {
#       learner <- train_learner(learner, tsk, tsk_train_ids, eval_metric = eval_metric)
#       scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, tuning_round = "eval_metric", eval_metric = eval_metric, 
#                              scores, model_features, use_dataset)
#     }
#   } 
#   
#   save(scores, file = scores_file)
#   
# }


# Tuning nrounds ----------------------------------------------------------


if (tune_nr) {
  
  for (ts_ in timeslices) {
  # for (ts_ in timeslices[4:12]) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    for(nrounds in c(5, 10, 15, 30)) { # changed on 25.5.21
    # for(nrounds in c(5, 10, 15, 30, 40, 50, 60)) {
        
            # get scores on training set using cross-validation
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
                             tuning_round = "nrounds", nrounds = nrounds, 
                             scores, model_features, use_dataset)
      
      # train on full training set and save results on validation set
      scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val", 
                             tuning_round = "nrounds", 
                             scores, model_features, use_dataset)
      
      save(scores, file = scores_file) 
    }
  }
}


# Tune tree depth and max child weight ------------------------------------


if (tune_trees) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    nrounds = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == use_dataset & 
                                  tuning_round == "nrounds",
                                .SD[which.min(logloss)], by = list(timeslice)][,.(nrounds)])
    
    # after evaluating the size of gains in nrounds, I choose to cap nrounds at 30
    # see report-on-tuning.R for more
    if (nrounds > 30) { nrounds = 30}
    
    # tune max_depth
    for(max_depth in c(2, 5, 8)) { # changed on 25.5.21
      # for(max_depth in c(2, 5, 8, 12, 16, 20)) { # first round
        
      # get scores on training set using cross-validation
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids,
                             tuning_round = "max_depth",
                             nrounds = nrounds,
                             max_depth = max_depth,
                             scores, model_features, use_dataset)

      # train on full training set and save results on validation set
      scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                             tuning_round = "max_depth", scores, model_features, use_dataset)

      save(scores, file = scores_file)

    }

    # # tune min_child_weight
    # for(min_child_weight in c(3, 4, 5)) { # first round
    # 
    #   # get scores on training set using cross-validation
    #   scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids,
    #                          nrounds = nrounds,
    #                          tuning_round = "min_child_weight",
    #                          min_child_weight = min_child_weight,
    #                          scores, model_features, use_dataset)
    # 
    #   scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
    #                          tuning_round = "min_child_weight", scores, model_features, use_dataset)
    # 
    #   save(scores, file = scores_file)
    # }
    # 
    
    # get best params
    best_param_val_md = as.numeric(scores[tsk_ids == "train" & timeslice == name_tsk & model_features == model_features & dataset == use_dataset & 
                                            tuning_round == "max_depth",
                                       .SD[which.min(logloss)], by = list(timeslice)][,.(max_depth)])
    
    best_param_val_mcw = 4 # changed on 21.5.21 
    
    # best_param_val_mcw = as.numeric(scores[tsk_ids == "train" & timeslice == name_tsk & model_features == model_features & dataset == use_dataset & 
    #                                          tuning_round == "min_child_weight",
    #                                       .SD[which.min(logloss)], by = list(timeslice)][,.(min_child_weight)])
    # 
    # tune in combination
    for(max_depth in c(best_param_val_md-1, best_param_val_md, best_param_val_md+1)) { # second round 
      for (min_child_weight in c(best_param_val_mcw-1, best_param_val_mcw, best_param_val_mcw+1)) {
        # get scores on training set using cross-validation
        scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
                               tuning_round = "tune_trees", 
                               nrounds = nrounds,
                               max_depth = max_depth,
                               min_child_weight = min_child_weight,
                               scores, model_features, use_dataset)
        
        # train on full training set and save results on validation set
        scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                               tuning_round = "tune_trees", scores, model_features, use_dataset)
        
        save(scores, file = scores_file) 
        
      }
    }
  } 
  
  # scores[tsk_ids == "val" & tuning_round == "max_depth" & model_features == model_features & dataset == use_dataset,
  #        .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, max_depth)]
  #
  # scores[tsk_ids == "val" & tuning_round == "min_child_weight" & model_features == model_features & dataset == use_dataset,
  #        .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, min_child_weight)]
  #
  # scores[tsk_ids == "val" & tuning_round == "max_depth" & model_features == model_features & dataset == use_dataset] %>%
  #   pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
  #   ggplot(aes(x = max_depth, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
  #   labs(y = "logloss", title = "Results of tuning max_depth of XGBoost for each timeslice - logloss scores")
  #
  # scores[tsk_ids == "val" & tuning_round == "min_child_weight" & model_features == model_features & dataset == use_dataset] %>%
  #   pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
  #   ggplot(aes(x = min_child_weight, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
  #   labs(y = "logloss", title = "Results of tuning min_child_weight of XGBoost for each timeslice - logloss scores")
  #
  # # trying to plot both params on one chart - not obvious how to
  # scores[tsk_ids == "val" & tuning_round == "tune_trees" & model_features == model_features & dataset == use_dataset, .(timeslice, logloss, max_depth, min_child_weight)] %>%
  #   pivot_longer(max_depth:min_child_weight)  %>%
  #   ggplot(aes(x = value, y = logloss, col = name)) + geom_line() + facet_grid(. ~ timeslice) +
  #   labs(y = "logloss", title = "Results of tuning min_child_weight of XGBoost for each timeslice - logloss scores")
  #
  # # looking at log loss to date
  #
  # s = data.table(scores[tsk_ids == "val"  & model_features == model_features & dataset == use_dataset]  %>%
  #                  pivot_longer(logloss) %>% select(timeslice, name, value, tuning_round, dttm))
  #
  #
  # s[, .SD[which.max(dttm)], by = list(timeslice, tuning_round)] %>%
  #   filter(!tuning_round %in% c("colsample_bytree", "max_depth", "min_child_weight", "subsample")) %>%
  #   ggplot(aes(x = factor(tuning_round, levels = c("base", "nrounds", "tune_trees", "gamma", "recal_nr", "tune_samples", "alpha", "reduce_lr")), y = value,
  #              group = "tuning_round")) +
  #   geom_line() + geom_point() + facet_grid(. ~ timeslice) +
  #   theme(axis.text.x=element_text(angle=45,hjust=1)) +
  #   labs(title = "Log loss values after each round of tuning - new approach to train-validation-test split",
  #        x = "Tuning round",
  #        y = "Log loss value")
  #
  # # looking at nrounds to date
  #
  # n = data.table(scores[tsk_ids == "val"  & model_features == model_features & dataset == use_dataset & tuning_round == "nrounds"]  %>%
  #                  pivot_longer(nrounds) %>% select(timeslice, name, value, tuning_round, logloss))
  #
  #
  # n[, .SD[which.min(logloss)], by = list(timeslice)] %>%
  #   ggplot(aes(x = timeslice, y = value)) +
  #   geom_line() + geom_point() +
  #   theme(axis.text.x=element_text(angle=45,hjust=1)) +
  #   labs(title = "Best nround values after nround tuning",
  #        x = "Tuning round",
  #        y = "nrounds")

}


# # Tune gamma --------------------------------------------------------------
# 
# if (tune_gamma) {
#   
#   for (ts_ in timeslices) {
#     name_tsk <- paste0("task", ts_)
#     tsk = get(name_tsk)
#     tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
#     tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
#     
#     nrounds = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == use_dataset & 
#                                   tuning_round == "tune_trees",
#                                 .SD[which.min(logloss)], by = list(timeslice)][,.(nrounds)])
#     
#     max_depth = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == use_dataset & 
#                                   nrounds == nrounds & tuning_round == "tune_trees",
#                                 .SD[which.min(logloss)], by = list(timeslice)][,.(max_depth)])
#     
#     
#     min_child_weight = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == use_dataset & 
#                                            nrounds == nrounds & tuning_round == "tune_trees",
#                                   .SD[which.min(logloss)], by = list(timeslice)][,.(min_child_weight)])
#     
#     # tune gamma
#     for(gamma in c(0, 0.1, 0.2, 0.3, 0.4)) { 
# 
#       # get scores on training set using cross-validation
#       scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
#                              tuning_round = "gamma", 
#                              nrounds = nrounds,
#                              max_depth = max_depth, 
#                              min_child_weight = min_child_weight, 
#                              gamma = gamma, 
#                              scores, model_features, use_dataset)
#       
#       # train on full training set and save results on validation set
#       scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
#                              tuning_round = "gamma", scores, model_features, use_dataset)
#     }
#     
#     save(scores, file = scores_file) # aggregate scores of 10 resamplings
# 
#   } 
# 
#   # scores[tsk_ids == "val" & tuning_round == "gamma" & model_features == model_features & dataset == use_dataset,
#   #        .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, gamma)]
#   # 
#   # scores[tsk_ids == "train" & tuning_round == "gamma" & model_features == model_features & dataset == use_dataset] %>%
#   #   pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
#   #   ggplot(aes(x = gamma, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
#   #   labs(y = "logloss", title = "Results of tuning gamma of XGBoost for each timeslice - logloss scores")
# 
# }

# Recalibrate nrounds -----------------------------------------------------


# if (recal_nr) {
#   
#   for (ts_ in timeslices) {
#     name_tsk <- paste0("task", ts_)
#     tsk = get(name_tsk)
#     tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
#     tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
#     
#     max_depth = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == use_dataset & 
#                                     tuning_round == "tune_trees",
#                                   .SD[which.min(logloss)], by = list(timeslice)][,.(max_depth)])
#     
#     
#     min_child_weight = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == use_dataset & 
#                                            tuning_round == "tune_trees",
#                                          .SD[which.min(logloss)], by = list(timeslice)][,.(min_child_weight)])
#     
#     retry_nr = seq(min(scores[tsk_ids == "val"& model_features == model_features  & dataset == use_dataset &  tuning_round == "tune_trees",
#                                .SD[which.min(logloss)], by = list(timeslice)][,.(nrounds)]), 
#                    max(scores[tsk_ids == "val"& model_features == model_features & dataset == use_dataset &  tuning_round == "tune_trees",
#                                .SD[which.min(logloss)], by = list(timeslice)][,.(nrounds)]), 
#                    10)
#     
#     # tune nrounds again trying all values of nrounds from the minimum to the maximum best in previous rounds
#     for(nrounds in retry_nr) { 
#       
#       # get scores on training set using cross-validation
#       scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
#                              tuning_round = "recal_nr", 
#                              nrounds = nrounds,
#                              max_depth = max_depth, 
#                              min_child_weight = min_child_weight, 
#                              # gamma = gamma, 
#                              scores, model_features, use_dataset)
#       
#       
#       # train on full training set and save results on validation set
#       scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
#                              tuning_round = "recal_nr", scores, model_features, use_dataset)
#     }
#     
#     save(scores, file = scores_file) 
#     
#   } 
#   
#   scores[tsk_ids == "val" & tuning_round %in% c( "recal_nr", "nrounds") & model_features == model_features & dataset == use_dataset,
#          .SD[which.min(logloss)], by = list(timeslice, tuning_round)][,.(timeslice, tuning_round, nrounds, logloss)]
# 
# 
#   scores[tsk_ids == "val" & tuning_round == "recal_nr" & model_features == model_features & dataset == use_dataset] %>%
#     pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
#     ggplot(aes(x = nrounds, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
#     labs(y = "logloss", title = "Results of recalibrating nrounds of XGBoost for each timeslice - logloss scores")
#   
# }


# Tune subsamples and col samples -----------------------------------------

if (tune_samples) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    
    nrounds = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == use_dataset & 
                                    tuning_round == "tune_trees",
                                  .SD[which.min(logloss)], by = list(timeslice)][,.(nrounds)])
    
    max_depth = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == use_dataset & 
                                    tuning_round == "tune_trees",
                                  .SD[which.min(logloss)], by = list(timeslice)][,.(max_depth)])
    
    min_child_weight = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == use_dataset & 
                                           tuning_round == "tune_trees",
                                         .SD[which.min(logloss)], by = list(timeslice)][,.(min_child_weight)])
    # 
    # gamma = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == use_dataset & 
    #                             tuning_round == "recal_nr",
    #                           .SD[which.min(logloss)], by = list(timeslice)][,.(gamma)])
    
    # # tune subsample 
    # for(subsample in c(0.6, 0.7, 0.8, 0.9)) { 
    #   
    #   # get scores on training set using cross-validation
    #   scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
    #                          tuning_round = "subsample", 
    #                          nrounds = nrounds,
    #                          max_depth = max_depth, 
    #                          min_child_weight = min_child_weight, 
    #                          # gamma = gamma, 
    #                          subsample = subsample, 
    #                          scores, model_features, use_dataset)
    # 
    #   # train on full training set and save results on validation set
    #   scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
    #                          tuning_round = "subsample", scores, model_features, use_dataset)
    # }
    # 
    # save(scores, file = scores_file) 
    
    # tune colsample 
    for(colsample_bytree in c(0.6, 0.7, 0.8, 0.9)) { 

      # get scores on training set using cross-validation
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
                             tuning_round = "colsample_bytree", 
                             nrounds = nrounds,
                             max_depth = max_depth, 
                             min_child_weight = min_child_weight, 
                             # gamma = gamma, 
                             colsample_bytree = colsample_bytree, 
                             scores, model_features, use_dataset)
      
      
      # train on full training set and save results on validation set
      scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                             tuning_round = "colsample_bytree", scores, model_features, use_dataset)
    }
    
    save(scores, file = scores_file) 
    
    # get best param values
    
    best_param_val_sub = 0.8 # changed on 25.5.21
    
    # best_param_val_sub = as.numeric(scores[tsk_ids == "train" & timeslice == name_tsk & model_features == model_features & dataset == use_dataset & 
    #                                      tuning_round == "subsample",
    #                                    .SD[which.min(logloss)], by = list(timeslice)][,.(subsample)])
    
    best_param_val_col = as.numeric(scores[tsk_ids == "train" & timeslice == name_tsk & model_features == model_features & dataset == use_dataset & 
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
                               # gamma = gamma, 
                               subsample = subsample, 
                               colsample_bytree = colsample_bytree,
                               scores, model_features, use_dataset)
        
        # train on full training set and save results on validation set
        scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
                               tuning_round = "tune_samples", scores, model_features, use_dataset)
        
      }

    }
    
    save(scores, file = scores_file) 
    
  } 

  # scores[tsk_ids == "val" & tuning_round == "tune_samples" & model_features == model_features & dataset == use_dataset & subsample != 1,
  #        .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, subsample, colsample_bytree)]
  # 
  # 
  # scores[tsk_ids == "val" & tuning_round == "subsample" & model_features == model_features & dataset == use_dataset] %>%
  #   pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
  #   ggplot(aes(x = subsample, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
  #   labs(y = "logloss", title = "Results of recalibrating subsample of XGBoost for each timeslice - logloss scores")
  # 
  # scores[tsk_ids == "train" & tuning_round == "colsample_bytree" & model_features == model_features & dataset == use_dataset] %>%
  #   pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
  #   ggplot(aes(x = colsample_bytree, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
  #   labs(y = "logloss", title = "Results of recalibrating colsample_bytree of XGBoost for each timeslice - logloss scores")
  
}


# # Tune alpha --------------------------------------------------------------
# 
# if (tune_alpha) {
#   
#   for (ts_ in timeslices) {
#     name_tsk <- paste0("task", ts_)
#     tsk = get(name_tsk)
#     tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
#     tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
#     
#     params = scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == use_dataset & 
#                            tuning_round == "tune_samples",
#                          .SD[which.min(logloss)], by = list(timeslice)]
#     
#     # tune gamma
#     # for(alpha in c(.005, 1, 10, 100)) { 
#     for(alpha in c(0, .01, .1, .5, 2, 5)) { 
#         
#       
#       # get scores on training set using cross-validation
#       scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
#                              tuning_round = "alpha", 
#                              nrounds = params$nrounds,
#                              max_depth = params$max_depth, 
#                              min_child_weight = params$min_child_weight, 
#                              gamma = params$gamma, 
#                              subsample = params$subsample,
#                              colsample_bytree = params$colsample_bytree,
#                              alpha = alpha,
#                              scores, model_features, use_dataset)
#       
#       # train on full training set and save results on validation set
#       scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
#                              tuning_round = "alpha", scores, model_features, use_dataset)
#     }
#     
#     save(scores, file = scores_file) # aggregate scores of 10 resamplings
#     
#   } 
# 
#   # scores[tsk_ids == "val" & tuning_round == "alpha" & model_features == model_features & dataset == use_dataset,
#   #        .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, alpha)]
#   # 
#   # scores[tsk_ids == "val" & tuning_round == "alpha" & model_features == model_features & dataset == use_dataset & alpha <= 10] %>%
#   #   pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
#   #   ggplot(aes(x = alpha, y = value)) + geom_line() + facet_grid(. ~ timeslice) +
#   #   labs(y = "logloss", title = "Results of tuning alpha of XGBoost for each timeslice - logloss scores")
# 
# }
# 
# # Looking at improvement on validation set with tuning --------------------
# 
# # s = data.table(scores[tsk_ids == "val"  & model_features == model_features & dataset == use_dataset]  %>%
# #   pivot_longer(logloss) %>% select(timeslice, name, value, tuning_round, dttm))
# #   
# # 
# # s[, .SD[which.max(dttm)], by = list(timeslice, tuning_round)] %>% 
# #   ggplot(aes(x = factor(tuning_round, levels = c("base", "nrounds", "tune_trees", "gamma")), y = value)) +
# #   geom_point() + facet_grid(. ~ timeslice) + 
# #   theme(axis.text.x=element_text(angle=45,hjust=1)) 
# #   
# 


# # Reduce learning rate ----------------------------------------------------
# 
# 
# if (reduce_lr) {
#   
#   for (ts_ in timeslices) {
#     name_tsk <- paste0("task", ts_)
#     tsk = get(name_tsk)
#     tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
#     tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
#     
#     params = scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == use_dataset & 
#                       tuning_round == "tune_samples",
#                     .SD[which.min(logloss)], by = list(timeslice)]
#     
#     for (eta in c(0.3, 0.2, 0.1, 0.05)) {
#       
#       # get scores on training set using cross-validation
#       scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
#                              tuning_round = "reduce_lr", 
#                              nrounds = as.integer(params$nrounds*(.3 / eta)),
#                              max_depth = params$max_depth, 
#                              min_child_weight = params$min_child_weight, 
#                              # gamma = params$gamma, 
#                              subsample = params$subsample,
#                              colsample_bytree = params$colsample_bytree,
#                              # alpha = params$alpha,
#                              eta = eta,
#                              scores, model_features, use_dataset)
#       
#       # train on full training set and save results on validation set
#       scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val",
#                              tuning_round = "reduce_lr", scores, model_features, use_dataset)
#       
#       
#       save(scores, file = scores_file) # aggregate scores of 10 resamplings
#     }
# 
# 
#     
#   } 
#   
#   # scores[tsk_ids == "val" & tuning_round == "reduce_lr" & model_features == model_features & dataset == use_dataset,
#   #        .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, alpha)]
# }

# # Looking at improvement on validation set with tuning --------------------
# 
# s = data.table(scores[tsk_ids == "val"  & model_features == model_features & dataset == use_dataset])
# 
# s1 = s[, .SD[which.min(logloss)], by = list(timeslice, tuning_round)] %>%
#   filter(!tuning_round %in% c("colsample_bytree", "max_depth", "min_child_weight", "subsample")) %>%
#   ggplot(aes(x = factor(tuning_round,
#                         levels = c("base", "nrounds", "tune_trees", "gamma", "recal_nr", "tune_samples", "alpha", "reduce_lr")),
#              y = logloss,
#              group = "tuning_round")) +
#   geom_line() + geom_point() + facet_grid(. ~ timeslice) +
#   theme(axis.text.x=element_text(angle=45,hjust=1)) +
#   labs(title = "XGBoost Log loss values after each round of tuning (scores on validation set)",
#        x = "Tuning round",
#        y = "Log loss value") + theme_grey(base_size = 16) +
#   theme(axis.text.x=element_text(angle=45,hjust=1)) 
# 
# 
# s2 = s[, .SD[which.max(auc)], by = list(timeslice, tuning_round)] %>%
#   filter(!tuning_round %in% c("colsample_bytree", "max_depth", "min_child_weight", "subsample")) %>%
#   ggplot(aes(x = factor(tuning_round,
#                         levels = c("base", "nrounds", "tune_trees", "gamma", "recal_nr", "tune_samples", "alpha", "reduce_lr")),
#              y = auc,
#              group = "tuning_round")) +
#   geom_line() + geom_point() + facet_grid(. ~ timeslice) +
#   theme(axis.text.x=element_text(angle=45,hjust=1)) +
#   labs(title = "XGBoost AUC scores after each round of tuning (scores on validation set)",
#        x = "Tuning round",
#        y = "AUC score") + theme_grey(base_size = 16) +
#   theme(axis.text.x=element_text(angle=45,hjust=1)) 
# 
# library(gridExtra)
# grid.arrange(s1, s2, nrow = 2)
# Save preds from final model ---------------------------------------------



if (final_preds) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))

    params = scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & dataset == use_dataset &
                      tuning_round == "tune_samples",
                    .SD[which.min(logloss)], by = list(timeslice)]

    learner <- update_learner(learner,
                              nrounds = params$nrounds,
                              eval_metric = params$eval_metric,
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
                           tuning_round = "final_preds", scores, model_features, use_dataset)
    
    # # get predictions on validation set
    # preds <- get_preds(name_tsk, tsk, learner, train_or_val_ids = dt$row_id, tsk_ids = "all", tuning_round = "final_preds",
    #                    param_value = "final_preds",
    #                    preds, model_features, use_dataset)
    # 
    # 
    # save(preds, file = preds_file)

    imps <- get_imps(name_tsk, learner, tsk_ids = "all", tuning_round = "final_preds",
                       param_value = "final_preds",
                       imps, model_features, use_dataset)
    save(imps, file = imps_file)

    # save learner data for future prediction

    learner_file  <- paste0("~/EDcrowding/predict-admission/data-output/xgb_",model_features,
                            "_", gsub(" +", "", use_dataset), "_learner_",name_tsk,"_",today(),".rda")
    save(learner, file = learner_file)

    #assign to named data table
    name_tsp <- paste0("dm", ts_, "p")
    ts = get(name_tsp)
    
    features_file <- paste0("~/EDcrowding/predict-admission/data-output/xgb_",model_features,
                            "_", gsub(" +", "", use_dataset), "_features_",name_tsk,"_",today(), ".rda")
    feature_list <- colnames(ts)
    
    save(feature_list, file =features_file)
    
  } 
  
  save(scores, file = scores_file) 
  
}



# Plot importances --------------------------------------------------------

# imps[tsk_ids == "all" & !feature %in% c("a_quarter_1", "a_quarter_2", "a_quarter_3", "a_quarter_4",
#                                                     "a_tod_1", "a_tod_2", "a_tod_3", "a_tod_4", "a_tod_5", "a_tod_6",
#                                         "a_sex_U") &
#        importance > 0.005] %>%
#   ggplot(aes(x = gsub("task","", timeslice), y = reorder(feature, desc(feature)), fill = importance)) + geom_tile() +
#   scale_fill_gradient(low="white", high="red") +
#   labs(title = "Feature importances by timeslice",
#        fill = "Importance",
#        x = "Timeslice",
#        y = "Feature")
# 
# 
# p1 = imps[tsk_ids == "all" & !feature %in% c("a_quarter_1", "a_quarter_2", "a_quarter_3", "a_quarter_4",
#                                         "a_tod_1", "a_tod_2", "a_tod_3", "a_tod_4", "a_tod_5", "a_tod_6",
#                                         "a_sex_U") &
#        timeslice == "task030"  &
#        importance > 0.01] %>%
#   ggplot(aes(x = importance, y = reorder(feature, desc(feature)), fill = importance)) + geom_bar(stat = "identity") +
#   scale_fill_gradient(low="white", high="red") +
#   labs(title = "Feature importances for 30  min timeslice",
#        fill = "Importance",
#        x = "Timeslice",
#        y = "Feature") +
#   theme(legend.position = "bottom")
# 
# 
# p2 = imps[tsk_ids == "all" & !feature %in% c("a_quarter_1", "a_quarter_2", "a_quarter_3", "a_quarter_4",
#                                         "a_tod_1", "a_tod_2", "a_tod_3", "a_tod_4", "a_tod_5", "a_tod_6",
#                                         "a_sex_U") &
#        timeslice == "task120"  &
#        importance > 0.01] %>%
#   ggplot(aes(x = importance, y = reorder(feature, desc(feature)), fill = importance)) + geom_bar(stat = "identity") +
#   scale_fill_gradient(low="white", high="red") +
#   labs(title = "Feature importances for 120  min timeslice",
#        fill = "Importance",
#        x = "Timeslice",
#        y = "Feature") +
#   theme(legend.position = "bottom") +
#   scale_x_continuous(limits = c(0,0.25))
# 
# library(gridExtra)
# grid.arrange(p1, p2,
#              ncol = 2, nrow = 1)
