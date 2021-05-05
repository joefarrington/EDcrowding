
# About this script -------------------------------------------------------

# Timeslice dataset are loaded and encoded into factors
# Each has a ML model trained on it with a binary outcome of admitted or discharged
# The steps are

# - prepare datasets using one-hot encoding, noting which visits should be in train or validation sets
# - train a basic model wihout tuning and score on training and validation sets
# - tune other RF parameters following the logic here:
# - https://towardsdatascience.com/random-forest-hyperparameters-and-how-to-fine-tune-them-17aee785ee0d

# For each step the timeslices are handled in loops which iterate through each timeslice in turn


# Set programme instructions ----------------------------------------------

# set date of file to include

file_date <- "2021-03-29"


# choose features to include - a - admission features; l = location; o = observation; p = pathology
model_features = "alop"
use_dataset = "Post"

base_model = FALSE
tune_num.trees = FALSE
tune_mtry = FALSE
tune_sample.fraction = TRUE
tune_class.weights = FALSE




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
                          num.trees = 500,
                          mtry = NA,
                          class.weights = NA,
                          sample.fraction = NA
) {
  
  learner$param_set$values = insert_named(
    learner$param_set$values, 
    list(
      "num.trees" = num.trees
    )
  )
  
  if (!is.na(mtry)) {
    learner$param_set$values = insert_named(
      learner$param_set$values, list("mtry" = mtry))
  }
  
  if (!is.na(class.weights)) {
    learner$param_set$values = insert_named(
      learner$param_set$values,list("class.weights" = class.weights
      )
    )
  }
  if (!is.na(sample.fraction)) {
    learner$param_set$values = insert_named(
      learner$param_set$values,list("sample.fraction" = sample.fraction
      )
    )
  }
  
  
  # train learner on training set
  set.seed(17L)
  learner$train(tsk, row_ids = tsk_train_ids)
  
  return(learner)
}

tune_learner <- function(name_tsk, tsk, learner, tsk_train_ids, tuning_round, scores, model_features,
                         # initialise params at default values
                         num.trees = 500,
                         num.threads = 8,
                         mtry = NA,
                         class.weights = NA, 
                         sample.fraction = NA
                         ) {
  
  learner$param_set$values = insert_named(
    learner$param_set$values, 
    list(
      "num.trees" = num.trees,
      "num.threads" = num.threads
      
    )
  )
  
  if (!is.na(mtry)) {
    learner$param_set$values = insert_named(
      learner$param_set$values, list("mtry" = mtry))
  }
  
  if (!is.na(class.weights)) {
    learner$param_set$values = insert_named(
      learner$param_set$values,list("class.weights" = class.weights
      )
    )
  }
  if (!is.na(sample.fraction)) {
    learner$param_set$values = insert_named(
      learner$param_set$values,list("sample.fraction" = sample.fraction
      )
    )
  }
  
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
                     num.trees = num.trees,
                     mtry = mtry,
                     class.weights = class.weights,
                     sample.fraction = sample.fraction, 
                     dttm = now()
  )
  
  scores <- bind_rows(scores, score)
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
                     num.trees = learner$param_set$values$num.trees,
                     mtry = learner$param_set$values$mtry,
                     sample.fraction = learner$param_set$values$sample.fraction,
                     dttm = now()
  ) 
  
  scores <- bind_rows(scores, score)
  
  return(scores)
}

# Load saved data ----------------------------------------------

scores_file <- paste0("~/EDcrowding/predict-admission/data-output/rf_scores_",today(),".rda")

if (file.exists(scores_file)) {
  load(scores_file)
} else {
  scores <- data.table()
}

preds_file <- paste0("~/EDcrowding/predict-admission/data-output/rf_preds_",today(),".rda")

if (file.exists(preds_file)) {
  load(preds_file)
} else {
  preds <- data.table()
}


imps_file <- paste0("~/EDcrowding/predict-admission/data-output/rf_imps_",today(),".rda")

if (file.exists(imps_file)) {
  load(imps_file)
} else {
  imps <- data.table()
}



# Load data and encode factors --------------------------------------------------------------

#timeslices <- c("000")
timeslices <- c("000", "015", "030", "060",  "090", "120", "180", "240", "300", "360", "480")



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
  
  dt[, row_id := seq_len(nrow(dt))]
  
  # create vectors identifying test, val and training ids
  assign(paste0("task", ts_, "_test_ids"), dt[in_set == "Test", row_id])
  assign(paste0("task", ts_, "_val_ids"), dt[in_set == "Val", row_id])
  assign(paste0("task", ts_, "_train_ids"), dt[in_set == "Train", row_id])
  
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
learner = lrn("classif.ranger", predict_type = "prob")

# Train without and then with cross validation and check against validation set, no tuning -----------------------------------

if (base_model) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    print(paste0("Processing with RF default paramaters ", name_tsk))
    scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, tuning_round = "base", scores, model_features)
    scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val", tuning_round = "base", scores, model_features)
    
  } 
  
  save(scores, file = scores_file)
  
}




# Tune trees -----------------------------------

if (tune_num.trees) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    print(paste0("Tuning trees ", name_tsk))
    
    for (num.trees in c(10, 50, 100)) {
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, tuning_round = "num.trees",
                             num.trees = num.trees,
                             scores, model_features)
      scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val", tuning_round = "num.trees", 
                             scores, model_features)
    }

    
  } 
  
  save(scores, file = scores_file)
  
  print ("Best results:")
  scores <- data.table(scores)
  print(scores[tsk_ids == "val" & tuning_round == "base" & model_features == model_features
               , .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, num.trees)])

  scores[tsk_ids == "val" & tuning_round == "base" & model_features == model_features] %>%
    pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
    ggplot(aes(x = num.trees, y = value)) + geom_line() + geom_point() + facet_grid(. ~ timeslice) +
    labs(y = "logloss", title = "Results of tuning num.trees of RF for each timeslice - logloss scores")

}
# Tuning mtry ----------------------------------------------------------


if (tune_mtry) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    for(mtry in c(as.integer(sqrt(length(tsk$feature_names))), 
                  as.integer(0.2 * length(tsk$feature_names)), 
                  length(tsk$feature_names))) {

            # get scores on training set using cross-validation
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
                             tuning_round = "mtry", 
                             # setting number of trees to be 100 based on tuning done on 2021-03-30
                             num.trees = 100, 
                             mtry = mtry,
                             scores, model_features)
      
      # train on full training set and save results on validation set
      scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val", 
                             tuning_round = "mtry", 
                             scores, model_features)
      
      save(scores, file = scores_file) 
    }
  } 
  

  print ("Best results:")
  scores <- data.table(scores)
  print(scores[tsk_ids == "val" & tuning_round == "mtry" & model_features == model_features
               , .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, mtry)])

  scores[tsk_ids == "val" & tuning_round == "mtry" & model_features == model_features] %>%
    pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
    ggplot(aes(x = mtry, y = value)) + geom_line() + geom_point() + facet_grid(. ~ timeslice) +
    labs(y = "logloss", title = "Results of tuning nrounds of mtry for each timeslice - logloss scores")


}


# Tune sample fraction
# https://stats.stackexchange.com/questions/400591/what-is-the-underlying-reasoning-behind-sample-fraction-or-nsamp-option-in-range

if (tune_sample.fraction) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    
    mtry = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & 
                                  tuning_round == "mtry",
                                .SD[which.min(logloss)], by = list(timeslice)][,.(mtry)])

    for(sample.fraction in c(0.25, 0.5, .75, 1)) {
      
      # get scores on training set using cross-validation
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
                             tuning_round = "sample.fraction", 
                             # setting number of trees to be 100 based on tuning done on 2021-03-30
                             num.trees = 50, 
                             mtry = mtry,
                             sample.fraction = sample.fraction,
                             scores, model_features)
      
      # train on full training set and save results on validation set
      scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val", 
                             tuning_round = "sample.fraction", 
                             scores, model_features)
      
      save(scores, file = scores_file) 
    }
  } 
  

  print ("Best results:")
  scores <- data.table(scores)
  print(scores[tsk_ids == "val" & tuning_round == "sample.fraction" & model_features == model_features
               , .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, sample.fraction)])

  scores[tsk_ids == "val" & tuning_round == "sample.fraction" & model_features == model_features] %>%
    pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
    ggplot(aes(x = sample.fraction, y = value)) + geom_line() + geom_point() + facet_grid(. ~ timeslice) +
    labs(y = "logloss", title = "Results of tuning nrounds of sample.fraction for each timeslice - logloss scores")

  
}

# Tune class weight -------------------------------------------------------


if (tune_class.weights) {
  
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    
    mtry = as.numeric(scores[tsk_ids == "val" & timeslice == name_tsk & model_features == model_features & 
                               tuning_round == "mtry",
                             .SD[which.min(logloss)], by = list(timeslice)][,.(mtry)])
    
    for(class.weights in c(0.5, 2)) {
      
      # get scores on training set using cross-validation
      scores <- tune_learner(name_tsk, tsk, learner, tsk_train_ids, 
                             tuning_round = "class.weights", 
                             # setting number of trees to be 100 based on tuning done on 2021-03-30
                             num.trees = 50, 
                             mtry = mtry,
                             class.weights = class.weights,
                             scores, model_features)
      
      # train on full training set and save results on validation set
      scores <- save_results(name_tsk, tsk, learner, tsk_train_ids, tsk_val_ids, tsk_ids = "val", 
                             tuning_round = "class.weights", 
                             scores, model_features)
      
      save(scores, file = scores_file) 
    }
  } 
  
    # 
    # print ("Best results:")
    # scores <- data.table(scores)
    # print(scores[tsk_ids == "val" & tuning_round == "class.weights" & model_features == model_features
    #              , .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, mtry)])
    # 
    # scores[tsk_ids == "val" & tuning_round == "mtry" & model_features == model_features] %>%
    #   pivot_longer(logloss:tn) %>% filter(name %in% c("logloss")) %>%
    #   ggplot(aes(x = mtry, y = value)) + geom_line() + geom_point() + facet_grid(. ~ timeslice) +
    #   labs(y = "logloss", title = "Results of tuning nrounds of mtry for each timeslice - logloss scores")
    # 
  
}

