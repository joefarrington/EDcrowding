
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
  # One-Hot-Encode unordered factors in a data.table
  # If cols = "auto", each unordered factor column in dt will be encoded. (Or specifcy a vector of column names to encode)
  # If dropCols=TRUE, the original factor columns are dropped
  # If dropUnusedLevels = TRUE, unused factor levels are dropped
  
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


# Set programme instructions ----------------------------------------------

if (file.exists("~/EDcrowding/predict-admission/data-output/scores_table.rda")) {
  load("~/EDcrowding/predict-admission/data-output/scores_table.rda")
} else {
  scores_table <- data.table()
}

file_date <- "2021-02-08"

base_model = FALSE
tune_spw = FALSE
tune_nr = FALSE
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


# Check timeslice class balance --------------------------------------------------------
# 
# 
# adm_summ <- data.table()
# 
# for (ts_ in timeslices) {
#   name_ <- paste0("dm", ts_, "p")
#   ts = get(name_)
#   num_adm <- ts[, .N, by = adm]
#   num_adm[, model := ts_]
#   adm_summ <- bind_rows(adm_summ, num_adm)
#   
# }
# 
# # Trying to show proportions on chart - can't think how to do it
# props <- adm_summ %>% pivot_wider(names_from = adm, values_from = N) 
# cols <- c("model", "admitted", "discharged") 
# setnames(props, cols )
# props <- props %>% mutate(prop = admitted/discharged)
# 
# # look at class balance as timeslices progress
# adm_summ %>% ggplot(aes(x = model, y = N, fill = adm)) + geom_bar(stat = "identity") + 
#   labs(title = "Numbers admitted / not admitted in each timeslice", 
#        fill = "Admitted (1 = TRUE)",
#        x = "Timeslice") +
#   theme(legend.position = "bottom") 


# Set up ML ------------------------------------------------------------

# inFile <- paste0("~/EDcrowding/predict-admission/data-raw/dm_", file_date, ".rda")
# load(inFile)

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

# Train without cross validation and check against validation set, no tuning -----------------------------------

if (base_model) {
  
  set.seed(17L)
  for (ts_ in timeslices) {
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))

    learner$param_set$values = insert_named(
      learner$param_set$values,
      list(
        "eval_metric" = "auc"
      )
    )
    
    # train learner on training set
    learner$train(tsk, row_ids = tsk_train_ids)
    
    # score predictions on training and validation set
    pred_train = learner$predict(tsk, row_ids = tsk_train_ids)
    score_train = data.table(train_mcc = pred_train$score(msr("classif.mcc")),
                             train_acc = pred_train$score(msr("classif.acc")),
                             train_auc = pred_train$score(msr("classif.auc")))
    pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
    score_val = data.table(val_mcc = pred_val$score(msr("classif.mcc")),
                           val_acc = pred_val$score(msr("classif.acc")),
                           val_auc = pred_val$score(msr("classif.auc"))) 
    score <- bind_cols(score_train, score_val)
    
    # save parameters with scores
    score$model = name_tsk
    score$tuning_round = "base"
    score$dttm = now()
    
    # save default values
    score$nrounds = learner$param_set$default$nrounds
    score$max_depth = learner$param_set$default$max_depth
    score$min_child_weight = learner$param_set$default$min_child_weight
    score$gamma = learner$param_set$default$gamma
    score$subsample = learner$param_set$default$subsample
    score$colsample_bytree = learner$param_set$default$colsample_bytree
    score$eta = learner$param_set$default$eta 
    score$early_stopping_rounds = NA
    score$scale_pos_weight = learner$param_set$default$scale_pos_weight 
    score$alpha = learner$param_set$default$alpha
    score$lambda = learner$param_set$default$lambda

    
    scores_table <- bind_rows(scores_table, score)
    
  } 
  
  save(scores_table, file = paste0("EDcrowding/predict-admission/data-output/scores_table.rda"))
  
}


# Tuning nrounds ----------------------------------------------------------

if (tune_nr) {
  
  if (file.exists(paste0("EDcrowding/predict-admission/data-output/nr_results_",today(),".rda"))) {
    load(paste0("EDcrowding/predict-admission/data-output/nr_results_",today(),".rda"))
  } else {
    nr_results <- data.table()
  }
  
  
  learner$param_set$values = insert_named(
    learner$param_set$values,
    list(
      "early_stopping_rounds" = 10,
      "nthread" = 8,
      "eval_metric" = "auc"
    )
  )
  
  # set param grid
  tune_ps = ParamSet$new(list(
    #  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("nrounds", lower = 20, upper = 60)
  ))
  
  # # set tuner values
  tuner = tnr("grid_search", resolution = 5)
  
  nr_results <- data.table()
  set.seed(17L)
  
  for (ts_ in timeslices) {
    
    print(paste0("Processing timeslice ", ts_))
    
    name_ts <- paste0("dm", ts_, "p")
    ts  = get(name_ts)
    
    # get class balance for use in scale_pos_weight
    sp <- nrow(ts[adm == 0])/nrow(ts[adm == 1])
    
    learner$param_set$values = mlr3misc::insert_named(
      learner$param_set$values,
      list(
        "scale_pos_weight" = sp
      )
    )
    
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    instance = TuningInstanceSingleCrit$new(
      task = tsk,
      learner = learner$train(tsk, row_ids = tsk_train_ids),
      resampling = rsmp("cv"),
      measure = msr("classif.auc"),
      search_space = tune_ps,
      terminator = trm("evals", n_evals = 20) # alternative is trm("stagnation", iters = 5, threshold = 1e-3)
    )
    tuner$optimize(instance)
    
    # save instance
    name_ins <- paste0("instance", ts_)
    assign(name_ins, instance)
    
    # save results for print
    res <- data.table(instance$archive$data())
    res[, model := name_ins]
    nr_results <- bind_rows(nr_results, res)
    
    # set learner parameters to best results and train with whole of training set
    learner$param_set$values = instance$result_learner_param_vals
    learner$train(tsk, row_ids = tsk_train_ids)
    
    # score predictions on training and validation set
    pred_train = learner$predict(tsk, row_ids = tsk_train_ids)
    score_train = data.table(train_mcc = pred_train$score(msr("classif.mcc")),
                             train_acc = pred_train$score(msr("classif.acc")),
                             train_auc = pred_train$score(msr("classif.auc")))
    pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
    score_val = data.table(val_mcc = pred_val$score(msr("classif.mcc")),
                           val_acc = pred_val$score(msr("classif.acc")),
                           val_auc = pred_val$score(msr("classif.auc"))) 
    score <- bind_cols(score_train, score_val)
    
    # save parameters with scores
    score$model = name_tsk
    score$tuning_round = "nrounds"
    score$scale_pos_weight = instance$result_learner_param_vals$scale_pos_weight
    score$nrounds = instance$result_learner_param_vals$nrounds
    score$early_stopping_rounds = instance$result_learner_param_vals$early_stopping_rounds
    score$dttm = now()
    
    
    # save default values for the rest
    score$max_depth = learner$param_set$default$max_depth
    score$min_child_weight = learner$param_set$default$min_child_weight
    score$gamma = learner$param_set$default$gamma
    score$subsample = learner$param_set$default$subsample
    score$colsample_bytree = learner$param_set$default$colsample_bytree
    score$eta = learner$param_set$default$eta 
    score$alpha = learner$param_set$default$alpha
    score$lambda = learner$param_set$default$lambda
    
    scores_table <- bind_rows(scores_table, score)
    save(scores_table, file = paste0("EDcrowding/predict-admission/data-output/scores_table.rda"))
    
    save(nr_results, file = paste0("EDcrowding/predict-admission/data-output/nr_results_",today(),".rda"))
    
    
  }
  
  # nr_results %>% ggplot(aes(x = nrounds, y = classif.auc, col = model, group = model)) + geom_line() + facet_grid(. ~ model)
  
}



# Tuning scale pos weight ------------------------------------------------------------------


if (tune_spw) {  
  
  spw_range <- vector()
  for (ts_ in timeslices) {
    name_ts <- paste0("dm", ts_, "p")
    ts  = get(name_ts)
    spw_range <- c(spw_range, nrow(ts[adm == 0])/nrow(ts[adm == 1]))
  }
  
  # set default params
  learner$param_set$values = insert_named(
    learner$param_set$values,
    list(
      "early_stopping_rounds" = 10,
      "nthread" = 8,
      "eval_metric" = "auc"
    )
  )
  
  # set param grid
  tune_ps = ParamSet$new(list(
    ParamDbl$new("scale_pos_weight", lower = min(spw_range), upper = max(spw_range))
  ))
  
  # set tuner values
  spw_design = data.table(scale_pos_weight = spw_range)
  tuner = tnr("design_points", design = spw_design)
  
  # iterate through timeslices - run tuning
  spw_results <- data.table()
  
  for (ts_ in timeslices) {
  
    print(paste0("Processing timeslice ", ts_))
  
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
  
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
  
    instance = TuningInstanceSingleCrit$new(
      task = tsk,
      learner = learner$train(tsk, row_ids = tsk_train_ids),
      resampling = rsmp("cv"),
      measure = msr("classif.auc"),
      search_space = tune_ps,
      terminator = trm("evals", n_evals = 20) # alternative is trm("stagnation", iters = 5, threshold = 1e-3)
    )
    tuner$optimize(instance)
  
    # save instance
    name_ins <- paste0("instance", ts_)
#    assign(name_ins, instance)
  
    # save results of all instances
    res <- data.table(instance$archive$data())
    res[, model := name_ins]
    spw_results <- bind_rows(spw_results, res)
    
    # set learner parameters to best results and train with whole of training set
    learner$param_set$values = instance$result_learner_param_vals
    learner$train(tsk, row_ids = tsk_train_ids)
    
    # score predictions on training and validation set
    pred_train = learner$predict(tsk, row_ids = tsk_train_ids)
    score_train = data.table(train_mcc = pred_train$score(msr("classif.mcc")),
                             train_acc = pred_train$score(msr("classif.acc")),
                             train_auc = pred_train$score(msr("classif.auc")))
    pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
    score_val = data.table(val_mcc = pred_val$score(msr("classif.mcc")),
                           val_acc = pred_val$score(msr("classif.acc")),
                           val_auc = pred_val$score(msr("classif.auc"))) 
    score <- bind_cols(score_train, score_val)
    
    # save parameters with scores
    score$model = name_tsk
    score$tuning_round = "spw"
    score$scale_pos_weight = instance$result_learner_param_vals$scale_pos_weight
    score$early_stopping_rounds = instance$result_learner_param_vals$early_stopping_rounds
    score$dttm = now()
    
    # save default values
    score$nrounds = learner$param_set$default$nrounds
    score$max_depth = learner$param_set$default$max_depth
    score$min_child_weight = learner$param_set$default$min_child_weight
    score$gamma = learner$param_set$default$gamma
    score$subsample = learner$param_set$default$subsample
    score$colsample_bytree = learner$param_set$default$colsample_bytree
    score$eta = learner$param_set$default$eta 
    score$alpha = learner$param_set$default$alpha
    score$lambda = learner$param_set$default$lambda

    scores_table <- bind_rows(scores_table, score)
  }
  
  # save results
  save(spw_results, file = paste0("EDcrowding/predict-admission/data-output/spw_results",today(),".rda"))
  save(scores_table, file = paste0("EDcrowding/predict-admission/data-output/scores_table.rda"))
  
  # scores_table[, .SD[which.max(val_auc)], by=.(model)]
}



# Tuning tree parameters ----------------------------------------------------------

if (tune_trees) {

  learner$param_set$values = insert_named(
    learner$param_set$values,
    list(
      "early_stopping_rounds" = 10,
      "nthread" = 8,
      "eval_metric" = "auc"
    )
  )
  
  tune_ps = ParamSet$new(list(
    #  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("max_depth", lower = 3, upper = 9),
    ParamInt$new("min_child_weight", lower = 2, upper = 5)
  ))
  
  tuner = tnr("grid_search", resolution = 3)
  
  trees_results <- data.table()
  set.seed(17L)
  
  for (ts_ in timeslices) {
    
    print(paste0("Processing timeslice ", ts_))
    
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    # get best value for nrounds from previous results
    nr <- as.numeric(scores_table[(model == name_tsk ), .SD[which.max(val_auc)], by=.(model)][,.(nrounds)])
    
    # retrieve best scale_pos_weight (if tuned; if not will return default
    sp <- as.numeric(scores_table[(model == name_tsk ), .SD[which.max(val_auc)], by=.(model)][,.(scale_pos_weight)])
    
    learner$param_set$values = mlr3misc::insert_named(
      learner$param_set$values,
      list(
        "scale_pos_weight" = sp,
        "nrounds" = nr
      )
    )

    instance = TuningInstanceSingleCrit$new(
      task = tsk,
      learner = learner$train(tsk, row_ids = tsk_train_ids),
      resampling = rsmp("cv"),
      measure = msr("classif.auc"),
      search_space = tune_ps,
      terminator = trm("evals", n_evals = 20) # alternative is trm("stagnation", iters = 5, threshold = 1e-3)
    )
    tuner$optimize(instance)
    
    # save instance
    name_ins <- paste0("instance", ts_)
    assign(name_ins, instance)
    
    # save results for print
    res <- data.table(instance$archive$data())
    res[, model := name_ins]
    trees_results <- bind_rows(trees_results, res)
    
    # set learner parameters to best results and train with whole of training set
    learner$param_set$values = instance$result_learner_param_vals
    learner$train(tsk, row_ids = tsk_train_ids)
    
    # score predictions on training and validation set
    pred_train = learner$predict(tsk, row_ids = tsk_train_ids)
    score_train = data.table(train_mcc = pred_train$score(msr("classif.mcc")),
                             train_acc = pred_train$score(msr("classif.acc")),
                             train_auc = pred_train$score(msr("classif.auc")))
    pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
    score_val = data.table(val_mcc = pred_val$score(msr("classif.mcc")),
                           val_acc = pred_val$score(msr("classif.acc")),
                           val_auc = pred_val$score(msr("classif.auc"))) 
    score <- bind_cols(score_train, score_val)
    
    # save parameters with scores
    score$model = name_tsk
    score$tuning_round = "trees"
    score$scale_pos_weight = instance$result_learner_param_vals$scale_pos_weight
    score$nrounds = instance$result_learner_param_vals$nrounds
    score$max_depth = instance$result_learner_param_vals$max_depth
    score$min_child_weight = instance$result_learner_param_vals$min_child_weight
    score$early_stopping_rounds = instance$result_learner_param_vals$early_stopping_rounds
    score$dttm = now()
    
    
    # save default values for the rest
    score$gamma = learner$param_set$default$gamma
    score$subsample = learner$param_set$default$subsample
    score$colsample_bytree = learner$param_set$default$colsample_bytree
    score$eta = learner$param_set$default$eta 
    score$alpha = learner$param_set$default$alpha
    score$lambda = learner$param_set$default$lambda
    
    scores_table <- bind_rows(scores_table, score)
    save(scores_table, file = paste0("EDcrowding/predict-admission/data-output/scores_table.rda"))
    
    save(trees_results, file = paste0("EDcrowding/predict-admission/data-output/trees_results_",today(),".rda"))
    
    
  }
  # trees_results %>% pivot_longer(max_depth: min_child_weight) %>%
  #   group_by(model, name, value) %>% summarise(mean_auc = mean(classif.auc)) %>%
  #   ggplot(aes(x = value, y = mean_auc, col = model)) + geom_line() + facet_grid(. ~ name)
}


# Tuning tree parameters - zooming in  ----------------------------------------------------------

if (tune_trees2) {
  
  if (file.exists(paste0("~/EDcrowding/predict-admission/data-output/trees_results_", today(), ".rda"))) {
    load(paste0("~/EDcrowding/predict-admission/data-output/trees_results_", today(), ".rda"))
  }
  else   {
    trees_results <- data.table()
  }
  
  learner$param_set$values = mlr3misc::insert_named(
    learner$param_set$values,
    list(
      "early_stopping_rounds" = 10,
      "nthread" = 8,
      "eval_metric" = "auc"
    )
  )
  
  tune_ps = ParamSet$new(list(
    #  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("max_depth", lower = 2, upper = 10),
    ParamInt$new("min_child_weight", lower = 1, upper = 6)
  ))
  
  set.seed(17L)
  
  for (ts_ in timeslices) {
    
    print(paste0("Processing timeslice ", ts_))
    
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    # get best value for nrounds from previous results
    nr <- as.numeric(scores_table[(model == name_tsk ), .SD[which.max(val_auc)], by=.(model)][,.(nrounds)])
    
    # retrieve best scale_pos_weight (if tuned; if not will return default
    sp <- as.numeric(scores_table[(model == name_tsk ), .SD[which.max(val_auc)], by=.(model)][,.(scale_pos_weight)])
    
    learner$param_set$values = mlr3misc::insert_named(
      learner$param_set$values,
      list(
        "scale_pos_weight" = sp,
        "nrounds" = nr
      )
    )
    
    # retrieve max_depth and min_child_weight from previous round for tuner values
    
    md <- as.numeric(scores_table[(model == name_tsk & tuning_round == "trees"), .SD[which.max(val_auc)], by=.(model)][,.(max_depth)])
    mcw <- as.numeric(scores_table[(model == name_tsk & tuning_round == "trees"), .SD[which.max(val_auc)], by=.(model)][,.(min_child_weight)])
    
    tree_design = data.table(max_depth = c(md-1, md-1, md+1, md+1), min_child_weight = c(mcw+1, mcw-1, mcw+1, mcw-1))
    tuner = tnr("design_points", design = tree_design)

    instance = TuningInstanceSingleCrit$new(
      task = tsk,
      learner = learner$train(tsk, row_ids = tsk_train_ids),
      resampling = rsmp("cv"),
      measure = msr("classif.auc"),
      search_space = tune_ps,
      terminator = trm("evals", n_evals = 20) # alternative is trm("stagnation", iters = 5, threshold = 1e-3)
    )
    tuner$optimize(instance)
    
    # save instance
    name_ins <- paste0("instance", ts_)
#    assign(name_ins, instance)
    
    # save results for print
    res <- data.table(instance$archive$data())
    res[, model := name_ins]
    trees_results <- bind_rows(trees_results, res)
    
    # set learner parameters to best results and train with whole of training set
    learner$param_set$values = instance$result_learner_param_vals
    learner$train(tsk, row_ids = tsk_train_ids)
    
    # score predictions on training and validation set
    pred_train = learner$predict(tsk, row_ids = tsk_train_ids)
    score_train = data.table(train_mcc = pred_train$score(msr("classif.mcc")),
                             train_acc = pred_train$score(msr("classif.acc")),
                             train_auc = pred_train$score(msr("classif.auc")))
    pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
    score_val = data.table(val_mcc = pred_val$score(msr("classif.mcc")),
                           val_acc = pred_val$score(msr("classif.acc")),
                           val_auc = pred_val$score(msr("classif.auc"))) 
    score <- bind_cols(score_train, score_val)
    
    # save parameters with scores
    score$model = name_tsk
    score$tuning_round = "trees2"
    score$scale_pos_weight = instance$result_learner_param_vals$scale_pos_weight
    score$nrounds = instance$result_learner_param_vals$nrounds
    score$max_depth = instance$result_learner_param_vals$max_depth
    score$min_child_weight = instance$result_learner_param_vals$min_child_weight
    score$early_stopping_rounds = instance$result_learner_param_vals$early_stopping_rounds
    score$dttm = now()
    
    
    # save default values for the rest
    score$gamma = learner$param_set$default$gamma
    score$subsample = learner$param_set$default$subsample
    score$colsample_bytree = learner$param_set$default$colsample_bytree
    score$eta = learner$param_set$default$eta 
    score$alpha = learner$param_set$default$alpha
    score$lambda = learner$param_set$default$lambda
    
    scores_table <- bind_rows(scores_table, score)
    save(scores_table, file = paste0("EDcrowding/predict-admission/data-output/scores_table.rda"))
    
    save(trees_results, file = paste0("EDcrowding/predict-admission/data-output/trees_results_",today(),".rda"))
    
    
  }
  # trees_results %>% pivot_longer(max_depth: min_child_weight) %>%
  #   group_by(model, name, value) %>% summarise(mean_auc = mean(classif.auc)) %>%
  #   ggplot(aes(x = value, y = mean_auc, col = model)) + geom_line() + facet_grid(. ~ name)
}



# Tuning gamma ------------------------------------------------------------


if (tune_gamma) {
  
  if (file.exists(paste0("~/EDcrowding/predict-admission/data-output/gamma_results_", today(), ".rda"))) {
    load(paste0("~/EDcrowding/predict-admission/data-output/gamma_results_", today(), ".rda"))
  }   else   {
    gamma_results <- data.table()
  }
  
  learner$param_set$values = mlr3misc::insert_named(
    learner$param_set$values,
    list(
      "early_stopping_rounds" = 10,
      "nthread" = 8,
      "eval_metric" = "auc"
    )
  )
  
  tune_ps = ParamSet$new(list(
    #  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamDbl$new("gamma", lower = 0, upper = 0.4)
  ))
  
  tuner = tnr("grid_search", resolution = 5)
  
  set.seed(17L)
  
  for (ts_ in timeslices) {
    
    print(paste0("Processing timeslice ", ts_))
    
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    # get best value for nrounds from previous results
    nr <- as.numeric(scores_table[(model == name_tsk ), .SD[which.max(val_auc)], by=.(model)][,.(nrounds)])
    
    # retrieve best scale_pos_weight (if tuned; if not will return default
    sp <- as.numeric(scores_table[(model == name_tsk ), .SD[which.max(val_auc)], by=.(model)][,.(scale_pos_weight)])
    
    # retrieve max_depth and min_child_weight
    
    md <- as.numeric(scores_table[(model == name_tsk & tuning_round == "trees2"), .SD[which.max(val_auc)], by=.(model)][,.(max_depth)])
    mcw <- as.numeric(scores_table[(model == name_tsk & tuning_round == "trees2"), .SD[which.max(val_auc)], by=.(model)][,.(min_child_weight)])
    
    
    learner$param_set$values = mlr3misc::insert_named(
      learner$param_set$values,
      list(
        "scale_pos_weight" = sp,
        "nrounds" = nr,
        "max_depth" = md, 
        "min_child_weight" = mcw
      )
    )
    
    instance = TuningInstanceSingleCrit$new(
      task = tsk,
      learner = learner$train(tsk, row_ids = tsk_train_ids),
      resampling = rsmp("cv"),
      measure = msr("classif.auc"),
      search_space = tune_ps,
      terminator = trm("evals", n_evals = 20) # alternative is trm("stagnation", iters = 5, threshold = 1e-3)
    )
    tuner$optimize(instance)
    
    # save instance
    name_ins <- paste0("instance", ts_)
    #    assign(name_ins, instance)
    
    # save results for print
    res <- data.table(instance$archive$data())
    res[, model := name_ins]
    gamma_results <- bind_rows(gamma_results, res)
    
    # set learner parameters to best results and train with whole of training set
    learner$param_set$values = instance$result_learner_param_vals
    learner$train(tsk, row_ids = tsk_train_ids)
    
    # score predictions on training and validation set
    pred_train = learner$predict(tsk, row_ids = tsk_train_ids)
    score_train = data.table(train_mcc = pred_train$score(msr("classif.mcc")),
                             train_acc = pred_train$score(msr("classif.acc")),
                             train_auc = pred_train$score(msr("classif.auc")))
    pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
    score_val = data.table(val_mcc = pred_val$score(msr("classif.mcc")),
                           val_acc = pred_val$score(msr("classif.acc")),
                           val_auc = pred_val$score(msr("classif.auc"))) 
    score <- bind_cols(score_train, score_val)
    
    # save parameters with scores
    score$model = name_tsk
    score$tuning_round = "gamma"
    score$scale_pos_weight = instance$result_learner_param_vals$scale_pos_weight
    score$nrounds = instance$result_learner_param_vals$nrounds
    score$max_depth = instance$result_learner_param_vals$max_depth
    score$min_child_weight = instance$result_learner_param_vals$min_child_weight
    score$min_child_weight = instance$result_learner_param_vals$min_child_weight  
    score$gamma = instance$result_learner_param_vals$gamma
    score$early_stopping_rounds = instance$result_learner_param_vals$early_stopping_rounds
    score$dttm = now()
    
    
    # save default values for the rest
    score$subsample = learner$param_set$default$subsample
    score$colsample_bytree = learner$param_set$default$colsample_bytree
    score$eta = learner$param_set$default$eta 
    score$alpha = learner$param_set$default$alpha
    score$lambda = learner$param_set$default$lambda
    
    scores_table <- bind_rows(scores_table, score)
    save(scores_table, file = paste0("EDcrowding/predict-admission/data-output/scores_table.rda"))
    
    save(gamma_results, file = paste0("EDcrowding/predict-admission/data-output/gamma_results_",today(),".rda"))
    
    
  }
  # 
  # scores_table[tuning_round == "gamma"]
  # 
  # gamma_results %>% 
  #   ggplot(aes(x = gamma, y = classif.auc, col = model)) + geom_line() + facet_grid(. ~ model)
  # 
  # gamma_results %>% group_by(model) %>% summarise(g_mean = mean(classif.auc), 
  #                                                 g_sd = sd(classif.auc))
  #   ggplot(aes(x = model, y = classif.auc, col = model)) + geom_line() + facet_grid(. ~ model)
  # 
  
}


# Recalibrating nrounds ---------------------------------------------------


if (recal_nr) {
  
  if (file.exists(paste0("~/EDcrowding/predict-admission/data-output/nr_results_", today(), ".rda"))) {
    load(paste0("~/EDcrowding/predict-admission/data-output/nr_results_", today(), ".rda"))
  }   else   {
    nr_results <- data.table()
  }
  
  learner$param_set$values = mlr3misc::insert_named(
    learner$param_set$values,
    list(
      "early_stopping_rounds" = 10,
      "nthread" = 8,
      "eval_metric" = "auc"
    )
  )
  
  # set param grid
  tune_ps = ParamSet$new(list(
    #  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("nrounds", lower = 25, upper = 55)
  ))
  
  # # set tuner values
  tuner = tnr("grid_search", resolution = 4)
  
  set.seed(17L)
  
  for (ts_ in timeslices) {
    
    print(paste0("Processing timeslice ", ts_))
    
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    # retrieve best scale_pos_weight (if tuned; if not will return default
    sp <- as.numeric(scores_table[(model == name_tsk ), .SD[which.max(val_auc)], by=.(model)][,.(scale_pos_weight)])
    
    # retrieve max_depth and min_child_weight
    md <- as.numeric(scores_table[(model == name_tsk & tuning_round == "trees2"), .SD[which.max(val_auc)], by=.(model)][,.(max_depth)])
    mcw <- as.numeric(scores_table[(model == name_tsk & tuning_round == "trees2"), .SD[which.max(val_auc)], by=.(model)][,.(min_child_weight)])
    
    # # retrieve gamma
    # gam <- as.numeric(scores_table[(tuning_round == "gamma"), .SD[which.max(val_auc)], by=.(model)][,.(gamma)])
    # 
    
    learner$param_set$values = mlr3misc::insert_named(
      learner$param_set$values,
      list(
        "scale_pos_weight" = sp,
        "max_depth" = md, 
        "min_child_weight" = mcw,
        "gamma" = 0.1
      )
    )
    
    instance = TuningInstanceSingleCrit$new(
      task = tsk,
      learner = learner$train(tsk, row_ids = tsk_train_ids),
      resampling = rsmp("cv"),
      measure = msr("classif.auc"),
      search_space = tune_ps,
      terminator = trm("evals", n_evals = 20) # alternative is trm("stagnation", iters = 5, threshold = 1e-3)
    )
    tuner$optimize(instance)
    
    # save instance
    name_ins <- paste0("instance", ts_)
    #    assign(name_ins, instance)
    
    # save results for print
    res <- data.table(instance$archive$data())
    res[, model := name_ins]
    nr_results <- bind_rows(nr_results, res)
    
    # set learner parameters to best results and train with whole of training set
    learner$param_set$values = instance$result_learner_param_vals
    learner$train(tsk, row_ids = tsk_train_ids)
    
    # score predictions on training and validation set
    pred_train = learner$predict(tsk, row_ids = tsk_train_ids)
    score_train = data.table(train_mcc = pred_train$score(msr("classif.mcc")),
                             train_acc = pred_train$score(msr("classif.acc")),
                             train_auc = pred_train$score(msr("classif.auc")))
    pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
    score_val = data.table(val_mcc = pred_val$score(msr("classif.mcc")),
                           val_acc = pred_val$score(msr("classif.acc")),
                           val_auc = pred_val$score(msr("classif.auc"))) 
    score <- bind_cols(score_train, score_val)
    
    # save parameters with scores
    score$model = name_tsk
    score$tuning_round = "recal_nr"
    score$scale_pos_weight = instance$result_learner_param_vals$scale_pos_weight
    score$nrounds = instance$result_learner_param_vals$nrounds
    score$max_depth = instance$result_learner_param_vals$max_depth
    score$min_child_weight = instance$result_learner_param_vals$min_child_weight
    score$min_child_weight = instance$result_learner_param_vals$min_child_weight  
    score$gamma = instance$result_learner_param_vals$gamma
    score$early_stopping_rounds = instance$result_learner_param_vals$early_stopping_rounds
    score$dttm = now()
    
    
    # save default values for the rest
    score$subsample = learner$param_set$default$subsample
    score$colsample_bytree = learner$param_set$default$colsample_bytree
    score$eta = learner$param_set$default$eta 
    score$alpha = learner$param_set$default$alpha
    score$lambda = learner$param_set$default$lambda
    
    scores_table <- bind_rows(scores_table, score)
    save(scores_table, file = paste0("EDcrowding/predict-admission/data-output/scores_table.rda"))
    
    save(nr_results, file = paste0("EDcrowding/predict-admission/data-output/nr_results_",today(),".rda"))
    
    
  }
  
  # nr_results %>% filter(timestamp > '2021-02-10 12:48:01') %>% 
  #   ggplot(aes(x = nrounds, y = classif.auc, col = model, group = model)) + geom_line() + facet_grid(. ~ model)
  # 
}


# Turning sample and colsample proportions  ---------------------------------------------------


if (tune_samples) {
  
  if (file.exists(paste0("~/EDcrowding/predict-admission/data-output/samples_results_", today(), ".rda"))) {
    load(paste0("~/EDcrowding/predict-admission/data-output/samples_results_", today(), ".rda"))
  }   else   {
    samples_results <- data.table()
  }
  
  learner$param_set$values = mlr3misc::insert_named(
    learner$param_set$values,
    list(
      "early_stopping_rounds" = 10,
      "nthread" = 8,
      "eval_metric" = "auc"
    )
  )
  
  # set param grid
  tune_ps = ParamSet$new(list(
    #  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamDbl$new("subsample", lower = 0.6, upper = 0.9),
    ParamDbl$new("colsample_bytree", lower = 0.6, upper = 0.9)
  ))
  
  # # set tuner values
  tuner = tnr("grid_search", resolution = 4)
  
  set.seed(17L)
  
  for (ts_ in timeslices) {
    
    print(paste0("Processing timeslice ", ts_))
    
    name_tsk <- paste0("task", ts_)
    tsk = get(name_tsk)
    
    tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
    tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
    
    # get best value for nrounds from recalibration
    nr <- as.numeric(scores_table[(model == name_tsk & tuning_round == "recal_nr"), .SD[which.max(val_auc)], by=.(model)][,.(nrounds)])
    
    # retrieve best scale_pos_weight (if tuned; if not will return default
    sp <- as.numeric(scores_table[(model == name_tsk ), .SD[which.max(val_auc)], by=.(model)][,.(scale_pos_weight)])
    
    # retrieve max_depth and min_child_weight
    md <- as.numeric(scores_table[(model == name_tsk & tuning_round == "trees2"), .SD[which.max(val_auc)], by=.(model)][,.(max_depth)])
    mcw <- as.numeric(scores_table[(model == name_tsk & tuning_round == "trees2"), .SD[which.max(val_auc)], by=.(model)][,.(min_child_weight)])
    
    # # retrieve gamma
    # gam <- as.numeric(scores_table[(tuning_round == "gamma"), .SD[which.max(val_auc)], by=.(model)][,.(gamma)])
    # 
    
    learner$param_set$values = mlr3misc::insert_named(
      learner$param_set$values,
      list(
        "scale_pos_weight" = sp,
        "nrounds" = nr,
        "max_depth" = md, 
        "min_child_weight" = mcw,
        "gamma" = 0.1
      )
    )
    
    instance = TuningInstanceSingleCrit$new(
      task = tsk,
      learner = learner$train(tsk, row_ids = tsk_train_ids),
      resampling = rsmp("cv"),
      measure = msr("classif.auc"),
      search_space = tune_ps,
      terminator = trm("evals", n_evals = 20) # alternative is trm("stagnation", iters = 5, threshold = 1e-3)
    )
    tuner$optimize(instance)
    
    # save instance
    name_ins <- paste0("instance", ts_)
    #    assign(name_ins, instance)
    
    # save results for print
    res <- data.table(instance$archive$data())
    res[, model := name_ins]
    samples_results <- bind_rows(samples_results, res)
    
    # set learner parameters to best results and train with whole of training set
    learner$param_set$values = instance$result_learner_param_vals
    learner$train(tsk, row_ids = tsk_train_ids)
    
    # score predictions on training and validation set
    pred_train = learner$predict(tsk, row_ids = tsk_train_ids)
    score_train = data.table(train_mcc = pred_train$score(msr("classif.mcc")),
                             train_acc = pred_train$score(msr("classif.acc")),
                             train_auc = pred_train$score(msr("classif.auc")))
    pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
    score_val = data.table(val_mcc = pred_val$score(msr("classif.mcc")),
                           val_acc = pred_val$score(msr("classif.acc")),
                           val_auc = pred_val$score(msr("classif.auc"))) 
    score <- bind_cols(score_train, score_val)
    
    # save parameters with scores
    score$model = name_tsk
    score$tuning_round = "samples"
    score$scale_pos_weight = instance$result_learner_param_vals$scale_pos_weight
    score$nrounds = instance$result_learner_param_vals$nrounds
    score$max_depth = instance$result_learner_param_vals$max_depth
    score$min_child_weight = instance$result_learner_param_vals$min_child_weight
    score$min_child_weight = instance$result_learner_param_vals$min_child_weight  
    score$gamma = instance$result_learner_param_vals$gamma
    score$early_stopping_rounds = instance$result_learner_param_vals$early_stopping_rounds
    score$subsample =  instance$result_learner_param_vals$subsample
    score$colsample_bytree =  instance$result_learner_param_vals$colsample_bytree
    score$dttm = now()
    
    
    # save default values for the rest
    score$eta = learner$param_set$default$eta 
    score$alpha = learner$param_set$default$alpha
    score$lambda = learner$param_set$default$lambda
    
    scores_table <- bind_rows(scores_table, score)
    save(scores_table, file = paste0("EDcrowding/predict-admission/data-output/scores_table.rda"))
    
    save(samples_results, file = paste0("EDcrowding/predict-admission/data-output/samples_results_",today(),".rda"))
    
    
  }

  scores_table[tuning_round == "samples"]
  
  samples_results %>% pivot_longer(subsample: colsample_bytree) %>%
    group_by(model, name, value) %>% summarise(mean_auc = mean(classif.auc)) %>%
    ggplot(aes(x = value, y = mean_auc, col = name)) + geom_line() + facet_grid(. ~ model)


}
