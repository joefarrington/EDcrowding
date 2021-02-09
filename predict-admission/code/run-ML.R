



# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(data.table)
library(tidymodels)
library(skimr)
library(lubridate)

# for mlr3
library(mlr3)
library(mlr3learners)
library(mlr3proba)
library(mlr3viz)
library(SHAPforxgboost)

library(GGally)
library(precrec)
library(paradox)
library(mlr3tuning)
library(mlr3fselect)



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

load("~/EDcrowding/predict-admission/data-output/scores_table.rda")
file_date <- "2021-02-08"

base_model = FALSE
tune_spw = FALSE
tune_nr = FALSE
tune_trees = FALSE
tune_trees2 = TRUE


# Load data and encode factors --------------------------------------------------------------

timeslices <- c("000", "015", "030", "060", "120", "180", "240", "300", "360")
#timeslices <- c( "060", "120")


for (ts_ in timeslices) {
  
  # load timeslice 
  inFile = paste0("~/EDcrowding/predict-admission/data-raw/dm", ts_, "_", file_date, ".rda")
  
  load(inFile)
  
  name_ <- paste0("dm", ts_)
  dt = get(name_)
  
  dt[, row_id := seq_len(nrow(dt))]
  
  # I noticed that the 360 timeslices has 52% of visits with num_OTF > 0
  # wherease 300 timeslice has only 12% - suspect this is reason for high perf on 360
  # however, the performance is still much higher even without this
  dt[, num_OTF := NULL]
  
  # create vectors identifying test, val and training ids
  assign(paste0("task", ts_, "_test_ids"), dt[in_set == "test", row_id])
  assign(paste0("task", ts_, "_val_ids"), dt[in_set == "val", row_id])
  assign(paste0("task", ts_, "_train_ids"), dt[in_set == "train", row_id])
  
  # remove train-val-test label so it's not included in features
  dt[, in_set := NULL]
  
  # encode factors
  ts <- one_hot(cols = "auto", dt = as.data.table(dt),  dropUnusedLevels=TRUE)
  ts[,adm:=as.factor(adm)] 
  
  # assign to named datatable
  name_ <- paste0("dm", ts_, "p")
  assign(name_, ts)
  
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
  name_ <- paste0("dm", ts_, "p")
  ts = get(name_)
  
  # create task
  tsk = TaskClassif$new(id = name_, backend = ts ,target="adm") 
  tsk$col_roles$name = "csn"
  tsk$col_roles$feature = setdiff(tsk$col_roles$feature, "csn")
  tsk$positive = "1" # tell mlr3 which is the positive class
  name_ <- paste0("task", ts_)
  assign(name_, tsk)
}

# create learner
learner = lrn("classif.xgboost", predict_type = "prob")

measures = list(
  #  msr("classif.auc", id = "auc_train", predict_sets = "train"),
  msr("classif.acc", id = "acc_test"),
  msr("classif.auc", id = "auc_test"),
  msr("classif.auc", id = "mcc_test")
  
)

# Train without cross validation and check against validation set, no tuning -----------------------------------

if (base_model) {
  
  set.seed(17L)
  for (ts_ in timeslices) {
    name_ <- paste0("task", ts_)
    tsk = get(name_)
    tsk_train_ids = get(paste0(name_, "_train_ids"))
    tsk_val_ids = get(paste0(name_, "_val_ids"))
    
    # train learner on training set
    learner$train(tsk, row_ids = tsk_train_ids)
    
    # get predictions on validation set
    pred_train = learner$predict(tsk, row_ids = tsk_train_ids)
    score_train = data.table(train_mcc = pred_train$score(msr("classif.mcc")),
                             train_acc = pred_train$score(msr("classif.acc")),
                             train_auc = pred_train$score(msr("classif.auc")))
    pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
    score_val = data.table(val_mcc = pred_val$score(msr("classif.mcc")),
                           val_acc = pred_val$score(msr("classif.acc")),
                           val_auc = pred_val$score(msr("classif.auc"))) 
    score <- bind_cols(score_train, score_val)
    
    # # save predictions for training and validation set
    # preds = as.data.table(learner$predict(tsk, row_ids = c(tsk_train_ids, tsk_val_ids)))
    # preds$task = name_
    # # add csns
    # dm = get(paste0("dm", ts_, "p"))
    # csns = data.table(csn = dm$csn, row_id = seq_len(nrow(dm)))
    # preds <- merge(preds, csns, by = "row_id")
    # pred_no_tune <- bind_rows(pred_no_tune, preds)
    
    # save scores
    score$model = name_
    score$best = NA
    score$param_ = "base"
    
    score$eta = NA
    score$early_stopping_rounds = NA
    score$scale_pos_weight = 1
    score$dttm = now()
    
    
    scores_table <- bind_rows(scores_table, score)
    
  } 
  
}



# Tuning scale pos weight ------------------------------------------------------------------


if (tune_spw) {  
  
  # choose resampling strategy and performance measure
  cv = rsmp("cv")
  measure = msr("classif.mcc")
  
  # set budget
  # to see list of possible terminators (when to end the ML)
  as.data.table(mlr_terminators)
  
  # short way to specify number of evaluations to use
  evals20 = trm("evals", n_evals = 20)
  # # or long way:
  # evals20 = mlr_terminators$get("evals")
  # evals20$param_set$values = list("n_evals" = 20)
  
  spw_range <- vector()
  
  for (ts_ in timeslices) {
  
    name_ <- name_ <- paste0("dm", ts_, "p")
    ts  = get(name_)
  
    spw_range <- c(spw_range, nrow(ts[adm == 0])/nrow(ts[adm == 1]))
  }
  
  # to see current learner set - all parameters that can be tuned
  learner$param_set
  
  # set param grid
  tune_ps = ParamSet$new(list(
    #  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamDbl$new("scale_pos_weight", lower = min(spw_range), upper = max(spw_range))
  ))
  
  
  # # set tuner values
  spw_design = data.table(scale_pos_weight = spw_range)
  tuner = tnr("design_points", design = spw_design)
  
  # iterate through timeslices - run tuning
  
  spw_results <- data.table()
  
  for (ts_ in timeslices) {
  
    print(paste0("Processing timeslice ", ts_))
  
    name_ <- paste0("task", ts_)
    tsk = get(name_)
  
    tsk_train_ids = get(paste0(name_, "_train_ids"))
  
    instance = TuningInstanceSingleCrit$new(
      task = tsk,
      learner = learner$train(tsk, row_ids = tsk_train_ids),
      resampling = cv,
      measure = measure,
      search_space = tune_ps,
      terminator = evals20
    )
    tuner$optimize(instance)
  
    # save instance
    name_ <- paste0("instance", ts_)
    assign(name_, instance)
  
    # save results for print
    res <- data.table(instance$archive$data())
    res[, model := name_]
    spw_results <- bind_rows(spw_results, res)
  
  }
  
  # save results
  save(spw_results, file = paste0("EDcrowding/predict-admission/data-output/spw_results",today(),".rda"))
  
  
  # # train with best parameters
  # 
  # imp_results <- data.table()
  # pred_results <- data.table()
  # 
  # for (ts_ in timeslices) {
  # 
  #   name_ <- paste0("instance", ts_)
  #   instance = get(name_)
  #   learner$param_set$values = instance$result_learner_param_vals
  # 
  #   name_ <- paste0("task", ts_)
  #   tsk = get(name_)
  #   tsk_train_ids = get(paste0(name_, "_train_ids"))
  #   tsk_val_ids = get(paste0(name_, "_val_ids"))
  # 
  #   learner$train(tsk, row_ids = tsk_train_ids)
  # 
  #   # save importances
  #   imp <- data.table(learner$importance())
  #   imp$feature <- names(learner$importance())
  #   imp[, model := name_]
  #   imp_results <- bind_rows(imp_results, imp)
  # 
  # 
  #   # predict
  #   pred = as.data.table(learner$predict(tsk, row_ids = tsk_val_ids))
  #   pred_ = data.table(pred$P)
  #   pred[, model := name_]
  #   pred_results <- bind_rows(pred_results, pred)
  # }
  # 
  # imp_results[, count := .N, by = feature]
  # imp_results[count >2] %>% ggplot(aes(x = gsub("task","", model), y = reorder(feature, desc(feature)), fill = V1)) + geom_tile() +
  #   scale_fill_gradient(low="blue", high="red") +
  #   labs(title = "Feature importances by timeslice",
  #        fill = "Importance",
  #        x = "Timeslice",
  #        y = "Feature")
  # 
  # save(imp_results, file = paste0("EDcrowding/predict-admission/data-output/imp_results",today(),".rda"))
  # save(pred_results, file = paste0("EDcrowding/predict-admission/data-output/pred_results",today(),".rda"))
  
}


# Tuning nrounds ----------------------------------------------------------

if (tune_nr) {
  learner$param_set$values = mlr3misc::insert_named(
    learner$param_set$values,
    list(
      "eta" = 0.1,
      "early_stopping_rounds" = 10,
      "nthread" = 4
    )
  )
  
  term5 = trm("stagnation", iters = 5, threshold = 1e-4)
  
  tune_ps = ParamSet$new(list(
    #  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("nrounds", lower = 75, upper = 200)
  ))
  
  nr_results <- data.table()
  set.seed(17L)
  
  for (ts_ in timeslices) {
    
    print(paste0("Processing timeslice ", ts_))
    
    name_ <- name_ <- paste0("dm", ts_, "p")
    ts  = get(name_)
    
    sp <- nrow(ts[adm == 0])/nrow(ts[adm == 1])
    
    learner$param_set$values = mlr3misc::insert_named(
      learner$param_set$values,
      list(
        "scale_pos_weight" = sp
      )
    )
    tuner = tnr("grid_search", resolution = 6)
    
    name_ <- paste0("task", ts_)
    tsk = get(name_)
    
    tsk_train_ids = get(paste0(name_, "_train_ids"))
    
    instance = TuningInstanceSingleCrit$new(
      task = tsk,
      learner = learner$train(tsk, row_ids = tsk_train_ids),
      resampling = rsmp("cv"),
      measure = msr("classif.mcc"),
      search_space = tune_ps,
      terminator = term5
    )
    tuner$optimize(instance)
    
    # save instance
    name_ <- paste0("instance", ts_)
    assign(name_, instance)
    
    # save results for print
    res <- data.table(instance$archive$data())
    res[, model := name_]
    nr_results <- bind_rows(nr_results, res)
    
  }
  
  
  # nr_results %>% ggplot(aes(x = nrounds, y = classif.mcc, col = model, group = model)) + geom_line()
  save(nr_results, file = paste0("EDcrowding/predict-admission/data-output/nr_results_",today(),".rda"))
  
  
  for (ts_ in timeslices) {
    
    name_ <- paste0("instance", ts_)
    instance = get(name_)
    learner$param_set$values = instance$result_learner_param_vals
    
    name_ <- paste0("task", ts_)
    tsk = get(name_)
    tsk_train_ids = get(paste0(name_, "_train_ids"))
    tsk_val_ids = get(paste0(name_, "_val_ids"))
    
    learner$train(tsk, row_ids = tsk_train_ids)
    
    pred_train = learner$predict(tsk, row_ids = tsk_train_ids)
    score_train = data.table(train_mcc = pred_train$score(msr("classif.mcc")),
                             train_acc = pred_train$score(msr("classif.acc")),
                             train_auc = pred_train$score(msr("classif.auc")))
    pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
    score_val = data.table(val_mcc = pred_val$score(msr("classif.mcc")),
                           val_acc = pred_val$score(msr("classif.acc")),
                           val_auc = pred_val$score(msr("classif.auc"))) 
    score <- bind_cols(score_train, score_val)
    
    score$model = name_
    score$best = instance$result_learner_param_vals$nrounds
    score$param_ = "nrounds"
    
    score$eta = instance$result_learner_param_vals$eta
    score$early_stopping_rounds = instance$result_learner_param_vals$early_stopping_rounds
    score$scale_pos_weight = instance$result_learner_param_vals$scale_pos_weight
    
    score$dttm = now()
    
    scores_table <- bind_rows(scores_table, score)  
  }
  
  save(scores_table, file = paste0("EDcrowding/predict-admission/data-output/scores_table.rda"))
  
  
}



# Tuning tree parameters ----------------------------------------------------------

if (tune_trees) {

  learner$param_set$values = mlr3misc::insert_named(
    learner$param_set$values,
    list(
      "eta" = 0.1,
      "early_stopping_rounds" = 10,
      "nthread" = 4
    )
  )
  
  term5 = trm("stagnation", iters = 5, threshold = 1e-4)
  
  tune_ps = ParamSet$new(list(
    #  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("max_depth", lower = 3, upper = 9),
    ParamInt$new("min_child_weight", lower = 2, upper = 5)
  ))
  
  trees_results <- data.table()
  set.seed(17L)
  
  for (ts_ in timeslices) {
    
    print(paste0("Processing timeslice ", ts_))
    
    name_ <- name_ <- paste0("dm", ts_, "p")
    ts  = get(name_)
    
    sp <- nrow(ts[adm == 0])/nrow(ts[adm == 1])
    
    name_ <- paste0("task", ts_)
    tsk = get(name_)
    
    tsk_train_ids = get(paste0(name_, "_train_ids"))
    
    # get value for nrounds
    nr <- scores_table[model == name_ & param_ == "nrounds" , best]
    
    learner$param_set$values = mlr3misc::insert_named(
      learner$param_set$values,
      list(
        "scale_pos_weight" = sp,
        "nrounds" = nr
      )
    )
    tuner = tnr("grid_search", resolution = 4)
    
    instance = TuningInstanceSingleCrit$new(
      task = tsk,
      learner = learner$train(tsk, row_ids = tsk_train_ids),
      resampling = rsmp("cv"),
      measure = msr("classif.mcc"),
      search_space = tune_ps,
      terminator = term5
    )
    tuner$optimize(instance)
    
    # save instance
    name_ <- paste0("instance", ts_)
    assign(name_, instance)
    
    # save results for print
    res <- data.table(instance$archive$data())
    res[, model := name_]
    trees_results <- bind_rows(trees_results, res)
    
  }
  
    save(trees_results, file = paste0("EDcrowding/predict-admission/data-output/trees_results_",today(),".rda"))
    
    # trees_results %>% pivot_longer(max_depth: min_child_weight) %>% 
    #   group_by(model, name, value) %>% summarise(mean_mcc = mean(classif.mcc)) %>% 
    #   ggplot(aes(x = value, y = mean_mcc, col = model)) + geom_line() + facet_grid(. ~ name)
  
  
  for (ts_ in timeslices) {
    
    name_ <- paste0("instance", ts_)
    instance = get(name_)
    learner$param_set$values = instance$result_learner_param_vals
    
    name_ <- paste0("task", ts_)
    tsk = get(name_)
    tsk_train_ids = get(paste0(name_, "_train_ids"))
    tsk_val_ids = get(paste0(name_, "_val_ids"))
    
    learner$train(tsk, row_ids = tsk_train_ids)
    
    pred_train = learner$predict(tsk, row_ids = tsk_train_ids)
    score_train = data.table(train_mcc = pred_train$score(msr("classif.mcc")),
                             train_acc = pred_train$score(msr("classif.acc")),
                             train_auc = pred_train$score(msr("classif.auc")))
    pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
    score_val = data.table(val_mcc = pred_val$score(msr("classif.mcc")),
                           val_acc = pred_val$score(msr("classif.acc")),
                           val_auc = pred_val$score(msr("classif.auc"))) 
    score <- bind_cols(score_train, score_val)
    
    # save best max_depth
    score$model = name_
    score$best = instance$result_learner_param_vals$max_depth
    score$param_ = "max_depth"
    
    score$eta = instance$result_learner_param_vals$eta
    score$early_stopping_rounds = instance$result_learner_param_vals$early_stopping_rounds
    score$scale_pos_weight = instance$result_learner_param_vals$scale_pos_weight
    score$dttm = now()
    scores_table <- bind_rows(scores_table, score)  
    
    # save best min_child_weight
    score$model = name_
    score$best = instance$result_learner_param_vals$min_child_weight
    score$param_ = "min_child_weight"
    
    score$eta = instance$result_learner_param_vals$eta
    score$early_stopping_rounds = instance$result_learner_param_vals$early_stopping_rounds
    score$scale_pos_weight = instance$result_learner_param_vals$scale_pos_weight
    score$dttm = now()
    scores_table <- bind_rows(scores_table, score)  
    
    save(scores_table, file = paste0("EDcrowding/predict-admission/data-output/scores_table.rda"))
    
    
  }
  
  # scores_table[param %in% c("max_depth", "min_child_weight")] %>% pivot_longer(param, values_to = "param") %>% 
  #   ggplot(aes(x = model, y = best, col = param)) + geom_point() + facet_grid(. ~ param)
  
}

if (tune_trees2) {
  
  load(paste0("~/EDcrowding/predict-admission/data-output/trees_results_", today(), ".rda"))
  
  learner$param_set$values = mlr3misc::insert_named(
    learner$param_set$values,
    list(
      "eta" = 0.1,
      "early_stopping_rounds" = 10,
      "nthread" = 4,
      "eval_metric" = "auc"
    )
  )
  
  term5 = trm("stagnation", iters = 5, threshold = 1e-4)
  
  tune_ps = ParamSet$new(list(
    #  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("max_depth", lower = 2, upper = 10),
    ParamInt$new("min_child_weight", lower = 1, upper = 6)
  ))
  
  set.seed(17L)
  
  for (ts_ in timeslices) {
    
    print(paste0("Processing timeslice ", ts_))
    
    name_ <- name_ <- paste0("dm", ts_, "p")
    ts  = get(name_)
    
    sp <- nrow(ts[adm == 0])/nrow(ts[adm == 1])
    
    name_ <- paste0("task", ts_)
    tsk = get(name_)
    
    tsk_train_ids = get(paste0(name_, "_train_ids"))
    
    # get value for nrounds
    nr <- as.numeric(scores_table[(model == name_ & param_ == "nrounds" ), .SD[which.max(val_auc)], by=.(model)][,.(best)])
    
    learner$param_set$values = mlr3misc::insert_named(
      learner$param_set$values,
      list(
        "scale_pos_weight" = sp,
        "nrounds" = nr
      )
    )
    
    
    # # set tuner values
    
    md <- as.numeric(scores_table[(model == name_ & param_ == "max_depth" ), .SD[which.max(val_auc)], by=.(model)][,.(best)])
    mcw <- as.numeric(scores_table[(model == name_ & param_ == "min_child_weight" ), .SD[which.max(val_auc)], by=.(model)][,.(best)])
    
    
    tree_design = data.table(max_depth = c(md-1, md-1, md+1, md+1), min_child_weight = c(mcw+1, mcw-1, mcw+1, mcw-1))
    tuner = tnr("design_points", design = tree_design)

    instance = TuningInstanceSingleCrit$new(
      task = tsk,
      learner = learner$train(tsk, row_ids = tsk_train_ids),
      resampling = rsmp("cv"),
      measure = msr("classif.mcc"),
      search_space = tune_ps,
      terminator = term5
    )
    tuner$optimize(instance)
    
    # save instance
    name_ <- paste0("instance", ts_)
    assign(name_, instance)
    
    # save results for print
    res <- data.table(instance$archive$data())
    res[, model := name_]
    trees_results <- bind_rows(trees_results, res)
    
  }
  
  save(trees_results, file = paste0("EDcrowding/predict-admission/data-output/trees_results_",today(),".rda"))
  
  # trees_results %>% pivot_longer(max_depth: min_child_weight) %>% 
  #   group_by(model, name, value) %>% summarise(mean_mcc = mean(classif.mcc)) %>% 
  #   ggplot(aes(x = value, y = mean_mcc, col = model)) + geom_line() + facet_grid(. ~ name)
  
  
  for (ts_ in timeslices) {
    
    name_ <- paste0("instance", ts_)
    instance = get(name_)
    learner$param_set$values = instance$result_learner_param_vals
    
    name_ <- paste0("task", ts_)
    tsk = get(name_)
    tsk_train_ids = get(paste0(name_, "_train_ids"))
    tsk_val_ids = get(paste0(name_, "_val_ids"))
    
    learner$train(tsk, row_ids = tsk_train_ids)
    
    pred_train = learner$predict(tsk, row_ids = tsk_train_ids)
    score_train = data.table(train_mcc = pred_train$score(msr("classif.mcc")),
                             train_acc = pred_train$score(msr("classif.acc")),
                             train_auc = pred_train$score(msr("classif.auc")))
    pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
    score_val = data.table(val_mcc = pred_val$score(msr("classif.mcc")),
                           val_acc = pred_val$score(msr("classif.acc")),
                           val_auc = pred_val$score(msr("classif.auc"))) 
    score <- bind_cols(score_train, score_val)
    
    # save best max_depth
    score$model = name_
    score$best = instance$result_learner_param_vals$max_depth
    score$param_ = "max_depth"
    
    score$eta = instance$result_learner_param_vals$eta
    score$early_stopping_rounds = instance$result_learner_param_vals$early_stopping_rounds
    score$scale_pos_weight = instance$result_learner_param_vals$scale_pos_weight
    score$dttm = now()
    scores_table <- bind_rows(scores_table, score)  
    
    # save best min_child_weight
    score$model = name_
    score$best = instance$result_learner_param_vals$min_child_weight
    score$param_ = "min_child_weight"
    
    score$eta = instance$result_learner_param_vals$eta
    score$early_stopping_rounds = instance$result_learner_param_vals$early_stopping_rounds
    score$scale_pos_weight = instance$result_learner_param_vals$scale_pos_weight
    score$dttm = now()
    scores_table <- bind_rows(scores_table, score)  
    
    save(scores_table, file = paste0("EDcrowding/predict-admission/data-output/scores_table.rda"))
    
  }
  
  scores_table[param_ %in% c("max_depth", "min_child_weight")] %>% pivot_longer(param_, values_to = "param_") %>%
    ggplot(aes(x = model, y = best, col = param_)) + geom_point() + facet_grid(. ~ param_)
  
  # look up which parameter value gets max scores 
  scores_table[param_ == "max_depth", .SD[which.max(val_auc)], by=.(model)][,.(model,best)]
  
  
  
}
