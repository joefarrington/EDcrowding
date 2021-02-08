



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

rpt <- function(dataframe) {
  print(dataframe %>% select(csn) %>% n_distinct())
}



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


# Load data and encode factors --------------------------------------------------------------

timeslices <- c("000", "015", "030", "060", "120", "180", "240", "300", "360")
#timeslices <- c( "060", "120")


for (ts_ in timeslices) {
  
  # load timeslice 
  file_date <- today()
  inFile = paste0("~/EDcrowding/predict-admission/data-raw/dm", ts_, "_", file_date, ".rda")
  
  load(inFile)
  
  name_ <- paste0("dm", ts_)
  dt = get(name_)
  
  dt[, row_id := seq_len(nrow(dt))]
  
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

pred_no_tune <- data.table()
scores_no_tune <- data.table()

for (ts_ in timeslices) {
  name_ <- paste0("task", ts_)
  tsk = get(name_)
  tsk_train_ids = get(paste0(name_, "_train_ids"))
  tsk_val_ids = get(paste0(name_, "_val_ids"))
  
  # train learner on training set
  learner$train(tsk, row_ids = tsk_train_ids)
  
  # get predictions on validation set
  pred = learner$predict(tsk, row_ids = tsk_val_ids)
  print(name_)
  print(pred$confusion)
  
  # save predictions for training and validation set
  preds = as.data.table(learner$predict(tsk, row_ids = c(tsk_train_ids, tsk_val_ids)))
  preds$task = name_
  # add csns
  dm = get(paste0("dm", ts_, "p"))
  csns = data.table(csn = dm$csn, row_id = seq_len(nrow(dm)))
  preds <- merge(preds, csns, by = "row_id")
  pred_no_tune <- bind_rows(pred_no_tune, preds)
  
  # save scores
  scores = data.table(score = pred$score(measures))
  scores$meas = names(pred$score(measures))
  scores$task <- paste0(name_, "_val")
  scores_no_tune <- bind_rows(scores_no_tune, scores)
  
} 

scores_no_tune %>% ggplot(aes(x = task, y = score, col = meas, group = meas)) + geom_line()

save(scores_no_tune, file = paste0("EDcrowding/predict-admission/data-output/scores_no_tune",today(),".rda"))

# Tuning scale pos weight ------------------------------------------------------------------

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


# # set tuner values - not sure of the relationship between this and param grid but you seen to need both
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

# print results of tuning
spw_results[, ts := gsub("instance", "", model)] 
spw_results %>% ggplot(aes(x = scale_pos_weight, y = classif.mcc, group = model, col = as.numeric(ts))) + 
  geom_line() +
  scale_x_continuous(limits = c(0, 4)) +
  labs(title = "Results of tuning scale_pos_weight", 
       col = "Timeslice (darker = earlier)") +
  theme(legend.position = "bottom")

# save results
save(spw_results, file = paste0("EDcrowding/predict-admission/data-output/spw_results",today(),".rda"))

# look at best param values
for (ts_ in timeslices) {
  
  name_ <- paste0("instance", ts_)
  instance = get(name_)
  print(name_)
  print(instance$result_learner_param_vals$scale_pos_weight)
}



# train with best parameters

imp_results <- data.table()
pred_results <- data.table()

for (ts_ in timeslices) {
  
  name_ <- paste0("instance", ts_)
  instance = get(name_)
  learner$param_set$values = instance$result_learner_param_vals
  
  name_ <- paste0("task", ts_)
  tsk = get(name_)
  tsk_train_ids = get(paste0(name_, "_train_ids"))
  tsk_val_ids = get(paste0(name_, "_val_ids"))
  
  learner$train(tsk, row_ids = tsk_train_ids)
  
  # save importances
  imp <- data.table(learner$importance())
  imp$feature <- names(learner$importance())
  imp[, model := name_]
  imp_results <- bind_rows(imp_results, imp)

  
  # predict
  pred = as.data.table(learner$predict(tsk, row_ids = tsk_val_ids))
  pred_ = data.table(pred$P)
  pred[, model := name_]
  pred_results <- bind_rows(pred_results, pred)
}

imp_results[, count := .N, by = feature]
imp_results[count >2] %>% ggplot(aes(x = gsub("task","", model), y = reorder(feature, desc(feature)), fill = V1)) + geom_tile() + 
  scale_fill_gradient(low="blue", high="red") + 
  labs(title = "Feature importances by timeslice",
       fill = "Importance", 
       x = "Timeslice",
       y = "Feature")

save(imp_results, file = paste0("EDcrowding/predict-admission/data-output/imp_results",today(),".rda"))
save(pred_results, file = paste0("EDcrowding/predict-admission/data-output/pred_results",today(),".rda"))



# Tuning nrounds ----------------------------------------------------------

spw_range <- vector()

for (ts_ in timeslices) {
  
  name_ <- name_ <- paste0("dm", ts_, "p")
  ts  = get(name_)
  
  spw_range <- c(spw_range, nrow(ts[adm == 0])/nrow(ts[adm == 1]))
}

# choose resampling strategy and performance measure
cv = rsmp("cv")
measure = msr("classif.mcc")
evals20 = trm("evals", n_evals = 20)
term5 = trm("stagnation", iters = 5, threshold = 1e-4)

tune_ps = ParamSet$new(list(
  #  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
  ParamInt$new("nrounds", lower = 1, upper = 1000),   
  ParamDbl$new("scale_pos_weight", lower = min(spw_range), upper = max(spw_range)),
  ParamDbl$new("eta", lower = .0001, upper = 1)
))

# I first ran this with 1000 as max rounds on the 60 and 120 min timeslices (and default learning rate)
# 1000 was no improvement over 10, and each of these was only marginally worse than 100
nr_design = data.table(nrounds = c(10, 100, 500))
nr_design$eta = 0.1
nr_results <- data.table()


for (ts_ in timeslices) {
  
  print(paste0("Processing timeslice ", ts_))
  
  name_ <- name_ <- paste0("dm", ts_, "p")
  ts  = get(name_)
  
  sp <- nrow(ts[adm == 0])/nrow(ts[adm == 1])
  nr_design$scale_pos_weight = sp
  tuner = tnr("design_points", design = nr_design)
  
  name_ <- paste0("task", ts_)
  tsk = get(name_)
  
  tsk_train_ids = get(paste0(name_, "_train_ids"))
  
  instance = TuningInstanceSingleCrit$new(
    task = tsk,
    learner = learner$train(tsk, row_ids = tsk_train_ids),
    resampling = cv,
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

# run again with narrower range 

nr_design = data.table(nrounds = c(50, 150))
nr_design$eta = 0.1

nr_results %>% ggplot(aes(x = nrounds, y = classif.mcc, col = model, group = model)) + geom_line()

nr_design = data.table(nrounds = c(70, 90, 110, 130, 150))
nr_design$eta = 0.1

# then re-run as above

save(nr_results, file = paste0("EDcrowding/predict-admission/data-output/nr_results",today(),".rda"))



# Tuning nrounds a different way --------------------------

learner$param_set$values = mlr3misc::insert_named(
  learner$param_set$values,
  list(
    "eta" = 0.1,
    "early_stopping_rounds" = 10,
    "nthread" = 4
  )
)


term5 = trm("stagnation", iters = 5, threshold = 1e-4)

timeslices <- c("015", "060", "120", "180")

tune_ps = ParamSet$new(list(
  #  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
  ParamInt$new("nrounds", lower = 50, upper = 175)
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


nr_results %>% ggplot(aes(x = nrounds, y = classif.mcc, col = model, group = model)) + geom_line()
save(nr_results, file = paste0("EDcrowding/predict-admission/data-output/nr_results2",today(),".rda"))

# look at best param values
for (ts_ in timeslices) {
  
  name_ <- paste0("instance", ts_)
  instance = get(name_)
  print(name_)
  print(instance$result_learner_param_vals$nrounds)
}


# train with best parameters

scores_nrounds = data.table()
measure = msr("classif.mcc")


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
  score = data.table(train_mcc = pred_train$score(measure))
  pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
  score$val_mcc = pred_val$score(measure)
  
  score$model = name_
  score$best = instance$result_learner_param_vals$nrounds
  score$param = "nrounds"
  
  score$eta = instance$result_learner_param_vals$eta
  score$early_stopping_rounds = instance$result_learner_param_vals$early_stopping_rounds
  score$scale_pos_weight = instance$result_learner_param_vals$scale_pos_weight
  
  scores_nrounds = bind_rows(scores_nrounds, score)
  
}

save(scores_nrounds, file = paste0("EDcrowding/predict-admission/data-output/scores_nrounds_",today(),".rda"))

