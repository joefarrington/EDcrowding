



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


# Load data --------------------------------------------------------------

load("~/EDcrowding/predict-admission/data-raw/dm0_2021-02-01.rda")
load("~/EDcrowding/predict-admission/data-raw/dm15_2021-02-01.rda")
load("~/EDcrowding/predict-admission/data-raw/dm30_2021-02-01.rda")
load("~/EDcrowding/predict-admission/data-raw/dm60_2021-02-01.rda")
load("~/EDcrowding/predict-admission/data-raw/dm120_2021-02-01.rda")
load("~/EDcrowding/predict-admission/data-raw/dm180_2021-02-01.rda")
load("~/EDcrowding/predict-admission/data-raw/dm240_2021-02-01.rda")
load("~/EDcrowding/predict-admission/data-raw/dm300_2021-02-01.rda")
load("~/EDcrowding/predict-admission/data-raw/dm360_2021-02-01.rda")

# Encode factors ----------------------------------------------------------


# encode factors
dm000p <- one_hot(cols = "auto", dt = as.data.table(dm0),  dropUnusedLevels=TRUE)
dm000p[,adm:=as.factor(adm)] 
dm015p <- one_hot(cols = "auto", dt = as.data.table(dm15),  dropUnusedLevels=TRUE)
dm015p[,adm:=as.factor(adm)] 
dm030p <- one_hot(cols = "auto", dt = as.data.table(dm30),  dropUnusedLevels=TRUE)
dm030p[,adm:=as.factor(adm)] 
dm060p <- one_hot(cols = "auto", dt = as.data.table(dm60),  dropUnusedLevels=TRUE)
dm060p[,adm:=as.factor(adm)] 
dm090p <- one_hot(cols = "auto", dt = as.data.table(dm60),  dropUnusedLevels=TRUE)
dm090p[,adm:=as.factor(adm)] 
dm120p <- one_hot(cols = "auto", dt = as.data.table(dm120),  dropUnusedLevels=TRUE)
dm120p[,adm:=as.factor(adm)] 
dm180p <- one_hot(cols = "auto", dt = as.data.table(dm180),  dropUnusedLevels=TRUE)
dm180p[,adm:=as.factor(adm)] 
dm240p <- one_hot(cols = "auto", dt = as.data.table(dm240),  dropUnusedLevels=TRUE)
dm240p[,adm:=as.factor(adm)] 
dm300p <- one_hot(cols = "auto", dt = as.data.table(dm300),  dropUnusedLevels=TRUE)
dm300p[,adm:=as.factor(adm)] 
dm360p <- one_hot(cols = "auto", dt = as.data.table(dm360),  dropUnusedLevels=TRUE)
dm360p[,adm:=as.factor(adm)] 
rm(dm0, dm15, dm30, dm60, dm120, dm180, dm240, dm300, dm360)


# Check timeslices --------------------------------------------------------

timeslices <- c("000", "015", "030", "060", "120", "180", "240", "300", "360")

adm_summ <- data.table()

for (ts_ in timeslices) {
  name_ <- paste0("dm", ts_, "p")
  ts = get(name_)
  num_adm <- ts[, .N, by = adm]
  num_adm[, model := ts_]
  adm_summ <- bind_rows(adm_summ, num_adm)
  
}

# Trying to show proportions on chart - can't think how to do it
props <- adm_summ %>% pivot_wider(names_from = adm, values_from = N) 
cols <- c("model", "admitted", "discharged") 
setnames(props, cols )
props <- props %>% mutate(prop = admitted/discharged)

  mutate(adm2 = if_else(adm == 1, "admitted", "discharged")) %>% mutate(prop = `1`/`0`) %>% select(model, prop)
# look at class balance as timeslices progress
adm_summ %>% ggplot(aes(x = model, y = N, fill = adm)) + geom_bar(stat = "identity") + 
  labs(title = "Numbers admitted / not admitted in each timeslice", 
       fill = "Admitted (1 = TRUE)",
       x = "Timeslice") +
  theme(legend.position = "bottom") +
  geom_text(aes(label = paste0(freq,"%")),
            position = position_stack(vjust = 0.5), size = 2)


# Set up ML ------------------------------------------------------------

# create task

for (ts_ in timeslices) {
  name_ <- paste0("dm", ts_, "p")
  ts = get(name_)
  
  tsk = TaskClassif$new(id = name_, backend = ts ,target="adm") 
  tsk$col_roles$name = "csn"
  tsk$col_roles$feature = setdiff(tsk$col_roles$feature, "csn")
  tsk$positive = "1" # tell mlr3 which is the positive class
  
  name_ <- paste0("task", ts_)
  assign(name_, tsk)
  
}

# create learner
learner = lrn("classif.xgboost", predict_type = "prob")


# Run without tuning ------------------------------------------------------

resamplings = rsmp("cv", folds = 5)

design = benchmark_grid(
  tasks = list(task000, task015, task030, task060, task120, task180, task240, task300, task360),
  learners = learner,
  resamplings = resamplings # note that you don't have to instantiate in this case
)
print(design)

bmr = benchmark(design)


measures = list(
  #  msr("classif.auc", id = "auc_train", predict_sets = "train"),
  msr("classif.acc", id = "acc_test"),
  msr("classif.ce", id = "ce_test"),
  msr("classif.auc", id = "auc_test"),
  msr("classif.auc", id = "mcc_test")
  
)

result <- data.table(bmr$aggregate(measures))
result[, timeslice := gsub("dm", "", task_id)]
result[, timeslice := gsub("p", "", timeslice)]

result %>% pivot_longer(cols = c(acc_test, mcc_test)) %>% 
  ggplot(aes(x = timeslice, y = value, group = name, col = name)) + geom_line() +
  labs(title = "Performance on training set: XGBoost with 5 fold cross-validation without tuning", 
       col = "Performance measure") +
  theme(legend.position = "bottom")

tab = bmr$aggregate(measures) # see book for more on ranking measures



# Tuning scale pos weight ------------------------------------------------------------------

# to see current learner set - all parameters that can be tuned
learner$param_set

# set param grid
tune_ps = ParamSet$new(list(
  #  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
  ParamDbl$new("scale_pos_weight", lower = 0.5, upper = 4)
))

# choose resampling strategy and performance measure
cv = rsmp("cv")
measure = msr("classif.mcc")

# set budget
# to see list of possible terminators (when to end the ML)
as.data.table(mlr_terminators)

# short way to specify number of evaluations to use
evals20 = trm("evals", n_evals = 20)
# or long way: 
evals20 = mlr_terminators$get("evals")
evals20$param_set$values = list("n_evals" = 20)
# 
# set tuner values - not sure of the relationship between this and param grid
as.data.table(mlr_tuners)
spw_design = data.table(scale_pos_weight = c(4, 3, 2, 1, 0.5))
tuner = tnr("design_points", design = spw_design)

# iterate through timeslices - run tuning

spw_results <- data.table()

for (ts_ in timeslices) {
  
  print(paste0("Processing timeslice ", ts_))
  
  name_ <- paste0("task", ts_)
  tsk = get(name_)
  
  instance = TuningInstanceSingleCrit$new(
    task = tsk,
    learner = learner,
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
  learner$train(tsk)
  
  # save importances
  imp <- data.table(learner$importance())
  imp$feature <- names(learner$importance())
  imp[, model := name_]
  imp_results <- bind_rows(imp_results, imp)
  
  # predict
  pred = as.data.table(learner$predict(tsk))
  pred_ = data.table(pred$P)
  pred[, model := name_]
  pred_results <- bind_rows(pred_results, pred)
}


save(imp_results, file = paste0("EDcrowding/predict-admission/data-output/imp_results",today(),".rda"))
save(pred_results, file = paste0("EDcrowding/predict-admission/data-output/pred_results",today(),".rda"))



instance000$result_learner_param_vals # best params
instance$result_y # best result


# see the results for each resampling iteration on a different performance measure
instance$archive$benchmark_result$score(msr("classif.acc"))

# use the best performing parameters and train on full dataset
learner$param_set$values = instance$result_learner_param_vals
learner$train(task60)

