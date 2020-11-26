
# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(data.table)
library(tidymodels)

# for mlr3
library(mlr3)
library(mlr3learners)
library(mlr3proba)
library(mlr3viz)
library(GGally)
library(precrec)


library(parallel)
library(parallelMap) 
parallelStartSocket(cpus = detectCores())
parallelStop()



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



# Load data --------------------------------------------------------------


load("~/EDcrowding/predict-admission/data-raw/matrix_60_2020-11-23.rda")
load("~/EDcrowding/predict-admission/data-raw/matrix_120_2020-11-23.rda") 


dm_60 <- matrix_60 %>%
  filter(age >= 18, age < 110, epoch == "Post_Surge1") %>% 
  mutate(sex = as.factor(sex)) %>% 
  select(-mrn, -csn_old, -ED_duration_final)


dm_120 <- matrix_120 %>%
  filter(age >= 18, age < 110, epoch == "Post_Surge1") %>% 
  mutate(sex = as.factor(sex)) %>% 
  select(-mrn, -csn_old, -ED_duration_final)

# put final test set on one side

set.seed(123)
dm_split_60 <- initial_split(dm_60, strata = adm, prop = 4/5)
dm_train_val_60 <- training(dm_split_60)

dm_split_120 <- initial_split(dm_120, strata = adm, prop = 4/5)
dm_train_val_120 <- training(dm_split_120)

# encode factors

proc_dm60 <- one_hot(cols = "auto", dt = as.data.table(dm_train_val_60),  dropUnusedLevels=TRUE)
proc_dm60[,adm:=as.factor(adm)] 


proc_dm120 <- one_hot(cols = "auto", dt = as.data.table(dm_train_val_120),  dropUnusedLevels=TRUE)
proc_dm120[,adm:=as.factor(adm)]



# Run xgboost with just dm60 ----------------------------------------------------------

# create task
task = TaskClassif$new(id = "dm60", backend = proc_dm60 ,target="adm") 
task$col_roles$name = "csn"
task$col_roles$feature = setdiff(task$col_roles$feature, "csn")
task$feature_names # csn should no longer be included
task$positive = "TRUE"

train_set = sample(task$nrow, 0.8 * task$nrow)
dm_train = as.data.table(task$data()[train_set])
test_set = setdiff(seq_len(task$nrow), train_set)
dm_test = as.data.table(task$data()[test_set])

learner = lrn("classif.xgboost", predict_type = "prob")

# create folds 
cvfolds = mlr_resamplings$get("cv") # note default is 10 faults
cvfolds$param_set$values = list(folds = 5) # change this to 5

# call method instantiate to generate folds on this task
cvfolds$instantiate(task)

# see which cases assigned 
str(cvfolds$train_set(1)) # cases in fold 1 training set
str(cvfolds$test_set(5)) # cases in fold 5 test set


# Train learner on training set
learner$train(task, row_ids = train_set)
learner$model # view learner model

# Predict on test set
pred = learner$predict(task, row_ids = test_set)
pred$confusion

# plotting predictions
autoplot(pred)
autoplot(pred, type = "roc")

# save predictions (test set)
save_pred = as.data.table(pred)
save_pred[, model:="base"]
save_pred[, csn:= proc_dm60[test_set]$csn] # add csn


# Cross validation --------------------------------------------------------

# Train learner on training set with cross-val
rr = resample(task, learner, cvfolds, store_models = TRUE)
print(rr)

# see average accuracy across all resamples
rr$aggregate(msr("classif.acc"))

# or get it for each
rr$score(msr("classif.acc"))

# Or retrieve the learner of a specific iteration and inspect it:
lrn = rr$learners[[1]]
lrn$model

# get predictions
rr$prediction() # all predictions merged into a single Prediction
rr$predictions()[[1]] # prediction of first resampling iteration


# plotting
autoplot(rr) # box plot
autoplot(rr, type = "roc")  # nice !!

# extract performance
measures = list(
  #  msr("classif.auc", id = "auc_train", predict_sets = "train"),
  msr("classif.acc", id = "acc_test"),
  msr("classif.auc", id = "auc_test")
)
perf = rr$score(measures)


# Benchmarking ------------------------------------------------------------

# A design is a combination of learner, task and resampling
# Here trying one learners on both datasets

task60 = TaskClassif$new(id = "dm60", backend = proc_dm60 ,target="adm") 
task120 = TaskClassif$new(id = "dm120", backend = proc_dm120 ,target="adm") 

task60$col_roles$name = "csn"
task60$col_roles$feature = setdiff(task60$col_roles$feature, "csn")

task120$col_roles$name = "csn"
task120$col_roles$feature = setdiff(task120$col_roles$feature, "csn")

resamplings = rsmp("cv", folds = 3)

design = benchmark_grid(
  tasks = list(task60, task120),
  learners = learner,
  resamplings = resamplings # note that you don't have to instantiate in this case
)
print(design)

bmr = benchmark(design)


measures = list(
#  msr("classif.auc", id = "auc_train", predict_sets = "train"),
  msr("classif.acc", id = "acc_test"),
  msr("classif.auc", id = "auc_test")
)
bmr$aggregate(measures)
tab = bmr$aggregate(measures) # see book for more on ranking measures

autoplot(bmr) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) # this plot doesn't generaet what I expect

# extract performance
i = which.min(bmr$acc_test)
print(bmr$learners[[i]]) # this is the learner with the best performance

# Here trying diffferent learners as well

learners = c("classif.xgboost", "classif.rpart")

learners = lapply(learners, lrn,
                  predict_type = "prob", predict_sets = c("train", "test"))

resamplings2 = rsmp("cv", folds = 5)

design2 = benchmark_grid(
  tasks = list(task60, task120),
  learners = learners,
  resamplings = resamplings2 # note that you don't have to instantiate in this case
)
print(design2)

bmr2 = benchmark(design2)
bmr2$aggregate(measures)
tab = bmr2$aggregate(measures) # see book for more on ranking measures


# extracting best perforrming learner is possible but I haven't achieved; see book
