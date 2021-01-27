
# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(data.table)
library(tidymodels)
library(skimr)

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


library(parallel)
library(parallelMap) 
parallelStartSocket(cpus = detectCores())
#parallelStop()



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


load("~/EDcrowding/predict-admission/data-raw/dm60_2021-01-27.rda")
load("~/EDcrowding/predict-admission/data-raw/dm120_2021-01-27.rda")

# put final test set on one side

set.seed(123)
dm_split_60 <- initial_split(dm60, strata = adm, prop = 4/5)
dm_train_val_60 <- training(dm_split_60)
rpt(dm_train_val_60)

dm_split_120 <- initial_split(dm120, strata = adm, prop = 4/5)
dm_train_val_120 <- training(dm_split_120)
rpt(dm_train_val_120)

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
task$positive = "1" # tell mlr3 which is the positive class

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

learner = lrn("classif.xgboost", predict_type = "prob")

resamplings = rsmp("cv", folds = 3)

design = benchmark_grid(
  tasks = list(task60, task120),
  learners = learner,
  resamplings = resamplings # note that you don't have to instantiate in this case
)
print(design)

bmr = benchmark(design)

# to see measures 
# as.data.table(mlr_measures)

measures = list(
#  msr("classif.auc", id = "auc_train", predict_sets = "train"),
  msr("classif.acc", id = "acc_test"),
  msr("classif.ce", id = "ce_test"),
  msr("classif.auc", id = "auc_test"),
  msr("classif.auc", id = "mcc_test")

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


# Tuning ------------------------------------------------------------------

# to see current learner set
learner$param_set

# set param grid
tune_ps = ParamSet$new(list(
#  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
  ParamInt$new("scale_pos_weight", lower = 1, upper = 5)
))

# choose resampling strategy and performance measure
cv = rsmp("cv")
measure = msr("classif.mcc")

# set budget
# to see list of possible terminators
as.data.table(mlr_terminators)

# short way
evals20 = trm("evals", n_evals = 20)
# or this way: 
evals20 = mlr_terminators$get("evals")
evals20$param_set$values = list("n_evals" = 20)

# create tuner instance
instance = TuningInstanceSingleCrit$new(
  task = task60,
  learner = learner,
  resampling = cv,
  measure = measure,
  search_space = tune_ps,
  terminator = evals20
)
instance

# set tuner class
as.data.table(mlr_tuners)

tuner = tnr("grid_search", resolution = 5)

# apply the instance to the tuner class
# see https://mlr3book.mlr-org.com/tuning.html for how it works
tuner$optimize(instance)

instance$result_learner_param_vals # best params
instance$result_y # best result

# can see all the instances
instance$archive$data()
results = as.data.frame(instance$archive$data())
results %>% ggplot(aes(x = scale_pos_weight, y = classif.mcc)) + geom_point()
autoplot(as.data.frame(instance$archive$data()))

# see the results for each resampling iteration on a different performance measure
instance$archive$benchmark_result$score(msr("classif.acc"))

# use the best performing parameters and train on full dataset
learner$param_set$values = instance$result_learner_param_vals
learner$train(task60)



# Trying feature selection ------------------------------------------------

# note - this doesn't optimise the parameters, just the tuning

cv2 = rsmp("cv")

instanceF = FSelectInstanceSingleCrit$new(
  task = task60,
  learner = learner,
  resampling = cv2,
  measure = measure,
  terminator = evals20
)
instanceF

fselector = fs("random_search")
fselector$optimize(instanceF)

instanceF$result_feature_set # has selected 223 of the 254 features but one of them is year?? 
instanceF$result_y

instanceF$archive$data()

instanceF$archive$benchmark_result$data

# fit the optimised feature set to the data in the task
task60$select(instanceF$result_feature_set)
learner$train(task60)



# Plot of shap values -----------------------------------------------------

# According to: https://www.kaggle.com/c/homesite-quote-conversion/discussion/18669
# In the R version, only the features used in at least one split make it to the xgb.importance output. If the feature didn't make it to the output, then it must either be utterly useless, have zero variance, or be extremely or perfectly correlated to another feature before it.

learner3$importance() # can get importance this way
# or this 
importance = xgboost::xgb.importance(model = learner$model)

# added learner3$importance() to the data table created by xgb.importance; seems like Gain and importance are the sam
importance[,importance := learner3$importance()]

# removing the column I just created
importance[, importance := NULL]

# I managed to create mean shap values by exluding the adm label which otherwise causes an error
shap_values = shap.values(learner$model, task$data()[train_set,2:ncol(task$data()[train_set])])

shap_tibble <- as_tibble(labels(shap_values$mean_shap_score)) %>% rename(Feature = value) %>% 
  bind_cols(Mean_Shap = shap_values$mean_shap_score)

importance %>% left_join(shap_tibble) %>% 
  pivot_longer(Gain:Mean_Shap, names_to = "importance_type", values_to = "values") %>% 
  mutate(Feature = fct_reorder(Feature, values)) %>% 
  ggplot(aes(x = Feature, y = values, fill = importance_type)) + geom_bar(stat = "identity") +
  facet_wrap( ~ importance_type) + coord_flip() + theme(legend.position = "none") +
  labs(title = "Model importances for 60 min timeslice excluding admission characteristics - Post Surge1")


# Trying to get shap plots ------------------------------------------------

# lots of warnings on the below
# shap_long <- shap.prep(shap_contrib = shap_values$shap_score[,labels(learner3$importance())], 
#                        X_train = task2$data()[train_set,2:367])

# this flagged warnings because all were not same datatype
# temp = task2$data()[train_set]
# sapply(temp, class) # useful way to see all their classes
# 
# temp[, names(temp) := lapply(.SD, as.numeric)]

# but the best way to do it seemed to be:
shap_long <- shap.prep(xgb_model = learner$model, X_train = task$data()[train_set,2:ncol(task$data()[train_set])])

# to see only the rows where mean score > 0
shap_gt0 = labels(shap_values$mean_shap_score[shap_values$mean_shap_score>0])
shap <- shap_long[shap_gt0]

shap.plot.summary(shap_long, dilute = 100)


# this took too long - then I found top_n - yay - but need to switch classes around! task$positive = "M"
shap.plot.summary.wrap1(model = learner$model, X = task$data()[train_set,2:ncol(task$data()[train_set])], top_n = 10)

