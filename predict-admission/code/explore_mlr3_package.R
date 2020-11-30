
# Load libraries ----------------------------------------------------------


# for mlr
library(mlr)
# library(mlrMBO)
# library(DiceKriging)
# library(rgenoud)
library(iml)


# for mlr3
library(mlr3)
library(mlr3learners)
library(mlr3proba)
library(mlr3viz)
library(Ggally)
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

# Trying using MLR --------------------------------------------------------
# working through the quick start on the cheat sheet

load("~/EDcrowding/predict-admission/data-raw/matrix_60_2020-11-23.rda")

# create dummy features
dm <- createDummyFeatures(matrix_60 %>%
                            filter(age >= 18, age < 110, epoch == "Post_Surge1") %>% 
                            select(-mrn, -csn, -csn_old, -ED_duration_final)  %>% mutate(sex = as.factor(sex))
                          ,target="adm") 


# create a task to predict the adm column
set.seed(123)
tsk = makeClassifTask(data=dm,target="adm") 
# distribution of adm between FALSE and TRUE is held in $class.distribution

# create training and test sets
ho = makeResampleInstance("Holdout",tsk) # with holdout, use split= to control split
tsk.train = subsetTask(tsk,ho$train.inds[[1]]) 
tsk.test = subsetTask(tsk,ho$test.inds[[1]])


# create xgboost learner that will build 10 trees
lrn = makeLearner("classif.xgboost",nrounds=10) # set predict type (response or prob here)

# test performance using 5 fold cross validation
cv = makeResampleDesc("CV",iters=5) # with makeResampleInstance use stratify to keep target proportions consistent across samples
res = resample(lrn,tsk.train,cv,acc)


# make parameter set
ps = makeParamSet(makeNumericParam("eta",0,1), makeNumericParam("lambda",0,200), makeIntegerParam("max_depth",1,20))

# use model-based-optimisation to control the search (requires additional packaages rgenoud, DeciKiriging and mlrMBO which I have now uninstalled) 
tc = makeTuneControlMBO(budget=100)
tr = tuneParams(lrn,tsk.train,cv5,acc,ps,tc) # results are in ty$y or more in tr$mbo.result / best params are in tr$x

# update the lrn task with the tuned hyperparameters
lrn = setHyperPars(lrn,par.vals=tr$x)


# train on full training set
mdl = train(lrn,tsk.train)
prd = predict(mdl,tsk.test)

calculateConfusionMatrix(prd)

# Train the model on the full set - to use on real world instances
#mdl = train(lrn,tsk)  

# trying to get feature importances 
lrn2 = makeLearner("classif.xgboost",nrounds=10, predict.type = "prob") # set predict type (response or prob here)


res2 = resample(lrn2,tsk.train,cv,acc)
mdl2 = train(lrn2,tsk.train)
prd2 = predict(mdl2,tsk.test)


X = dm[which(names(dm) != "adm")]
# this may work but how do I access the data in the training set ???
predictor = Predictor$new(mod, data = X, y = dm$adm)

## stopped here and moved to MRL3


# Basic MLRs intro --------------------------------------------------------
# from https://mlr3gallery.mlr-org.com/posts/2020-03-18-iris-mlr3-basics/

# creates mlr3 task from scratch, from a data.frame
# 'target' names the column in the dataset we want to learn to predict

load("~/EDcrowding/predict-admission/data-raw/matrix_60_2020-11-23.rda")

# 3 ways of doing the same thing
task = TaskClassif$new(id = "iris", backend = iris, target = "Species")
task = mlr_tasks$get("iris") # some tasks are stored in 
task = tsk("iris")
print(task)

# need to encode factors in my data

proc_dm <- one_hot(cols = "auto", dt = as.data.table(matrix_60 %>%
                                                       filter(age >= 18, age < 110, epoch == "Post_Surge1") %>% 
                                                       mutate(sex = as.factor(sex)) %>% 
                                                       select(-mrn, -csn, -csn_old, -ED_duration_final)),  dropUnusedLevels=TRUE)

proc_dm[,adm:=as.factor(adm)] # omitting the = shows the result without updating

# create task
task = TaskClassif$new(id = "dm_60_proc", backend = proc_dm ,target="adm") 

# create learner
mlr_learners_table = as.data.table(mlr_learners) # see options

learner1 = mlr_learners$get("classif.xgboost")
# or learner1 = lrn("classif.xgboost")

# set response type
learner1$predict_type = "prob"

# train and predict
learner1$train(task, row_ids = 1:120) # note - learner1 just doing 120 rows
print(learner1$param_set)



print(learner1$model)
preds = learner1$predict(task, row_ids = 121:150)

# performance measures
head(as.data.table(mlr_measures))
s = preds$score(msr("classif.acc"))
s

# adding another learner
learner2 = lrn("classif.rpart", predict_type = "prob", minsplit = 50)

# this is how to set parameters - but note the command above also does it
learner2$param_set$values$minsplit = 50

# you can also insert parameters into an existing list
learner$param_set$values = mlr3misc::insert_named(
  learner$param_set$values,
  list(cp = 0.02, minsplit = 2)
)


# resampling
cv10 = rsmp("cv", folds = 10)
r = resample(task, learner2, cv10) # I think this did all rows, not just the 120
print(r)
r$score(msrs(c("classif.acc", "classif.ce")))
r$data # messy / meaningless ??

preds = r$prediction()
as.data.table(preds) # to get predictions out

cm = preds$confusion

# benchmarking learners

learners = list(learner1, learner2)
bm_grid = benchmark_grid(task, learners, cv10)
bm = benchmark(bm_grid)
print(bm)
print(bm$aggregate(measures = msrs(c("classif.acc", "classif.ce"))))


# other
task$nrow # get number of rows
task$ncol
task$data()

# subsetting the data
task$data(rows = c(1, 51, 101), cols = "age")

# to extract complete training data
d = as.data.table(task)

# roles of columns
task$col_roles
names(task$col_roles) # see supported col roles
# assign column csn (if it existed the role of name)
task$col_roles$name = "csn"
task$col_roles$feature = setdiff(task$col_roles$feature, "csn")
task$feature_names # csn should no longer be included

# can also assign roles to rows - eg use them only for validation
task$select(head(task$feature_names, 3))
autoplot(task)
autoplot(task, type = "pairs")
autoplot(task, type = "duo") # this is so much easier than the method I was doing!!


# Now following MLR3 book -------------------------------------------------
# https://mlr3book.mlr-org.com/


# doing with train-test split

task2 = TaskClassif$new(id = "dm_60_proc", backend = proc_dm ,target="adm") 


train_set = sample(task2$nrow, 0.8 * task2$nrow)
test_set = setdiff(seq_len(task2$nrow), train_set)

# to get the data out of these 
task2$data()[train_set]

learner = lrn("classif.xgboost")
learner$model # null before we train it

learner$train(task2, row_ids = train_set)
learner$model # can now access the model

pred2 = learner$predict(task, row_ids = test_set)
pred2$confusion

# changing the prediction type
learner$predict_type = "prob"

# re-fit the model
learner$train(task2, row_ids = train_set)
pred2 = learner$predict(task2, row_ids = test_set)
pred2$confusion
head(as.data.table(pred2))


# plotting predictions
autoplot(pred2)
autoplot(pred2, type = "roc")

# predictions for regression
local({ # we do this locally to not overwrite the objects from previous chunks
  task = tsk("mtcars")
  learner = lrn("regr.lm")
  learner$train(task)
  prediction = learner$predict(task)
  autoplot(prediction)
})



# Resampling --------------------------------------------------------------

# see types of resampling
as.data.table(mlr_resamplings)

resampling = mlr_resamplings$get("holdout") # instatiated = FALSE means we haven't applied it yet
# or rsmp("holdout")

# to change split
resampling$param_set$values = list(ratio = 0.8)
# Or during creation
rsmp("holdout", ratio = 0.8)

# call method instantiate to 
resampling$instantiate(task)

resampling = rsmp("cv", folds = 3L)
resampling$instantiate(task2)
resampling$iters

# see which cases assigned 
str(resampling$train_set(1))
str(resampling$test_set(1))

# trying a change to one of the parameters
learner$param_set$values = list(max_depth = 3, nrounds = 5)
rr = resample(task2, learner, resampling, store_models = TRUE)
print(rr)

# see average accuracy across all resamples
rr$aggregate(msr("classif.acc"))

# or get it for each
rr$score(msr("classif.acc"))

# Or retrieve the learner of a specific iteration and inspect it:
lrn = rr$learners[[1]]
lrn$model

# get predictions
rr$prediction() # all predictions merged into a single Prediction - note: these are for training set
rr$predictions()[[1]] # prediction of first resampling iteration

# plotting
autoplot(rr)
autoplot(rr, type = "roc")  # nice !!


# Benchmarking ------------------------------------------------------------

# A design is a combination of learner, task and resampling

tasks = lapply(c("german_credit", "sonar"), tsk) # this is looking up existing tasks in ml3 dictionary; see this in as.data.table(mlr_tasks)
# not sure if you can add a task that covers more than one dataset; I assume not

design = benchmark_grid(
  tasks = task,
  learners = list(lrn("classif.rpart"), lrn("classif.featureless")),
  resamplings = rsmp("holdout")
)
print(design)

learners = lapply(learners, lrn,
                  predict_type = "prob", predict_sets = c("train", "test"))
# incomplete -= see below



# Trying to get feature importance ----------------------------------------
learner3 = lrn("classif.xgboost")
learner3$train(task2, row_ids = train_set)
learner3$model # can now access the model

learner3$importance() # shows feature importance

# According to: https://www.kaggle.com/c/homesite-quote-conversion/discussion/18669
# In the R version, only the features used in at least one split make it to the xgb.importance output. If the feature didn't make it to the output, then it must either be utterly useless, have zero variance, or be extremely or perfectly correlated to another feature before it.

learner3$importance() # can get importance this way
# or this 
importance = xgboost::xgb.importance(model = learner3$model)

# added learner3$importance() to the data table created by xgb.importance; seems like Gain and importance are the sam
importance[,importance := learner3$importance()]

# removing the column I just created
importance[, importance := NULL]

# I managed to create mean shap values by exluding the adm label which otherwise causes an error
shap_values = shap.values(learner3$model, task2$data()[train_set,2:367])

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
shap_long <- shap.prep(xgb_model = learner3$model, X_train = task2$data()[train_set,2:367])

# to see only the rows where mean score > 0
shap_gt0 = labels(shap_values$mean_shap_score[shap_values$mean_shap_score>0])
shap <- shap_long[shap_gt0]

shap.plot.summary(shap_long, dilute = 100)


# this took too long - then I found top_n - yay - but need to switch classes around! task$positive = "M"
shap.plot.summary.wrap1(model = learner3$model, X = task2$data()[train_set,2:367], top_n = 10)
