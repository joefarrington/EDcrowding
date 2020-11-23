## Following the steps in https://xgboost.readthedocs.io/en/latest/R-package/discoverYourData.html

library(xgboost)
library(Matrix)
library(data.table)
library(vcd)
library(SHAPforxgboost)
library(dplyr)
library(tidymodels)

load("~/EDcrowding/predict-admission/data-raw/matrix_60_2020-11-09.rda")


dm <- matrix_60 %>%
  filter(age >= 18, age < 110, epoch == "Post_Surge1") %>% 
  select(-mrn, -csn_old, -ED_duration_final
  ) %>% 
  select(adm, age, sex, visited_Waiting:visited_MAJORS, visited_UTC)
# select(adm, age, sex, quarter, day_of_week,
#        weekend, night, time_of_day, days_since_last_visit, 
#        num_prior_visits, num_ED_rows, visited_Waiting:visited_MAJORS, visited_UTC)

df <- data.table(dm, keep.rownames = FALSE)

head(df)
str(df)

# note sure if sparse matrix needed - is this just to save storage space? 
sparse_matrix <- sparse.model.matrix(adm~.-1, data = df)
output_vector = df[,adm] == TRUE


bst <- xgboost(data = sparse_matrix, label = output_vector, max.depth = 4,
               eta = 1, nthread = 2, max_delta_step = 0, nrounds = 10,objective = "binary:logistic")


# col names
sparse_matrix@Dimnames[[2]]


importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
head(importance)



importanceRaw <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst, data = sparse_matrix, label = output_vector)
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]

head(importanceRaw)


xgb.plot.importance(importance_matrix = importanceRaw)


# fromi https://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
str(train)

class(train$data)[1]
class(train$label)

# sparse matrix
bstSparse <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
bstDense <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")

# using a XGBoost DMatrix
dtrain <- xgb.DMatrix(data = train$data, label = train$label)
bstDMatrix <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")


# from https://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/


# from https://liuyanguu.github.io/post/2019/07/18/visualization-of-shap-for-xgboost/
# this works with a sparse matrix
shap_values <- shap.values(xgb_model = bst, X_train = sparse_matrix)
# but this doesn't work with a sparse matrix
shap_long <- shap.prep(xgb_model = bst, X_train = sparse_matrix)
# so try this - this just hung forever
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = sparse_matrix)



set.seed(123)
dm_split_train_test <- initial_split(dm, strata = adm, prop = 1/2)
dm_train <- training(dm_split_train_test)
dm_test <- testing(dm_split_train_test)

# note - I took sex out as its a dummy variable and I haven't coded it yet
df <- data.table(dm_train %>% select(-sex), keep.rownames = FALSE)

y_var <- "adm"

dataX <- as.matrix(df[,-..y_var])
output_vector = as.numeric(df[,adm] == TRUE) # making sure they are coded correctly

# according to xgboost help, set booster which booster to use, can be gbtree or gblinear. Default: gbtree.
bst <- xgboost(data = dataX, 
               label = output_vector, 
               booster = "gbtree", # alternative is gblinear
               max.depth = 4,
               eta = 1, 
               nthread = 2, 
               max_delta_step = 0, 
               nrounds = 10,
               objective = "binary:logistic")

shap_values <- shap.values(xgb_model = bst, X_train = dataX)
shap_long <- shap.prep(xgb_model = bst, X_train = dataX) # worked this time!

shap.plot.summary(shap_long, x_bound  = 1.2, dilute = 10)


# from https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
# I was looking for hot do do tuning and found this which only tunes by number of rounds
params <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

bstcv <- xgb.cv(data = dataX, 
               label = output_vector, 
               params = params, 
               nfold = 5, 
               showsd = T, 
               stratified = T, 
               print_every_n = 10, 
               early_stopping_rounds = 80,
               maximize = F,
               nrounds = 100)

# best iteration error was 18
min(bstcv$evaluation_log$test_error_mean)

# running on test set to check test set accuracy
bstcv_test <- xgb.train (params = params, data = dataX, 
                   label = output_vector, 
                   nrounds = 18, 
                   watchlist = list(val=dtest,train=dtrain), 
                   print_every_n = 10, 
                   early_stopping_rounds = 10,
                   maximize = F , 
                   eval_metric = "error")
