


# Exploring RF, I read here that RF is very sensitive to correlated predictors
# https://brunaw.com/slides/rladies-dublin/RF/intro-to-rf.html#31
cols = colnames(dt)[grep("^o_", colnames(dt))]

corrplot::corrplot(cor(dt[, ..cols] %>% select_if(is.numeric),
method = "spearman"))



# Exploring learner parameters

learner = lrn("classif.ranger", predict_type = "prob", num.trees = 50)
learner$param_set




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

# see class imbalance
table(tsk$truth())
sum(tsk$truth() == 1)/sum(tsk$truth() != 1)



# https://mlr3gallery.mlr-org.com/posts/2020-03-30-imbalanced-data/

library("mlr3pipelines") # create ML pipelines
# undersample majority class (relative to majority class)
po_under = po("classbalancing",
              id = "undersample", adjust = "major",
              reference = "major", shuffle = FALSE, ratio = sum(tsk$truth() == 1)/sum(tsk$truth() != 1))
# reduce majority class by factor '1/ratio'
table(po_under$train(list(tsk))$output$truth())


lrn_under = GraphLearner$new(po_under %>>% learner)

