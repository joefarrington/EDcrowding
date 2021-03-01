# Create functions --------------------------------------------------------


madcap = function(pred){
  # ref to MADCAP section in Model Evaluation.docx
  
  pred[, truth := as.numeric(truth)]
  pred[, truth := if_else(truth == 2, 0, truth)]  
  new_pred = pred[order(pred$prob.1),] # step 1
  
  L = nrow(new_pred)
  
  y1 = c()
  y2 = c()
  x = c(1:L)
  model = 0 
  real = 0
  for (i in 1:L){ # step 2
    
    # s = new_pred[1:i,]
    # print(new_pred$.pred_TRUE[i])
    model = model + new_pred$prob.1[i] # step 4: getting the expectation from predictions
    # step 3 not actually necessary,
    real = real + as.numeric(new_pred$truth[i])
    y1 = c(y1, model)
    y2 = c(y2, real)
    
  }
  
  return(data.frame(x=x,y1=y1,y2=y2))
}


# Load data ---------------------------------------------------------------

# Use run_MLR.R to generate dm000p datasets and tasks

load("~/EDcrowding/predict-admission/data-output/scores_table.rda")

# create learner
learner = lrn("classif.xgboost", predict_type = "prob")

learner$param_set$values = mlr3misc::insert_named(
  learner$param_set$values,
  list(
    "early_stopping_rounds" = 10,
    "eval_metric" = "auc"
  )
)




# Predict using optimised parameters ---------------------------------
all_madcap = data.table()

for (ts_ in timeslices) {
  name_tsk <- paste0("task", ts_)
  tsk = get(name_tsk)
  tsk_train_ids = get(paste0(name_tsk, "_train_ids"))
  tsk_val_ids = get(paste0(name_tsk, "_val_ids"))
  
  learner$param_set$values = mlr3misc::insert_named(
    learner$param_set$values,
    list(
      "scale_pos_weight" = as.numeric(scores_table[(model == name_tsk & tuning_round == "recal_nr"), 
                                                   .SD[which.max(val_auc)], by=.(model)][,.(scale_pos_weight)]),
      "nrounds" = as.numeric(scores_table[(model == name_tsk & tuning_round == "recal_nr"), 
                                          .SD[which.max(val_auc)], by=.(model)][,.(nrounds)]),
      "max_depth" = as.numeric(scores_table[(model == name_tsk & tuning_round == "recal_nr"), 
                                            .SD[which.max(val_auc)], by=.(model)][,.(max_depth)]), 
      "min_child_weight" = as.numeric(scores_table[(model == name_tsk & tuning_round == "recal_nr"), 
                                                   .SD[which.max(val_auc)], by=.(model)][,.(min_child_weight)]),
      "gamma" = as.numeric(scores_table[(model == name_tsk & tuning_round == "recal_nr"), 
                                        .SD[which.max(val_auc)], by=.(model)][,.(gamma)])
    )
  )
  
  # train learner on training set
  learner$train(tsk, row_ids = tsk_train_ids)
  
  # score predictions on training and validation set
  pred_val = learner$predict(tsk, row_ids = tsk_val_ids)
  name_pred_val <- paste0("pred_val", ts_)
  assign(name_pred_val, as.data.table(pred_val))
  
  # get roc plot
  
  name_roc <- paste0("plot_roc", ts_)
  assign(name_roc, autoplot(pred_val, type = "roc") + labs(title = paste0("ROC curve for timeslice ", ts_)))
  
  # get data for madcap
  
  mc_result = as.data.table(madcap(as.data.table(pred_val)))
  mc_result[, model := ts_]
  
  all_madcap = bind_rows(all_madcap, mc_result)
  
} 



for (ts_ in timeslices) {
  
}

# single madcap plot
pred_val060 = pred_val060[order(pred_val060$prob.1),]
mc_result = madcap(pred_val060[3001:nrow(pred_val060),])
ggplot(mc_result, aes(x))+ 
  geom_line(aes(y = y1, colour = "model"), size = 1) +
  geom_line(aes(y = y2, colour = "data"), size = 1) +
              scale_color_manual(breaks = c('model','data'), values = c('model'='red','data'='black')) + 
              labs(x='No. of patients (ordered by risk factor)',y='Number of admissions', 
                   title = paste0("Madcap plot for timeslice ", as.numeric(ts_))) +
              theme(legend.title = element_blank())


# all timeslices
all_madcap %>% ggplot(aes(x))+ 
  geom_line(aes(y = y1, colour = "model"), size = 2) +
  geom_line(aes(y = y2, colour = "data"), size = 2) +
  scale_color_manual(breaks = c('model','data'), values = c('model'='red','data'='black')) + 
  labs(x='No. of patients (ordered by risk factor)',y='Number of admissions', 
       title = paste0("Madcap plot for timeslice ")) +
  theme(legend.title = element_blank()) + 
  facet_wrap(vars(model), nrow = 3, ncol = 3, scales = "free")



# Look at individual predictions ------------------------------------------



# from http://ema.drwhy.ai/shapley.html

library(DALEX)

model_ = learner$train(tsk, row_ids = tsk_train_ids)

data_ = dm360p[tsk_val_ids]
data_[, adm:= NULL]
y_ = dm360[tsk_val_ids, adm]


# create explainer
explain_ <- DALEX::explain(model = model_,  
                             data = data_,
                             y = y_) # note y_ has value 1 for admitted and 2 for discharged


pred = as.data.table(learner$predict(tsk, row_ids = tsk_val_ids))
# pred = bind_cols(pred, dm30p[tsk_val_ids])


# understand predictions for a particular case
bd <- predict_parts(explainer = explain_,
                       new_observation = data_[140,],
                       type = "break_down")
plot(bd)

imp <- data.table(learner$importance())
imp$feature <- names(learner$importance())





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

# Plots of results --------------------------------------------------------


scores_table %>% filter(param_ != "max_depth") %>% 
  mutate(param_ = factor(param_, levels = c("base", "nrounds", "min_child_weight"))) %>% 
  group_by(model, param_) %>% summarise(max_ = max(val_auc)) %>% 
  ggplot(aes(col = model, y = max_, x = param_, group = model)) + geom_line() +
  facet_grid(. ~ model)



# I managed to create mean shap values by exluding the adm label which otherwise causes an error
shap_values = shap.values(learner$model, dm360p[tsk_train_ids, 3:ncol(dm360p)])

shap_tibble <- as_tibble(labels(shap_values$mean_shap_score)) %>% rename(Feature = value) %>% 
  bind_cols(Mean_Shap = shap_values$mean_shap_score)

importance %>% left_join(shap_tibble) %>% 
  pivot_longer(Gain:Mean_Shap, names_to = "importance_type", values_to = "values") %>% 
  mutate(Feature = fct_reorder(Feature, values)) %>% 
  ggplot(aes(x = Feature, y = values, fill = importance_type)) + geom_bar(stat = "identity") +
  facet_wrap( ~ importance_type) + coord_flip() + theme(legend.position = "none") +
  labs(title = "Model importances for 60 min timeslice excluding admission characteristics - Post Surge1")


# train leaner on task

pred = as.data.table(learner$predict(tsk, row_ids = tsk_val_ids))
x = merge(dm360p[tsk_val_ids], pred, by = row_id)