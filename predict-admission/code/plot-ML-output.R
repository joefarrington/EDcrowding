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

scores_file <- paste0("~/EDcrowding/predict-admission/data-output/scores_",today(),".rda")
load(scores_file)

preds_file <- paste0("~/EDcrowding/predict-admission/data-output/preds_",today(),".rda")
load(preds_file)



# Plot madcap  ---------------------------------

all_madcap = data.table()

preds <- preds[tuning_round == "nrounds" & dttm > '2021-02-22 11:00:09']

for (ts_ in timeslices) {
  name_tsk <- paste0("task", ts_)

    # score predictions on training and validation set
  pred_val = preds[timeslice == name_tsk & tsk_ids == "all"]
  
  # get data for madcap
  
  mc_result = as.data.table(madcap(pred_val))
  mc_result[, model := ts_]
  
  # mc_result[, distribution := case_when(x < nrow(mc_result)/3 ~ "lower",
  #                                       x > nrow(mc_result)*2/3 ~ "upper",
  #                                       TRUE ~ "middle")]
  
  all_madcap = bind_rows(all_madcap, mc_result)
  
} 




# all timeslices
all_madcap %>% ggplot(aes(x))+ 
  geom_line(aes(y = y1, colour = "model"), size = 1) +
  geom_line(aes(y = y2, colour = "data"), size = 1) +
  scale_color_manual(breaks = c('model','data'), values = c('model'='red','data'='black')) + 
  labs(x='No. of patients (ordered by risk factor)',y='Number of admissions', 
       title = paste0("Madcap plot for timeslice")) +
  theme(legend.title = element_blank()) + 
  facet_wrap(vars(model), scales = "free")





# Scores summary ----------------------------------------------------------


s = scores[tsk_ids == "val" & model_features == "alop" & 
         tuning_round == "tune_samples",
       .SD[which.min(logloss)], by = list(timeslice)]
s[, timeslice := gsub("task", "", timeslice)]

s %>% pivot_longer(logloss:auc) %>% ggplot(aes(x = timeslice, y = value, colour = name, group = name)) + geom_line(size = 2) + geom_point(size = 4) +
  theme_classic(base_size = 22) +
  labs(title = "Scores on validation set", 
       colour = "Metric",
       y = "Score")


# Table of a few features importances -------------------------------------


i = imps[feature %in% c("a_age", "a_arrival_Ambulance", "a_num_prior_adm_after_ED", 
                    "l_visited_MAJORS", 
                    "o_num_news_med"), .(importance, feature, timeslice)] %>% 
  mutate(timeslice = gsub("task","", timeslice)) %>% 
  pivot_wider(names_from = timeslice, values_from = importance)



# Plot feature importances  ------------------------
imps[, feature_group := substr(feature, 1,2)]

# all
imps = imps[dttm > '2021-03-23 09:45:09' & !feature %in% c("a_epoch_Post_surge1",
                                                           "a_epoch_Pre_Covid",
                                                           "a_epoch_Surge1", 
                                                           "a_epoch_Surge2")]
imps[, count := .N, by = feature]
imps[tsk_ids == "all" & !feature %in% c("a_quarter_1", "a_quarter_2", "a_quarter_3", "a_quarter_4",
                                                 "a_tod_1", "a_tod_2", "a_tod_3", "a_tod_4", "a_tod_5", "a_tod_6")] %>% 
  ggplot(aes(x = gsub("task","", timeslice), y = reorder(feature, desc(feature)), fill = importance)) + geom_tile() +
  scale_fill_gradient(low="white", high="red") +
  labs(title = "Feature importances by timeslice",
       fill = "Importance",
       x = "Timeslice",
       y = "Feature")

# by timeslice
imps[!feature %in% c("a_quarter_1", "a_quarter_2", "a_quarter_3", "a_quarter_4",
                                        "a_tod_1", "a_tod_2", "a_tod_3", "a_tod_4", "a_tod_5", "a_tod_6")] %>% 
  mutate(timeslice = gsub("task","", timeslice)) %>% 
  ggplot(aes(x = 1, y = reorder(feature, desc(feature)), fill = importance)) + geom_tile() +
  scale_fill_gradient(low="white", high="red") +
  labs(title = "Feature importances by timeslice",
       fill = "Importance",
       x = "Timeslice",
       y = "Feature") +
  facet_grid(. ~ timeslice) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# 

imps = imps[!grep("l_current", feature)]
# by timeslice, just arrival and location chars
imps[!feature %in% c("a_quarter_1", "a_quarter_2", "a_quarter_3", "a_quarter_4",
                                               "a_tod_1", "a_tod_2", "a_tod_3", "a_tod_4", "a_tod_5", "a_tod_6") &
       feature_group %in% c("a_", "l_") ] %>% 
  mutate(timeslice = gsub("task","", timeslice)) %>% 
  ggplot(aes(x = 1, y = reorder(feature, desc(feature)), fill = importance)) + geom_tile() +
  scale_fill_gradient(low="white", high="red") +
  labs(title = "Feature importances by timeslice - arrival and location features only",
       fill = "Importance",
       x = "Timeslice",
       y = "Feature") +
  facet_grid(. ~ timeslice) +
  theme_classic(base_size = 20) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# by timeslice, just location
imps[!feature %in% c("a_quarter_1", "a_quarter_2", "a_quarter_3", "a_quarter_4",
                     "a_tod_1", "a_tod_2", "a_tod_3", "a_tod_4", "a_tod_5", "a_tod_6") &
       feature_group %in% c("l_")] %>% 
  mutate(timeslice = gsub("task","", timeslice)) %>% 
  ggplot(aes(x = 1, y = reorder(feature, desc(feature)), fill = importance)) + geom_tile() +
  scale_fill_gradient(low="white", high="red", limits = c(0, .301)) +
  labs(title = "Feature importances by timeslice",
       fill = "Importance",
       x = "Timeslice",
       y = "Feature") +
  facet_wrap(. ~ timeslice, ncol = 11) +
  theme_classic(base_size = 20) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# by timeslice, just obs
imps[!feature %in% c("a_quarter_1", "a_quarter_2", "a_quarter_3", "a_quarter_4",
                     "a_tod_1", "a_tod_2", "a_tod_3", "a_tod_4", "a_tod_5", "a_tod_6") &
       feature_group %in% c("o_")] %>% 
  mutate(timeslice = gsub("task","", timeslice)) %>% 
  ggplot(aes(x = 1, y = reorder(feature, desc(feature)), fill = importance)) + geom_tile() +
  scale_fill_gradient(low="white", high="red", limits = c(0, .301)) +
  labs(title = "Feature importances by timeslice - observations only",
       fill = "Importance",
       x = "Timeslice",
       y = "Feature") +
  facet_grid(. ~ timeslice) +
  theme_classic() +
  theme_classic(base_size = 20) +
  
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# by timeslice, just labs
imps[!feature %in% c("a_quarter_1", "a_quarter_2", "a_quarter_3", "a_quarter_4",
                     "a_tod_1", "a_tod_2", "a_tod_3", "a_tod_4", "a_tod_5", "a_tod_6") &
       feature_group %in% c("p_")] %>% 
  mutate(timeslice = gsub("task","", timeslice)) %>% 
  ggplot(aes(x = 1, y = reorder(feature, desc(feature)), fill = importance)) + geom_tile() +
  scale_fill_gradient(low="white", high="red", limits = c(0, .301)) +
  labs(title = "Feature importances by timeslice - pathology only",
       fill = "Importance",
       x = "Timeslice",
       y = "Feature") +
  facet_grid(. ~ timeslice) +
  theme_classic() +
  theme_classic(base_size = 20) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# Plot feature importances relative to eacah other ------------------------


imps[, importance2 := importance/max(importance), by = timeslice]

# by timeslice, just labs
imps[!feature %in% c("a_quarter_1", "a_quarter_2", "a_quarter_3", "a_quarter_4",
                     "a_tod_1", "a_tod_2", "a_tod_3", "a_tod_4", "a_tod_5", "a_tod_6") &
       feature_group %in% c("p_")] %>% 
  mutate(timeslice = gsub("task","", timeslice)) %>% 
  ggplot(aes(x = 1, y = reorder(feature, desc(feature)), fill = importance2)) + geom_tile() +
  scale_fill_gradient(low="white", high="red") +
  labs(title = "Feature importances by timeslice - pathology only",
       fill = "Importance",
       x = "Timeslice",
       y = "Feature") +
  facet_grid(. ~ timeslice) +
  theme_classic() +
  theme_classic(base_size = 16) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



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