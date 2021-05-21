# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)


# Load tuning results  ---------------------------------------------------------------

model_date <- "2021-05-21"
model_features = "alop"
use_dataset = "Post"

scores_longfile <- paste0("~/EDcrowding/predict-admission/data-output/xgb_",model_features, "_scores_long", model_date,".rda")
load(scores_longfile)



# Report on class balance -------------------------------------------------


load("~/EDcrowding/predict-admission/data-raw/dm_2021-05-19.rda")
timeslices <- c("000", "015", "030", "060",  "090", "120", "180", "240", "300", "360", "480", "720")

class_balance = data.table()
for (ts_ in timeslices) {
  ts_bal = dm[duration > as.numeric(ts_), .N, by = .(a_epoch, adm)]
  ts_bal[, timeslice := ts_]
  class_balance = bind_rows(class_balance, ts_bal)
}

odds = class_balance %>% pivot_wider(names_from = adm, values_from = N) %>% mutate(odds = `1`/`0`)

# Pre covid
p_pre = class_balance %>% filter(a_epoch == "Pre") %>% 
  ggplot(aes(x = timeslice, y = N, fill = as.factor(adm))) + geom_bar(stat= "identity", position = "stack") + 
  theme(legend.position = "none") +
  labs(title = "Class balance for timeslices",
       subtitle = "Pre Covid")
odds_pre = odds %>% filter(a_epoch == "Pre")
for (i in 1:nrow(odds)) {
  p_pre = p_pre +  annotate(geom = "text", x = odds_pre$timeslice[i], y = 10000, label = round(odds_pre$odds[i],2), size = 4)
}
p_pre = p_pre + annotate(geom = "text", x = odds_pre$timeslice[1], y = 20000, label = "Odds of admission", size = 4, hjust = 0.1)
# p_pre

# Post covid
p_post = class_balance %>% filter(a_epoch == "Post") %>% 
  ggplot(aes(x = timeslice, y = N, fill = as.factor(adm))) + geom_bar(stat= "identity", position = "stack")  + 
  theme(legend.position = "bottom") +
  labs(subtitle = "Post Covid",
       fill = "Admitted")
  
odds_post = odds %>% filter(a_epoch == "Post")
for (i in 1:length(timeslices)) {
  p_post = p_post +  annotate(geom = "text", x = odds_post$timeslice[i], y = 10000, label = round(odds_post$odds[i],2), size = 4)
}
p_post = p_post + annotate(geom = "text", x = odds_post$timeslice[1], y = 20000, label = "Odds of admission", size = 4, hjust = 0.1)
# p_post

# library(gridExtra)
grid.arrange(p_pre, p_post, nrow = 2)

# Evaluate nrounds --------------------------------------------------------


print ("Best nrounds results:")
scores <- data.table(scores)
scores[, dataset := factor(dataset, levels = c("Pre", "Post", "Pre + Post"))]
print(scores[tsk_ids == "val" & tuning_round == "nrounds" & model_features == model_features & dataset == use_dataset
             , .SD[which.min(logloss)], by = list(timeslice)][,.(timeslice, nrounds)])

scores_long = data.table(scores[tsk_ids == "val" & tuning_round == "nrounds" & model_features == model_features, 
       .(dataset, timeslice, logloss, prauc, auc, nrounds)] %>%
  pivot_longer(c("logloss", "prauc", "auc"), names_to = "metric", values_to = "score"))

order_scores = c("dataset", "timeslice", "metric", "nrounds")
setorderv(scores_long, order_scores)
scores_long[, gt0.01 := score - lag(score) > .01, by = .(dataset, timeslice, metric)]
scores_long[, lt0.01 := lag(score) - score > .01, by = .(dataset, timeslice, metric)]
scores_long[, gt0.005 := score - lag(score) > .005, by = .(dataset, timeslice, metric)] # for metrics where positive is good
scores_long[, lt0.005 := lag(score) - score > .005, by = .(dataset, timeslice, metric)] # for metrics where negative is good


scores_long %>%  filter(metric == "logloss") %>%
  ggplot(aes(x = nrounds, y = score)) + geom_line() + geom_point( aes(col = lt0.005)) + facet_grid(dataset ~ timeslice) +
  labs(y = "Score", title = "Results of tuning nrounds of XGBoost for each timeslice: logloss",
       col = "Greater than 0.05 decrease in log loss") +
  theme(legend.position = "bottom")

scores_long %>%  filter(metric == "prauc") %>%
  ggplot(aes(x = nrounds, y = score)) + geom_line() + geom_point( aes(col = gt0.005)) + facet_grid(dataset ~ timeslice) +
  labs(y = "Score", title = "Results of tuning nrounds of XGBoost for each timeslice: prauc",
       col = "Greater than 0.05 increase in area under precision recall curve") +
  theme(legend.position = "bottom")


# explore best nrounds which offer logloss delta of more than 0.05
scores_long[(lt0.005 | is.na(lt0.005)) & metric == "logloss", .SD[which.min(score)], 
            by = .(dataset, timeslice)]

# this suggests I can cap nrounds at 30 


