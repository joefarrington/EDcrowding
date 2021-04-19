
# Evaluate distributions created by Joe and Ken -----------------------------------------------

# Take input of predictions of admission numbers from the location level ML
# and examines the distribution of probabilities



# Functions ---------------------------------------------------------------

get_random_dttm <- function(dttm_start, dttm_end) {
  dt <- as.numeric(difftime(dttm_end, dttm_start,unit="sec"))
  increment <- runif(1, 0, dt)
  return(dttm_start + increment)
}

# Libraries ---------------------------------------------------------------


library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)
library(polynom)
library(readr)


# Load data ---------------------------------------------------------------



pred_dist <- read_csv("~/EDcrowding/data-prep-for-ML/data-output/df_y_predicted2.csv",
                            col_types = cols(X1 = col_skip()))
pred_dist <- read_csv("~/EDcrowding/data-prep-for-ML/data-output/df_y_test1_dummy.csv",
                      col_types = cols(X1 = col_skip()))
fcnn_neg_bin_probs <- read_csv("~/EDcrowding/data-prep-for-ML/data-output/2021-03-18_fcnn_neg_bin_probs.csv")
ngboost_normal_probs <- read_csv("~/EDcrowding/data-prep-for-ML/data-output/2021-03-18_ngboost_normal_probs.csv")

# for Ken's file
max_predicted <- as.numeric(colnames(pred_dist)[length(colnames(pred_dist))])
seq_nums = seq(0, max_predicted, 1)
seq_nums = paste0("adm_", seq_nums)
colnames(pred_dist) <- c("dttm", seq_nums)
pred_dist <- data.table(pred_dist %>% mutate(dttm = as.POSIXct(dttm)))

# get actual admissions in next four hours
loc_nums <- data.table(read_csv(
  "~/EDcrowding/data-prep-for-ML/data-output/before_covid_num_in_location_2021-02-16.csv"))
setnames(loc_nums, "DateTime", "dttm")
loc_nums[, dttm := as.POSIXct(dttm)]
loc_nums = loc_nums[dttm >= min(pred_dist$dttm)]
loc_nums[, actual := lead(adm, n = 1) + lead(adm, n = 2) + lead(adm, n = 3) + lead(adm, n = 4)]


# checking that Ken's y_true aligns
loc_nums_k <- data.table(read_csv(
  "~/EDcrowding/data-prep-for-ML/data-output/y_test_true.csv"))
loc_nums_k <- loc_nums_k %>% as_tibble(loc_nums_k) 
colnames(loc_nums_k) <- c("X1", seq_nums)
loc_nums_k %>% pivot_longer(adm_0: adm_25) %>% filter(value == 1)

# plot of actual admissions
loc_nums[dttm > min(pred_dist$dttm)] %>% ggplot(aes(x = dttm, y = actual)) + geom_line()

ken = TRUE
joe = FALSE

# Read data for Joe's file ------------------------------------------------


# for Joe's file
pred_dist <- fcnn_neg_bin_probs
pred_dist <- ngboost_normal_probs
max_predicted <- as.numeric(colnames(pred_dist)[length(colnames(pred_dist))-1])
seq_nums = seq(0, max_predicted, 1)
seq_nums = paste0("adm_", seq_nums)
colnames(pred_dist) <- c("dttm", seq_nums, "actual")
pred_dist <- data.table(pred_dist %>% mutate(dttm = as.POSIXct(dttm)))

loc_nums = pred_dist[, .(dttm, actual)]

ken = FALSE
joe = TRUE

# Get a sample of time points of interest ---------------------------------

set.seed(17L)
time_pts <- get_random_dttm(min(pred_dist$dttm), min(pred_dist$dttm) + hours(12))
last_pt <- time_pts

while (last_pt + hours(12) < max(pred_dist$dttm)) {
  next_pt <- get_random_dttm(last_pt, last_pt + hours(12))
  time_pts <- c(time_pts, next_pt)
  last_pt <- next_pt
  
}

# or use whole sample
time_pts <- seq(min(pred_dist$dttm), max(pred_dist$dttm), 60*60)


# Quick plots to look at the distribution ---------------------------------------------------------------

# get row with latest dttm less than  the sampled
d = pred_dist[pred_dist$dttm < time_pts[1], ]
d = pred_dist[pred_dist$dttm < "2020-01-14 20:00:01 GMT", ]
d = d[nrow(d),]

# quick plot of distributions
p1 = d[nrow(d),] %>% pivot_longer(adm_0:adm_25) %>%
  mutate(name = as.numeric(substr(name, 5, nchar(name)))) %>% ggplot(aes(x = name, y = value)) + geom_line() +
  geom_vline(xintercept = 18, linetype="dashed", color = "red") + 
  labs(title = "Probability - randomly chosen timepoint 2020-01-14 20:00:00",
       subtitle = "Actual number of admissions was 18",
       x = "Number of admissions",
       y = "Probability")

p3 = d[nrow(d),] %>% pivot_longer(adm_0:adm_25) %>%
  mutate(name = as.numeric(substr(name, 5, nchar(name)))) %>% ggplot(aes(x = name, y = cumsum(value))) + geom_line() +
  geom_vline(xintercept = 18, linetype="dashed", color = "red") + 
  labs(title = "CDF - randomly chosen timepoint 2020-01-14 20:00:00",
       subtitle = "Actual number of admissions was 18",
       x = "Number of admissions",
       y = "Cumulative probability distribution")


# get row with latest dttm less than the sampled maximum
d = pred_dist[pred_dist$dttm < time_pts[257], ]
d = pred_dist[pred_dist$dttm < "2020-03-18 08:35:52 GMT", ]
d = d[nrow(d),]

# quick plot of distributions
p2 = d[nrow(d),] %>% pivot_longer(adm_0:adm_25) %>%
  mutate(name = as.numeric(substr(name, 5, nchar(name)))) %>% ggplot(aes(x = name, y = value)) + geom_line() +
  geom_vline(xintercept = 3, linetype="dashed", color = "red") + 
  labs(title = "Probability - randomly chosen timepoint 2020-03-18 08:35:52",
       subtitle = "Actual number of admissions was 3",
       x = "Number of admissions",
       y = "Probability")

# quick plot of distributions
p4 = d[nrow(d),] %>% pivot_longer(adm_0:adm_25) %>%
  mutate(name = as.numeric(substr(name, 5, nchar(name)))) %>% ggplot(aes(x = name, y = cumsum(value))) + geom_line() +
  labs(title = "CDF - randomly chosen timepoint 2020-03-18 08:35:52",
       subtitle = "Actual number of admissions was 3",
       x = "Number of admissions",
       y = "Cumulative probability distribution")


library("gridExtra")
grid.arrange(p1, p2, p3, p4 +   geom_vline(xintercept = 3, linetype="dashed", color = "red"),
             ncol = 2, nrow = 2)

# to get example plot to demonstrate the method

# looking at distr to get the most recent set of probs; actual number admitted is 

p4 + coord_flip() + 
  geom_vline(xintercept = 3, linetype="dashed", color = "red") + 
  labs(y = "values of alpha on CDF", title = "Flipped CDF - randomly chosen timepoint 2020-03-18 08:35:52",
       subtitle = "Actual number of admissions was 3 (red line); dark rectangles show spread of model cdf and predicted admissions at each alpha") +
  scale_y_continuous(minor_breaks = seq(0, 1, .05)) +
  # geom_hline(yintercept = .05, colour = "blue") +
  # annotate("text", y = .1, x = 10, label = "At alpha = .05, model lower = 1; upper = 2 \nActual adm is not less than this", colour = "blue") +
  # geom_hline(yintercept = .75, colour = "red") +
  # # geom_hline(yintercept = seq(.05, 1, .05), colour = "grey", linetype="dashed") +
  # annotate("text", y = .75, x = 20, label = "At alpha = .75, model lower = 7; upper = 8 \nActual adm is less than this", colour = "red") +
  
  geom_rect(aes(ymin = 0.0369, ymax = 0.0909, xmin= 1, xmax = 2)) +
  # geom_vline(xintercept = c(1, 2), color = "blue", linetype="dashed") + 
  geom_rect(aes(ymin = 0.0909, ymax = 0.189 , xmin= 2, xmax = 3)) +
  geom_rect(aes(ymin = 0.189, ymax = 0.331 , xmin= 3, xmax = 4)) +
  geom_rect(aes(ymin = 0.331, ymax = 0.476 , xmin= 4, xmax = 5)) +
  geom_rect(aes(ymin = 0.476, ymax = 0.592 , xmin= 5, xmax = 6)) +
  geom_rect(aes(ymin = 0.592, ymax = .677 , xmin= 6, xmax = 7)) +
  geom_rect(aes(ymin = .677, ymax = .752 , xmin= 7, xmax = 8)) +
  # geom_vline(xintercept = c(7, 8), color = "blue", linetype="dashed") + 
  geom_rect(aes(ymin = 0.752, ymax = .805 , xmin= 8, xmax = 9)) +
  geom_rect(aes(ymin = 0.841, ymax = .872 , xmin= 10, xmax = 11)) +
  geom_rect(aes(ymin = 0.891, ymax = .906 , xmin= 12, xmax = 13)) +
  geom_rect(aes(ymin = 0.942, ymax = .952 , xmin= 16, xmax = 17)) +
  geom_hline(yintercept = .35, colour = "black") +
  annotate("text", y = .355, x = 25, label = "Actual num admitted\nis <  model lower \nfor all alpha \n >= 0.35", hjust = 0 , vjust = 1) +
  geom_hline(yintercept = .2, colour = "black") +
  annotate("text", y = .205, x = 20, label = "Actual num admitted\nis < model upper \nfor all alpha \n >= 0.20", colour = "black", hjust = 0)




# Generate array of predicted probabilities --------------------------------


# 
# a = loc_nums[loc_nums$dttm < time_pts[1] , ]
# a[nrow(a), actual]

distr_coll = data.table()
adm_coll = data.table()

for (i in (1:length(time_pts))) {
  
  # get latest row with dttm less than the sampled date time
  d = pred_dist[pred_dist$dttm < time_pts[i], ]
  d = d[nrow(d),]

  num_adm = seq(0, max_predicted, 1)# make an array from 0 admissions to max admissions (ie all patients admitted)
  
  if (ken)  {
    distr = bind_cols(sample_time = time_pts[i], num_adm_pred = num_adm, 
                      probs = as.numeric(d[, adm_0:adm_25]), # adding 1 to max_predicted to reference last col of d
                      cdf = cumsum(as.numeric(d[, adm_0:adm_25])))
  } else if (joe) {
    distr = bind_cols(sample_time = time_pts[i], num_adm_pred = num_adm, 
                      cdf = as.numeric(d[, adm_0:adm_25]))
  }

  
  
  
  distr_coll = bind_rows(distr_coll, distr)
  
  
  # get number admitted in following four hours 
  a = loc_nums[loc_nums$dttm < time_pts[i] , ] 

  
  num_adm = bind_cols(sample_time = time_pts[i],  num_adm = a[nrow(a), actual])
  
  adm_coll = bind_rows(adm_coll, num_adm)
  
}


# Identify where the cutoff points on the cdf are -------------------------





# now treat the full set of cdfs (all time points) as a single discrete variable
distr_coll[, upper_M_discrete_value := cdf]
distr_coll[, lower_M_discrete_value := lag(cdf), by = sample_time]
distr_coll[num_adm_pred == 0, lower_M_discrete_value := 0]

# for Joe's data add probability back in
if (joe) {
  distr_coll[, probs := cdf - lag(cdf), by = sample_time]
  distr_coll[is.na(probs), probs := cdf]
}


# for the array of low cdf values (now considered a discrete distribution) work out its cdf

lower_M = distr_coll[, .(value = lower_M_discrete_value), probs]
setorder(lower_M, value)
lower_M[, cum_weight := cumsum(probs)]
lower_M[, cum_weight_normed := cumsum(probs)/length(time_pts)]
lower_M[, dist := "model lower"]

# same for high cdf values

upper_M = distr_coll[, .(value = upper_M_discrete_value), probs]
setorder(upper_M, value)
upper_M[, cum_weight := cumsum(probs)]
upper_M[, cum_weight_normed := cumsum(probs)/length(time_pts)]
upper_M[, dist := "model upper"]

# same for mid point

mid_M = distr_coll[, .(value = (upper_M_discrete_value+lower_M_discrete_value)/2, probs)]
setorder(mid_M, value)
mid_M[, cum_weight := cumsum(probs)]
mid_M[, cum_weight_normed := cumsum(probs)/length(time_pts)]
mid_M[, dist := "model mid"]


# compare the observed values against their predicted distribution 
# and find their position on the cdf; combine this into a distribution

adm_coll = prob_dist[[2]]


adm_coll = merge(adm_coll, 
                 distr_coll[, .(sample_time, num_adm = num_adm_pred, 
                                lower_E = lower_M_discrete_value, 
                                upper_E = upper_M_discrete_value)], 
                 by = c("sample_time", "num_adm"))

# for the array of low cdf values (now considered a discrete distribution) work out its cdf

lower_E = adm_coll[, .(value = lower_E)]
setorder(lower_E, value)
lower_E_prob = lower_E[, .N, by = value]
lower_E_prob[, cum_weight := N/length(time_pts)]
lower_E_prob[, cum_weight_normed := cumsum(cum_weight)]
lower_E_prob[, dist := "actual lower"]

# same for high cdf values

upper_E = adm_coll[, .(value = upper_E)]
setorder(upper_E, value)
upper_E_prob = upper_E[, .N, by = value]
upper_E_prob[, cum_weight := N/length(time_pts)]
upper_E_prob[, cum_weight_normed := cumsum(cum_weight)]
upper_E_prob[, dist := "actual upper"]

# same for mid point

mid_E = adm_coll[, .(value = (upper_E+lower_E)/2)]
setorder(mid_E, value)
mid_E_prob = mid_E[, .N, by = value]
mid_E_prob[, cum_weight := N/length(time_pts)]
mid_E_prob[, cum_weight_normed := cumsum(cum_weight)]
mid_E_prob[, dist := "acutal mid"]



# Plot chart --------------------------------------------------------------



plot_data = bind_rows(lower_M, mid_M, upper_M, lower_E_prob, mid_E_prob, upper_E_prob)
plot_data %>% ggplot(aes(x = value, y = cum_weight_normed, colour = dist)) + geom_point() +
  labs(title = "Proportion of demand values <= threshold X on predicted cdf - Joe's ngboost_probs",
       x = "X",
       y = NULL) +
  theme_classic() +
  scale_color_manual(values = c("deeppink" , "chartreuse4" ,  "lightblue","black","black", "black", guide = NULL, name = NULL)) +
  theme(legend.position = "none") 
