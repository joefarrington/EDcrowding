
# Process Ken's predictions -----------------------------------------------

# Take input of predictions of admission numbers from the location level ML
# and examines the distribution of probabilities

# The input data is a set of probabilities of N patients being admitted in the next 4 hours
# where N ranges from 0 to 25

# First step is to read data and check that actual ys are the same as Ken was given

# second step is to generate a random sample of timestamps

# Then, for each sample,
# - find the nearest hour in Ken's matrix of predictions
# - retrieve the cumulative probabilities and the actual number admitted

# Then for each sampled timestamps in turn, do the following:
# - for each of 20 points along the cdf from 0 to 1, starting at 0.05, referred to as alpha, do the following:
# - - find the number of admissions that have a cumulative probability of less than alpha (this is the model's lower number of admissions at that value of alpha)
# - - add one to this (to get the model's upper number of admissions at that value of alpha)
# - - record the model's cdf at the model's lower number of admissions 
# - - record the model's cdf at the model's higher number of admissions (this and the one above lie either side of each alpha)
# - - record whether the actual number of admissions for that timestamp was less than the model's lower at that value of alpha (True or False)
# - - record whether the actual number of admissions for that timestamp was more than the model's lower at that value of alpha (True or False)

# Then take the mean for all sample timestamps, by doing the following:
# - for each of 20 values of alpha
# - - calculate the mean model's cdf at the model's lower number of admissions below that value of alpha 
# - - calculate the mean model's cdf at the model's higher number of admissions above that value of alpha 
# - - calculate the proportion of days when the actual number of admissions for that timestamp was less than the model's lower limit at that level of alpha
# - - calculate the proportion of days when the actual number of admissions for that timestamp was less than the model's upper limit at that level of alpha

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

# Get a sample of time points of interest ---------------------------------



set.seed(17L)
time_pts <- get_random_dttm(min(pred_dist$dttm), min(pred_dist$dttm) + hours(12))
last_pt <- time_pts

while (last_pt + hours(12) < max(pred_dist$dttm)) {
  next_pt <- get_random_dttm(last_pt, last_pt + hours(12))
  time_pts <- c(time_pts, next_pt)
  last_pt <- next_pt
  
}


# Quick plots to look at the distribution ---------------------------------------------------------------

# get row with latest dttm less than  the sampled
d = pred_dist[pred_dist$dttm < time_pts[1], ]
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
  annotate("text", y = .35, x = 25, label = "Actual num admitted\nis <=  model lower \nfor all alpha \n below 0.35", hjust = 1, vjust = 1) +
  geom_hline(yintercept = .2, colour = "black") +
  annotate("text", y = .2, x = 20, label = "Actual num admitted\nis <= model upper \nfor all alpha \n below 0.2", colour = "black", hjust = 1)




# Generate array of predicted probabilities --------------------------------



a = loc_nums[loc_nums$dttm < time_pts[1] , ]
a[nrow(a), actual]

distr_coll = data.table()
adm_coll = data.table()

for (i in (1:length(time_pts))) {
  
  # get latest row with dttm less than the sampled date time
  d = pred_dist[pred_dist$dttm < time_pts[i], ]
  d = d[nrow(d),]

  num_adm = seq(0, max_predicted, 1)# make an array from 0 admissions to max admissions (ie all patients admitted)
  distr = bind_cols(sample_time = time_pts[i], num_adm_pred = num_adm, 
                    probs = as.numeric(d[, adm_0:adm_25]), # adding 1 to max_predicted to reference last col of d
                    cdf = cumsum(as.numeric(d[, adm_0:adm_25])))
  
  distr_coll = bind_rows(distr_coll, distr)
  
  
  # get number admitted in following four hours 
  a = loc_nums[loc_nums$dttm < time_pts[i] , ] 

  
  num_adm = bind_cols(sample_time = time_pts[i],  num_adm = a[nrow(a), actual])
  
  adm_coll = bind_rows(adm_coll, num_adm)
  
}


# Identify where the cutoff points on the cdf are -------------------------




cutoff_cdf_at_mult_days = tibble()


for (i in 1:nrow(adm_coll)) {
  
  distr = as_tibble(distr_coll[sample_time == adm_coll$sample_time[i]])
  
  actual_adm = adm_coll[sample_time == adm_coll$sample_time[i], num_adm]
  
  alpha_increments = 0.05
  cutoff_cdf_at = as_tibble(seq(alpha_increments,1,alpha_increments)) %>% rename(cutoff = value)
  cutoff_cdf_at$date = adm_coll$sample_time[i]
  cutoff_cdf_at$actual_adm = actual_adm
  cutoff_cdf_at$model_lower_num_adm = NA
  cutoff_cdf_at$model_lower_cdf = NA
  cutoff_cdf_at$model_upper_num_adm = NA
  cutoff_cdf_at$model_upper_cdf = NA
  
  for (j in 1:nrow(cutoff_cdf_at)) {
    
    cutoff_cdf_at$model_lower_num_adm[j] = 
      nrow(distr %>% filter(cdf < cutoff_cdf_at$cutoff[j]))
    
    if (j != nrow(cutoff_cdf_at)) {
      
      cutoff_cdf_at$model_upper_num_adm[j] = cutoff_cdf_at$model_lower_num_adm[j] + 1 # This is correct
      
    }      else {
      cutoff_cdf_at$model_upper_num_adm[j] = 
        nrow(distr %>% filter(cdf < cutoff_cdf_at$cutoff[j]))
    }
    
    # Enoch also looks up the cdf at the model threshold 
    # (although note that Enoch adds 1 to i to get the upper threshold, returning the next row in the cdf
    # rather than setting the higher band at the next alpha threshhold for the cdf
    
    if (cutoff_cdf_at$model_lower_num_adm[j] != 0) {
      cutoff_cdf_at$model_lower_cdf[j] = distr$cdf[cutoff_cdf_at$model_lower_num_adm[j]] 
    }
    cutoff_cdf_at$model_upper_cdf[j] = distr$cdf[cutoff_cdf_at$model_upper_num_adm[j]]
    
  }
  
  cutoff_cdf_at_mult_days <- cutoff_cdf_at_mult_days %>% bind_rows(cutoff_cdf_at)
}


# to check the calculations of model_lower_cdf and model_upper_cdf,
# they should sum to the number of days covered by the model
# in today's meeting, Martin said he was apportioning the propabilities for each day 
# into a line from 0 to 1 - so its sum should be the number of days in the test
cutoff_cdf_at_mult_days %>% group_by(cutoff) %>% summarise(tot = sum(model_lower_cdf, na.rm = TRUE)) %>%  arrange(desc(tot))

# Enoch's code counts the number of days where the actual admission is less than the model threshold limits
cutoff_cdf_at_mult_days <- cutoff_cdf_at_mult_days %>% 
  mutate(actual_less_than_lower = ifelse(actual_adm < model_lower_num_adm, TRUE, FALSE),
         actual_less_than_upper = ifelse(actual_adm < model_upper_num_adm, TRUE, FALSE))


# he then divides by the number of days but he's doing this across all days
# therefore I will create a grouped version across all days

cutoff_cdf_normalised <- cutoff_cdf_at_mult_days %>% 
  group_by(cutoff) %>% summarise(model_lower_limits = mean(model_lower_cdf, na.rm = TRUE),
                                 actual_less_than_lower_limit = mean(actual_less_than_lower, na.rm = TRUE),
                                 model_upper_limits = mean(model_upper_cdf, na.rm = TRUE),
                                 actual_less_than_upper_limit = mean(actual_less_than_upper, na.rm = TRUE),
                                 
  )




cutoff_cdf_normalised %>% pivot_longer(model_lower_limits:actual_less_than_upper_limit, 
                                       names_to = "model", 
                                       values_to = "proportion") %>% 
  mutate(model = factor(model, levels = c("model_lower_limits", "actual_less_than_lower_limit", "model_upper_limits", "actual_less_than_upper_limit"))) %>% 
  ggplot(aes(x = cutoff, y = proportion, col = model)) + geom_point() + geom_line()  + theme_classic() +
  scale_x_continuous(breaks = seq(0, 1, .05)) +
  labs(title = paste0("Admission probability distribution evaluation - based on ", length(time_pts), " randomly sampled time points of interest"), 
       y = "Proportion of instances <= X on cdf",
       # subtitle = "At each time point of interest, probability of admission is calculated for each patient in ED. These probabilities are converted into a cumulative probability distribution.\n
       # 20 equally spaced points on the cdf are chosen (shown on the X axis); these are the probability that the number of admissions is less than or equal to a number x \n
       # The distribution generated by the model is used to retrieve what x (the number of admissions) would be at each of these 20 points on the cdf",
       x = "X",
       col = "model/actual")  +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("#F8766D" , "#FFB2B2","#00BFC4","#99E4E7", guide = NULL, name = NULL)) 


