
# Process Ken's predictions -----------------------------------------------

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


# Compare -----------------------------------------------------------------

# get row with latest dttm less than  the sampled
d = pred_dist[pred_dist$dttm < time_pts[1], ]
d = d[nrow(d),]

# quick plot of distributions
d[nrow(d),] %>% pivot_longer(adm_0:adm_19) %>% 
  mutate(name = as.numeric(substr(name, 5, nchar(name)))) %>% ggplot(aes(x = name, y = value)) + geom_line() +
  labs(title = "Probability of number of admissions for 4 hours following randomly chosen timepoint 2020-01-14 20:00:00", 
       subtitle = "Actual number of admissions was 18",
       x = "Number of admissions", 
       y = "Probability") 


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

