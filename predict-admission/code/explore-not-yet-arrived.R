# work out distribution of not yet arrived

# summary of csns to get minimum and maxium
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-04-13.rda")
summ = summ[!is.na(discharge_time)]
summ[, left_ED := coalesce(first_outside_proper_admission, last_inside_discharge)]

# Generate sample of time points of interest post COVID ------------------------------

set.seed(17L)
#  using validation set for post COVID
time_pts <- get_random_dttm(as.POSIXct('2020-03-19 00:00:00') + hours(4), as.POSIXct('2020-12-01 00:00:00') + hours(8))
last_pt <- time_pts

# each sample to be more than 12 hours and less than 24 hours after the previous one
while (last_pt + hours(12) < as.POSIXct('2020-12-29 00:00:00')) {
  next_pt <- get_random_dttm(last_pt + hours(12), last_pt + hours(24))
  time_pts <- c(time_pts, next_pt)
  last_pt <- next_pt
}

# 
# 
# # Get patients in ED who were admitted less than timeslice later ----------
# 
# in_ED_all = data.table()
# not_in_ED_yet_all = data.table()
# 
# time_window = 4
# 
# for (i in 1:length(time_pts)) {
#   
#   t = time_pts[i] + hours(time_window)
#   
#   in_ED = summ[first_ED_admission < time_pts[i] & left_ED > time_pts[i], 
#                .(csn, first_ED_admission, adm, 
#                  left_ED,
#                  elapsed = difftime(time_pts[i], first_ED_admission, units = "mins"),
#                  remaining = difftime(left_ED, time_pts[i], units = "mins"))]
#   
#   in_ED$time_pt <- time_pts[i]
#   
#   in_ED_all = bind_rows(in_ED_all, in_ED)
#   
#   not_in_ED_yet = summ[first_ED_admission >time_pts[i] & first_ED_admission < t & first_outside_proper_admission < t,
#                        .(csn, first_ED_admission, adm,
#                          first_outside_proper_admission,
#                          mins_arrived_after_time_pt = difftime(first_ED_admission, time_pts[i],  units = "mins"),
#                          total_time_in_ED = difftime(first_outside_proper_admission, first_ED_admission, units = "mins"))]
#   
#   not_in_ED_yet$time_pt <- time_pts[i]
#   
#   not_in_ED_yet_all = bind_rows(not_in_ED_yet_all, not_in_ED_yet)
#                        
# }
# 


# Summarise number not in who get admitted later --------------------------


not_in_ED_yet_all = data.table()

for (i in 1:length(time_pts)) {
  
  for (time_window in c(2,3,4,6,8,12)) {
    
    not_in_ED_yet = data.table(time_window = time_window, 
                               N = summ[first_ED_admission >time_pts[i] & first_ED_admission < time_pts[i] + hours(time_window)
                         & first_outside_proper_admission < time_pts[i] + hours(time_window), .N])
    
    not_in_ED_yet$time_pt <- time_pts[i]
    
    not_in_ED_yet_all = bind_rows(not_in_ED_yet_all, not_in_ED_yet)
    
  }
}

not_in_ED_yet_all %>% ggplot(aes(x = N)) + geom_histogram(binwidth = 1) + facet_wrap(time_window ~ ., scales = "free") +
  labs(title = "Frequency distributions for patients who have not yet arrived and who are admitted within various time windows",
       subtitle = paste("Based on", length(time_pts), "randomly sampled time points (not less than 4 hours apart) in the post COVID era"),
       x = "Number of patients admitted")


# Do again for pre COVID --------------------------------------------------



set.seed(17L)
#  using validation set for post COVID
time_pts <- get_random_dttm(as.POSIXct('2019-05-01 00:00:00') + hours(4), as.POSIXct('2019-12-31 00:00:00') + hours(8))
last_pt <- time_pts

# each sample to be more than 12 hours and less than 24 hours after the previous one
while (last_pt + hours(12) < as.POSIXct('2020-12-29 00:00:00')) {
  next_pt <- get_random_dttm(last_pt + hours(12), last_pt + hours(24))
  time_pts <- c(time_pts, next_pt)
  last_pt <- next_pt
}

not_in_ED_yet_all_pre = data.table()

for (i in 1:length(time_pts)) {
  
  for (time_window in c(2,3,4,6,8,12)) {
    
    not_in_ED_yet = data.table(time_window = time_window, 
                               N = summ[first_ED_admission >time_pts[i] & first_ED_admission < time_pts[i] + hours(time_window)
                                        & first_outside_proper_admission < time_pts[i] + hours(time_window), .N])
    
    not_in_ED_yet$time_pt <- time_pts[i]
    
    not_in_ED_yet_all_pre = bind_rows(not_in_ED_yet_all_pre, not_in_ED_yet)
    
  }
}

not_in_ED_yet_all_pre %>% ggplot(aes(x = N)) + geom_histogram(binwidth = 1) + facet_wrap(time_window ~ ., scales = "free") +
  labs(title = "Frequency distributions for patients who have not yet arrived and who are admitted within various time windows",
       subtitle = paste("Based on", length(time_pts), "randomly sampled time points (not less than 4 hours apart) in the pre COVID era"),
       x = "Number of patients admitted")


# Fit distribution --------------------------------------------------------

library(fitdistrplus)

poisson = fitdist(not_in_ED_yet_all[time_window == 8, N], 'pois', method = 'mle')
pdist = dpois(1:21, lambda = poisson$estimate)
pdist = pdist*length(time_pts)

not_in_ED_yet_all[time_window == 8] %>% ggplot() + geom_histogram(aes(x = N), binwidth = 1) + geom_line(aes(x = N, y = pdist))


# sample from poisson distribution
rpois(1, lambda = poisson$estimate)
