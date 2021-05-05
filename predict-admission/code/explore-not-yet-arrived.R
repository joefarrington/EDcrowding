# work out distribution of not yet arrived

# summary of csns to get minimum and maxium
load("~/EDcrowding/flow-mapping/data-raw/summ_2021-04-13.rda")
summ = summ[!is.na(discharge_time)]
summ[, left_ED := coalesce(first_outside_proper_admission, last_inside_discharge)]

# Generate sample of time points of interest post COVID ------------------------------

set.seed(17L)
start_of_set = as.POSIXct('2020-03-19 00:00:00')
end_of_set = as.POSIXct('2020-12-01 00:00:00')
#  find the first random date
time_pts_train_post <- get_random_dttm(start_of_set + hours(4), 
                                  start_of_set + hours(8))
last_pt <- time_pts_train_post 

# each sample to be more than 12 hours and less than 24 hours after the previous one
while (last_pt + hours(8) < end_of_set) {
  next_pt <- get_random_dttm(last_pt + hours(4), last_pt + hours(8))
  time_pts_train_post <- c(time_pts_train_post, next_pt)
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
# for (i in 1:length(time_pts_train_post)) {
#   
#   t = time_pts_train_post[i] + hours(time_window)
#   
#   in_ED = summ[first_ED_admission < time_pts_train_post[i] & left_ED > time_pts_train_post[i], 
#                .(csn, first_ED_admission, adm, 
#                  left_ED,
#                  elapsed = difftime(time_pts_train_post[i], first_ED_admission, units = "mins"),
#                  remaining = difftime(left_ED, time_pts_train_post[i], units = "mins"))]
#   
#   in_ED$time_pt <- time_pts_train_post[i]
#   
#   in_ED_all = bind_rows(in_ED_all, in_ED)
#   
#   not_in_ED_yet = summ[first_ED_admission >time_pts_train_post[i] & first_ED_admission < t & first_outside_proper_admission < t,
#                        .(csn, first_ED_admission, adm,
#                          first_outside_proper_admission,
#                          mins_arrived_after_time_pt = difftime(first_ED_admission, time_pts_train_post[i],  units = "mins"),
#                          total_time_in_ED = difftime(first_outside_proper_admission, first_ED_admission, units = "mins"))]
#   
#   not_in_ED_yet$time_pt <- time_pts_train_post[i]
#   
#   not_in_ED_yet_all = bind_rows(not_in_ED_yet_all, not_in_ED_yet)
#                        
# }
# 


# Summarise number not in who get admitted later --------------------------


not_in_ED_yet_all = data.table()

for (i in 1:length(time_pts_train_post)) {
  
  for (time_window in c(2,3,4,6,8,12)) {
    
    not_in_ED_yet = data.table(time_window = time_window, 
                               N = summ[first_ED_admission >time_pts_train_post[i] & first_ED_admission < time_pts_train_post[i] + hours(time_window)
                         & first_outside_proper_admission < time_pts_train_post[i] + hours(time_window), .N])
    
    not_in_ED_yet$time_pt <- time_pts_train_post[i]
    
    not_in_ED_yet_all = bind_rows(not_in_ED_yet_all, not_in_ED_yet)
    
  }
}

not_in_ED_yet_all %>% ggplot(aes(x = N)) + geom_histogram(binwidth = 1) + facet_wrap(time_window ~ ., scales = "free") +
  labs(title = "Frequency distributions for patients who have not yet arrived and who are admitted within various time windows",
       subtitle = paste("Based on", length(time_pts_train_post), "randomly sampled time points (not less than 4 hours apart) in the post COVID training set"),
       x = "Number of patients admitted")


# Do again for pre COVID --------------------------------------------------


set.seed(17L)
start_of_set = as.POSIXct('2019-05-01 00:00:00')
end_of_set = as.POSIXct('2019-11-19 00:00:00')
#  find the first random date
time_pts_train_pre <- get_random_dttm(start_of_set + hours(4), 
                                       start_of_set + hours(8))
last_pt <- time_pts_train_pre 

# each sample to be more than 12 hours and less than 24 hours after the previous one
while (last_pt + hours(4) < end_of_set) {
  next_pt <- get_random_dttm(last_pt + hours(4), last_pt + hours(8))
  time_pts_train_pre <- c(time_pts_train_pre, next_pt)
  last_pt <- next_pt
}

not_in_ED_yet_all_pre = data.table()

for (i in 1:length(time_pts_train_pre)) {
  
  for (time_window in c(2,3,4,6,8,12)) {
    
    not_in_ED_yet = data.table(time_window = time_window, 
                               N = summ[first_ED_admission >time_pts_train_pre[i] & first_ED_admission < time_pts_train_pre[i] + hours(time_window)
                                        & first_outside_proper_admission < time_pts_train_pre[i] + hours(time_window), .N])
    
    not_in_ED_yet$time_pt <- time_pts_train_pre[i]
    
    not_in_ED_yet_all_pre = bind_rows(not_in_ED_yet_all_pre, not_in_ED_yet)
    
  }
}

not_in_ED_yet_all_pre %>% ggplot(aes(x = N)) + geom_histogram(binwidth = 1) + facet_wrap(time_window ~ ., scales = "free") +
  labs(title = "Frequency distributions for patients who have not yet arrived and who are admitted within various time windows",
       subtitle = paste("Based on", length(time_pts_train_pre), "randomly sampled time points (not less than 4 hours apart) in the pre COVID training set"),
       x = "Number of patients admitted")


# Fit distribution --------------------------------------------------------

library(fitdistrplus)
# 
# poisson = fitdist(not_in_ED_yet_all[time_window == 8, N], 'pois', method = 'mle')
# pdist = dpois(1:21, lambda = poisson$estimate)
# pdist = pdist*length(time_pts_train)
# 

pdist = dpois(0:21, lambda = not_in_ED_yet_all[time_window == 8, mean(N)])
pdist = pdist*length(time_pts_train)

not_in_ED_yet_all[time_window == 8] %>% ggplot() + geom_histogram(aes(x = N), binwidth = 1) + 
  stat_function(geom = "line", fun = dpois, args = list(lambda = 5.83), colour = "red", fill = NA, n = length(time_pts_train))

# can't work out how to superimpose Poisson on a histogram, so creating my own
not_in_ED_yet_all[time_window == 8, (V1 = .N), by = N] %>% ggplot(aes(x = N, y = V1)) + geom_bar(stat = "identity")

# rdist = rpois(length(time_pts_train), lambda = not_in_ED_yet_all[time_window == 8, mean(N)])
# pois = data.table(table(rdist))
# 
# pois = setnames(pois, "N", "Poisson")
# pois = setnames(pois, "rdist", "N")
# pois$N = as.numeric(pois$N)
# 
# plotdata = merge(data.table(N = seq(0, max(rdist), 1)), pois, all.x = TRUE)
# plotdata = merge(not_in_ED_yet_all[time_window == 8, (V1 = .N), by = N] , plotdata, all.x = TRUE, by = "N")
# 
# plotdata %>% ggplot(aes(x = N, y = V1)) + geom_bar(stat = "identity") +
#   geom_line(aes(x = N, y = Poisson))

plotdata_all = data.table()

for (tw in c(2,3,4,6,8,12)) {
  
  pdist = dpois(0:21, lambda = not_in_ED_yet_all[time_window == tw, mean(N)])
  
  plotdata = data.table(N = (seq(1, length(pdist), 1))-1, poisson = pdist*length(time_pts_train))
  plotdata = merge(plotdata, not_in_ED_yet_all[time_window == tw, (V1 = .N), by = N] , all.x = TRUE, by = "N")
  plotdata$time_window = tw
  plotdata_all = bind_rows(plotdata_all, plotdata)
    
  
}

plotdata_all %>% ggplot(aes(x = N, y = V1)) + geom_bar(stat = "identity") +
  geom_line(aes(x = N, y = poisson), color = "red")  + facet_wrap(time_window ~ ., scales = "free") +
  labs(title = "Frequency distributions compared with Poisson for patients who have not yet arrived and who are admitted within various time windows",
       subtitle = paste("Based on", length(time_pts_train), "randomly sampled time points (not less than 4 hours apart) in the post COVID training set"),
       x = "Number of patients admitted")

not_in_ED_yet_all[time_window == 8, (V1 = .N), by = N] %>% ggplot(aes(x = N, y = V1)) + geom_bar(stat = "identity")


# this is how to get a probability of 0 admissions given the lamda provided
s = ppois(1, lambda = not_in_ED_yet_all[time_window == 8, mean(N)])
not_in_ED_yet_all[, mean(N), by = time_window]
