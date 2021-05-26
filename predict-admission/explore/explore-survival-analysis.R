# from: https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(data.table)
library(tidyverse)

#------------
data(veteran)
head(veteran)


km <- with(veteran, Surv(time, status))
head(km,80)


km_fit <- survfit(Surv(time, status) ~ 1, data=veteran)
summary(km_fit, times = c(1,30,60,90*(1:10)))
autoplot(km_fit)

cox <- coxph(Surv(time, status) ~ trt + celltype + karno  + diagtime + age + prior , data = veteran)
summary(cox)



# Load data ---------------------------------------------------------------


file_date = '2021-05-17'

load(paste0("~/EDcrowding/flow-mapping/data-raw/summ_", file_date,".rda"))

summ[, in_set := case_when(first_ED_admission < '2019-11-19 00:00:00' ~ "Train",
                           first_ED_admission < '2019-12-13 00:00:00' ~ "Val",
                           first_ED_admission < '2020-03-19 00:00:00' ~ "Test",
                           first_ED_admission < '2020-12-01 00:00:00' ~ "Train",
                           first_ED_admission < '2020-12-29 00:00:00' ~ "Val",
                           first_ED_admission < '2021-05-01 00:00:00' ~ "Test",
                           TRUE ~ "After")]

summ[, epoch := case_when(date(first_ED_admission) < '2020-03-19' ~ "Pre",
                          date(first_ED_admission) < '2021-05-01' ~ "Post",
                          TRUE ~ "After")]

summ[, left_ED := coalesce(first_outside_proper_admission, last_inside_discharge)]

summ[, tta := as.integer(difftime(first_outside_proper_admission, first_ED_admission, units = "hours"))]
summ[, duration := difftime(left_ED, first_ED_admission, units = "hours")]
summ[, adm := if_else(adm %in% c("direct_adm", "indirect_adm"), 1, 0)]


s = summ[adm == 1 & tta < 24 & epoch != "After",.(csn, duration, epoch, set = paste(epoch, in_set), first_ED_admission)]
s[, tod := factor((hour(first_ED_admission) %/% 4)+1)]
s[, tod_6hr := factor((hour(first_ED_admission) %/% 6)+1)]
s[, quarter := factor(case_when( month(first_ED_admission) <= 3 ~ 1,
                                 month(first_ED_admission) <= 6 ~ 2, 
                                 month(first_ED_admission) <= 9 ~ 3, 
                                 month(first_ED_admission) <= 12 ~ 4))]
s[, year := factor(year(first_ED_admission))]
s[, weekend := factor(if_else(weekdays(first_ED_admission, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0))]
# the lab closes at 10 pm 
s[, night := factor(ifelse(hour(first_ED_admission) < 22 & hour(first_ED_admission) > 7, 0, 1))]

s$status = 1

# kaplan meier analysis
km <- with(s, Surv(duration, status))
head(km)
km_fit <- survfit(Surv(duration, status) ~ 1, data=s)

timeslices <- as.integer(c("000", "015", "030", "060",  "090", "120", "180", "240", "300", "360", "480", "720"))/60
summary(km_fit, times = timeslices)
autoplot(km_fit)

# looking at how this varies by epoch

km_fit <- survfit(Surv(duration, status) ~ epoch, data=s)
autoplot(km_fit)

km_fit <- survfit(Surv(duration, status) ~ set, data=s[epoch == "Pre"])
ppre = autoplot(km_fit) + labs(title = "KM plot for time to admission")

km_fit <- survfit(Surv(duration, status) ~ set, data=s[epoch == "Post"])
ppost = autoplot(km_fit) + labs(title = "KM plot for time to admission")

library(gridExtra)
grid.arrange(ppre, ppost)

# by time of day
km_fit <- survfit(Surv(duration, status) ~ night, data=s)
autoplot(km_fit)

km_fit <- survfit(Surv(duration, status) ~ tod_6hr, data=s)
autoplot(km_fit)



cox <- coxph(Surv(duration, status) ~ epoch + quarter + night  + weekend  , data = s)
summary(cox)

# adding in numbers in ED
sp = s[set == "Pre Train"]
sp = sp[0:1000]

num_in_ED = as.integer()
for (i in 1:nrow(sp)) {
  num_in_ED = c(num_in_ED, nrow(summ[first_ED_admission < sp$first_ED_admission[i] &
         left_ED > sp$first_ED_admission[i]]))
}

sp[, num_in_ED_q := with(sp, cut(num_in_ED, 
                                 breaks=quantile(num_in_ED, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                                 include.lowest=TRUE))]

cox <- coxph(Surv(duration, status) ~ + quarter + night  + weekend + num_in_ED_q , data = sp)
summary(cox)

km_fit <- survfit(Surv(duration, status) ~ num_in_ED_q, data=sp)
autoplot(km_fit) + labs(title = "KM plot for time in ED for 1000 admitted patients, pre Covid training set",
                          legend = "quartile of number of patients in ED")
