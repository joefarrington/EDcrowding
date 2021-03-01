
# About this file ---------------------------------------------------------

# thiS looks at patterns of OTF use for discharge and admitted patients
# in order to decide whether to keep OTF rows as part of ED


# Using moves data pre-loaded ---------------------------------------------



# group exits via relevant locations
moves[, "otf" := case_when(location == "OTF" ~ 1, 
                           TRUE ~ 0)]
moves[, "visited_otf" := sum(otf, na.rm = TRUE) > 0, by = csn]

moves = merge(moves, summ[,.(csn, adm)], all.x = TRUE)

o_dis =moves[(visited_otf & !adm %in% c("direct_adm", "indirect_adm"))]

o_dis2 = unique(o_dis[,.(csn,first_admission, final_location)])
o_dis2[, .N, by = date(first_admission)] %>% ggplot(aes(x = date, y = N)) + geom_bar(stat = "identity") +
  labs(title = "Number of visits marked as OTF and then discharged",
       subtitle = "taken from star_a which currently only has visits from 1 Jan 2020")



o_adm =moves[(visited_otf & adm %in% c("direct_adm", "indirect_adm"))]

o_amd2 = o_adm[otf == 1,.(csn,first_admission, row_duration)]
o_amd2[, .N, by = date(first_admission)] %>% ggplot(aes(x = date, y = N)) + geom_bar(stat = "identity") +
  labs(title = "Number of visits marked as OTF and then admitted",
       subtitle = "taken from star_a which currently only has visits from 1 Jan 2020")


o_amd2 %>% ggplot(aes(x = row_duration/60)) + geom_boxplot()  +
  labs(title = "Boxplot of duration of time in OTF (hours) for admitted patients only",
       x = "Time in OTF (hours)",
       subtitle = "taken from star_a which currently only has visits from 1 Jan 2020")



o_amd2[row_duration < 24*60] %>% ggplot(aes(x = row_duration/60)) + geom_boxplot()  +
  labs(title = "Boxplot of duration of time in OTF (hours) where less than 48 hours for admitted patients only",
       x = "Time in OTF (hours)",
       subtitle = "taken from star_a which currently only has visits from 1 Jan 2020") 

o_amd2[first_admission > '2020-03-19'] %>% ggplot(aes(x = row_duration/60)) + geom_boxplot()  +
  labs(title = "Boxplot of duration in OTF (hours) for after Covid dataset, admitted patients",
       x = "Time in OTF (hours)",
       subtitle = "from 19 March 2020 onwards") 
