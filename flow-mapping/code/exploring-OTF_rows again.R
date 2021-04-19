
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



# Exploring prior to editing OTF ------------------------------------------

# group exits via relevant locations
moves[, "otf" := case_when(location == "OTF" ~ 1, 
                           TRUE ~ 0)]
moves[, "visited_otf" := sum(otf, na.rm = TRUE) > 0, by = csn]

moves = merge(moves, summ[,.(csn, adm)], all.x = TRUE)
moves[, adm2 := if_else(adm %in% c("direct_adm", "indirect_adm"), TRUE, FALSE)]

moves[, num_OTF := sum(otf), by = csn]

m = moves[, .(N = uniqueN(csn)), by = .(adm2, num_OTF)]
m %>% pivot_wider(names_from = num_OTF, values_from = N)


m = moves[, .(N = uniqueN(csn)), by = .(adm, num_OTF)]
m %>% pivot_wider(names_from = num_OTF, values_from = N)

m = moves[first_ED_admission > '2020-03-19', .(N = uniqueN(csn)), by = .(adm, num_OTF)]

# looking abit at OTF rows - add lag and lead rows and delete where next csn is not the same (from create-data-tables.R)

m2 = moves[otf == 1, .(csn, location, discharge, lag_location, lag_discharge, lead_location, duration)]
# looks mostly sensible eyeballing this - next locations are what you would expext after OTF

moves[, duration := difftime(discharge, admission, units = "mins")]
m[duration > 60, .N]

m[duration > 120, .N] 


# update with next row's discharge time when locations are repeated
#moves[csn == lead_csn & location == lead_location, discharge := lead_discharge]
moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]
moves[csn == lead_csn & location == lead_location, drop_row := TRUE]
rpt(moves) 
rpt(moves[(drop_row)]) # this is number of csns where a row will be dropped

# update 
moves[csn == lag_csn & location == "OTF", otf_row_drop_candidate := TRUE]
moves[csn == lag_csn & location == "OTF" & is.na(lead_csn), otf_row_to_drop_last_row := TRUE]

# two people have OTF as last row and were admitted but OK to delete in both cases
moves[(otf_row_to_drop_last_row & adm2)]


m2 = moves[otf == 1, .(csn, location, discharge, lag_location, lag_discharge, lead_location, duration, otf_row_to_drop_last_row)]



moves[, "visited_otf" := sum(otf, na.rm = TRUE) > 0, by = csn]

