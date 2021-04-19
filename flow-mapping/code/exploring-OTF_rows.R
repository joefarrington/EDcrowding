
# About this file ---------------------------------------------------------

# thiS looks at whether people in OTF are still physically in ED
# if someone arrives in the location previously occupied by someone
# now marked at OTF, that suggests that these people are not still
# occupying a location

# Investigating OTF -------------------------------------------------------

load("~/EDcrowding/flow-mapping/data-raw/ED_bed_moves_raw_2021-01-25.rda")
m = data.table(ED_bed_moves_raw)



m <- data.table(ED_bed_moves_raw %>% 
                  mutate(location = case_when(department == "ED" & room4 == "TRIAGE" ~ "Waiting",
                                              department == "ED" ~ room4, 
                                              TRUE ~ department)) %>% 
                  select(csn, admission, discharge, location_string, duration_row) %>% 
                  arrange(csn, admission))
setkey(m, csn)

# add lead csn, admission and location - see https://rdrr.io/cran/data.table/man/shift.html
cols = c("csn","admission","discharge", "location_string")
lagcols = paste("last_loc", cols, sep="_")

m[, (lagcols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]       

otf = m[location_string == "ED^UCHED OTF POOL^OTF"]
otf[, location_string := NULL]
otf[, csn := NULL]
setnames(otf, "admission", "otf_admission")
setnames(otf, "discharge", "otf_discharge")

locs <- data.table(ED_bed_moves_raw %>% 
                     mutate(location = case_when(department == "ED" & room4 == "TRIAGE" ~ "Waiting",
                                                 department == "ED" ~ room4, 
                                                 TRUE ~ department)) %>% 
                     select(csn, admission, discharge, department, location_string) %>% 
                     arrange(location_string, admission))

locs <- locs[department == "ED"]
cols = c("location_string", "admission","discharge")

nextcols = paste("next_occupant", cols, sep="_")
locs[, (nextcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]   
locs[, department := NULL]
locs = locs[location_string == next_occupant_location_string]
locs[, next_occupant_location_string := NULL]

otf_next_occupant_of_prior_location = as_tibble(otf) %>% 
  left_join(as_tibble(locs), 
            by = c("last_loc_location_string" = "location_string",
                   "last_loc_admission" = "admission", 
                   "last_loc_discharge" = "discharge"))

otf_next_occupant_of_prior_location = data.table(otf_next_occupant_of_prior_location)

# to get number of rows where someone is admitted to a location previously occupied by someone now showing as OTF
otf_next_occupant_of_prior_location[next_occupant_admission < last_loc_discharge]
# 9 % of cases
nrow(otf_next_occupant_of_prior_location[next_occupant_admission < last_loc_discharge])/
  nrow(otf_next_occupant_of_prior_location)

otf_next_occupant_of_prior_location[, overlap := 
                                      next_occupant_admission < last_loc_discharge]

otf_next_occupant_of_prior_location %>% group_by(date(otf_admission)) %>% summarise(N = sum(overlap)) %>% 
  ggplot(aes(x = `date(otf_admission)`, y = N)) + geom_line() +
  labs(title = "Number of overlaps of prior-to-OTF-locations with next occupant", x = "Date")
