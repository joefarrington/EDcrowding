# About the script  -------------------------------------------

# Gets all patients currently in ED



# Load libraries
# ==============

library(DBI)
library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)
library(polynom)


# for mlr3
library(mlr3)
library(mlr3learners)
library(mlr3proba)
library(mlr3fselect)
library(mlr3misc)


# Create functions --------------------------------------------------------

rpt <- function(dataframe) {
  print(dataframe %>% select(csn) %>% n_distinct())
}

poly_prod = function(df){
  
  # polynomial(vector) is a polynomial t
  y = polynomial(c(1,0))# 
  for (n in 1:nrow(df)){
    y = y*polynomial(c(1-df$prob.1[n],df$prob.1[n]))
  }
  return(coef(y))
}




split_location <- function(hl7_location, n) {
  s <- unlist(strsplit(hl7_location, "\\^"))
  return(s[n])
}
split_location <- Vectorize(split_location)

# clean room data
# function removes bay and chair numbers

clean_room_names <- function(department, room) {
  if (department == "ED" && !is.na(room)) {
    room = gsub("UCHED ", "", room)
    room = gsub("UCH ED ", "", room)
    room = gsub("UCH ", "", room)
    room = gsub("^ED ","",room)  
    room = gsub("CHAIR [0-9]{2}", "", room)
    room = gsub("[0-9]{3}", "", room)
    room = gsub("[0-9]{2}", "", room)
    room = gsub("MAJ-CHAIR","MAJORS CH",room)
    room = gsub("MAJCH","MAJORS CH",room)
    room = gsub("SPECIALTY ASSESSMENT AREA","SAA",room)
    room = gsub("RAT-CHAIR","RAT",room)
    room = gsub("RATBED","RAT",room)
    room = gsub("SDEC","SDEC",room)
    room = gsub("  "," ",room)
    room = gsub(" $","",room)
    room = gsub("^ ","",room)  
    room = gsub("OTF POOL","OTF",room)  
    room = gsub("CONS", "", room)
    room = gsub(" ","_",room)  
  }
  else if (grepl("UCHT00CDU",department)) {
    room = "CDU"
  }
  return(room)
}
clean_room_names <- Vectorize(clean_room_names)

# function to group room names
# NB RAT COVID MAJORS could be both - need to check which to prioritise
group_room_names <- function(room) {
  room_ <- case_when(
    length(grep("UTC", room)) >0 ~ "UTC",
    length(grep("MAJ", room)) >0 ~ "MAJORS",
    length(grep("RAT", room)) >0 ~ "RAT",
    length(grep("TRIAGE", room)) >0 ~ "TRIAGE",
    length(grep("SPECIALTY ASSESSMENT AREA", room)) >0 ~ "SAA",
    length(grep("SDEC", room)) >0 ~ "SDEC",
    room %in% c( "null", "WR POOL") ~ "Waiting",
    TRUE ~ room)
  return(room_)
}

group_room_names <- Vectorize(group_room_names)

# function to create design matrix

create_timeslice <- function (moves, dm, obs_real, lab_orders_real, lab_results_real, cutoff, nextcutoff) {
  
  loc_cutoff <- loc[duration > cutoff & duration <= nextcutoff]
  
  # count number of location rows up to ED - note will include any pre- ED locations
  loc_count <- loc_cutoff[, .N, by = csn]
  setnames(loc_count, "N", "l_num")
  
  loc_count <- merge(loc_count, loc_cutoff[is.na(discharge_e), location, by = csn],
                     all.x = TRUE)
  setnames(loc_count, "location", "l_current")
  loc_count[, l_current := factor(l_current)]
  
  # add indicator of location visited at csn level
  loc_cutoff_csn <- merge(loc_count, data.table(
    loc_cutoff[(!outside), .N > 0, by = .(csn, location)]  %>% 
      pivot_wider(names_from = location, names_prefix = "l_visited_", values_from = V1, values_fill = 0)
  ), all.x = TRUE)
  
  
  # rename CDU
  
  if (sum(grepl("UCHT00CDU", colnames(loc))) > 0) {
    setnames(loc_cutoff_csn, "num_UCHT00CDU", "num_CDU")
  }
  
  # observation data - this will create counts of all observation data prior to cutoff + margin
  # select for cutoff
  obs_cutoff <- obs_real[duration > cutoff & duration <= nextcutoff]
  
  if (nrow(obs_cutoff) > 0) {
    # add number of observation measurements up to cutoff
    obs_cutoff[, o_num_meas := .N, by = csn]
    
    # add number of types of results by csn
    obs_cutoff_csn <- merge(unique(obs_cutoff[, .(csn, o_num_meas)]), 
                            obs_cutoff[, .(o_num_types = uniqueN(obs_name)), by = csn], by = "csn")
    
    # add number of observation events per csn
    obs_cutoff_csn <- merge(obs_cutoff_csn, obs_cutoff[, .(o_num_events = uniqueN(elapsed_mins)), by = csn])
    
    # blood pressure has two measurements per event, so delete one type
    obs_cutoff_csn[, o_num_types := o_num_types -1]
    # obs_cutoff_csn[, o_num := o_num - o_num_Bloodpressure_sys]
    obs_cutoff_csn[, o_has := 1] # this will be 1 for all csns currently; zeros added later
    
    
    # add count of times when O2 sat dropped below 90 or 95
    
    sat_lt90 <- obs_cutoff[obs_name == "Oxygensaturation" & value_as_real < 90, .N, by = .(csn)]
    setnames(sat_lt90, "N", "o_num_o2sat_lt90")
    sat_lt95 <- obs_cutoff[obs_name == "Oxygensaturation" & value_as_real < 95, .N, by = .(csn)]
    setnames(sat_lt95, "N", "o_num_o2sat_lt95")
    
    obs_cutoff_csn <- merge(obs_cutoff_csn, sat_lt90, all.x = TRUE, by = "csn")
    obs_cutoff_csn <- merge(obs_cutoff_csn, sat_lt95, all.x = TRUE, by = "csn")
    
    # add count of times when news score was medium or high
    
    news_med <- obs_cutoff[obs_name == "NEWSscore" & value_as_real < 7 & value_as_real > 4, .N, by = .(csn)] 
    setnames(news_med, "N", "o_num_news_med")
    news_high <- obs_cutoff[obs_name == "NEWSscore" & value_as_real >= 7, .N, by = .(csn)] 
    setnames(news_high, "N", "o_num_news_high")
    
    obs_cutoff_csn <- merge(obs_cutoff_csn, news_med, all.x = TRUE, by = "csn")
    obs_cutoff_csn <- merge(obs_cutoff_csn, news_high, all.x = TRUE, by = "csn")
    
    # add count of times ACVPU not equal to A
    ACVPU_notA <- obs_cutoff[obs_name == "ACVPU" & value_as_real > 1, .N, by = .(csn)] 
    setnames(ACVPU_notA, "N", "o_num_ACVPU_notA")
    
    obs_cutoff_csn <- merge(obs_cutoff_csn, ACVPU_notA, all.x = TRUE, by = "csn")
    
    # add count of times GCS <= 8
    GCS_lt9 <- obs_cutoff[obs_name == "GCStotal" & value_as_real < 9, .N, by = .(csn)] 
    setnames(GCS_lt9, "N", "o_num_GCS_lt9")
    
    obs_cutoff_csn <- merge(obs_cutoff_csn, GCS_lt9, all.x = TRUE, by = "csn")
    
    # generate counts of each observation by csn
    obs_cutoff_csn_w <- obs_cutoff[, .N, by = .(csn, obs_name)] %>%
      pivot_wider(names_from = obs_name, names_prefix = "o_num_", values_from = N, values_fill = 0)
    
    if (sum(grepl("o_num_Bloodpressure_dia", colnames(obs_cutoff_csn_w))) > 0) {
      obs_cutoff_csn_w <- obs_cutoff_csn_w %>% select(-o_num_Bloodpressure_dia)
      obs_cutoff_csn_w <- obs_cutoff_csn_w %>% rename(o_num_Bloodpressure = o_num_Bloodpressure_sys)
    }
    
    obs_cutoff_csn <- data.table(merge(obs_cutoff_csn, obs_cutoff_csn_w))
    
    # add valued obs data
    obs_cutoff_csn_val <- data.table(
      obs_cutoff %>% 
        filter(obs_name %in% c("TempinCelsius", "Heartrate","MAPnoninvasive", "Respiratoryrate", "FiO2" )) %>% 
        group_by(csn, obs_name) %>%
        # using max allows for possibility of two measurements in same minute
        summarise(latest_value = max(value_as_real, na.rm = TRUE))  %>%
        pivot_wider(names_from = obs_name, names_prefix = "o_latest_", values_from = latest_value)
    )
  }
  
  
  
  # 
  # add lab data

  # select for cutoff
  lab_orders_cutoff <- lab_orders_real[duration > cutoff & duration <= nextcutoff]
  lab_results_cutoff <- lab_results_real[duration > cutoff & duration <= nextcutoff]
  
  # # add number of lab orders up to cutoff
  # lab_cutoff[, p_num_orders := .N, by = csn]
  
  # add number of types of orders by csn
  
  if (nrow(lab_orders_cutoff) > 0) {
    lab_cutoff_csn <- lab_orders_cutoff[, .(p_num_battery = uniqueN(battery_code)), by = csn]
    
    # add whether each cluster was requested
    
    lab_cutoff_csn_battery = lab_orders_cutoff[, (N =.N > 0), by = .(csn, battery_code)] %>% 
      pivot_wider(names_from = battery_code, names_prefix = "p_req_battery_", values_from = V1, values_fill = 0)
    
    if (nrow(lab_results_cutoff) > 0) {
      # add number of lab results that are out of range high and low
      if (nrow(lab_results_cutoff[(oor_high), .(p_num_oor_high =.N), by = csn]) > 0) {
        lab_cutoff_csn <- merge(lab_cutoff_csn, lab_results_cutoff[(oor_high), .(p_num_oor_high =.N), by = csn])
        
      }
      
      if (nrow(lab_results_cutoff[(oor_low), .(p_num_oor_low =.N), by = csn]) > 0) {
        lab_cutoff_csn <- merge(lab_cutoff_csn, lab_results_cutoff[(oor_low), .(p_num_oor_low =.N), by = csn])
        
      }
      
      if (nrow(lab_results_cutoff[(abnormal), .(p_num_abnormal =.N), by = csn]) > 0) {
        lab_cutoff_csn <- merge(lab_cutoff_csn, lab_results_cutoff[(abnormal), .(p_num_abnormal =.N), by = csn])
        
      }
      
      if (nrow(lab_results_cutoff %>%
                       filter(test_lab_code %in% c("K", "NA", "CREA", "HCTU", "WCC")))> 0) {
        # add score for each lab test in APACHE (add other values from ED clinicians)
        
        # first check whether any one has two measurements in same minute
        lab_cutoff_csn_val_temp <- lab_results_cutoff %>%
            filter(test_lab_code %in% c("K", "NA", "CREA", "HCTU", "WCC")) %>% 
            group_by(csn, test_lab_code) 
        
        if (nrow(lab_cutoff_csn_val_temp %>% summarise(N = n()) %>% filter(N > 1)) > 0) {
          lab_cutoff_csn_val <- data.table(
            lab_cutoff_csn_val_temp %>%
              # using max allows for possibility of two measurements in same minute
              summarise(latest_value = max(value_as_real, na.rm = TRUE))  %>%
              pivot_wider(names_from = test_lab_code, names_prefix = "p_latest_", values_from = latest_value))
        }
        
        else {
          lab_cutoff_csn_val <- data.table(
            lab_cutoff_csn_val_temp %>%
              # using max will cause warnings if there are no values to maximise
              summarise(latest_value = value_as_real)  %>%
              pivot_wider(names_from = test_lab_code, names_prefix = "p_latest_", values_from = latest_value))
        }
        
      }
      
    }
    lab_cutoff_csn <- data.table(merge(lab_cutoff_csn, lab_cutoff_csn_battery))
  }
  
  ## combine everything
  # just use csn from summ to start with - add the other summ fields (which may have genuine NAs) later
  matrix_cutoff <- merge(data.table(csn = dm[duration > cutoff & duration <= nextcutoff, csn]), loc_cutoff_csn, all.x = TRUE, by = "csn")
  
  if (nrow(obs_cutoff) > 0) {
    matrix_cutoff <- merge(matrix_cutoff, obs_cutoff_csn, all.x = TRUE)
    
  }
  
  if (nrow(lab_orders_cutoff) > 0) {
    matrix_cutoff <- merge(matrix_cutoff, lab_cutoff_csn, all.x = TRUE)
    
  }
  
  matrix_cutoff[is.na(matrix_cutoff)] <- 0
  
  # add other info where there may be genuine NAs
  matrix_cutoff <- merge(matrix_cutoff, dm[duration > cutoff & duration <= nextcutoff], by = c("csn")) 
  
  if (nrow(obs_cutoff) > 0) {
    matrix_cutoff <- merge(matrix_cutoff, obs_cutoff_csn_val, by = "csn", all.x = TRUE) 
  }
  
  if (nrow(lab_results_cutoff) > 0) {
    if (nrow(lab_results_cutoff %>%
             filter(test_lab_code %in% c("K", "NA", "CREA", "HCTU", "WCC")))> 0) {
      matrix_cutoff <- merge(matrix_cutoff, lab_cutoff_csn_val, by = "csn", all.x = TRUE)
      
    }
  }

  return(matrix_cutoff)
  
  
}


one_hot <- function(dt, cols="auto", dropCols=TRUE, dropUnusedLevels=FALSE){
  
  if(cols[1] == "auto") cols <- colnames(dt)[which(sapply(dt, function(x) is.factor(x) & !is.ordered(x)))]
  
  if(length(cols) == 0) {
    return(dt) }
  else {
    
    # Build tempDT containing and ID column and 'cols' columns
    tempDT <- dt[, cols, with=FALSE]
    tempDT[, ID := .I]
    setcolorder(tempDT, unique(c("ID", colnames(tempDT))))
    for(col in cols) set(tempDT, j=col, value=factor(paste(col, tempDT[[col]], sep="_"), levels=paste(col, levels(tempDT[[col]]), sep="_")))
    
    # One-hot-encode
    if(dropUnusedLevels == TRUE){
      newCols <- dcast(melt(tempDT, id = 'ID', value.factor = T), ID ~ value, drop = T, fun = length)
    } else{
      newCols <- dcast(melt(tempDT, id = 'ID', value.factor = T), ID ~ value, drop = F, fun = length)
    }
    
    # Combine binarized columns with the original dataset
    result <- cbind(dt, newCols[, !"ID"])
    
    # If dropCols = TRUE, remove the original factor columns
    if(dropCols == TRUE){
      result <- result[, !cols, with=FALSE]
    }
    
    return(result)
  }
}


get_prob_dist = function(time_window, preds_all_ts, poisson_means, tta_probs) {
  
  
  distr_all = data.table()
  # get probs for all in ED being admitted
  
  num_adm_ = seq(0,nrow(preds_all_ts), 1)
  # the probabilities of each of these numbers being admitted
  probs = poly_prod(preds_all_ts) 
  
  distr_all = bind_cols(num_adm_pred = num_adm_, 
                    probs = probs, cdf = cumsum(probs),
                    dist = "In ED now; admission at some point")
  
  for (time_window in c(4, 8)) {
    
    df = merge(preds_all_ts[, .(prob.1, csn, timeslice = as.numeric(gsub("task", "", timeslice)))], 
               tta_probs[tta_hr == time_window, .(timeslice, prob_adm_in_time_window = cdf)], 
               by = "timeslice", all.x = TRUE)
    
    # temp fix where there is no match with time_window
    df[is.na(prob_adm_in_time_window), prob_adm_in_time_window := mean(df$prob_adm_in_time_window, na.rm = TRUE)]
    
    df[, prob.1 := prob.1 * prob_adm_in_time_window]
    
    probs_in_ED = poly_prod(df) 
    
    dist = bind_cols(num_adm_pred = num_adm_, 
                     probs = probs_in_ED, cdf = cumsum(probs_in_ED),
                     dist = paste("In ED now; admission in", time_window, "hours"))
    
    distr_all = bind_rows(distr_all, dist)
    
    time_window_ = time_window
    probs_not_yet_arrived = dpois(c(num_adm_ , seq(max(num_adm_), max(num_adm_+20),1)),
                                  lambda = poisson_means[time_window == time_window_, poisson_mean])
    
    dist_ = data.table()
    
    for (i in 1:length(probs_in_ED)) {
      for (j in 1:length(probs_not_yet_arrived)) {
      
        tot_adm_ = i-1 + j-1
        prob_tot_ = probs_in_ED[i] * probs_not_yet_arrived [j]
        row = data.table(num_adm_pred = tot_adm_, prob_tot = prob_tot_)
        
        dist_ = bind_rows(dist_, row)
      }
    }
    
    dist_ = dist_[, .(probs = sum(prob_tot)), by = num_adm_pred]
    dist_[, cdf := cumsum(probs)]
    dist = dist_[, dist := paste("Including not yet arrived; admission in", time_window, "hours")]

    distr_all = bind_rows(distr_all, dist)            
  }
  
  

  return(data.table(distr_all))
  
}


# Set up connection -------------------------------------------------------



# Set up connection
ctn <- DBI::dbConnect(RPostgres::Postgres(),
                      host = Sys.getenv("UDS_HOST"),
                      port = 5432,
                      user = Sys.getenv("UDS_USER"),
                      password = Sys.getenv("UDS_PWD"),
                      dbname = "uds")


# Get  data ---------------------------------------------------------

time_of_extract = Sys.time()

# all patients in ED now

sqlQuery <- "select m.mrn, hv.encounter as csn, hv.hospital_visit_id, hv.patient_class, hv.presentation_time, hv.admission_time,  hv.arrival_method
    from star.hospital_visit hv,
      star.mrn m,    
      star.location_visit lv,
      star.location l 
  where hv.mrn_id = m.mrn_id
  and hv.discharge_time is NULL
  and hv.admission_time is not NULL
  and lv.hospital_visit_id = hv.hospital_visit_id 
  and l.location_id = lv.location_id 
  and hv.admission_time > NOW() - INTERVAL '3 DAY' 
  and left(l.location_string, 3) ='ED^'
  and lv.discharge_time is null
    order by csn, admission_time desc;"

sqlQuery %>% gsub('\n','',sqlQuery)
summ_now <- as_tibble(dbGetQuery(ctn, sqlQuery))
rpt(summ_now)
summ_now <- summ_now %>% mutate(time_since_arrival = difftime(time_of_extract, presentation_time, units = "mins"))


# demographics

sqlQuery <- "select mrn, 
 date_of_birth, 
 sex
 from star.core_demographic d,
  star.hospital_visit hv,
  star.mrn m,    
  star.location_visit lv,
  star.location l 
  where hv.mrn_id = m.mrn_id
  and hv.discharge_time is NULL
  and hv.admission_time is not NULL
  and lv.hospital_visit_id = hv.hospital_visit_id 
  and l.location_id = lv.location_id 
  and hv.admission_time > NOW() - INTERVAL '3 DAY' 
  and left(l.location_string, 3) ='ED^'
  and lv.discharge_time is null
  and m.mrn_id = d.mrn_id
"

sqlQuery <- gsub('\n','',sqlQuery)
demog_raw <- as_tibble(dbGetQuery(ctn, sqlQuery))

# location data

sqlQuery <- "   select m.mrn, hv.encounter as csn, hv.hospital_visit_id, hv.patient_class, hv.presentation_time, hv.admission_time,  hv.arrival_method, lv.admission_time as admission, lv.discharge_time as discharge, l.location_string    
    from star.hospital_visit hv,
      star.mrn m,    
      star.location_visit lv,
      star.location l
      where lv.hospital_visit_id in 
            (
            select hv.hospital_visit_id
            from star.hospital_visit hv,
              star.location_visit lv,
              star.location l 
          where hv.discharge_time is NULL
          and hv.admission_time is not NULL
          and lv.hospital_visit_id = hv.hospital_visit_id 
          and l.location_id = lv.location_id 
          and hv.admission_time > NOW() - INTERVAL '3 DAY' 
          and left(l.location_string, 3) ='ED^'
          and lv.discharge_time is null
            )
      and l.location_id = lv.location_id 
      and lv.hospital_visit_id = hv.hospital_visit_id 
      and hv.mrn_id = m.mrn_id;
"
sqlQuery <- gsub('\n','',sqlQuery)
moves_now <- as_tibble(dbGetQuery(ctn, sqlQuery)) %>% arrange(csn, admission)

# visit history

sqlQuery <- "select m.mrn, hv.encounter as csn, hv.patient_class, 
  hv.presentation_time, hv.admission_time, hv.discharge_time, hv.discharge_destination, hv.discharge_disposition
    from star.hospital_visit hv,
      star.mrn m
      where m.mrn in 
            (
            select m.mrn 
              from star.hospital_visit hv,
                star.mrn m,    
                star.location_visit lv,
                star.location l 
            where hv.mrn_id = m.mrn_id
            and hv.discharge_time is NULL
            and hv.admission_time is not NULL
            and lv.hospital_visit_id = hv.hospital_visit_id 
            and l.location_id = lv.location_id 
            and hv.admission_time > NOW() - INTERVAL '3 DAY' 
            and left(l.location_string, 3) ='ED^'
            and lv.discharge_time is null
            )
    and hv.mrn_id = m.mrn_id
"
sqlQuery <- gsub('\n','',sqlQuery)
summ_hist <- as_tibble(dbGetQuery(ctn, sqlQuery))

# patient class change history

sqlQuery <- "  select encounter as csn, hospital_visit_id, max(valid_until) as max_emerg_class 
from star.hospital_visit_audit hva,
      star.mrn m
   where m.mrn_id in 
            (
            select m.mrn_id 
              from star.hospital_visit hv,
                star.mrn m,    
                star.location_visit lv,
                star.location l 
            where hv.mrn_id = m.mrn_id
            and hv.discharge_time is NULL
            and hv.admission_time is not NULL
            and lv.hospital_visit_id = hv.hospital_visit_id 
            and l.location_id = lv.location_id 
            and hv.admission_time > NOW() - INTERVAL '3 DAY' 
            and left(l.location_string, 3) ='ED^'
            and lv.discharge_time is null
            ) 
        and hva.mrn_id = m.mrn_id
  group by encounter, hospital_visit_id"

sqlQuery <- gsub('\n','',sqlQuery)
patient_class_hist <- as_tibble(dbGetQuery(ctn, sqlQuery))

# observation data

sqlQuery <- "  select hv.encounter as csn, hv.admission_time, vo.observation_datetime, vo.unit, vo.value_as_real, vo.value_as_text,
        vo.visit_observation_type_id, vot.id_in_application
    from star.hospital_visit hv,
      star.visit_observation vo,
      star.visit_observation_type vot
  where hv.hospital_visit_id = vo.hospital_visit_id
    and hv.hospital_visit_id in 
        (select hv.hospital_visit_id
              from star.hospital_visit hv,
                star.mrn m,    
                star.location_visit lv,
                star.location l 
            where hv.mrn_id = m.mrn_id
            and hv.discharge_time is NULL
            and hv.admission_time is not NULL
            and lv.hospital_visit_id = hv.hospital_visit_id 
            and l.location_id = lv.location_id 
            and hv.admission_time > NOW() - INTERVAL '3 DAY' 
            and left(l.location_string, 3) ='ED^'
            and lv.discharge_time is null )
      and vo.observation_datetime < hv.admission_time + INTERVAL'2 days'  
  and vo.visit_observation_type_id = vot.visit_observation_type  
            "

sqlQuery <- gsub('\n','',sqlQuery)
obs <- data.table(dbGetQuery(ctn, sqlQuery))

# lab orders

sqlQuery <- "select hv.encounter as csn, lo.lab_order_id, lo.order_datetime, lo.request_datetime, lo.lab_battery_id, lb.battery_code
  from star.hospital_visit hv,
  star.lab_order lo,
  star.lab_battery lb
  where hv.hospital_visit_id = lo.hospital_visit_id
  and hv.hospital_visit_id in 
        (select hv.hospital_visit_id
              from star.hospital_visit hv,
                star.mrn m,    
                star.location_visit lv,
                star.location l 
            where hv.mrn_id = m.mrn_id
            and hv.discharge_time is NULL
            and hv.admission_time is not NULL
            and lv.hospital_visit_id = hv.hospital_visit_id 
            and l.location_id = lv.location_id 
            and hv.admission_time > NOW() - INTERVAL '3 DAY' 
            and left(l.location_string, 3) ='ED^'
            and lv.discharge_time is null )
  and lo.lab_battery_id = lb.lab_battery_id;"

sqlQuery <- gsub('\n','',sqlQuery)
lab_orders <- data.table(dbGetQuery(ctn, sqlQuery))

# lab results

sqlQuery <- "  select hv.encounter as csn, lr.lab_order_id, lr.abnormal_flag, lr.comment, lr.units, lr.range_high, lr.range_low, lr.result_last_modified_time, lr.value_as_real, lr.value_as_text, ltd.test_lab_code
    from star.hospital_visit hv,
      star.lab_result lr,
    star.lab_order lo,
    star.lab_test_definition ltd
  where hv.hospital_visit_id = lo.hospital_visit_id
    and lr.lab_order_id = lo.lab_order_id
    and ltd.lab_test_definition_id = lr.lab_test_definition_id
  and hv.hospital_visit_id in 
        (select hv.hospital_visit_id
              from star.hospital_visit hv,
                star.mrn m,    
                star.location_visit lv,
                star.location l 
            where hv.mrn_id = m.mrn_id
            and hv.discharge_time is NULL
            and hv.admission_time is not NULL
            and lv.hospital_visit_id = hv.hospital_visit_id 
            and l.location_id = lv.location_id 
            and hv.admission_time > NOW() - INTERVAL '3 DAY' 
            and left(l.location_string, 3) ='ED^'
            and lv.discharge_time is null )"

sqlQuery <- gsub('\n','',sqlQuery)
lab_results <- data.table(dbGetQuery(ctn, sqlQuery))

# Check data --------------------------------------------------------------

# check for missing admission time in location
moves_now %>% filter(is.na(admission))
moves_now %>% filter(admission == discharge)
moves_now %>% filter(admission > discharge)

# # check for NA age eg patient had no record on demographics table
# summ_now %>% filter(is.na(age))

# Process summ data -------------------------------------------------------

# filter out under 18s
summ_now = summ_now %>% left_join(demog_raw) %>% 
  # clean records with birthdate of 1900-01-01
  mutate(age = case_when(date_of_birth <= "1915-01-01" ~ NA_integer_,
                         TRUE ~ as.integer(as.period(interval(start = date_of_birth, end = admission_time))$year)))

# flag under 18s but don't delete them
summ_now = summ_now %>% mutate(under_18 = case_when(age >= 18 ~ FALSE,
                                                    is.na(age) ~ FALSE, # retain patients with NA age for now
                                                    age <18 ~ TRUE))

# add visit history

summ_hist = summ_hist %>% left_join(patient_class_hist) 

prior_visits <- summ_hist %>% filter(!is.na(discharge_time)) %>% 
  select(mrn, csn, patient_class, admission_time, discharge_time) %>%  
  filter(patient_class %in% c("EMERGENCY", "INPATIENT"))  %>% 
  left_join(patient_class_hist) %>% 
  mutate(type = case_when(is.na(max_emerg_class) & patient_class == "INPATIENT" ~ "planned_inpatient",
                          !is.na(max_emerg_class) & patient_class == "INPATIENT" ~ "emergency_inpatient",
                          patient_class == "EMERGENCY" ~ "emergency_discharge"))

summ_now = summ_now %>% left_join(
  prior_visits %>% 
    group_by(mrn) %>% 
    summarise(num_adm_after_ED = sum(type == "emergency_inpatient"),
              num_ED_visits = sum(type %in% c("emergency_inpatient", "emergency_discharge"))), 
  by = "mrn"
)

# Process moves data ------------------------------------------------------------

moves_now <- moves_now %>% mutate(department = split_location(location_string, 1))
moves_now <- moves_now %>% mutate(room = split_location(location_string, 2))
moves_now <- moves_now %>% mutate(ED = case_when(department == "ED" | department == "UCHT00CDU" ~ 1,
                            TRUE ~ 0))
moves_now <- moves_now %>% mutate(room3 = clean_room_names(department, room))
moves_now <- moves_now %>% mutate(room4 = group_room_names(room3))
moves_now <- moves_now %>% mutate(duration_row = difftime(discharge, admission, units = "hours"))

# indicate whether row is OTF location
moves_now <- moves_now %>% mutate(OTF_row = case_when(room == "UCHED OTF POOL" ~ 1, TRUE ~ 0))

# update summ_now to mark any patients who are OTF now
summ_now = left_join(summ_now, 
                     moves_now %>% filter(OTF_row == 1, is.na(discharge)) %>% select(csn) %>% mutate(OTF_now = TRUE)
                     )

summ_now = summ_now %>% mutate(exclude = case_when(OTF_now ~ TRUE, 
                                        under_18 ~ TRUE,
                                        TRUE ~ FALSE))
# from create-data-tables.R
moves <- data.table(moves_now %>% anti_join(summ_now %>% filter(OTF_now | under_18) %>% select(csn)) %>% 
  mutate(location = case_when(department == "ED" & room4 == "TRIAGE" ~ "Waiting",
                            TRUE ~ room4)) %>% 
  select(csn, admission, discharge, department, location) %>% 
  arrange(csn, admission))

setkey(moves, csn)
moves[, "ED" := if_else(department == "ED", 1, 0)]

# Deal with repeated locations 
# update with next row's discharge time when locations are repeated
cols = c("csn","admission","discharge", "department", "location")
leadcols = paste("lead", cols, sep="_")
lagcols = paste("lag", cols, sep="_")
moves[, (leadcols) := shift(.SD, 1, NA, "lead"), .SDcols=cols]

# find rows where locations are repeated so the first one can be dropped
moves[csn == lead_csn & location == lead_location, drop_row := TRUE]
# remove rows where location is repeated
moves <- moves[is.na(drop_row)]

# update admission date of the remaining row
moves[, (lagcols) := shift(.SD, 1, NA, "lag"), .SDcols=cols]
moves[csn == lag_csn & admission != lag_discharge, amend_row := TRUE]
rpt(moves[(amend_row)])
moves[csn == lag_csn & admission != lag_discharge, admission := lag_discharge]

# remove any lag and lead rows for avoidance of error
set(moves, NULL , c("drop_row", "amend_row", 
                    "lag_csn", "lag_location", "lag_department", "lag_admission", "lag_discharge",  
                    "lead_csn", "lead_location", "lead_department", "lead_admission", "lead_discharge"), NULL)


# get first ED rows
moves[ED == 1, first_ED := if_else(admission == min(admission, na.rm = TRUE), TRUE, FALSE), by = csn]
first_ED_ = unique(moves[(first_ED), list(csn, admission)])
setnames(first_ED_, "admission", "first_ED_admission")
moves = merge(moves, first_ED_, all.x = TRUE)
rm(first_ED_)

moves[, "outside" := department != "ED"]

# added unique summ here as there are duplicate rows on summ_now - need to fix
summ <- merge(data.table(unique(summ_now) %>% select(-OTF_now, -exclude)), (unique(moves[,.(csn, first_ED_admission)])), by = "csn")

# Process lab data --------------------------------------------------------

setkey(lab_orders, csn)
lab_orders_real <- merge(lab_orders, summ[,.(csn, first_ED_admission)], by = "csn") 

setkey(lab_results, csn)
lab_results_real <- merge(lab_results, summ[,.(csn, first_ED_admission)]) 

# add elapsed time
lab_orders_real[, elapsed_mins := as.numeric(difftime(request_datetime, first_ED_admission, units = "mins"))]
lab_results_real[, elapsed_mins := as.numeric(difftime(result_last_modified_time, first_ED_admission, units = "mins"))] 

# remove obs from prior to ED by more than 2 hours
lab_orders_real <- lab_orders_real[elapsed_mins >= -120]
lab_results_real <- lab_results_real[elapsed_mins >= -120]

# create out of range values
lab_results_real <- lab_results_real %>% 
  mutate(oor_low = value_as_real < range_low,
         oor_high = value_as_real > range_high,
         abnormal = abnormal_flag == "A")


lab_results_real[, lab_results_real := case_when(is.na(value_as_real) & abnormal_flag == "A" ~ 1,
                                                 TRUE ~ value_as_real
)]

# remove lab battery orders not included in ML model
load("~/EDcrowding/real-time/data-raw/lab_orders_to_include.rda")
lab_orders_real = lab_orders_real[battery_code %in% lab_orders_to_include]


# Process obs data --------------------------------------------------------

obs_real <- data.table(obs)
setkey(obs_real, csn)
obs_real <- merge(obs_real, summ[,.(csn, first_ED_admission)]) 

# add elapsed time
obs_real[, elapsed_mins := as.numeric(difftime(observation_datetime, first_ED_admission, units = "mins"))]

# remove obs from prior to ED by more than 2 hours
obs_real <- obs_real[elapsed_mins >= -120]

# read mapped names for obs codes
vo_mapping <- read_csv("~/Emap Mapping Spreadsheet - all questions.csv") %>% data.table()
vo_mapping = vo_mapping[,.(`Friendly name`, `epic id`)]
setnames(vo_mapping, "Friendly name", "obs_name")
setnames(vo_mapping, "epic id", "id_in_application")
vo_mapping = unique(vo_mapping[, obs_name := max(obs_name), by = id_in_application])
vo_mapping[, obs_name := gsub(" ", "", obs_name)]

# add mapped names to obs data
obs_real[, id_in_application := as.numeric(id_in_application)]
obs_real = merge(obs_real, vo_mapping, by = "id_in_application", allow.cartesian=TRUE, all.x = TRUE)

# remove rows where there is no obs_name - ie not in mapping
obs_real = obs_real[(!is.na(obs_name))]
obs_real = obs_real[obs_name != "Reportedsymptomsonadmission"]

# Temperature measured in both celcius and farenheit
# Note that making a manual conversion will generate a different values to the original temp value
obs_real[obs_name == "Temperature", value_as_real := (value_as_real - 32) * 5/9]
obs_real[obs_name %in% c("Temperature", "Temp(inCelsius)"), num_temp_in_dttm := .N, by = .(csn, observation_datetime)]
obs_real[num_temp_in_dttm == 2 & obs_name == "Temperature", delete_row := TRUE]
obs_real = obs_real[is.na(delete_row)]
obs_real[obs_name == "Temperature", obs_name := "Temp(inCelsius)"]

# remove duplicate measurements (there are still a bunch with multiple temp measurements in one obs event)
obs_real[obs_name %in% c("Temperature", "Temp(inCelsius)"), num_temp_in_dttm := .N, by = .(csn, observation_datetime)]
obs_real[num_temp_in_dttm > 1]

# first delete cols id_in_application and visit_observation_type_id as these have diff values for celcius and f meas
obs_real[, id_in_application := NULL]
obs_real[, visit_observation_type_id := NULL]
obs_real = unique(obs_real)

# convert ACVPU to numeric
obs_real[, value_as_real := case_when(obs_name == "ACVPU" & value_as_text == "A" ~ 1,
                                      obs_name == "ACVPU" & value_as_text == "C" ~ 2,
                                      obs_name == "ACVPU" & value_as_text == "V" ~ 3,
                                      obs_name == "ACVPU" & value_as_text == "P" ~ 4,
                                      obs_name == "ACVPU" & value_as_text == "U" ~ 5,
                                      TRUE ~ value_as_real
)]

# convert Bloodpressure to numeric

Bloodpressure <- as_tibble(obs_real[obs_name == "Bloodpressure"]) %>% select(-value_as_real, -obs_name) %>% 
  separate(value_as_text, into = c("Bloodpressure_sys","Bloodpressure_dia"), sep = "/") %>% 
  mutate(Bloodpressure_sys = as.numeric(Bloodpressure_sys),
         Bloodpressure_dia = as.numeric(Bloodpressure_dia)) %>% 
  pivot_longer(Bloodpressure_sys:Bloodpressure_dia, names_to = "obs_name", values_to = "value_as_real"
  )

Bloodpressure <- Bloodpressure %>% mutate(value_as_text = NA)

obs_real <- bind_rows(obs_real[obs_name != "Bloodpressure"], Bloodpressure)

# convert resp assist type

obs_real[, value_as_real := case_when(obs_name == "Roomairoroxygen" & value_as_text == "Supplemental Oxygen" ~ 1,
                                      obs_name == "Roomairoroxygen" & value_as_text == "Room air" ~ 0,
                                      TRUE ~ value_as_real)]


# convert text to numeric where straightfoward
obs_real[obs_name == "NEWSscore", value_as_real := as.numeric(value_as_text)]
obs_real[obs_name == "NEWS2score", value_as_real := as.numeric(value_as_text)]
obs_real[obs_name == "RASS", value_as_real := as.numeric(value_as_text)]
obs_real[obs_name == "Painscore-verbalatrest", value_as_real := as.numeric(value_as_text)]
obs_real[obs_name == "Painscore-verbalonmovement", value_as_real := as.numeric(value_as_text)]

# remove outliers
obs_real <- obs_real %>%
  mutate(value_as_real = case_when(value_as_real >46 & obs_name == "Temp(inCelsius)" ~ NA_real_,
                                   TRUE ~ value_as_real))

# create final dataset of results (real values)
obs_real <- obs_real[, .(csn, observation_datetime, value_as_real, obs_name, elapsed_mins)]

# remove any punctuation that will make column names problematic
obs_real[, obs_name := gsub("\\(|\\)|\\-|\\>|\\?|\\/","", obs_name)]
obs_real[, obs_name := gsub("Currenttemperature>37.5orhistoryoffeverinthelast24hours","Fever", obs_name)]

load("~/EDcrowding/real-time/data-raw/obs_to_include.rda")
obs_real = obs_real[obs_name %in% obs_to_include]


# Generate timeslices -----------------------------------------------------

dm <- summ[,.(csn, age, sex, presentation_time, arrival_method, first_ED_admission, patient_class,
              num_prior_adm_after_ED = num_adm_after_ED, num_prior_ED_visits = num_ED_visits)]

dm[, tod := factor((hour(presentation_time) %/% 4)+1)]
dm[, quarter := factor(case_when( month(presentation_time) <= 3 ~ 1,
                                  month(presentation_time) <= 6 ~ 2, 
                                  month(presentation_time) <= 9 ~ 3, 
                                  month(presentation_time) <= 12 ~ 4))]
#dm[, year := factor(year(presentation_time))]
dm[, weekend := factor(if_else(weekdays(presentation_time, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0))]
# the lab closes at 10 pm 
dm[, night := factor(ifelse(hour(presentation_time) < 22 & hour(presentation_time) > 7, 0, 1))]
dm[, gt70 := factor(age >= 70)]
dm[, sex := factor(sex)]

# was an inpatient, as part of this visit, before ED
dm[, inpatient := if_else(patient_class == "INPATIENT", 1, 0)]
dm[, inpatient := factor(inpatient)]

# include a feature to capture time delay between presentation and arrival at ED;
dm[, beforeED := as.numeric(difftime(first_ED_admission, presentation_time, units = "mins"))]
# small number have negative time for before ED
dm[, beforeED := if_else(beforeED <0, 0, beforeED) ]

dm[, duration := as.numeric(difftime(time_of_extract, first_ED_admission, units = "mins"))]


# simplify arrival method
dm[, arrival := gsub(" |-", "", arrival_method)]
dm[, arrival := factor(if_else(!arrival %in% c("Ambulance",
                                               "Walkin",
                                               "PublicTrans",
                                               "Ambnomedic"), "Other", arrival)) ]
dm[, arrival_method := NULL]
setkey(dm, csn)



# Prepare location data --------------------------------------------------

loc <- moves[csn %in% dm$csn, .(csn, admission, discharge, location, visited_CDU = FALSE, outside)]
loc <- merge(loc, dm[,.(csn, first_ED_admission, duration)])

# remove rows where admission occurs to locations before arrival at ED
loc <- loc[admission >= first_ED_admission]


loc[, admission_e := as.numeric(difftime(admission, first_ED_admission, units = "mins"))]
loc[, discharge_e := as.numeric(difftime(discharge, first_ED_admission, units = "mins"))]

loc[, c("admission", "discharge", "first_ED_admission") := NULL]
dm[, c("presentation_time", "first_ED_admission") := NULL]

cols = colnames(copy(dm)[, c("csn","duration") := NULL])
cols_ = paste("a", cols, sep="_")
setnames(dm, cols, cols_)

setkey(loc, csn)

# Prepare obs and lab data
obs_real <- merge(obs_real, dm[,.(csn, duration)], by = "csn")
lab_orders_real <- merge(lab_orders_real, dm[,.(csn, duration)], by = "csn")
lab_results_real <- merge(lab_results_real, dm[,.(csn, duration)], by = "csn")

# Create timeslices -------------------------------------------------------


timeslices <- c(0, 15, 30, 60, 90, 120, 180, 240, 300, 360, 480, 480+4*60)

timeslices_to_predict <- as.character()

for (i in seq(1, length(timeslices) -1, 1)) {
  print(paste0("Processing timeslice ", timeslices[i]))
  
  if (nrow(dm[duration >timeslices[i] & duration < timeslices[i+1]]) > 0) {
    ts_ <- case_when(nchar(as.character(timeslices[i])) == 1 ~ paste0("00", timeslices[i]),
                         nchar(as.character(timeslices[i])) == 2 ~ paste0("0", timeslices[i]),
                         TRUE ~ as.character(timeslices[i]))
    name_ <- paste0("dm", ts_)
    ts <- create_timeslice(loc, dm, obs_real, lab_orders_real, lab_results_real, timeslices[i], timeslices[i+1])

    assign(name_, ts)
    timeslices_to_predict = c(timeslices_to_predict, name_)
  }
}




# Make predictions --------------------------------------------------------

model_date = '2021-04-29'

pred_file = paste0("~/EDcrowding/real-time/data-output/real_time_preds.rda")

if (file.exists(pred_file)) {
  load(pred_file)
} else {
  preds_all_ts <- data.table()
}

for (ts_ in timeslices_to_predict) {

  name_ts <- ts_
  dt = get(name_ts)
  
  # encode factors
  ts <- one_hot(cols = "auto", dt = as.data.table(dt),  dropUnusedLevels=TRUE)
  name_tsk <- gsub("dm", "task", ts_)
  
  # add other features that might be missing
  inFile = paste0("~/EDcrowding/predict-admission/data-output/features_", name_tsk, "_", model_date, ".rda")
  load(inFile)
  
  ts_cols = colnames(ts)
  missing_features = feature_list[!feature_list %in% ts_cols] # load this from file later
  
  missing_cols <- data.table(matrix(0, nrow = nrow(ts), ncol = length(missing_features)))
  ts = bind_cols(ts, missing_cols)
  colnames(ts) = c(ts_cols, missing_features)

  # load learner
  inFile = paste0("~/EDcrowding/predict-admission/data-output/learner_", name_tsk, "_", model_date, ".rda")
  load(inFile)
  
  # get predictions
  pred_values = as.data.table(predict(learner, ts, predict_type = "prob"))
  setnames(pred_values, c("1", "0"), c("prob.1", "prob.0"))
  pred_values$timeslice = name_tsk
  pred_values[, csn:= ts$csn]
  pred_values[, extract_dttm := time_of_extract]
  
  preds_all_ts <- bind_rows(preds_all_ts, pred_values)
  
  
}



save(preds_all_ts, file = pred_file)


# Predict distribution ----------------------------------------------------

# load poisson means for not-yet-arrived
poisson_file = "~/EDcrowding/real-time/data-output/poisson_means.rda"
load(poisson_file)

# load probabilities of time to admit
tta_hr_file = "EDcrowding/real-time/data-raw/tta_prob.rda"
load(tta_hr_file)


get_report = case_when(as.numeric(substr(time_of_extract, 12,13)) < 6 ~ "6:00",
                       as.numeric(substr(time_of_extract, 12,13)) < 12 ~ "12:00",
                       as.numeric(substr(time_of_extract, 12,13)) < 16 ~ "16:00",
                       TRUE ~ "22:00")

is_weekend = if_else(weekdays(time_of_extract, abbreviate = TRUE) %in% c("Sun", "Sat"), 1,0)


poisson_means = poisson_means[epoch == "Post" & in_set == "Train" & time_of_report == get_report & weekend == is_weekend]
tta_probs = tta_prob_2[epoch == "Post" & in_set == "Train" & time_of_report == get_report]

distr_all = get_prob_dist(time_window = NA, preds_all_ts[extract_dttm == time_of_extract], poisson_means, tta_probs)

distr_all = data.table(distr_all)

distr_all[, N := case_when(num_adm_pred > 35 ~ "> 35",
                           num_adm_pred < 5 ~ "< 5",
                           TRUE ~ as.character(num_adm_pred))]
distr_all[, N := factor(N, levels = c("< 5", as.character(seq(5, 35, 1)), "> 35"))]
unique(distr_all$dist)


# Charts ------------------------------------------------------------------

# get patients admitted yesterday - this may include inpatients who were rerouted to ED

sqlQuery <- paste(" select count(*) from star.hospital_visit hv where
        hv.hospital_visit_id in (
            select distinct hospital_visit_id
              from star.location_visit lv,
                   star.location l
              where left(l.location_string, 3) ='ED^'
                      and lv.admission_time > '", substr(time_of_extract - days(1), 1,10), "00:00:00'
                      and lv.admission_time <= '", substr(time_of_extract - days(1), 1,10), "23:59:59' 
                  and l.location_id = lv.location_id   ) and patient_class = 'INPATIENT';")

sqlQuery <- gsub('\n','',sqlQuery)
adm_yest <- as_tibble(dbGetQuery(ctn, sqlQuery))

# get patients admitted last 24 hours

sqlQuery <- paste(" select count(*) from star.hospital_visit hv where
        hv.hospital_visit_id in (
            select distinct hospital_visit_id
              from star.location_visit lv,
                   star.location l
              where left(l.location_string, 3) ='ED^'
                      and lv.admission_time > '", time_of_extract - days(1), "'
                      and lv.admission_time <= '", time_of_extract,  "'
                  and l.location_id = lv.location_id   ) and patient_class = 'INPATIENT';")

sqlQuery <- gsub('\n','',sqlQuery)
adm_last_24 <- as_tibble(dbGetQuery(ctn, sqlQuery))


moves_now = moves_now %>% mutate(room5 = case_when(room4 == "TRIAGE" ~ "Waiting",
                                       TRUE ~ room4)) %>% 
  mutate(room5 = factor(room5, levels = c("Waiting", "RAT", "MAJORS", "RESUS", "UTC", "SDEC", "TAF", "PAEDS", "OTF"),
                        labels = c("Waiting/TRIAGE", "RAT", "MAJORS", "RESUS", "UTC", "SDEC", "TAF", "PAEDS", "OTF")))

# chart in hours - last 24 hours
moves_now %>% filter(is.na(discharge)) %>% 
  left_join(unique(summ_now %>% select(csn, time_since_arrival, OTF_now, under_18)), by = "csn") %>% 
  filter(time_since_arrival <= 24*60) %>% 
  mutate(time_since_arrival = time_since_arrival/60) %>% 
  ggplot(aes(x = as.numeric(time_since_arrival), y = fct_rev(room5))) + geom_point(size = 4, aes(shape = under_18)) +
  labs(y = "Current location", x = "Hours since arrival",
       title = paste("Patients in ED at ", substr(time_of_extract, 1, 16), "- arrivals in last 24 hours")) +
  theme_grey(base_size = 18) +
  # geom_vline(xintercept = timeslices/60)  + theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(0,24,4), limits = c(0,24)) +
  annotate("text", x = 16, y = "Waiting/TRIAGE", 
           label = paste("Number in ED now:", nrow(summ), "\nAdmissions yesterday :", adm_yest$count, "\nAdmissions last 24 hrs:",  adm_last_24$count),
           size = 6)


# chart in hours - last 3 days
moves_now %>% filter(is.na(discharge)) %>% 
  left_join(unique(summ_now %>% select(csn, time_since_arrival, OTF_now, under_18)), by = "csn") %>% 
  mutate(time_since_arrival = time_since_arrival/60) %>% 
  ggplot(aes(x = as.numeric(time_since_arrival), y = fct_rev(room5))) + geom_point(size = 4, aes(shape = under_18)) +
  labs(y = "Current location", x = "Hours since arrival",
       title = paste("Patients in ED at ", substr(time_of_extract, 1, 16), "- arrivals in last 3 days")) +
  theme_grey(base_size = 18) +
  # geom_vline(xintercept = timeslices/60)  + theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(0,24*3,8)) +
  annotate("text", x = 16, y = "Waiting/TRIAGE", 
           label = paste("Number in ED now:", nrow(summ), "\nAdmissions yesterday :", adm_yest$count, "\nAdmissions last 24 hrs:",  adm_last_24$count),
           size = 6)

# chart with predictions
p1 = moves_now %>% filter(is.na(discharge)) %>% 
  left_join(unique(summ_now %>% select(csn, time_since_arrival, OTF_now, under_18)), by = "csn") %>% 
  mutate(extract_dttm = time_of_extract) %>% 
  left_join(preds_all_ts, by = c("csn", "extract_dttm"))  %>% 
  filter(time_since_arrival <= 24*60) %>% 
  mutate(time_since_arrival = time_since_arrival/60) %>% 
  ggplot(aes(x = as.numeric(time_since_arrival), y = fct_rev(room5))) + geom_point(size = 4, aes(shape = under_18, col = prob.1)) +
  labs(y = "Current location", x = "Hours since arrival",
       title = paste("Individual probability of admission for all patients in ED at ", substr(time_of_extract, 1, 16)),
       col = "Probability of admission", 
       shape = "Under 18") +
  theme_grey(base_size = 18) +
  # geom_vline(xintercept = timeslices/60)  + theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(0,24,1)) +
  scale_colour_gradient2(low = "blue", mid = "yellow", high = "red", limits = c(0,1), breaks = c(0,0.5, 1)) + 
  theme(legend.text=element_text(size=10)) + 
  geom_vline(xintercept = timeslices/60, linetype = "dashed")

p1
p2 = distr_all %>% filter(grepl("Including", dist)) %>% 
  mutate(dist = gsub("Including not yet arrived; a", "A", dist)) %>% 
  ggplot(aes(x = N,y = 1, fill = probs)) + geom_tile() +
  theme_grey(base_size = 14)+ 
  scale_fill_gradient(low="white", high="blue", breaks = c(0, .05, .1, 0.15))   + theme(axis.title.y=element_blank(),
                                                                                        axis.text.y=element_blank(),
                                                                                        axis.ticks.y=element_blank()) +
  labs(title = "Predicted number of admissions from ED (including patients who have not yet arrived)",
       fill = "Probability of\nthis number of beds", 
       x = "Number of admissions") +
  theme(legend.position = "bottom") +
  facet_wrap(dist~ ., nrow = 5) 


library(gridExtra)
library(cowplot)
plot_grid(p1, p2 +theme(legend.position = "right"), nrow = 2, rel_heights = c(2/3, 1/3))

# Evaluate ----------------------------------------------------------------

# get patients admitted yesterday - this may include inpatients who were rerouted to ED

sqlQuery <- paste(" select count(*) from star.hospital_visit hv where
        hv.hospital_visit_id in (
            select distinct hospital_visit_id
              from star.location_visit lv,
                   star.location l
              where left(l.location_string, 3) ='ED^'
                      and lv.admission_time > '", substr(time_of_extract - days(1), 1,10), "00:00:00'
                      and lv.admission_time <= '", substr(time_of_extract - days(1), 1,10), "23:59:59' 
                  and l.location_id = lv.location_id   ) and patient_class = 'INPATIENT';")

sqlQuery <- gsub('\n','',sqlQuery)
adm_yest <- as_tibble(dbGetQuery(ctn, sqlQuery))

# get patients admitted last 24 hours

sqlQuery <- paste(" select count(*) from star.hospital_visit hv where
        hv.hospital_visit_id in (
            select distinct hospital_visit_id
              from star.location_visit lv,
                   star.location l
              where left(l.location_string, 3) ='ED^'
                      and lv.admission_time > '", time_of_extract - days(1), "'
                      and lv.admission_time <= '", time_of_extract,  "'
                  and l.location_id = lv.location_id   ) and patient_class = 'INPATIENT';")

sqlQuery <- gsub('\n','',sqlQuery)
adm_last_24 <- as_tibble(dbGetQuery(ctn, sqlQuery))


# Investigate one observation -----------------------------------------------------------

library(DALEX)
library(DALEXtra)

# set identify ts to explain
explain_ts = "240"
dt = dm240



# encode factors
ts <- one_hot(cols = "auto", dt = as.data.table(dt),  dropUnusedLevels=TRUE)
name_tsk <- paste0("task", explain_ts)

# add other features that might be missing
inFile = paste0("~/EDcrowding/predict-admission/data-output/features_", name_tsk, "_", model_date, ".rda")
load(inFile)

ts_cols = colnames(ts)
missing_features = feature_list[!feature_list %in% ts_cols] # load this from file later

missing_cols <- data.table(matrix(0, nrow = nrow(ts), ncol = length(missing_features)))
ts = bind_cols(ts, missing_cols)
colnames(ts) = c(ts_cols, missing_features)

# load learner
inFile = paste0("~/EDcrowding/predict-admission/data-output/learner_", name_tsk, "_", model_date, ".rda")
load(inFile)


# create explainer for this timeslice
explainer <- explain_mlr3(learner, 
                           data     = ts[, csn := NULL],
                           label    = paste0("task", as.character(explain_ts)),
                           colorize = FALSE)

# pick an example by looking at timeslice
preds_all_ts[extract_dttm == time_of_extract & timeslice == paste0("task", as.character(explain_ts))]

explain_csn_row = ts[3,]
bd = predict_parts(explainer,
              new_observation = explain_csn_row)


bd = data.table(bd)

bd[, sign_ := case_when(variable == "intercept" ~ 'x',
                        sign == "1" ~ "3",
                        sign == "3" ~ "1",
                        TRUE ~ as.character(sign))]



# possible waterfall chart here: https://gist.github.com/rentrop/36f07b67cb6c4b82088e2115fee2498f

bd[, abs_contribution := abs(contribution)]
plot_data = bd[!variable %in% c( "prediction", "intercept")]
setorder(plot_data, -abs_contribution)
plot_data = plot_data[ 1:12]

prediction = bd[variable == "prediction"]
intercept = bd[variable == "intercept"]
rest = sum(plot_data$contribution) - prediction$contribution + intercept$contribution


p = bind_rows(intercept, plot_data, data.table(variable = "+ all other factors",
                                                           contribution = -rest,
                                                           sign_ = case_when(rest > 0 ~ "-1",
                                                                             rest <= 0 ~ "3")),
              prediction)

# setorder(plot_data, -abs_contribution)
p[, contribution_ := case_when(variable == "intercept" ~ 1 - contribution,
                               variable == "prediction" ~ 1 - contribution,
                               TRUE ~ contribution * -1)]
p[, end := cumsum(contribution_)]
p[, start := case_when(variable == "intercept" ~ 0,
                               TRUE ~ lag(end))]
p[, id := 16-seq(1:nrow(p))]
p[id == 1, end := 0]
p[id == 1, start := contribution_]

# p__ = bind_rows(p, data.table(variable_ = "prediction", 
#                               contribution_ = prediction, 
#                               end = 0, 
#                               start = p[12,end], id = 0,
#                               sign_ = 'x'))


p[, variable_ := factor(variable, levels = c(p$variable)) ]

pl = p %>% ggplot(aes(x = reorder(variable_, desc(variable_)), fill = as.factor(sign_))) +
  geom_rect(aes(x = reorder(variable_, desc(variable_)), xmin = id -0.45, xmax = id + 0.45, ymin = end, ymax =start)) + xlab("") +
  theme(legend.position = "none") + 
  # geom_hline(yintercept = intercept) + 
  # geom_hline(yintercept = prediction) + 
  coord_flip()

for (i in 1:14) {
  pl = pl + annotate(geom = "segment", x = p$id[i] + .25, xend =  p$id[i] - 1.25, y = p$end[i], yend = p$end[i])
  pl = pl + annotate(geom = "text", x = p$id[i], y = (p$start[i]+p$end[i])/2, label = round(p$contribution_[i],2), size = 4)
}

pl = pl + annotate(geom = "text", x = p$id[15], y = (p$start[15]+p$end[15])/2, label = round(p$contribution_[15],3), size = 4)

pl + labs(title = paste0("Break down plot for patient in timeslice ",explain_ts, " and location ", moves[(is.na(discharge)) & (csn == explain_csn_row$csn), location]))


