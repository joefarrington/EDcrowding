library(dplyr)
library(lubridate)

madcap = function(pred){
  # ref to MADCAP section in Model Evaluation.docx
  new_pred = pred[order(pred$.pred_TRUE),] # step 1
  
  truth = as.numeric(new_pred$truth)-1
  
  L = 20000 #nrow(new_pred)
  
  y1 = c()
  y2 = c()
  x = c(1:L)
  model = 0 
  real = 0
  for (i in 1:L){ # step 2
    
    # s = new_pred[1:i,]
    # print(new_pred$.pred_TRUE[i])
    model = model + new_pred$.pred_TRUE[i] # step 4: getting the expectation from predictions
                                           # step 3 not actually necessary,
    real = real + as.numeric(new_pred$truth[i])-1
    y1 = c(y1, model)
    y2 = c(y2, real)
    
  }
  
  return(data.frame(x=x,y1=y1,y2=y2))
}


dist_eval = function(preds){
  
  adm = c()
  for (i in 1:ncol(preds)){
    adm = c(adm,sum(preds[,2*(i-1)+1]))
    distr_collect[[i]] = poly_prod(preds[,2*i])
  }
  
  return(eval_model(adm,distr_collect))
  
}

poly_prod = function(df){
  probs = df[,4] # get column of probabilities for admission
  
  # polynomial(vector) is a polynomial t
  y = polynomial(c(1,0))# 
  for (n in 1:nrow(df)){
    y = y*polynomial(c(1-probs[n],probs[n]))
  }
  return(coef(y))
}

eval_model = function(count,distr_collect){
  # refer to Admission probability distribution evaluation in Model Evaluation.docx
  
  # change all distr to cdfs
  cdf_collect = pdf_to_cdf(distr_collect)  # step 1
  # for each threshold alpha, cycle through all distr and get corresponding number of adm
  M = 100
  D = 100
  model_1 = c()
  model_2 = c()
  model_3 = c()
  real_1 = c()
  real_2 = c()
  real_3 = c()
  for (i in 1:M){
    alpha = i*1/M
    real_low = 0
    real_mid = 0
    real_hi = 0
    model_low = 0
    model_mid = 0
    model_hi = 0
    for (j in 1:D){
      
      distr = distr_collect[[j]]
      cdf = cdf_collect[[j]]
      # step 2
      if (alpha < 1){
        thresh_N = threshold_to_N(cdf,alpha) 
      } else {
        thresh_N = c(length(distr)-1,length(distr),length(distr))
      }
      ## for lower, alpha corresponds to lower
      ## for mid, alpha corresponds to lower if alpha is less than midpoint between lower and upper level
      ## for high, alpha corresponds to higher
      
      # for each day, add one if adm count is less than threshold num of adm and zero if not
      
      #step 3
      if (count[j]<thresh_N[1]){
        real_low = real_low + 1/D
      }
      if (count[j]<thresh_N[2]){
        real_mid = real_mid + 1/D
      }
      if (count[j]<thresh_N[3]){
        real_hi = real_hi + 1/D
      }
      
      model_low = model_low + cdf[thresh_N[1]]/D
      model_mid = model_mid + cdf[thresh_N[2]]/D
      model_hi = model_hi + cdf[thresh_N[3]]/D
    }
    #step 4: taking average was incorporated into step 3
    
    real_1 = c(real_1, real_low)
    real_2 = c(real_2, real_mid)
    real_3 = c(real_3, real_hi)
    model_1 = c(model_1, model_low)
    model_2 = c(model_2, model_mid)
    model_3 = c(model_3, model_hi)
    
  }
  result = data.frame(x = c(1:M)/M,r_low=real_1,r_mid=real_2,r_hi=real_3,
                      m_low=model_1,m_mid=model_2,m_hi=model_3)
}

pdf_to_cdf = function(distr_collect){
  L = length(distr_collect)
  cdf_collect = array(list())
  for (i in 1:L){
    distr=distr_collect[[i]]
    cdf = c()
    for (j in 1:length(distr)){
      cdf = c(cdf,sum(distr[1:j]))
    }
    cdf_collect[[i]] = cdf
  }
  return(cdf_collect)
}

threshold_to_N = function(cdf,alpha){
  s=0
  i=0
  low = 0
  mid = 0
  hi = 0
  while (s < alpha){
    # print(s)
    i=i+1
    s = cdf[i]
  }
  # print(alpha)
  low = i-1
  hi = i
  
  if (i==1){
    if(alpha<0.5*cdf[i]){
      mid=i-1
    }else{
      mid = i
    }
  } else if (alpha < 0.5*(cdf[i-1]+cdf[i])){
    mid = i-1
  } else {
    mid = i
  }
  
  return(c(low,mid,hi))
}



stats_all <- function(stages,area){
  # given data and area, find the usage per 6 hours from 6 am
  
  stage1 <- stages %>% filter(Area==area) %>% group_by('StageStart')
  if (nrow(stage1)!=0){
    stage2 = stage1[with(stage1,order(stage1$StageStart)),]
    
    min_time = as_datetime(date(min(stage2$StageStart)),tz="UTC")
    max_time = date(max(stage2$StageStart))+1
    max_time = as_datetime(max_time,tz="UTC")
    
    # count number of visits in six hour intervals from min_time to max_time
    count_all = c()
    time_intervals = 4*as.numeric(max_time-min_time)
    for (i in 1:time_intervals){
      t1 = min_time + (i-1)*60*60*6
      t2 = min_time + i*60*60*6
      
      stage3 = stage2 %>% filter(difftime(t2,StageStart)>0 & difftime(StageStart,t1)>0)
      
      count_all = c(count_all,nrow(stage3))
    }
    return(tail(count_all,600))
  } else{c()}
}

stats_avg <- function(count){
  
  # take average of visits at the four different times of day
  
  if (length(count)!=0){
    night = c()
    morn = c()
    aftern = c()
    evening = c()
    
    L=length(count)/4
    
    for (i in 1:(L-1)){
      night = c(night,count[(1+(i-1)*4)])
      morn = c(morn,count[(2+(i-1)*4)])
      aftern = c(aftern,count[(3+(i-1)*4)])
      evening = c(evening,count[(4+(i-1)*4)])
    }
    avg = c(mean(night),mean(morn),mean(aftern),mean(evening))
    return(avg)} else{ c()}
}

mult_visits <- function(stages,areas){
  # input data and list of areas
  # output: for each area, count how many patients visit only n times in one visit
  # how: group by visitID, then by Area, then count
  
  stage1 = stages %>% group_by(VisitID,Area)
  
  id_area_count = stages %>% group_by(VisitID,Area) %>% summarise(count=n())
  
  L=length(areas)
  colnames(id_area_count) <- c("VisitID","Area","No. Visits")
  
  M=id_area_count %>% group_by(Area,`No. Visits`) %>% summarise(count=n())
  
  for (i in 1:1){
    area_visits = M %>% filter(Area == areas[i])
    total_patients = sum(area_visits$count)
    area_visits$count = area_visits$count / total_patients * 100
    print(area_visits)
    bp = barplot(area_visits$count, width = 0.5, main=area_visits$Area[1], xlim=c(0,4),ylim=c(0,100),names.arg=c(1:nrow(area_visits)))
    text(bp,60,round(area_visits$count,3),cex=1,pos=1)
  }
  
  return
}

corr_adm = function(stages,area){
  #input: stages and list of areas
  #output: for a specific area, yield vector such that n-th entry equals percentage of admission for patients that visited area only n times
  #method: get visitID for n visits, then count number of these visitID's that end in admission
  
  id_area_count = stages %>% filter(Area==area) %>% group_by(VisitID) %>% summarise(count=n())
  adm_corr = c()
  patient_count=c()
  for (i in 1:max(id_area_count$count)){
    VID = id_area_count %>% filter(count==i)
    stage1 = stages %>% filter(VisitID %in% VID$VisitID) %>% group_by(VisitID,Transfer) %>% summarise(count=n())
    vid_count = length(unique(stage1$VisitID))
    adms = (stage1 %>% filter(!(Transfer %in% c('Arrival-Discharge','Discharged','Transfer','Arrival'))))
    N=vid_count
    M=sum(adms$count)
    patient_count = c(patient_count,N)
    adm_corr = c(adm_corr,M/N)
  }
  
  return(data.frame(patient_count,adm_corr))
}

count_daily_adm = function(stages){
  # count the daily admission from beginning to end
  
  min_date = as.Date(min(stages$StageStart))
  max_date = as.Date(max(stages$StageEnd))
  
  days = max_date-min_date
  adm_vid = stages %>% filter(!is.element(Transfer,c('Arrival','Discharged','Transfer','Arrival-Discharge')))
  adm_vid = adm_vid$VisitID
  for (i in 1:days){
    daily = stages %>% filter(is.element(VisitID,adm_vid),StageStart>=min_date+i-1,StageEnd<min_date+i)
    adm = c(adm,nrow(daily))
  }
  
  return(adm)
}







