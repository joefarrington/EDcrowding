### convert predictions to distribution
### then perform evaluations
library(dplyr)
library(lubridate)
library(polynom)



# Create functions --------------------------------------------------------


madcap = function(pred){
  # ref to MADCAP section in Model Evaluation.docx
  new_pred = pred[order(pred$.pred_TRUE),] # step 1
  
  truth = as.numeric(new_pred$truth)-1
  
  L = nrow(new_pred)
  
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
    adm = c(adm,sum(preds[,2*(i-1)+1])) # i = 1 = first col # i = 2 - third col # i = 3 - fifth col # generates sum of number admitted
    distr_collect[[i]] = poly_prod(preds[,2*i]) # collects probability density for the probs in cols 2 4 6 etc
  }
  
  return(eval_model(adm,distr_collect))
  
}

poly_prod = function(df){
  probs = df[,4][order(df[,4])] # get column of probabilities for admission
  
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
  cdf_collect = pdf_to_cdf(distr_collect_)  # step 1
  # for each threshold alpha, cycle through all distr and get corresponding number of adm
  M = 10
  D = length(count_)
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
      
      distr_ = distr_collect_[[j]]
      cdf = cdf_collect[[j]]
      # step 2
      if (alpha < 1){
        thresh_N = threshold_to_N(cdf,alpha) 
      } else {
        thresh_N = c(length(distr_)-1,length(distr_),length(distr_))
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
  hi = i # note - Enoch adds 1 to i here, rather than setting the higher band at the next alpha threshhold for the cdf
  
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



#------------------------------------------------------------
#------------------------------------------------------------
#evaluate
# given a prediction matrix, we will evaluate how good it is
# In this program, a prediction matrix, which offers individual predictions (probability of TRUE/FALSE)
# is used to create a probability density function of the number of patients admitted i.e.
# p(n) = probability that n patients will be admitted.
#
# Then we use the hosmer-lemeshow test and MADCAP test as its performance test. (More can/will be added)
#
# 1. reading prediction matrix from prediction.csv, which is output from xgboost program
# 2. run poly_prod to get a prob density function (be careful with the number of rows in a prediction)
# 3. run hosmer-lemeshow and obtain a single number statistic. performance is better when lower
# 4. run MADCAP and obtain plot
#
#------------------------------------------------------------
#------------------------------------------------------------

load("~/EDcrowding/predict-admission/data-raw/matrix_60_2020-11-09.rda")
# for now load proc_dm_train via other script to get date and include its csn
load("~/EDcrowding/flow-mapping/data-raw/ED_csn_summ_all_2020-10-14.rda")

pred_with_csn <- bind_cols(pred, proc_dm_train$csn)
pred_with_csn <- pred_with_csn %>% rename(csn = ...5)
pred_with_csn <- pred_with_csn %>% left_join(ED_csn_summ %>% select(csn, arrival_dttm))
pred_with_csn <- pred_with_csn %>% mutate(date = date(arrival_dttm))

save(pred_with_csn, file = "~/EDcrowding/predict-admission/data-raw/pred_with_csn_60_2020-11-09.rda")

# Simple test of the poly prod function ---------------------------------------------

load("~/EDcrowding/predict-admission/data-raw/pred_with_csn_60_2020-11-09.rda")
date_range = seq(date("2020-08-01"), date("2020-08-31"), length = 100)
i = 1
# get dataframe of predictions for 1 August
df <- as.data.frame(pred_with_csn %>% filter(date == as.character(date_range[i])) %>% select(truth:.pred_TRUE))
probs = df[,4][order(df[,4])]

# show the individual predictions
probs %>% plot()
probs %>% ecdf() %>% plot()

# show the distribution 
pp <- poly_prod(df)
pp %>% plot()
pp %>% ecdf() %>% plot()

# End ----------------------------------------------------------------------



### predictions to distribution -----------------------------
# Using individual admission probabilities, create a prob density function
# for the number of patients admitted from the prediction


# -------------------------------------------------------
### distribution evaluation -----------------------------
### requires as input a series of predictions. each prediction is of format 
### (truth, prediction, 2 probabilities). to apply a prepared function that would generate 
### the curves, the format of the input for the series of predictions should be a variable preds with format
### (truth_1, probability of admission 1, truth_2, probability of admission 2, ... , truth_N, probability of admission N)


# generate using the xgboost model the series of predictions and change to necessary format (leave this to you, sorry)

# create date range to analyse
date_range = seq(date("2020-05-31"), date("2020-09-01"), length = 94)

distr_collect_ = list()
count_ = c()

for (i in 1:length(date_range)) {
  # note sure why I had to add the as.character ??
  df <- as.data.frame(pred_with_csn %>% filter(date == as.character(date_range[i])) %>% select(truth:.pred_TRUE))
  distr_collect_[[i]] <- poly_prod(df)
  count_ <- c(count_,sum(as.numeric(df$truth)-1))
}


# plug into the function

result = eval_model(count_,distr_collect_)

library(ggplot2)
print(result)

# plot the curves
p = ggplot(data=result, aes(x,y)) + geom_point(aes(y=r_low,color='data: mid approx'),size=0.8) +
  geom_point(aes(x=x,y=m_low,color="model: lower approx"),size=0.8) +
  geom_point(aes(x=x, y=r_mid,color="data: lower approx"),size=0.8) +
  geom_point(aes(y=m_mid,color='model: mid approx'),alpha=0.7,size=0.8) +
  geom_point(aes(y=r_hi,color='data: hi approx'),size=0.8) +
  geom_point(aes(y=m_hi,color='model: hi approx'),alpha=0.3,size=0.8)+
  scale_color_manual(labels=c('data:','2','3'),values = c('green','orange','red','black','black','black'))


print(p + labs(x="confidence",y="proportion of cases under bound") + theme(legend.title = element_blank()))

classColor = c('green','red','orange','black','black','black')
className = c('r_low','r_mid','r_hi','m_low','m_mid','m_hi')

p = ggplot(data=result, aes(x,y)) + geom_point(aes(y=r_low,color='data: low approx'),size=1) +
  geom_point(aes(x=x,y=m_low,color="model: low approx"),size=1) +
  geom_point(aes(x=x, y=r_mid,color="data: mid approx"),size=1) +
  geom_point(aes(y=m_mid,color='model: mid approx'),size=1) +
  geom_point(aes(y=r_hi,color='data: hi approx'),size=1) +
  geom_point(aes(y=m_hi,color='model: hi approx'),size=1)+
  scale_color_manual(breaks=c("data: low approx",'data: mid approx','data: hi approx',"model: low approx",'model: mid approx', 'model: hi approx'),values = c("data: low approx"='green','data: mid approx'='orange','data: hi approx'='red',"model: low approx"='grey50','model: mid approx'='grey44','model: hi approx'='black'))

print(p + labs(x="threshold",y="proportion of cases under bound") + theme(legend.title = element_blank()))


#---------------------------------------------------------
### Hosmer ( requires pred only) -------------------------
# refer to hosmer-lemeshow test section of Model Evaluation.docx

M=10  # divisions

hosmer <- function(pred,M){
  # pred: truth, prediction, pred false, pred true
  
  L = (0:M)/M
  H = 0
  for (i in 1:M){
    pred_g = pred %>% filter(.pred_TRUE>L[i],.pred_TRUE<=L[i+1]) # step 1
    
    # step 2
    obs_0 = nrow(pred_g %>% filter(truth==FALSE)) 
    obs_1 = nrow(pred_g %>% filter(truth==TRUE))
    
    exp_0 = sum(pred_g$.pred_FALSE)
    exp_1 = sum(pred_g$.pred_TRUE)
    
    H = H + (obs_0-exp_0)^2/exp_0 + (obs_1-exp_1)^2/exp_1
    print(H)
  }
  
  return(H)
}

hosmer_deciles <- function(pred,M){
  # pred: truth, prediction, pred false, pred true
  
  H = 0
  
  pred <- pred %>%
    mutate(decile = ntile(.pred_TRUE, M))
  
  for (i in 1:M){
    pred_g = pred %>% filter(decile == M) # step 1
    
    # step 2
    obs_0 = nrow(pred_g %>% filter(truth==FALSE)) 
    obs_1 = nrow(pred_g %>% filter(truth==TRUE))
    
    exp_0 = sum(pred_g$.pred_FALSE)
    exp_1 = sum(pred_g$.pred_TRUE)
    
    H = H + (obs_0-exp_0)^2/exp_0 + (obs_1-exp_1)^2/exp_1
    print(H)
  }
  
  return(H)
}

M = 10
H = hosmer(output,M)
H = hosmer_deciles(pred,M)
print(H)
dof = M-2

pchisq(H,df=dof,lower.tail=FALSE)

#-------------------------------------------
#MADCAP evaluation -------------------------  
# refer to MADCAP section of Model Evaluation.docx


mc_result = madcap(output) # step 5
print(mc_result)
ggplot(mc_result, aes(x))+
  geom_point(aes(y=y1,colour = 'model')) +
  geom_point(aes(y=y2, colour = 'data')) +
  scale_color_manual(breaks = c('model','data'), values = c('model'='red','data'='black')) + 
  labs(x='No. of patients (ordered by risk factor)',y='number of admissions', 
       title = "Madcap plot for randomly selected model applied to all patients in training set (60 min timeslice)") +
  theme(legend.title = element_blank())



