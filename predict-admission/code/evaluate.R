### convert predictions to distribution
### then perform evaluations
library(tidymodels)
library(discrim)
library(dplyr)
library(lubridate)
library(xgboost)
library(parsnip)
library(caret)
library(polynom)

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

pred = read.csv('F:/Saved/ENOCKUNG/ED project/prediction.csv')

### predictions to distribution -----------------------------
# Using individual admission probabilities, create a prob density function
# for the number of patients admitted from the prediction

poly_prod = function(df){
  probs = df$.pred_TRUE # get column of probabilities for admission
  
  # polynomial(vector) is a polynomial t
  y = polynomial(c(1,0))# 
  for (n in 1:nrow(df)){
    y = y*polynomial(c(1-probs[n],probs[n]))
  }
  return(coef(y))
}

dist = poly_prod(pred)

# -------------------------------------------------------
### distribution evaluation -----------------------------
### requires as input a series of predictions. each prediction is of format 
### (truth, prediction, 2 probabilities). to apply a prepared function that would generate 
### the curves, the format of the input for the series of predictions should be a variable preds with format
### (truth_1, probability of admission 1, truth_2, probability of admission 2, ... , truth_N, probability of admission N)


# generate using the xgboost model the series of predictions and change to necessary format (leave this to you, sorry)


# plug into the function

result = dist_eval(preds)

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

H = hosmer(output,M)
print(H)
dof = M-2

pchisq(H,df=dof,lower.tail=FALSE)

#-------------------------------------------
#MADCAP evaluation -------------------------  
# refer to MADCAP section of Model Evaluation.docx

# step 1-4 in misc_funcs.R
source('misc_funcs.R')

mc_result = madcap(output) # step 5
print(mc_result)
q = ggplot(mc_result, aes(x))+
  geom_point(aes(y=y1,colour = 'model')) +
  geom_point(aes(y=y2, colour = 'data')) +
  scale_color_manual(breaks = c('model','data'), values = c('model'='red','data'='black'))

print(q + labs(x='No. of patients (ordered by risk factor)',y='number of admissions') + theme(legend.title = element_blank()))



