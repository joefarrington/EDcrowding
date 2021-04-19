library(aod)
library(lmtest) # for LR test

# set dependent variable
dm[, adm_ := as.numeric(adm == 1)]

# set up independent variables
dm[, a_adm_hist := !is.na(a_days_since_last_visit)]
dm[, a_arrival := factor(a_arrival, levels = c("Walkin", "PublicTrans", "Ambulance", "Ambnomedic", "Other"))]

# need to think about converting to factors and handling missing values

base1 <- glm(adm_ ~ a_age + a_sex + a_weekend + a_night + a_arrival +
                 a_adm_hist + a_inpatient , 
               data = dm, family = "binomial")
summary(base1)

# gender, nighttime and inpatient not signic

base0 <- glm(adm_ ~ a_age  + a_weekend  + a_arrival +
                 a_adm_hist  , 
               data = dm, family = "binomial")
summary(base0)


# LR test from https://api.rpubs.com/tomanderson_34/lrt
# (A <- logLik(base0))
# (B <- logLik(base1))
# (teststat <- -2 * (as.numeric(A)-as.numeric(B)))
# (p.val <- pchisq(teststat, df = (13-8), lower.tail = FALSE))

lrtest(base0, base1) # highly signic - more complex model is better

base2 <- glm(adm_ ~ a_age + a_sex + a_arrival +
               a_adm_hist , 
             data = dm, family = "binomial")
summary(base2)

lrtest(base2, base1)

# confidence intervals from https://stats.idre.ucla.edu/r/dae/logit-regression/
confint(base1) # profiled log likelihood function
confint.default(base1) # similar results, quicker

# get OR and CIs
exp(cbind(OR = coef(base1), confint(base1)))

# wald test for test of one coefficient
wald.test(b = coef(base1), Sigma = vcov(base1), Terms = 8:11) # arrival method - signic
wald.test(b = coef(base1), Sigma = vcov(base1), Terms = 3:5) # gender - not signic
