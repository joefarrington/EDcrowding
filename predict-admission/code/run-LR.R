library(aod)

dm480p[, adm_ := as.numeric(adm == 1)]

# need to think about converting to factors and handling missing values

mylogit <- glm(adm_ ~ a_age + a_sex_F + l_num, data = dm480p, family = "binomial")
summary(mylogit)
