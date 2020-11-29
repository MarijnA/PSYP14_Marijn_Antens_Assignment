data_pain = read.csv("https://tinyurl.com/ha-dataset1")

library(tidyverse)
library(gridExtra)
library(car)
library(lmtest)
library(sandwich)
library(boot)
library(lmboot)
library(psych)
library(lm.beta)
library(ggplot2)

View(data_pain)

data_pain = data_pain %>% 
  mutate(sex = factor(sex))

#Check the data
data_pain %>% 
  select(age, sex, STAI_trait, pain_cat, mindfulness, cortisol_serum, cortisol_saliva, pain) %>% 
  summary()

data_pain %>% 
  ggplot() +
  aes(x = pain) +
  geom_histogram(bins = 10)

#In this curve, we can see that there's one outlier in age, this person having an age of 444. This will be corrected in data_pain_corrected.
data_pain %>% 
  ggplot() +
  aes(x = age) +
  geom_density()

data_pain %>% 
  ggplot() +
  aes(x = sex) + 
  geom_bar()

#The STAI_trait uses a scale from 20 to 80. This distribution revealed one of the scores to be lower than 20. This row will be removed in data_pain_corrected.
data_pain %>% 
  ggplot() +
  aes(x = STAI_trait) +
  geom_density()

data_pain %>% 
  ggplot() +
  aes(x = pain_cat) +
  geom_density()

data_pain %>% 
  ggplot() +
  aes(x = mindfulness) +
  geom_histogram(bins = 12)

data_pain %>% 
  ggplot() +
  aes(x = cortisol_serum) +
  geom_histogram()

data_pain %>% 
  ggplot() +
  aes(x = cortisol_saliva) +
  geom_histogram()


data_pain %>% 
  ggplot() +
  aes(x = weight) +
  geom_density()

data_pain %>% 
  ggplot() +
  aes(x = IQ) +
  geom_density()

data_pain %>% 
  ggplot() +
  aes(x = household_income) + 
  geom_histogram(bins = 40)
#This histogram shows us that one person has an income below 0. These data will be excluded from the analysis.
  
data_pain_corrected = data_pain %>% 
  mutate(age = replace(age,  age=="444", NA)) %>% 
  mutate(STAI_trait = replace(STAI_trait, STAI_trait== "3.9", NA)) %>% 
  mutate(household_income = replace(household_income, household_income== "-3732", NA))

data_pain_corrected %>% 
  ggplot() +
  aes(x = age) +
  geom_density()  

data_pain_corrected %>% 
  ggplot() +
  aes(x = STAI_trait) +
  geom_density()

data_pain_corrected %>% 
  ggplot() +
  aes(x = household_income) + 
  geom_histogram(bins = 40)

#Exclude the missing values from the analysis
data_pain_corrected_no_NA <- data_pain_corrected[complete.cases(data_pain_corrected), ]
str(data_pain_corrected_no_NA)

#Models
mod_1 = lm(pain ~ age + sex, data = data_pain_corrected_no_NA)
mod_2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_pain_corrected_no_NA)

#Check for assumptions
#Assumption of normality
mod_2 %>% 
  plot(which = 2)
describe(residuals(mod_2))
#The skewness and kurtosis are close to 0

#Assumption for linearity 
mod_2 %>% 
  residualPlots()
#All Test stats are non-significant and the lines in the plots are roughly flat.

#Assumption for homoscedasticity
mod_2 %>% 
  plot(which = 3)
mod_2 %>% 
  ncvTest
#The p-value of the homoscedasticity test is non-significant.

#Assumption of no excess multicollinearity
mod_2 %>% 
  vif()
data_pain_corrected_no_NA %>% 
  select(age, sex, STAI_trait, pain_cat, mindfulness, cortisol_serum, cortisol_saliva, pain) %>% 
  pairs.panels(col = "red", lm = T)
#cortisol_serum and cortisol_salive have a very high VIF, and after checking the correlation matrices, we can see that these two variables have a relatively high correlation. This shouldn't be too much of a problem however, since we're mainly interested in the prediction accuracy of our model.

#Test statistics of model 1
sm_mod_1 = summary(mod_1)
sm_mod_1

confint(mod_1)
lm.beta(mod_1)

#Test statistics of model 2
sm_mod_2 = summary(mod_2)
sm_mod_2

confint(mod_2)
lm.beta(mod_2)

#Comparison of the two models
AIC(mod_1)
AIC(mod_2)
#The difference is larger than 2, with model 2 showing a smaller AIC. 

anova(mod_1, mod_2)