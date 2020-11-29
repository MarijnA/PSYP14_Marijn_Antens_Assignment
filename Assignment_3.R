data_pain_3 = read.csv("https://tinyurl.com/ha-dataset3")
data_pain_4 = read.csv("https://tinyurl.com/ha-dataset4")

View(data_pain_3)
View(data_pain_4)

library(cAIC4)
library(r2glmm)
library(lme4)
library(lmerTest)
library(MuMIn)
library(optimx)

#Check data
data_pain_3 = data_pain_3 %>% 
  mutate(sex = factor(sex))

summary(data_pain_3)

data_pain_3 %>% 
  ggplot()+
  aes(x = pain)+
  geom_histogram(bins = 10)

data_pain_3 %>% 
  ggplot() +
  aes(x = age) +
  geom_density()

data_pain_3 %>% 
  ggplot() +
  aes(x = sex) + 
  geom_bar()
#One participant filled in femlae instead of female. 

data_pain_3 <- data_pain_3 %>% 
  mutate(sex = replace(sex, sex=="femlae", "female"))

data_pain_3 %>% 
  ggplot() +
  aes(x = STAI_trait) +
  geom_density()

data_pain_3 %>% 
  ggplot() +
  aes(x = pain_cat) +
  geom_density()

data_pain_3 %>% 
  ggplot() +
  aes(x = mindfulness) +
  geom_histogram(bins = 12)

data_pain_3 %>% 
  ggplot() +
  aes(x = cortisol_serum) +
  geom_histogram()

data_pain_3 %>% 
  ggplot() +
  aes(x = cortisol_saliva) +
  geom_histogram()

data_pain_3 %>% 
  ggplot() +
  aes(x = weight) +
  geom_density()

data_pain_3 %>% 
  ggplot() +
  aes(x = IQ) +
  geom_density()

data_pain_4 = data_pain_4 %>% 
  mutate(sex = factor(sex))

summary(data_pain_4)

data_pain_4 %>% 
  ggplot()+
  aes(x = pain)+
  geom_histogram(bins = 10)

data_pain_4 %>% 
  ggplot() +
  aes(x = age) +
  geom_density()

data_pain_4 %>% 
  ggplot() +
  aes(x = sex) + 
  geom_bar()

data_pain_4 %>% 
  ggplot() +
  aes(x = STAI_trait) +
  geom_density()

data_pain_4 %>% 
  ggplot() +
  aes(x = pain_cat) +
  geom_density()

data_pain_4 %>% 
  ggplot() +
  aes(x = mindfulness) +
  geom_histogram(bins = 12)
#One person had a score of 6.05

data_pain_4 %>% 
  ggplot() +
  aes(x = cortisol_serum) +
  geom_histogram()

data_pain_4 %>% 
  ggplot() +
  aes(x = cortisol_saliva) +
  geom_histogram()

data_pain_4 %>% 
  ggplot() +
  aes(x = weight) +
  geom_density()

data_pain_4 %>% 
  ggplot() +
  aes(x = IQ) +
  geom_density()

data_pain_4_corrected = data_pain_4 %>% 
  mutate(mindfulness = replace(mindfulness,  mindfulness=="6.05", NA))

data_pain_4_corrected %>% 
  ggplot() +
  aes(x = mindfulness) +
  geom_histogram(bins = 12)

#Random intercept model
mod_rnd_int_3 = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva + (1|hospital), data = data_pain_3)
summary(mod_rnd_int_3)

stdCoef.merMod <-function(object) {
  sdy <-sd(getME(object,"y"))
  sdx <-apply(getME(object,"X"), 2, sd)
  sc <-fixef(object)*sdx/sdy
  se.fixef <-coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
  }

confint(mod_rnd_int_3)
stdCoef.merMod(mod_rnd_int_3)

#Marginal and conditional R squared
r2beta(mod_rnd_int_3, method = "nsj", data = data_pain_3)

r.squaredGLMM(mod_rnd_int_3)
#Since the CI doesn't include 0, the marginal R squared is significant

#Regression equation for data file 4
mod_rnd_int_4 = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva + (1|hospital), data = data_pain_4)
summary(mod_rnd_int_4)



#Calculating the variance of this model
RSS_mod_rnd_int_4 = sum(residuals(mod_rnd_int_4)^2)
RSS_mod_rnd_int_4

mod_mean_4 = lm(pain ~ 1, data = data_pain_4)
TSS_mod_4 = sum((data_pain_4$pain - predict(mod_mean_4))^2)
TSS_mod_4

R2 = 1-(RSS_mod_rnd_int_4/TSS_mod_4)
R2

#The most influential predictor from the previous model is cortisol saliva
mod_3 = lmer(pain ~ cortisol_saliva + (cortisol_saliva|hospital), control = lmerControl(optimizer = "Nelder_Mead"), data = data_pain_3)
summary(mod_3)

#Visualization
data_pain_3_slope = data_pain_3 %>%
  mutate(pred_slope =predict(mod_3))

data_pain_3_slope %>%
  ggplot()+
  aes(y = pain, x = cortisol_saliva, group = hospital)+
  geom_point(aes(color = hospital), size = 10)+
  geom_line(color='red',aes(y=pred_slope, x=cortisol_saliva))+
              facet_wrap(~hospital, ncol = 2)

#Comparison
cAIC(mod_rnd_int_3)$caic
cAIC(mod_3)$caic

anova(mod_rnd_int_3, mod_3)