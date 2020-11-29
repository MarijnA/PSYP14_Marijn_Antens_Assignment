#Check the newly included variables
data_pain %>% 
  select(weight, IQ, household_income) %>% 
  summary()

#Backward regression
mod_comment = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_pain_corrected_no_NA)
summary(mod_comment)

mod_back_comment = step(mod_comment, direction = "backward")
mod_back_comment

backward_model = lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + household_income, data = data_pain_corrected_no_NA)
summary(backward_model)

theory_based_model = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_pain_corrected_no_NA)
summary(theory_based_model)

#Test statistics backward model
confint(backward_model)
lm.beta(backward_model)

#Comparison of the models
AIC(theory_based_model)
AIC(backward_model)
AIC(mod_comment)

anova(backward_model, mod_comment)
anova(theory_based_model, backward_model)


#dataset 2
data_pain_2 = read.csv("https://tinyurl.com/ha-dataset2")

View(data_pain_2)

data_pain_2 = data_pain_2 %>% 
  mutate(sex = factor(sex))

#Check the data
data_pain_2 %>% 
  ggplot()+
  aes(x = pain)+
  geom_histogram(bins = 10)

data_pain_2 %>% 
  ggplot() +
  aes(x = age) +
  geom_density()

data_pain_2 %>% 
  ggplot() +
  aes(x = sex) + 
  geom_bar()

data_pain_2 %>% 
  ggplot() +
  aes(x = STAI_trait) +
  geom_density()

data_pain_2 %>% 
  ggplot() +
  aes(x = pain_cat) +
  geom_density()

data_pain_2 %>% 
  ggplot() +
  aes(x = mindfulness) +
  geom_histogram(bins = 12)

data_pain_2 %>% 
  ggplot() +
  aes(x = cortisol_serum) +
  geom_histogram()

data_pain_2 %>% 
  ggplot() +
  aes(x = cortisol_saliva) +
  geom_histogram()

data_pain_2 %>% 
  select(weight, IQ, household_income) %>% 
  summary()

data_pain_2 %>% 
  ggplot() +
  aes(x = weight) +
  geom_density()

data_pain_2 %>% 
  ggplot() +
  aes(x = IQ) +
  geom_density()

#Models
backward_model_2 = lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + household_income, data = data_pain_2)
summary(backward_model_2)

theory_based_model_2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_pain_2)
summary(theory_based_model_2)

#Comparison of these models
AIC(theory_based_model_2)
AIC(backward_model_2)

anova(theory_based_model_2, backward_model_2)

#Prediction performance
pred_theory_based_model_2 = predict(theory_based_model_2, data_pain_2) 
pred_backward_model_2 = predict(backward_model_2, data_pain_2)

RSS_theory_based_model_2 = sum((data_pain_2[, "pain"] - pred_theory_based_model_2)^2)
RSS_backward_model_2 = sum((data_pain_2[,"pain"] - pred_backward_model_2)^2)

RSS_theory_based_model_2
RSS_backward_model_2