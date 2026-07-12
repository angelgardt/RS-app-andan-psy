library(tidyverse)
theme_set(theme_bw())
theme_update(legend.position = "bottom")

td <- read_csv("data/traits-decision.csv")
str(td)

unique(td$risky_choice)
hist(td$decision_quality)

td %>% mutate(risky_choice = decision_quality < 80) -> td
unique(td$risky_choice)

td %>% mutate(risky_choice = as.numeric(risky_choice)) %>% 
  filter(gender != "other") -> td
unique(td$risky_choice)
table(td$risky_choice)

## Pearson's Chi-squared test
table(td$gender, td$risky_choice)
chisq.test(td$gender, td$risky_choice)
sqrt(chisq.test(td$gender, td$risky_choice)$statistic / nrow(td)) # phi coefficient as effect size


## Logistic regression

td %>% 
  pivot_longer(cols = c(extraversion, neuroticism, openness, agreeableness, conscientiousness),
               names_to = "trait",
               values_to = "score") %>% 
  ggplot(aes(score, risky_choice)) +
  geom_point() +
  facet_wrap(~ trait)

model1 <- glm(risky_choice ~ extraversion + neuroticism + openness + agreeableness + conscientiousness, 
              family = binomial, data = td)
summary(model1)

model0 <- glm(risky_choice ~ 1, 
              family = binomial, data = td)
anova(model0, model1, test = "Chi")
(model1$null.deviance - model1$deviance) / model1$null.deviance ## pseudo R-squared
summary(model1)
exp(-0.12841) ## conscientiousness

model1.1 <- update(model1, .~. -extraversion)
summary(model1.1)
model1.2 <- update(model1.1, .~. -agreeableness)
summary(model1.2)
model1.3 <- update(model1.2, .~. -openness)
summary(model1.3)
model1.4 <- update(model1.3, .~. -neuroticism)
summary(model1.4)

AIC(model1.4, model1)
BIC(model1.4, model1)

model2 <- glm(risky_choice ~ gender * task_type,
              family = binomial, data = td)
anova(model2, model0)
summary(model2)
