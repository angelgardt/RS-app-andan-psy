pkgs <- c("psych", "car", "moments", "ggcorrplot")
install.packages(pkgs[!pkgs %in% installed.packages()])

## load packages
library(tidyverse)
theme_set(theme_bw())

## read data, check structure
td <- read_csv("data/traits-decision.csv")
str(td)

## basic descriptive stats
summary(td)

## more detailed descriptive stats
psych::describe(td %>% select(age, 
                              extraversion, neuroticism, openness, agreeableness, conscientiousness,
                              working_memory, stress_pre, stress_post,
                              decision_quality))
psych::describeBy(td %>% select(age, 
                              extraversion, neuroticism, openness, agreeableness, conscientiousness,
                              working_memory, stress_pre, stress_post,
                              decision_quality),
                  group = td$gender)

td %>% select(age, gender,
              extraversion, neuroticism, openness, agreeableness, conscientiousness,
              working_memory, stress_pre, stress_post,
              decision_quality) %>% 
  pivot_longer(cols = -gender) %>% 
  summarise(n = n(),
            mean = mean(value),
            trimmed = mean(value, trim = .2),
            var = var(value),
            sd = sd(value),
            median = median(value),
            min = min(value),
            max = max(value),
            range = max - min,
            skew = moments::skewness(value),
            kurt = moments::kurtosis(value) - 3,
            .by = c(gender, name))

## correlation
hist(td$decision_quality)
boxplot(td$decision_quality)

hist(td$neuroticism)
boxplot(td$neuroticism)


cor(td$decision_quality, td$neuroticism)
cor(td$decision_quality, td$working_memory, method = "sp")

cor.test(td$decision_quality, td$neuroticism)
cor.test(td$decision_quality, td$working_memory, method = "sp")

td %>% ggplot(aes(neuroticism, decision_quality)) +
  geom_point() +
  # geom_smooth()
  geom_smooth(method = "lm")

cor(td %>% select(conscientiousness, 
                  agreeableness, 
                  neuroticism, 
                  openness, 
                  extraversion)) %>% 
  round(2)

cor(td %>% select(conscientiousness, 
                  agreeableness, 
                  neuroticism, 
                  openness, 
                  extraversion)) %>% 
  ggcorrplot::ggcorrplot()

cor(td %>% select(conscientiousness, 
                  agreeableness, 
                  neuroticism, 
                  openness, 
                  extraversion)) %>% 
  ggcorrplot::ggcorrplot(type = "lower",
                         lab = TRUE, show.legend = FALSE,
                         colors = c("salmon", "white", "royalblue"))


## regression
model1 <- lm(decision_quality ~ neuroticism, data = td)
summary(model1)

model2 <- lm(decision_quality ~ neuroticism + working_memory, data = td)
summary(model2)

model3 <- lm(decision_quality ~ neuroticism + task_type, data = td)
summary(model3)

model4 <- lm(decision_quality ~ neuroticism * social_pressure, data = td)
summary(model4)
model4.1 <- lm(decision_quality ~ neuroticism + social_pressure, data = td)
summary(model4.1)

par(mfrow = c(2, 2))
plot(model4)

anova(model4, model4.1)

