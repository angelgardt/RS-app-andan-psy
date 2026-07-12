library(tidyverse)
# library(ggplot2)
theme_set(theme_bw())
theme_update(legend.position = "bottom")

cattel <- read_csv("data/cattel.csv")
str(cattel)

hist(cattel$age)
boxplot(cattel$age)
plot(cattel$age, cattel$A1)


ggplot()

cattel %>% 
  ggplot(aes(x = age))

cattel %>% 
  ggplot(aes(x = age)) +
  geom_histogram()

cattel %>% 
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 2)

cattel %>% 
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 2) +
  labs(x = "Возраст, лет",
       y = "Количество")

cattel %>% 
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 2) +
  labs(x = "Возраст, лет",
       y = "Количество") +
  theme_bw()

cattel %>% 
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 2, fill = "royalblue") +
  labs(x = "Возраст, лет",
       y = "Количество") +
  theme_bw()


unique(cattel$country)

cattel %>% 
  ggplot(aes(gender, fill = country)) +
  geom_bar(position = position_dodge()) +
  labs(x = "Пол",
       y = "Количество",
       fill = "Страна") +
  scale_x_discrete(labels = c("female"= "Женский",
                              "male" = "Мужской",
                              "other" = "Другое"))


cattel %>% 
  ggplot(aes(gender)) +
  geom_bar() +
  facet_wrap(~ country) +
  labs(x = "Пол",
       y = "Количество") +
  scale_x_discrete(labels = c("female"= "Женский",
                              "male" = "Мужской",
                              "other" = "Другое"))


cattel %>% 
  filter(country == "GB") %>% 
  select(id, age, gender, starts_with("B")) %>% # colnames()
  pivot_longer(cols = -c(id, age, gender),
               names_to = "item",
               values_to = "score") %>% 
  summarise(score = sum(score),
            .by = c(id, age, gender)) %>% 
  ggplot(aes(x = age, y = score, 
             color = gender,
             shape = gender)) +
  geom_point(alpha = .5) +
  facet_wrap(~ gender) +
  guides(color = "none",
         shape = "none") +
  labs(x = "Возраст, лет",
       y = "Суммарный балл",
       color = "Пол", shape = "Пол",
       title = "Связь возраста с личностными чертами",
       subtitle = "Шкала B опросника 16PF",
       caption = "данные по респондентам Великобритании")


