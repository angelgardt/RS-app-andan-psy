pkgs <- c("tidyverse", "moments", "rstatix")
install.packages(pkgs[!pkgs %in% installed.packages()],
                 repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
## https://cran.r-project.org/mirrors.html


library(rstatix)
library(tidyverse)
theme_set(theme_bw())
theme_update(legend.position = "bottom")

## read data
td <- read_csv("data/traits-decision.csv")
str(td)

## get descriptives
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
            .by = c(gender, name)) %>% View()

td %>% sapply(is.na) %>% apply(2, sum)

t.test(decision_quality ~ gender, data = td %>% filter(gender != "other"))
t.test(td$stress_pre, td$stress_post, paired = TRUE)

fit1 <- aov(decision_quality ~ task_type, data = td)
summary(fit1)
TukeyHSD(fit1)

fit1.1 <- lm(decision_quality ~ task_type, data = td)
summary(fit1.1)
anova(fit1.1)



## new data
# share <- read_csv("https://raw.githubusercontent.com/angelgardt/wlm2023/master/data/hw5/share.csv")
# share <- read_csv2("https://raw.githubusercontent.com/angelgardt/wlm2023/master/data/hw5/share.csv")

share <- read_delim("https://raw.githubusercontent.com/angelgardt/wlm2023/master/data/hw5/share.csv", 
                    delim = " ", 
                    locale = locale(decimal_mark = ","))
str(share)

share %>% 
  summarise(mean = mean(time1),
            median = median(time1),
            sd = sd(time1),
            min = min(time1),
            max = max(time1),
            skew = moments::skewness(time1),
            kurt = moments::kurtosis(time1),
            .by = c(platform, trialtype, setsize))


share %>% sapply(is.na) %>% apply(2, sum)
unique(share$id)
unique(share$trialtype)

# quantile(share$time1, .75)

share %>% # nrow()
  filter(trialtype != "both" & correct1) %>% # nrow()
  mutate(is_outlier = ifelse(
    time1 > quantile(time1, .75) + 1.5 * IQR(time1) |
      time1 < quantile(time1, .25) - 1.5 * IQR(time1),
    TRUE, FALSE
  ),
  .by = c(id, setsize, platform, trialtype)
  ) %>% 
  filter(!is_outlier) %>% 
  summarise(rt = mean(time1),
            .by = c(id, setsize, platform, trialtype)) %>% 
  mutate(setsize = as_factor(setsize)) -> share_agg
share_agg


results <- anova_test(
  data = share_agg,
  dv = rt,
  wid = id,
  between = platform,
  within = c(setsize, trialtype),
  type = 2,
  effect.size = "pes"
)
results
get_anova_table(results)

# levene_test(rt ~ platform, data = share_agg)

pairwise.t.test(x = share_agg$rt,
                g = interaction(share_agg$setsize, share_agg$trialtype),
                p.adjust.method = "bonf", paired = TRUE)


share_agg %>% 
  ggplot(aes(setsize, rt, color = platform, shape = trialtype,
             group = interaction(trialtype, platform))) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange",
               position = position_dodge(.3)) +
  stat_summary(fun.data = mean_cl_boot, geom = "line", linetype = "dashed",
               position = position_dodge(.3)) +
  scale_color_discrete(labels = c("android" = "Android",
                                  "ios" = "iOS")) +
  scale_shape(labels = c("dots" = "Three dots",
                         "tray" = "Outgoing Tray")) +
  labs(x = "Количество стимулов в пробе",
       y = "Время реакции, с",
       color = "Платформа",
       shape = "Тип пробы",
       caption = "отображен 95% доверительный интервал")
