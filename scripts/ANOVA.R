library(tidyverse)
library(ez)
# psychReport

share <- read_csv("https://raw.githubusercontent.com/angelgardt/mk_ggplot2/master/sharexp_data.csv")

str(share)
unique(share$trialtype)

share %>% 
  filter(trialtype != "both")

share %>% 
  mutate(trialtype = ifelse(trialtype == "both", NA, trialtype)) -> share_na

  
sum(is.na(share_na$trialtype))

sapply(share_na, function(x) sum(is.na(x)))  

share_na %>% 
  mutate(is_na_trialtype = ifelse(is.na(trialtype), TRUE, FALSE)) %>% view


share %>% 
  mutate(trialtype = ifelse(trialtype == "both", NA, trialtype)) %>% 
  drop_na() %>% 
  # nrow()
  group_by(id, setsize, trialtype, platform) %>% 
  summarise(rt = mean(time1)) -> share_a


ezANOVA(data = share_a,
        dv = rt,
        wid = id,
        between = platform,
        detailed = TRUE) %>% 
  psychReport::aovEffectSize()




ezANOVA(data = share_a,
        dv = rt,
        wid = id,
        within = .(trialtype, setsize),
        detailed = TRUE) %>% 
  psychReport::aovEffectSize()

pairwise.t.test(share_a$rt, share_a$setsize, p.adjust.method = "bonf")
pairwise.t.test(share_a$rt, interaction(share_a$setsize, share_a$trialtype), p.adjust.method = "bonf")

# fit <- aov()
# TukeyHSD(fit)



ezANOVA(data = share_a,
        dv = rt,
        wid = id,
        between = platform,
        within = .(trialtype, setsize),
        detailed = TRUE) %>% 
  psychReport::aovEffectSize()


share_a %>% 
  ggplot(aes(as_factor(setsize), rt,
             color = trialtype, shape = platform)) +
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange",
               position = position_dodge(.3))
