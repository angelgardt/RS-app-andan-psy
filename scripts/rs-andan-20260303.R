pkgs <- c("tidyverse", "psych", "EFAtools", "GPArotation")
install.packages(pkgs[!pkgs %in% installed.packages()],
                 repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
## https://cran.r-project.org/mirrors.html


library(tidyverse)
theme_set(theme_bw())
theme_update(legend.position = "bottom")
library(psych)


### TAIA

taia <- read_csv(
  sprintf(
    "https://docs.google.com/uc?id=%s&export=download",
    "1L-6pG9eaJz09ILrG6sKipnHoOxiU_T2S"
  )
)
str(taia)

EFAtools::BARTLETT(taia)
EFAtools::KMO(taia)

fa.parallel(taia, fm = "ml", fa = "fa")

fa1 <- fa(taia, 
          nfactors = 6, 
          scores = "regression")
fa1
fa1$scores

fa(taia, 
   nfactors = 6, 
   scores = "regression",
   rotate = "promax")

fa(taia, 
   factors = 5, 
   scores = "regression",
   rotate = "varimax") 

fa(taia, 
   factors = 5, 
   scores = "regression",
   rotation = "promax")


### PIZZA

pizza <- read_csv('https://raw.githubusercontent.com/angelgardt/hseuxlab-andan/master/Pizza.csv')
str(pizza)

pizza %>% select(-id, -brand) -> pizza_efa

EFAtools::BARTLETT(pizza_efa)
EFAtools::KMO(pizza_efa)

factanal(pizza_efa,
         factors = 5, 
         scores = 'regression',
         scale. = TRUE) 

factanal(pizza_efa, 
         factors = 4, 
         scores = 'regression',
         scale. = TRUE) 

fan <- factanal(pizza_efa, 
                factors = 3, 
                scores = 'regression',
                scale. = TRUE) 

fan

fan2 <- factanal(pizza_efa, 
                 factors = 2, 
                 scores = 'regression',
                 scale. = TRUE)
fan2
fan2$scores

pizza %>% 
  ggplot(aes(fan2$scores[,1], 
             fan2$scores[,2],
             color = brand)) +
  geom_point() +
  labs(x = "Factor 1", y = "Factor 2")
