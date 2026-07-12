pkgs <- c("tidyverse", "lavaan", "semPlot")
install.packages(pkgs[!pkgs %in% installed.packages()],
                 repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
## https://cran.r-project.org/mirrors.html


library(tidyverse)
library(lavaan)
library(semPlot)

### TOLERANCE TO UNCERTAINTY

# reading data
tolunc <- read_csv2('https://raw.githubusercontent.com/angelgardt/hseuxlab-wlm2021/master/data/tolerance_uncertainty.csv')

# checking data
str(tolunc)
summary(tolunc)

# CFA model
m1 <- "F1 =~ tu1 + tu2 + tu3 + tu4 + tu5 + tu6 + tu7 + tu8 + tu9 + tu10 + tu11 + tu12 + tu13 + tu14 + tu15 + tu16 + tu17 + tu18 + tu19 + tu20 + tu21 + tu22"
fit1 <- cfa(m1, data = tolunc)
summary(fit1)

# standardized solution
sfit <- standardizedsolution(fit1)

# factor loadings
sfit %>% 
  filter(op == "=~")

# correlations
# in this case, no correlations coz there is none in our model
sfit %>% 
  filter(op == "~~" & lhs != rhs)

# residuals
sfit %>% 
  filter(op == "~~" & sfit$lhs == sfit$rhs)

# fit measures
fitmeasures(fit1,
            c("chisq", "gfi", "agfi","cfi", "tli", "srmr", "rmsea"))

# residual correlations
resid(fit1, type = 'cor')

# visualization
semPaths(fit1, 'std')

# modification indices
# modindices(fit1)
modificationindices(fit1) %>% #nrow()
  arrange(desc(mi)) %>% 
  filter(mi > 100)


# model comparison
m2 <- "F1 =~ tu1 + tu2 + tu3 + tu4 + tu5 + tu6 + tu7 + tu8 + tu9 + tu10 + tu11 + tu12 + tu13 + tu14 + tu15 + tu16 + tu17 + tu18 + tu19 + tu20 + tu21 + tu22
tu1 ~~ tu2"

fit2 <- cfa(m2, data = tolunc)
summary(fit2)

anova(fit1, fit2)



### TAIA

taia <- read_csv(
  sprintf(
    "https://docs.google.com/uc?id=%s&export=download",
    "1L-6pG9eaJz09ILrG6sKipnHoOxiU_T2S"
  )
)

mdl1 <- "
PR =~ pr01 + pr02 + pr03 + pr04 + pr05 + pr06 + pr07 + pr08 + pr09 + pr10
CO =~ co01 + co02 + co03 + co04 + co05 + co06 + co08 + co09 + co10
UT =~ ut01 + ut02 + ut03 + ut04 + ut05 + ut06 + ut07 + ut08 + ut09 + ut11 + ut12
FA =~ fa01 + fa02 + fa03 + fa04 + fa05 + fa06 + fa07 + fa08 + fa09 + fa10
DE =~ de01 + de02 + de03 + de05 + de06 + de07 + de08 + de09 + de10 + de11
UN =~ un01 + un02 + un03 + un04 + un05 + un06 + un07 + un08 + un09 + un10 + un11 + un12
"

model1 <- cfa(mdl1, taia)
summary(model1)

fitmeasures(model1,
            c("chisq", "df", "pvalue",
              "cfi", "tli", "srmr", "rmsea"))

smodel1 <- standardizedsolution(model1)

smodel1 %>%
  filter(op == "=~") %>% 
  filter(lhs == "PR")

smodel1 %>%
  filter(op == "~~" & lhs != rhs)

smodel1 %>%
  filter(op == "~~" & lhs == rhs)

semPaths(model1,
         what = "std",
         whatLabels = "est",
         style = "lisrel",
         residScale = 10,
         theme = "colorblind",
         rotation = 1,
         layout = "tree",
         cardinal = "lat cov",
         curvePivot = TRUE,
         sizeMan = 3,
         sizeLat = 7)


mdl2 <- "
PR =~ pr01 + pr02 + pr03 + pr04 + pr05 + pr06 + pr07 + pr08 + pr09 + pr10
CO =~ co01 + co02 + co03 + co04 + co05 + co06 + co08 + co09 + co10
UT =~ ut01 + ut02 + ut03 + ut04 + ut05 + ut06 + ut07 + ut08 + ut09 + ut11 + ut12
FA =~ fa01 + fa02 + fa03 + fa04 + fa05 + fa06 + fa07 + fa08 + fa09 + fa10
DE =~ de01 + de02 + de03 + de05 + de06 + de07 + de08 + de09 + de10 + de11
UN =~ un01 + un02 + un03 + un04 + un05 + un06 + un07 + un08 + un09 + un10 + un11 + un12
DT =~ PR + CO + UT + FA + DE + UN
"

model2 <- cfa(mdl2, taia)
summary(model2)

fitmeasures(model2,
            c("chisq", "df", "pvalue",
              "cfi", "tli", "srmr", "rmsea"))

smodel2 <- standardizedsolution(model2)
smodel2 %>% 
  filter(op == "=~")

smodel2 %>% 
  filter(op == "~~")

modin <- modificationindices(model1)

modin %>% 
  arrange(desc(mi))
