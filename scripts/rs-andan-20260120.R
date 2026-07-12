# install.packages("tidyverse")

library(tidyverse)

# read.csv()
diams <- read_csv("data/diamonds.csv")
diams

nrow(diams)
ncol(diams)

str(diams)
View(diams)

head(diams)
tail(diams, n = 10)

diams$cut
unique(diams$cut)
table(diams$cut)
max(table(diams$cut))

table(diams$color)

min(diams$carat)
max(diams$carat)
mean(diams$carat)
median(diams$carat)

colnames(diams)
# rownames(diams)

max(pull(diams, price))
max(diams$price)

?mean

cos(0)

0 %>% cos() %>% 
  sin() %>% 
  sqrt() %>% 
  log(3)

log(sqrt(sin(cos(0))), 3)

# max(pull(diams, price))
diams %>% 
  pull(price) %>% 
  max()

# max(table(diams$cut))

diams %>% 
  pull(cut) %>% 
  table() %>% 
  max()


