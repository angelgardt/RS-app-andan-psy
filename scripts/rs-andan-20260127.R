## install required packages
# install.packages(c("tidyverse", "moments"), repos = "https://mirrors.bfsu.edu.cn/CRAN/")

## another way to install required packages, more safe
pkgs <- c("tidyverse", "moments")
install.packages(pkgs[!pkgs %in% installed.packages()], repos = "https://mirrors.bfsu.edu.cn/CRAN/")

## load required package
library(tidyverse)

## create subfolder for data files
# dir.create("data")


## read another csv file --- 16PF questionnaire
## source: https://www.kaggle.com/datasets/tunguz/cattells-16-personality-factors
ket <- read_csv("data/ket.csv") ## pay attertion to message in console
str(ket) ## check structure --- see smth strange?

# readLines("data/ket.csv", n = 4) ## read several lines from the file
## read ket.csv with correct tab separator
ket <- read_tsv("data/ket.csv") ## rewrite ket object
str(ket) ## check structure again --- now looks good
View(ket) ## another way to check
head(ket) ## look at several (6 by default) first rows
tail(ket, n = 4) ## look at several (4 in this case) last rows

## get some basic info about the data
nrow(ket) ## number of rows
ncol(ket) ## number of columns
colnames(ket) ## names of columns
class(ket) ## wow, that's dataframe/tibble
sapply(ket, class) ## check data type of each column --- apply class function to each column of our data

## get some descriptive stats
## let's start with A1 column (A1 item in questionnaire)
mean(ket$A1) ## [arithmetic] mean
mean(ket$A1, trim = .2) ## trimmed mean

?mean ## what's the 'trim' argument in previous? let's see help about the 'mean' function
help(mean) ## or let's see help this way

var(ket$A1) ## variance (dispersion)
sd(ket$A1) ## standard deviation
median(ket$A1) ## median
quantile(ket$A1) ## by default --- return quartiles
quantile(ket$A1, probs = .20) ## you may tell R, what specific quantile you are interested in
max(ket$A1) ## get max value
min(ket$A1) ## get min value
table(ket$A1) ## get prop table, A1 is a discrete var, it will work
table(ket$country) ## or same for countries --- doesn't look so good
sort(table(ket$country)) ## let's sort previous output (ascending by default) --- now looks better
sort(table(ket$country), decreasing = TRUE) ## you may sort it descending as well
ket$country %>% table() %>% sort(decreasing = TRUE) ## same as previous line, but tidyverse-styles with pipe

## or another way using group_by() %>% summarise()
ket %>% ## take data
  group_by(country) %>% ## group data by country
  summarise( ## calculate something
    n = n() ## number of observations
  ) %>% 
  # arrange(n) ## arrange (sort) aggregated data by new column 'n' --- ascending by default 
  arrange(desc(n)) ## arrange (sort) aggregated data by new column 'n' --- now descending

ket %>% 
  filter(country == "US") %>% nrow()
ket %>% 
  filter(country == "US" & country == "FR") %>% nrow()
ket %>% 
  filter(country == "US" | country == "RU") %>% nrow()
ket %>% 
  filter(age < 18) %>% nrow()
ket %>% 
  filter(age > 18 & age < 35) %>% nrow()
ket %>% 
  filter(age < 18 & age > 60) %>% nrow()


summary(ket) ## get basic descriptive stats for all columns


ket$A1 %>% is.na() %>% sum() ## check number of NA in A1 columns --- no NA, cool!
ket %>% sapply(is.na) %>% apply(2, sum) ## check number of NA for all columns
ket %>% sapply(function(x) x %>% is.na() %>% sum()) ## check number of NA for all columns another way --- same result


unique(ket$gender) ## check unique values of gender variable --- hmmm, need codebook…

## let's fix gender var
ket %>% 
  mutate(
    gender = ifelse(gender == 0, NA,
                    ifelse(gender == 1, "male",
                           ifelse(gender == 2, "female", "other")))
  ) %>% 
  pull(gender) %>% ## pull gender column as vector
  # is.na() %>% sum() ## check number of NA
  unique() ## check unique values
## we didn't save these changes!


## or another shorter way
ket %>% ## take data 
  mutate( ## change something
    gender = recode( ## save to gender (rewrite column) as follow [according codebook]
      gender, ## take gender var
      "0" = NA_character_, ## set NA for 0 --- recode function requires explicit type of NA
      "1" = "male", ## set 'male' for 1
      "2" = "female", ## set 'female' for 2
      "3" = "other" ## set 'other' for 3
    )
  ) -> ket ## save changes
ket %>% pull(gender) %>% unique() ## check new gender values

## let's remove NA
ket %>% drop_na() ## remove all rows that contains at least one NA
ket %>% drop_na() %>% nrow() ## check the number of rows after NA removing --- ok, appropriate
ket %>% drop_na() -> cattel ## save the data without NA to new object

## maybe we don't want to keep raw tibble ket
rm(ket) ## let's remove it

# rm(list = ls()) ## remove all objects from environment -- use carefully!



## let's count desriptive stats for each item more stylish
## let's do it for the age first
cattel %>% ## take data
  summarise( ## calculate something
    n_age = n(), ## number of observations --- name output column as 'n_age'
    mean_age = mean(age), ## mean age --- name output column as 'mean_age'
    sd_age = sd(age), ## standard deviation of age  --- name output column as 'sd_age'
    max_age = max(age), ## max age --- name output column as 'max_age'
    min_age = min(age) ## min age --- name output column as 'min_age'
  )

## or simplier
cattel %>% ## take data
  summarise( ## calculate something
    n = n(), ## number of observations --- name output column as 'n'
    mean = mean(age), ## mean age --- name output column as 'mean'
    sd = sd(age), ## standard deviation of age  --- name output column as 'sd'
    max = max(age), ## max age --- name output column as 'max'
    min = min(age) ## min age --- name output column as 'min'
  )
## hmmm, look at max age

## breefly check basic graphs
hist(cattel$age) ## histogram
boxplot(cattel$age) ## boxplot

## what happens, if we set some threshold for the age?
cattel %>% # nrow()
  filter(age <= 100) %>% # nrow()
  pull(age) %>% boxplot()
## ok, more or less

cattel %>% filter(age <= 100) -> cattel ## remove inappropriate observations (based on age), rewrite data

## let's get descripstipe stats for all items at once
cattel %>% # colnames()
  select(cols = -c(age, gender, accuracy, country, source, elapsed)) %>% # colnames() ## [de]select cols
  pivot_longer( ## pivot data to long format
    cols = everything(), ## with all columns
    names_to = "item", ## name column with titles as 'item'
    values_to = "score" ## name column with values as 'score'
  ) %>% 
  summarise( ## calculate something
    n = n(), ## number of observations
    mean = mean(score), ## mean score
    median = median(score), ## median score
    sd = sd(score), ## sd of score
    min = min(score), ## min score
    max = max(score), ## max score
    skew = moments::skewness(score), ## skewness of score --- call function skewness() from moments package
    kurt = moments::kurtosis(score), ## kurtosis of score --- call function kurtosis() from moments package
    .by = "item" ## group rows by 'item' variable
  ) # %>% filter(str_detect(item, "^A")) ## filter rows where item name starts with A

## or we can do it by scale using select(starts_with(…))
cattel %>% 
  select(cols = -(age:elapsed)) %>% # colnames() ## [de]select cols
  select(starts_with("A")) %>% ## select all columns where name starts with 'A'
  pivot_longer( ## pivot data to long format
    cols = everything(), ## with all columns
    names_to = "item", ## name column with titles as 'item'
    values_to = "score" ## name column with values as 'score'
  ) %>% 
  summarise( ## calculate something
    n = n(), ## number of observations
    mean = mean(score), ## mean score
    median = median(score), ## median score
    sd = sd(score), ## sd of score
    min = min(score), ## min score
    max = max(score), ## max score
    skew = moments::skewness(score), ## skewness of score --- call function skewness() from moments package
    kurt = moments::kurtosis(score), ## kurtosis of score --- call function kurtosis() from moments package
    .by = "item" ## group rows by 'item' variable
  )


## let's see what is *_join()
colnames(cattel) ## we don't have id column

## add id column
cattel %>% 
  mutate(id = 1:nrow(cattel)) -> cattel

## make a socdem dataset for US and GB only
cattel %>% 
  select(id, age, gender, country) %>% 
  filter(country %in% c("US", "GB")) -> cattel_socdem
cattel_socdem %>% sapply(is.na) %>% apply(2, sum)


## make a survey dataset for participants older than 17
cattel %>% # colnames()
  filter(age > 17) %>% 
  select(-(age:elapsed)) %>% # colnames()
  pivot_longer(cols = -id, 
               names_to = "item", 
               values_to = "score") %>% 
  mutate(scale = item %>% str_remove("\\d+")) %>% 
  summarise(
    scale_score = sum(score),
    .by = c(id, scale)
  ) %>% 
  pivot_wider(values_from = scale_score,
              names_from = scale) -> cattel_survey
cattel_survey %>% sapply(is.na) %>% apply(2, sum)

nrow(cattel_socdem)
nrow(cattel_survey)

cattel_survey %>% 
  left_join(cattel_socdem) %>% # View()
  # filter(is.na(age)) %>% View()
  nrow()

cattel_survey %>% 
  right_join(cattel_socdem) %>% # View()
  # filter(is.na(A)) %>% View()
  nrow()

cattel_survey %>% 
  full_join(cattel_socdem) %>% # View()
  # sapply(is.na) %>% apply(2, sum)
  nrow()

cattel_survey %>% 
  inner_join(cattel_socdem) %>% # View()
  # sapply(is.na) %>% apply(2, sum)
  nrow()
