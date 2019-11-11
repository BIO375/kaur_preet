#Exam 2 problems 9-11

rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()
library("DescTools")
install.packages("Hmisc")
library(Hmisc)

# Problem 9 ####
#read in the dataset
data01 <- read.csv("datasets/exams/feathers.csv")
# mutate the dataset to add a difference column called diff
# diff = typical - odd
data01 <- mutate(data01, diff = typical - odd)
# see if assumption of normality is met
ggplot(data01) +
  geom_histogram(aes(diff), binwidth = 0.5)

ggplot(data01) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(data01)+
  geom_qq(aes(sample = diff))
## normality assumption is met therefore can perform paired t-test
# One-sided, HA that typical feather is more yellow than odd feather
t.test(data01$typical, data01$odd, 
       alternative = "greater", paired =  TRUE, conf.level = 0.95)
#mutating the untidy data into tidy format
tidy_data01 <- data01 %>%
  gather(typical, odd, key="feather", value = "yellowness")


# Problem 10 ####
#read in the dataset
data02 <- read.csv("datasets/exams/baker.csv")
# mutate the dataset to add a difference column called diff
# diff = Before - After
data02 <- mutate(data02, diff = Before - After)

#diff = After - Before
data02.1 <- mutate(data02, diff= After- Before)
# see if assumption of normality is met
ggplot(data02) +
  geom_histogram(aes(diff), binwidth = 5)

ggplot(data02) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(data02)+
  geom_qq(aes(sample = diff))
# not normal therefore cannot do paired t-test and must do non parametric sign test
# One-sided, HA that Before is less than After; expecting more negatives
SignTest(data02$diff, alternative = "less", mu = 0, conf.level = 0.95)

# One-sided, HA that After is greater than Before; expecting more positives
SignTest(data02.1$diff, alternative = "greater", mu = 0, conf.level = 0.95)



# Problem 11 ####
# read in dataset
data03 <- read.csv("datasets/demos/chlamydomonas.csv")
# summary stats
summ_growthrate <- data03 %>%
  group_by(treatment) %>% 
  summarise(mean_growthrate = mean(growthrate),
            sd_growthrate = sd(growthrate),
            n_growthrate = n())
# see if homoscedasticity assumption is met (low variability)
ratio01 <-(max(summ_growthrate$sd_growthrate))/(min(summ_growthrate$sd_growthrate))
# ratio (1.13) is less than 3 therefore homoscedasticity assumption is met
# see if assumption of normality is met
ggplot(data03) +
  geom_histogram(aes(growthrate), binwidth = 2)+
  facet_wrap(~treatment)

ggplot(data03) +
  geom_boxplot(aes(x = treatment, y = growthrate))

ggplot(data03)+
  geom_qq(aes(sample = growthrate, color = treatment))
#normality assumption is met therefore can use two-sided two-sample t-test
t.test(growthrate ~ treatment, data = data03, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)


### CODE RUNS WITHOUT BREAKS, GOOD JOB, 6/6 PTS ####







