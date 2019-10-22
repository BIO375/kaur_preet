rm(list = ls())
getwd()
library("tidyverse")
tidyverse_update()

#to perform sign tests
library("DescTools")

# For later plotting
install.packages("Hmisc")
library(Hmisc)

# Chapter 12 Problem 18 ####
data01 <- read.csv("datasets/abd/chapter12/chap12q18StalkieEyespan.csv")

#summary statistics
summ_eyespan <- data01 %>%
  group_by(food) %>% 
  summarise(mean_eyespan = mean(eyeSpan),
            sd_eyespan = sd(eyeSpan),
            n_eyespan = n())

#look for homoscadasticity (ratio should be less than 3)
ratio01 <-(max(summ_eyespan$sd_eyespan))/(min(summ_eyespan$sd_eyespan))

#look for normality with plots
ggplot(data01) +
  geom_histogram(aes(eyeSpan), binwidth = 0.2)+
  facet_wrap(~food)

ggplot(data01) +
  geom_boxplot(aes(x = food, y = eyeSpan))

ggplot(data01)+
  geom_qq(aes(sample = eyeSpan, color = food))

#median is about in the middle of the IQR, the whiskers are about equal, and qq plots are mostly linear therefore assumption of normality is met;
#ratio is 3.81 which is greater than three therefore the variances are equal assumption is not met and Welch's test should be used

#welch's test
t.test(eyeSpan ~ food, data = data01, alternative = "two.sided", conf.level = 0.99)
# Corn eating group had on average bigger eyespan than cotton eating group (Welch's t-test, two sided, t=8.35, df=26.57, p<0.0001)

# Chapter 12 Problem 20 ####
data02 <- read.csv("datasets/abd/chapter12/chap12q20ElectricFish.csv")

# calculating mean difference diff
data02 <- mutate(data02, diff = speciesUpstream - speciesDownstream)

#is assumption of normality met?
ggplot(data02) +
  geom_histogram(aes(diff), binwidth = 10)

ggplot(data02) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(data02)+
  geom_qq(aes(sample = diff))
# assumption of normality is met - the median is not center IQR box but it is 
#just a little off and the whiskers are about equivalent and the qq plot is relatively
# straight
#two sample paired t-test
t.test(data02$speciesUpstream, data02$speciesDownstream, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)

#chapter 12 problem 33 ####
data03 <- read.csv("datasets/abd/chapter12/chap12q33SpinocerebellarAtaxia.csv")
# summary stats
summ_lifespan <- data03 %>%
  group_by(treatment) %>% 
  summarise(mean_lifespan = mean(lifespan),
            sd_lifespan = sd(lifespan),
            n_lifespan = n())
#test normality and homoscedascity
ratio02 <- (max(summ_lifespan$sd_lifespan))/(min(summ_lifespan$sd_lifespan))

ggplot(data03) +
  geom_histogram(aes(lifespan), binwidth = 55)+
  facet_wrap(~treatment)

ggplot(data03) +
  geom_boxplot(aes(x = treatment, y = lifespan))

ggplot(data03)+
  geom_qq(aes(sample = lifespan, color = treatment))

# ratio is less than 3; the normality is not super great but there is a small sample size
# two sample t-test
t.test(lifespan ~ treatment, data = data03, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#chapter 13 problem 21 ####
data04 <- read.csv("datasets/abd/chapter13/chap13q21StressAndIncompatibleMates.csv")

data04 <- mutate(data04, diff = corticosterone_concentration_compatible - corticosterone_concentration_incompatible)

#plots
ggplot(data04) +
  geom_histogram(aes(diff), binwidth = 10)

ggplot(data04) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(data04)+
  geom_qq(aes(sample = diff))
# plots look bad tbh; qq plot is not linear at all, boxplot has outliers
## log transformed data
data04 <- mutate(data04, log10diff = log2(-diff))
## not okay because NaNs produced
#sign test for paired two sided
SignTest(data04$diff, alternative = "two.sided", mu = 0, conf.level = 0.95)

##incompatible males cause more stress to females (sign test, two-sided: S = 1,
# n = 43, p < 0.0001).


#chapter 13 problem 30 ####
data05 <- read.csv("datasets/abd/chapter13/chap13q30DengueMosquiteTiter.csv")






          