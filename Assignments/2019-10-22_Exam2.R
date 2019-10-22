#Exam 2 problems 9-12

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

# Problem 10 ####
data02

# Problem 11 ####
data03

# Problem 12 ####
data04
