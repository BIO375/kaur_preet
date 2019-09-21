rm(list = ls())

getwd()

library("tidyverse")

tidyverse_update()

##reading in dataset
chap13e5SagebrushCrickets<-read_csv("datasets/abd/chapter13/chap13e5SagebrushCrickets.csv", col_names = TRUE)
view(chap13e5SagebrushCrickets)

## creating log of time to mating data
chap13e5SagebrushCrickets<-mutate(chap13e5SagebrushCrickets, log_time = log(timeToMating))

##creating four histograms
## frequency of time to mating for female sagebrush crickets who were starved and frequency of time to mating for female sagebrush crickets who were fed

ggplot(chap13e5SagebrushCrickets) +
  geom_histogram(aes(timeToMating), binwidth = 17)+
  facet_wrap(~feedingStatus)

## log-transformed histogram of time to mating for starved females and log-transformed histogram of time to mating for fed females
ggplot(chap13e5SagebrushCrickets) +
  geom_histogram(aes(log_time), binwidth = 1.5)+
  facet_wrap(~feedingStatus)



