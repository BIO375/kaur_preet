#### Exam 3 ####
rm(list = ls())
getwd()
# loading libraries and checking for updates
library("ggfortify")
library("multcomp")
library("tidyverse")
tidyverse_update()


#Problem 9####
bacteria <- read_csv("datasets/exams/bacteria.csv")

#Problem 10####
aphids <- read_csv("datasets/exams/aphids.csv", col_types = cols(gall_number = col_factor() ) )

#Problem 11####
glucose <- read_csv("datasets/exams/glucose.csv")


#Problem 12 ####
road <- read_csv("datasets/exams/DriverVision.csv")
#in order to plot residuals by fitted 
modelroad <- lm(Distance ~ Age, data = road)
autoplot(modelroad, smooth.colour = NA)
# residuals vs fitted plot does not have a fan shape therefore assumption of homogeneity of variance is
# met. trying residuals by x plot now
road <- road %>%
  mutate(Resid_distance = resid(modelroad))
ggplot(data = road) +
  geom_point(aes(x = Age, y = Resid_distance))
# seeing the statistical results
summary(modelroad)
# setting 95% confidence interval bands
ggplot(data = road, aes(x = Age, y = Distance)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "Driver Age", y = "Long Distance Vision")
# a lot of the dots were not within the bands






