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
data01 <- read.csv(("datasets/abd/chapter12/chap12q18StalkieEyespan.csv")

            